use std::collections::HashMap;
use std::fmt;

use super::diagnostics::*;
use super::lexer::*;

pub type Bindings = HashMap<String, Expr>;

macro_rules! fun_args {
    () => { vec![] };
    ($name:ident) => { vec![expr!($name)] };
    ($name:ident,$($rest:tt)*) => {
        {
            let mut t = vec![expr!($name)];
            t.append(&mut fun_args!($($rest)*));
            t
        }
    };
    ($name:ident($($args:tt)*)) => {
        vec![expr!($name($($args)*))]
    };
    ($name:ident($($args:tt)*),$($rest:tt)*) => {
        {
            let mut t = vec![expr!($name($($args)*))];
            t.append(&mut fun_args!($($rest)*));
            t
        }
    }
}

macro_rules! expr {
    ($name:ident) => {
        Expr::make_ident(stringify!($name), loc_here!())
    };
    ($name:ident($($args:tt)*)) => {
        Expr::Fun(Box::new(Expr::make_ident(stringify!($name), loc_here!())), fun_args!($($args)*))
    };
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    Eql,
}

impl Op {
    fn from_token_kind(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Plus => Some(Op::Add),
            TokenKind::Dash => Some(Op::Sub),
            TokenKind::Asterisk => Some(Op::Mul),
            TokenKind::Slash => Some(Op::Div),
            TokenKind::Caret => Some(Op::Pow),
            TokenKind::Percent => Some(Op::Mod),
            TokenKind::EqualsEquals => Some(Op::Eql),
            _ => None,
        }
    }

    pub fn precedence(&self) -> usize {
        use Op::*;
        match self {
            Eql => 0,
            Add | Sub => 1,
            Mul | Div | Mod => 2,
            Pow => 3,
        }
    }

    const MAX_PRECEDENCE: usize = 3;
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Op::Eql => write!(f, "=="),
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::Mod => write!(f, "%"),
            Op::Pow => write!(f, "^"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Sym(Token),
    Var(Token),
    Fun(Box<Expr>, Vec<Expr>),
    Op(Op, Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn replace_head() -> Expr {
        expr!(apply_rule(Strategy, Head, Body, Expr))
    }

    pub fn substitute(&mut self, bindings: &Bindings) {
        match self {
            Self::Sym(_) => {}

            Self::Var(name) => {
                if let Some(value) = bindings.get(&name.text) {
                    *self = value.clone()
                }
            }

            Self::Op(_, lhs, rhs) => {
                lhs.substitute(bindings);
                rhs.substitute(bindings);
            }

            Self::Fun(head, args) => {
                head.substitute(bindings);
                for arg in args {
                    arg.substitute(bindings)
                }
            }
        }
    }

    pub fn make_ident(name: &str, loc: Loc) -> Self {
        Self::parse_ident(Token {
            kind: TokenKind::Ident,
            text: name.to_string(),
            loc,
        })
    }

    pub fn parse_ident(token: Token) -> Self {
        assert!(token.kind == TokenKind::Ident);
        let x = token
            .text
            .chars()
            .next()
            .expect("Empty names are not allowed. This might be a bug in the lexer.");
        if x.is_uppercase() || x == '_' {
            Self::Var(token)
        } else {
            Self::Sym(token)
        }
    }

    pub fn human_name(&self) -> &'static str {
        match self {
            Self::Sym(_) => "a symbol",
            Self::Var(_) => "a variable",
            Self::Fun(_, _) => "a functor",
            Self::Op(_, _, _) => "a binary operator",
        }
    }

    fn parse_fun_args(lexer: &mut Lexer, diag: &mut impl Diagnoster) -> Option<Vec<Self>> {
        use TokenKind::*;
        let mut args = Vec::new();
        let open_paren_token = lexer
            .expect_token(OpenParen)
            .map_err(|(expected_kind, actual_token)| {
                diag.report(
                    &actual_token.loc,
                    Severity::Error,
                    &format!(
                        "Functor argument list must start with {}, but we got {} instead",
                        expected_kind,
                        actual_token.report()
                    ),
                )
            })
            .ok()?;
        if lexer.peek_token().kind == CloseParen {
            lexer.next_token();
            return Some(args);
        }
        args.push(Self::parse(lexer, diag)?);
        while lexer.peek_token().kind == Comma {
            lexer.next_token();
            args.push(Self::parse(lexer, diag)?);
        }
        lexer
            .expect_token(CloseParen)
            .map_err(|(expected_kind, actual_token)| {
                diag.report(
                    &actual_token.loc,
                    Severity::Error,
                    &format!(
                        "Functor argument list must end with {}, but we got {} instead",
                        expected_kind,
                        actual_token.report()
                    ),
                );
                diag.report(
                    &open_paren_token.loc,
                    Severity::Info,
                    &format!("The corresponding {} is here.", open_paren_token.kind),
                );
            })
            .ok()?;
        Some(args)
    }

    fn parse_primary(lexer: &mut Lexer, diag: &mut impl Diagnoster) -> Option<Self> {
        let mut head = {
            let token = lexer.next_token();
            match token.kind {
                TokenKind::OpenParen => {
                    let result = Self::parse(lexer, diag)?;
                    lexer.expect_token(TokenKind::CloseParen).map_err(|(expected_kind, actual_token)| {
                        diag.report(&actual_token.loc, Severity::Error, &format!("Expected {} at the end of the expression, but we got {} instead.", expected_kind, actual_token.report()));
                        diag.report(&token.loc, Severity::Info, &format!("The corresponding {} is here.", token.kind));
                    }).ok()?;
                    result
                }

                TokenKind::Ident => Self::parse_ident(token),

                _ => {
                    diag.report(&token.loc, Severity::Error, &format!("Expected start of a primary expression. Primary expressions start with {} or {}.", TokenKind::Ident, TokenKind::OpenParen));
                    return None;
                }
            }
        };

        while lexer.peek_token().kind == TokenKind::OpenParen {
            head = Expr::Fun(Box::new(head), Self::parse_fun_args(lexer, diag)?)
        }
        Some(head)
    }

    fn parse_binary_operator(
        lexer: &mut Lexer,
        current_precedence: usize,
        diag: &mut impl Diagnoster,
    ) -> Option<Self> {
        if current_precedence > Op::MAX_PRECEDENCE {
            return Self::parse_primary(lexer, diag);
        }

        let mut result = Self::parse_binary_operator(lexer, current_precedence + 1, diag)?;

        while let Some(op) = Op::from_token_kind(lexer.peek_token().kind) {
            if current_precedence != op.precedence() {
                break;
            }

            lexer.next_token();

            result = Expr::Op(
                op,
                Box::new(result),
                Box::new(Self::parse_binary_operator(
                    lexer,
                    current_precedence,
                    diag,
                )?),
            );
        }

        Some(result)
    }

    pub fn parse(lexer: &mut Lexer, diag: &mut impl Diagnoster) -> Option<Self> {
        Self::parse_binary_operator(lexer, 0, diag)
    }

    pub fn pattern_match(&self, value: &Expr) -> Option<Bindings> {
        fn pattern_match_impl(pattern: &Expr, value: &Expr, bindings: &mut Bindings) -> bool {
            use Expr::*;
            match (pattern, value) {
                (Sym(name1), Sym(name2)) => name1 == name2,
                (Var(name), _) => {
                    if name.text == "_" {
                        true
                    } else if let Some(bound_value) = bindings.get(&name.text) {
                        bound_value == value
                    } else {
                        bindings.insert(name.text.clone(), value.clone());
                        true
                    }
                }
                (Op(op1, lhs1, rhs1), Op(op2, lhs2, rhs2)) => {
                    *op1 == *op2
                        && pattern_match_impl(lhs1, lhs2, bindings)
                        && pattern_match_impl(rhs1, rhs2, bindings)
                }
                (Fun(name1, args1), Fun(name2, args2)) => {
                    if pattern_match_impl(name1, name2, bindings) && args1.len() == args2.len() {
                        args1
                            .iter()
                            .zip(args2)
                            .all(|(arg1, arg2)| pattern_match_impl(arg1, arg2, bindings))
                    } else {
                        false
                    }
                }
                _ => false,
            }
        }

        let mut bindings = HashMap::new();

        if pattern_match_impl(self, value, &mut bindings) {
            Some(bindings)
        } else {
            None
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Sym(name) | Expr::Var(name) => write!(f, "{}", name.text),
            Expr::Fun(head, args) => {
                match &**head {
                    Expr::Sym(name) | Expr::Var(name) => write!(f, "{}", name.text)?,
                    other => write!(f, "({})", other)?,
                }
                write!(f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expr::Op(op, lhs, rhs) => {
                match **lhs {
                    Expr::Op(sub_op, _, _) => {
                        if sub_op.precedence() <= op.precedence() {
                            write!(f, "({})", lhs)?
                        } else {
                            write!(f, "{}", lhs)?
                        }
                    }
                    _ => write!(f, "{}", lhs)?,
                }
                if op.precedence() <= 1 {
                    write!(f, " {} ", op)?;
                } else {
                    write!(f, "{}", op)?;
                }
                match **rhs {
                    Expr::Op(sub_op, _, _) => {
                        if sub_op.precedence() <= op.precedence() {
                            write!(f, "({})", rhs)
                        } else {
                            write!(f, "{}", rhs)
                        }
                    }
                    _ => write!(f, "{}", rhs),
                }
            }
        }
    }
}

pub fn matches_at_least_one<'a>(pattern: &'a Expr, expr: &'a Expr) -> bool {
    if pattern.pattern_match(expr).is_some() {
        return true;
    }

    match expr {
        Expr::Fun(head, args) => {
            if matches_at_least_one(pattern, head) {
                return true;
            }
            for arg in args {
                if matches_at_least_one(pattern, arg) {
                    return true;
                }
            }
        }
        Expr::Op(_, lhs, rhs) => {
            if matches_at_least_one(pattern, lhs) || matches_at_least_one(pattern, rhs) {
                return true;
            }
        }
        Expr::Sym(_) | Expr::Var(_) => {}
    }

    false
}

pub fn find_all_subexprs<'a>(pattern: &'a Expr, expr: &'a Expr) -> Vec<&'a Expr> {
    let mut subexprs = Vec::new();

    fn find_all_subexprs_impl<'a>(pattern: &'a Expr, expr: &'a Expr, subexprs: &mut Vec<&'a Expr>) {
        if pattern.pattern_match(expr).is_some() {
            subexprs.push(expr);
        }

        match expr {
            Expr::Fun(head, args) => {
                find_all_subexprs_impl(pattern, head, subexprs);
                for arg in args {
                    find_all_subexprs_impl(pattern, arg, subexprs);
                }
            }
            Expr::Op(_, lhs, rhs) => {
                find_all_subexprs_impl(pattern, lhs, subexprs);
                find_all_subexprs_impl(pattern, rhs, subexprs);
            }
            Expr::Sym(_) | Expr::Var(_) => {}
        }
    }

    find_all_subexprs_impl(pattern, expr, &mut subexprs);
    subexprs
}

pub fn highlight_subexpr(expr: &Expr, subexpr: &Expr) -> Result<String, std::fmt::Error> {
    use std::fmt::Write;
    let mut result = String::new();
    if std::ptr::eq(expr, subexpr) {
        write!(result, "\x1b[47m\x1b[30m{expr}\x1b[0m")?;
    } else {
        match expr {
            Expr::Sym(name) | Expr::Var(name) => write!(result, "{}", name.text)?,
            Expr::Fun(head, args) => {
                match &**head {
                    Expr::Sym(name) | Expr::Var(name) => write!(result, "{}", name.text)?,
                    other => write!(result, "({})", highlight_subexpr(other, subexpr)?)?,
                }
                write!(result, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(result, ", ")?;
                    }
                    write!(result, "{}", highlight_subexpr(arg, subexpr)?)?;
                }
                write!(result, ")")?;
            }
            Expr::Op(op, lhs, rhs) => {
                match **lhs {
                    Expr::Op(sub_op, _, _) => {
                        if sub_op.precedence() <= op.precedence() {
                            write!(result, "({})", highlight_subexpr(lhs, subexpr)?)?
                        } else {
                            write!(result, "{}", highlight_subexpr(lhs, subexpr)?)?
                        }
                    }
                    _ => write!(result, "{}", highlight_subexpr(lhs, subexpr)?)?,
                }
                if op.precedence() <= 1 {
                    write!(result, " {} ", op)?;
                } else {
                    write!(result, "{}", op)?;
                }
                match **rhs {
                    Expr::Op(sub_op, _, _) => {
                        if sub_op.precedence() <= op.precedence() {
                            write!(result, "({})", highlight_subexpr(rhs, subexpr)?)?;
                        } else {
                            write!(result, "{}", highlight_subexpr(rhs, subexpr)?)?;
                        }
                    }
                    _ => write!(result, "{}", highlight_subexpr(rhs, subexpr)?)?,
                }
            }
        }
    }
    Ok(result)
}

#[cfg(test)]
mod pattern_match_tests {
    use std::collections::HashMap;

    use crate::engine::lexer::Loc;

    use super::Expr;

    #[test]
    fn anything_bindings_with_a_variable() {
        assert_bindings(expr!(A), expr!(A), vec![("A", expr!(A))]);
        assert_bindings(expr!(A), expr!(B), vec![("A", expr!(B))]);
        assert_bindings(expr!(A), expr!(f()), vec![("A", expr!(f()))]);
        assert_bindings(expr!(A), expr!(f(X)), vec![("A", expr!(f(X)))]);
        assert_bindings(
            expr!(A),
            expr!(f(X, g(Y), Z)),
            vec![("A", expr!(f(X, g(Y), Z)))],
        );
    }

    #[test]
    fn function_pattern_only_bindings_with_other_functions_with_same_name_and_number_of_args() {
        assert_no_bindings(expr!(f()), expr!(a));
        assert_no_bindings(expr!(f()), expr!(g()));
        assert_no_bindings(expr!(f(X0)), expr!(f(Y0, Y1)));

        assert_bindings(expr!(f()), expr!(f()), vec![]);
        assert_bindings(expr!(f(X0)), expr!(f(Y0)), vec![("X0", expr!(Y0))]);
    }

    #[test]
    fn test_with_same_repeated_variables() {
        assert_bindings(expr!(f(X0, X0)), expr!(f(Y0, Y0)), vec![("X0", expr!(Y0))]);
        assert_bindings(
            expr!(f(g(X0, X1), X0)),
            expr!(f(g(Y0, Y1), Y0)),
            vec![("X0", expr!(Y0)), ("X1", expr!(Y1))],
        );

        assert_no_bindings(expr!(f(X0, X0)), expr!(f(Y0, Y1)));
        assert_no_bindings(expr!(f(g(X0, X0), X1)), expr!(f(g(Y0, Y1), X1)));
    }

    #[test]
    fn test_recursive_pattern_matching() {
        assert_bindings(
            expr!(f(X0, X1)),
            expr!(f(Y0, Y1)),
            vec![("X0", expr!(Y0)), ("X1", expr!(Y1))],
        );
        assert_bindings(
            expr!(f(X0, g(X1), X2)),
            expr!(f(Y0, g(Y1), Y2)),
            vec![("X0", expr!(Y0)), ("X1", expr!(Y1)), ("X2", expr!(Y2))],
        );
        assert_bindings(
            expr!(f(X0)),
            expr!(f(g(Y0, Y1))),
            vec![("X0", expr!(g(Y0, Y1)))],
        );

        assert_no_bindings(expr!(f(g(X0))), expr!(f(g(Y0, Y1))));
    }

    fn assert_bindings(pattern: Expr, with: Expr, expected_bindings: Vec<(&str, Expr)>) {
        let expected_bindings = expected_bindings
            .into_iter()
            .map(|(name, ex)| (name.to_string(), ex))
            .collect::<HashMap<String, Expr>>();

        let actual_bindings = pattern.pattern_match(&with).unwrap();
        assert_eq!(expected_bindings, actual_bindings)
    }

    fn assert_no_bindings(pattern: Expr, with: Expr) {
        assert_eq!(Option::None, pattern.pattern_match(&with));
    }
}
