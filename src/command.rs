use std::fs;
use std::io;
use std::io::Write;

use super::engine::diagnostics::*;
use super::engine::expr::*;
use super::engine::lexer::*;
use super::engine::rule::*;

#[derive(Clone)]
pub enum AppliedRule {
    ByName { name: Token, reversed: bool },
    Anonymous { head: Expr, body: Expr },
}

#[derive(Clone)]
pub enum Command {
    DefineRule {
        name: Token,
        rule: Rule,
    },
    DefineRuleViaShaping {
        name: Token,
        expr: Expr,
    },
    StartShaping {
        expr: Expr,
    },
    ApplyRule {
        bar: Token,
        strategy_name: Token,
        applied_rule: AppliedRule,
    },
    FinishShaping {
        token: Token,
    },
    UndoRule(Loc),
    Quit,
    DeleteRule(Loc, Token),
    Load(Token),
    Save(Token),
    List,
    Show {
        name: Token,
    },
    History {
        keyword: Token,
    },
}

impl Command {
    pub fn parse(lexer: &mut Lexer, diag: &mut impl Diagnoster) -> Option<Command> {
        let keyword_kind = lexer.peek_token().kind;
        let keyword_loc = lexer.peek_token().loc.clone();
        match keyword_kind {
            TokenKind::Load => {
                lexer.next_token();
                let token = lexer.expect_token(TokenKind::Str).map_err(|(expected_kind, actual_token)| {
                    diag.report(&actual_token.loc, Severity::Error, &format!("`load` command expects {expected_kind} as the file path, but got {actual_token} instead", actual_token = actual_token.report()));
                }).ok()?;
                Some(Self::Load(token))
            }
            TokenKind::Save => {
                lexer.next_token();
                let token = lexer.expect_token(TokenKind::Str).map_err(|(expected_kind, actual_token)| {
                    diag.report(&actual_token.loc, Severity::Error, &format!("`save` command expects {expected_kind} as the file path, but got {actual_token} instead", actual_token = actual_token.report()));
                }).ok()?;
                Some(Self::Save(token))
            }
            TokenKind::CloseCurly => {
                let token = lexer.next_token();
                Some(Command::FinishShaping { token })
            }
            TokenKind::Undo => {
                let keyword = lexer.next_token();
                Some(Command::UndoRule(keyword.loc))
            }
            TokenKind::Quit => {
                lexer.next_token();
                Some(Command::Quit)
            }
            TokenKind::List => {
                lexer.next_token();
                Some(Command::List)
            }
            TokenKind::History => {
                let keyword = lexer.next_token();
                Some(Command::History { keyword })
            }
            TokenKind::Show => {
                lexer.next_token();
                let name = lexer.expect_token(TokenKind::Ident).map_err(|(expected_kind, actual_token)| {
                    diag.report(&actual_token.loc, Severity::Error, &format!("`show` command expects {expected_kind} as the rule name, but got {actual_token} instead", actual_token = actual_token.report()));
                }).ok()?;
                Some(Command::Show { name })
            }
            TokenKind::Delete => {
                let keyword = lexer.next_token();
                let name = lexer.expect_token(TokenKind::Ident).map_err(|(expected_kind, actual_token)| {
                    diag.report(&actual_token.loc, Severity::Error, &format!("`delete` command expects {expected_kind} as an argument but got {actual_token} instead", actual_token = actual_token.report()));
                }).ok()?;
                Some(Command::DeleteRule(keyword.loc, name))
            }
            _ => {
                let expr = Expr::parse(lexer, diag)?;

                fn report_unexpected_token_for_strategy_name(
                    diag: &mut impl Diagnoster,
                    expected_kind: &TokenKind,
                    actual_token: &Token,
                ) {
                    diag.report(&actual_token.loc, Severity::Error, &format!("applied strategy name must be {expected_kind}, but we got {actual_token} instead", actual_token = actual_token.report()));
                }

                match lexer.peek_token().kind {
                    TokenKind::Bar => {
                        let bar = lexer.next_token();
                        let (reversed, strategy_name) = {
                            let token = lexer.next_token();
                            if token.kind == TokenKind::Bang {
                                (
                                    true,
                                    lexer
                                        .expect_token(TokenKind::Ident)
                                        .map_err(|(expected_kind, actual_token)| {
                                            report_unexpected_token_for_strategy_name(
                                                diag,
                                                &expected_kind,
                                                &actual_token,
                                            )
                                        })
                                        .ok()?,
                                )
                            } else if token.kind == TokenKind::Ident {
                                (false, token)
                            } else {
                                report_unexpected_token_for_strategy_name(
                                    diag,
                                    &TokenKind::Ident,
                                    &token,
                                );
                                return None;
                            }
                        };
                        if let Expr::Sym(name) = expr {
                            Some(Command::ApplyRule {
                                bar,
                                strategy_name,
                                applied_rule: AppliedRule::ByName { name, reversed },
                            })
                        } else {
                            diag.report(
                                &keyword_loc,
                                Severity::Error,
                                &format!(
                                    "Applied rule must be a symbol but got {} instead",
                                    expr.human_name()
                                ),
                            );
                            return None;
                        }
                    }
                    TokenKind::Equals => {
                        let head = expr;
                        let _equals = lexer.next_token();
                        let body = Expr::parse(lexer, diag)?;
                        let bar = lexer.expect_token(TokenKind::Bar).map_err(|(expected_kind, actual_token)| {
                            diag.report(&actual_token.loc, Severity::Error, &format!("Expected {expected_kind} since you defined an annonymous rule `{head} = {body}`, which must be applied in-place. But instead of {expected_kind} we got {actual_token}", actual_token = actual_token.report()))
                        }).ok()?;
                        let (reversed, strategy_name) = {
                            let token = lexer.next_token();
                            if token.kind == TokenKind::Bang {
                                (
                                    true,
                                    lexer
                                        .expect_token(TokenKind::Ident)
                                        .map_err(|(expected_kind, actual_token)| {
                                            report_unexpected_token_for_strategy_name(
                                                diag,
                                                &expected_kind,
                                                &actual_token,
                                            )
                                        })
                                        .ok()?,
                                )
                            } else if token.kind == TokenKind::Ident {
                                (false, token)
                            } else {
                                report_unexpected_token_for_strategy_name(
                                    diag,
                                    &TokenKind::Ident,
                                    &token,
                                );
                                return None;
                            }
                        };
                        Some(Command::ApplyRule {
                            bar,
                            strategy_name,
                            applied_rule: if reversed {
                                AppliedRule::Anonymous {
                                    head: body,
                                    body: head,
                                }
                            } else {
                                AppliedRule::Anonymous { head, body }
                            },
                        })
                    }
                    TokenKind::OpenCurly => {
                        lexer.next_token();
                        Some(Command::StartShaping { expr })
                    }
                    TokenKind::DoubleColon => {
                        let keyword = lexer.next_token();
                        match expr {
                            Expr::Sym(name) => {
                                let head = Expr::parse(lexer, diag)?;
                                match lexer.peek_token().kind {
                                    TokenKind::OpenCurly => {
                                        lexer.next_token();
                                        Some(Command::DefineRuleViaShaping { name, expr: head })
                                    }
                                    TokenKind::Equals => {
                                        lexer.next_token();
                                        let body = Expr::parse(lexer, diag)?;
                                        Some(Command::DefineRule {
                                            name,
                                            rule: Rule::User { head, body },
                                        })
                                    }
                                    _ => {
                                        let token = lexer.next_token();
                                        diag.report(
                                            &token.loc,
                                            Severity::Error,
                                            &format!(
                                                "unexpected Rule Definition Separator {}",
                                                token.report()
                                            ),
                                        );
                                        None
                                    }
                                }
                            }
                            _ => {
                                diag.report(
                                    &keyword.loc,
                                    Severity::Error,
                                    &format!("expected symbol"),
                                );
                                None
                            }
                        }
                    }
                    _ => {
                        let token = lexer.next_token();
                        diag.report(
                            &token.loc,
                            Severity::Error,
                            "It's unclear what you want in here",
                        );
                        diag.report(
                            &token.loc,
                            Severity::Info,
                            &format!("{expr} {{                     - to start shaping {expr}"),
                        );
                        diag.report(&token.loc, Severity::Info, &format!("{expr} = <body> | <strategy> - to use {expr} as a head of an anonymous rule to apply to the currently shaping expression"));
                        if let Expr::Sym(_) = expr {
                            diag.report(&token.loc, Severity::Info, &format!("{expr} | <strategy>          - to apply rule {expr} to the currently shaping expression"));
                            diag.report(&token.loc, Severity::Info, &format!("{expr} :: <head> = <body>    - to define new rule with the name {expr}"));
                        }
                        None
                    }
                }
            }
        }
    }
}

pub struct ShapingFrame {
    pub expr: Expr,
    history: Vec<(Expr, Command)>,
    rule_via_shaping: Option<(Token, Expr)>,
}

impl ShapingFrame {
    fn new(expr: Expr) -> Self {
        Self {
            expr,
            history: Vec::new(),
            rule_via_shaping: None,
        }
    }

    fn new_rule_via_shaping(name: Token, head: Expr) -> Self {
        Self {
            expr: head.clone(),
            history: Vec::new(),
            rule_via_shaping: Some((name, head)),
        }
    }
}

struct RuleDefinition {
    rule: Rule,
    history: Vec<(Expr, Command)>,
}

pub struct Context {
    interactive: bool,
    rules: Vec<(Token, RuleDefinition)>,
    pub shaping_stack: Vec<ShapingFrame>,
    pub quit: bool,
}

fn get_item_by_key<'a, K, V>(assoc: &'a [(K, V)], needle: &'a K) -> Option<&'a (K, V)>
where
    K: PartialEq<K>,
{
    assoc.iter().find(|(key, _)| key == needle)
}

fn delete_item_by_key<'a, K, V>(assoc: &'a mut Vec<(K, V)>, needle: &'a K) -> bool
where
    K: PartialEq<K>,
{
    for i in 0..assoc.len() {
        if &assoc[i].0 == needle {
            assoc.remove(i);
            return true;
        }
    }
    false
}

impl Context {
    pub fn new(interactive: bool) -> Self {
        let mut rules = Vec::new();
        rules.push((
            Token {
                kind: TokenKind::Ident,
                text: "replace".to_string(),
                loc: loc_here!(),
            },
            RuleDefinition {
                rule: Rule::Replace,
                history: vec![],
            },
        ));
        Self {
            interactive,
            rules,
            shaping_stack: Default::default(),
            quit: false,
        }
    }

    fn save_rules_to_file(&self, file_path: &str) -> Result<(), io::Error> {
        let mut sink = fs::File::create(file_path)?;
        for (name, RuleDefinition { rule, history }) in self.rules.iter() {
            match rule {
                Rule::User { head, body, .. } => {
                    write!(sink, "{name} :: {head}", name = name.text)?;
                    if history.len() > 0 {
                        writeln!(sink, " {{")?;
                        for (_, command) in history {
                            match command {
                                Command::ApplyRule {
                                    strategy_name,
                                    applied_rule,
                                    ..
                                } => match applied_rule {
                                    AppliedRule::ByName { name, reversed } => {
                                        write!(sink, "    {name} |", name = name.text)?;
                                        if *reversed {
                                            write!(sink, "!")?;
                                        }
                                        writeln!(
                                            sink,
                                            " {strategy_name}",
                                            strategy_name = strategy_name.text
                                        )?;
                                    }
                                    AppliedRule::Anonymous { head, body, .. } => {
                                        writeln!(
                                            sink,
                                            "    {head} = {body} | {strategy_name}",
                                            strategy_name = strategy_name.text
                                        )?;
                                    }
                                },
                                _ => unreachable!(),
                            }
                        }
                        writeln!(sink, "}}")?;
                    } else {
                        writeln!(sink, " = {body}")?;
                    }
                }
                Rule::Replace => {}
            }
        }
        Ok(())
    }

    fn load_source(&self, loc: Loc, file_path: &str, diag: &mut impl Diagnoster) -> Option<String> {
        match fs::read_to_string(file_path) {
            Ok(source) => Some(source),
            Err(err) => {
                diag.report(
                    &loc,
                    Severity::Error,
                    &format!("could not load file {}: {}", file_path, err),
                );
                None
            }
        }
    }

    fn process_file(&mut self, file_path: Token, diag: &mut impl Diagnoster) -> Option<()> {
        let source = if file_path.text.starts_with("std/") {
            match std::env::var("RET_STD_PATH") {
                Ok(std_path) => self.load_source(
                    file_path.loc,
                    &(std_path + file_path.text.strip_prefix("std/").unwrap()),
                    diag,
                ),
                Err(_) => self.load_source(file_path.loc, &file_path.text, diag),
            }
        } else {
            self.load_source(file_path.loc, &file_path.text, diag)
        };
        if source.is_none() {
            return None;
        }
        let mut lexer = Lexer::new(source?.chars().collect(), Some(file_path.text));
        while lexer.peek_token().kind != TokenKind::End {
            self.process_command(Command::parse(&mut lexer, diag)?, diag)?
        }
        Some(())
    }

    pub fn process_command(&mut self, command: Command, diag: &mut impl Diagnoster) -> Option<()> {
        match command.clone() {
            Command::Load(file_path) => {
                let saved_interactive = self.interactive;
                self.interactive = false;
                self.process_file(file_path, diag)?;
                self.interactive = saved_interactive;
            }
            Command::DefineRule { name, rule } => {
                if let Some((existing_name, _)) = get_item_by_key(&self.rules, &name) {
                    diag.report(
                        &name.loc,
                        Severity::Error,
                        &format!("redefinition of existing rule {}", name.text),
                    );
                    diag.report(
                        &existing_name.loc,
                        Severity::Info,
                        &format!("the original definition is located here"),
                    );
                    return None;
                }
                if let Rule::User { head, body, .. } = &rule {
                    diag.report(
                        &name.loc,
                        Severity::Info,
                        &format!("defined rule {name} :: {head} = {body}", name = &name.text),
                    );
                } else {
                    unreachable!("Users can only define Rule::User rules");
                }
                self.rules.push((
                    name,
                    RuleDefinition {
                        rule,
                        history: vec![],
                    },
                ));
            }
            Command::DefineRuleViaShaping { name, expr, .. } => {
                println!(" => {}", &expr);
                self.shaping_stack
                    .push(ShapingFrame::new_rule_via_shaping(name, expr))
            }
            Command::StartShaping { expr } => {
                println!(" => {}", &expr);
                self.shaping_stack.push(ShapingFrame::new(expr))
            }
            Command::ApplyRule {
                bar,
                strategy_name,
                applied_rule,
            } => {
                if let Some(frame) = self.shaping_stack.last_mut() {
                    let rule = match applied_rule {
                        AppliedRule::ByName { name, reversed } => {
                            match get_item_by_key(&self.rules, &name) {
                                Some((_, RuleDefinition { rule, .. })) => {
                                    if reversed {
                                        match rule.clone() {
                                            Rule::User { head, body } => Rule::User {
                                                head: body,
                                                body: head,
                                            },
                                            Rule::Replace => {
                                                diag.report(
                                                    &bar.loc,
                                                    Severity::Error,
                                                    &format!("irreversible rule"),
                                                );
                                                return None;
                                            }
                                        }
                                    } else {
                                        rule.clone()
                                    }
                                }

                                None => {
                                    diag.report(
                                        &bar.loc,
                                        Severity::Error,
                                        &format!("rule {} does not exist", name.text),
                                    );
                                    return None;
                                }
                            }
                        }
                        AppliedRule::Anonymous { head, body } => Rule::User { head, body },
                    };

                    let previous_expr = frame.expr.clone();
                    match Strategy::by_name(&strategy_name.text) {
                        Some(strategy) => rule.apply(&mut frame.expr, &strategy, &bar.loc, diag)?,
                        None => {
                            diag.report(
                                &bar.loc,
                                Severity::Error,
                                &format!(
                                    "unknown rule application strategy '{}'",
                                    strategy_name.text
                                ),
                            );
                            return None;
                        }
                    };
                    println!(" => {}", &frame.expr);
                    frame.history.push((previous_expr, command));
                } else {
                    diag.report(&bar.loc, Severity::Error, &format!("To apply a rule to an expression you need to first start shaping the expression, but no shaping is currently in place"));
                    diag.report(
                        &bar.loc,
                        Severity::Info,
                        &format!("<expression> {{    - to start shaping"),
                    );
                    return None;
                }
            }
            Command::FinishShaping { token } => {
                if let Some(mut frame) = self.shaping_stack.pop() {
                    let body = frame.expr;
                    if let Some((name, head)) = frame.rule_via_shaping.take() {
                        if let Some((existing_name, _)) = get_item_by_key(&self.rules, &name) {
                            diag.report(
                                &token.loc,
                                Severity::Error,
                                &format!("redefinition of existing rule {}", &name.text),
                            );
                            diag.report(
                                &existing_name.loc,
                                Severity::Info,
                                &format!("the original definition is located here"),
                            );
                            return None;
                        }
                        diag.report(
                            &name.loc,
                            Severity::Info,
                            &format!("defined rule {} :: {head} = {body}", &name.text),
                        );
                        self.rules.push((
                            name,
                            RuleDefinition {
                                rule: Rule::User { head, body },
                                history: frame.history,
                            },
                        ));
                    }
                } else {
                    diag.report(&token.loc, Severity::Error, "no shaping in place");
                    return None;
                }
            }
            Command::UndoRule(loc) => {
                if let Some(frame) = self.shaping_stack.last_mut() {
                    if let Some((previous_expr, _)) = frame.history.pop() {
                        println!(" => {}", &previous_expr);
                        frame.expr = previous_expr;
                    } else {
                        diag.report(&loc, Severity::Error, "end of history");
                        return None;
                    }
                } else {
                    diag.report(&loc, Severity::Error, "no shaping in place");
                    return None;
                }
            }
            Command::Quit => {
                self.quit = true;
            }
            Command::List => {
                for (name, rule) in self.rules.iter() {
                    if let RuleDefinition {
                        rule: Rule::User { head, body },
                        ..
                    } = rule
                    {
                        println!("{name} :: {head} = {body}", name = name.text)
                    }
                }
            }
            Command::History { keyword } => {
                if let Some(frame) = self.shaping_stack.last() {
                    for (_, command) in frame.history.iter() {
                        match command {
                            Command::ApplyRule {
                                strategy_name,
                                applied_rule,
                                ..
                            } => match applied_rule {
                                AppliedRule::ByName { name, reversed } => {
                                    print!("    {name} |", name = name.text);
                                    if *reversed {
                                        print!("!");
                                    }
                                    println!(
                                        " {strategy_name}",
                                        strategy_name = strategy_name.text
                                    );
                                }
                                AppliedRule::Anonymous { head, body, .. } => {
                                    println!(
                                        "    {head} = {body} | {strategy_name}",
                                        strategy_name = strategy_name.text
                                    );
                                }
                            },
                            _ => unreachable!(),
                        }
                    }
                } else {
                    diag.report(&keyword.loc, Severity::Error, "no shaping in place");
                    return None;
                }
            }
            Command::Show { name } => match get_item_by_key(&self.rules, &name) {
                Some((
                    _,
                    RuleDefinition {
                        rule: Rule::User { head, body },
                        history,
                    },
                )) => {
                    print!("{name} :: {head}", name = name.text);
                    if history.len() > 0 {
                        println!(" {{");
                        for (_, command) in history {
                            match command {
                                Command::ApplyRule {
                                    strategy_name,
                                    applied_rule,
                                    ..
                                } => match applied_rule {
                                    AppliedRule::ByName { name, reversed } => {
                                        print!("    {name} |", name = name.text);
                                        if *reversed {
                                            print!("!");
                                        }
                                        println!(
                                            " {strategy_name}",
                                            strategy_name = strategy_name.text
                                        );
                                    }
                                    AppliedRule::Anonymous { head, body, .. } => {
                                        println!(
                                            "    {head} = {body} | {strategy_name}",
                                            strategy_name = strategy_name.text
                                        );
                                    }
                                },
                                _ => unreachable!(),
                            }
                        }
                        println!("}}");
                    } else {
                        println!(" = {body}");
                    }
                }
                Some((
                    _,
                    RuleDefinition {
                        rule: Rule::Replace,
                        ..
                    },
                )) => {
                    println!("`replace` is a built-in rule");
                }
                None => {
                    diag.report(
                        &name.loc,
                        Severity::Error,
                        &format!("rule `{}` does not exist", name.text),
                    );
                    return None;
                }
            },
            Command::DeleteRule(loc, name) => {
                if delete_item_by_key(&mut self.rules, &name) {
                    diag.report(
                        &loc,
                        Severity::Info,
                        &format!("rule `{}` has been removed", name.text),
                    );
                } else {
                    diag.report(
                        &loc,
                        Severity::Error,
                        &format!("rule `{}` does not exist", name.text),
                    );
                    return None;
                }
            }
            Command::Save(file_path) => {
                if let Err(err) = self.save_rules_to_file(&file_path.text) {
                    diag.report(
                        &file_path.loc,
                        Severity::Error,
                        &format!("could not save file {}: {}", file_path.text, err),
                    );
                    return None;
                }
            }
        }
        Some(())
    }
}
