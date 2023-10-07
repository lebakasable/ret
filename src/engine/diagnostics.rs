use super::lexer::Loc;

use std::fmt;

#[derive(Copy, Clone, PartialEq)]
pub enum Severity {
    Info,
    Error,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Info => write!(f, "INFO"),
            Self::Error => write!(f, "ERROR"),
        }
    }
}

pub trait Diagnoster {
    fn report(&mut self, loc: &Loc, severity: Severity, message: &str);
}

pub struct StdoutDiagnoster {}

impl Diagnoster for StdoutDiagnoster {
    fn report(&mut self, loc: &Loc, severity: Severity, message: &str) {
        match loc {
            Loc::File { path, row, col } => {
                eprintln!("{}:{}:{}: {}: {}", path, row, col, severity, message);
            }
            Loc::Repl { col, line } => {
                if severity == Severity::Error {
                    eprintln!("{}", line.iter().collect::<String>());
                    eprintln!("{:>width$}^", "", width = col);
                }
                eprintln!("{}: {}", severity, message);
            }
        }
    }
}
