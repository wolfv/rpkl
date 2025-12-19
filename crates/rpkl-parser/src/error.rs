//! Parser error types

use pest::error::Error as PestError;
use thiserror::Error;

use crate::parser::Rule;

/// Result type for parse operations
pub type ParseResult<T> = Result<T, ParseError>;

/// Parse error type
#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Syntax error: {0}")]
    Syntax(#[from] Box<PestError<Rule>>),

    #[error("Invalid number literal: {0}")]
    InvalidNumber(String),

    #[error("Invalid escape sequence: {0}")]
    InvalidEscape(String),

    #[error("Unknown operator: {0}")]
    UnknownOperator(String),

    #[error("Unexpected token: {0}")]
    UnexpectedToken(String),
}

impl From<PestError<Rule>> for ParseError {
    fn from(err: PestError<Rule>) -> Self {
        ParseError::Syntax(Box::new(err))
    }
}
