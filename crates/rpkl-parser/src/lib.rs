//! PKL Parser
//!
//! This crate provides a parser for the PKL configuration language.
//! It uses pest for parsing and produces an AST representation.

pub mod ast;
pub mod error;
pub mod parser;

pub use ast::*;
pub use error::{ParseError, ParseResult};
pub use parser::{parse_expression, parse_module};
