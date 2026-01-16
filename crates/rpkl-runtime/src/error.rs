//! Runtime error types

use thiserror::Error;

/// Result type for evaluation operations
pub type EvalResult<T> = Result<T, EvalError>;

/// Evaluation error
#[derive(Debug, Error)]
pub enum EvalError {
    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("Undefined property: {0}")]
    UndefinedProperty(String),

    #[error("Undefined method: {0}.{1}")]
    UndefinedMethod(String, String),

    #[error("Type error: expected {expected}, got {actual}")]
    TypeError { expected: String, actual: String },

    #[error("Cannot call non-function value of type {0}")]
    NotCallable(String),

    #[error("Wrong number of arguments: expected {expected}, got {actual}")]
    WrongArgCount { expected: usize, actual: usize },

    #[error("Circular reference detected")]
    CircularReference,

    #[error("Index out of bounds: {index} (length: {length})")]
    IndexOutOfBounds { index: i64, length: usize },

    #[error("Division by zero")]
    DivisionByZero,

    #[error("Cannot subscript value of type {0}")]
    NotSubscriptable(String),

    #[error("Cannot iterate over value of type {0}")]
    NotIterable(String),

    #[error("Null pointer exception: {0}")]
    NullPointer(String),

    #[error("Assertion failed: {0}")]
    AssertionFailed(String),

    #[error("User exception: {0}")]
    UserException(String),

    #[error("IO error: {0}")]
    IoError(String),

    #[error("Parse error: {0}")]
    ParseError(String),

    #[error("Module not found: {0}")]
    ModuleNotFound(String),

    #[error("Invalid operation: {0}")]
    InvalidOperation(String),

    #[error("Type constraint violated: {0}")]
    ConstraintViolation(String),

    #[error("Stack overflow")]
    StackOverflow,

    #[error("Type cast failed: cannot cast {actual} to {target}")]
    TypeCastFailed { actual: String, target: String },

    #[error("Invalid package URI: {0}")]
    InvalidPackageUri(String),

    #[error("Package not found: {0}")]
    PackageNotFound(String),
}

impl EvalError {
    /// Create a type error
    pub fn type_error(expected: impl Into<String>, actual: impl Into<String>) -> Self {
        EvalError::TypeError {
            expected: expected.into(),
            actual: actual.into(),
        }
    }

    /// Create an undefined variable error
    pub fn undefined_var(name: impl Into<String>) -> Self {
        EvalError::UndefinedVariable(name.into())
    }

    /// Create an undefined property error
    pub fn undefined_prop(name: impl Into<String>) -> Self {
        EvalError::UndefinedProperty(name.into())
    }
}
