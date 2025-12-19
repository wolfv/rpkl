//! PKL Runtime
//!
//! This crate provides the runtime and evaluation engine for PKL.

// Allow Arc with non-Send/Sync types - this is a single-threaded runtime
// that uses Arc for shared ownership, not for cross-thread sharing.
#![allow(clippy::arc_with_non_send_sync)]

pub mod error;
pub mod evaluator;
pub mod loader;
pub mod object;
pub mod scope;
pub mod value;

pub use error::{EvalError, EvalResult};
pub use evaluator::{Evaluator, ExternalFn, ExternalRegistry};
pub use loader::{LoadedModule, ModuleLoader};
pub use object::{ObjectKind, ObjectMember, VmObject};
pub use scope::{Scope, ScopeRef};
pub use value::{DataSizeUnit, DurationUnit, LambdaClosure, VmValue};
