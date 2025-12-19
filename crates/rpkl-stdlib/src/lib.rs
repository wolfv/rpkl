//! PKL Standard Library
//!
//! This crate provides the standard library implementations for PKL,
//! including external functions for built-in types.

// Allow Arc with non-Send/Sync types - this is a single-threaded runtime
#![allow(clippy::arc_with_non_send_sync)]

mod base;
mod collections;
mod math;
mod string;

use rpkl_runtime::ExternalRegistry;

/// Register all standard library external functions
pub fn register_stdlib(registry: &mut ExternalRegistry) {
    base::register(registry);
    string::register(registry);
    collections::register(registry);
    math::register(registry);
}

/// Create an external registry with all stdlib functions registered
pub fn stdlib_registry() -> ExternalRegistry {
    let mut registry = ExternalRegistry::new();
    register_stdlib(&mut registry);
    registry
}
