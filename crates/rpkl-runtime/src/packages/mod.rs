//! Package management for PKL
//!
//! This module handles remote package resolution, downloading, and caching.
//! Supports `package://` and `projectpackage://` URI schemes.

mod checksums;
mod dependency;
mod http;
mod project_deps;
mod resolver;
mod uri;

pub use checksums::Checksums;
pub use dependency::DependencyMetadata;
pub use project_deps::ProjectDeps;
pub use resolver::{DiskCachedPackageResolver, PackageResolver};
pub use uri::{PackageAssetUri, PackageUri};
