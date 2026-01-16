//! PklProject.deps.json parsing
//!
//! This file contains resolved dependencies for a PKL project.

use std::collections::HashMap;
use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::error::{EvalError, EvalResult};

use super::Checksums;

/// Schema version for PklProject.deps.json
const SCHEMA_VERSION: u32 = 1;

/// Resolved project dependencies from PklProject.deps.json
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ProjectDeps {
    /// Schema version
    pub schema_version: u32,

    /// Map of canonical package URI to resolved dependency
    pub resolved_dependencies: HashMap<String, ResolvedDependency>,
}

/// A resolved dependency entry
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum ResolvedDependency {
    /// A remote dependency (downloaded from a package server)
    #[serde(rename = "remote")]
    Remote {
        /// The resolved projectpackage:// URI
        uri: String,
        /// Checksums for verification
        checksums: Checksums,
    },

    /// A local dependency (relative path to another project)
    #[serde(rename = "local")]
    Local {
        /// The resolved projectpackage:// URI
        uri: String,
        /// Relative path to the local project
        path: String,
    },
}

impl ResolvedDependency {
    /// Get the resolved URI
    pub fn uri(&self) -> &str {
        match self {
            ResolvedDependency::Remote { uri, .. } => uri,
            ResolvedDependency::Local { uri, .. } => uri,
        }
    }

    /// Check if this is a remote dependency
    pub fn is_remote(&self) -> bool {
        matches!(self, ResolvedDependency::Remote { .. })
    }

    /// Get checksums if this is a remote dependency
    pub fn checksums(&self) -> Option<&Checksums> {
        match self {
            ResolvedDependency::Remote { checksums, .. } => Some(checksums),
            ResolvedDependency::Local { .. } => None,
        }
    }
}

impl ProjectDeps {
    /// Parse from JSON string
    pub fn from_json(json: &str) -> EvalResult<Self> {
        let deps: ProjectDeps = serde_json::from_str(json).map_err(|e| {
            EvalError::IoError(format!("Failed to parse PklProject.deps.json: {}", e))
        })?;

        if deps.schema_version != SCHEMA_VERSION {
            return Err(EvalError::IoError(format!(
                "Unsupported PklProject.deps.json schema version: {}. Expected: {}",
                deps.schema_version, SCHEMA_VERSION
            )));
        }

        Ok(deps)
    }

    /// Load from a file path
    pub fn load(path: &Path) -> EvalResult<Self> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| EvalError::IoError(format!("Failed to read {}: {}", path.display(), e)))?;
        Self::from_json(&content)
    }

    /// Find the deps.json file starting from a given directory
    ///
    /// Searches upward through parent directories for PklProject.deps.json
    pub fn find_and_load(start_dir: &Path) -> EvalResult<Option<(Self, std::path::PathBuf)>> {
        let mut current = start_dir.to_path_buf();

        loop {
            let deps_file = current.join("PklProject.deps.json");
            if deps_file.exists() {
                let deps = Self::load(&deps_file)?;
                return Ok(Some((deps, deps_file)));
            }

            if !current.pop() {
                break;
            }
        }

        Ok(None)
    }

    /// Resolve a canonical package URI to its full resolved URI
    ///
    /// The canonical URI is in the format: `package://authority/path@majorVersion`
    pub fn resolve(&self, canonical_uri: &str) -> Option<&ResolvedDependency> {
        self.resolved_dependencies.get(canonical_uri)
    }

    /// Serialize to JSON
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_project_deps() {
        let json = r#"{
            "schemaVersion": 1,
            "resolvedDependencies": {
                "package://pkg.pkl-lang.org/pkl-pantry/pkl.toml@1": {
                    "type": "remote",
                    "uri": "projectpackage://pkg.pkl-lang.org/pkl-pantry/pkl.toml@1.0.0",
                    "checksums": {
                        "sha256": "abc123"
                    }
                },
                "package://example.com/local-pkg@2": {
                    "type": "local",
                    "uri": "projectpackage://example.com/local-pkg@2.1.0",
                    "path": "../local-pkg"
                }
            }
        }"#;

        let deps = ProjectDeps::from_json(json).unwrap();
        assert_eq!(deps.schema_version, 1);
        assert_eq!(deps.resolved_dependencies.len(), 2);

        let remote = deps
            .resolve("package://pkg.pkl-lang.org/pkl-pantry/pkl.toml@1")
            .unwrap();
        assert!(remote.is_remote());

        let local = deps.resolve("package://example.com/local-pkg@2").unwrap();
        assert!(!local.is_remote());
    }
}
