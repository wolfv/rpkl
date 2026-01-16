//! Package dependency metadata
//!
//! Represents the metadata JSON fetched from a package server.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use super::Checksums;

/// Package metadata fetched from the package server
///
/// This is the JSON structure returned when fetching a package's metadata URL.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DependencyMetadata {
    /// Package name
    pub name: String,

    /// Full package URI
    pub package_uri: String,

    /// Package version
    pub version: String,

    /// URL to download the package ZIP
    pub package_zip_url: String,

    /// Checksums for the package ZIP
    #[serde(default)]
    pub package_zip_checksums: Option<Checksums>,

    /// Dependencies of this package
    #[serde(default)]
    pub dependencies: HashMap<String, PackageDependency>,

    /// Source code URL scheme for documentation links
    #[serde(default)]
    pub source_code_url_scheme: Option<String>,

    /// Source code repository URL
    #[serde(default)]
    pub source_code: Option<String>,

    /// Documentation URL
    #[serde(default)]
    pub documentation: Option<String>,

    /// License identifier (e.g., "Apache-2.0")
    #[serde(default)]
    pub license: Option<String>,

    /// Package authors
    #[serde(default)]
    pub authors: Vec<String>,

    /// Issue tracker URL
    #[serde(default)]
    pub issue_tracker: Option<String>,

    /// Package description
    #[serde(default)]
    pub description: Option<String>,
}

/// A dependency reference in package metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageDependency {
    /// The package URI of the dependency
    pub uri: String,

    /// Checksums for verifying the dependency
    #[serde(default)]
    pub checksums: Option<Checksums>,
}

impl DependencyMetadata {
    /// Parse metadata from JSON
    pub fn from_json(json: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(json)
    }

    /// Serialize metadata to JSON
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_metadata() {
        let json = r#"{
            "name": "pkl.toml",
            "packageUri": "package://pkg.pkl-lang.org/pkl-pantry/pkl.toml@1.0.0",
            "version": "1.0.0",
            "packageZipUrl": "https://pkg.pkl-lang.org/pkl-pantry/pkl.toml@1.0.0/pkl.toml@1.0.0.zip",
            "packageZipChecksums": {
                "sha256": "abc123"
            },
            "dependencies": {
                "base": {
                    "uri": "package://pkg.pkl-lang.org/pkl-pantry/base@1.0.0",
                    "checksums": {
                        "sha256": "def456"
                    }
                }
            },
            "license": "Apache-2.0",
            "authors": ["Apple Inc."],
            "description": "TOML support for PKL"
        }"#;

        let metadata = DependencyMetadata::from_json(json).unwrap();
        assert_eq!(metadata.name, "pkl.toml");
        assert_eq!(metadata.version, "1.0.0");
        assert_eq!(metadata.dependencies.len(), 1);
        assert!(metadata.dependencies.contains_key("base"));
    }
}
