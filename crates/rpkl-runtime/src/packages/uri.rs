//! Package URI parsing
//!
//! Handles `package://` and `projectpackage://` URIs.
//! Format: `package://authority/path@version[#/asset/path]`

use std::fmt;

use crate::error::{EvalError, EvalResult};

use super::Checksums;

/// A package URI representing a versioned package
///
/// Format: `package://authority/path@version`
/// Example: `package://pkg.pkl-lang.org/pkl-pantry/pkl.toml@1.0.0`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageUri {
    /// The URI scheme (package or projectpackage)
    pub scheme: PackageScheme,
    /// The authority (host and optional port)
    pub authority: String,
    /// The package path (without version)
    pub path: String,
    /// The package version
    pub version: String,
    /// Optional checksums (appended with ::)
    pub checksums: Option<Checksums>,
}

/// Package URI scheme
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PackageScheme {
    /// Standard package URI
    Package,
    /// Project-local package URI (resolved via PklProject.deps.json)
    ProjectPackage,
}

impl fmt::Display for PackageScheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PackageScheme::Package => write!(f, "package"),
            PackageScheme::ProjectPackage => write!(f, "projectpackage"),
        }
    }
}

impl PackageUri {
    /// Parse a package URI string
    ///
    /// Format: `package://authority/path@version[::checksums]`
    pub fn parse(uri: &str) -> EvalResult<Self> {
        // Determine scheme
        let (scheme, rest) = if let Some(rest) = uri.strip_prefix("package://") {
            (PackageScheme::Package, rest)
        } else if let Some(rest) = uri.strip_prefix("projectpackage://") {
            (PackageScheme::ProjectPackage, rest)
        } else {
            return Err(EvalError::InvalidPackageUri(format!(
                "Invalid package URI scheme: {}",
                uri
            )));
        };

        // Split off checksums if present (::sha256:...)
        let (main_part, checksums) = if let Some(idx) = rest.find("::") {
            let checksum_part = &rest[idx + 2..];
            let checksums = Checksums::parse(checksum_part)?;
            (&rest[..idx], Some(checksums))
        } else {
            (rest, None)
        };

        // Split authority and path
        let slash_idx = main_part
            .find('/')
            .ok_or_else(|| EvalError::InvalidPackageUri(format!("Missing path in URI: {}", uri)))?;

        let authority = main_part[..slash_idx].to_string();
        let path_with_version = &main_part[slash_idx..];

        // Split path and version at @
        let at_idx = path_with_version.rfind('@').ok_or_else(|| {
            EvalError::InvalidPackageUri(format!("Missing version in URI: {}", uri))
        })?;

        let path = path_with_version[..at_idx].to_string();
        let version = path_with_version[at_idx + 1..].to_string();

        if authority.is_empty() {
            return Err(EvalError::InvalidPackageUri(format!(
                "Empty authority in URI: {}",
                uri
            )));
        }

        if path.is_empty() {
            return Err(EvalError::InvalidPackageUri(format!(
                "Empty path in URI: {}",
                uri
            )));
        }

        if version.is_empty() {
            return Err(EvalError::InvalidPackageUri(format!(
                "Empty version in URI: {}",
                uri
            )));
        }

        Ok(Self {
            scheme,
            authority,
            path,
            version,
            checksums,
        })
    }

    /// Get the HTTPS URL for fetching package metadata
    ///
    /// Transforms `package://authority/path@version` to `https://authority/path@version`
    pub fn metadata_url(&self) -> String {
        format!("https://{}{}", self.authority, self.path_with_version())
    }

    /// Get the path with version suffix
    pub fn path_with_version(&self) -> String {
        format!("{}@{}", self.path, self.version)
    }

    /// Get the canonical form (major version only, for dependency grouping)
    pub fn canonical(&self) -> CanonicalPackageUri {
        let major_version = self
            .version
            .split('.')
            .next()
            .unwrap_or(&self.version)
            .to_string();

        CanonicalPackageUri {
            authority: self.authority.clone(),
            path: self.path.clone(),
            major_version,
        }
    }

    /// Convert to a projectpackage:// URI
    pub fn to_project_package(&self) -> Self {
        Self {
            scheme: PackageScheme::ProjectPackage,
            ..self.clone()
        }
    }

    /// Get the cache directory path for this package
    pub fn cache_path(&self, cache_dir: &std::path::Path) -> std::path::PathBuf {
        // Encode authority and path for filesystem safety
        let encoded_authority = encode_path_component(&self.authority);
        let encoded_path = encode_path_component(&format!("{}@{}", self.path, self.version));

        cache_dir
            .join("package-2")
            .join(encoded_authority)
            .join(encoded_path)
    }
}

impl fmt::Display for PackageUri {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}://{}{}",
            self.scheme,
            self.authority,
            self.path_with_version()
        )?;
        if let Some(ref checksums) = self.checksums {
            write!(f, "::{}", checksums)?;
        }
        Ok(())
    }
}

/// Canonical package URI (for dependency grouping)
///
/// Uses only major version for grouping multiple versions of the same package.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CanonicalPackageUri {
    pub authority: String,
    pub path: String,
    pub major_version: String,
}

impl fmt::Display for CanonicalPackageUri {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "package://{}{}@{}",
            self.authority, self.path, self.major_version
        )
    }
}

/// A package asset URI (package URI with asset path)
///
/// Format: `package://authority/path@version#/asset/path`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageAssetUri {
    /// The base package URI
    pub package: PackageUri,
    /// The asset path within the package (without leading #)
    pub asset_path: String,
}

impl PackageAssetUri {
    /// Parse a package asset URI
    ///
    /// Format: `package://authority/path@version#/asset/path.pkl`
    pub fn parse(uri: &str) -> EvalResult<Self> {
        // Split at fragment
        let (package_part, asset_path) = if let Some(idx) = uri.find('#') {
            let asset = uri[idx + 1..].to_string();
            (&uri[..idx], asset)
        } else {
            return Err(EvalError::InvalidPackageUri(format!(
                "Package asset URI missing fragment: {}",
                uri
            )));
        };

        let package = PackageUri::parse(package_part)?;

        if asset_path.is_empty() {
            return Err(EvalError::InvalidPackageUri(format!(
                "Empty asset path in URI: {}",
                uri
            )));
        }

        Ok(Self {
            package,
            asset_path,
        })
    }

    /// Check if this is a package asset URI (has fragment)
    pub fn is_package_asset_uri(uri: &str) -> bool {
        (uri.starts_with("package://") || uri.starts_with("projectpackage://")) && uri.contains('#')
    }
}

impl fmt::Display for PackageAssetUri {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}#{}", self.package, self.asset_path)
    }
}

/// Encode a path component for safe filesystem storage
fn encode_path_component(s: &str) -> String {
    s.chars()
        .map(|c| match c {
            '/' => '%',
            ':' => '%',
            '@' => '%',
            c if c.is_alphanumeric() || c == '.' || c == '-' || c == '_' => c,
            _ => '%',
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_package_uri() {
        let uri =
            PackageUri::parse("package://pkg.pkl-lang.org/pkl-pantry/pkl.toml@1.0.0").unwrap();
        assert_eq!(uri.scheme, PackageScheme::Package);
        assert_eq!(uri.authority, "pkg.pkl-lang.org");
        assert_eq!(uri.path, "/pkl-pantry/pkl.toml");
        assert_eq!(uri.version, "1.0.0");
        assert!(uri.checksums.is_none());
    }

    #[test]
    fn test_parse_projectpackage_uri() {
        let uri = PackageUri::parse("projectpackage://example.com/my/pkg@2.0.0").unwrap();
        assert_eq!(uri.scheme, PackageScheme::ProjectPackage);
        assert_eq!(uri.authority, "example.com");
        assert_eq!(uri.path, "/my/pkg");
        assert_eq!(uri.version, "2.0.0");
    }

    #[test]
    fn test_metadata_url() {
        let uri =
            PackageUri::parse("package://pkg.pkl-lang.org/pkl-pantry/pkl.toml@1.0.0").unwrap();
        assert_eq!(
            uri.metadata_url(),
            "https://pkg.pkl-lang.org/pkl-pantry/pkl.toml@1.0.0"
        );
    }

    #[test]
    fn test_canonical() {
        let uri = PackageUri::parse("package://example.com/pkg@1.2.3").unwrap();
        let canonical = uri.canonical();
        assert_eq!(canonical.major_version, "1");
        assert_eq!(canonical.to_string(), "package://example.com/pkg@1");
    }

    #[test]
    fn test_parse_asset_uri() {
        let uri = PackageAssetUri::parse(
            "package://pkg.pkl-lang.org/pkl-pantry/pkl.toml@1.0.0#/PklProject.pkl",
        )
        .unwrap();
        assert_eq!(uri.package.version, "1.0.0");
        assert_eq!(uri.asset_path, "/PklProject.pkl");
    }
}
