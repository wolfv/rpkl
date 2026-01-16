//! Package resolution and caching

use std::collections::HashMap;
use std::fs::{self, File};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use zip::ZipArchive;

use crate::error::{EvalError, EvalResult};

use super::http::HttpClient;
use super::{Checksums, DependencyMetadata, PackageUri, ProjectDeps};

/// Package resolver interface
pub trait PackageResolver {
    /// Get the source code for a file within a package
    fn get_bytes(&self, package: &PackageUri, path: &str) -> EvalResult<Vec<u8>>;

    /// Get the source code as a string
    fn get_text(&self, package: &PackageUri, path: &str) -> EvalResult<String> {
        let bytes = self.get_bytes(package, path)?;
        String::from_utf8(bytes).map_err(|e| {
            EvalError::IoError(format!("Invalid UTF-8 in {}#{}: {}", package, path, e))
        })
    }

    /// Get package metadata
    fn get_metadata(&self, package: &PackageUri) -> EvalResult<DependencyMetadata>;

    /// Check if a file exists in a package
    fn has_file(&self, package: &PackageUri, path: &str) -> EvalResult<bool>;

    /// List files in a package directory
    fn list_files(&self, package: &PackageUri, dir: &str) -> EvalResult<Vec<String>>;
}

/// Disk-cached package resolver
///
/// Downloads packages from the network and caches them on disk.
pub struct DiskCachedPackageResolver {
    /// Cache directory
    cache_dir: PathBuf,
    /// HTTP client
    http: HttpClient,
    /// Project dependencies (if available)
    project_deps: Option<Arc<ProjectDeps>>,
    /// In-memory cache of opened ZIP archives
    #[allow(clippy::type_complexity)]
    zip_cache: std::cell::RefCell<HashMap<String, Arc<Vec<u8>>>>,
    /// In-memory cache of metadata
    metadata_cache: std::cell::RefCell<HashMap<String, DependencyMetadata>>,
}

impl DiskCachedPackageResolver {
    /// Create a new resolver with the default cache directory
    pub fn new() -> EvalResult<Self> {
        let cache_dir = default_cache_dir()?;
        Self::with_cache_dir(cache_dir)
    }

    /// Create a resolver with a specific cache directory
    pub fn with_cache_dir(cache_dir: PathBuf) -> EvalResult<Self> {
        fs::create_dir_all(&cache_dir).map_err(|e| {
            EvalError::IoError(format!(
                "Failed to create cache directory {}: {}",
                cache_dir.display(),
                e
            ))
        })?;

        Ok(Self {
            cache_dir,
            http: HttpClient::new()?,
            project_deps: None,
            zip_cache: std::cell::RefCell::new(HashMap::new()),
            metadata_cache: std::cell::RefCell::new(HashMap::new()),
        })
    }

    /// Set project dependencies for resolving projectpackage:// URIs
    pub fn with_project_deps(mut self, deps: Arc<ProjectDeps>) -> Self {
        self.project_deps = Some(deps);
        self
    }

    /// Get the cache directory
    pub fn cache_dir(&self) -> &Path {
        &self.cache_dir
    }

    /// Ensure a package is downloaded and cached
    fn ensure_cached(&self, package: &PackageUri) -> EvalResult<()> {
        let package_dir = package.cache_path(&self.cache_dir);
        let zip_path = package_dir.join("package.zip");
        let metadata_path = package_dir.join("package.json");

        // Already cached?
        if zip_path.exists() && metadata_path.exists() {
            return Ok(());
        }

        // Create directory
        fs::create_dir_all(&package_dir).map_err(|e| {
            EvalError::IoError(format!(
                "Failed to create package directory {}: {}",
                package_dir.display(),
                e
            ))
        })?;

        // Fetch metadata
        let metadata_url = package.metadata_url();
        let metadata_json = self.http.fetch_text(&metadata_url)?;
        let metadata = DependencyMetadata::from_json(&metadata_json).map_err(|e| {
            EvalError::IoError(format!(
                "Failed to parse package metadata from {}: {}",
                metadata_url, e
            ))
        })?;

        // Verify metadata checksum if provided
        if let Some(ref expected) = package.checksums {
            let computed = Checksums::compute(metadata_json.as_bytes());
            if !expected.verify_computed(&computed) {
                return Err(EvalError::IoError(format!(
                    "Checksum mismatch for package metadata {}. Expected: {}, Got: {}",
                    package, expected.sha256, computed.sha256
                )));
            }
        }

        // Fetch package ZIP
        let zip_bytes = self.http.fetch_bytes(&metadata.package_zip_url)?;

        // Verify ZIP checksum
        if let Some(ref expected) = metadata.package_zip_checksums {
            let computed = Checksums::compute(&zip_bytes);
            if !expected.verify_computed(&computed) {
                return Err(EvalError::IoError(format!(
                    "Checksum mismatch for package ZIP {}. Expected: {}, Got: {}",
                    package, expected.sha256, computed.sha256
                )));
            }
        }

        // Write files atomically (write to temp, then rename)
        let tmp_dir = self.cache_dir.join("tmp");
        fs::create_dir_all(&tmp_dir).ok();

        // Write metadata
        let tmp_metadata = tmp_dir.join(format!("{}.json", uuid_simple()));
        let mut f = File::create(&tmp_metadata)
            .map_err(|e| EvalError::IoError(format!("Failed to create temp file: {}", e)))?;
        f.write_all(metadata_json.as_bytes())
            .map_err(|e| EvalError::IoError(format!("Failed to write metadata: {}", e)))?;
        fs::rename(&tmp_metadata, &metadata_path)
            .map_err(|e| EvalError::IoError(format!("Failed to move metadata file: {}", e)))?;

        // Write ZIP
        let tmp_zip = tmp_dir.join(format!("{}.zip", uuid_simple()));
        let mut f = File::create(&tmp_zip)
            .map_err(|e| EvalError::IoError(format!("Failed to create temp file: {}", e)))?;
        f.write_all(&zip_bytes)
            .map_err(|e| EvalError::IoError(format!("Failed to write ZIP: {}", e)))?;
        fs::rename(&tmp_zip, &zip_path)
            .map_err(|e| EvalError::IoError(format!("Failed to move ZIP file: {}", e)))?;

        Ok(())
    }

    /// Get the ZIP archive bytes for a package
    fn get_zip_bytes(&self, package: &PackageUri) -> EvalResult<Arc<Vec<u8>>> {
        let key = package.to_string();

        // Check cache
        if let Some(bytes) = self.zip_cache.borrow().get(&key) {
            return Ok(Arc::clone(bytes));
        }

        // Ensure downloaded
        self.ensure_cached(package)?;

        // Read ZIP
        let zip_path = package.cache_path(&self.cache_dir).join("package.zip");
        let bytes = fs::read(&zip_path).map_err(|e| {
            EvalError::IoError(format!(
                "Failed to read package ZIP {}: {}",
                zip_path.display(),
                e
            ))
        })?;

        let arc = Arc::new(bytes);
        self.zip_cache.borrow_mut().insert(key, Arc::clone(&arc));
        Ok(arc)
    }
}

impl PackageResolver for DiskCachedPackageResolver {
    fn get_bytes(&self, package: &PackageUri, path: &str) -> EvalResult<Vec<u8>> {
        let zip_bytes = self.get_zip_bytes(package)?;
        let cursor = std::io::Cursor::new(zip_bytes.as_ref());
        let mut archive = ZipArchive::new(cursor).map_err(|e| {
            EvalError::IoError(format!("Failed to open package ZIP for {}: {}", package, e))
        })?;

        // Normalize path (remove leading /)
        let normalized_path = path.strip_prefix('/').unwrap_or(path);

        let mut file = archive.by_name(normalized_path).map_err(|e| {
            EvalError::IoError(format!(
                "File not found in package {}: {} ({})",
                package, path, e
            ))
        })?;

        let mut contents = Vec::new();
        file.read_to_end(&mut contents).map_err(|e| {
            EvalError::IoError(format!(
                "Failed to read file from package {}: {} ({})",
                package, path, e
            ))
        })?;

        Ok(contents)
    }

    fn get_metadata(&self, package: &PackageUri) -> EvalResult<DependencyMetadata> {
        let key = package.to_string();

        // Check cache
        if let Some(metadata) = self.metadata_cache.borrow().get(&key) {
            return Ok(metadata.clone());
        }

        // Ensure downloaded
        self.ensure_cached(package)?;

        // Read metadata
        let metadata_path = package.cache_path(&self.cache_dir).join("package.json");
        let json = fs::read_to_string(&metadata_path).map_err(|e| {
            EvalError::IoError(format!(
                "Failed to read package metadata {}: {}",
                metadata_path.display(),
                e
            ))
        })?;

        let metadata = DependencyMetadata::from_json(&json)
            .map_err(|e| EvalError::IoError(format!("Failed to parse package metadata: {}", e)))?;

        self.metadata_cache
            .borrow_mut()
            .insert(key, metadata.clone());
        Ok(metadata)
    }

    fn has_file(&self, package: &PackageUri, path: &str) -> EvalResult<bool> {
        let zip_bytes = self.get_zip_bytes(package)?;
        let cursor = std::io::Cursor::new(zip_bytes.as_ref());
        let archive = ZipArchive::new(cursor).map_err(|e| {
            EvalError::IoError(format!("Failed to open package ZIP for {}: {}", package, e))
        })?;

        let normalized_path = path.strip_prefix('/').unwrap_or(path);
        let exists = archive.file_names().any(|name| name == normalized_path);
        Ok(exists)
    }

    fn list_files(&self, package: &PackageUri, dir: &str) -> EvalResult<Vec<String>> {
        let zip_bytes = self.get_zip_bytes(package)?;
        let cursor = std::io::Cursor::new(zip_bytes.as_ref());
        let archive = ZipArchive::new(cursor).map_err(|e| {
            EvalError::IoError(format!("Failed to open package ZIP for {}: {}", package, e))
        })?;

        let normalized_dir = dir.strip_prefix('/').unwrap_or(dir);
        let prefix = if normalized_dir.is_empty() {
            String::new()
        } else if normalized_dir.ends_with('/') {
            normalized_dir.to_string()
        } else {
            format!("{}/", normalized_dir)
        };

        let files: Vec<String> = archive
            .file_names()
            .filter(|name| name.starts_with(&prefix))
            .map(|s| format!("/{}", s))
            .collect();

        Ok(files)
    }
}

/// Get the default cache directory
fn default_cache_dir() -> EvalResult<PathBuf> {
    let cache = dirs::cache_dir()
        .ok_or_else(|| EvalError::IoError("Could not determine cache directory".to_string()))?;
    Ok(cache.join("pkl").join("packages"))
}

/// Generate a simple random-ish identifier for temp files
fn uuid_simple() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let duration = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    format!("{}-{}", duration.as_nanos(), std::process::id())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_cache_dir() {
        let dir = default_cache_dir().unwrap();
        assert!(dir.to_string_lossy().contains("pkl"));
    }
}
