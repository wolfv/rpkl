//! Module loader for PKL
//!
//! Handles loading and caching of PKL modules from files and packages.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::error::{EvalError, EvalResult};
use crate::packages::{
    DiskCachedPackageResolver, PackageAssetUri, PackageResolver, PackageUri, ProjectDeps,
};
use crate::value::VmValue;

/// A loaded and evaluated module
pub struct LoadedModule {
    /// The module's evaluated value (as an object)
    pub value: VmValue,
    /// The module's source path
    pub path: PathBuf,
}

/// Module loader that handles loading PKL files and packages
pub struct ModuleLoader {
    /// Cache of loaded modules by absolute path
    cache: HashMap<PathBuf, Arc<LoadedModule>>,
    /// Cache of loaded modules from packages (by URI string)
    package_cache: HashMap<String, Arc<LoadedModule>>,
    /// Base path for resolving relative imports
    base_path: PathBuf,
    /// Package resolver (lazy initialized)
    package_resolver: Option<DiskCachedPackageResolver>,
    /// Project dependencies (loaded from PklProject.deps.json)
    project_deps: Option<Arc<ProjectDeps>>,
}

impl ModuleLoader {
    /// Create a new module loader with the given base path
    pub fn new(base_path: impl AsRef<Path>) -> Self {
        Self {
            cache: HashMap::new(),
            package_cache: HashMap::new(),
            base_path: base_path.as_ref().to_path_buf(),
            package_resolver: None,
            project_deps: None,
        }
    }

    /// Create a module loader with the current directory as base
    pub fn current_dir() -> std::io::Result<Self> {
        Ok(Self::new(std::env::current_dir()?))
    }

    /// Get the base path
    pub fn base_path(&self) -> &Path {
        &self.base_path
    }

    /// Set the base path for resolving relative imports
    pub fn set_base_path(&mut self, path: impl AsRef<Path>) {
        self.base_path = path.as_ref().to_path_buf();
    }

    /// Load project dependencies from PklProject.deps.json
    pub fn load_project_deps(&mut self) -> EvalResult<bool> {
        if let Some((deps, _path)) = ProjectDeps::find_and_load(&self.base_path)? {
            self.project_deps = Some(Arc::new(deps));
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Set project dependencies explicitly
    pub fn set_project_deps(&mut self, deps: ProjectDeps) {
        self.project_deps = Some(Arc::new(deps));
    }

    /// Get or create the package resolver
    fn get_package_resolver(&mut self) -> EvalResult<&DiskCachedPackageResolver> {
        if self.package_resolver.is_none() {
            let resolver = DiskCachedPackageResolver::new()?;
            self.package_resolver = Some(resolver);
        }
        Ok(self.package_resolver.as_ref().unwrap())
    }

    /// Check if a URI is a package URI
    pub fn is_package_uri(uri: &str) -> bool {
        uri.starts_with("package://") || uri.starts_with("projectpackage://")
    }

    /// Check if a URI is a standard library URI
    pub fn is_stdlib_uri(uri: &str) -> bool {
        uri.starts_with("pkl:")
    }

    /// Resolve a URI to an absolute path
    pub fn resolve_uri(&self, uri: &str) -> EvalResult<PathBuf> {
        // Handle different URI schemes
        if uri.starts_with("file://") {
            let path = uri.strip_prefix("file://").unwrap();
            return Ok(PathBuf::from(path));
        }

        // Handle package URIs
        if Self::is_package_uri(uri) || PackageAssetUri::is_package_asset_uri(uri) {
            return Err(EvalError::IoError(format!(
                "Use resolve_package_uri() for package URIs: {}",
                uri
            )));
        }

        // Handle pkl: standard library URIs
        if Self::is_stdlib_uri(uri) {
            return Err(EvalError::IoError(format!(
                "Standard library URIs not yet supported: {}",
                uri
            )));
        }

        // Relative path
        let path = Path::new(uri);
        if path.is_absolute() {
            Ok(path.to_path_buf())
        } else {
            Ok(self.base_path.join(path))
        }
    }

    /// Resolve a URI relative to another path
    pub fn resolve_uri_relative(&self, uri: &str, relative_to: &Path) -> EvalResult<PathBuf> {
        // Handle different URI schemes
        if uri.starts_with("file://") {
            let path = uri.strip_prefix("file://").unwrap();
            return Ok(PathBuf::from(path));
        }

        // Handle package URIs
        if Self::is_package_uri(uri) || PackageAssetUri::is_package_asset_uri(uri) {
            return Err(EvalError::IoError(format!(
                "Use resolve_package_uri() for package URIs: {}",
                uri
            )));
        }

        // Handle pkl: standard library URIs
        if Self::is_stdlib_uri(uri) {
            return Err(EvalError::IoError(format!(
                "Standard library URIs not yet supported: {}",
                uri
            )));
        }

        // Relative path - resolve against the directory of the importing file
        let path = Path::new(uri);
        if path.is_absolute() {
            Ok(path.to_path_buf())
        } else {
            let base = relative_to.parent().unwrap_or(Path::new("."));
            Ok(base.join(path))
        }
    }

    /// Resolve a package URI to a PackageUri and asset path
    pub fn resolve_package_uri(&self, uri: &str) -> EvalResult<(PackageUri, String)> {
        if PackageAssetUri::is_package_asset_uri(uri) {
            let asset_uri = PackageAssetUri::parse(uri)?;
            Ok((asset_uri.package, asset_uri.asset_path))
        } else if uri.starts_with("package://") || uri.starts_with("projectpackage://") {
            let package = PackageUri::parse(uri)?;
            // Default to the main module
            Ok((package, "/PklProject.pkl".to_string()))
        } else {
            Err(EvalError::InvalidPackageUri(format!(
                "Not a package URI: {}",
                uri
            )))
        }
    }

    /// Load module source from a path
    pub fn load_source(&self, path: &Path) -> EvalResult<String> {
        std::fs::read_to_string(path)
            .map_err(|e| EvalError::IoError(format!("Failed to read '{}': {}", path.display(), e)))
    }

    /// Load module source from a package
    pub fn load_package_source(
        &mut self,
        package: &PackageUri,
        asset_path: &str,
    ) -> EvalResult<String> {
        let resolver = self.get_package_resolver()?;
        resolver.get_text(package, asset_path)
    }

    /// Check if a module is already cached
    pub fn get_cached(&self, path: &Path) -> Option<Arc<LoadedModule>> {
        let canonical = path.canonicalize().ok()?;
        self.cache.get(&canonical).cloned()
    }

    /// Check if a package module is already cached
    pub fn get_cached_package(&self, uri: &str) -> Option<Arc<LoadedModule>> {
        self.package_cache.get(uri).cloned()
    }

    /// Store a loaded module in the cache
    pub fn cache_module(&mut self, path: &Path, module: LoadedModule) -> Arc<LoadedModule> {
        let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        let arc = Arc::new(module);
        self.cache.insert(canonical, Arc::clone(&arc));
        arc
    }

    /// Store a loaded package module in the cache
    pub fn cache_package_module(&mut self, uri: &str, module: LoadedModule) -> Arc<LoadedModule> {
        let arc = Arc::new(module);
        self.package_cache.insert(uri.to_string(), Arc::clone(&arc));
        arc
    }
}

impl Default for ModuleLoader {
    fn default() -> Self {
        Self::current_dir().unwrap_or_else(|_| Self::new("."))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_package_uri() {
        assert!(ModuleLoader::is_package_uri(
            "package://example.com/pkg@1.0.0"
        ));
        assert!(ModuleLoader::is_package_uri(
            "projectpackage://example.com/pkg@1.0.0"
        ));
        assert!(!ModuleLoader::is_package_uri("file:///path/to/file.pkl"));
        assert!(!ModuleLoader::is_package_uri("./relative/path.pkl"));
    }

    #[test]
    fn test_is_stdlib_uri() {
        assert!(ModuleLoader::is_stdlib_uri("pkl:base"));
        assert!(ModuleLoader::is_stdlib_uri("pkl:json"));
        assert!(!ModuleLoader::is_stdlib_uri(
            "package://example.com/pkg@1.0.0"
        ));
    }
}
