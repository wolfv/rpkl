//! Module loader for PKL
//!
//! Handles loading and caching of PKL modules from files.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::error::{EvalError, EvalResult};
use crate::value::VmValue;

/// A loaded and evaluated module
pub struct LoadedModule {
    /// The module's evaluated value (as an object)
    pub value: VmValue,
    /// The module's source path
    pub path: PathBuf,
}

/// Module loader that handles loading PKL files
pub struct ModuleLoader {
    /// Cache of loaded modules by absolute path
    cache: HashMap<PathBuf, Arc<LoadedModule>>,
    /// Base path for resolving relative imports
    base_path: PathBuf,
}

impl ModuleLoader {
    /// Create a new module loader with the given base path
    pub fn new(base_path: impl AsRef<Path>) -> Self {
        Self {
            cache: HashMap::new(),
            base_path: base_path.as_ref().to_path_buf(),
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

    /// Resolve a URI to an absolute path
    pub fn resolve_uri(&self, uri: &str) -> EvalResult<PathBuf> {
        // Handle different URI schemes
        if uri.starts_with("file://") {
            let path = uri.strip_prefix("file://").unwrap();
            return Ok(PathBuf::from(path));
        }

        // Handle package URIs (pkl:, package:, etc.) - not implemented yet
        if uri.starts_with("pkl:") || uri.starts_with("package:") {
            return Err(EvalError::IoError(format!(
                "Package URIs not yet supported: {}",
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
        if uri.starts_with("pkl:") || uri.starts_with("package:") {
            return Err(EvalError::IoError(format!(
                "Package URIs not yet supported: {}",
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

    /// Load module source from a path
    pub fn load_source(&self, path: &Path) -> EvalResult<String> {
        std::fs::read_to_string(path)
            .map_err(|e| EvalError::IoError(format!("Failed to read '{}': {}", path.display(), e)))
    }

    /// Check if a module is already cached
    pub fn get_cached(&self, path: &Path) -> Option<Arc<LoadedModule>> {
        let canonical = path.canonicalize().ok()?;
        self.cache.get(&canonical).cloned()
    }

    /// Store a loaded module in the cache
    pub fn cache_module(&mut self, path: &Path, module: LoadedModule) -> Arc<LoadedModule> {
        let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        let arc = Arc::new(module);
        self.cache.insert(canonical, Arc::clone(&arc));
        arc
    }
}

impl Default for ModuleLoader {
    fn default() -> Self {
        Self::current_dir().unwrap_or_else(|_| Self::new("."))
    }
}
