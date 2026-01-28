//! Scope and environment handling for PKL evaluation

use std::collections::HashMap;
use std::sync::Arc;

use crate::object::VmObject;
use crate::value::VmValue;

/// Reference to a scope (shared, immutable)
pub type ScopeRef = Arc<Scope>;

/// Lexical scope for variable resolution
#[derive(Debug, Default)]
pub struct Scope {
    /// Local bindings (let, function parameters, for loop variables)
    locals: HashMap<String, VmValue>,

    /// Parent scope (lexical)
    parent: Option<ScopeRef>,

    /// The `this` object in this scope
    this_obj: Option<Arc<VmObject>>,

    /// The `outer` reference (enclosing object)
    outer_obj: Option<Arc<VmObject>>,

    /// The module object
    module_obj: Option<Arc<VmObject>>,
}

impl Scope {
    /// Create a new root scope
    pub fn new() -> ScopeRef {
        Arc::new(Self {
            locals: HashMap::new(),
            parent: None,
            this_obj: None,
            outer_obj: None,
            module_obj: None,
        })
    }

    /// Create a new scope with a module
    pub fn with_module(module: Arc<VmObject>) -> ScopeRef {
        Arc::new(Self {
            locals: HashMap::new(),
            parent: None,
            this_obj: Some(Arc::clone(&module)),
            outer_obj: None,
            module_obj: Some(module),
        })
    }

    /// Create a child scope with additional local bindings
    pub fn with_locals(parent: &ScopeRef, bindings: Vec<(String, VmValue)>) -> ScopeRef {
        let mut locals = HashMap::new();
        for (name, value) in bindings {
            locals.insert(name, value);
        }
        Arc::new(Self {
            locals,
            parent: Some(Arc::clone(parent)),
            this_obj: parent.this_obj.clone(),
            outer_obj: parent.outer_obj.clone(),
            module_obj: parent.module_obj.clone(),
        })
    }

    /// Create a child scope with a new `this` object
    pub fn with_this(parent: &ScopeRef, this_obj: Arc<VmObject>) -> ScopeRef {
        Arc::new(Self {
            locals: HashMap::new(),
            parent: Some(Arc::clone(parent)),
            this_obj: Some(this_obj),
            outer_obj: parent.this_obj.clone(), // Previous this becomes outer
            module_obj: parent.module_obj.clone(),
        })
    }

    /// Create a child scope for lambda evaluation
    pub fn for_lambda(parent: &ScopeRef, params: Vec<(String, VmValue)>) -> ScopeRef {
        let mut locals = HashMap::new();
        for (name, value) in params {
            locals.insert(name, value);
        }
        Arc::new(Self {
            locals,
            parent: Some(Arc::clone(parent)),
            this_obj: parent.this_obj.clone(),
            outer_obj: parent.outer_obj.clone(),
            module_obj: parent.module_obj.clone(),
        })
    }

    /// Create a child scope with an overridden module object
    ///
    /// This is used when evaluating inherited properties in an amended module.
    /// The inherited expression was captured with the parent module's scope, but
    /// it should see the child module's properties when resolving identifiers.
    pub fn with_module_override(parent: &ScopeRef, module: Arc<VmObject>) -> ScopeRef {
        Arc::new(Self {
            locals: HashMap::new(),
            parent: Some(Arc::clone(parent)),
            this_obj: Some(Arc::clone(&module)),
            outer_obj: parent.outer_obj.clone(),
            module_obj: Some(module),
        })
    }

    /// Resolve an identifier in this scope
    pub fn resolve(&self, name: &str) -> Option<VmValue> {
        // Check locals first
        if let Some(v) = self.locals.get(name) {
            return Some(v.clone());
        }

        // Check parent scope
        if let Some(parent) = &self.parent {
            if let Some(v) = parent.resolve(name) {
                return Some(v);
            }
        }

        None
    }

    /// Get the `this` object
    pub fn this(&self) -> Option<&Arc<VmObject>> {
        self.this_obj.as_ref()
    }

    /// Get the `outer` object
    pub fn outer(&self) -> Option<&Arc<VmObject>> {
        self.outer_obj.as_ref()
    }

    /// Get the `super` object (parent of this)
    pub fn super_obj(&self) -> Option<Arc<VmObject>> {
        self.this_obj.as_ref().and_then(|obj| obj.parent.clone())
    }

    /// Get the module object
    pub fn module(&self) -> Option<&Arc<VmObject>> {
        self.module_obj.as_ref()
    }

    /// Check if a local binding exists
    pub fn has_local(&self, name: &str) -> bool {
        self.locals.contains_key(name)
    }

    /// Get all local binding names (for debugging)
    pub fn local_names(&self) -> Vec<&str> {
        self.locals.keys().map(|s| s.as_str()).collect()
    }
}

impl Scope {
    /// Create a scope with `this` bound to a plain value (for type constraint evaluation)
    /// This is different from with_this which expects a VmObject
    pub fn for_constraint(this_value: VmValue) -> ScopeRef {
        let mut locals = HashMap::new();
        locals.insert("this".to_string(), this_value);
        Arc::new(Self {
            locals,
            parent: None,
            this_obj: None,
            outer_obj: None,
            module_obj: None,
        })
    }

    /// Define a local binding in this scope (mutable version for building scopes)
    pub fn define(&mut self, name: String, value: VmValue) {
        self.locals.insert(name, value);
    }
}
