//! Expression evaluator for PKL

use std::path::Path;
use std::sync::Arc;

use rpkl_parser::{
    BinaryOp, ClassDef, Expr, ExprKind, ImportKind, Module, ModuleKind, ModuleMember, ObjectBody,
    ObjectMember, Property, PropertyValue, StringLiteral, StringPart, UnaryOp,
};

use crate::error::{EvalError, EvalResult};
use crate::loader::{LoadedModule, ModuleLoader};
use crate::object::{MemberMetadata, ObjectKind, ObjectMember as ObjMember, VmObject};
use crate::scope::{Scope, ScopeRef};
use crate::value::{DataSizeUnit, DurationUnit, LambdaClosure, VmValue};

/// External function type
pub type ExternalFn =
    Arc<dyn Fn(&[VmValue], &Evaluator, &ScopeRef) -> EvalResult<VmValue> + Send + Sync>;

/// External function registry
#[derive(Default)]
pub struct ExternalRegistry {
    /// Methods: type_name -> method_name -> fn
    methods: std::collections::HashMap<String, std::collections::HashMap<String, ExternalFn>>,
    /// Properties: type_name -> property_name -> fn
    properties: std::collections::HashMap<String, std::collections::HashMap<String, ExternalFn>>,
    /// Module-level functions: module_name -> function_name -> fn
    /// Used for `external function` declarations in PKL modules
    functions: std::collections::HashMap<String, std::collections::HashMap<String, ExternalFn>>,
}

impl ExternalRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register_method(&mut self, type_name: &str, method: &str, f: ExternalFn) {
        self.methods
            .entry(type_name.to_string())
            .or_default()
            .insert(method.to_string(), f);
    }

    pub fn register_property(&mut self, type_name: &str, prop: &str, f: ExternalFn) {
        self.properties
            .entry(type_name.to_string())
            .or_default()
            .insert(prop.to_string(), f);
    }

    /// Register an external function for a module
    ///
    /// This is used for `external function` declarations in PKL modules.
    /// The module_name can be:
    /// - A simple module name like "Variant"
    /// - A full module path for more specific matching
    pub fn register_function(&mut self, module_name: &str, function_name: &str, f: ExternalFn) {
        self.functions
            .entry(module_name.to_string())
            .or_default()
            .insert(function_name.to_string(), f);
    }

    pub fn get_method(&self, type_name: &str, method: &str) -> Option<&ExternalFn> {
        self.methods.get(type_name)?.get(method)
    }

    pub fn get_property(&self, type_name: &str, prop: &str) -> Option<&ExternalFn> {
        self.properties.get(type_name)?.get(prop)
    }

    /// Get an external function for a module
    pub fn get_function(&self, module_name: &str, function_name: &str) -> Option<&ExternalFn> {
        self.functions.get(module_name)?.get(function_name)
    }
}

/// PKL evaluator
pub struct Evaluator {
    /// External function registry
    pub externals: ExternalRegistry,
    /// Module loader for handling imports
    loader: std::cell::RefCell<ModuleLoader>,
    /// Recursion depth limit
    max_depth: usize,
    /// Current recursion depth
    depth: std::cell::Cell<usize>,
    /// Class definitions by name, along with the module scope where they were defined
    classes: std::cell::RefCell<std::collections::HashMap<String, (Arc<ClassDef>, ScopeRef)>>,
}

impl Evaluator {
    /// Create a new evaluator
    pub fn new() -> Self {
        Self {
            externals: ExternalRegistry::new(),
            loader: std::cell::RefCell::new(ModuleLoader::default()),
            max_depth: 1000,
            depth: std::cell::Cell::new(0),
            classes: std::cell::RefCell::new(std::collections::HashMap::new()),
        }
    }

    /// Create an evaluator with external functions
    pub fn with_externals(externals: ExternalRegistry) -> Self {
        Self {
            externals,
            loader: std::cell::RefCell::new(ModuleLoader::default()),
            classes: std::cell::RefCell::new(std::collections::HashMap::new()),
            max_depth: 1000,
            depth: std::cell::Cell::new(0),
        }
    }

    /// Create an evaluator with a custom module loader
    pub fn with_loader(externals: ExternalRegistry, loader: ModuleLoader) -> Self {
        Self {
            externals,
            loader: std::cell::RefCell::new(loader),
            classes: std::cell::RefCell::new(std::collections::HashMap::new()),
            max_depth: 1000,
            depth: std::cell::Cell::new(0),
        }
    }

    /// Set the base path for module loading
    pub fn set_base_path(&self, path: impl AsRef<Path>) {
        self.loader.borrow_mut().set_base_path(path);
    }

    /// Force all lazy members in a value (recursively for objects)
    ///
    /// This should be called before serializing a value to ensure all
    /// lazy members are evaluated.
    pub fn force_value(&self, value: &VmValue) -> EvalResult<()> {
        if let VmValue::Object(obj) = value {
            // For modules that amend other modules, use the module override
            // so inherited properties see the current module's values
            if obj.parent.is_some() {
                self.force_object_with_module(obj, Some(obj))?;
            } else {
                self.force_object(obj)?;
            }
        }
        Ok(())
    }

    /// Evaluate a module from a file path
    pub fn eval_file(&self, path: impl AsRef<Path>) -> EvalResult<VmValue> {
        let path = path.as_ref();

        // Set the base path to the file's directory for relative imports
        if let Some(parent) = path.parent() {
            self.loader.borrow_mut().set_base_path(parent);
        }

        // Check cache first
        if let Some(cached) = self.loader.borrow().get_cached(path) {
            return Ok(cached.value.clone());
        }

        // Load and parse the source
        let source = self.loader.borrow().load_source(path)?;
        let module = rpkl_parser::parse_module(&source).map_err(|e| {
            EvalError::IoError(format!("Failed to parse '{}': {}", path.display(), e))
        })?;

        // Evaluate the module
        let value = self.eval_module_with_path(&module, path)?;

        // Cache the result
        let loaded = LoadedModule {
            value: value.clone(),
            path: path.to_path_buf(),
        };
        self.loader.borrow_mut().cache_module(path, loaded);

        Ok(value)
    }

    /// Evaluate a module
    pub fn eval_module(&self, module: &Module) -> EvalResult<VmValue> {
        // Use a dummy path for modules evaluated from source
        self.eval_module_with_path(module, Path::new("<source>"))
    }

    /// Evaluate a module with a specific path context
    fn eval_module_with_path(&self, module: &Module, module_path: &Path) -> EvalResult<VmValue> {
        // Check if module amends or extends another module
        let parent_obj = if let Some(header) = &module.header {
            match &header.kind {
                ModuleKind::Amends { uri } | ModuleKind::Extends { uri } => {
                    // Get the parent module URI
                    let parent_uri = uri.as_simple().ok_or_else(|| {
                        EvalError::IoError("Module URI cannot be interpolated".to_string())
                    })?;

                    // Resolve and load the parent module
                    let parent_path = self
                        .loader
                        .borrow()
                        .resolve_uri_relative(parent_uri, module_path)?;
                    let parent_value = self.eval_file(&parent_path)?;

                    // Get the parent object
                    match parent_value {
                        VmValue::Object(obj) => Some(obj),
                        _ => {
                            return Err(EvalError::InvalidOperation(
                                "Parent module must evaluate to an object".to_string(),
                            ))
                        }
                    }
                }
                ModuleKind::Module { .. } => None,
            }
        } else {
            None
        };

        // Create the module object, potentially with a parent
        let scope = Scope::new();
        let obj = if let Some(parent) = parent_obj {
            // Create object that amends/extends the parent
            let amended = parent.amend(Arc::clone(&scope));
            Arc::new(amended)
        } else {
            Arc::new(VmObject::new_dynamic(Arc::clone(&scope)))
        };

        let module_scope = Scope::with_module(Arc::clone(&obj));

        // Process imports first
        let mut imports: Vec<(String, VmValue)> = Vec::new();
        for import in &module.imports {
            let (name, value) = self.process_import(import, module_path)?;
            imports.push((name, value));
        }

        // Add imports to the module object as properties
        for (name, value) in imports {
            let member = ObjMember::with_value(value);
            obj.add_property(name, member);
        }

        // Evaluate all module members
        for member in &module.members {
            match member {
                ModuleMember::Property(prop) => {
                    self.add_property_to_object(&obj, prop, &module_scope)?;
                }
                ModuleMember::Method(method) => {
                    // Methods are added as properties with lambda values
                    if let Some(body_expr) = &method.body {
                        let params: Vec<String> =
                            method.params.iter().map(|p| p.name.node.clone()).collect();
                        let closure = LambdaClosure {
                            params,
                            body: body_expr.clone(),
                            captured_scope: Arc::clone(&module_scope),
                        };
                        let member = ObjMember::with_value(VmValue::Lambda(Arc::new(closure)));
                        obj.add_property(method.name.node.clone(), member);
                    } else if method.modifiers.is_external {
                        // External function - look up in the registry
                        // Get module name from header or path
                        let module_name = module
                            .header
                            .as_ref()
                            .and_then(|h| match &h.kind {
                                rpkl_parser::ModuleKind::Module { name, .. } => name
                                    .as_ref()
                                    .map(|n| n.parts.last().map(|p| p.node.as_str()).unwrap_or("")),
                                _ => None,
                            })
                            .unwrap_or_else(|| {
                                module_path
                                    .file_stem()
                                    .and_then(|s| s.to_str())
                                    .unwrap_or("")
                            });

                        let method_name = method.name.node.as_str();

                        // Create a wrapper lambda that calls the external function
                        if let Some(ext_fn) = self.externals.get_function(module_name, method_name)
                        {
                            let ext_fn = Arc::clone(ext_fn);
                            let num_params = method.params.len();
                            let wrapper = VmValue::ExternalFunc {
                                func: ext_fn,
                                num_params,
                            };
                            let member = ObjMember::with_value(wrapper);
                            obj.add_property(method.name.node.clone(), member);
                        }
                        // If no external implementation is found, the function will fail at runtime
                    }
                }
                ModuleMember::Class(class) => {
                    // Store class definition along with the module scope where it was defined
                    // This ensures that class methods can access imports from their defining module
                    let name = class.name.node.clone();
                    self.classes
                        .borrow_mut()
                        .insert(name, (Arc::new(class.clone()), Arc::clone(&module_scope)));
                }
                ModuleMember::TypeAlias(_) => {
                    // Type aliases don't produce runtime values
                }
            }
        }

        // Don't force properties here - defer to force_value which is called
        // at the top level. This allows parent modules to have unset typed
        // properties that are filled in by child modules.
        Ok(VmValue::Object(obj))
    }

    /// Process an import and return the name and value
    fn process_import(
        &self,
        import: &rpkl_parser::Import,
        importing_path: &Path,
    ) -> EvalResult<(String, VmValue)> {
        // Get the import URI
        let uri = import
            .uri
            .as_simple()
            .ok_or_else(|| EvalError::IoError("Import URI cannot be interpolated".to_string()))?;

        // Resolve the import path
        let resolved_path = self
            .loader
            .borrow()
            .resolve_uri_relative(uri, importing_path)?;

        // Handle glob imports differently
        if matches!(import.kind, ImportKind::Glob) {
            return Err(EvalError::IoError(
                "Glob imports not yet supported".to_string(),
            ));
        }

        // Determine the name for this import
        let import_name = if let Some(alias) = &import.alias {
            alias.node.clone()
        } else {
            // Use the file stem as the name
            resolved_path
                .file_stem()
                .and_then(|s| s.to_str())
                .map(|s| s.to_string())
                .ok_or_else(|| {
                    EvalError::IoError(format!("Cannot determine import name for '{}'", uri))
                })?
        };

        // Load and evaluate the imported module
        let value = self.eval_file(&resolved_path)?;

        Ok((import_name, value))
    }

    /// Add a property to an object
    fn add_property_to_object(
        &self,
        obj: &Arc<VmObject>,
        prop: &Property,
        scope: &ScopeRef,
    ) -> EvalResult<()> {
        let name = prop.name.node.clone();

        // Extract type hint from type annotation (for nullable typed properties)
        let type_hint = prop
            .ty
            .as_ref()
            .and_then(|ty| self.extract_base_type_name(ty));

        // Create metadata from property modifiers
        let metadata = MemberMetadata {
            is_hidden: prop.modifiers.is_hidden,
            is_local: prop.modifiers.is_local,
            type_hint: type_hint.clone(),
        };

        match &prop.value {
            Some(PropertyValue::Expr(expr)) => {
                let member =
                    ObjMember::new_with_metadata(expr.clone(), Arc::clone(scope), metadata);
                obj.add_property(name, member);
            }
            Some(PropertyValue::Object(body)) => {
                // Property with object body - check if we're amending an existing property
                if let Some(existing) = obj.get_property_member(&name) {
                    // Amend the existing property from parent
                    // We need to eagerly evaluate this to get the amended value,
                    // because the base comes from the parent object
                    let base_value = existing.force(|expr, scope| self.eval_expr(expr, scope))?;
                    // Get type hint from existing member if available
                    let existing_type_hint = existing.metadata().type_hint.clone();
                    let amended = self.eval_amendment_with_type_hint(
                        base_value,
                        body,
                        scope,
                        existing_type_hint,
                    )?;
                    let member = ObjMember::with_value_and_metadata(amended, metadata);
                    obj.add_property(name, member);
                } else {
                    // Create new nested object - store as lazy expression
                    // This allows the object body to be evaluated later with the
                    // correct scope context (important for amends where child
                    // module properties should be visible)
                    let new_expr = Expr {
                        kind: ExprKind::New {
                            class_ref: None,
                            body: body.clone(),
                        },
                        span: prop.name.span.clone(),
                    };
                    let member =
                        ObjMember::new_with_metadata(new_expr, Arc::clone(scope), metadata);
                    obj.add_property(name, member);
                }
            }
            None => {
                // Property without value - will be undefined
                let member = ObjMember::with_value_and_metadata(VmValue::Null, metadata);
                obj.add_property(name, member);
            }
        }

        Ok(())
    }

    /// Evaluate an expression
    pub fn eval_expr(&self, expr: &Expr, scope: &ScopeRef) -> EvalResult<VmValue> {
        // Check recursion depth
        let current_depth = self.depth.get();
        if current_depth >= self.max_depth {
            return Err(EvalError::StackOverflow);
        }
        self.depth.set(current_depth + 1);
        let result = self.eval_expr_inner(expr, scope);
        self.depth.set(current_depth);
        result
    }

    fn eval_expr_inner(&self, expr: &Expr, scope: &ScopeRef) -> EvalResult<VmValue> {
        match &expr.kind {
            // Literals
            ExprKind::Null => Ok(VmValue::Null),
            ExprKind::Bool(b) => Ok(VmValue::Boolean(*b)),
            ExprKind::Int(i) => Ok(VmValue::Int(*i)),
            ExprKind::Float(f) => Ok(VmValue::Float(*f)),
            ExprKind::String(lit) => self.eval_string_literal(lit, scope),

            // Identifiers and references
            ExprKind::Identifier(name) => self.resolve_identifier(name, scope),
            ExprKind::This => {
                // First check if `this` is a local variable (e.g., in type constraint evaluation)
                if let Some(value) = scope.resolve("this") {
                    return Ok(value);
                }
                // Otherwise, check for `this` object in scope
                scope
                    .this()
                    .map(|obj| VmValue::Object(Arc::clone(obj)))
                    .ok_or_else(|| EvalError::UndefinedVariable("this".to_string()))
            }
            ExprKind::Super => scope
                .super_obj()
                .map(VmValue::Object)
                .ok_or_else(|| EvalError::UndefinedVariable("super".to_string())),
            ExprKind::Outer => scope
                .outer()
                .map(|obj| VmValue::Object(Arc::clone(obj)))
                .ok_or_else(|| EvalError::UndefinedVariable("outer".to_string())),
            ExprKind::Module => scope
                .module()
                .map(|obj| VmValue::Object(Arc::clone(obj)))
                .ok_or_else(|| EvalError::UndefinedVariable("module".to_string())),

            // Object creation and amendment
            ExprKind::New { class_ref, body } => {
                let (kind, class_info) = match class_ref {
                    Some(name) => {
                        let type_name = name.to_string();
                        match type_name.as_str() {
                            "Listing" => (ObjectKind::Listing, None),
                            "Mapping" => (ObjectKind::Mapping, None),
                            "Dynamic" => (ObjectKind::Dynamic, None),
                            _ => {
                                // Look up class definition - try full name first, then just the class name
                                // Returns (ClassDef, ScopeRef) tuple where ScopeRef is the module scope
                                // where the class was defined
                                let class_info =
                                    self.classes.borrow().get(&type_name).cloned().or_else(|| {
                                        // For qualified names like Build.CMakeConfig, try just CMakeConfig
                                        if let Some(dot_pos) = type_name.rfind('.') {
                                            let simple_name = &type_name[dot_pos + 1..];
                                            self.classes.borrow().get(simple_name).cloned()
                                        } else {
                                            None
                                        }
                                    });
                                (ObjectKind::Typed(type_name), class_info)
                            }
                        }
                    }
                    None => (ObjectKind::Dynamic, None),
                };
                self.eval_new_object_with_class(kind, class_info, body, scope)
            }
            ExprKind::Amend { base, body } => {
                let base_value = self.eval_expr(base, scope)?;
                self.eval_amendment(base_value, body, scope)
            }

            // Member access
            ExprKind::MemberAccess { base, member } => {
                let base_value = self.eval_expr(base, scope)?;
                self.eval_member_access(&base_value, &member.node, scope)
            }
            ExprKind::OptionalMemberAccess { base, member } => {
                let base_value = self.eval_expr(base, scope)?;
                if base_value.is_null() {
                    Ok(VmValue::Null)
                } else {
                    self.eval_member_access(&base_value, &member.node, scope)
                }
            }

            // Binary operations
            ExprKind::Binary { op, left, right } => self.eval_binary_op(*op, left, right, scope),

            // Unary operations
            ExprKind::Unary { op, operand } => {
                let value = self.eval_expr(operand, scope)?;
                self.eval_unary_op(*op, value)
            }

            // Subscript
            ExprKind::Subscript { base, index } => {
                let base_value = self.eval_expr(base, scope)?;
                let index_value = self.eval_expr(index, scope)?;
                self.eval_subscript(&base_value, &index_value, scope)
            }

            // Function call
            ExprKind::Call { callee, args } => {
                // Check if this is a method call (callee is MemberAccess)
                if let ExprKind::MemberAccess { base, member } = &callee.kind {
                    let base_value = self.eval_expr(base, scope)?;
                    let method_name = &member.node;

                    // Try external method lookup first
                    if let Some(ext_fn) = self
                        .externals
                        .get_method(base_value.type_name(), method_name)
                    {
                        let mut all_args = vec![base_value];
                        for arg in args {
                            all_args.push(self.eval_expr(arg, scope)?);
                        }
                        return ext_fn(&all_args, self, scope);
                    }

                    // Also check for Object types based on their kind
                    if let VmValue::Object(obj) = &base_value {
                        let kind_type = match &obj.kind {
                            ObjectKind::Listing => "Listing",
                            ObjectKind::Mapping => "Mapping",
                            ObjectKind::Dynamic => "Dynamic",
                            ObjectKind::Typed(name) => name.as_str(),
                        };
                        if let Some(ext_fn) = self.externals.get_method(kind_type, method_name) {
                            let mut all_args = vec![base_value];
                            for arg in args {
                                all_args.push(self.eval_expr(arg, scope)?);
                            }
                            return ext_fn(&all_args, self, scope);
                        }

                        // For typed objects with generics like "Mapping<String, String>",
                        // also try the base type name ("Mapping")
                        if let ObjectKind::Typed(name) = &obj.kind {
                            if let Some(base_type) = name.split('<').next() {
                                if base_type != name.as_str() {
                                    if let Some(ext_fn) =
                                        self.externals.get_method(base_type, method_name)
                                    {
                                        let mut all_args = vec![base_value];
                                        for arg in args {
                                            all_args.push(self.eval_expr(arg, scope)?);
                                        }
                                        return ext_fn(&all_args, self, scope);
                                    }
                                }
                            }
                        }
                    }

                    // Fall back to getting the member as a callable value
                    let callee_value = self.eval_member_access(&base_value, method_name, scope)?;
                    let arg_values: Vec<VmValue> = args
                        .iter()
                        .map(|arg| self.eval_expr(arg, scope))
                        .collect::<EvalResult<_>>()?;
                    return self.eval_call(&callee_value, &arg_values, scope);
                }

                let callee_value = self.eval_expr(callee, scope)?;
                let arg_values: Vec<VmValue> = args
                    .iter()
                    .map(|arg| self.eval_expr(arg, scope))
                    .collect::<EvalResult<_>>()?;
                self.eval_call(&callee_value, &arg_values, scope)
            }

            // Control flow
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.eval_expr(condition, scope)?;
                if cond.is_truthy() {
                    self.eval_expr(then_branch, scope)
                } else {
                    self.eval_expr(else_branch, scope)
                }
            }
            ExprKind::Let {
                name,
                ty: _,
                value,
                body,
            } => {
                let val = self.eval_expr(value, scope)?;
                let new_scope = Scope::with_locals(scope, vec![(name.node.clone(), val)]);
                self.eval_expr(body, &new_scope)
            }

            // Lambda
            ExprKind::Lambda { params, body } => {
                let param_names: Vec<String> = params.iter().map(|p| p.name.node.clone()).collect();
                Ok(VmValue::Lambda(Arc::new(LambdaClosure {
                    params: param_names,
                    body: (**body).clone(),
                    captured_scope: Arc::clone(scope),
                })))
            }

            // Type operations
            ExprKind::Is { value, ty } => {
                let val = self.eval_expr(value, scope)?;
                let matches = self.check_type(&val, ty);
                Ok(VmValue::Boolean(matches))
            }
            ExprKind::As { value, ty } => {
                let val = self.eval_expr(value, scope)?;
                // Check if the value matches the target type
                if self.check_type(&val, ty) {
                    // Type matches - perform any conversions if needed
                    self.coerce_to_type(val, ty)
                } else {
                    // Type doesn't match - return an error
                    Err(EvalError::TypeCastFailed {
                        actual: val.type_name().to_string(),
                        target: self.type_annotation_to_string(ty),
                    })
                }
            }

            // Null handling
            ExprKind::NonNullAssertion(inner) => {
                let value = self.eval_expr(inner, scope)?;
                if value.is_null() {
                    Err(EvalError::NullPointer(
                        "Non-null assertion failed".to_string(),
                    ))
                } else {
                    Ok(value)
                }
            }
            ExprKind::NullCoalesce { value, default } => {
                let val = self.eval_expr(value, scope)?;
                if val.is_null() {
                    self.eval_expr(default, scope)
                } else {
                    Ok(val)
                }
            }

            // Pipe operator
            ExprKind::Pipe { value, function } => {
                let val = self.eval_expr(value, scope)?;
                let func = self.eval_expr(function, scope)?;
                self.eval_call(&func, &[val], scope)
            }

            // Special operations
            ExprKind::Throw(message) => {
                let msg = self.eval_expr(message, scope)?;
                let msg_str = match msg {
                    VmValue::String(s) => s.to_string(),
                    _ => msg.to_string(),
                };
                Err(EvalError::UserException(msg_str))
            }
            ExprKind::Trace(inner) => {
                let value = self.eval_expr(inner, scope)?;
                eprintln!("trace: {}", value);
                Ok(value)
            }
            ExprKind::Read { uri, is_nullable } => {
                let uri_val = self.eval_expr(uri, scope)?;
                let uri_str = uri_val
                    .as_string()
                    .ok_or_else(|| EvalError::type_error("String", uri_val.type_name()))?;

                // Handle different URI schemes
                if let Some(env_name) = uri_str.strip_prefix("env:") {
                    // Read environment variable
                    match std::env::var(env_name) {
                        Ok(value) => Ok(VmValue::string(value)),
                        Err(_) => {
                            if *is_nullable {
                                Ok(VmValue::Null)
                            } else {
                                Err(EvalError::IoError(format!(
                                    "Environment variable not set: {}",
                                    env_name
                                )))
                            }
                        }
                    }
                } else if let Some(prop_name) = uri_str.strip_prefix("prop:") {
                    // Read system property (similar to Java system properties)
                    // For now, just map to environment variables
                    match std::env::var(prop_name) {
                        Ok(value) => Ok(VmValue::string(value)),
                        Err(_) => {
                            if *is_nullable {
                                Ok(VmValue::Null)
                            } else {
                                Err(EvalError::IoError(format!(
                                    "System property not set: {}",
                                    prop_name
                                )))
                            }
                        }
                    }
                } else {
                    // Other URIs not yet supported
                    if *is_nullable {
                        Ok(VmValue::Null)
                    } else {
                        Err(EvalError::IoError(format!("Cannot read URI: {}", uri_str)))
                    }
                }
            }
            ExprKind::ReadGlob { uri: _ } => {
                // Glob reading not implemented yet
                Ok(VmValue::list(vec![]))
            }

            ExprKind::Parenthesized(inner) => self.eval_expr(inner, scope),
        }
    }

    /// Evaluate a string literal with interpolation
    fn eval_string_literal(&self, lit: &StringLiteral, scope: &ScopeRef) -> EvalResult<VmValue> {
        let mut result = String::new();
        for part in &lit.parts {
            match part {
                StringPart::Literal(s) => result.push_str(s),
                StringPart::Interpolation(expr) => {
                    let value = self.eval_expr(expr, scope)?;
                    match value {
                        VmValue::String(s) => result.push_str(&s),
                        VmValue::Int(i) => result.push_str(&i.to_string()),
                        VmValue::Float(f) => result.push_str(&f.to_string()),
                        VmValue::Boolean(b) => result.push_str(&b.to_string()),
                        VmValue::Null => result.push_str("null"),
                        _ => result.push_str(&value.to_string()),
                    }
                }
            }
        }
        Ok(VmValue::string(result))
    }

    /// Resolve an identifier in scope
    fn resolve_identifier(&self, name: &str, scope: &ScopeRef) -> EvalResult<VmValue> {
        // First check scope locals
        if let Some(value) = scope.resolve(name) {
            return Ok(value);
        }

        // Then check `this` object properties
        if let Some(this) = scope.this() {
            if let Some(member) = this.get_property_member(name) {
                return member.force(|expr, scope| self.eval_expr(expr, scope));
            }
        }

        // Then check `outer` object properties (for nested objects)
        if let Some(outer) = scope.outer() {
            if let Some(member) = outer.get_property_member(name) {
                return member.force(|expr, scope| self.eval_expr(expr, scope));
            }
        }

        // Then check module properties
        if let Some(module) = scope.module() {
            if let Some(member) = module.get_property_member(name) {
                return member.force(|expr, scope| self.eval_expr(expr, scope));
            }
        }

        // Finally check builtin functions
        if let Some(builtin) = Self::get_builtin_function(name) {
            return Ok(builtin);
        }

        Err(EvalError::undefined_var(name))
    }

    /// Get a builtin function by name
    fn get_builtin_function(name: &str) -> Option<VmValue> {
        match name {
            "List" => Some(VmValue::ExternalFunc {
                func: Arc::new(|args, _, _| Ok(VmValue::list(args.to_vec()))),
                num_params: usize::MAX, // Variadic
            }),
            "Set" => Some(VmValue::ExternalFunc {
                func: Arc::new(|args, _, _| {
                    let set: indexmap::IndexSet<VmValue> = args.iter().cloned().collect();
                    Ok(VmValue::Set(Arc::new(set)))
                }),
                num_params: usize::MAX, // Variadic
            }),
            "Map" => Some(VmValue::ExternalFunc {
                func: Arc::new(|args, _, _| {
                    // Map takes alternating key-value pairs
                    if args.len() % 2 != 0 {
                        return Err(EvalError::InvalidOperation(
                            "Map() requires an even number of arguments (key-value pairs)"
                                .to_string(),
                        ));
                    }
                    let mut map = indexmap::IndexMap::new();
                    for chunk in args.chunks(2) {
                        map.insert(chunk[0].clone(), chunk[1].clone());
                    }
                    Ok(VmValue::Map(Arc::new(map)))
                }),
                num_params: usize::MAX, // Variadic
            }),
            "IntSeq" => Some(VmValue::ExternalFunc {
                func: Arc::new(|args, _, _| {
                    let start = args
                        .first()
                        .and_then(|v| v.as_int())
                        .ok_or_else(|| EvalError::type_error("Int", "none"))?;
                    let end = args
                        .get(1)
                        .and_then(|v| v.as_int())
                        .ok_or_else(|| EvalError::type_error("Int", "none"))?;
                    let step = args.get(2).and_then(|v| v.as_int()).unwrap_or(1);
                    Ok(VmValue::IntSeq { start, end, step })
                }),
                num_params: usize::MAX, // 2-3 params
            }),
            "Pair" => Some(VmValue::ExternalFunc {
                func: Arc::new(|args, _, _| {
                    if args.len() != 2 {
                        return Err(EvalError::WrongArgCount {
                            expected: 2,
                            actual: args.len(),
                        });
                    }
                    Ok(VmValue::Pair(Arc::new((args[0].clone(), args[1].clone()))))
                }),
                num_params: 2,
            }),
            "Regex" => Some(VmValue::ExternalFunc {
                func: Arc::new(|args, _, _| {
                    if args.len() != 1 {
                        return Err(EvalError::WrongArgCount {
                            expected: 1,
                            actual: args.len(),
                        });
                    }
                    let pattern = args[0]
                        .as_string()
                        .ok_or_else(|| EvalError::type_error("String", args[0].type_name()))?;
                    Ok(VmValue::Regex(Arc::new(crate::value::RegexValue {
                        pattern: pattern.to_string(),
                    })))
                }),
                num_params: 1,
            }),
            "Bytes" => Some(VmValue::ExternalFunc {
                func: Arc::new(|args, _, _| {
                    // Bytes takes integers and creates a byte array (represented as a List for now)
                    let bytes: Result<Vec<VmValue>, _> = args
                        .iter()
                        .map(|v| {
                            let i = v
                                .as_int()
                                .ok_or_else(|| EvalError::type_error("Int", v.type_name()))?;
                            if !(0..=255).contains(&i) {
                                return Err(EvalError::InvalidOperation(format!(
                                    "Byte value must be 0-255, got {}",
                                    i
                                )));
                            }
                            Ok(VmValue::Int(i))
                        })
                        .collect();
                    Ok(VmValue::list(bytes?))
                }),
                num_params: usize::MAX, // Variadic
            }),
            _ => None,
        }
    }

    /// Evaluate member access
    fn eval_member_access(
        &self,
        base: &VmValue,
        member: &str,
        scope: &ScopeRef,
    ) -> EvalResult<VmValue> {
        // Check external properties first
        if let Some(ext_fn) = self.externals.get_property(base.type_name(), member) {
            return ext_fn(std::slice::from_ref(base), self, scope);
        }

        match base {
            VmValue::Object(obj) => {
                if let Some(member_obj) = obj.get_property_member(member) {
                    member_obj.force(|expr, scope| self.eval_expr(expr, scope))
                } else {
                    Err(EvalError::undefined_prop(member))
                }
            }
            VmValue::String(s) => {
                // String properties
                match member {
                    "length" => Ok(VmValue::Int(s.chars().count() as i64)),
                    "isEmpty" => Ok(VmValue::Boolean(s.is_empty())),
                    "isNotEmpty" => Ok(VmValue::Boolean(!s.is_empty())),
                    "isBlank" => Ok(VmValue::Boolean(s.trim().is_empty())),
                    "isNotBlank" => Ok(VmValue::Boolean(!s.trim().is_empty())),
                    "lastIndex" => {
                        let len = s.chars().count();
                        if len == 0 {
                            Ok(VmValue::Int(-1))
                        } else {
                            Ok(VmValue::Int(len as i64 - 1))
                        }
                    }
                    "chars" => {
                        // Return a List of single-character strings
                        let chars: Vec<VmValue> = s
                            .chars()
                            .map(|c| VmValue::String(Arc::from(c.to_string())))
                            .collect();
                        Ok(VmValue::list(chars))
                    }
                    "codePoints" => {
                        // Return a List of Unicode code points as integers
                        let codepoints: Vec<VmValue> =
                            s.chars().map(|c| VmValue::Int(c as i64)).collect();
                        Ok(VmValue::list(codepoints))
                    }
                    _ => Err(EvalError::undefined_prop(member)),
                }
            }
            VmValue::List(l) => match member {
                "length" => Ok(VmValue::Int(l.len() as i64)),
                "isEmpty" => Ok(VmValue::Boolean(l.is_empty())),
                "isNotEmpty" => Ok(VmValue::Boolean(!l.is_empty())),
                "first" => l.first().cloned().ok_or(EvalError::IndexOutOfBounds {
                    index: 0,
                    length: 0,
                }),
                "last" => l.last().cloned().ok_or(EvalError::IndexOutOfBounds {
                    index: 0,
                    length: 0,
                }),
                "rest" => {
                    if l.is_empty() {
                        Ok(VmValue::list(vec![]))
                    } else {
                        Ok(VmValue::list(l[1..].to_vec()))
                    }
                }
                "single" => {
                    if l.len() == 1 {
                        Ok(l[0].clone())
                    } else {
                        Err(EvalError::InvalidOperation(format!(
                            "Expected list with single element, got {} elements",
                            l.len()
                        )))
                    }
                }
                "lastIndex" => {
                    if l.is_empty() {
                        Ok(VmValue::Int(-1))
                    } else {
                        Ok(VmValue::Int(l.len() as i64 - 1))
                    }
                }
                "distinct" => {
                    // Remove duplicates preserving order
                    let mut seen = std::collections::HashSet::new();
                    let distinct: Vec<VmValue> = l
                        .iter()
                        .filter(|v| {
                            let hash = format!("{:?}", v);
                            seen.insert(hash)
                        })
                        .cloned()
                        .collect();
                    Ok(VmValue::list(distinct))
                }
                "isDistinct" => {
                    let mut seen = std::collections::HashSet::new();
                    let is_distinct = l.iter().all(|v| {
                        let hash = format!("{:?}", v);
                        seen.insert(hash)
                    });
                    Ok(VmValue::Boolean(is_distinct))
                }
                "min" => {
                    if l.is_empty() {
                        Err(EvalError::InvalidOperation(
                            "Cannot get min of empty list".to_string(),
                        ))
                    } else {
                        // For now, assume numeric comparison
                        let mut min = l[0].clone();
                        for v in l.iter().skip(1) {
                            if let (Some(a), Some(b)) = (v.as_float(), min.as_float()) {
                                if a < b {
                                    min = v.clone();
                                }
                            }
                        }
                        Ok(min)
                    }
                }
                "max" => {
                    if l.is_empty() {
                        Err(EvalError::InvalidOperation(
                            "Cannot get max of empty list".to_string(),
                        ))
                    } else {
                        let mut max = l[0].clone();
                        for v in l.iter().skip(1) {
                            if let (Some(a), Some(b)) = (v.as_float(), max.as_float()) {
                                if a > b {
                                    max = v.clone();
                                }
                            }
                        }
                        Ok(max)
                    }
                }
                _ => Err(EvalError::undefined_prop(member)),
            },
            VmValue::Map(m) => match member {
                "length" => Ok(VmValue::Int(m.len() as i64)),
                "isEmpty" => Ok(VmValue::Boolean(m.is_empty())),
                "keys" => Ok(VmValue::list(m.keys().cloned().collect())),
                "values" => Ok(VmValue::list(m.values().cloned().collect())),
                _ => Err(EvalError::undefined_prop(member)),
            },
            VmValue::Int(i) => {
                // Integer properties for Duration/DataSize creation
                match member {
                    "ns" => Ok(VmValue::Duration {
                        value: *i as f64,
                        unit: DurationUnit::Nanoseconds,
                    }),
                    "us" => Ok(VmValue::Duration {
                        value: *i as f64,
                        unit: DurationUnit::Microseconds,
                    }),
                    "ms" => Ok(VmValue::Duration {
                        value: *i as f64,
                        unit: DurationUnit::Milliseconds,
                    }),
                    "s" => Ok(VmValue::Duration {
                        value: *i as f64,
                        unit: DurationUnit::Seconds,
                    }),
                    "min" => Ok(VmValue::Duration {
                        value: *i as f64,
                        unit: DurationUnit::Minutes,
                    }),
                    "h" => Ok(VmValue::Duration {
                        value: *i as f64,
                        unit: DurationUnit::Hours,
                    }),
                    "d" => Ok(VmValue::Duration {
                        value: *i as f64,
                        unit: DurationUnit::Days,
                    }),
                    "b" => Ok(VmValue::DataSize {
                        value: *i as f64,
                        unit: DataSizeUnit::Bytes,
                    }),
                    "kb" => Ok(VmValue::DataSize {
                        value: *i as f64,
                        unit: DataSizeUnit::Kilobytes,
                    }),
                    "mb" => Ok(VmValue::DataSize {
                        value: *i as f64,
                        unit: DataSizeUnit::Megabytes,
                    }),
                    "gb" => Ok(VmValue::DataSize {
                        value: *i as f64,
                        unit: DataSizeUnit::Gigabytes,
                    }),
                    "tb" => Ok(VmValue::DataSize {
                        value: *i as f64,
                        unit: DataSizeUnit::Terabytes,
                    }),
                    "kib" => Ok(VmValue::DataSize {
                        value: *i as f64,
                        unit: DataSizeUnit::Kibibytes,
                    }),
                    "mib" => Ok(VmValue::DataSize {
                        value: *i as f64,
                        unit: DataSizeUnit::Mebibytes,
                    }),
                    "gib" => Ok(VmValue::DataSize {
                        value: *i as f64,
                        unit: DataSizeUnit::Gibibytes,
                    }),
                    "isPositive" => Ok(VmValue::Boolean(*i > 0)),
                    "isNegative" => Ok(VmValue::Boolean(*i < 0)),
                    "isZero" => Ok(VmValue::Boolean(*i == 0)),
                    "isEven" => Ok(VmValue::Boolean(*i % 2 == 0)),
                    "isOdd" => Ok(VmValue::Boolean(*i % 2 != 0)),
                    "abs" => Ok(VmValue::Int(i.abs())),
                    "sign" => Ok(VmValue::Int(i.signum())),
                    _ => Err(EvalError::undefined_prop(member)),
                }
            }
            VmValue::Float(f) => match member {
                "isPositive" => Ok(VmValue::Boolean(*f > 0.0)),
                "isNegative" => Ok(VmValue::Boolean(*f < 0.0)),
                "isZero" => Ok(VmValue::Boolean(*f == 0.0)),
                "isFinite" => Ok(VmValue::Boolean(f.is_finite())),
                "isInfinite" => Ok(VmValue::Boolean(f.is_infinite())),
                "isNaN" => Ok(VmValue::Boolean(f.is_nan())),
                "abs" => Ok(VmValue::Float(f.abs())),
                "sign" => Ok(VmValue::Float(f.signum())),
                "ceil" => Ok(VmValue::Float(f.ceil())),
                "floor" => Ok(VmValue::Float(f.floor())),
                "round" => Ok(VmValue::Float(f.round())),
                _ => Err(EvalError::undefined_prop(member)),
            },
            VmValue::Duration { value, unit } => match member {
                "value" => Ok(VmValue::Float(*value)),
                "unit" => Ok(VmValue::String(Arc::from(unit.suffix()))),
                "isPositive" => Ok(VmValue::Boolean(*value > 0.0)),
                "isNegative" => Ok(VmValue::Boolean(*value < 0.0)),
                "isZero" => Ok(VmValue::Boolean(*value == 0.0)),
                _ => Err(EvalError::undefined_prop(member)),
            },
            VmValue::DataSize { value, unit } => match member {
                "value" => Ok(VmValue::Float(*value)),
                "unit" => Ok(VmValue::String(Arc::from(unit.suffix()))),
                "isPositive" => Ok(VmValue::Boolean(*value > 0.0)),
                "isNegative" => Ok(VmValue::Boolean(*value < 0.0)),
                "isZero" => Ok(VmValue::Boolean(*value == 0.0)),
                _ => Err(EvalError::undefined_prop(member)),
            },
            _ => Err(EvalError::InvalidOperation(format!(
                "Cannot access member '{}' on {}",
                member,
                base.type_name()
            ))),
        }
    }

    /// Evaluate a binary operation
    fn eval_binary_op(
        &self,
        op: BinaryOp,
        left: &Expr,
        right: &Expr,
        scope: &ScopeRef,
    ) -> EvalResult<VmValue> {
        // Short-circuit evaluation for logical operators and null coalescing
        match op {
            BinaryOp::And => {
                let l = self.eval_expr(left, scope)?;
                if !l.is_truthy() {
                    return Ok(VmValue::Boolean(false));
                }
                let r = self.eval_expr(right, scope)?;
                return Ok(VmValue::Boolean(r.is_truthy()));
            }
            BinaryOp::Or => {
                let l = self.eval_expr(left, scope)?;
                if l.is_truthy() {
                    return Ok(VmValue::Boolean(true));
                }
                let r = self.eval_expr(right, scope)?;
                return Ok(VmValue::Boolean(r.is_truthy()));
            }
            BinaryOp::NullCoalesce => {
                let l = self.eval_expr(left, scope)?;
                if !matches!(l, VmValue::Null) {
                    return Ok(l);
                }
                return self.eval_expr(right, scope);
            }
            _ => {}
        }

        let lval = self.eval_expr(left, scope)?;
        let rval = self.eval_expr(right, scope)?;

        match op {
            BinaryOp::Add => self.eval_add(&lval, &rval),
            BinaryOp::Sub => self.eval_sub(&lval, &rval),
            BinaryOp::Mul => self.eval_mul(&lval, &rval),
            BinaryOp::Div => self.eval_div(&lval, &rval),
            BinaryOp::IntDiv => self.eval_int_div(&lval, &rval),
            BinaryOp::Mod => self.eval_mod(&lval, &rval),
            BinaryOp::Pow => self.eval_pow(&lval, &rval),
            BinaryOp::Eq => Ok(VmValue::Boolean(lval == rval)),
            BinaryOp::Ne => Ok(VmValue::Boolean(lval != rval)),
            BinaryOp::Lt => self.eval_comparison(&lval, &rval, |ord| ord.is_lt()),
            BinaryOp::Le => self.eval_comparison(&lval, &rval, |ord| ord.is_le()),
            BinaryOp::Gt => self.eval_comparison(&lval, &rval, |ord| ord.is_gt()),
            BinaryOp::Ge => self.eval_comparison(&lval, &rval, |ord| ord.is_ge()),
            BinaryOp::And | BinaryOp::Or | BinaryOp::NullCoalesce => unreachable!(), // Handled above
            BinaryOp::Pipe => {
                // Pipe operator: x |> f means f(x)
                match &rval {
                    VmValue::Lambda(lambda) => {
                        let params: Vec<(String, VmValue)> = lambda
                            .params
                            .first()
                            .map(|p| vec![(p.clone(), lval.clone())])
                            .unwrap_or_default();
                        let lambda_scope = Scope::for_lambda(&lambda.captured_scope, params);
                        self.eval_expr(&lambda.body, &lambda_scope)
                    }
                    _ => Err(EvalError::type_error("Function", rval.type_name())),
                }
            }
            BinaryOp::Is => {
                // Type check - basic implementation
                // In full PKL this would check against actual types
                Ok(VmValue::Boolean(true)) // TODO: implement proper type checking
            }
            BinaryOp::As => {
                // Type cast - basic implementation
                // In full PKL this would perform type casting
                Ok(lval) // TODO: implement proper type casting
            }
        }
    }

    fn eval_add(&self, l: &VmValue, r: &VmValue) -> EvalResult<VmValue> {
        match (l, r) {
            (VmValue::Int(a), VmValue::Int(b)) => Ok(VmValue::Int(a + b)),
            (VmValue::Float(a), VmValue::Float(b)) => Ok(VmValue::Float(a + b)),
            (VmValue::Int(a), VmValue::Float(b)) => Ok(VmValue::Float(*a as f64 + b)),
            (VmValue::Float(a), VmValue::Int(b)) => Ok(VmValue::Float(a + *b as f64)),
            (VmValue::String(a), VmValue::String(b)) => Ok(VmValue::string(format!("{}{}", a, b))),
            (VmValue::List(a), VmValue::List(b)) => {
                let mut result = (**a).clone();
                result.extend((**b).iter().cloned());
                Ok(VmValue::list(result))
            }
            // Duration + Duration
            (
                VmValue::Duration {
                    value: v1,
                    unit: u1,
                },
                VmValue::Duration {
                    value: v2,
                    unit: u2,
                },
            ) => {
                // Convert both to nanoseconds, add, convert back to first unit
                let ns1 = v1 * u1.to_nanos_factor();
                let ns2 = v2 * u2.to_nanos_factor();
                let result = (ns1 + ns2) / u1.to_nanos_factor();
                Ok(VmValue::Duration {
                    value: result,
                    unit: *u1,
                })
            }
            // DataSize + DataSize
            (
                VmValue::DataSize {
                    value: v1,
                    unit: u1,
                },
                VmValue::DataSize {
                    value: v2,
                    unit: u2,
                },
            ) => {
                // Convert both to bytes, add, convert back to first unit
                let b1 = v1 * u1.to_bytes_factor();
                let b2 = v2 * u2.to_bytes_factor();
                let result = (b1 + b2) / u1.to_bytes_factor();
                Ok(VmValue::DataSize {
                    value: result,
                    unit: *u1,
                })
            }
            _ => Err(EvalError::type_error(
                "numeric or string",
                format!("{} and {}", l.type_name(), r.type_name()),
            )),
        }
    }

    fn eval_sub(&self, l: &VmValue, r: &VmValue) -> EvalResult<VmValue> {
        match (l, r) {
            (VmValue::Int(a), VmValue::Int(b)) => Ok(VmValue::Int(a - b)),
            (VmValue::Float(a), VmValue::Float(b)) => Ok(VmValue::Float(a - b)),
            (VmValue::Int(a), VmValue::Float(b)) => Ok(VmValue::Float(*a as f64 - b)),
            (VmValue::Float(a), VmValue::Int(b)) => Ok(VmValue::Float(a - *b as f64)),
            // Duration - Duration
            (
                VmValue::Duration {
                    value: v1,
                    unit: u1,
                },
                VmValue::Duration {
                    value: v2,
                    unit: u2,
                },
            ) => {
                let ns1 = v1 * u1.to_nanos_factor();
                let ns2 = v2 * u2.to_nanos_factor();
                let result = (ns1 - ns2) / u1.to_nanos_factor();
                Ok(VmValue::Duration {
                    value: result,
                    unit: *u1,
                })
            }
            // DataSize - DataSize
            (
                VmValue::DataSize {
                    value: v1,
                    unit: u1,
                },
                VmValue::DataSize {
                    value: v2,
                    unit: u2,
                },
            ) => {
                let b1 = v1 * u1.to_bytes_factor();
                let b2 = v2 * u2.to_bytes_factor();
                let result = (b1 - b2) / u1.to_bytes_factor();
                Ok(VmValue::DataSize {
                    value: result,
                    unit: *u1,
                })
            }
            _ => Err(EvalError::type_error(
                "numeric",
                format!("{} and {}", l.type_name(), r.type_name()),
            )),
        }
    }

    fn eval_mul(&self, l: &VmValue, r: &VmValue) -> EvalResult<VmValue> {
        match (l, r) {
            (VmValue::Int(a), VmValue::Int(b)) => Ok(VmValue::Int(a * b)),
            (VmValue::Float(a), VmValue::Float(b)) => Ok(VmValue::Float(a * b)),
            (VmValue::Int(a), VmValue::Float(b)) => Ok(VmValue::Float(*a as f64 * b)),
            (VmValue::Float(a), VmValue::Int(b)) => Ok(VmValue::Float(a * *b as f64)),
            (VmValue::String(s), VmValue::Int(n)) | (VmValue::Int(n), VmValue::String(s)) => {
                Ok(VmValue::string(s.repeat(*n as usize)))
            }
            // Duration * Number
            (VmValue::Duration { value, unit }, VmValue::Int(n)) => Ok(VmValue::Duration {
                value: value * (*n as f64),
                unit: *unit,
            }),
            (VmValue::Duration { value, unit }, VmValue::Float(n)) => Ok(VmValue::Duration {
                value: value * n,
                unit: *unit,
            }),
            // Number * Duration
            (VmValue::Int(n), VmValue::Duration { value, unit }) => Ok(VmValue::Duration {
                value: (*n as f64) * value,
                unit: *unit,
            }),
            (VmValue::Float(n), VmValue::Duration { value, unit }) => Ok(VmValue::Duration {
                value: n * value,
                unit: *unit,
            }),
            // DataSize * Number
            (VmValue::DataSize { value, unit }, VmValue::Int(n)) => Ok(VmValue::DataSize {
                value: value * (*n as f64),
                unit: *unit,
            }),
            (VmValue::DataSize { value, unit }, VmValue::Float(n)) => Ok(VmValue::DataSize {
                value: value * n,
                unit: *unit,
            }),
            // Number * DataSize
            (VmValue::Int(n), VmValue::DataSize { value, unit }) => Ok(VmValue::DataSize {
                value: (*n as f64) * value,
                unit: *unit,
            }),
            (VmValue::Float(n), VmValue::DataSize { value, unit }) => Ok(VmValue::DataSize {
                value: n * value,
                unit: *unit,
            }),
            _ => Err(EvalError::type_error(
                "numeric",
                format!("{} and {}", l.type_name(), r.type_name()),
            )),
        }
    }

    fn eval_div(&self, l: &VmValue, r: &VmValue) -> EvalResult<VmValue> {
        match (l, r) {
            (VmValue::Int(a), VmValue::Int(b)) => {
                if *b == 0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(VmValue::Float(*a as f64 / *b as f64))
                }
            }
            (VmValue::Float(a), VmValue::Float(b)) => {
                if *b == 0.0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(VmValue::Float(a / b))
                }
            }
            (VmValue::Int(a), VmValue::Float(b)) => {
                if *b == 0.0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(VmValue::Float(*a as f64 / b))
                }
            }
            (VmValue::Float(a), VmValue::Int(b)) => {
                if *b == 0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(VmValue::Float(a / *b as f64))
                }
            }
            // Duration / Number
            (VmValue::Duration { value, unit }, VmValue::Int(n)) => {
                if *n == 0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(VmValue::Duration {
                        value: value / (*n as f64),
                        unit: *unit,
                    })
                }
            }
            (VmValue::Duration { value, unit }, VmValue::Float(n)) => {
                if *n == 0.0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(VmValue::Duration {
                        value: value / n,
                        unit: *unit,
                    })
                }
            }
            // Duration / Duration returns a Float (ratio)
            (
                VmValue::Duration {
                    value: v1,
                    unit: u1,
                },
                VmValue::Duration {
                    value: v2,
                    unit: u2,
                },
            ) => {
                let ns1 = v1 * u1.to_nanos_factor();
                let ns2 = v2 * u2.to_nanos_factor();
                if ns2 == 0.0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(VmValue::Float(ns1 / ns2))
                }
            }
            // DataSize / Number
            (VmValue::DataSize { value, unit }, VmValue::Int(n)) => {
                if *n == 0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(VmValue::DataSize {
                        value: value / (*n as f64),
                        unit: *unit,
                    })
                }
            }
            (VmValue::DataSize { value, unit }, VmValue::Float(n)) => {
                if *n == 0.0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(VmValue::DataSize {
                        value: value / n,
                        unit: *unit,
                    })
                }
            }
            // DataSize / DataSize returns a Float (ratio)
            (
                VmValue::DataSize {
                    value: v1,
                    unit: u1,
                },
                VmValue::DataSize {
                    value: v2,
                    unit: u2,
                },
            ) => {
                let b1 = v1 * u1.to_bytes_factor();
                let b2 = v2 * u2.to_bytes_factor();
                if b2 == 0.0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(VmValue::Float(b1 / b2))
                }
            }
            _ => Err(EvalError::type_error(
                "numeric",
                format!("{} and {}", l.type_name(), r.type_name()),
            )),
        }
    }

    fn eval_int_div(&self, l: &VmValue, r: &VmValue) -> EvalResult<VmValue> {
        match (l, r) {
            (VmValue::Int(a), VmValue::Int(b)) => {
                if *b == 0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(VmValue::Int(a / b))
                }
            }
            _ => Err(EvalError::type_error(
                "Int",
                format!("{} and {}", l.type_name(), r.type_name()),
            )),
        }
    }

    fn eval_mod(&self, l: &VmValue, r: &VmValue) -> EvalResult<VmValue> {
        match (l, r) {
            (VmValue::Int(a), VmValue::Int(b)) => {
                if *b == 0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(VmValue::Int(a % b))
                }
            }
            (VmValue::Float(a), VmValue::Float(b)) => {
                if *b == 0.0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(VmValue::Float(a % b))
                }
            }
            _ => Err(EvalError::type_error(
                "numeric",
                format!("{} and {}", l.type_name(), r.type_name()),
            )),
        }
    }

    fn eval_pow(&self, l: &VmValue, r: &VmValue) -> EvalResult<VmValue> {
        match (l, r) {
            (VmValue::Int(a), VmValue::Int(b)) => {
                if *b >= 0 {
                    Ok(VmValue::Int(a.pow(*b as u32)))
                } else {
                    Ok(VmValue::Float((*a as f64).powi(*b as i32)))
                }
            }
            (VmValue::Float(a), VmValue::Float(b)) => Ok(VmValue::Float(a.powf(*b))),
            (VmValue::Int(a), VmValue::Float(b)) => Ok(VmValue::Float((*a as f64).powf(*b))),
            (VmValue::Float(a), VmValue::Int(b)) => Ok(VmValue::Float(a.powi(*b as i32))),
            _ => Err(EvalError::type_error(
                "numeric",
                format!("{} and {}", l.type_name(), r.type_name()),
            )),
        }
    }

    fn eval_comparison<F>(&self, l: &VmValue, r: &VmValue, f: F) -> EvalResult<VmValue>
    where
        F: Fn(std::cmp::Ordering) -> bool,
    {
        match l.partial_cmp(r) {
            Some(ord) => Ok(VmValue::Boolean(f(ord))),
            None => Err(EvalError::type_error(
                "comparable types",
                format!("{} and {}", l.type_name(), r.type_name()),
            )),
        }
    }

    /// Evaluate unary operation
    fn eval_unary_op(&self, op: UnaryOp, value: VmValue) -> EvalResult<VmValue> {
        match op {
            UnaryOp::Neg => match value {
                VmValue::Int(i) => Ok(VmValue::Int(-i)),
                VmValue::Float(f) => Ok(VmValue::Float(-f)),
                _ => Err(EvalError::type_error("numeric", value.type_name())),
            },
            UnaryOp::Not => match value {
                VmValue::Boolean(b) => Ok(VmValue::Boolean(!b)),
                _ => Ok(VmValue::Boolean(!value.is_truthy())),
            },
        }
    }

    /// Evaluate subscript access
    fn eval_subscript(
        &self,
        base: &VmValue,
        index: &VmValue,
        _scope: &ScopeRef,
    ) -> EvalResult<VmValue> {
        match base {
            VmValue::List(list) => {
                let idx = index
                    .as_int()
                    .ok_or_else(|| EvalError::type_error("Int", index.type_name()))?;
                let len = list.len();
                let actual_idx = if idx < 0 { len as i64 + idx } else { idx };
                if actual_idx < 0 || actual_idx >= len as i64 {
                    Err(EvalError::IndexOutOfBounds {
                        index: idx,
                        length: len,
                    })
                } else {
                    Ok(list[actual_idx as usize].clone())
                }
            }
            VmValue::Map(map) => map
                .get(index)
                .cloned()
                .ok_or_else(|| EvalError::UndefinedProperty(index.to_string())),
            VmValue::String(s) => {
                let idx = index
                    .as_int()
                    .ok_or_else(|| EvalError::type_error("Int", index.type_name()))?;
                let chars: Vec<char> = s.chars().collect();
                let len = chars.len();
                let actual_idx = if idx < 0 { len as i64 + idx } else { idx };
                if actual_idx < 0 || actual_idx >= len as i64 {
                    Err(EvalError::IndexOutOfBounds {
                        index: idx,
                        length: len,
                    })
                } else {
                    Ok(VmValue::string(chars[actual_idx as usize].to_string()))
                }
            }
            VmValue::Object(obj) => {
                match index {
                    VmValue::Int(idx) => {
                        // Element access
                        if let Some(member) = obj.get_element_member(*idx as usize) {
                            member.force(|expr, scope| self.eval_expr(expr, scope))
                        } else {
                            Err(EvalError::IndexOutOfBounds {
                                index: *idx,
                                length: obj.element_count(),
                            })
                        }
                    }
                    _ => {
                        // Entry access
                        if let Some(member) = obj.get_entry_member(index) {
                            member.force(|expr, scope| self.eval_expr(expr, scope))
                        } else {
                            Err(EvalError::UndefinedProperty(index.to_string()))
                        }
                    }
                }
            }
            _ => Err(EvalError::NotSubscriptable(base.type_name().to_string())),
        }
    }

    /// Evaluate function call
    fn eval_call(
        &self,
        callee: &VmValue,
        args: &[VmValue],
        scope: &ScopeRef,
    ) -> EvalResult<VmValue> {
        match callee {
            VmValue::Lambda(closure) => {
                if args.len() != closure.params.len() {
                    return Err(EvalError::WrongArgCount {
                        expected: closure.params.len(),
                        actual: args.len(),
                    });
                }
                let bindings: Vec<(String, VmValue)> = closure
                    .params
                    .iter()
                    .cloned()
                    .zip(args.iter().cloned())
                    .collect();
                let new_scope = Scope::for_lambda(&closure.captured_scope, bindings);
                self.eval_expr(&closure.body, &new_scope)
            }
            VmValue::ExternalFunc { func, num_params } => {
                // usize::MAX indicates a variadic function
                if *num_params != usize::MAX && args.len() != *num_params {
                    return Err(EvalError::WrongArgCount {
                        expected: *num_params,
                        actual: args.len(),
                    });
                }
                // Call the external function
                func(args, self, scope)
            }
            _ => Err(EvalError::NotCallable(callee.type_name().to_string())),
        }
    }

    /// Create a new object from body
    fn eval_new_object(
        &self,
        kind: ObjectKind,
        body: &ObjectBody,
        scope: &ScopeRef,
    ) -> EvalResult<VmValue> {
        self.eval_new_object_with_class(kind, None, body, scope)
    }

    /// Evaluate new object creation with optional class definition
    ///
    /// `class_info` contains both the class definition and the module scope where the class
    /// was defined. This is important because class methods need access to imports from their
    /// defining module, not the instantiation site.
    fn eval_new_object_with_class(
        &self,
        kind: ObjectKind,
        class_info: Option<(Arc<ClassDef>, ScopeRef)>,
        body: &ObjectBody,
        scope: &ScopeRef,
    ) -> EvalResult<VmValue> {
        let obj = match &kind {
            ObjectKind::Dynamic => VmObject::new_dynamic(Arc::clone(scope)),
            ObjectKind::Listing => VmObject::new_listing(Arc::clone(scope)),
            ObjectKind::Mapping => VmObject::new_mapping(Arc::clone(scope)),
            ObjectKind::Typed(ref name) => VmObject::new_typed(name.clone(), Arc::clone(scope)),
        };
        let obj = Arc::new(obj);
        let obj_scope = Scope::with_this(scope, Arc::clone(&obj));

        // First, apply class default properties if there's a class definition
        // Use the class definition's module scope so methods can access imports from
        // the module where the class was defined
        if let Some((class_def, class_scope)) = &class_info {
            // Create a scope that has the new object as `this` but uses the class
            // definition's module for resolving imports
            let class_obj_scope = Scope::with_this(class_scope, Arc::clone(&obj));
            self.apply_class_defaults(&obj, class_def, &class_obj_scope)?;
        }

        // Then apply the body members (which can override class defaults)
        // This uses the caller's scope so instantiation-site code can access local variables
        self.populate_object(&obj, body, &obj_scope)?;
        self.force_object(&obj)?;

        Ok(VmValue::Object(obj))
    }

    /// Apply class default property values to an object
    fn apply_class_defaults(
        &self,
        obj: &Arc<VmObject>,
        class: &ClassDef,
        scope: &ScopeRef,
    ) -> EvalResult<()> {
        use rpkl_parser::ClassMember;

        // If class extends another class, apply parent defaults first
        // Use the parent class's module scope so its methods can access its imports
        if let Some(parent_name) = &class.extends {
            let parent_type_name = parent_name.to_string();
            if let Some((parent_class, parent_scope)) =
                self.classes.borrow().get(&parent_type_name).cloned()
            {
                // Create a scope with the parent's module but the new object as `this`
                let parent_obj_scope = Scope::with_this(&parent_scope, Arc::clone(obj));
                self.apply_class_defaults(obj, &parent_class, &parent_obj_scope)?;
            }
        }

        // Apply this class's properties
        for member in &class.members {
            match member {
                ClassMember::Property(prop) => {
                    let name = prop.name.node.clone();
                    // Extract type hint from type annotation
                    let type_hint = prop
                        .ty
                        .as_ref()
                        .and_then(|ty| self.extract_base_type_name(ty));
                    // Create metadata from property modifiers
                    let metadata = MemberMetadata {
                        is_hidden: prop.modifiers.is_hidden,
                        is_local: prop.modifiers.is_local,
                        type_hint,
                    };
                    if let Some(value) = &prop.value {
                        // Property has a default value
                        match value {
                            PropertyValue::Expr(expr) => {
                                let m = ObjMember::new_with_metadata(
                                    expr.clone(),
                                    Arc::clone(scope),
                                    metadata,
                                );
                                obj.add_property(name, m);
                            }
                            PropertyValue::Object(body) => {
                                let nested = self.eval_object_body(body, scope)?;
                                let m = ObjMember::with_value_and_metadata(nested, metadata);
                                obj.add_property(name, m);
                            }
                        }
                    } else if prop.ty.is_some() {
                        // Property has type annotation but no default value
                        // Add it as null so methods can access it
                        // The type_hint is already stored in metadata
                        let m = ObjMember::with_value_and_metadata(VmValue::Null, metadata);
                        obj.add_property(name, m);
                    }
                }
                ClassMember::Method(method) => {
                    // Create a lambda for the method
                    if let Some(body_expr) = &method.body {
                        let params: Vec<String> =
                            method.params.iter().map(|p| p.name.node.clone()).collect();
                        let closure = LambdaClosure {
                            params,
                            body: body_expr.clone(),
                            captured_scope: Arc::clone(scope),
                        };
                        let m = ObjMember::with_value(VmValue::Lambda(Arc::new(closure)));
                        obj.add_property(method.name.node.clone(), m);
                    }
                }
            }
        }

        Ok(())
    }

    /// Evaluate amendment
    fn eval_amendment(
        &self,
        base: VmValue,
        body: &ObjectBody,
        scope: &ScopeRef,
    ) -> EvalResult<VmValue> {
        self.eval_amendment_with_type_hint(base, body, scope, None)
    }

    /// Evaluate amendment with an optional type hint
    ///
    /// When amending a null value, if a type hint is provided, we'll create
    /// an instance of that type with class defaults applied. This is important
    /// for nullable typed properties like `about: Recipe.About?`.
    fn eval_amendment_with_type_hint(
        &self,
        base: VmValue,
        body: &ObjectBody,
        scope: &ScopeRef,
        type_hint: Option<String>,
    ) -> EvalResult<VmValue> {
        match base {
            VmValue::Object(base_obj) => {
                let amended = Arc::new(base_obj.amend(Arc::clone(scope)));
                let obj_scope = Scope::with_this(scope, Arc::clone(&amended));

                self.populate_object(&amended, body, &obj_scope)?;
                self.force_object(&amended)?;

                Ok(VmValue::Object(amended))
            }
            VmValue::Null => {
                // When amending null, create a new object
                // If we have a type hint and it's a known class, create a typed object
                if let Some(type_name) = type_hint {
                    // Try the full type name first, then try without module prefix
                    // e.g., "Recipe.About" -> try "Recipe.About", then "About"
                    let simple_name = type_name
                        .rsplit('.')
                        .next()
                        .unwrap_or(&type_name)
                        .to_string();

                    // Check if this type is a known class
                    let class_info = self
                        .classes
                        .borrow()
                        .get(&type_name)
                        .cloned()
                        .or_else(|| self.classes.borrow().get(&simple_name).cloned());

                    if let Some((class_def, class_scope)) = class_info {
                        // Create a typed object with class defaults
                        let new_obj = Arc::new(VmObject::new_typed(type_name, Arc::clone(scope)));
                        let class_obj_scope = Scope::with_this(&class_scope, Arc::clone(&new_obj));
                        self.apply_class_defaults(&new_obj, &class_def, &class_obj_scope)?;

                        let obj_scope = Scope::with_this(scope, Arc::clone(&new_obj));
                        self.populate_object(&new_obj, body, &obj_scope)?;
                        self.force_object(&new_obj)?;

                        return Ok(VmValue::Object(new_obj));
                    }
                }

                // No type hint or unknown class - infer from body contents
                let kind = self.infer_object_kind_from_body(body);
                let new_obj = Arc::new(match kind {
                    ObjectKind::Listing => VmObject::new_listing(Arc::clone(scope)),
                    ObjectKind::Mapping => VmObject::new_mapping(Arc::clone(scope)),
                    _ => VmObject::new_dynamic(Arc::clone(scope)),
                });
                let obj_scope = Scope::with_this(scope, Arc::clone(&new_obj));

                self.populate_object(&new_obj, body, &obj_scope)?;
                self.force_object(&new_obj)?;

                Ok(VmValue::Object(new_obj))
            }
            _ => Err(EvalError::type_error("Object", base.type_name())),
        }
    }

    /// Extract the base type name from a type annotation
    ///
    /// For nullable types like `Recipe.About?`, returns "Recipe.About".
    /// For parameterized types, returns the base type.
    /// For union types or other complex types, returns None.
    fn extract_base_type_name(&self, ty: &rpkl_parser::TypeAnnotation) -> Option<String> {
        use rpkl_parser::TypeKind;
        match &ty.kind {
            TypeKind::Named(name) => Some(name.to_string()),
            TypeKind::Nullable(inner) => self.extract_base_type_name(inner),
            TypeKind::Parameterized { base, .. } => Some(base.to_string()),
            _ => None,
        }
    }

    /// Populate an object with members from body
    fn populate_object(
        &self,
        obj: &Arc<VmObject>,
        body: &ObjectBody,
        scope: &ScopeRef,
    ) -> EvalResult<()> {
        for member in &body.members {
            self.add_object_member(obj, member, scope)?;
        }
        Ok(())
    }

    /// Add a single member to an object
    fn add_object_member(
        &self,
        obj: &Arc<VmObject>,
        member: &ObjectMember,
        scope: &ScopeRef,
    ) -> EvalResult<()> {
        match member {
            ObjectMember::Property { name, value, .. } => {
                let m = ObjMember::new(value.clone(), Arc::clone(scope));
                obj.add_property(name.node.clone(), m);
            }
            ObjectMember::PropertyAmend { name, body, .. } => {
                // Amend existing property
                if let Some(existing) = obj.get_property_member(&name.node) {
                    let base_value = existing.force(|expr, scope| self.eval_expr(expr, scope))?;
                    // Get type hint from existing member if available
                    let type_hint = existing.metadata().type_hint.clone();
                    let amended =
                        self.eval_amendment_with_type_hint(base_value, body, scope, type_hint)?;
                    let m = ObjMember::with_value(amended);
                    obj.add_property(name.node.clone(), m);
                } else {
                    // No existing property - infer the object kind from body contents
                    // This handles typed properties like `enable: Listing<String>?`
                    // where no default value exists
                    let kind = self.infer_object_kind_from_body(body);
                    let new_obj = self.eval_new_object(kind, body, scope)?;
                    let m = ObjMember::with_value(new_obj);
                    obj.add_property(name.node.clone(), m);
                }
            }
            ObjectMember::Element { value, .. } => {
                let m = ObjMember::new(value.clone(), Arc::clone(scope));
                obj.add_element(m);
            }
            ObjectMember::Entry { key, value, .. } => {
                let key_value = self.eval_expr(key, scope)?;
                let m = ObjMember::new(value.clone(), Arc::clone(scope));
                obj.add_entry(key_value, m);
            }
            ObjectMember::EntryAmend { key, body, .. } => {
                let key_value = self.eval_expr(key, scope)?;
                if let Some(existing) = obj.get_entry_member(&key_value) {
                    let base_value = existing.force(|expr, scope| self.eval_expr(expr, scope))?;
                    let amended = self.eval_amendment(base_value, body, scope)?;
                    let m = ObjMember::with_value(amended);
                    obj.add_entry(key_value, m);
                } else {
                    let new_obj = self.eval_object_body(body, scope)?;
                    let m = ObjMember::with_value(new_obj);
                    obj.add_entry(key_value, m);
                }
            }
            ObjectMember::Spread {
                value, is_nullable, ..
            } => {
                let spread_value = self.eval_expr(value, scope)?;
                if spread_value.is_null() {
                    if !is_nullable {
                        return Err(EvalError::NullPointer("Spread of null value".to_string()));
                    }
                } else if let VmValue::Object(spread_obj) = spread_value {
                    // Copy properties
                    for name in spread_obj.property_names() {
                        if let Some(member) = spread_obj.get_property_member(&name) {
                            let value = member.force(|expr, scope| self.eval_expr(expr, scope))?;
                            obj.add_property(name, ObjMember::with_value(value));
                        }
                    }
                    // Copy elements
                    for i in 0..spread_obj.element_count() {
                        if let Some(member) = spread_obj.get_element_member(i) {
                            let value = member.force(|expr, scope| self.eval_expr(expr, scope))?;
                            obj.add_element(ObjMember::with_value(value));
                        }
                    }
                    // Copy entries
                    for key in spread_obj.entry_keys() {
                        if let Some(member) = spread_obj.get_entry_member(&key) {
                            let value = member.force(|expr, scope| self.eval_expr(expr, scope))?;
                            obj.add_entry(key, ObjMember::with_value(value));
                        }
                    }
                } else {
                    return Err(EvalError::type_error("Object", spread_value.type_name()));
                }
            }
            ObjectMember::For {
                key_var,
                value_var,
                iterable,
                body,
                ..
            } => {
                let iter_value = self.eval_expr(iterable, scope)?;
                self.eval_for_generator(obj, key_var, value_var, &iter_value, body, scope)?;
            }
            ObjectMember::When {
                condition,
                body,
                else_body,
                ..
            } => {
                let cond = self.eval_expr(condition, scope)?;
                if cond.is_truthy() {
                    self.populate_object(obj, body, scope)?;
                } else if let Some(else_b) = else_body {
                    self.populate_object(obj, else_b, scope)?;
                }
            }
        }
        Ok(())
    }

    /// Evaluate a for generator
    fn eval_for_generator(
        &self,
        obj: &Arc<VmObject>,
        key_var: &Option<rpkl_parser::Identifier>,
        value_var: &rpkl_parser::Identifier,
        iterable: &VmValue,
        body: &ObjectBody,
        scope: &ScopeRef,
    ) -> EvalResult<()> {
        match iterable {
            VmValue::List(items) => {
                for (idx, item) in items.iter().enumerate() {
                    let mut bindings = vec![(value_var.node.clone(), item.clone())];
                    if let Some(key) = key_var {
                        bindings.push((key.node.clone(), VmValue::Int(idx as i64)));
                    }
                    let iter_scope = Scope::with_locals(scope, bindings);
                    self.populate_object(obj, body, &iter_scope)?;
                }
            }
            VmValue::Set(items) => {
                for (idx, item) in items.iter().enumerate() {
                    let mut bindings = vec![(value_var.node.clone(), item.clone())];
                    if let Some(key) = key_var {
                        bindings.push((key.node.clone(), VmValue::Int(idx as i64)));
                    }
                    let iter_scope = Scope::with_locals(scope, bindings);
                    self.populate_object(obj, body, &iter_scope)?;
                }
            }
            VmValue::Map(items) => {
                for (k, v) in items.iter() {
                    let mut bindings = vec![(value_var.node.clone(), v.clone())];
                    if let Some(key) = key_var {
                        bindings.push((key.node.clone(), k.clone()));
                    }
                    let iter_scope = Scope::with_locals(scope, bindings);
                    self.populate_object(obj, body, &iter_scope)?;
                }
            }
            VmValue::Object(iter_obj) => {
                // Iterate over elements
                for i in 0..iter_obj.element_count() {
                    if let Some(member) = iter_obj.get_element_member(i) {
                        let item = member.force(|expr, scope| self.eval_expr(expr, scope))?;
                        let mut bindings = vec![(value_var.node.clone(), item)];
                        if let Some(key) = key_var {
                            bindings.push((key.node.clone(), VmValue::Int(i as i64)));
                        }
                        let iter_scope = Scope::with_locals(scope, bindings);
                        self.populate_object(obj, body, &iter_scope)?;
                    }
                }
            }
            VmValue::IntSeq { start, end, step } => {
                let mut idx = 0i64;
                let mut current = *start;
                while (step > &0 && current < *end) || (step < &0 && current > *end) {
                    let mut bindings = vec![(value_var.node.clone(), VmValue::Int(current))];
                    if let Some(key) = key_var {
                        bindings.push((key.node.clone(), VmValue::Int(idx)));
                    }
                    let iter_scope = Scope::with_locals(scope, bindings);
                    self.populate_object(obj, body, &iter_scope)?;
                    current += step;
                    idx += 1;
                }
            }
            _ => {
                return Err(EvalError::NotIterable(iterable.type_name().to_string()));
            }
        }
        Ok(())
    }

    /// Evaluate an object body and return the value
    fn eval_object_body(&self, body: &ObjectBody, scope: &ScopeRef) -> EvalResult<VmValue> {
        self.eval_new_object(ObjectKind::Dynamic, body, scope)
    }

    /// Force evaluation of all object members
    fn force_object(&self, obj: &Arc<VmObject>) -> EvalResult<()> {
        self.force_object_with_module(obj, None)
    }

    /// Force evaluation of all object members with an optional module override
    ///
    /// When `module_override` is Some, inherited properties will be evaluated
    /// with that module instead of their stored module. This is used when
    /// evaluating inherited properties in an amended module.
    fn force_object_with_module(
        &self,
        obj: &Arc<VmObject>,
        module_override: Option<&Arc<VmObject>>,
    ) -> EvalResult<()> {
        // Force all properties
        for name in obj.property_names() {
            if let Some(member) = obj.get_property_member(&name) {
                let is_local = obj.has_local_property(&name);
                let value = if is_local {
                    // Local property - use stored scope
                    member.force(|expr, scope| self.eval_expr(expr, scope))?
                } else if let Some(module) = module_override {
                    // Inherited property with module override
                    member.force_with_module(Arc::clone(module), |expr, scope| {
                        self.eval_expr(expr, scope)
                    })?
                } else {
                    // Inherited property without override
                    member.force(|expr, scope| self.eval_expr(expr, scope))?
                };

                // If the value is an object, recursively force its members.
                // Pass the module override so nested objects also see the correct module.
                if let VmValue::Object(nested_obj) = &value {
                    self.force_object_with_module(nested_obj, module_override)?;
                }
            }
        }

        // Force all elements
        for i in 0..obj.element_count() {
            if let Some(member) = obj.get_element_member(i) {
                let value = member.force(|expr, scope| self.eval_expr(expr, scope))?;
                // Recursively force nested objects with module override
                if let VmValue::Object(nested_obj) = &value {
                    self.force_object_with_module(nested_obj, module_override)?;
                }
            }
        }

        // Force all entries
        for key in obj.entry_keys() {
            if let Some(member) = obj.get_entry_member(&key) {
                let value = member.force(|expr, scope| self.eval_expr(expr, scope))?;
                // Recursively force nested objects with module override
                if let VmValue::Object(nested_obj) = &value {
                    self.force_object_with_module(nested_obj, module_override)?;
                }
            }
        }

        Ok(())
    }

    /// Check if a value matches a type annotation
    fn check_type(&self, value: &VmValue, ty: &rpkl_parser::TypeAnnotation) -> bool {
        use rpkl_parser::TypeKind;
        match &ty.kind {
            TypeKind::Named(name) => {
                let type_name = name.to_string();
                self.check_named_type(value, &type_name)
            }
            TypeKind::Parameterized { base, args } => {
                let base_name = base.to_string();
                self.check_parameterized_type(value, &base_name, args)
            }
            TypeKind::Nullable(inner) => {
                matches!(value, VmValue::Null) || self.check_type(value, inner)
            }
            TypeKind::Union(types) => types.iter().any(|t| self.check_type(value, t)),
            TypeKind::Constrained { base, constraint } => {
                // First check the base type
                if !self.check_type(value, base) {
                    return false;
                }
                // Then evaluate the constraint with `this` bound to the value
                self.check_constraint(value, constraint)
            }
            TypeKind::Function {
                params,
                return_type: _,
            } => {
                // Check if value is a lambda with matching arity
                if let VmValue::Lambda(lambda) = value {
                    lambda.params.len() == params.len()
                } else {
                    false
                }
            }
            TypeKind::StringLiteral(expected) => {
                // Check if value is a string matching the literal
                if let VmValue::String(s) = value {
                    s.as_ref() == expected
                } else {
                    false
                }
            }
            TypeKind::Nothing => false,
            TypeKind::Unknown => true,
            TypeKind::Module => {
                // Module type matches objects that represent modules
                matches!(value, VmValue::Object(_))
            }
        }
    }

    /// Check if a value matches a named type
    fn check_named_type(&self, value: &VmValue, type_name: &str) -> bool {
        match type_name {
            "Any" => true,
            "Null" => matches!(value, VmValue::Null),
            "Boolean" => matches!(value, VmValue::Boolean(_)),
            "Int" => matches!(value, VmValue::Int(_)),
            "Float" => matches!(value, VmValue::Float(_)),
            "Number" => matches!(value, VmValue::Int(_) | VmValue::Float(_)),
            "String" => matches!(value, VmValue::String(_)),
            "Duration" => matches!(value, VmValue::Duration { .. }),
            "DataSize" => matches!(value, VmValue::DataSize { .. }),
            "List" => matches!(value, VmValue::List(_)),
            "Set" => matches!(value, VmValue::Set(_)),
            "Map" => matches!(value, VmValue::Map(_)),
            "Listing" => matches!(value, VmValue::Object(o) if o.is_listing()),
            "Mapping" => matches!(value, VmValue::Object(o) if o.is_mapping()),
            "Dynamic" => matches!(value, VmValue::Object(o) if o.is_dynamic()),
            "Object" => matches!(value, VmValue::Object(_)),
            "Function" => matches!(value, VmValue::Lambda(_)),
            "Regex" => matches!(value, VmValue::Regex(_)),
            "IntSeq" => matches!(value, VmValue::IntSeq { .. }),
            "Pair" => matches!(value, VmValue::Pair(_)),
            "Comparable" => {
                // Types that can be compared
                matches!(
                    value,
                    VmValue::Int(_)
                        | VmValue::Float(_)
                        | VmValue::String(_)
                        | VmValue::Boolean(_)
                        | VmValue::Duration { .. }
                        | VmValue::DataSize { .. }
                )
            }
            _ => {
                // Check typed objects
                if let VmValue::Object(obj) = value {
                    if let ObjectKind::Typed(obj_type) = &obj.kind {
                        return obj_type == type_name;
                    }
                }
                false
            }
        }
    }

    /// Check if a value matches a parameterized type like List<Int> or Map<String, Int>
    fn check_parameterized_type(
        &self,
        value: &VmValue,
        base_name: &str,
        args: &[rpkl_parser::TypeAnnotation],
    ) -> bool {
        match base_name {
            "List" => {
                if let VmValue::List(items) = value {
                    if let Some(elem_type) = args.first() {
                        items.iter().all(|item| self.check_type(item, elem_type))
                    } else {
                        true // List without type param matches any List
                    }
                } else {
                    false
                }
            }
            "Set" => {
                if let VmValue::Set(items) = value {
                    if let Some(elem_type) = args.first() {
                        items.iter().all(|item| self.check_type(item, elem_type))
                    } else {
                        true
                    }
                } else {
                    false
                }
            }
            "Map" => {
                if let VmValue::Map(items) = value {
                    let key_type = args.first();
                    let val_type = args.get(1);
                    items.iter().all(|(k, v)| {
                        let key_ok = key_type.is_none_or(|t| self.check_type(k, t));
                        let val_ok = val_type.is_none_or(|t| self.check_type(v, t));
                        key_ok && val_ok
                    })
                } else {
                    false
                }
            }
            "Listing" => {
                if let VmValue::Object(obj) = value {
                    if !obj.is_listing() {
                        return false;
                    }
                    if let Some(elem_type) = args.first() {
                        // Check all elements match the type
                        let count = obj.element_count();
                        for i in 0..count {
                            if let Some(member) = obj.get_element_member(i) {
                                if let Some(val) = member.get_if_evaluated() {
                                    if !self.check_type(&val, elem_type) {
                                        return false;
                                    }
                                }
                            }
                        }
                    }
                    true
                } else {
                    false
                }
            }
            "Mapping" => {
                if let VmValue::Object(obj) = value {
                    if !obj.is_mapping() {
                        return false;
                    }
                    let key_type = args.first();
                    let val_type = args.get(1);
                    // Check all entries match
                    for key in obj.entry_keys() {
                        if let Some(kt) = key_type {
                            if !self.check_type(&key, kt) {
                                return false;
                            }
                        }
                        if let Some(vt) = val_type {
                            if let Some(member) = obj.get_entry_member(&key) {
                                if let Some(val) = member.get_if_evaluated() {
                                    if !self.check_type(&val, vt) {
                                        return false;
                                    }
                                }
                            }
                        }
                    }
                    true
                } else {
                    false
                }
            }
            "Pair" => {
                if let VmValue::Pair(p) = value {
                    let first_ok = args.first().is_none_or(|t| self.check_type(&p.0, t));
                    let second_ok = args.get(1).is_none_or(|t| self.check_type(&p.1, t));
                    first_ok && second_ok
                } else {
                    false
                }
            }
            "Collection" => {
                // Collection<T> matches List, Set, or Map with value type T
                if let Some(elem_type) = args.first() {
                    match value {
                        VmValue::List(items) => {
                            items.iter().all(|item| self.check_type(item, elem_type))
                        }
                        VmValue::Set(items) => {
                            items.iter().all(|item| self.check_type(item, elem_type))
                        }
                        VmValue::Map(items) => {
                            items.values().all(|item| self.check_type(item, elem_type))
                        }
                        _ => false,
                    }
                } else {
                    matches!(value, VmValue::List(_) | VmValue::Set(_) | VmValue::Map(_))
                }
            }
            _ => {
                // For unknown parameterized types, just check the base
                self.check_named_type(value, base_name)
            }
        }
    }

    /// Evaluate a type constraint expression with `this` bound to the value
    fn check_constraint(&self, value: &VmValue, constraint: &Expr) -> bool {
        // Create a scope with `this` bound to the value
        let scope_ref = Scope::for_constraint(value.clone());

        // Evaluate the constraint expression
        match self.eval_expr(constraint, &scope_ref) {
            Ok(result) => {
                // Constraint should evaluate to a boolean
                matches!(result, VmValue::Boolean(true))
            }
            Err(_) => false, // Constraint evaluation failed, treat as non-match
        }
    }

    /// Coerce a value to a target type (for the `as` operator)
    /// The type check has already passed, so this performs conversions where needed
    fn coerce_to_type(
        &self,
        value: VmValue,
        ty: &rpkl_parser::TypeAnnotation,
    ) -> EvalResult<VmValue> {
        use rpkl_parser::TypeKind;
        match &ty.kind {
            TypeKind::Named(name) => {
                let type_name = name.to_string();
                match type_name.as_str() {
                    // Convert Int to Float if target is Float
                    "Float" => match &value {
                        VmValue::Int(i) => Ok(VmValue::Float(*i as f64)),
                        _ => Ok(value),
                    },
                    // Convert Float to Int if target is Int (truncates)
                    "Int" => match &value {
                        VmValue::Float(f) => Ok(VmValue::Int(*f as i64)),
                        _ => Ok(value),
                    },
                    // Number accepts both Int and Float as-is
                    "Number" => Ok(value),
                    // For most types, just return the value unchanged
                    _ => Ok(value),
                }
            }
            TypeKind::Nullable(inner) => {
                // For nullable types, coerce the inner value if not null
                if matches!(value, VmValue::Null) {
                    Ok(value)
                } else {
                    self.coerce_to_type(value, inner)
                }
            }
            TypeKind::Union(types) => {
                // For unions, find the first matching type and coerce to it
                for t in types {
                    if self.check_type(&value, t) {
                        return self.coerce_to_type(value, t);
                    }
                }
                // Should not reach here since check_type passed
                Ok(value)
            }
            // For other types, just return unchanged
            _ => Ok(value),
        }
    }

    /// Convert a type annotation to a human-readable string
    fn type_annotation_to_string(&self, ty: &rpkl_parser::TypeAnnotation) -> String {
        use rpkl_parser::TypeKind;
        match &ty.kind {
            TypeKind::Named(name) => name.to_string(),
            TypeKind::Parameterized { base, args } => {
                let args_str: Vec<String> = args
                    .iter()
                    .map(|a| self.type_annotation_to_string(a))
                    .collect();
                format!("{}<{}>", base, args_str.join(", "))
            }
            TypeKind::Nullable(inner) => {
                format!("{}?", self.type_annotation_to_string(inner))
            }
            TypeKind::Union(types) => {
                let types_str: Vec<String> = types
                    .iter()
                    .map(|t| self.type_annotation_to_string(t))
                    .collect();
                types_str.join("|")
            }
            TypeKind::Constrained {
                base,
                constraint: _,
            } => {
                format!("{}(...)", self.type_annotation_to_string(base))
            }
            TypeKind::Function {
                params,
                return_type,
            } => {
                let params_str: Vec<String> = params
                    .iter()
                    .map(|p| self.type_annotation_to_string(p))
                    .collect();
                format!(
                    "({}) -> {}",
                    params_str.join(", "),
                    self.type_annotation_to_string(return_type)
                )
            }
            TypeKind::StringLiteral(s) => format!("\"{}\"", s),
            TypeKind::Nothing => "nothing".to_string(),
            TypeKind::Unknown => "unknown".to_string(),
            TypeKind::Module => "module".to_string(),
        }
    }

    /// Infer the object kind from the body contents
    /// If the body only contains elements, it's a Listing
    /// If the body only contains entries, it's a Mapping
    /// Otherwise, it's Dynamic
    fn infer_object_kind_from_body(&self, body: &ObjectBody) -> ObjectKind {
        use rpkl_parser::ObjectMember;

        let mut has_elements = false;
        let mut has_entries = false;
        let mut has_properties = false;

        for member in &body.members {
            match member {
                ObjectMember::Element { .. } => has_elements = true,
                ObjectMember::Entry { .. } | ObjectMember::EntryAmend { .. } => has_entries = true,
                ObjectMember::Property { .. } | ObjectMember::PropertyAmend { .. } => {
                    has_properties = true
                }
                // For, When, Spread are handled recursively in their bodies
                ObjectMember::For { body: for_body, .. } => {
                    // Check inner body
                    let inner_kind = self.infer_object_kind_from_body(for_body);
                    match inner_kind {
                        ObjectKind::Listing => has_elements = true,
                        ObjectKind::Mapping => has_entries = true,
                        _ => {}
                    }
                }
                ObjectMember::When {
                    body: when_body, ..
                } => {
                    let inner_kind = self.infer_object_kind_from_body(when_body);
                    match inner_kind {
                        ObjectKind::Listing => has_elements = true,
                        ObjectKind::Mapping => has_entries = true,
                        _ => {}
                    }
                }
                ObjectMember::Spread { .. } => {
                    // Spreads could be anything, treat as dynamic
                    has_properties = true;
                }
            }
        }

        // Infer kind based on what we found
        if has_elements && !has_entries && !has_properties {
            ObjectKind::Listing
        } else if has_entries && !has_elements && !has_properties {
            ObjectKind::Mapping
        } else {
            ObjectKind::Dynamic
        }
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}
