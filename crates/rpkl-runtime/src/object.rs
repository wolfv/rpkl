//! PKL Object representation with lazy evaluation

use std::cell::RefCell;
use std::fmt;
use std::sync::Arc;

use indexmap::IndexMap;
use serde::ser::{SerializeMap, SerializeSeq};
use serde::{Serialize, Serializer};

use crate::error::EvalResult;
use crate::scope::ScopeRef;
use crate::value::VmValue;

/// Object kind (Dynamic, Listing, Mapping, or typed)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjectKind {
    /// Dynamic object (untyped)
    Dynamic,
    /// Listing (array-like)
    Listing,
    /// Mapping (dict-like)
    Mapping,
    /// Typed object with class name
    Typed(String),
}

/// Shared reference to an object member
pub type ObjectMemberRef = Arc<ObjectMember>;

/// A PKL object
#[derive(Debug)]
pub struct VmObject {
    /// The kind of this object
    pub kind: ObjectKind,

    /// Parent object for amendment chain
    pub parent: Option<Arc<VmObject>>,

    /// Properties (name -> member) - Arc so references share state
    pub properties: RefCell<IndexMap<String, ObjectMemberRef>>,

    /// Elements (for Listing/Dynamic) - index -> member
    pub elements: RefCell<Vec<ObjectMemberRef>>,

    /// Entries (for Mapping/Dynamic) - key -> member
    pub entries: RefCell<IndexMap<VmValue, ObjectMemberRef>>,

    /// Scope for evaluation
    pub scope: ScopeRef,
}

/// Metadata for an object member
#[derive(Debug, Clone, Default)]
pub struct MemberMetadata {
    /// Whether this member is hidden (excluded from output)
    pub is_hidden: bool,
    /// Whether this member is local (not inherited)
    pub is_local: bool,
    /// Type hint for nullable typed properties (e.g., "Recipe.About" for `about: Recipe.About?`)
    /// Used when amending a null value to create the correct typed object
    pub type_hint: Option<String>,
}

/// An object member (lazily evaluated)
#[derive(Debug)]
pub struct ObjectMember {
    state: RefCell<MemberState>,
    metadata: MemberMetadata,
}

#[derive(Debug, Clone)]
enum MemberState {
    /// Not yet evaluated
    Unevaluated {
        expr: rpkl_parser::Expr,
        scope: ScopeRef,
    },
    /// Currently being evaluated (for cycle detection)
    Evaluating,
    /// Already evaluated, but keeps the original expression for re-evaluation
    EvaluatedWithExpr {
        value: VmValue,
        expr: rpkl_parser::Expr,
        scope: ScopeRef,
    },
    /// Already evaluated (no original expression available)
    Evaluated(VmValue),
}

impl ObjectMember {
    /// Create a new unevaluated member
    pub fn new(expr: rpkl_parser::Expr, scope: ScopeRef) -> Self {
        Self {
            state: RefCell::new(MemberState::Unevaluated { expr, scope }),
            metadata: MemberMetadata::default(),
        }
    }

    /// Create a new unevaluated member with metadata
    pub fn new_with_metadata(
        expr: rpkl_parser::Expr,
        scope: ScopeRef,
        metadata: MemberMetadata,
    ) -> Self {
        Self {
            state: RefCell::new(MemberState::Unevaluated { expr, scope }),
            metadata,
        }
    }

    /// Create a member with a pre-evaluated value
    pub fn with_value(value: VmValue) -> Self {
        Self {
            state: RefCell::new(MemberState::Evaluated(value)),
            metadata: MemberMetadata::default(),
        }
    }

    /// Create a member with a pre-evaluated value and metadata
    pub fn with_value_and_metadata(value: VmValue, metadata: MemberMetadata) -> Self {
        Self {
            state: RefCell::new(MemberState::Evaluated(value)),
            metadata,
        }
    }

    /// Check if this member is hidden
    pub fn is_hidden(&self) -> bool {
        self.metadata.is_hidden
    }

    /// Get the metadata for this member
    pub fn metadata(&self) -> &MemberMetadata {
        &self.metadata
    }

    /// Check if this member is already evaluated
    pub fn is_evaluated(&self) -> bool {
        matches!(
            *self.state.borrow(),
            MemberState::Evaluated(_) | MemberState::EvaluatedWithExpr { .. }
        )
    }

    /// Get the value if already evaluated
    pub fn get_if_evaluated(&self) -> Option<VmValue> {
        match &*self.state.borrow() {
            MemberState::Evaluated(v) => Some(v.clone()),
            MemberState::EvaluatedWithExpr { value, .. } => Some(value.clone()),
            _ => None,
        }
    }

    /// Force evaluation (used by evaluator)
    pub fn force<F>(&self, eval_fn: F) -> EvalResult<VmValue>
    where
        F: FnOnce(&rpkl_parser::Expr, &ScopeRef) -> EvalResult<VmValue>,
    {
        let current = self.state.borrow().clone();
        match current {
            MemberState::Unevaluated { expr, scope } => {
                // Mark as evaluating to detect cycles
                *self.state.borrow_mut() = MemberState::Evaluating;

                // Evaluate
                let result = eval_fn(&expr, &scope)?;

                // Store result along with original expression for potential re-evaluation
                *self.state.borrow_mut() = MemberState::EvaluatedWithExpr {
                    value: result.clone(),
                    expr,
                    scope,
                };
                Ok(result)
            }
            MemberState::Evaluating => Err(crate::error::EvalError::CircularReference),
            MemberState::EvaluatedWithExpr { value, .. } => Ok(value),
            MemberState::Evaluated(v) => Ok(v),
        }
    }

    /// Force evaluation with a module override
    ///
    /// This is used when evaluating inherited properties in an amended module.
    /// The `module_override` replaces the module in the stored scope, allowing
    /// the expression to see the amended module's properties.
    ///
    /// If the member was already evaluated with a different scope, it will be
    /// re-evaluated with the new module override.
    pub fn force_with_module<F>(
        &self,
        module_override: Arc<crate::object::VmObject>,
        eval_fn: F,
    ) -> EvalResult<VmValue>
    where
        F: FnOnce(&rpkl_parser::Expr, &ScopeRef) -> EvalResult<VmValue>,
    {
        let current = self.state.borrow().clone();
        match current {
            MemberState::Unevaluated { expr, scope }
            | MemberState::EvaluatedWithExpr { expr, scope, .. } => {
                // Mark as evaluating to detect cycles
                *self.state.borrow_mut() = MemberState::Evaluating;

                // Create a new scope with the module override
                let new_scope = crate::scope::Scope::with_module_override(&scope, module_override);

                // Evaluate with the overridden scope
                let result = eval_fn(&expr, &new_scope)?;

                // Store result (don't keep expr since we've overridden the scope)
                *self.state.borrow_mut() = MemberState::Evaluated(result.clone());
                Ok(result)
            }
            MemberState::Evaluating => Err(crate::error::EvalError::CircularReference),
            MemberState::Evaluated(v) => Ok(v),
        }
    }

    /// Set evaluated value directly
    pub fn set_value(&self, value: VmValue) {
        *self.state.borrow_mut() = MemberState::Evaluated(value);
    }
}

impl VmObject {
    /// Create a new empty dynamic object
    pub fn new_dynamic(scope: ScopeRef) -> Self {
        Self {
            kind: ObjectKind::Dynamic,
            parent: None,
            properties: RefCell::new(IndexMap::new()),
            elements: RefCell::new(Vec::new()),
            entries: RefCell::new(IndexMap::new()),
            scope,
        }
    }

    /// Create a new listing object
    pub fn new_listing(scope: ScopeRef) -> Self {
        Self {
            kind: ObjectKind::Listing,
            parent: None,
            properties: RefCell::new(IndexMap::new()),
            elements: RefCell::new(Vec::new()),
            entries: RefCell::new(IndexMap::new()),
            scope,
        }
    }

    /// Create a new mapping object
    pub fn new_mapping(scope: ScopeRef) -> Self {
        Self {
            kind: ObjectKind::Mapping,
            parent: None,
            properties: RefCell::new(IndexMap::new()),
            elements: RefCell::new(Vec::new()),
            entries: RefCell::new(IndexMap::new()),
            scope,
        }
    }

    /// Create a new typed object
    pub fn new_typed(class_name: String, scope: ScopeRef) -> Self {
        Self {
            kind: ObjectKind::Typed(class_name),
            parent: None,
            properties: RefCell::new(IndexMap::new()),
            elements: RefCell::new(Vec::new()),
            entries: RefCell::new(IndexMap::new()),
            scope,
        }
    }

    /// Create an amended copy of this object
    pub fn amend(self: &Arc<Self>, new_scope: ScopeRef) -> Self {
        Self {
            kind: self.kind.clone(),
            parent: Some(Arc::clone(self)),
            properties: RefCell::new(IndexMap::new()),
            elements: RefCell::new(Vec::new()),
            entries: RefCell::new(IndexMap::new()),
            scope: new_scope,
        }
    }

    /// Get the type name of this object
    pub fn type_name(&self) -> &'static str {
        match &self.kind {
            ObjectKind::Dynamic => "Dynamic",
            ObjectKind::Listing => "Listing",
            ObjectKind::Mapping => "Mapping",
            ObjectKind::Typed(_) => "Object",
        }
    }

    /// Check if this is a Dynamic object
    pub fn is_dynamic(&self) -> bool {
        matches!(self.kind, ObjectKind::Dynamic)
    }

    /// Check if this is a Listing
    pub fn is_listing(&self) -> bool {
        matches!(self.kind, ObjectKind::Listing)
    }

    /// Check if this is a Mapping
    pub fn is_mapping(&self) -> bool {
        matches!(self.kind, ObjectKind::Mapping)
    }

    /// Add a property (wraps in Arc)
    pub fn add_property(&self, name: String, member: ObjectMember) {
        self.properties.borrow_mut().insert(name, Arc::new(member));
    }

    /// Add an element (wraps in Arc)
    pub fn add_element(&self, member: ObjectMember) {
        self.elements.borrow_mut().push(Arc::new(member));
    }

    /// Add an entry (wraps in Arc)
    pub fn add_entry(&self, key: VmValue, member: ObjectMember) {
        self.entries.borrow_mut().insert(key, Arc::new(member));
    }

    /// Check if a property exists (including parent chain)
    pub fn has_property(&self, name: &str) -> bool {
        if self.properties.borrow().contains_key(name) {
            return true;
        }
        if let Some(parent) = &self.parent {
            return parent.has_property(name);
        }
        false
    }

    /// Check if a property is local (not inherited from parent)
    pub fn has_local_property(&self, name: &str) -> bool {
        self.properties.borrow().contains_key(name)
    }

    /// Get a property member reference (shares state via Arc)
    pub fn get_property_member(&self, name: &str) -> Option<ObjectMemberRef> {
        if let Some(member) = self.properties.borrow().get(name) {
            return Some(Arc::clone(member));
        }
        if let Some(parent) = &self.parent {
            return parent.get_property_member(name);
        }
        None
    }

    /// Get an element member by index
    pub fn get_element_member(&self, index: usize) -> Option<ObjectMemberRef> {
        let local_len = self.elements.borrow().len();

        // First check parent elements
        let parent_len = self.parent.as_ref().map_or(0, |p| p.element_count());

        if index < parent_len {
            // Get from parent
            if let Some(parent) = &self.parent {
                return parent.get_element_member(index);
            }
        } else {
            // Get from local
            let local_index = index - parent_len;
            if local_index < local_len {
                return self.elements.borrow().get(local_index).cloned();
            }
        }
        None
    }

    /// Get entry member by key
    pub fn get_entry_member(&self, key: &VmValue) -> Option<ObjectMemberRef> {
        // Check local entries first (amendment overrides)
        if let Some(member) = self.entries.borrow().get(key) {
            return Some(Arc::clone(member));
        }
        if let Some(parent) = &self.parent {
            return parent.get_entry_member(key);
        }
        None
    }

    /// Count total elements (including parent chain)
    pub fn element_count(&self) -> usize {
        let local = self.elements.borrow().len();
        let parent = self.parent.as_ref().map_or(0, |p| p.element_count());
        local + parent
    }

    /// Iterate all property names (including parent chain)
    pub fn property_names(&self) -> Vec<String> {
        let mut names = Vec::new();

        // Add parent properties first
        if let Some(parent) = &self.parent {
            names.extend(parent.property_names());
        }

        // Add/override with local properties
        for name in self.properties.borrow().keys() {
            if !names.contains(name) {
                names.push(name.clone());
            }
        }

        names
    }

    /// Iterate all visible (non-hidden) property names (including parent chain)
    pub fn visible_property_names(&self) -> Vec<String> {
        let mut names = Vec::new();
        let mut hidden_names = Vec::new();

        // Collect hidden names from local properties first
        for (name, member) in self.properties.borrow().iter() {
            if member.is_hidden() {
                hidden_names.push(name.clone());
            }
        }

        // Add parent visible properties first (respecting local hidden status)
        if let Some(parent) = &self.parent {
            for name in parent.visible_property_names() {
                if !hidden_names.contains(&name) {
                    names.push(name);
                }
            }
        }

        // Add/override with local visible properties
        for (name, member) in self.properties.borrow().iter() {
            if !member.is_hidden() && !names.contains(name) {
                names.push(name.clone());
            }
        }

        names
    }

    /// Iterate all entry keys (including parent chain)
    pub fn entry_keys(&self) -> Vec<VmValue> {
        let mut keys = Vec::new();

        // Add parent keys first
        if let Some(parent) = &self.parent {
            keys.extend(parent.entry_keys());
        }

        // Add/override with local keys
        for key in self.entries.borrow().keys() {
            if !keys.contains(key) {
                keys.push(key.clone());
            }
        }

        keys
    }
}

impl fmt::Display for VmObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ObjectKind::Dynamic => write!(f, "new {{...}}"),
            ObjectKind::Listing => write!(f, "new Listing {{...}}"),
            ObjectKind::Mapping => write!(f, "new Mapping {{...}}"),
            ObjectKind::Typed(name) => write!(f, "new {} {{...}}", name),
        }
    }
}

impl Serialize for VmObject {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match &self.kind {
            ObjectKind::Listing => {
                // Serialize as array
                let count = self.element_count();
                let mut seq = serializer.serialize_seq(Some(count))?;
                for i in 0..count {
                    if let Some(member) = self.get_element_member(i) {
                        if let Some(value) = member.get_if_evaluated() {
                            seq.serialize_element(&value)?;
                        }
                    }
                }
                seq.end()
            }
            ObjectKind::Mapping => {
                // Serialize as object
                let keys = self.entry_keys();
                let mut map = serializer.serialize_map(Some(keys.len()))?;
                for key in keys {
                    if let Some(member) = self.get_entry_member(&key) {
                        if let Some(value) = member.get_if_evaluated() {
                            let key_str = match &key {
                                VmValue::String(s) => s.to_string(),
                                _ => key.to_string(),
                            };
                            map.serialize_entry(&key_str, &value)?;
                        }
                    }
                }
                map.end()
            }
            ObjectKind::Dynamic | ObjectKind::Typed(_) => {
                // Serialize properties as object, but also include elements if present
                // Use visible_property_names to filter out hidden properties
                let names = self.visible_property_names();
                let element_count = self.element_count();
                let has_elements = element_count > 0;
                let entry_count = self.entry_keys().len();
                let has_entries = entry_count > 0;

                if has_elements && !has_entries && names.is_empty() {
                    // Pure element object -> serialize as array
                    let mut seq = serializer.serialize_seq(Some(element_count))?;
                    for i in 0..element_count {
                        if let Some(member) = self.get_element_member(i) {
                            if let Some(value) = member.get_if_evaluated() {
                                seq.serialize_element(&value)?;
                            }
                        }
                    }
                    seq.end()
                } else {
                    // Serialize as object
                    let mut map = serializer.serialize_map(None)?;

                    // Properties
                    for name in names {
                        if let Some(member) = self.get_property_member(&name) {
                            if let Some(value) = member.get_if_evaluated() {
                                map.serialize_entry(&name, &value)?;
                            }
                        }
                    }

                    // Elements as numbered keys if mixed
                    if has_elements {
                        for i in 0..element_count {
                            if let Some(member) = self.get_element_member(i) {
                                if let Some(value) = member.get_if_evaluated() {
                                    map.serialize_entry(&i.to_string(), &value)?;
                                }
                            }
                        }
                    }

                    // Entries
                    for key in self.entry_keys() {
                        if let Some(member) = self.get_entry_member(&key) {
                            if let Some(value) = member.get_if_evaluated() {
                                let key_str = match &key {
                                    VmValue::String(s) => s.to_string(),
                                    _ => key.to_string(),
                                };
                                map.serialize_entry(&key_str, &value)?;
                            }
                        }
                    }

                    map.end()
                }
            }
        }
    }
}
