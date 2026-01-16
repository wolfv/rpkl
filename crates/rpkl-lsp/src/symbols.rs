//! Document symbols, hover, and completion support

use rpkl_parser::{
    ClassDef, ClassMember, Expr, ExprKind, Method, Module, ModuleMember, ObjectBody, ObjectMember,
    Property, TypeAlias, TypeAnnotation, TypeKind,
};
use std::path::Path;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, DocumentSymbol, Hover, HoverContents, MarkupContent,
    MarkupKind, SymbolKind,
};

use crate::document::Document;

/// Result of a definition lookup that may point to another file
#[derive(Debug)]
#[allow(dead_code)]
pub enum DefinitionResult {
    /// Definition is in the same file at (start, end) offsets
    SameFile { start: usize, end: usize },
    /// Definition is in another file (navigate to beginning)
    OtherFile { path: String },
    /// Definition is a specific member in another file
    OtherFileMember { path: String, member_name: String },
}

/// Check if an identifier at the given offset matches an import or import member
pub fn find_import_definition(
    doc: &Document,
    offset: usize,
    current_file: &str,
) -> Option<DefinitionResult> {
    let module = doc.ast.as_ref()?;

    // First, check if we're on a member access like Platform.isWindows
    if let Some(result) = find_import_member_at_offset(module, offset, current_file) {
        return Some(result);
    }

    // Otherwise, find what identifier we're on
    let info = find_identifier_info_at_offset(module, offset)?;
    tracing::debug!("find_import_definition: looking for '{}'", info.name);

    // Check if this identifier matches an import
    for import in &module.imports {
        let import_name = get_import_name(import);
        tracing::debug!(
            "  Checking import '{}' (alias: {:?})",
            import_name,
            import.alias
        );

        if import_name == info.name {
            // Resolve the import path relative to the current file
            if let Some(uri) = import.uri.as_simple() {
                let resolved = resolve_import_path(current_file, uri);
                tracing::debug!("  Matched! Resolved path: {:?}", resolved);
                if let Some(path) = resolved {
                    return Some(DefinitionResult::OtherFile { path });
                }
            }
        }
    }

    None
}

/// Check if we're on a member of an import (e.g., `isWindows` in `Platform.isWindows`)
fn find_import_member_at_offset(
    module: &Module,
    offset: usize,
    current_file: &str,
) -> Option<DefinitionResult> {
    // Search through all expressions to find MemberAccess where:
    // 1. The offset is on the member name
    // 2. The base is an identifier that matches an import
    find_import_member_in_members(&module.members, offset, module, current_file)
}

fn find_import_member_in_members(
    members: &[ModuleMember],
    offset: usize,
    module: &Module,
    current_file: &str,
) -> Option<DefinitionResult> {
    for member in members {
        match member {
            ModuleMember::Property(prop) => {
                if let Some(ref value) = prop.value {
                    match value {
                        PropertyValue::Expr(expr) => {
                            if let Some(result) =
                                find_import_member_in_expr(expr, offset, module, current_file)
                            {
                                return Some(result);
                            }
                        }
                        PropertyValue::Object(body) => {
                            if let Some(result) =
                                find_import_member_in_body(body, offset, module, current_file)
                            {
                                return Some(result);
                            }
                        }
                    }
                }
            }
            ModuleMember::Method(method) => {
                if let Some(ref body) = method.body {
                    if let Some(result) =
                        find_import_member_in_expr(body, offset, module, current_file)
                    {
                        return Some(result);
                    }
                }
            }
            ModuleMember::Class(class) => {
                for m in &class.members {
                    match m {
                        ClassMember::Property(p) => {
                            if let Some(ref value) = p.value {
                                match value {
                                    PropertyValue::Expr(expr) => {
                                        if let Some(result) = find_import_member_in_expr(
                                            expr,
                                            offset,
                                            module,
                                            current_file,
                                        ) {
                                            return Some(result);
                                        }
                                    }
                                    PropertyValue::Object(body) => {
                                        if let Some(result) = find_import_member_in_body(
                                            body,
                                            offset,
                                            module,
                                            current_file,
                                        ) {
                                            return Some(result);
                                        }
                                    }
                                }
                            }
                        }
                        ClassMember::Method(m) => {
                            if let Some(ref body) = m.body {
                                if let Some(result) =
                                    find_import_member_in_expr(body, offset, module, current_file)
                                {
                                    return Some(result);
                                }
                            }
                        }
                    }
                }
            }
            ModuleMember::TypeAlias(_) => {}
        }
    }
    None
}

fn find_import_member_in_expr(
    expr: &Expr,
    offset: usize,
    module: &Module,
    current_file: &str,
) -> Option<DefinitionResult> {
    // Check if offset is within this expression
    if expr.span.start > offset || expr.span.end < offset {
        return None;
    }

    match &expr.kind {
        ExprKind::MemberAccess { base, member } => {
            // Check if offset is on the member name
            if member.span.start <= offset && offset <= member.span.end {
                // Check if base is an identifier that matches an import
                if let ExprKind::Identifier(base_name) = &base.kind {
                    for import in &module.imports {
                        let import_name = get_import_name(import);
                        if import_name == *base_name {
                            if let Some(uri) = import.uri.as_simple() {
                                if let Some(path) = resolve_import_path(current_file, uri) {
                                    tracing::debug!(
                                        "Found import member access: {}.{} -> {}",
                                        base_name,
                                        member.node,
                                        path
                                    );
                                    return Some(DefinitionResult::OtherFileMember {
                                        path,
                                        member_name: member.node.clone(),
                                    });
                                }
                            }
                        }
                    }
                }
            }
            // Recurse into base
            find_import_member_in_expr(base, offset, module, current_file)
        }
        ExprKind::OptionalMemberAccess { base, member } => {
            if member.span.start <= offset && offset <= member.span.end {
                if let ExprKind::Identifier(base_name) = &base.kind {
                    for import in &module.imports {
                        let import_name = get_import_name(import);
                        if import_name == *base_name {
                            if let Some(uri) = import.uri.as_simple() {
                                if let Some(path) = resolve_import_path(current_file, uri) {
                                    return Some(DefinitionResult::OtherFileMember {
                                        path,
                                        member_name: member.node.clone(),
                                    });
                                }
                            }
                        }
                    }
                }
            }
            find_import_member_in_expr(base, offset, module, current_file)
        }
        ExprKind::Call { callee, args } => {
            if let Some(result) = find_import_member_in_expr(callee, offset, module, current_file) {
                return Some(result);
            }
            for arg in args {
                if let Some(result) = find_import_member_in_expr(arg, offset, module, current_file)
                {
                    return Some(result);
                }
            }
            None
        }
        ExprKind::Binary { left, right, .. } => {
            find_import_member_in_expr(left, offset, module, current_file)
                .or_else(|| find_import_member_in_expr(right, offset, module, current_file))
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => find_import_member_in_expr(condition, offset, module, current_file)
            .or_else(|| find_import_member_in_expr(then_branch, offset, module, current_file))
            .or_else(|| find_import_member_in_expr(else_branch, offset, module, current_file)),
        ExprKind::New { body, .. } => {
            find_import_member_in_body(body, offset, module, current_file)
        }
        ExprKind::Amend { base, body } => {
            find_import_member_in_expr(base, offset, module, current_file)
                .or_else(|| find_import_member_in_body(body, offset, module, current_file))
        }
        ExprKind::Parenthesized(inner) => {
            find_import_member_in_expr(inner, offset, module, current_file)
        }
        _ => None,
    }
}

fn find_import_member_in_body(
    body: &ObjectBody,
    offset: usize,
    module: &Module,
    current_file: &str,
) -> Option<DefinitionResult> {
    for member in &body.members {
        match member {
            ObjectMember::Property { value, .. } => {
                if let Some(result) =
                    find_import_member_in_expr(value, offset, module, current_file)
                {
                    return Some(result);
                }
            }
            ObjectMember::PropertyAmend {
                body: inner_body, ..
            } => {
                if let Some(result) =
                    find_import_member_in_body(inner_body, offset, module, current_file)
                {
                    return Some(result);
                }
            }
            ObjectMember::Element { value, .. } => {
                if let Some(result) =
                    find_import_member_in_expr(value, offset, module, current_file)
                {
                    return Some(result);
                }
            }
            ObjectMember::Entry { key, value, .. } => {
                if let Some(result) = find_import_member_in_expr(key, offset, module, current_file)
                {
                    return Some(result);
                }
                if let Some(result) =
                    find_import_member_in_expr(value, offset, module, current_file)
                {
                    return Some(result);
                }
            }
            ObjectMember::For {
                iterable,
                body: inner_body,
                ..
            } => {
                if let Some(result) =
                    find_import_member_in_expr(iterable, offset, module, current_file)
                {
                    return Some(result);
                }
                if let Some(result) =
                    find_import_member_in_body(inner_body, offset, module, current_file)
                {
                    return Some(result);
                }
            }
            ObjectMember::When {
                condition,
                body: inner_body,
                else_body,
                ..
            } => {
                if let Some(result) =
                    find_import_member_in_expr(condition, offset, module, current_file)
                {
                    return Some(result);
                }
                if let Some(result) =
                    find_import_member_in_body(inner_body, offset, module, current_file)
                {
                    return Some(result);
                }
                if let Some(ref eb) = else_body {
                    if let Some(result) =
                        find_import_member_in_body(eb, offset, module, current_file)
                    {
                        return Some(result);
                    }
                }
            }
            _ => {}
        }
    }
    None
}

/// Find a member definition in a module by name, returning its span
pub fn find_member_in_module(module: &Module, member_name: &str) -> Option<(usize, usize)> {
    for member in &module.members {
        match member {
            ModuleMember::Property(prop) if prop.name.node == member_name => {
                return Some((prop.name.span.start, prop.name.span.end));
            }
            ModuleMember::Method(method) if method.name.node == member_name => {
                return Some((method.name.span.start, method.name.span.end));
            }
            ModuleMember::Class(class) if class.name.node == member_name => {
                return Some((class.name.span.start, class.name.span.end));
            }
            ModuleMember::TypeAlias(alias) if alias.name.node == member_name => {
                return Some((alias.name.span.start, alias.name.span.end));
            }
            _ => {}
        }
    }
    None
}

/// Get the name used to reference an import (alias or derived from filename)
fn get_import_name(import: &rpkl_parser::Import) -> String {
    if let Some(ref alias) = import.alias {
        alias.node.clone()
    } else if let Some(uri) = import.uri.as_simple() {
        // Derive name from filename: "Platform.pkl" -> "Platform"
        Path::new(uri)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("")
            .to_string()
    } else {
        String::new()
    }
}

/// Resolve an import path relative to the current file
fn resolve_import_path(current_file: &str, import_uri: &str) -> Option<String> {
    // Handle relative imports
    if import_uri.starts_with("./") || import_uri.starts_with("../") || !import_uri.contains(':') {
        // Relative path - resolve relative to current file
        let current_path = Path::new(current_file);
        let parent = current_path.parent()?;
        let resolved = parent.join(import_uri);

        // Normalize the path
        if resolved.exists() {
            return resolved
                .canonicalize()
                .ok()?
                .to_str()
                .map(|s| s.to_string());
        } else {
            // Return the path even if it doesn't exist (might not be saved yet)
            return Some(resolved.to_string_lossy().to_string());
        }
    }

    // For package imports like "package://..." or absolute paths,
    // we'd need more sophisticated resolution
    None
}

/// Get document symbols from the AST
pub fn document_symbols(doc: &Document) -> Vec<DocumentSymbol> {
    let Some(ref module) = doc.ast else {
        return vec![];
    };

    module
        .members
        .iter()
        .filter_map(|member| member_to_symbol(member, doc))
        .collect()
}

/// Convert a module member to a document symbol
fn member_to_symbol(member: &ModuleMember, doc: &Document) -> Option<DocumentSymbol> {
    match member {
        ModuleMember::Property(prop) => Some(property_to_symbol(prop, doc)),
        ModuleMember::Method(method) => Some(method_to_symbol(method, doc)),
        ModuleMember::Class(class) => Some(class_to_symbol(class, doc)),
        ModuleMember::TypeAlias(alias) => Some(type_alias_to_symbol(alias, doc)),
    }
}

fn property_to_symbol(prop: &Property, doc: &Document) -> DocumentSymbol {
    let range = doc.span_to_range(prop.span);
    let selection_range = doc.span_to_range(prop.name.span);

    let detail = prop.ty.as_ref().map(type_annotation_to_string);

    #[allow(deprecated)]
    DocumentSymbol {
        name: prop.name.node.clone(),
        detail,
        kind: SymbolKind::PROPERTY,
        tags: None,
        deprecated: None,
        range,
        selection_range,
        children: None,
    }
}

fn method_to_symbol(method: &Method, doc: &Document) -> DocumentSymbol {
    let range = doc.span_to_range(method.span);
    let selection_range = doc.span_to_range(method.name.span);

    let params: Vec<String> = method
        .params
        .iter()
        .map(|p| {
            if let Some(ref ty) = p.ty {
                format!("{}: {}", p.name.node, type_annotation_to_string(ty))
            } else {
                p.name.node.clone()
            }
        })
        .collect();

    let detail = Some(format!(
        "({}){}",
        params.join(", "),
        method
            .return_type
            .as_ref()
            .map(|t| format!(" -> {}", type_annotation_to_string(t)))
            .unwrap_or_default()
    ));

    #[allow(deprecated)]
    DocumentSymbol {
        name: method.name.node.clone(),
        detail,
        kind: SymbolKind::METHOD,
        tags: None,
        deprecated: None,
        range,
        selection_range,
        children: None,
    }
}

fn class_to_symbol(class: &ClassDef, doc: &Document) -> DocumentSymbol {
    let range = doc.span_to_range(class.span);
    let selection_range = doc.span_to_range(class.name.span);

    let children: Vec<DocumentSymbol> = class
        .members
        .iter()
        .map(|m| match m {
            ClassMember::Property(p) => property_to_symbol(p, doc),
            ClassMember::Method(m) => method_to_symbol(m, doc),
        })
        .collect();

    let detail = class.extends.as_ref().map(|e| format!("extends {}", e));

    #[allow(deprecated)]
    DocumentSymbol {
        name: class.name.node.clone(),
        detail,
        kind: SymbolKind::CLASS,
        tags: None,
        deprecated: None,
        range,
        selection_range,
        children: if children.is_empty() {
            None
        } else {
            Some(children)
        },
    }
}

fn type_alias_to_symbol(alias: &TypeAlias, doc: &Document) -> DocumentSymbol {
    let range = doc.span_to_range(alias.span);
    let selection_range = doc.span_to_range(alias.name.span);

    #[allow(deprecated)]
    DocumentSymbol {
        name: alias.name.node.clone(),
        detail: Some(type_annotation_to_string(&alias.ty)),
        kind: SymbolKind::TYPE_PARAMETER,
        tags: None,
        deprecated: None,
        range,
        selection_range,
        children: None,
    }
}

/// Convert a type annotation to a string representation
fn type_annotation_to_string(ty: &TypeAnnotation) -> String {
    match &ty.kind {
        TypeKind::Named(ident) => ident.to_string(),
        TypeKind::Parameterized { base, args } => {
            let args_str: Vec<String> = args.iter().map(type_annotation_to_string).collect();
            format!("{}<{}>", base, args_str.join(", "))
        }
        TypeKind::Nullable(inner) => format!("{}?", type_annotation_to_string(inner)),
        TypeKind::Union(types) => types
            .iter()
            .map(type_annotation_to_string)
            .collect::<Vec<_>>()
            .join(" | "),
        TypeKind::Constrained { base, .. } => {
            format!("{}(...)", type_annotation_to_string(base))
        }
        TypeKind::Function {
            params,
            return_type,
        } => {
            let params_str: Vec<String> = params.iter().map(type_annotation_to_string).collect();
            format!(
                "({}) -> {}",
                params_str.join(", "),
                type_annotation_to_string(return_type)
            )
        }
        TypeKind::StringLiteral(s) => format!("\"{}\"", s),
        TypeKind::Nothing => "nothing".to_string(),
        TypeKind::Unknown => "unknown".to_string(),
        TypeKind::Module => "module".to_string(),
    }
}

/// Information about an identifier at a position
#[derive(Debug)]
struct IdentifierInfo {
    name: String,
    kind: IdentifierKind,
    /// Local scope bindings (for let, lambda params, for vars)
    local_bindings: Vec<LocalBinding>,
}

#[derive(Debug)]
enum IdentifierKind {
    /// A variable or property reference
    Variable,
    /// A type reference (in type annotations)
    Type,
    /// A class reference (in `new ClassName {}`)
    Class,
}

#[derive(Debug, Clone)]
struct LocalBinding {
    name: String,
    span: rpkl_parser::Span,
}

/// Find the definition at a given offset
pub fn find_definition(doc: &Document, offset: usize) -> Option<(usize, usize)> {
    tracing::debug!("find_definition at offset {}", offset);
    let module = doc.ast.as_ref()?;

    // Find the identifier at the current position with context
    let info = find_identifier_info_at_offset(module, offset);
    tracing::debug!("find_identifier_info_at_offset returned: {:?}", info);
    let info = info?;

    // First check local bindings (let, lambda params, for vars)
    for binding in &info.local_bindings {
        if binding.name == info.name {
            return Some((binding.span.start, binding.span.end));
        }
    }

    // Search for the definition based on kind
    match info.kind {
        IdentifierKind::Type | IdentifierKind::Class => {
            // Look for class or typealias definitions
            find_type_definition_in_module(module, &info.name)
        }
        IdentifierKind::Variable => {
            // Look for property, method, or class definitions
            find_definition_in_module(module, &info.name)
        }
    }
}

/// Find identifier info including local scope
fn find_identifier_info_at_offset(module: &Module, offset: usize) -> Option<IdentifierInfo> {
    for member in &module.members {
        if let Some(info) = find_identifier_info_in_member(member, offset, &[]) {
            return Some(info);
        }
    }
    None
}

fn find_identifier_info_in_member(
    member: &ModuleMember,
    offset: usize,
    bindings: &[LocalBinding],
) -> Option<IdentifierInfo> {
    match member {
        ModuleMember::Property(prop) => {
            tracing::trace!(
                "Checking property '{}' at {:?}, offset={}",
                prop.name.node,
                prop.span,
                offset
            );
            if prop.name.span.start <= offset && offset <= prop.name.span.end {
                return Some(IdentifierInfo {
                    name: prop.name.node.clone(),
                    kind: IdentifierKind::Variable,
                    local_bindings: bindings.to_vec(),
                });
            }
            // Check type annotation
            if let Some(ref ty) = prop.ty {
                if let Some(info) = find_identifier_info_in_type(ty, offset, bindings) {
                    return Some(info);
                }
            }
            if let Some(PropertyValue::Expr(expr)) = &prop.value {
                tracing::trace!("Property has Expr value, checking expr at {:?}", expr.span);
                return find_identifier_info_in_expr(expr, offset, bindings);
            }
            if let Some(PropertyValue::Object(body)) = &prop.value {
                tracing::trace!(
                    "Property has Object value, checking body at {:?}",
                    body.span
                );
                return find_identifier_info_in_body(body, offset, bindings);
            }
        }
        ModuleMember::Method(method) => {
            if method.name.span.start <= offset && offset <= method.name.span.end {
                return Some(IdentifierInfo {
                    name: method.name.node.clone(),
                    kind: IdentifierKind::Variable,
                    local_bindings: bindings.to_vec(),
                });
            }
            // Add method parameters to bindings
            let mut method_bindings = bindings.to_vec();
            for param in &method.params {
                method_bindings.push(LocalBinding {
                    name: param.name.node.clone(),
                    span: param.name.span,
                });
                // Check parameter type
                if let Some(ref ty) = param.ty {
                    if let Some(info) = find_identifier_info_in_type(ty, offset, &method_bindings) {
                        return Some(info);
                    }
                }
            }
            // Check return type
            if let Some(ref ty) = method.return_type {
                if let Some(info) = find_identifier_info_in_type(ty, offset, &method_bindings) {
                    return Some(info);
                }
            }
            if let Some(ref body) = method.body {
                return find_identifier_info_in_expr(body, offset, &method_bindings);
            }
        }
        ModuleMember::Class(class) => {
            if class.name.span.start <= offset && offset <= class.name.span.end {
                return Some(IdentifierInfo {
                    name: class.name.node.clone(),
                    kind: IdentifierKind::Class,
                    local_bindings: bindings.to_vec(),
                });
            }
            // Check extends clause
            if let Some(ref extends) = class.extends {
                for part in &extends.parts {
                    if part.span.start <= offset && offset <= part.span.end {
                        return Some(IdentifierInfo {
                            name: part.node.clone(),
                            kind: IdentifierKind::Type,
                            local_bindings: bindings.to_vec(),
                        });
                    }
                }
            }
            // Search in class members
            for m in &class.members {
                match m {
                    ClassMember::Property(p) => {
                        if let Some(info) = find_identifier_info_in_member(
                            &ModuleMember::Property(p.clone()),
                            offset,
                            bindings,
                        ) {
                            return Some(info);
                        }
                    }
                    ClassMember::Method(m) => {
                        if let Some(info) = find_identifier_info_in_member(
                            &ModuleMember::Method(m.clone()),
                            offset,
                            bindings,
                        ) {
                            return Some(info);
                        }
                    }
                }
            }
        }
        ModuleMember::TypeAlias(alias) => {
            if alias.name.span.start <= offset && offset <= alias.name.span.end {
                return Some(IdentifierInfo {
                    name: alias.name.node.clone(),
                    kind: IdentifierKind::Type,
                    local_bindings: bindings.to_vec(),
                });
            }
            // Check the aliased type
            if let Some(info) = find_identifier_info_in_type(&alias.ty, offset, bindings) {
                return Some(info);
            }
        }
    }
    None
}

fn find_identifier_info_in_type(
    ty: &TypeAnnotation,
    offset: usize,
    bindings: &[LocalBinding],
) -> Option<IdentifierInfo> {
    if ty.span.start > offset || ty.span.end < offset {
        return None;
    }

    match &ty.kind {
        TypeKind::Named(ident) => {
            for part in &ident.parts {
                if part.span.start <= offset && offset <= part.span.end {
                    return Some(IdentifierInfo {
                        name: part.node.clone(),
                        kind: IdentifierKind::Type,
                        local_bindings: bindings.to_vec(),
                    });
                }
            }
        }
        TypeKind::Parameterized { base, args } => {
            for part in &base.parts {
                if part.span.start <= offset && offset <= part.span.end {
                    return Some(IdentifierInfo {
                        name: part.node.clone(),
                        kind: IdentifierKind::Type,
                        local_bindings: bindings.to_vec(),
                    });
                }
            }
            for arg in args {
                if let Some(info) = find_identifier_info_in_type(arg, offset, bindings) {
                    return Some(info);
                }
            }
        }
        TypeKind::Nullable(inner) => {
            return find_identifier_info_in_type(inner, offset, bindings);
        }
        TypeKind::Union(types) => {
            for t in types {
                if let Some(info) = find_identifier_info_in_type(t, offset, bindings) {
                    return Some(info);
                }
            }
        }
        TypeKind::Constrained { base, constraint } => {
            if let Some(info) = find_identifier_info_in_type(base, offset, bindings) {
                return Some(info);
            }
            return find_identifier_info_in_expr(constraint, offset, bindings);
        }
        TypeKind::Function {
            params,
            return_type,
        } => {
            for p in params {
                if let Some(info) = find_identifier_info_in_type(p, offset, bindings) {
                    return Some(info);
                }
            }
            return find_identifier_info_in_type(return_type, offset, bindings);
        }
        _ => {}
    }
    None
}

use rpkl_parser::PropertyValue;

fn find_identifier_info_in_expr(
    expr: &Expr,
    offset: usize,
    bindings: &[LocalBinding],
) -> Option<IdentifierInfo> {
    tracing::trace!(
        "find_identifier_info_in_expr: expr kind={:?}, span={:?}, offset={}",
        std::mem::discriminant(&expr.kind),
        expr.span,
        offset
    );
    if expr.span.start > offset || expr.span.end < offset {
        tracing::trace!("  -> offset outside expr span, returning None");
        return None;
    }

    match &expr.kind {
        ExprKind::Identifier(name) => {
            tracing::trace!("  -> Found Identifier '{}'", name);
            Some(IdentifierInfo {
                name: name.clone(),
                kind: IdentifierKind::Variable,
                local_bindings: bindings.to_vec(),
            })
        }
        ExprKind::MemberAccess { base, member } => {
            if member.span.start <= offset && offset <= member.span.end {
                Some(IdentifierInfo {
                    name: member.node.clone(),
                    kind: IdentifierKind::Variable,
                    local_bindings: bindings.to_vec(),
                })
            } else {
                find_identifier_info_in_expr(base, offset, bindings)
            }
        }
        ExprKind::OptionalMemberAccess { base, member } => {
            if member.span.start <= offset && offset <= member.span.end {
                Some(IdentifierInfo {
                    name: member.node.clone(),
                    kind: IdentifierKind::Variable,
                    local_bindings: bindings.to_vec(),
                })
            } else {
                find_identifier_info_in_expr(base, offset, bindings)
            }
        }
        ExprKind::Call { callee, args } => {
            if let Some(info) = find_identifier_info_in_expr(callee, offset, bindings) {
                return Some(info);
            }
            for arg in args {
                if let Some(info) = find_identifier_info_in_expr(arg, offset, bindings) {
                    return Some(info);
                }
            }
            None
        }
        ExprKind::Binary { left, right, .. } => {
            find_identifier_info_in_expr(left, offset, bindings)
                .or_else(|| find_identifier_info_in_expr(right, offset, bindings))
        }
        ExprKind::Unary { operand, .. } => find_identifier_info_in_expr(operand, offset, bindings),
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => find_identifier_info_in_expr(condition, offset, bindings)
            .or_else(|| find_identifier_info_in_expr(then_branch, offset, bindings))
            .or_else(|| find_identifier_info_in_expr(else_branch, offset, bindings)),
        ExprKind::Let {
            name,
            ty,
            value,
            body,
        } => {
            // Check if cursor is on the let variable name
            if name.span.start <= offset && offset <= name.span.end {
                return Some(IdentifierInfo {
                    name: name.node.clone(),
                    kind: IdentifierKind::Variable,
                    local_bindings: bindings.to_vec(),
                });
            }
            // Check type annotation
            if let Some(ref t) = ty {
                if let Some(info) = find_identifier_info_in_type(t, offset, bindings) {
                    return Some(info);
                }
            }
            // Check value expression
            if let Some(info) = find_identifier_info_in_expr(value, offset, bindings) {
                return Some(info);
            }
            // Check body with the new binding added
            let mut new_bindings = bindings.to_vec();
            new_bindings.push(LocalBinding {
                name: name.node.clone(),
                span: name.span,
            });
            find_identifier_info_in_expr(body, offset, &new_bindings)
        }
        ExprKind::Lambda { params, body } => {
            // Add lambda parameters to bindings
            let mut lambda_bindings = bindings.to_vec();
            for param in params {
                // Check if cursor is on a parameter
                if param.name.span.start <= offset && offset <= param.name.span.end {
                    return Some(IdentifierInfo {
                        name: param.name.node.clone(),
                        kind: IdentifierKind::Variable,
                        local_bindings: bindings.to_vec(),
                    });
                }
                // Check parameter type
                if let Some(ref ty) = param.ty {
                    if let Some(info) = find_identifier_info_in_type(ty, offset, &lambda_bindings) {
                        return Some(info);
                    }
                }
                lambda_bindings.push(LocalBinding {
                    name: param.name.node.clone(),
                    span: param.name.span,
                });
            }
            find_identifier_info_in_expr(body, offset, &lambda_bindings)
        }
        ExprKind::New { class_ref, body } => {
            // Check if clicking on class reference
            if let Some(ref class) = class_ref {
                for part in &class.parts {
                    if part.span.start <= offset && offset <= part.span.end {
                        return Some(IdentifierInfo {
                            name: part.node.clone(),
                            kind: IdentifierKind::Class,
                            local_bindings: bindings.to_vec(),
                        });
                    }
                }
            }
            find_identifier_info_in_body(body, offset, bindings)
        }
        ExprKind::Amend { base, body } => find_identifier_info_in_expr(base, offset, bindings)
            .or_else(|| find_identifier_info_in_body(body, offset, bindings)),
        ExprKind::Subscript { base, index } => find_identifier_info_in_expr(base, offset, bindings)
            .or_else(|| find_identifier_info_in_expr(index, offset, bindings)),
        ExprKind::Is { value, ty } | ExprKind::As { value, ty } => {
            if let Some(info) = find_identifier_info_in_expr(value, offset, bindings) {
                return Some(info);
            }
            find_identifier_info_in_type(ty, offset, bindings)
        }
        ExprKind::Parenthesized(inner) => find_identifier_info_in_expr(inner, offset, bindings),
        ExprKind::NonNullAssertion(inner) => find_identifier_info_in_expr(inner, offset, bindings),
        ExprKind::NullCoalesce { value, default } => {
            find_identifier_info_in_expr(value, offset, bindings)
                .or_else(|| find_identifier_info_in_expr(default, offset, bindings))
        }
        ExprKind::Pipe { value, function } => find_identifier_info_in_expr(value, offset, bindings)
            .or_else(|| find_identifier_info_in_expr(function, offset, bindings)),
        ExprKind::Throw(inner) | ExprKind::Trace(inner) => {
            find_identifier_info_in_expr(inner, offset, bindings)
        }
        ExprKind::Read { uri, .. } | ExprKind::ReadGlob { uri } => {
            find_identifier_info_in_expr(uri, offset, bindings)
        }
        ExprKind::String(string_lit) => {
            // Check interpolations inside the string
            for part in &string_lit.parts {
                if let rpkl_parser::StringPart::Interpolation(expr) = part {
                    if let Some(info) = find_identifier_info_in_expr(expr, offset, bindings) {
                        return Some(info);
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn find_identifier_info_in_body(
    body: &ObjectBody,
    offset: usize,
    bindings: &[LocalBinding],
) -> Option<IdentifierInfo> {
    tracing::trace!(
        "find_identifier_info_in_body at {:?}, offset={}, {} members",
        body.span,
        offset,
        body.members.len()
    );
    for member in &body.members {
        if let Some(info) = find_identifier_info_in_object_member(member, offset, bindings) {
            return Some(info);
        }
    }
    None
}

fn find_identifier_info_in_object_member(
    member: &ObjectMember,
    offset: usize,
    bindings: &[LocalBinding],
) -> Option<IdentifierInfo> {
    match member {
        ObjectMember::Property { name, value, span } => {
            tracing::trace!(
                "Checking ObjectMember::Property '{}' at {:?}, value expr at {:?}, offset={}",
                name.node,
                span,
                value.span,
                offset
            );
            if name.span.start <= offset && offset <= name.span.end {
                return Some(IdentifierInfo {
                    name: name.node.clone(),
                    kind: IdentifierKind::Variable,
                    local_bindings: bindings.to_vec(),
                });
            }
            find_identifier_info_in_expr(value, offset, bindings)
        }
        ObjectMember::PropertyAmend { name, body, .. } => {
            if name.span.start <= offset && offset <= name.span.end {
                return Some(IdentifierInfo {
                    name: name.node.clone(),
                    kind: IdentifierKind::Variable,
                    local_bindings: bindings.to_vec(),
                });
            }
            find_identifier_info_in_body(body, offset, bindings)
        }
        ObjectMember::Element { value, .. } => {
            find_identifier_info_in_expr(value, offset, bindings)
        }
        ObjectMember::Entry { key, value, .. } => {
            find_identifier_info_in_expr(key, offset, bindings)
                .or_else(|| find_identifier_info_in_expr(value, offset, bindings))
        }
        ObjectMember::EntryAmend { key, body, .. } => {
            find_identifier_info_in_expr(key, offset, bindings)
                .or_else(|| find_identifier_info_in_body(body, offset, bindings))
        }
        ObjectMember::Spread { value, .. } => find_identifier_info_in_expr(value, offset, bindings),
        ObjectMember::For {
            key_var,
            value_var,
            iterable,
            body,
            ..
        } => {
            // Check if cursor is on loop variables
            if let Some(ref kv) = key_var {
                if kv.span.start <= offset && offset <= kv.span.end {
                    return Some(IdentifierInfo {
                        name: kv.node.clone(),
                        kind: IdentifierKind::Variable,
                        local_bindings: bindings.to_vec(),
                    });
                }
            }
            if value_var.span.start <= offset && offset <= value_var.span.end {
                return Some(IdentifierInfo {
                    name: value_var.node.clone(),
                    kind: IdentifierKind::Variable,
                    local_bindings: bindings.to_vec(),
                });
            }
            // Check iterable
            if let Some(info) = find_identifier_info_in_expr(iterable, offset, bindings) {
                return Some(info);
            }
            // Check body with loop variables added
            let mut for_bindings = bindings.to_vec();
            if let Some(ref kv) = key_var {
                for_bindings.push(LocalBinding {
                    name: kv.node.clone(),
                    span: kv.span,
                });
            }
            for_bindings.push(LocalBinding {
                name: value_var.node.clone(),
                span: value_var.span,
            });
            find_identifier_info_in_body(body, offset, &for_bindings)
        }
        ObjectMember::When {
            condition,
            body,
            else_body,
            ..
        } => find_identifier_info_in_expr(condition, offset, bindings)
            .or_else(|| find_identifier_info_in_body(body, offset, bindings))
            .or_else(|| {
                else_body
                    .as_ref()
                    .and_then(|b| find_identifier_info_in_body(b, offset, bindings))
            }),
    }
}

/// Find the definition of a name in the module (properties, methods, classes)
fn find_definition_in_module(module: &Module, name: &str) -> Option<(usize, usize)> {
    for member in &module.members {
        match member {
            ModuleMember::Property(prop) if prop.name.node == name => {
                return Some((prop.name.span.start, prop.name.span.end));
            }
            ModuleMember::Method(method) if method.name.node == name => {
                return Some((method.name.span.start, method.name.span.end));
            }
            ModuleMember::Class(class) => {
                if class.name.node == name {
                    return Some((class.name.span.start, class.name.span.end));
                }
                // Also search within class members
                for m in &class.members {
                    match m {
                        ClassMember::Property(p) if p.name.node == name => {
                            return Some((p.name.span.start, p.name.span.end));
                        }
                        ClassMember::Method(m) if m.name.node == name => {
                            return Some((m.name.span.start, m.name.span.end));
                        }
                        _ => {}
                    }
                }
            }
            ModuleMember::TypeAlias(alias) if alias.name.node == name => {
                return Some((alias.name.span.start, alias.name.span.end));
            }
            _ => {}
        }
    }
    None
}

/// Find a type definition (class or typealias) in the module
fn find_type_definition_in_module(module: &Module, name: &str) -> Option<(usize, usize)> {
    for member in &module.members {
        match member {
            ModuleMember::Class(class) if class.name.node == name => {
                return Some((class.name.span.start, class.name.span.end));
            }
            ModuleMember::TypeAlias(alias) if alias.name.node == name => {
                return Some((alias.name.span.start, alias.name.span.end));
            }
            _ => {}
        }
    }
    None
}

/// Get hover information at a given offset
pub fn get_hover(doc: &Document, offset: usize) -> Option<Hover> {
    tracing::debug!("get_hover at offset {}", offset);
    let module = doc.ast.as_ref()?;

    // First, check if we're hovering over a definition (property, method, class, etc.)
    for member in &module.members {
        if let Some(hover) = get_hover_for_member(member, offset) {
            tracing::debug!("Found hover via get_hover_for_member");
            return Some(hover);
        }
    }

    // If not on a definition, check if we're on an identifier reference
    // and look up its definition to provide hover info
    let info = find_identifier_info_at_offset(module, offset);
    tracing::debug!(
        "get_hover: find_identifier_info_at_offset returned: {:?}",
        info
    );
    let info = info?;

    // Look up the definition based on the identifier kind
    match info.kind {
        IdentifierKind::Variable => {
            // First check local bindings
            for binding in &info.local_bindings {
                if binding.name == info.name {
                    // For local bindings, just show the name (we don't have type info)
                    let content = format!("```pkl\n(local) {}\n```", info.name);
                    return Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: content,
                        }),
                        range: None,
                    });
                }
            }
            // Look for module-level property or method
            get_hover_for_name(module, &info.name)
        }
        IdentifierKind::Type | IdentifierKind::Class => {
            // Look for class or typealias
            get_hover_for_type_name(module, &info.name)
        }
    }
}

/// Get hover info for a module-level name (property, method, or class)
fn get_hover_for_name(module: &Module, name: &str) -> Option<Hover> {
    for member in &module.members {
        match member {
            ModuleMember::Property(prop) if prop.name.node == name => {
                return get_hover_for_property(prop);
            }
            ModuleMember::Method(method) if method.name.node == name => {
                return get_hover_for_method(method);
            }
            ModuleMember::Class(class) if class.name.node == name => {
                return get_hover_for_class(class);
            }
            _ => {}
        }
    }
    None
}

/// Get hover info for a type name (class or typealias)
fn get_hover_for_type_name(module: &Module, name: &str) -> Option<Hover> {
    for member in &module.members {
        match member {
            ModuleMember::Class(class) if class.name.node == name => {
                return get_hover_for_class(class);
            }
            ModuleMember::TypeAlias(alias) if alias.name.node == name => {
                return get_hover_for_typealias(alias);
            }
            _ => {}
        }
    }
    None
}

fn get_hover_for_property(prop: &Property) -> Option<Hover> {
    let type_str = prop
        .ty
        .as_ref()
        .map(type_annotation_to_string)
        .unwrap_or_else(|| "unknown".to_string());

    let mut modifiers = Vec::new();
    if prop.modifiers.is_local {
        modifiers.push("local");
    }
    if prop.modifiers.is_hidden {
        modifiers.push("hidden");
    }
    if prop.modifiers.is_fixed {
        modifiers.push("fixed");
    }
    if prop.modifiers.is_const {
        modifiers.push("const");
    }

    let modifier_str = if modifiers.is_empty() {
        String::new()
    } else {
        format!("{} ", modifiers.join(" "))
    };

    let content = format!(
        "```pkl\n{}{}: {}\n```",
        modifier_str, prop.name.node, type_str
    );

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: content,
        }),
        range: None,
    })
}

fn get_hover_for_method(method: &Method) -> Option<Hover> {
    let params: Vec<String> = method
        .params
        .iter()
        .map(|p| {
            if let Some(ref ty) = p.ty {
                format!("{}: {}", p.name.node, type_annotation_to_string(ty))
            } else {
                p.name.node.clone()
            }
        })
        .collect();

    let return_type = method
        .return_type
        .as_ref()
        .map(type_annotation_to_string)
        .unwrap_or_else(|| "unknown".to_string());

    let content = format!(
        "```pkl\nfunction {}({}): {}\n```",
        method.name.node,
        params.join(", "),
        return_type
    );

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: content,
        }),
        range: None,
    })
}

fn get_hover_for_class(class: &ClassDef) -> Option<Hover> {
    let mut modifiers = Vec::new();
    if class.modifiers.is_abstract {
        modifiers.push("abstract");
    }
    if class.modifiers.is_open {
        modifiers.push("open");
    }
    if class.modifiers.is_local {
        modifiers.push("local");
    }

    let modifier_str = if modifiers.is_empty() {
        String::new()
    } else {
        format!("{} ", modifiers.join(" "))
    };

    let extends_str = class
        .extends
        .as_ref()
        .map(|e| format!(" extends {}", e))
        .unwrap_or_default();

    let content = format!(
        "```pkl\n{}class {}{}\n```",
        modifier_str, class.name.node, extends_str
    );

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: content,
        }),
        range: None,
    })
}

fn get_hover_for_typealias(alias: &TypeAlias) -> Option<Hover> {
    let content = format!(
        "```pkl\ntypealias {} = {}\n```",
        alias.name.node,
        type_annotation_to_string(&alias.ty)
    );

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: content,
        }),
        range: None,
    })
}

fn get_hover_for_member(member: &ModuleMember, offset: usize) -> Option<Hover> {
    match member {
        ModuleMember::Property(prop) => {
            if prop.name.span.start <= offset && offset <= prop.name.span.end {
                return get_hover_for_property(prop);
            }
        }
        ModuleMember::Method(method) => {
            if method.name.span.start <= offset && offset <= method.name.span.end {
                return get_hover_for_method(method);
            }
        }
        ModuleMember::Class(class) => {
            if class.name.span.start <= offset && offset <= class.name.span.end {
                return get_hover_for_class(class);
            }

            // Check class members
            for m in &class.members {
                match m {
                    ClassMember::Property(p) => {
                        if p.name.span.start <= offset && offset <= p.name.span.end {
                            return get_hover_for_property(p);
                        }
                    }
                    ClassMember::Method(m) => {
                        if m.name.span.start <= offset && offset <= m.name.span.end {
                            return get_hover_for_method(m);
                        }
                    }
                }
            }
        }
        ModuleMember::TypeAlias(alias) => {
            if alias.name.span.start <= offset && offset <= alias.name.span.end {
                return get_hover_for_typealias(alias);
            }
        }
    }

    None
}

/// Get completion items at a given offset
pub fn get_completions(doc: &Document, _offset: usize) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Add PKL keywords
    let keywords = [
        ("module", "Module declaration"),
        ("import", "Import declaration"),
        ("class", "Class definition"),
        ("typealias", "Type alias definition"),
        ("function", "Method definition"),
        ("if", "Conditional expression"),
        ("else", "Else branch"),
        ("let", "Local binding"),
        ("new", "Object creation"),
        ("for", "For generator"),
        ("when", "When generator"),
        ("local", "Local modifier"),
        ("hidden", "Hidden modifier"),
        ("fixed", "Fixed modifier"),
        ("const", "Const modifier"),
        ("abstract", "Abstract modifier"),
        ("open", "Open modifier"),
        ("external", "External modifier"),
        ("extends", "Class extension"),
        ("amends", "Module amendment"),
        ("true", "Boolean true"),
        ("false", "Boolean false"),
        ("null", "Null value"),
        ("this", "Current object reference"),
        ("outer", "Enclosing object reference"),
        ("super", "Parent class reference"),
        ("throw", "Throw exception"),
        ("trace", "Debug trace"),
        ("read", "Read external resource"),
    ];

    for (kw, desc) in keywords {
        items.push(CompletionItem {
            label: kw.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(desc.to_string()),
            ..Default::default()
        });
    }

    // Add built-in types
    let types = [
        ("String", "String type"),
        ("Int", "Integer type"),
        ("Float", "Floating point type"),
        ("Boolean", "Boolean type"),
        ("Duration", "Duration type"),
        ("DataSize", "Data size type"),
        ("List", "List collection type"),
        ("Set", "Set collection type"),
        ("Map", "Map collection type"),
        ("Listing", "Listing collection type"),
        ("Mapping", "Mapping collection type"),
        ("Dynamic", "Dynamic object type"),
        ("Any", "Any type"),
        ("Null", "Null type"),
        ("Pair", "Pair type"),
        ("Regex", "Regular expression type"),
        ("IntSeq", "Integer sequence type"),
    ];

    for (ty, desc) in types {
        items.push(CompletionItem {
            label: ty.to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some(desc.to_string()),
            ..Default::default()
        });
    }

    // Add symbols from the current document
    if let Some(ref module) = doc.ast {
        for member in &module.members {
            match member {
                ModuleMember::Property(prop) => {
                    items.push(CompletionItem {
                        label: prop.name.node.clone(),
                        kind: Some(CompletionItemKind::PROPERTY),
                        detail: prop.ty.as_ref().map(type_annotation_to_string),
                        ..Default::default()
                    });
                }
                ModuleMember::Method(method) => {
                    items.push(CompletionItem {
                        label: method.name.node.clone(),
                        kind: Some(CompletionItemKind::METHOD),
                        detail: Some(format!(
                            "({}) -> {}",
                            method.params.len(),
                            method
                                .return_type
                                .as_ref()
                                .map(type_annotation_to_string)
                                .unwrap_or_else(|| "unknown".to_string())
                        )),
                        ..Default::default()
                    });
                }
                ModuleMember::Class(class) => {
                    items.push(CompletionItem {
                        label: class.name.node.clone(),
                        kind: Some(CompletionItemKind::CLASS),
                        detail: class.extends.as_ref().map(|e| format!("extends {}", e)),
                        ..Default::default()
                    });
                }
                ModuleMember::TypeAlias(alias) => {
                    items.push(CompletionItem {
                        label: alias.name.node.clone(),
                        kind: Some(CompletionItemKind::TYPE_PARAMETER),
                        detail: Some(type_annotation_to_string(&alias.ty)),
                        ..Default::default()
                    });
                }
            }
        }
    }

    items
}
