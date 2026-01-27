//! Abstract Syntax Tree definitions for PKL

use std::fmt;

/// Source location span
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

/// A node with associated source span
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}

/// Simple identifier
pub type Identifier = Spanned<String>;

/// Qualified identifier (e.g., `foo.bar.baz`)
#[derive(Debug, Clone, PartialEq)]
pub struct QualifiedIdent {
    pub parts: Vec<Identifier>,
}

impl QualifiedIdent {
    pub fn simple(name: Identifier) -> Self {
        Self { parts: vec![name] }
    }
}

impl fmt::Display for QualifiedIdent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s: String = self
            .parts
            .iter()
            .map(|p| p.node.as_str())
            .collect::<Vec<_>>()
            .join(".");
        write!(f, "{}", s)
    }
}

// =============================================================================
// Module
// =============================================================================

/// A complete PKL module
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub header: Option<ModuleHeader>,
    pub imports: Vec<Import>,
    pub members: Vec<ModuleMember>,
    pub span: Span,
}

/// Module header declaration
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleHeader {
    pub annotations: Vec<Annotation>,
    pub kind: ModuleKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleKind {
    /// `module foo.bar`
    Module {
        is_open: bool,
        name: Option<QualifiedIdent>,
    },
    /// `amends "path/to/module.pkl"`
    Amends { uri: StringLiteral },
    /// `extends "path/to/module.pkl"`
    Extends { uri: StringLiteral },
}

/// Import declaration
#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub kind: ImportKind,
    pub uri: StringLiteral,
    pub alias: Option<Identifier>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImportKind {
    Normal,
    Glob,
}

/// Annotation (e.g., `@Deprecated { message = "..." }`)
#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
    pub name: QualifiedIdent,
    pub body: Option<ObjectBody>,
    pub span: Span,
}

// =============================================================================
// Module Members
// =============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleMember {
    Class(ClassDef),
    TypeAlias(TypeAlias),
    Property(Property),
    Method(Method),
}

// =============================================================================
// Class Definition
// =============================================================================

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDef {
    pub annotations: Vec<Annotation>,
    pub modifiers: ClassModifiers,
    pub name: Identifier,
    pub type_params: Vec<TypeParameter>,
    pub extends: Option<QualifiedIdent>,
    pub members: Vec<ClassMember>,
    pub span: Span,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ClassModifiers {
    pub is_abstract: bool,
    pub is_open: bool,
    pub is_local: bool,
    pub is_external: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeParameter {
    pub variance: Option<Variance>,
    pub name: Identifier,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Variance {
    In,
    Out,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClassMember {
    Property(Property),
    Method(Method),
}

// =============================================================================
// Type Alias
// =============================================================================

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAlias {
    pub annotations: Vec<Annotation>,
    pub modifiers: TypeAliasModifiers,
    pub name: Identifier,
    pub type_params: Vec<TypeParameter>,
    pub ty: TypeAnnotation,
    pub span: Span,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TypeAliasModifiers {
    pub is_local: bool,
    pub is_external: bool,
}

// =============================================================================
// Property and Method
// =============================================================================

#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    pub annotations: Vec<Annotation>,
    pub modifiers: PropertyModifiers,
    pub name: Identifier,
    pub ty: Option<TypeAnnotation>,
    pub value: Option<PropertyValue>,
    pub span: Span,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct PropertyModifiers {
    pub is_local: bool,
    pub is_hidden: bool,
    pub is_fixed: bool,
    pub is_const: bool,
    pub is_external: bool,
    pub is_abstract: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PropertyValue {
    Expr(Expr),
    Object(ObjectBody),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    pub annotations: Vec<Annotation>,
    pub modifiers: PropertyModifiers,
    pub name: Identifier,
    pub params: Vec<Parameter>,
    pub return_type: Option<TypeAnnotation>,
    pub body: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Identifier,
    pub ty: Option<TypeAnnotation>,
    pub span: Span,
}

// =============================================================================
// Type Annotations
// =============================================================================

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnotation {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    /// Simple type reference: `String`, `Int`, `MyClass`
    Named(QualifiedIdent),

    /// Parameterized type: `List<String>`, `Map<String, Int>`
    Parameterized {
        base: QualifiedIdent,
        args: Vec<TypeAnnotation>,
    },

    /// Nullable type: `String?`
    Nullable(Box<TypeAnnotation>),

    /// Union type: `String | Int`
    Union(Vec<TypeAnnotation>),

    /// Constrained type: `Int(this > 0)`, `String(!isEmpty)`
    Constrained {
        base: Box<TypeAnnotation>,
        constraint: Box<Expr>,
    },

    /// Function type: `(Int, Int) -> Int`
    Function {
        params: Vec<TypeAnnotation>,
        return_type: Box<TypeAnnotation>,
    },

    /// String literal type: `"json"` | `"yaml"`
    StringLiteral(String),

    /// `nothing` type (bottom type)
    Nothing,

    /// `unknown` type
    Unknown,

    /// `module` type
    Module,
}

// =============================================================================
// Expressions
// =============================================================================

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    // Literals
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(StringLiteral),

    // Identifiers and references
    Identifier(String),
    This,
    Super,
    Outer,
    Module,

    // Object creation and amendment
    New {
        class_ref: Option<QualifiedIdent>,
        body: ObjectBody,
    },
    Amend {
        base: Box<Expr>,
        body: ObjectBody,
    },

    // Member access
    MemberAccess {
        base: Box<Expr>,
        member: Identifier,
    },
    OptionalMemberAccess {
        base: Box<Expr>,
        member: Identifier,
    },

    // Binary operations
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    // Unary operations
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },

    // Subscript
    Subscript {
        base: Box<Expr>,
        index: Box<Expr>,
    },

    // Function/method call
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },

    // Control flow
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    Let {
        name: Identifier,
        ty: Option<TypeAnnotation>,
        value: Box<Expr>,
        body: Box<Expr>,
    },

    // Lambda
    Lambda {
        params: Vec<Parameter>,
        body: Box<Expr>,
    },

    // Type operations
    Is {
        value: Box<Expr>,
        ty: TypeAnnotation,
    },
    As {
        value: Box<Expr>,
        ty: TypeAnnotation,
    },

    // Null handling
    NonNullAssertion(Box<Expr>),
    NullCoalesce {
        value: Box<Expr>,
        default: Box<Expr>,
    },

    // Pipe operator
    Pipe {
        value: Box<Expr>,
        function: Box<Expr>,
    },

    // Special operations
    Throw(Box<Expr>),
    Trace(Box<Expr>),
    Read {
        uri: Box<Expr>,
        is_nullable: bool,
    },
    ReadGlob {
        uri: Box<Expr>,
    },

    // Parenthesized (for preserving source info)
    Parenthesized(Box<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    IntDiv,
    Mod,
    Pow,
    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    // Logical
    And,
    Or,
    // Null coalescing and pipe
    NullCoalesce,
    Pipe,
    // Type operators
    Is,
    As,
}

impl BinaryOp {
    /// Returns the precedence (higher = binds tighter)
    pub fn precedence(self) -> u8 {
        match self {
            BinaryOp::NullCoalesce => 0,
            BinaryOp::Pipe => 0,
            BinaryOp::Or => 1,
            BinaryOp::And => 2,
            BinaryOp::Eq | BinaryOp::Ne => 3,
            BinaryOp::Is | BinaryOp::As => 3,
            BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => 4,
            BinaryOp::Add | BinaryOp::Sub => 5,
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::IntDiv | BinaryOp::Mod => 6,
            BinaryOp::Pow => 7,
        }
    }

    /// Returns true if the operator is right-associative
    pub fn is_right_assoc(self) -> bool {
        matches!(self, BinaryOp::Pow | BinaryOp::NullCoalesce)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

// =============================================================================
// String Literals
// =============================================================================

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub parts: Vec<StringPart>,
    pub span: Span,
}

impl StringLiteral {
    pub fn simple(value: String, span: Span) -> Self {
        Self {
            parts: vec![StringPart::Literal(value)],
            span,
        }
    }

    /// Returns true if this is a simple string with no interpolation
    pub fn is_simple(&self) -> bool {
        self.parts.len() == 1 && matches!(&self.parts[0], StringPart::Literal(_))
    }

    /// Get the simple string value if this is not interpolated
    pub fn as_simple(&self) -> Option<&str> {
        if self.is_simple() {
            if let StringPart::Literal(s) = &self.parts[0] {
                return Some(s);
            }
        }
        None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringPart {
    Literal(String),
    Interpolation(Expr),
}

// =============================================================================
// Object Body
// =============================================================================

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectBody {
    pub members: Vec<ObjectMember>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectMember {
    /// `property = value`
    Property {
        name: Identifier,
        value: Expr,
        span: Span,
    },

    /// `property { ... }` (amend shorthand)
    PropertyAmend {
        name: Identifier,
        body: ObjectBody,
        span: Span,
    },

    /// Unlabeled expression (element)
    Element { value: Expr, span: Span },

    /// `[key] = value`
    Entry { key: Expr, value: Expr, span: Span },

    /// `[key] { ... }` (entry amend)
    EntryAmend {
        key: Expr,
        body: ObjectBody,
        span: Span,
    },

    /// `...expr` or `...?expr`
    Spread {
        is_nullable: bool,
        value: Expr,
        span: Span,
    },

    /// `for (x in iterable) { ... }`
    For {
        key_var: Option<Identifier>,
        value_var: Identifier,
        iterable: Expr,
        body: ObjectBody,
        span: Span,
    },

    /// `when (condition) { ... } else { ... }`
    When {
        condition: Expr,
        body: ObjectBody,
        else_body: Option<ObjectBody>,
        span: Span,
    },
}
