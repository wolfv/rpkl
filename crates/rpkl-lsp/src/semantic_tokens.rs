//! Semantic token support for syntax highlighting

use rpkl_parser::{
    ClassMember, Expr, ExprKind, Module, ModuleMember, ObjectBody, ObjectMember, PropertyValue,
    Span, TypeAnnotation, TypeKind,
};
use tower_lsp::lsp_types::{SemanticToken, SemanticTokenType, SemanticTokensLegend};

use crate::document::Document;

/// Token types for semantic highlighting
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
#[allow(dead_code)]
pub enum TokenType {
    Namespace = 0,
    Type = 1,
    Class = 2,
    Enum = 3,
    Interface = 4,
    Struct = 5,
    TypeParameter = 6,
    Parameter = 7,
    Variable = 8,
    Property = 9,
    EnumMember = 10,
    Event = 11,
    Function = 12,
    Method = 13,
    Macro = 14,
    Keyword = 15,
    Modifier = 16,
    Comment = 17,
    String = 18,
    Number = 19,
    Regexp = 20,
    Operator = 21,
}

impl TokenType {
    #[allow(dead_code)]
    fn as_str(&self) -> &'static str {
        match self {
            TokenType::Namespace => "namespace",
            TokenType::Type => "type",
            TokenType::Class => "class",
            TokenType::Enum => "enum",
            TokenType::Interface => "interface",
            TokenType::Struct => "struct",
            TokenType::TypeParameter => "typeParameter",
            TokenType::Parameter => "parameter",
            TokenType::Variable => "variable",
            TokenType::Property => "property",
            TokenType::EnumMember => "enumMember",
            TokenType::Event => "event",
            TokenType::Function => "function",
            TokenType::Method => "method",
            TokenType::Macro => "macro",
            TokenType::Keyword => "keyword",
            TokenType::Modifier => "modifier",
            TokenType::Comment => "comment",
            TokenType::String => "string",
            TokenType::Number => "number",
            TokenType::Regexp => "regexp",
            TokenType::Operator => "operator",
        }
    }
}

/// Get the semantic token legend
pub fn semantic_token_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::NAMESPACE,
            SemanticTokenType::TYPE,
            SemanticTokenType::CLASS,
            SemanticTokenType::ENUM,
            SemanticTokenType::INTERFACE,
            SemanticTokenType::STRUCT,
            SemanticTokenType::TYPE_PARAMETER,
            SemanticTokenType::PARAMETER,
            SemanticTokenType::VARIABLE,
            SemanticTokenType::PROPERTY,
            SemanticTokenType::ENUM_MEMBER,
            SemanticTokenType::EVENT,
            SemanticTokenType::FUNCTION,
            SemanticTokenType::METHOD,
            SemanticTokenType::MACRO,
            SemanticTokenType::KEYWORD,
            SemanticTokenType::MODIFIER,
            SemanticTokenType::COMMENT,
            SemanticTokenType::STRING,
            SemanticTokenType::NUMBER,
            SemanticTokenType::REGEXP,
            SemanticTokenType::OPERATOR,
        ],
        token_modifiers: vec![],
    }
}

/// A raw token before encoding
struct RawToken {
    span: Span,
    token_type: TokenType,
}

/// Get semantic tokens for a document
pub fn semantic_tokens_full(doc: &Document) -> Vec<SemanticToken> {
    let Some(ref module) = doc.ast else {
        return vec![];
    };

    let mut tokens = Vec::new();
    collect_module_tokens(module, &mut tokens);

    // Sort tokens by position
    tokens.sort_by_key(|t| t.span.start);

    // Encode tokens as deltas
    encode_tokens(&tokens, doc)
}

fn collect_module_tokens(module: &Module, tokens: &mut Vec<RawToken>) {
    // Collect import tokens
    for import in &module.imports {
        // The import keyword would be nice to highlight, but we don't have its span
        // We can highlight the URI
        tokens.push(RawToken {
            span: import.uri.span,
            token_type: TokenType::String,
        });

        if let Some(ref alias) = import.alias {
            tokens.push(RawToken {
                span: alias.span,
                token_type: TokenType::Variable,
            });
        }
    }

    // Collect member tokens
    for member in &module.members {
        collect_member_tokens(member, tokens);
    }
}

fn collect_member_tokens(member: &ModuleMember, tokens: &mut Vec<RawToken>) {
    match member {
        ModuleMember::Property(prop) => {
            tokens.push(RawToken {
                span: prop.name.span,
                token_type: TokenType::Property,
            });

            if let Some(ref ty) = prop.ty {
                collect_type_tokens(ty, tokens);
            }

            if let Some(ref value) = prop.value {
                match value {
                    PropertyValue::Expr(expr) => collect_expr_tokens(expr, tokens),
                    PropertyValue::Object(body) => collect_body_tokens(body, tokens),
                }
            }
        }
        ModuleMember::Method(method) => {
            tokens.push(RawToken {
                span: method.name.span,
                token_type: TokenType::Method,
            });

            for param in &method.params {
                tokens.push(RawToken {
                    span: param.name.span,
                    token_type: TokenType::Parameter,
                });
                if let Some(ref ty) = param.ty {
                    collect_type_tokens(ty, tokens);
                }
            }

            if let Some(ref ty) = method.return_type {
                collect_type_tokens(ty, tokens);
            }

            if let Some(ref body) = method.body {
                collect_expr_tokens(body, tokens);
            }
        }
        ModuleMember::Class(class) => {
            tokens.push(RawToken {
                span: class.name.span,
                token_type: TokenType::Class,
            });

            for param in &class.type_params {
                tokens.push(RawToken {
                    span: param.name.span,
                    token_type: TokenType::TypeParameter,
                });
            }

            if let Some(ref extends) = class.extends {
                for part in &extends.parts {
                    tokens.push(RawToken {
                        span: part.span,
                        token_type: TokenType::Type,
                    });
                }
            }

            for m in &class.members {
                match m {
                    ClassMember::Property(p) => {
                        collect_member_tokens(&ModuleMember::Property(p.clone()), tokens);
                    }
                    ClassMember::Method(m) => {
                        collect_member_tokens(&ModuleMember::Method(m.clone()), tokens);
                    }
                }
            }
        }
        ModuleMember::TypeAlias(alias) => {
            tokens.push(RawToken {
                span: alias.name.span,
                token_type: TokenType::Type,
            });

            for param in &alias.type_params {
                tokens.push(RawToken {
                    span: param.name.span,
                    token_type: TokenType::TypeParameter,
                });
            }

            collect_type_tokens(&alias.ty, tokens);
        }
    }
}

fn collect_type_tokens(ty: &TypeAnnotation, tokens: &mut Vec<RawToken>) {
    match &ty.kind {
        TypeKind::Named(ident) => {
            for part in &ident.parts {
                tokens.push(RawToken {
                    span: part.span,
                    token_type: TokenType::Type,
                });
            }
        }
        TypeKind::Parameterized { base, args } => {
            for part in &base.parts {
                tokens.push(RawToken {
                    span: part.span,
                    token_type: TokenType::Type,
                });
            }
            for arg in args {
                collect_type_tokens(arg, tokens);
            }
        }
        TypeKind::Nullable(inner) => {
            collect_type_tokens(inner, tokens);
        }
        TypeKind::Union(types) => {
            for t in types {
                collect_type_tokens(t, tokens);
            }
        }
        TypeKind::Constrained { base, constraint } => {
            collect_type_tokens(base, tokens);
            collect_expr_tokens(constraint, tokens);
        }
        TypeKind::Function {
            params,
            return_type,
        } => {
            for p in params {
                collect_type_tokens(p, tokens);
            }
            collect_type_tokens(return_type, tokens);
        }
        TypeKind::StringLiteral(_) => {
            tokens.push(RawToken {
                span: ty.span,
                token_type: TokenType::String,
            });
        }
        TypeKind::Nothing | TypeKind::Unknown | TypeKind::Module => {
            tokens.push(RawToken {
                span: ty.span,
                token_type: TokenType::Type,
            });
        }
    }
}

fn collect_expr_tokens(expr: &Expr, tokens: &mut Vec<RawToken>) {
    match &expr.kind {
        ExprKind::Null | ExprKind::Bool(_) => {
            tokens.push(RawToken {
                span: expr.span,
                token_type: TokenType::Keyword,
            });
        }
        ExprKind::Int(_) | ExprKind::Float(_) => {
            tokens.push(RawToken {
                span: expr.span,
                token_type: TokenType::Number,
            });
        }
        ExprKind::String(s) => {
            tokens.push(RawToken {
                span: s.span,
                token_type: TokenType::String,
            });
            // Also collect interpolation expressions
            for part in &s.parts {
                if let rpkl_parser::StringPart::Interpolation(e) = part {
                    collect_expr_tokens(e, tokens);
                }
            }
        }
        ExprKind::Identifier(_) => {
            tokens.push(RawToken {
                span: expr.span,
                token_type: TokenType::Variable,
            });
        }
        ExprKind::This | ExprKind::Super | ExprKind::Outer | ExprKind::Module => {
            tokens.push(RawToken {
                span: expr.span,
                token_type: TokenType::Keyword,
            });
        }
        ExprKind::New { class_ref, body } => {
            if let Some(ref class) = class_ref {
                for part in &class.parts {
                    tokens.push(RawToken {
                        span: part.span,
                        token_type: TokenType::Type,
                    });
                }
            }
            collect_body_tokens(body, tokens);
        }
        ExprKind::Amend { base, body } => {
            collect_expr_tokens(base, tokens);
            collect_body_tokens(body, tokens);
        }
        ExprKind::MemberAccess { base, member } => {
            collect_expr_tokens(base, tokens);
            tokens.push(RawToken {
                span: member.span,
                token_type: TokenType::Property,
            });
        }
        ExprKind::OptionalMemberAccess { base, member } => {
            collect_expr_tokens(base, tokens);
            tokens.push(RawToken {
                span: member.span,
                token_type: TokenType::Property,
            });
        }
        ExprKind::Binary { left, right, .. } => {
            collect_expr_tokens(left, tokens);
            collect_expr_tokens(right, tokens);
        }
        ExprKind::Unary { operand, .. } => {
            collect_expr_tokens(operand, tokens);
        }
        ExprKind::Subscript { base, index } => {
            collect_expr_tokens(base, tokens);
            collect_expr_tokens(index, tokens);
        }
        ExprKind::Call { callee, args } => {
            collect_expr_tokens(callee, tokens);
            for arg in args {
                collect_expr_tokens(arg, tokens);
            }
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_expr_tokens(condition, tokens);
            collect_expr_tokens(then_branch, tokens);
            collect_expr_tokens(else_branch, tokens);
        }
        ExprKind::Let {
            name,
            ty,
            value,
            body,
        } => {
            tokens.push(RawToken {
                span: name.span,
                token_type: TokenType::Variable,
            });
            if let Some(ref t) = ty {
                collect_type_tokens(t, tokens);
            }
            collect_expr_tokens(value, tokens);
            collect_expr_tokens(body, tokens);
        }
        ExprKind::Lambda { params, body } => {
            for param in params {
                tokens.push(RawToken {
                    span: param.name.span,
                    token_type: TokenType::Parameter,
                });
                if let Some(ref ty) = param.ty {
                    collect_type_tokens(ty, tokens);
                }
            }
            collect_expr_tokens(body, tokens);
        }
        ExprKind::Is { value, ty } => {
            collect_expr_tokens(value, tokens);
            collect_type_tokens(ty, tokens);
        }
        ExprKind::As { value, ty } => {
            collect_expr_tokens(value, tokens);
            collect_type_tokens(ty, tokens);
        }
        ExprKind::NonNullAssertion(inner) => {
            collect_expr_tokens(inner, tokens);
        }
        ExprKind::NullCoalesce { value, default } => {
            collect_expr_tokens(value, tokens);
            collect_expr_tokens(default, tokens);
        }
        ExprKind::Pipe { value, function } => {
            collect_expr_tokens(value, tokens);
            collect_expr_tokens(function, tokens);
        }
        ExprKind::Throw(inner) | ExprKind::Trace(inner) => {
            collect_expr_tokens(inner, tokens);
        }
        ExprKind::Read { uri, .. } | ExprKind::ReadGlob { uri } => {
            collect_expr_tokens(uri, tokens);
        }
        ExprKind::Parenthesized(inner) => {
            collect_expr_tokens(inner, tokens);
        }
    }
}

fn collect_body_tokens(body: &ObjectBody, tokens: &mut Vec<RawToken>) {
    for member in &body.members {
        collect_object_member_tokens(member, tokens);
    }
}

fn collect_object_member_tokens(member: &ObjectMember, tokens: &mut Vec<RawToken>) {
    match member {
        ObjectMember::Property { name, value, .. } => {
            tokens.push(RawToken {
                span: name.span,
                token_type: TokenType::Property,
            });
            collect_expr_tokens(value, tokens);
        }
        ObjectMember::PropertyAmend { name, body, .. } => {
            tokens.push(RawToken {
                span: name.span,
                token_type: TokenType::Property,
            });
            collect_body_tokens(body, tokens);
        }
        ObjectMember::Element { value, .. } => {
            collect_expr_tokens(value, tokens);
        }
        ObjectMember::Entry { key, value, .. } => {
            collect_expr_tokens(key, tokens);
            collect_expr_tokens(value, tokens);
        }
        ObjectMember::EntryAmend { key, body, .. } => {
            collect_expr_tokens(key, tokens);
            collect_body_tokens(body, tokens);
        }
        ObjectMember::Spread { value, .. } => {
            collect_expr_tokens(value, tokens);
        }
        ObjectMember::For {
            key_var,
            value_var,
            iterable,
            body,
            ..
        } => {
            if let Some(ref k) = key_var {
                tokens.push(RawToken {
                    span: k.span,
                    token_type: TokenType::Variable,
                });
            }
            tokens.push(RawToken {
                span: value_var.span,
                token_type: TokenType::Variable,
            });
            collect_expr_tokens(iterable, tokens);
            collect_body_tokens(body, tokens);
        }
        ObjectMember::When {
            condition,
            body,
            else_body,
            ..
        } => {
            collect_expr_tokens(condition, tokens);
            collect_body_tokens(body, tokens);
            if let Some(ref eb) = else_body {
                collect_body_tokens(eb, tokens);
            }
        }
    }
}

/// Encode raw tokens as semantic tokens with delta encoding
fn encode_tokens(raw_tokens: &[RawToken], doc: &Document) -> Vec<SemanticToken> {
    let mut result = Vec::with_capacity(raw_tokens.len());
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    for token in raw_tokens {
        let Some(start_pos) = doc.offset_to_position(token.span.start) else {
            continue;
        };
        let Some(end_pos) = doc.offset_to_position(token.span.end) else {
            continue;
        };

        let line = start_pos.line;
        let start = start_pos.character;
        let length = if start_pos.line == end_pos.line {
            end_pos.character.saturating_sub(start_pos.character)
        } else {
            // Multi-line token, just use the first line
            // Get the length of the token text on the first line
            let line_end = doc.text[token.span.start..]
                .find('\n')
                .map(|i| i as u32)
                .unwrap_or((token.span.end - token.span.start) as u32);
            line_end
        };

        let delta_line = line - prev_line;
        let delta_start = if delta_line == 0 {
            start - prev_start
        } else {
            start
        };

        result.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: token.token_type as u32,
            token_modifiers_bitset: 0,
        });

        prev_line = line;
        prev_start = start;
    }

    result
}
