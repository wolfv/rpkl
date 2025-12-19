//! Parser implementation: converts pest output to AST

use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

use crate::ast::*;
use crate::error::{ParseError, ParseResult};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct PklParser;

/// Parse a PKL module from source text
pub fn parse_module(source: &str) -> ParseResult<Module> {
    let pairs = PklParser::parse(Rule::module, source)?;
    let pair = pairs.into_iter().next().unwrap();
    build_module(pair)
}

/// Parse a single expression (requires full input consumption)
pub fn parse_expression(source: &str) -> ParseResult<Expr> {
    let pairs = PklParser::parse(Rule::standalone_expression, source)?;
    let pair = pairs.into_iter().next().unwrap();
    // The standalone_expression contains SOI ~ expression ~ EOI, extract the expression
    let inner = pair
        .into_inner()
        .find(|p| p.as_rule() == Rule::expression)
        .unwrap();
    build_expression(inner)
}

// =============================================================================
// Helper functions
// =============================================================================

fn span_from_pair(pair: &Pair<Rule>) -> Span {
    let pest_span = pair.as_span();
    Span::new(pest_span.start(), pest_span.end())
}

// =============================================================================
// Module building
// =============================================================================

fn build_module(pair: Pair<Rule>) -> ParseResult<Module> {
    debug_assert_eq!(pair.as_rule(), Rule::module);
    let span = span_from_pair(&pair);

    let mut header = None;
    let mut imports = Vec::new();
    let mut members = Vec::new();

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::module_header => {
                header = Some(build_module_header(inner)?);
            }
            Rule::import_clause => {
                imports.push(build_import(inner)?);
            }
            Rule::module_member => {
                members.push(build_module_member(inner)?);
            }
            Rule::EOI => {}
            _ => {}
        }
    }

    Ok(Module {
        header,
        imports,
        members,
        span,
    })
}

fn build_module_header(pair: Pair<Rule>) -> ParseResult<ModuleHeader> {
    debug_assert_eq!(pair.as_rule(), Rule::module_header);
    let span = span_from_pair(&pair);

    let mut annotations = Vec::new();
    let mut kind = None;

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::annotation => {
                annotations.push(build_annotation(inner)?);
            }
            Rule::module_clause => {
                kind = Some(build_module_clause(inner)?);
            }
            _ => {}
        }
    }

    Ok(ModuleHeader {
        annotations,
        kind: kind.unwrap(),
        span,
    })
}

fn build_module_clause(pair: Pair<Rule>) -> ParseResult<ModuleKind> {
    debug_assert_eq!(pair.as_rule(), Rule::module_clause);

    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::module_decl => {
            let mut is_open = false;
            let mut name = None;
            for part in inner.into_inner() {
                match part.as_rule() {
                    Rule::module_modifier => {
                        is_open = part.as_str() == "open";
                    }
                    Rule::qualified_identifier => {
                        name = Some(build_qualified_ident(part)?);
                    }
                    _ => {}
                }
            }
            Ok(ModuleKind::Module { is_open, name })
        }
        Rule::amends_clause => {
            let uri = build_string_literal(inner.into_inner().next().unwrap())?;
            Ok(ModuleKind::Amends { uri })
        }
        Rule::extends_clause => {
            let uri = build_string_literal(inner.into_inner().next().unwrap())?;
            Ok(ModuleKind::Extends { uri })
        }
        _ => unreachable!("unexpected module clause: {:?}", inner.as_rule()),
    }
}

fn build_import(pair: Pair<Rule>) -> ParseResult<Import> {
    let span = span_from_pair(&pair);
    let inner = pair.into_inner().next().unwrap();

    let (kind, mut parts) = match inner.as_rule() {
        Rule::normal_import => (ImportKind::Normal, inner.into_inner()),
        Rule::glob_import => (ImportKind::Glob, inner.into_inner()),
        _ => unreachable!(),
    };

    let uri = build_string_literal(parts.next().unwrap())?;
    let alias = parts.next().map(|p| {
        // import_alias contains "as" + identifier
        let ident = p.into_inner().next().unwrap();
        build_identifier(ident)
    });

    Ok(Import {
        kind,
        uri,
        alias,
        span,
    })
}

fn build_annotation(pair: Pair<Rule>) -> ParseResult<Annotation> {
    debug_assert_eq!(pair.as_rule(), Rule::annotation);
    let span = span_from_pair(&pair);

    let mut inner = pair.into_inner();
    let name = build_qualified_ident(inner.next().unwrap())?;
    let body = inner.next().map(|p| build_object_body(p)).transpose()?;

    Ok(Annotation { name, body, span })
}

// =============================================================================
// Module members
// =============================================================================

fn build_module_member(pair: Pair<Rule>) -> ParseResult<ModuleMember> {
    debug_assert_eq!(pair.as_rule(), Rule::module_member);

    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::class_definition => Ok(ModuleMember::Class(build_class_def(inner)?)),
        Rule::type_alias_definition => Ok(ModuleMember::TypeAlias(build_type_alias(inner)?)),
        Rule::annotated_property => Ok(ModuleMember::Property(build_annotated_property(inner)?)),
        Rule::annotated_method => Ok(ModuleMember::Method(build_annotated_method(inner)?)),
        _ => unreachable!("unexpected module member: {:?}", inner.as_rule()),
    }
}

fn build_class_def(pair: Pair<Rule>) -> ParseResult<ClassDef> {
    debug_assert_eq!(pair.as_rule(), Rule::class_definition);
    let span = span_from_pair(&pair);

    let mut annotations = Vec::new();
    let mut modifiers = ClassModifiers::default();
    let mut name = None;
    let mut type_params = Vec::new();
    let mut extends = None;
    let mut members = Vec::new();

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::annotation => {
                annotations.push(build_annotation(inner)?);
            }
            Rule::class_modifier => match inner.as_str() {
                "abstract" => modifiers.is_abstract = true,
                "open" => modifiers.is_open = true,
                "local" => modifiers.is_local = true,
                "external" => modifiers.is_external = true,
                _ => {}
            },
            Rule::identifier => {
                name = Some(build_identifier(inner));
            }
            Rule::type_parameters => {
                type_params = build_type_parameters(inner)?;
            }
            Rule::extends_clause_class => {
                // extends_clause_class -> type_reference -> qualified_identifier
                let type_ref = inner.into_inner().next().unwrap();
                let qualified = type_ref.into_inner().next().unwrap();
                extends = Some(build_qualified_ident(qualified)?);
            }
            Rule::class_body => {
                for member in inner.into_inner() {
                    members.push(build_class_member(member)?);
                }
            }
            _ => {}
        }
    }

    Ok(ClassDef {
        annotations,
        modifiers,
        name: name.unwrap(),
        type_params,
        extends,
        members,
        span,
    })
}

fn build_class_member(pair: Pair<Rule>) -> ParseResult<ClassMember> {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::annotated_property => Ok(ClassMember::Property(build_annotated_property(inner)?)),
        Rule::annotated_method => Ok(ClassMember::Method(build_annotated_method(inner)?)),
        _ => unreachable!("unexpected class member: {:?}", inner.as_rule()),
    }
}

fn build_type_alias(pair: Pair<Rule>) -> ParseResult<TypeAlias> {
    debug_assert_eq!(pair.as_rule(), Rule::type_alias_definition);
    let span = span_from_pair(&pair);

    let mut annotations = Vec::new();
    let mut modifiers = TypeAliasModifiers::default();
    let mut name = None;
    let mut type_params = Vec::new();
    let mut ty = None;

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::annotation => {
                annotations.push(build_annotation(inner)?);
            }
            Rule::type_alias_modifier => match inner.as_str() {
                "local" => modifiers.is_local = true,
                "external" => modifiers.is_external = true,
                _ => {}
            },
            Rule::identifier => {
                name = Some(build_identifier(inner));
            }
            Rule::type_parameters => {
                type_params = build_type_parameters(inner)?;
            }
            Rule::type_annotation => {
                ty = Some(build_type_annotation(inner)?);
            }
            _ => {}
        }
    }

    Ok(TypeAlias {
        annotations,
        modifiers,
        name: name.unwrap(),
        type_params,
        ty: ty.unwrap(),
        span,
    })
}

fn build_type_parameters(pair: Pair<Rule>) -> ParseResult<Vec<TypeParameter>> {
    debug_assert_eq!(pair.as_rule(), Rule::type_parameters);

    let mut params = Vec::new();
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::type_parameter {
            params.push(build_type_parameter(inner)?);
        }
    }
    Ok(params)
}

fn build_type_parameter(pair: Pair<Rule>) -> ParseResult<TypeParameter> {
    debug_assert_eq!(pair.as_rule(), Rule::type_parameter);
    let span = span_from_pair(&pair);

    let mut variance = None;
    let mut name = None;

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::variance => {
                variance = Some(match inner.as_str() {
                    "in" => Variance::In,
                    "out" => Variance::Out,
                    _ => unreachable!(),
                });
            }
            Rule::identifier => {
                name = Some(build_identifier(inner));
            }
            _ => {}
        }
    }

    Ok(TypeParameter {
        variance,
        name: name.unwrap(),
        span,
    })
}

// =============================================================================
// Properties and Methods
// =============================================================================

fn build_annotated_property(pair: Pair<Rule>) -> ParseResult<Property> {
    debug_assert_eq!(pair.as_rule(), Rule::annotated_property);
    let span = span_from_pair(&pair);

    let mut annotations = Vec::new();
    let mut prop = None;

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::annotation => {
                annotations.push(build_annotation(inner)?);
            }
            Rule::property => {
                prop = Some(build_property(inner, annotations.clone())?);
            }
            _ => {}
        }
    }

    let mut result = prop.unwrap();
    result.annotations = annotations;
    result.span = span;
    Ok(result)
}

fn build_property(pair: Pair<Rule>, annotations: Vec<Annotation>) -> ParseResult<Property> {
    debug_assert_eq!(pair.as_rule(), Rule::property);
    let span = span_from_pair(&pair);

    let mut modifiers = PropertyModifiers::default();
    let mut name = None;
    let mut ty = None;
    let mut value = None;

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::property_modifier => match inner.as_str() {
                "local" => modifiers.is_local = true,
                "hidden" => modifiers.is_hidden = true,
                "fixed" => modifiers.is_fixed = true,
                "const" => modifiers.is_const = true,
                "external" => modifiers.is_external = true,
                _ => {}
            },
            Rule::identifier => {
                name = Some(build_identifier(inner));
            }
            Rule::type_annotation_clause => {
                ty = Some(build_type_annotation(inner.into_inner().next().unwrap())?);
            }
            Rule::property_value => {
                value = Some(build_property_value(inner)?);
            }
            _ => {}
        }
    }

    Ok(Property {
        annotations,
        modifiers,
        name: name.unwrap(),
        ty,
        value,
        span,
    })
}

fn build_property_value(pair: Pair<Rule>) -> ParseResult<PropertyValue> {
    debug_assert_eq!(pair.as_rule(), Rule::property_value);

    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::expression => Ok(PropertyValue::Expr(build_expression(inner)?)),
        Rule::object_body => Ok(PropertyValue::Object(build_object_body(inner)?)),
        _ => unreachable!("unexpected property value: {:?}", inner.as_rule()),
    }
}

fn build_annotated_method(pair: Pair<Rule>) -> ParseResult<Method> {
    debug_assert_eq!(pair.as_rule(), Rule::annotated_method);
    let span = span_from_pair(&pair);

    let mut annotations = Vec::new();
    let mut method = None;

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::annotation => {
                annotations.push(build_annotation(inner)?);
            }
            Rule::method => {
                method = Some(build_method(inner, annotations.clone())?);
            }
            _ => {}
        }
    }

    let mut result = method.unwrap();
    result.annotations = annotations;
    result.span = span;
    Ok(result)
}

fn build_method(pair: Pair<Rule>, annotations: Vec<Annotation>) -> ParseResult<Method> {
    debug_assert_eq!(pair.as_rule(), Rule::method);
    let span = span_from_pair(&pair);

    let mut modifiers = PropertyModifiers::default();
    let mut name = None;
    let mut params = Vec::new();
    let mut return_type = None;
    let mut body = None;

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::property_modifier => match inner.as_str() {
                "local" => modifiers.is_local = true,
                "hidden" => modifiers.is_hidden = true,
                "fixed" => modifiers.is_fixed = true,
                "const" => modifiers.is_const = true,
                "external" => modifiers.is_external = true,
                _ => {}
            },
            Rule::identifier => {
                name = Some(build_identifier(inner));
            }
            Rule::function_parameters => {
                params = build_function_parameters(inner)?;
            }
            Rule::type_annotation_clause => {
                return_type = Some(build_type_annotation(inner.into_inner().next().unwrap())?);
            }
            Rule::method_body => {
                body = Some(build_expression(inner.into_inner().next().unwrap())?);
            }
            _ => {}
        }
    }

    Ok(Method {
        annotations,
        modifiers,
        name: name.unwrap(),
        params,
        return_type,
        body,
        span,
    })
}

fn build_function_parameters(pair: Pair<Rule>) -> ParseResult<Vec<Parameter>> {
    debug_assert_eq!(pair.as_rule(), Rule::function_parameters);

    let mut params = Vec::new();
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::parameter_list {
            for param in inner.into_inner() {
                if param.as_rule() == Rule::parameter {
                    params.push(build_parameter(param)?);
                }
            }
        }
    }
    Ok(params)
}

fn build_parameter(pair: Pair<Rule>) -> ParseResult<Parameter> {
    debug_assert_eq!(pair.as_rule(), Rule::parameter);
    let span = span_from_pair(&pair);

    let mut name = None;
    let mut ty = None;

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::identifier => {
                name = Some(build_identifier(inner));
            }
            Rule::type_annotation_clause => {
                ty = Some(build_type_annotation(inner.into_inner().next().unwrap())?);
            }
            _ => {}
        }
    }

    Ok(Parameter {
        name: name.unwrap(),
        ty,
        span,
    })
}

// =============================================================================
// Type annotations
// =============================================================================

fn build_type_annotation(pair: Pair<Rule>) -> ParseResult<TypeAnnotation> {
    debug_assert_eq!(pair.as_rule(), Rule::type_annotation);
    let span = span_from_pair(&pair);

    let inner = pair.into_inner().next().unwrap();
    build_type_union(inner, span)
}

fn build_type_union(pair: Pair<Rule>, outer_span: Span) -> ParseResult<TypeAnnotation> {
    debug_assert_eq!(pair.as_rule(), Rule::type_union);

    let mut types: Vec<TypeAnnotation> = Vec::new();

    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::type_nullable {
            types.push(build_type_nullable(inner)?);
        }
    }

    if types.len() == 1 {
        Ok(types.pop().unwrap())
    } else {
        Ok(TypeAnnotation {
            kind: TypeKind::Union(types),
            span: outer_span,
        })
    }
}

fn build_type_nullable(pair: Pair<Rule>) -> ParseResult<TypeAnnotation> {
    debug_assert_eq!(pair.as_rule(), Rule::type_nullable);
    let span = span_from_pair(&pair);

    let text = pair.as_str();
    let is_nullable = text.ends_with('?');

    let inner = pair.into_inner().next().unwrap();
    let base = build_type_primary(inner)?;

    if is_nullable {
        Ok(TypeAnnotation {
            kind: TypeKind::Nullable(Box::new(base)),
            span,
        })
    } else {
        Ok(base)
    }
}

fn build_type_primary(pair: Pair<Rule>) -> ParseResult<TypeAnnotation> {
    debug_assert_eq!(pair.as_rule(), Rule::type_primary);
    let span = span_from_pair(&pair);

    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::function_type => build_function_type(inner),
        Rule::constrained_type => build_constrained_type(inner),
        Rule::parameterized_type => build_parameterized_type(inner),
        Rule::string_literal_type => {
            // string_literal_type = { basic_string }
            let basic_str = inner.into_inner().next().unwrap();
            let lit = build_basic_string(basic_str, span)?;
            let s = lit.as_simple().unwrap_or_default().to_string();
            Ok(TypeAnnotation {
                kind: TypeKind::StringLiteral(s),
                span,
            })
        }
        Rule::nothing_type => Ok(TypeAnnotation {
            kind: TypeKind::Nothing,
            span,
        }),
        Rule::unknown_type => Ok(TypeAnnotation {
            kind: TypeKind::Unknown,
            span,
        }),
        Rule::module_type => Ok(TypeAnnotation {
            kind: TypeKind::Module,
            span,
        }),
        Rule::type_reference => {
            let name = build_qualified_ident(inner.into_inner().next().unwrap())?;
            Ok(TypeAnnotation {
                kind: TypeKind::Named(name),
                span,
            })
        }
        Rule::type_annotation => build_type_annotation(inner),
        _ => unreachable!("unexpected type primary: {:?}", inner.as_rule()),
    }
}

fn build_function_type(pair: Pair<Rule>) -> ParseResult<TypeAnnotation> {
    debug_assert_eq!(pair.as_rule(), Rule::function_type);
    let span = span_from_pair(&pair);

    let mut params = Vec::new();
    let mut return_type = None;
    let mut seen_arrow = false;

    for inner in pair.into_inner() {
        let rule = inner.as_rule();
        let text = inner.as_str();

        if rule == Rule::type_annotation {
            if seen_arrow {
                return_type = Some(build_type_annotation(inner)?);
            } else {
                params.push(build_type_annotation(inner)?);
            }
        } else if text == "->" {
            // Track when we're past the arrow
            seen_arrow = true;
        }
    }

    Ok(TypeAnnotation {
        kind: TypeKind::Function {
            params,
            return_type: Box::new(return_type.unwrap()),
        },
        span,
    })
}

fn build_parameterized_type(pair: Pair<Rule>) -> ParseResult<TypeAnnotation> {
    debug_assert_eq!(pair.as_rule(), Rule::parameterized_type);
    let span = span_from_pair(&pair);

    let mut inner = pair.into_inner();
    // parameterized_type = { qualified_identifier ~ type_args }
    let base = build_qualified_ident(inner.next().unwrap())?;

    let mut args = Vec::new();
    for type_args in inner {
        if type_args.as_rule() == Rule::type_args {
            for arg in type_args.into_inner() {
                if arg.as_rule() == Rule::type_annotation {
                    args.push(build_type_annotation(arg)?);
                }
            }
        }
    }

    Ok(TypeAnnotation {
        kind: TypeKind::Parameterized { base, args },
        span,
    })
}

fn build_constrained_type(pair: Pair<Rule>) -> ParseResult<TypeAnnotation> {
    debug_assert_eq!(pair.as_rule(), Rule::constrained_type);
    let span = span_from_pair(&pair);

    let mut inner = pair.into_inner();
    // constrained_type = { type_reference ~ "(" ~ expression ~ ")" }
    // type_reference = { qualified_identifier }
    let base_pair = inner.next().unwrap();
    let base_name = build_qualified_ident(base_pair.into_inner().next().unwrap())?;
    let base = TypeAnnotation {
        kind: TypeKind::Named(base_name),
        span,
    };

    let constraint = build_expression(inner.next().unwrap())?;

    Ok(TypeAnnotation {
        kind: TypeKind::Constrained {
            base: Box::new(base),
            constraint: Box::new(constraint),
        },
        span,
    })
}

// =============================================================================
// Expressions
// =============================================================================

fn build_expression(pair: Pair<Rule>) -> ParseResult<Expr> {
    debug_assert_eq!(pair.as_rule(), Rule::expression);

    let mut inner = pair.into_inner();
    let first = inner.next().unwrap();
    let left = build_prefix_expr(first)?;

    // Collect all operators and operands
    let mut ops_and_exprs: Vec<(BinaryOp, Expr)> = Vec::new();
    while let Some(op_pair) = inner.next() {
        let op = parse_binary_op(&op_pair)?;
        let right_pair = inner.next().unwrap();
        let right = build_prefix_expr(right_pair)?;
        ops_and_exprs.push((op, right));
    }

    if ops_and_exprs.is_empty() {
        return Ok(left);
    }

    // Build the expression tree with proper precedence using a Pratt-style approach
    build_expr_with_precedence(left, &ops_and_exprs, 0)
}

/// Build expression tree with proper precedence using a modified Pratt parser
fn build_expr_with_precedence(
    left: Expr,
    ops_and_exprs: &[(BinaryOp, Expr)],
    start_idx: usize,
) -> ParseResult<Expr> {
    if start_idx >= ops_and_exprs.len() {
        return Ok(left);
    }

    // Convert flat list to tree respecting precedence
    let mut exprs: Vec<Expr> = vec![left];
    let mut ops: Vec<BinaryOp> = vec![];

    for (op, expr) in ops_and_exprs.iter().skip(start_idx) {
        ops.push(*op);
        exprs.push(expr.clone());
    }

    // Process operators by precedence (highest first)
    for precedence in (0..=7).rev() {
        let mut i = 0;
        while i < ops.len() {
            let op = ops[i];
            if op.precedence() == precedence {
                let left_expr = exprs.remove(i);
                let right_expr = exprs.remove(i);
                let span = left_expr.span;

                let combined = Expr {
                    kind: ExprKind::Binary {
                        op,
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                    },
                    span,
                };
                exprs.insert(i, combined);
                ops.remove(i);
            } else {
                i += 1;
            }
        }
    }

    debug_assert_eq!(exprs.len(), 1);
    debug_assert!(ops.is_empty());

    Ok(exprs.pop().unwrap())
}

fn parse_binary_op(pair: &Pair<Rule>) -> ParseResult<BinaryOp> {
    // The binary_op rule is silent and passes through sub-rules like additive_op
    // These sub-rules directly match the operator text, so we use as_str()
    let op_str = pair.as_str();
    let op = match op_str {
        "+" => BinaryOp::Add,
        "-" => BinaryOp::Sub,
        "*" => BinaryOp::Mul,
        "/" => BinaryOp::Div,
        "~/" => BinaryOp::IntDiv,
        "%" => BinaryOp::Mod,
        "**" => BinaryOp::Pow,
        "==" => BinaryOp::Eq,
        "!=" => BinaryOp::Ne,
        "<" => BinaryOp::Lt,
        "<=" => BinaryOp::Le,
        ">" => BinaryOp::Gt,
        ">=" => BinaryOp::Ge,
        "&&" => BinaryOp::And,
        "||" => BinaryOp::Or,
        "??" => BinaryOp::NullCoalesce,
        "|>" => BinaryOp::Pipe,
        "is" => BinaryOp::Is,
        "as" => BinaryOp::As,
        _ => return Err(ParseError::UnknownOperator(op_str.to_string())),
    };
    Ok(op)
}

fn build_prefix_expr(pair: Pair<Rule>) -> ParseResult<Expr> {
    debug_assert_eq!(pair.as_rule(), Rule::prefix_expr);
    let span = span_from_pair(&pair);

    let mut inner = pair.into_inner().peekable();
    let mut ops = Vec::new();

    // Collect prefix operators
    while inner.peek().map(|p| p.as_rule()) == Some(Rule::prefix_op) {
        ops.push(inner.next().unwrap());
    }

    // Build the postfix expression
    let postfix = inner.next().unwrap();
    let mut expr = build_postfix_expr(postfix)?;

    // Apply prefix operators in reverse order
    for op_pair in ops.into_iter().rev() {
        let op = match op_pair.as_str() {
            "!" => UnaryOp::Not,
            "-" => UnaryOp::Neg,
            _ => unreachable!(),
        };
        expr = Expr {
            kind: ExprKind::Unary {
                op,
                operand: Box::new(expr),
            },
            span,
        };
    }

    Ok(expr)
}

fn build_postfix_expr(pair: Pair<Rule>) -> ParseResult<Expr> {
    debug_assert_eq!(pair.as_rule(), Rule::postfix_expr);

    let mut inner = pair.into_inner();
    let primary = inner.next().unwrap();
    let mut expr = build_primary_expr(primary)?;

    // Apply postfix suffixes
    for suffix in inner {
        expr = apply_postfix_suffix(expr, suffix)?;
    }

    Ok(expr)
}

fn apply_postfix_suffix(base: Expr, suffix: Pair<Rule>) -> ParseResult<Expr> {
    debug_assert_eq!(suffix.as_rule(), Rule::postfix_suffix);
    let span = base.span.merge(span_from_pair(&suffix));

    let inner = suffix.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::member_access_suffix => {
            let ident = build_identifier(inner.into_inner().next().unwrap());
            Ok(Expr {
                kind: ExprKind::MemberAccess {
                    base: Box::new(base),
                    member: ident,
                },
                span,
            })
        }
        Rule::optional_member_access_suffix => {
            let ident = build_identifier(inner.into_inner().next().unwrap());
            Ok(Expr {
                kind: ExprKind::OptionalMemberAccess {
                    base: Box::new(base),
                    member: ident,
                },
                span,
            })
        }
        Rule::method_call_suffix => {
            let mut parts = inner.into_inner();
            let ident = build_identifier(parts.next().unwrap());
            let args = build_call_args(parts.next().unwrap())?;
            Ok(Expr {
                kind: ExprKind::Call {
                    callee: Box::new(Expr {
                        kind: ExprKind::MemberAccess {
                            base: Box::new(base),
                            member: ident,
                        },
                        span,
                    }),
                    args,
                },
                span,
            })
        }
        Rule::subscript_suffix => {
            let index = build_expression(inner.into_inner().next().unwrap())?;
            Ok(Expr {
                kind: ExprKind::Subscript {
                    base: Box::new(base),
                    index: Box::new(index),
                },
                span,
            })
        }
        Rule::non_null_suffix => Ok(Expr {
            kind: ExprKind::NonNullAssertion(Box::new(base)),
            span,
        }),
        Rule::call_suffix => {
            let args = build_call_args(inner.into_inner().next().unwrap())?;
            Ok(Expr {
                kind: ExprKind::Call {
                    callee: Box::new(base),
                    args,
                },
                span,
            })
        }
        Rule::amend_suffix => {
            let body = build_object_body(inner.into_inner().next().unwrap())?;
            Ok(Expr {
                kind: ExprKind::Amend {
                    base: Box::new(base),
                    body,
                },
                span,
            })
        }
        Rule::is_suffix => {
            // is_suffix = { is_keyword ~ type_annotation }
            let mut parts = inner.into_inner();
            let _ = parts.next(); // skip is_keyword
            let ty = build_type_annotation(parts.next().unwrap())?;
            Ok(Expr {
                kind: ExprKind::Is {
                    value: Box::new(base),
                    ty,
                },
                span,
            })
        }
        Rule::as_suffix => {
            // as_suffix = { as_keyword ~ type_annotation }
            let mut parts = inner.into_inner();
            let _ = parts.next(); // skip as_keyword
            let ty = build_type_annotation(parts.next().unwrap())?;
            Ok(Expr {
                kind: ExprKind::As {
                    value: Box::new(base),
                    ty,
                },
                span,
            })
        }
        _ => unreachable!("unexpected postfix suffix: {:?}", inner.as_rule()),
    }
}

fn build_call_args(pair: Pair<Rule>) -> ParseResult<Vec<Expr>> {
    debug_assert_eq!(pair.as_rule(), Rule::call_args);

    let mut args = Vec::new();
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::arg_list {
            for arg in inner.into_inner() {
                if arg.as_rule() == Rule::expression {
                    args.push(build_expression(arg)?);
                }
            }
        }
    }
    Ok(args)
}

fn build_primary_expr(pair: Pair<Rule>) -> ParseResult<Expr> {
    debug_assert_eq!(pair.as_rule(), Rule::primary_expr);
    let span = span_from_pair(&pair);

    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::if_expr => build_if_expr(inner),
        Rule::let_expr => build_let_expr(inner),
        Rule::throw_expr => {
            let expr = build_expression(inner.into_inner().next().unwrap())?;
            Ok(Expr {
                kind: ExprKind::Throw(Box::new(expr)),
                span,
            })
        }
        Rule::trace_expr => {
            let expr = build_expression(inner.into_inner().next().unwrap())?;
            Ok(Expr {
                kind: ExprKind::Trace(Box::new(expr)),
                span,
            })
        }
        Rule::read_expr => {
            let text = inner.as_str();
            let is_nullable = text.starts_with("read?");
            let uri = build_expression(inner.into_inner().next().unwrap())?;
            Ok(Expr {
                kind: ExprKind::Read {
                    uri: Box::new(uri),
                    is_nullable,
                },
                span,
            })
        }
        Rule::read_glob_expr => {
            let uri = build_expression(inner.into_inner().next().unwrap())?;
            Ok(Expr {
                kind: ExprKind::ReadGlob { uri: Box::new(uri) },
                span,
            })
        }
        Rule::new_expr => build_new_expr(inner),
        Rule::lambda_expr => build_lambda_expr(inner),
        Rule::parenthesized_or_amended => build_parenthesized_or_amended(inner),
        Rule::this_expr => Ok(Expr {
            kind: ExprKind::This,
            span,
        }),
        Rule::super_expr => Ok(Expr {
            kind: ExprKind::Super,
            span,
        }),
        Rule::outer_expr => Ok(Expr {
            kind: ExprKind::Outer,
            span,
        }),
        Rule::module_expr => Ok(Expr {
            kind: ExprKind::Module,
            span,
        }),
        Rule::null_literal => Ok(Expr {
            kind: ExprKind::Null,
            span,
        }),
        Rule::bool_literal => {
            let value = inner.as_str() == "true";
            Ok(Expr {
                kind: ExprKind::Bool(value),
                span,
            })
        }
        Rule::float_literal => {
            let value: f64 = inner
                .as_str()
                .parse()
                .map_err(|_| ParseError::InvalidNumber(inner.as_str().to_string()))?;
            Ok(Expr {
                kind: ExprKind::Float(value),
                span,
            })
        }
        Rule::int_literal => {
            let text = inner.as_str();
            let value = parse_int_literal(text)?;
            Ok(Expr {
                kind: ExprKind::Int(value),
                span,
            })
        }
        Rule::string_literal => {
            let lit = build_string_literal(inner)?;
            Ok(Expr {
                kind: ExprKind::String(lit),
                span,
            })
        }
        Rule::identifier => {
            let name = inner.as_str().to_string();
            Ok(Expr {
                kind: ExprKind::Identifier(name),
                span,
            })
        }
        _ => unreachable!("unexpected primary expr: {:?}", inner.as_rule()),
    }
}

fn build_if_expr(pair: Pair<Rule>) -> ParseResult<Expr> {
    debug_assert_eq!(pair.as_rule(), Rule::if_expr);
    let span = span_from_pair(&pair);

    let mut inner = pair.into_inner();
    let condition = build_expression(inner.next().unwrap())?;
    let then_branch = build_expression(inner.next().unwrap())?;
    let else_branch = build_expression(inner.next().unwrap())?;

    Ok(Expr {
        kind: ExprKind::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        },
        span,
    })
}

fn build_let_expr(pair: Pair<Rule>) -> ParseResult<Expr> {
    debug_assert_eq!(pair.as_rule(), Rule::let_expr);
    let span = span_from_pair(&pair);

    let mut inner = pair.into_inner();
    let name = build_identifier(inner.next().unwrap());

    let mut ty = None;
    let mut value = None;
    let mut body = None;

    for part in inner {
        match part.as_rule() {
            Rule::type_annotation_clause => {
                ty = Some(build_type_annotation(part.into_inner().next().unwrap())?);
            }
            Rule::expression => {
                if value.is_none() {
                    value = Some(build_expression(part)?);
                } else {
                    body = Some(build_expression(part)?);
                }
            }
            _ => {}
        }
    }

    Ok(Expr {
        kind: ExprKind::Let {
            name,
            ty,
            value: Box::new(value.unwrap()),
            body: Box::new(body.unwrap()),
        },
        span,
    })
}

fn build_new_expr(pair: Pair<Rule>) -> ParseResult<Expr> {
    debug_assert_eq!(pair.as_rule(), Rule::new_expr);
    let span = span_from_pair(&pair);

    let mut class_ref = None;
    let mut body = None;

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::type_reference => {
                class_ref = Some(build_qualified_ident(inner.into_inner().next().unwrap())?);
            }
            Rule::object_body => {
                body = Some(build_object_body(inner)?);
            }
            _ => {}
        }
    }

    Ok(Expr {
        kind: ExprKind::New {
            class_ref,
            body: body.unwrap(),
        },
        span,
    })
}

fn build_lambda_expr(pair: Pair<Rule>) -> ParseResult<Expr> {
    debug_assert_eq!(pair.as_rule(), Rule::lambda_expr);
    let span = span_from_pair(&pair);

    let mut inner = pair.into_inner();
    let params_pair = inner.next().unwrap();
    let body = build_expression(inner.next().unwrap())?;

    let mut params = Vec::new();
    for part in params_pair.into_inner() {
        if part.as_rule() == Rule::lambda_param_list {
            for param in part.into_inner() {
                if param.as_rule() == Rule::lambda_param {
                    params.push(build_lambda_param(param)?);
                }
            }
        }
    }

    Ok(Expr {
        kind: ExprKind::Lambda {
            params,
            body: Box::new(body),
        },
        span,
    })
}

fn build_lambda_param(pair: Pair<Rule>) -> ParseResult<Parameter> {
    debug_assert_eq!(pair.as_rule(), Rule::lambda_param);
    let span = span_from_pair(&pair);

    let mut name = None;
    let mut ty = None;

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::identifier => {
                name = Some(build_identifier(inner));
            }
            Rule::type_annotation_clause => {
                ty = Some(build_type_annotation(inner.into_inner().next().unwrap())?);
            }
            _ => {}
        }
    }

    Ok(Parameter {
        name: name.unwrap(),
        ty,
        span,
    })
}

fn build_parenthesized_or_amended(pair: Pair<Rule>) -> ParseResult<Expr> {
    debug_assert_eq!(pair.as_rule(), Rule::parenthesized_or_amended);
    let span = span_from_pair(&pair);

    let mut inner = pair.into_inner();
    let expr = build_expression(inner.next().unwrap())?;

    if let Some(body_pair) = inner.next() {
        // This is an amendment
        let body = build_object_body(body_pair)?;
        Ok(Expr {
            kind: ExprKind::Amend {
                base: Box::new(expr),
                body,
            },
            span,
        })
    } else {
        // Just parenthesized
        Ok(Expr {
            kind: ExprKind::Parenthesized(Box::new(expr)),
            span,
        })
    }
}

// =============================================================================
// Object body
// =============================================================================

fn build_object_body(pair: Pair<Rule>) -> ParseResult<ObjectBody> {
    debug_assert_eq!(pair.as_rule(), Rule::object_body);
    let span = span_from_pair(&pair);

    let mut members = Vec::new();
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::object_member {
            members.push(build_object_member(inner)?);
        }
    }

    Ok(ObjectBody { members, span })
}

fn build_object_member(pair: Pair<Rule>) -> ParseResult<ObjectMember> {
    debug_assert_eq!(pair.as_rule(), Rule::object_member);
    let span = span_from_pair(&pair);

    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::for_generator => build_for_generator(inner),
        Rule::when_generator => build_when_generator(inner),
        Rule::spread_member => build_spread_member(inner),
        Rule::entry_member => build_entry_member(inner),
        Rule::property_member => build_property_member(inner),
        Rule::element_member => {
            let expr = build_expression(inner.into_inner().next().unwrap())?;
            Ok(ObjectMember::Element { value: expr, span })
        }
        _ => unreachable!("unexpected object member: {:?}", inner.as_rule()),
    }
}

fn build_property_member(pair: Pair<Rule>) -> ParseResult<ObjectMember> {
    debug_assert_eq!(pair.as_rule(), Rule::property_member);
    let span = span_from_pair(&pair);

    let mut inner = pair.into_inner();
    let name = build_identifier(inner.next().unwrap());

    let next = inner.next().unwrap();
    match next.as_rule() {
        Rule::expression => {
            let value = build_expression(next)?;
            Ok(ObjectMember::Property { name, value, span })
        }
        Rule::object_body => {
            let body = build_object_body(next)?;
            Ok(ObjectMember::PropertyAmend { name, body, span })
        }
        _ => unreachable!(),
    }
}

fn build_entry_member(pair: Pair<Rule>) -> ParseResult<ObjectMember> {
    debug_assert_eq!(pair.as_rule(), Rule::entry_member);
    let span = span_from_pair(&pair);

    let mut inner = pair.into_inner();
    let key = build_expression(inner.next().unwrap())?;

    let next = inner.next().unwrap();
    match next.as_rule() {
        Rule::expression => {
            let value = build_expression(next)?;
            Ok(ObjectMember::Entry { key, value, span })
        }
        Rule::object_body => {
            let body = build_object_body(next)?;
            Ok(ObjectMember::EntryAmend { key, body, span })
        }
        _ => unreachable!(),
    }
}

fn build_spread_member(pair: Pair<Rule>) -> ParseResult<ObjectMember> {
    debug_assert_eq!(pair.as_rule(), Rule::spread_member);
    let span = span_from_pair(&pair);
    let text = pair.as_str();
    let is_nullable = text.contains("...?");

    let expr = build_expression(pair.into_inner().next().unwrap())?;
    Ok(ObjectMember::Spread {
        is_nullable,
        value: expr,
        span,
    })
}

fn build_for_generator(pair: Pair<Rule>) -> ParseResult<ObjectMember> {
    debug_assert_eq!(pair.as_rule(), Rule::for_generator);
    let span = span_from_pair(&pair);

    let mut inner = pair.into_inner();
    let binding = inner.next().unwrap();
    let iterable = build_expression(inner.next().unwrap())?;
    let body = build_object_body(inner.next().unwrap())?;

    // Parse for_binding
    let mut binding_inner = binding.into_inner();
    let first = build_identifier(binding_inner.next().unwrap());
    let second = binding_inner.next().map(build_identifier);

    let (key_var, value_var) = if let Some(val) = second {
        (Some(first), val)
    } else {
        (None, first)
    };

    Ok(ObjectMember::For {
        key_var,
        value_var,
        iterable,
        body,
        span,
    })
}

fn build_when_generator(pair: Pair<Rule>) -> ParseResult<ObjectMember> {
    debug_assert_eq!(pair.as_rule(), Rule::when_generator);
    let span = span_from_pair(&pair);

    let mut inner = pair.into_inner();
    let condition = build_expression(inner.next().unwrap())?;
    let body = build_object_body(inner.next().unwrap())?;
    let else_body = inner
        .next()
        .map(|p| build_object_body(p.into_inner().next().unwrap()))
        .transpose()?;

    Ok(ObjectMember::When {
        condition,
        body,
        else_body,
        span,
    })
}

// =============================================================================
// String literals
// =============================================================================

fn build_string_literal(pair: Pair<Rule>) -> ParseResult<StringLiteral> {
    debug_assert_eq!(pair.as_rule(), Rule::string_literal);
    let span = span_from_pair(&pair);

    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::basic_string => build_basic_string(inner, span),
        Rule::multiline_string => build_multiline_string(inner, span),
        _ => unreachable!("unexpected string type: {:?}", inner.as_rule()),
    }
}

fn build_basic_string(pair: Pair<Rule>, span: Span) -> ParseResult<StringLiteral> {
    debug_assert_eq!(pair.as_rule(), Rule::basic_string);

    let mut parts = Vec::new();
    let mut current_literal = String::new();

    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::basic_string_inner {
            for part in inner.into_inner() {
                if part.as_rule() == Rule::basic_string_part {
                    // Iterate over all children (there can be multiple basic_string_char)
                    for sub in part.into_inner() {
                        match sub.as_rule() {
                            Rule::interpolation => {
                                // Flush current literal
                                if !current_literal.is_empty() {
                                    parts.push(StringPart::Literal(std::mem::take(
                                        &mut current_literal,
                                    )));
                                }
                                let expr = build_expression(sub.into_inner().next().unwrap())?;
                                parts.push(StringPart::Interpolation(expr));
                            }
                            Rule::escape_sequence => {
                                current_literal.push_str(&unescape(sub.as_str())?);
                            }
                            Rule::basic_string_char => {
                                current_literal.push_str(sub.as_str());
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
    }

    // Flush remaining literal
    if !current_literal.is_empty() || parts.is_empty() {
        parts.push(StringPart::Literal(current_literal));
    }

    Ok(StringLiteral { parts, span })
}

fn build_multiline_string(pair: Pair<Rule>, span: Span) -> ParseResult<StringLiteral> {
    debug_assert_eq!(pair.as_rule(), Rule::multiline_string);

    let mut parts = Vec::new();
    let mut current_literal = String::new();

    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::multiline_string_inner {
            for part in inner.into_inner() {
                if part.as_rule() == Rule::multiline_string_part {
                    // Iterate over all children (there can be multiple multiline_string_char)
                    for sub in part.into_inner() {
                        match sub.as_rule() {
                            Rule::interpolation => {
                                if !current_literal.is_empty() {
                                    parts.push(StringPart::Literal(std::mem::take(
                                        &mut current_literal,
                                    )));
                                }
                                let expr = build_expression(sub.into_inner().next().unwrap())?;
                                parts.push(StringPart::Interpolation(expr));
                            }
                            Rule::escape_sequence => {
                                current_literal.push_str(&unescape(sub.as_str())?);
                            }
                            Rule::multiline_string_char => {
                                current_literal.push_str(sub.as_str());
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
    }

    if !current_literal.is_empty() || parts.is_empty() {
        parts.push(StringPart::Literal(current_literal));
    }

    Ok(StringLiteral { parts, span })
}

fn unescape(s: &str) -> ParseResult<String> {
    let mut result = String::new();
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some('(') => result.push('('),
                Some('0') => result.push('\0'),
                Some('u') => {
                    // Skip the opening brace
                    if chars.next() != Some('{') {
                        return Err(ParseError::InvalidEscape(s.to_string()));
                    }
                    let mut hex = String::new();
                    while let Some(&c) = chars.peek() {
                        if c == '}' {
                            chars.next();
                            break;
                        }
                        hex.push(chars.next().unwrap());
                    }
                    let code = u32::from_str_radix(&hex, 16)
                        .map_err(|_| ParseError::InvalidEscape(s.to_string()))?;
                    let ch = char::from_u32(code)
                        .ok_or_else(|| ParseError::InvalidEscape(s.to_string()))?;
                    result.push(ch);
                }
                _ => return Err(ParseError::InvalidEscape(s.to_string())),
            }
        } else {
            result.push(c);
        }
    }

    Ok(result)
}

// =============================================================================
// Helpers
// =============================================================================

fn build_identifier(pair: Pair<Rule>) -> Identifier {
    debug_assert_eq!(pair.as_rule(), Rule::identifier);
    let raw = pair.as_str();
    // Strip backticks from backtick identifiers
    let name = if raw.starts_with('`') && raw.ends_with('`') {
        raw[1..raw.len() - 1].to_string()
    } else {
        raw.to_string()
    };
    Spanned::new(name, span_from_pair(&pair))
}

fn build_qualified_ident(pair: Pair<Rule>) -> ParseResult<QualifiedIdent> {
    debug_assert_eq!(pair.as_rule(), Rule::qualified_identifier);

    let parts: Vec<Identifier> = pair
        .into_inner()
        .filter(|p| p.as_rule() == Rule::identifier)
        .map(build_identifier)
        .collect();

    Ok(QualifiedIdent { parts })
}

fn parse_int_literal(text: &str) -> ParseResult<i64> {
    let text = text.replace('_', "");

    if text.starts_with("0x") || text.starts_with("0X") {
        i64::from_str_radix(&text[2..], 16).map_err(|_| ParseError::InvalidNumber(text.to_string()))
    } else if text.starts_with("0b") || text.starts_with("0B") {
        i64::from_str_radix(&text[2..], 2).map_err(|_| ParseError::InvalidNumber(text.to_string()))
    } else if text.starts_with("0o") || text.starts_with("0O") {
        i64::from_str_radix(&text[2..], 8).map_err(|_| ParseError::InvalidNumber(text.to_string()))
    } else {
        text.parse::<i64>()
            .map_err(|_| ParseError::InvalidNumber(text.to_string()))
    }
}
