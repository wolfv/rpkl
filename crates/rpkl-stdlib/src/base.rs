//! Base type implementations

use std::sync::Arc;

use rpkl_runtime::{EvalError, EvalResult, ExternalRegistry, VmValue};

pub fn register(registry: &mut ExternalRegistry) {
    // Boolean methods
    registry.register_method("Boolean", "xor", Arc::new(bool_xor));
    registry.register_method("Boolean", "implies", Arc::new(bool_implies));

    // Any type methods
    registry.register_method("Any", "toString", Arc::new(any_to_string));
    registry.register_method("Null", "toString", Arc::new(any_to_string));
    registry.register_method("Boolean", "toString", Arc::new(any_to_string));
    registry.register_method("Int", "toString", Arc::new(any_to_string));
    registry.register_method("Float", "toString", Arc::new(any_to_string));
    registry.register_method("String", "toString", Arc::new(any_to_string));

    // Int methods
    registry.register_method("Int", "toInt", Arc::new(int_to_int));
    registry.register_method("Int", "toFloat", Arc::new(int_to_float));
    registry.register_method("Float", "toInt", Arc::new(float_to_int));
    registry.register_method("Float", "toFloat", Arc::new(float_to_float));
    registry.register_method("Int", "isBetween", Arc::new(int_is_between));
    registry.register_method("Float", "isBetween", Arc::new(float_is_between));

    // Number comparisons
    registry.register_method("Int", "compareTo", Arc::new(int_compare_to));
    registry.register_method("Float", "compareTo", Arc::new(float_compare_to));

    // Pair properties (accessed without parentheses)
    registry.register_property("Pair", "first", Arc::new(pair_first));
    registry.register_property("Pair", "second", Arc::new(pair_second));
    registry.register_property("Pair", "key", Arc::new(pair_first)); // Alias for first
    registry.register_property("Pair", "value", Arc::new(pair_second)); // Alias for second
}

fn bool_xor(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().and_then(|v| v.as_bool()).ok_or_else(|| {
        EvalError::type_error("Boolean", args.first().map_or("none", |v| v.type_name()))
    })?;
    let other = args.get(1).and_then(|v| v.as_bool()).ok_or_else(|| {
        EvalError::type_error("Boolean", args.get(1).map_or("none", |v| v.type_name()))
    })?;
    Ok(VmValue::Boolean(this ^ other))
}

fn bool_implies(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().and_then(|v| v.as_bool()).ok_or_else(|| {
        EvalError::type_error("Boolean", args.first().map_or("none", |v| v.type_name()))
    })?;
    let other = args.get(1).and_then(|v| v.as_bool()).ok_or_else(|| {
        EvalError::type_error("Boolean", args.get(1).map_or("none", |v| v.type_name()))
    })?;
    // a implies b == !a || b
    Ok(VmValue::Boolean(!this || other))
}

fn any_to_string(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().ok_or(EvalError::WrongArgCount {
        expected: 1,
        actual: 0,
    })?;
    match this {
        VmValue::String(s) => Ok(VmValue::String(Arc::clone(s))),
        _ => Ok(VmValue::string(this.to_string())),
    }
}

fn int_to_int(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.first().map_or("none", |v| v.type_name()))
    })?;
    Ok(VmValue::Int(this))
}

fn int_to_float(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.first().map_or("none", |v| v.type_name()))
    })?;
    Ok(VmValue::Float(this as f64))
}

fn float_to_int(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().and_then(|v| v.as_float()).ok_or_else(|| {
        EvalError::type_error("Float", args.first().map_or("none", |v| v.type_name()))
    })?;
    Ok(VmValue::Int(this as i64))
}

fn float_to_float(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().and_then(|v| v.as_float()).ok_or_else(|| {
        EvalError::type_error("Float", args.first().map_or("none", |v| v.type_name()))
    })?;
    Ok(VmValue::Float(this))
}

fn int_is_between(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.first().map_or("none", |v| v.type_name()))
    })?;
    let min = args.get(1).and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.get(1).map_or("none", |v| v.type_name()))
    })?;
    let max = args.get(2).and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.get(2).map_or("none", |v| v.type_name()))
    })?;
    Ok(VmValue::Boolean(this >= min && this <= max))
}

fn float_is_between(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().and_then(|v| v.as_float()).ok_or_else(|| {
        EvalError::type_error("Float", args.first().map_or("none", |v| v.type_name()))
    })?;
    let min = args.get(1).and_then(|v| v.as_float()).ok_or_else(|| {
        EvalError::type_error("Float", args.get(1).map_or("none", |v| v.type_name()))
    })?;
    let max = args.get(2).and_then(|v| v.as_float()).ok_or_else(|| {
        EvalError::type_error("Float", args.get(2).map_or("none", |v| v.type_name()))
    })?;
    Ok(VmValue::Boolean(this >= min && this <= max))
}

fn int_compare_to(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.first().map_or("none", |v| v.type_name()))
    })?;
    let other = args.get(1).and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.get(1).map_or("none", |v| v.type_name()))
    })?;
    Ok(VmValue::Int(this.cmp(&other) as i64))
}

fn float_compare_to(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().and_then(|v| v.as_float()).ok_or_else(|| {
        EvalError::type_error("Float", args.first().map_or("none", |v| v.type_name()))
    })?;
    let other = args.get(1).and_then(|v| v.as_float()).ok_or_else(|| {
        EvalError::type_error("Float", args.get(1).map_or("none", |v| v.type_name()))
    })?;
    let result = if this < other {
        -1
    } else if this > other {
        1
    } else {
        0
    };
    Ok(VmValue::Int(result))
}

fn get_pair_arg(args: &[VmValue], idx: usize) -> EvalResult<Arc<(VmValue, VmValue)>> {
    args.get(idx)
        .and_then(|v| {
            if let VmValue::Pair(p) = v {
                Some(Arc::clone(p))
            } else {
                None
            }
        })
        .ok_or_else(|| {
            EvalError::type_error("Pair", args.get(idx).map_or("none", |v| v.type_name()))
        })
}

fn pair_first(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let pair = get_pair_arg(args, 0)?;
    Ok(pair.0.clone())
}

fn pair_second(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let pair = get_pair_arg(args, 0)?;
    Ok(pair.1.clone())
}
