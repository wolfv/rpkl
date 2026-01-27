//! Base type implementations

use std::sync::Arc;

use rpkl_runtime::{
    value::{DataSizeUnit, DurationUnit},
    EvalError, EvalResult, ExternalRegistry, ObjectMember, VmObject, VmValue,
};

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
    registry.register_method("Duration", "toString", Arc::new(any_to_string));
    registry.register_method("DataSize", "toString", Arc::new(any_to_string));

    // Int methods
    registry.register_method("Int", "toInt", Arc::new(int_to_int));
    registry.register_method("Int", "toFloat", Arc::new(int_to_float));
    registry.register_method("Int", "toRadixString", Arc::new(int_to_radix_string));
    registry.register_method("Float", "toInt", Arc::new(float_to_int));
    registry.register_method("Float", "toFloat", Arc::new(float_to_float));
    registry.register_method("Int", "isBetween", Arc::new(int_is_between));
    registry.register_method("Float", "isBetween", Arc::new(float_is_between));

    // Number comparisons
    registry.register_method("Int", "compareTo", Arc::new(int_compare_to));
    registry.register_method("Float", "compareTo", Arc::new(float_compare_to));

    // Int bitwise operations
    registry.register_method("Int", "shl", Arc::new(int_shl));
    registry.register_method("Int", "shr", Arc::new(int_shr));
    registry.register_method("Int", "ushr", Arc::new(int_ushr));
    registry.register_method("Int", "and", Arc::new(int_and));
    registry.register_method("Int", "or", Arc::new(int_or));
    registry.register_method("Int", "xor", Arc::new(int_xor));

    // Pair properties (accessed without parentheses)
    registry.register_property("Pair", "first", Arc::new(pair_first));
    registry.register_property("Pair", "second", Arc::new(pair_second));
    registry.register_property("Pair", "key", Arc::new(pair_first)); // Alias for first
    registry.register_property("Pair", "value", Arc::new(pair_second)); // Alias for second

    // Duration methods
    registry.register_method("Duration", "toUnit", Arc::new(duration_to_unit));
    registry.register_method("Duration", "isBetween", Arc::new(duration_is_between));

    // DataSize methods
    registry.register_method("DataSize", "toUnit", Arc::new(datasize_to_unit));
    registry.register_method("DataSize", "isBetween", Arc::new(datasize_is_between));

    // Object type conversion methods
    registry.register_method("Dynamic", "toDynamic", Arc::new(to_dynamic));
    registry.register_method("Listing", "toDynamic", Arc::new(to_dynamic));
    registry.register_method("Mapping", "toDynamic", Arc::new(to_dynamic));
    registry.register_method("Object", "toDynamic", Arc::new(to_dynamic));
    registry.register_method("Dynamic", "toTyped", Arc::new(to_typed));
    registry.register_method("Object", "toTyped", Arc::new(to_typed));
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

fn int_to_radix_string(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.first().map_or("none", |v| v.type_name()))
    })?;
    let radix = args.get(1).and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.get(1).map_or("none", |v| v.type_name()))
    })?;

    if radix < 2 || radix > 36 {
        return Err(EvalError::InvalidOperation(format!(
            "radix must be between 2 and 36, got {}",
            radix
        )));
    }

    // Convert integer to string in given radix
    let result = if this < 0 {
        format!("-{}", to_radix_string((-this) as u64, radix as u32))
    } else {
        to_radix_string(this as u64, radix as u32)
    };

    Ok(VmValue::String(Arc::from(result)))
}

// Helper function to convert a number to a string in a given radix
fn to_radix_string(mut n: u64, radix: u32) -> String {
    if n == 0 {
        return "0".to_string();
    }
    const DIGITS: &[u8] = b"0123456789abcdefghijklmnopqrstuvwxyz";
    let mut result = Vec::new();
    while n > 0 {
        result.push(DIGITS[(n % radix as u64) as usize] as char);
        n /= radix as u64;
    }
    result.into_iter().rev().collect()
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

// Bitwise operations for Int

fn int_shl(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.first().map_or("none", |v| v.type_name()))
    })?;
    let n = args.get(1).and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.get(1).map_or("none", |v| v.type_name()))
    })?;
    Ok(VmValue::Int(this << n))
}

fn int_shr(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.first().map_or("none", |v| v.type_name()))
    })?;
    let n = args.get(1).and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.get(1).map_or("none", |v| v.type_name()))
    })?;
    // Arithmetic right shift (preserves sign bit)
    Ok(VmValue::Int(this >> n))
}

fn int_ushr(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.first().map_or("none", |v| v.type_name()))
    })?;
    let n = args.get(1).and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.get(1).map_or("none", |v| v.type_name()))
    })?;
    // Logical right shift (zero-fills from left) - cast to u64 for unsigned shift
    Ok(VmValue::Int(((this as u64) >> n) as i64))
}

fn int_and(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.first().map_or("none", |v| v.type_name()))
    })?;
    let n = args.get(1).and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.get(1).map_or("none", |v| v.type_name()))
    })?;
    Ok(VmValue::Int(this & n))
}

fn int_or(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.first().map_or("none", |v| v.type_name()))
    })?;
    let n = args.get(1).and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.get(1).map_or("none", |v| v.type_name()))
    })?;
    Ok(VmValue::Int(this | n))
}

fn int_xor(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = args.first().and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.first().map_or("none", |v| v.type_name()))
    })?;
    let n = args.get(1).and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.get(1).map_or("none", |v| v.type_name()))
    })?;
    Ok(VmValue::Int(this ^ n))
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

// Duration methods

fn duration_to_unit(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let (value, unit) = match &args[0] {
        VmValue::Duration { value, unit } => (*value, *unit),
        _ => {
            return Err(EvalError::type_error(
                "Duration",
                args.first().map_or("none", |v| v.type_name()),
            ))
        }
    };

    let target_unit_str = args.get(1).and_then(|v| v.as_string()).ok_or_else(|| {
        EvalError::type_error("String", args.get(1).map_or("none", |v| v.type_name()))
    })?;

    let target_unit = DurationUnit::from_suffix(target_unit_str).ok_or_else(|| {
        EvalError::InvalidOperation(format!("Unknown duration unit: {}", target_unit_str))
    })?;

    // Convert to nanoseconds, then to target unit
    let nanos = value * unit.to_nanos_factor();
    let converted = nanos / target_unit.to_nanos_factor();

    Ok(VmValue::Duration {
        value: converted,
        unit: target_unit,
    })
}

fn duration_is_between(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let (value, unit) = match &args[0] {
        VmValue::Duration { value, unit } => (*value, *unit),
        _ => {
            return Err(EvalError::type_error(
                "Duration",
                args.first().map_or("none", |v| v.type_name()),
            ))
        }
    };

    let (min_value, min_unit) = match &args.get(1) {
        Some(VmValue::Duration { value, unit }) => (*value, *unit),
        _ => {
            return Err(EvalError::type_error(
                "Duration",
                args.get(1).map_or("none", |v| v.type_name()),
            ))
        }
    };

    let (max_value, max_unit) = match &args.get(2) {
        Some(VmValue::Duration { value, unit }) => (*value, *unit),
        _ => {
            return Err(EvalError::type_error(
                "Duration",
                args.get(2).map_or("none", |v| v.type_name()),
            ))
        }
    };

    // Convert all to nanoseconds for comparison
    let nanos = value * unit.to_nanos_factor();
    let min_nanos = min_value * min_unit.to_nanos_factor();
    let max_nanos = max_value * max_unit.to_nanos_factor();

    Ok(VmValue::Boolean(nanos >= min_nanos && nanos <= max_nanos))
}

// DataSize methods

fn datasize_to_unit(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let (value, unit) = match &args[0] {
        VmValue::DataSize { value, unit } => (*value, *unit),
        _ => {
            return Err(EvalError::type_error(
                "DataSize",
                args.first().map_or("none", |v| v.type_name()),
            ))
        }
    };

    let target_unit_str = args.get(1).and_then(|v| v.as_string()).ok_or_else(|| {
        EvalError::type_error("String", args.get(1).map_or("none", |v| v.type_name()))
    })?;

    let target_unit = DataSizeUnit::from_suffix(target_unit_str).ok_or_else(|| {
        EvalError::InvalidOperation(format!("Unknown data size unit: {}", target_unit_str))
    })?;

    // Convert to bytes, then to target unit
    let bytes = value * unit.to_bytes_factor();
    let converted = bytes / target_unit.to_bytes_factor();

    Ok(VmValue::DataSize {
        value: converted,
        unit: target_unit,
    })
}

fn datasize_is_between(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let (value, unit) = match &args[0] {
        VmValue::DataSize { value, unit } => (*value, *unit),
        _ => {
            return Err(EvalError::type_error(
                "DataSize",
                args.first().map_or("none", |v| v.type_name()),
            ))
        }
    };

    let (min_value, min_unit) = match &args.get(1) {
        Some(VmValue::DataSize { value, unit }) => (*value, *unit),
        _ => {
            return Err(EvalError::type_error(
                "DataSize",
                args.get(1).map_or("none", |v| v.type_name()),
            ))
        }
    };

    let (max_value, max_unit) = match &args.get(2) {
        Some(VmValue::DataSize { value, unit }) => (*value, *unit),
        _ => {
            return Err(EvalError::type_error(
                "DataSize",
                args.get(2).map_or("none", |v| v.type_name()),
            ))
        }
    };

    // Convert all to bytes for comparison
    let bytes = value * unit.to_bytes_factor();
    let min_bytes = min_value * min_unit.to_bytes_factor();
    let max_bytes = max_value * max_unit.to_bytes_factor();

    Ok(VmValue::Boolean(bytes >= min_bytes && bytes <= max_bytes))
}

// Object type conversion methods

fn get_object_arg(args: &[VmValue], idx: usize) -> EvalResult<Arc<VmObject>> {
    args.get(idx)
        .and_then(|v| {
            if let VmValue::Object(obj) = v {
                Some(Arc::clone(obj))
            } else {
                None
            }
        })
        .ok_or_else(|| {
            EvalError::type_error("Object", args.get(idx).map_or("none", |v| v.type_name()))
        })
}

fn to_dynamic(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_object_arg(args, 0)?;

    // Create new Dynamic object
    let new_obj = VmObject::new_dynamic(Arc::clone(scope));

    // Copy only visible (non-hidden) properties
    // Per PKL spec: "Object conversions omit hidden properties"
    for name in this.visible_property_names() {
        if let Some(member) = this.get_property_member(&name) {
            let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
            new_obj.add_property(name, ObjectMember::with_value(value));
        }
    }

    // Copy all elements
    for i in 0..this.element_count() {
        if let Some(member) = this.get_element_member(i) {
            let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
            new_obj.add_element(ObjectMember::with_value(value));
        }
    }

    // Copy all entries
    for key in this.entry_keys() {
        if let Some(member) = this.get_entry_member(&key) {
            let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
            new_obj.add_entry(key, ObjectMember::with_value(value));
        }
    }

    Ok(VmValue::Object(Arc::new(new_obj)))
}

fn to_typed(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_object_arg(args, 0)?;
    let class_name = args.get(1).and_then(|v| v.as_string()).ok_or_else(|| {
        EvalError::type_error("String", args.get(1).map_or("none", |v| v.type_name()))
    })?;

    // Create new Typed object with the given class name
    let new_obj = VmObject::new_typed(class_name.to_string(), Arc::clone(scope));

    // Copy only visible (non-hidden) properties from the source object
    // Per PKL spec: "Object conversions omit hidden properties"
    for name in this.visible_property_names() {
        if let Some(member) = this.get_property_member(&name) {
            let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
            new_obj.add_property(name, ObjectMember::with_value(value));
        }
    }

    // Copy elements (though Typed objects typically don't have elements)
    for i in 0..this.element_count() {
        if let Some(member) = this.get_element_member(i) {
            let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
            new_obj.add_element(ObjectMember::with_value(value));
        }
    }

    // Copy entries (though Typed objects typically don't have entries)
    for key in this.entry_keys() {
        if let Some(member) = this.get_entry_member(&key) {
            let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
            new_obj.add_entry(key, ObjectMember::with_value(value));
        }
    }

    Ok(VmValue::Object(Arc::new(new_obj)))
}
