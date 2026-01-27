//! Math module implementations

use std::sync::Arc;

use rpkl_runtime::{EvalError, EvalResult, ExternalRegistry, VmValue};

pub fn register(registry: &mut ExternalRegistry) {
    // Int properties (accessed without parentheses)
    registry.register_property("Int", "abs", Arc::new(int_abs));
    registry.register_property("Int", "sign", Arc::new(int_sign));
    registry.register_property("Int", "isPositive", Arc::new(int_is_positive));
    registry.register_property("Int", "isNegative", Arc::new(int_is_negative));
    registry.register_property("Int", "isZero", Arc::new(int_is_zero));
    registry.register_property("Int", "isNonZero", Arc::new(int_is_non_zero));
    registry.register_property("Int", "isEven", Arc::new(int_is_even));
    registry.register_property("Int", "isOdd", Arc::new(int_is_odd));

    // Float properties (accessed without parentheses)
    registry.register_property("Float", "abs", Arc::new(float_abs));
    registry.register_property("Float", "sign", Arc::new(float_sign));
    registry.register_property("Float", "isPositive", Arc::new(float_is_positive));
    registry.register_property("Float", "isNegative", Arc::new(float_is_negative));
    registry.register_property("Float", "isFinite", Arc::new(float_is_finite));
    registry.register_property("Float", "isInfinite", Arc::new(float_is_infinite));
    registry.register_property("Float", "isNaN", Arc::new(float_is_nan));
    registry.register_property("Float", "isZero", Arc::new(float_is_zero));
    registry.register_property("Float", "isNonZero", Arc::new(float_is_non_zero));

    // Float methods (called with parentheses)
    registry.register_method("Float", "ceil", Arc::new(float_ceil));
    registry.register_method("Float", "floor", Arc::new(float_floor));
    registry.register_method("Float", "round", Arc::new(float_round));
    registry.register_method("Float", "truncate", Arc::new(float_truncate));

    // Trig and advanced math
    registry.register_method("Float", "sqrt", Arc::new(float_sqrt));
    registry.register_method("Float", "cbrt", Arc::new(float_cbrt));
    registry.register_method("Float", "exp", Arc::new(float_exp));
    registry.register_method("Float", "log", Arc::new(float_log));
    registry.register_method("Float", "log10", Arc::new(float_log10));
    registry.register_method("Float", "log2", Arc::new(float_log2));
    registry.register_method("Float", "sin", Arc::new(float_sin));
    registry.register_method("Float", "cos", Arc::new(float_cos));
    registry.register_method("Float", "tan", Arc::new(float_tan));
    registry.register_method("Float", "asin", Arc::new(float_asin));
    registry.register_method("Float", "acos", Arc::new(float_acos));
    registry.register_method("Float", "atan", Arc::new(float_atan));
    registry.register_method("Float", "sinh", Arc::new(float_sinh));
    registry.register_method("Float", "cosh", Arc::new(float_cosh));
    registry.register_method("Float", "tanh", Arc::new(float_tanh));
}

fn get_float_arg(args: &[VmValue], idx: usize) -> EvalResult<f64> {
    args.get(idx).and_then(|v| v.as_float()).ok_or_else(|| {
        EvalError::type_error("Float", args.get(idx).map_or("none", |v| v.type_name()))
    })
}

fn get_int_arg(args: &[VmValue], idx: usize) -> EvalResult<i64> {
    args.get(idx).and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.get(idx).map_or("none", |v| v.type_name()))
    })
}

// Int methods
fn int_abs(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_int_arg(args, 0)?;
    Ok(VmValue::Int(this.abs()))
}

fn int_sign(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_int_arg(args, 0)?;
    Ok(VmValue::Int(this.signum()))
}

fn int_is_positive(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_int_arg(args, 0)?;
    Ok(VmValue::Boolean(this > 0))
}

fn int_is_negative(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_int_arg(args, 0)?;
    Ok(VmValue::Boolean(this < 0))
}

fn int_is_zero(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_int_arg(args, 0)?;
    Ok(VmValue::Boolean(this == 0))
}

fn int_is_non_zero(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_int_arg(args, 0)?;
    Ok(VmValue::Boolean(this != 0))
}

fn int_is_even(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_int_arg(args, 0)?;
    Ok(VmValue::Boolean(this % 2 == 0))
}

fn int_is_odd(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_int_arg(args, 0)?;
    Ok(VmValue::Boolean(this % 2 != 0))
}

// Float methods
fn float_abs(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.abs()))
}

fn float_sign(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.signum()))
}

fn float_ceil(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.ceil()))
}

fn float_floor(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.floor()))
}

fn float_round(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.round()))
}

fn float_is_positive(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Boolean(this > 0.0))
}

fn float_is_negative(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Boolean(this < 0.0))
}

fn float_is_finite(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Boolean(this.is_finite()))
}

fn float_is_infinite(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Boolean(this.is_infinite()))
}

fn float_is_nan(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Boolean(this.is_nan()))
}

fn float_is_zero(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Boolean(this == 0.0))
}

fn float_is_non_zero(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Boolean(this != 0.0))
}

fn float_sqrt(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.sqrt()))
}

fn float_cbrt(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.cbrt()))
}

fn float_exp(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.exp()))
}

fn float_log(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.ln()))
}

fn float_log10(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.log10()))
}

fn float_log2(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.log2()))
}

fn float_sin(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.sin()))
}

fn float_cos(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.cos()))
}

fn float_tan(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.tan()))
}

fn float_asin(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.asin()))
}

fn float_acos(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.acos()))
}

fn float_atan(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.atan()))
}

fn float_sinh(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.sinh()))
}

fn float_cosh(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.cosh()))
}

fn float_tanh(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.tanh()))
}

fn float_truncate(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_float_arg(args, 0)?;
    Ok(VmValue::Float(this.trunc()))
}
