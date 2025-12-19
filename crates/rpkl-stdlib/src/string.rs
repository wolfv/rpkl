//! String type implementations

use std::sync::Arc;

use rpkl_runtime::{EvalError, EvalResult, ExternalRegistry, VmValue};

pub fn register(registry: &mut ExternalRegistry) {
    // String methods
    registry.register_method("String", "contains", Arc::new(string_contains));
    registry.register_method("String", "startsWith", Arc::new(string_starts_with));
    registry.register_method("String", "endsWith", Arc::new(string_ends_with));
    registry.register_method("String", "indexOf", Arc::new(string_index_of));
    registry.register_method("String", "lastIndexOf", Arc::new(string_last_index_of));
    registry.register_method("String", "split", Arc::new(string_split));
    registry.register_method("String", "trim", Arc::new(string_trim));
    registry.register_method("String", "trimStart", Arc::new(string_trim_start));
    registry.register_method("String", "trimEnd", Arc::new(string_trim_end));
    registry.register_method("String", "toUpperCase", Arc::new(string_to_upper_case));
    registry.register_method("String", "toLowerCase", Arc::new(string_to_lower_case));
    registry.register_method("String", "reverse", Arc::new(string_reverse));
    registry.register_method("String", "repeat", Arc::new(string_repeat));
    registry.register_method("String", "replaceAll", Arc::new(string_replace_all));
    registry.register_method("String", "replaceFirst", Arc::new(string_replace_first));
    registry.register_method("String", "replaceLast", Arc::new(string_replace_last));
    registry.register_method("String", "take", Arc::new(string_take));
    registry.register_method("String", "takeLast", Arc::new(string_take_last));
    registry.register_method("String", "drop", Arc::new(string_drop));
    registry.register_method("String", "dropLast", Arc::new(string_drop_last));
    registry.register_method("String", "substring", Arc::new(string_substring));
    registry.register_method("String", "getOrNull", Arc::new(string_get_or_null));
    registry.register_method("String", "toInt", Arc::new(string_to_int));
    registry.register_method("String", "toIntOrNull", Arc::new(string_to_int_or_null));
    registry.register_method("String", "toFloat", Arc::new(string_to_float));
    registry.register_method("String", "toFloatOrNull", Arc::new(string_to_float_or_null));
    registry.register_method("String", "capitalize", Arc::new(string_capitalize));
    registry.register_method("String", "decapitalize", Arc::new(string_decapitalize));
    registry.register_method("String", "padStart", Arc::new(string_pad_start));
    registry.register_method("String", "padEnd", Arc::new(string_pad_end));
    registry.register_method("String", "chars", Arc::new(string_chars));
}

fn get_string_arg(args: &[VmValue], idx: usize) -> EvalResult<Arc<str>> {
    args.get(idx)
        .and_then(|v| {
            if let VmValue::String(s) = v {
                Some(Arc::clone(s))
            } else {
                None
            }
        })
        .ok_or_else(|| {
            EvalError::type_error("String", args.get(idx).map_or("none", |v| v.type_name()))
        })
}

fn get_int_arg(args: &[VmValue], idx: usize) -> EvalResult<i64> {
    args.get(idx).and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.get(idx).map_or("none", |v| v.type_name()))
    })
}

fn string_contains(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let pattern = get_string_arg(args, 1)?;
    Ok(VmValue::Boolean(this.contains(&*pattern)))
}

fn string_starts_with(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let prefix = get_string_arg(args, 1)?;
    Ok(VmValue::Boolean(this.starts_with(&*prefix)))
}

fn string_ends_with(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let suffix = get_string_arg(args, 1)?;
    Ok(VmValue::Boolean(this.ends_with(&*suffix)))
}

fn string_index_of(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let pattern = get_string_arg(args, 1)?;
    match this.find(&*pattern) {
        Some(idx) => Ok(VmValue::Int(idx as i64)),
        None => Ok(VmValue::Int(-1)),
    }
}

fn string_last_index_of(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let pattern = get_string_arg(args, 1)?;
    match this.rfind(&*pattern) {
        Some(idx) => Ok(VmValue::Int(idx as i64)),
        None => Ok(VmValue::Int(-1)),
    }
}

fn string_split(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let delimiter = get_string_arg(args, 1)?;
    let parts: Vec<VmValue> = this
        .split(&*delimiter)
        .map(|s| VmValue::string(s.to_string()))
        .collect();
    Ok(VmValue::list(parts))
}

fn string_trim(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    Ok(VmValue::string(this.trim().to_string()))
}

fn string_trim_start(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    Ok(VmValue::string(this.trim_start().to_string()))
}

fn string_trim_end(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    Ok(VmValue::string(this.trim_end().to_string()))
}

fn string_to_upper_case(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    Ok(VmValue::string(this.to_uppercase()))
}

fn string_to_lower_case(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    Ok(VmValue::string(this.to_lowercase()))
}

fn string_reverse(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    Ok(VmValue::string(this.chars().rev().collect::<String>()))
}

fn string_repeat(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let count = get_int_arg(args, 1)?;
    if count < 0 {
        return Err(EvalError::InvalidOperation(
            "repeat count must be non-negative".to_string(),
        ));
    }
    Ok(VmValue::string(this.repeat(count as usize)))
}

fn string_replace_all(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let pattern = get_string_arg(args, 1)?;
    let replacement = get_string_arg(args, 2)?;
    Ok(VmValue::string(this.replace(&*pattern, &replacement)))
}

fn string_replace_first(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let pattern = get_string_arg(args, 1)?;
    let replacement = get_string_arg(args, 2)?;
    Ok(VmValue::string(this.replacen(&*pattern, &replacement, 1)))
}

fn string_replace_last(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let pattern = get_string_arg(args, 1)?;
    let replacement = get_string_arg(args, 2)?;

    // Find last occurrence and replace
    if let Some(pos) = this.rfind(&*pattern) {
        let mut result = this.to_string();
        result.replace_range(pos..pos + pattern.len(), &replacement);
        Ok(VmValue::string(result))
    } else {
        Ok(VmValue::String(Arc::clone(&this)))
    }
}

fn string_take(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let n = get_int_arg(args, 1)?;
    let chars: String = this.chars().take(n.max(0) as usize).collect();
    Ok(VmValue::string(chars))
}

fn string_take_last(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let n = get_int_arg(args, 1)?;
    let chars: Vec<char> = this.chars().collect();
    let start = chars.len().saturating_sub(n.max(0) as usize);
    Ok(VmValue::string(chars[start..].iter().collect::<String>()))
}

fn string_drop(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let n = get_int_arg(args, 1)?;
    let chars: String = this.chars().skip(n.max(0) as usize).collect();
    Ok(VmValue::string(chars))
}

fn string_drop_last(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let n = get_int_arg(args, 1)?;
    let chars: Vec<char> = this.chars().collect();
    let end = chars.len().saturating_sub(n.max(0) as usize);
    Ok(VmValue::string(chars[..end].iter().collect::<String>()))
}

fn string_substring(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let start = get_int_arg(args, 1)?;
    let end = get_int_arg(args, 2)?;

    let chars: Vec<char> = this.chars().collect();
    let start = start.max(0) as usize;
    let end = end.max(0) as usize;

    if start > chars.len() || end > chars.len() || start > end {
        return Err(EvalError::IndexOutOfBounds {
            index: if start > chars.len() {
                start as i64
            } else {
                end as i64
            },
            length: chars.len(),
        });
    }

    Ok(VmValue::string(
        chars[start..end].iter().collect::<String>(),
    ))
}

fn string_get_or_null(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let index = get_int_arg(args, 1)?;

    let chars: Vec<char> = this.chars().collect();
    let idx = if index < 0 {
        (chars.len() as i64 + index) as usize
    } else {
        index as usize
    };

    if idx >= chars.len() {
        Ok(VmValue::Null)
    } else {
        Ok(VmValue::string(chars[idx].to_string()))
    }
}

fn string_to_int(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    match this.trim().parse::<i64>() {
        Ok(n) => Ok(VmValue::Int(n)),
        Err(_) => Err(EvalError::InvalidOperation(format!(
            "Cannot parse '{}' as Int",
            this
        ))),
    }
}

fn string_to_int_or_null(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    match this.trim().parse::<i64>() {
        Ok(n) => Ok(VmValue::Int(n)),
        Err(_) => Ok(VmValue::Null),
    }
}

fn string_to_float(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    match this.trim().parse::<f64>() {
        Ok(n) => Ok(VmValue::Float(n)),
        Err(_) => Err(EvalError::InvalidOperation(format!(
            "Cannot parse '{}' as Float",
            this
        ))),
    }
}

fn string_to_float_or_null(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    match this.trim().parse::<f64>() {
        Ok(n) => Ok(VmValue::Float(n)),
        Err(_) => Ok(VmValue::Null),
    }
}

fn string_capitalize(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let mut chars = this.chars();
    let result = match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
    };
    Ok(VmValue::string(result))
}

fn string_decapitalize(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let mut chars = this.chars();
    let result = match chars.next() {
        None => String::new(),
        Some(first) => first.to_lowercase().collect::<String>() + chars.as_str(),
    };
    Ok(VmValue::string(result))
}

fn string_pad_start(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let length = get_int_arg(args, 1)?;
    let pad_char = get_string_arg(args, 2)?;

    let pad = pad_char.chars().next().unwrap_or(' ');
    let current_len = this.chars().count();
    let target_len = length.max(0) as usize;

    if current_len >= target_len {
        Ok(VmValue::String(this))
    } else {
        let padding: String = std::iter::repeat_n(pad, target_len - current_len).collect();
        Ok(VmValue::string(padding + &this))
    }
}

fn string_pad_end(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let length = get_int_arg(args, 1)?;
    let pad_char = get_string_arg(args, 2)?;

    let pad = pad_char.chars().next().unwrap_or(' ');
    let current_len = this.chars().count();
    let target_len = length.max(0) as usize;

    if current_len >= target_len {
        Ok(VmValue::String(this))
    } else {
        let padding: String = std::iter::repeat_n(pad, target_len - current_len).collect();
        Ok(VmValue::string(this.to_string() + &padding))
    }
}

fn string_chars(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_string_arg(args, 0)?;
    let chars: Vec<VmValue> = this
        .chars()
        .map(|c| VmValue::string(c.to_string()))
        .collect();
    Ok(VmValue::list(chars))
}
