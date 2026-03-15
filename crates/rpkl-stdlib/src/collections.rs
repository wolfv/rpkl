//! Collection type implementations (List, Set, Map)

use std::sync::Arc;

use indexmap::{IndexMap, IndexSet};
use rpkl_runtime::{EvalError, EvalResult, ExternalRegistry, VmValue};

pub fn register(registry: &mut ExternalRegistry) {
    // List methods
    registry.register_method("List", "contains", Arc::new(list_contains));
    registry.register_method("List", "indexOf", Arc::new(list_index_of));
    registry.register_method("List", "lastIndexOf", Arc::new(list_last_index_of));
    registry.register_method("List", "take", Arc::new(list_take));
    registry.register_method("List", "takeLast", Arc::new(list_take_last));
    registry.register_method("List", "drop", Arc::new(list_drop));
    registry.register_method("List", "dropLast", Arc::new(list_drop_last));
    registry.register_method("List", "reverse", Arc::new(list_reverse));
    registry.register_method("List", "distinct", Arc::new(list_distinct));
    registry.register_method("List", "flatten", Arc::new(list_flatten));
    registry.register_method("List", "join", Arc::new(list_join));
    registry.register_method("List", "getOrNull", Arc::new(list_get_or_null));
    registry.register_method("List", "toList", Arc::new(list_to_list));
    registry.register_method("List", "toSet", Arc::new(list_to_set));
    registry.register_method("List", "single", Arc::new(list_single));
    registry.register_method("List", "singleOrNull", Arc::new(list_single_or_null));
    registry.register_method("List", "min", Arc::new(list_min));
    registry.register_method("List", "max", Arc::new(list_max));
    registry.register_method("List", "minOrNull", Arc::new(list_min_or_null));
    registry.register_method("List", "maxOrNull", Arc::new(list_max_or_null));

    // Higher-order functions
    registry.register_method("List", "map", Arc::new(list_map));
    registry.register_method("List", "filter", Arc::new(list_filter));
    registry.register_method("List", "fold", Arc::new(list_fold));
    registry.register_method("List", "flatMap", Arc::new(list_flat_map));
    registry.register_method("List", "any", Arc::new(list_any));
    registry.register_method("List", "every", Arc::new(list_every));
    registry.register_method("List", "find", Arc::new(list_find));
    registry.register_method("List", "findOrNull", Arc::new(list_find_or_null));
    registry.register_method("List", "count", Arc::new(list_count));
    registry.register_method("List", "partition", Arc::new(list_partition));
    registry.register_method("List", "groupBy", Arc::new(list_group_by));
    registry.register_method("List", "add", Arc::new(list_add));
    registry.register_method("List", "replace", Arc::new(list_replace));
    registry.register_method("List", "sort", Arc::new(list_sort));
    registry.register_method("List", "sortBy", Arc::new(list_sort_by));
    registry.register_method("List", "mapIndexed", Arc::new(list_map_indexed));
    registry.register_method("List", "filterIndexed", Arc::new(list_filter_indexed));
    registry.register_method("List", "foldIndexed", Arc::new(list_fold_indexed));
    registry.register_method("List", "zip", Arc::new(list_zip));
    registry.register_method("List", "minBy", Arc::new(list_min_by));
    registry.register_method("List", "maxBy", Arc::new(list_max_by));
    registry.register_method("List", "split", Arc::new(list_split));
    registry.register_method("List", "findLast", Arc::new(list_find_last));
    registry.register_method("List", "findIndex", Arc::new(list_find_index));
    registry.register_method("List", "repeat", Arc::new(list_repeat));
    registry.register_method("List", "toListing", Arc::new(list_to_listing));
    registry.register_method("List", "toMap", Arc::new(list_to_map));

    // Map methods
    registry.register_method("Map", "containsKey", Arc::new(map_contains_key));
    registry.register_method("Map", "getOrNull", Arc::new(map_get_or_null));
    registry.register_method("Map", "toMap", Arc::new(map_to_map));
    registry.register_method("Map", "remove", Arc::new(map_remove));

    // Map properties (accessed without parentheses)
    registry.register_property("Map", "entries", Arc::new(map_entries));
    registry.register_property("Map", "keys", Arc::new(map_keys));
    registry.register_property("Map", "values", Arc::new(map_values));
    registry.register_property("Map", "length", Arc::new(map_length));
    registry.register_property("Map", "isEmpty", Arc::new(map_is_empty));

    // Mapping methods (VmObject with Mapping kind)
    registry.register_method("Mapping", "toMap", Arc::new(mapping_to_map));
    registry.register_method("Mapping", "containsKey", Arc::new(mapping_contains_key));
    registry.register_method("Mapping", "getOrNull", Arc::new(mapping_get_or_null));
    registry.register_property("Mapping", "length", Arc::new(mapping_length));
    registry.register_property("Mapping", "isEmpty", Arc::new(mapping_is_empty));

    // Listing methods (VmObject with Listing kind)
    registry.register_method("Listing", "toList", Arc::new(listing_to_list));
    registry.register_method("Listing", "join", Arc::new(listing_join));
    registry.register_method("Listing", "map", Arc::new(listing_map));
    registry.register_method("Listing", "filter", Arc::new(listing_filter));
    registry.register_method("Listing", "fold", Arc::new(listing_fold));
    registry.register_property("Listing", "length", Arc::new(listing_length));
    registry.register_property("Listing", "isEmpty", Arc::new(listing_is_empty));
    registry.register_property("Listing", "first", Arc::new(listing_first));
    registry.register_property("Listing", "last", Arc::new(listing_last));
    registry.register_property("Listing", "rest", Arc::new(listing_rest));

    // Mapping additional methods
    registry.register_method("Mapping", "filter", Arc::new(mapping_filter));
    registry.register_method("Mapping", "map", Arc::new(mapping_map));
    registry.register_method("Mapping", "fold", Arc::new(mapping_fold));
    registry.register_property("Mapping", "keys", Arc::new(mapping_keys));
    registry.register_property("Mapping", "values", Arc::new(mapping_values));

    // Set methods
    registry.register_method("Set", "contains", Arc::new(set_contains));
    registry.register_method("Set", "add", Arc::new(set_add));
    registry.register_method("Set", "remove", Arc::new(set_remove));
    registry.register_method("Set", "intersect", Arc::new(set_intersect));
    registry.register_method("Set", "difference", Arc::new(set_difference));
    registry.register_method("Set", "union", Arc::new(set_union));
    registry.register_method("Set", "filter", Arc::new(set_filter));
    registry.register_method("Set", "map", Arc::new(set_map));
    registry.register_method("Set", "flatMap", Arc::new(set_flat_map));
    registry.register_method("Set", "fold", Arc::new(set_fold));
    registry.register_method("Set", "any", Arc::new(set_any));
    registry.register_method("Set", "every", Arc::new(set_every));
    registry.register_method("Set", "toList", Arc::new(set_to_list));
    registry.register_method("Set", "toSet", Arc::new(set_to_set));

    // IntSeq methods
    registry.register_method("IntSeq", "toList", Arc::new(intseq_to_list));
    registry.register_method("IntSeq", "map", Arc::new(intseq_map));
    registry.register_method("IntSeq", "step", Arc::new(intseq_step));

    // Map additional methods
    registry.register_method("Map", "put", Arc::new(map_put));
    registry.register_method("Map", "filter", Arc::new(map_filter));
    registry.register_method("Map", "map", Arc::new(map_map));
    registry.register_method("Map", "fold", Arc::new(map_fold));
    registry.register_method("Map", "any", Arc::new(map_any));
    registry.register_method("Map", "every", Arc::new(map_every));

    // List additional methods
    registry.register_method("List", "addAll", Arc::new(list_add_all));
    registry.register_method("List", "replaceRange", Arc::new(list_replace_range));
    registry.register_method("List", "findLastIndex", Arc::new(list_find_last_index));
    registry.register_method("List", "takeWhile", Arc::new(list_take_while));
    registry.register_method("List", "dropWhile", Arc::new(list_drop_while));
    registry.register_method("List", "takeLastWhile", Arc::new(list_take_last_while));
    registry.register_method("List", "dropLastWhile", Arc::new(list_drop_last_while));
}

fn get_list_arg(args: &[VmValue], idx: usize) -> EvalResult<Arc<Vec<VmValue>>> {
    args.get(idx)
        .and_then(|v| {
            if let VmValue::List(l) = v {
                Some(Arc::clone(l))
            } else {
                None
            }
        })
        .ok_or_else(|| {
            EvalError::type_error("List", args.get(idx).map_or("none", |v| v.type_name()))
        })
}

fn get_map_arg(args: &[VmValue], idx: usize) -> EvalResult<Arc<IndexMap<VmValue, VmValue>>> {
    args.get(idx)
        .and_then(|v| {
            if let VmValue::Map(m) = v {
                Some(Arc::clone(m))
            } else {
                None
            }
        })
        .ok_or_else(|| {
            EvalError::type_error("Map", args.get(idx).map_or("none", |v| v.type_name()))
        })
}

fn get_int_arg(args: &[VmValue], idx: usize) -> EvalResult<i64> {
    args.get(idx).and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.get(idx).map_or("none", |v| v.type_name()))
    })
}

fn list_contains(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let element = args.get(1).ok_or(EvalError::WrongArgCount {
        expected: 2,
        actual: 1,
    })?;
    Ok(VmValue::Boolean(this.contains(element)))
}

fn list_index_of(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let element = args.get(1).ok_or(EvalError::WrongArgCount {
        expected: 2,
        actual: 1,
    })?;
    match this.iter().position(|x| x == element) {
        Some(idx) => Ok(VmValue::Int(idx as i64)),
        None => Ok(VmValue::Int(-1)),
    }
}

fn list_last_index_of(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let element = args.get(1).ok_or(EvalError::WrongArgCount {
        expected: 2,
        actual: 1,
    })?;
    match this.iter().rposition(|x| x == element) {
        Some(idx) => Ok(VmValue::Int(idx as i64)),
        None => Ok(VmValue::Int(-1)),
    }
}

fn list_take(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let n = get_int_arg(args, 1)?;
    let result: Vec<VmValue> = this.iter().take(n.max(0) as usize).cloned().collect();
    Ok(VmValue::list(result))
}

fn list_take_last(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let n = get_int_arg(args, 1)?;
    let start = this.len().saturating_sub(n.max(0) as usize);
    let result: Vec<VmValue> = this[start..].to_vec();
    Ok(VmValue::list(result))
}

fn list_drop(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let n = get_int_arg(args, 1)?;
    let result: Vec<VmValue> = this.iter().skip(n.max(0) as usize).cloned().collect();
    Ok(VmValue::list(result))
}

fn list_drop_last(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let n = get_int_arg(args, 1)?;
    let end = this.len().saturating_sub(n.max(0) as usize);
    let result: Vec<VmValue> = this[..end].to_vec();
    Ok(VmValue::list(result))
}

fn list_reverse(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let result: Vec<VmValue> = this.iter().rev().cloned().collect();
    Ok(VmValue::list(result))
}

fn list_distinct(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let mut seen = indexmap::IndexSet::new();
    let result: Vec<VmValue> = this
        .iter()
        .filter(|v| seen.insert((*v).clone()))
        .cloned()
        .collect();
    Ok(VmValue::list(result))
}

fn list_flatten(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let mut result = Vec::new();
    for item in this.iter() {
        match item {
            VmValue::List(inner) => result.extend(inner.iter().cloned()),
            other => result.push(other.clone()),
        }
    }
    Ok(VmValue::list(result))
}

fn list_join(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let separator = args.get(1).and_then(|v| v.as_string()).unwrap_or("");

    let result: String = this
        .iter()
        .map(|v| match v {
            VmValue::String(s) => s.to_string(),
            _ => v.to_string(),
        })
        .collect::<Vec<_>>()
        .join(separator);

    Ok(VmValue::string(result))
}

fn list_get_or_null(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let index = get_int_arg(args, 1)?;

    let idx = if index < 0 {
        (this.len() as i64 + index) as usize
    } else {
        index as usize
    };

    if idx >= this.len() {
        Ok(VmValue::Null)
    } else {
        Ok(this[idx].clone())
    }
}

fn list_to_list(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    Ok(VmValue::List(this))
}

fn list_to_set(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let set: indexmap::IndexSet<VmValue> = this.iter().cloned().collect();
    Ok(VmValue::Set(Arc::new(set)))
}

fn list_single(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    if this.len() != 1 {
        Err(EvalError::InvalidOperation(format!(
            "Expected list with exactly one element, got {}",
            this.len()
        )))
    } else {
        Ok(this[0].clone())
    }
}

fn list_single_or_null(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    if this.len() == 1 {
        Ok(this[0].clone())
    } else {
        Ok(VmValue::Null)
    }
}

fn list_min(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    if this.is_empty() {
        return Err(EvalError::InvalidOperation(
            "Cannot get min of empty list".to_string(),
        ));
    }
    this.iter()
        .min_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
        .cloned()
        .ok_or_else(|| EvalError::InvalidOperation("Cannot compare elements".to_string()))
}

fn list_max(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    if this.is_empty() {
        return Err(EvalError::InvalidOperation(
            "Cannot get max of empty list".to_string(),
        ));
    }
    this.iter()
        .max_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
        .cloned()
        .ok_or_else(|| EvalError::InvalidOperation("Cannot compare elements".to_string()))
}

fn list_min_or_null(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    if this.is_empty() {
        return Ok(VmValue::Null);
    }
    Ok(this
        .iter()
        .min_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
        .cloned()
        .unwrap_or(VmValue::Null))
}

fn list_max_or_null(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    if this.is_empty() {
        return Ok(VmValue::Null);
    }
    Ok(this
        .iter()
        .max_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
        .cloned()
        .unwrap_or(VmValue::Null))
}

fn map_contains_key(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_map_arg(args, 0)?;
    let key = args.get(1).ok_or(EvalError::WrongArgCount {
        expected: 2,
        actual: 1,
    })?;
    Ok(VmValue::Boolean(this.contains_key(key)))
}

fn map_get_or_null(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_map_arg(args, 0)?;
    let key = args.get(1).ok_or(EvalError::WrongArgCount {
        expected: 2,
        actual: 1,
    })?;
    Ok(this.get(key).cloned().unwrap_or(VmValue::Null))
}

fn map_to_map(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_map_arg(args, 0)?;
    Ok(VmValue::Map(this))
}

fn map_remove(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_map_arg(args, 0)?;
    let key = args.get(1).ok_or(EvalError::WrongArgCount {
        expected: 2,
        actual: 1,
    })?;

    // Clone the map and remove the key
    let mut new_map = (*this).clone();
    new_map.shift_remove(key);

    Ok(VmValue::Map(Arc::new(new_map)))
}

// Helper to call a lambda with arguments
fn call_lambda(
    lambda: &rpkl_runtime::LambdaClosure,
    args: Vec<VmValue>,
    eval: &rpkl_runtime::Evaluator,
) -> EvalResult<VmValue> {
    let params: Vec<(String, VmValue)> = lambda
        .params
        .iter()
        .zip(args)
        .map(|(name, value)| (name.clone(), value))
        .collect();
    let lambda_scope = rpkl_runtime::Scope::for_lambda(&lambda.captured_scope, params);
    eval.eval_expr(&lambda.body, &lambda_scope)
}

fn get_lambda_arg(args: &[VmValue], idx: usize) -> EvalResult<Arc<rpkl_runtime::LambdaClosure>> {
    args.get(idx)
        .and_then(|v| {
            if let VmValue::Lambda(l) = v {
                Some(Arc::clone(l))
            } else {
                None
            }
        })
        .ok_or_else(|| {
            EvalError::type_error("Function", args.get(idx).map_or("none", |v| v.type_name()))
        })
}

fn list_map(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    let mut result = Vec::with_capacity(this.len());
    for item in this.iter() {
        let mapped = call_lambda(&func, vec![item.clone()], eval)?;
        result.push(mapped);
    }
    Ok(VmValue::list(result))
}

fn list_filter(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    let mut result = Vec::new();
    for item in this.iter() {
        let keep = call_lambda(&func, vec![item.clone()], eval)?;
        if keep.is_truthy() {
            result.push(item.clone());
        }
    }
    Ok(VmValue::list(result))
}

fn list_fold(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let initial = args.get(1).ok_or(EvalError::WrongArgCount {
        expected: 3,
        actual: args.len(),
    })?;
    let func = get_lambda_arg(args, 2)?;

    let mut acc = initial.clone();
    for item in this.iter() {
        acc = call_lambda(&func, vec![acc, item.clone()], eval)?;
    }
    Ok(acc)
}

fn list_flat_map(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    let mut result = Vec::new();
    for item in this.iter() {
        let mapped = call_lambda(&func, vec![item.clone()], eval)?;
        match mapped {
            VmValue::List(inner) => result.extend(inner.iter().cloned()),
            other => result.push(other),
        }
    }
    Ok(VmValue::list(result))
}

fn list_any(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    for item in this.iter() {
        let result = call_lambda(&func, vec![item.clone()], eval)?;
        if result.is_truthy() {
            return Ok(VmValue::Boolean(true));
        }
    }
    Ok(VmValue::Boolean(false))
}

fn list_every(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    for item in this.iter() {
        let result = call_lambda(&func, vec![item.clone()], eval)?;
        if !result.is_truthy() {
            return Ok(VmValue::Boolean(false));
        }
    }
    Ok(VmValue::Boolean(true))
}

fn list_find(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    for item in this.iter() {
        let result = call_lambda(&func, vec![item.clone()], eval)?;
        if result.is_truthy() {
            return Ok(item.clone());
        }
    }
    Err(EvalError::InvalidOperation(
        "No element matching predicate found".to_string(),
    ))
}

fn list_find_or_null(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    for item in this.iter() {
        let result = call_lambda(&func, vec![item.clone()], eval)?;
        if result.is_truthy() {
            return Ok(item.clone());
        }
    }
    Ok(VmValue::Null)
}

fn list_count(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    let mut count = 0i64;
    for item in this.iter() {
        let result = call_lambda(&func, vec![item.clone()], eval)?;
        if result.is_truthy() {
            count += 1;
        }
    }
    Ok(VmValue::Int(count))
}

fn list_partition(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    let mut first = Vec::new();
    let mut second = Vec::new();
    for item in this.iter() {
        let result = call_lambda(&func, vec![item.clone()], eval)?;
        if result.is_truthy() {
            first.push(item.clone());
        } else {
            second.push(item.clone());
        }
    }
    Ok(VmValue::Pair(Arc::new((
        VmValue::list(first),
        VmValue::list(second),
    ))))
}

fn list_group_by(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    let mut groups: IndexMap<VmValue, Vec<VmValue>> = IndexMap::new();
    for item in this.iter() {
        let key = call_lambda(&func, vec![item.clone()], eval)?;
        groups.entry(key).or_default().push(item.clone());
    }

    let result: IndexMap<VmValue, VmValue> = groups
        .into_iter()
        .map(|(k, v)| (k, VmValue::list(v)))
        .collect();
    Ok(VmValue::Map(Arc::new(result)))
}

// Listing methods (for VmObject with Listing kind)

fn get_listing_arg(args: &[VmValue], idx: usize) -> EvalResult<Arc<rpkl_runtime::VmObject>> {
    args.get(idx)
        .and_then(|v| {
            if let VmValue::Object(obj) = v {
                Some(Arc::clone(obj))
            } else {
                None
            }
        })
        .ok_or_else(|| {
            EvalError::type_error("Listing", args.get(idx).map_or("none", |v| v.type_name()))
        })
}

fn listing_to_list(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_listing_arg(args, 0)?;
    let count = obj.element_count();
    let mut result = Vec::with_capacity(count);

    for i in 0..count {
        if let Some(member) = obj.get_element_member(i) {
            let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
            result.push(value);
        }
    }

    Ok(VmValue::list(result))
}

fn listing_length(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_listing_arg(args, 0)?;
    Ok(VmValue::Int(obj.element_count() as i64))
}

fn listing_is_empty(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_listing_arg(args, 0)?;
    Ok(VmValue::Boolean(obj.element_count() == 0))
}

// Map additional methods

fn map_entries(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_map_arg(args, 0)?;
    let entries: Vec<VmValue> = this
        .iter()
        .map(|(k, v)| VmValue::Pair(Arc::new((k.clone(), v.clone()))))
        .collect();
    Ok(VmValue::list(entries))
}

fn map_keys(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_map_arg(args, 0)?;
    let keys: Vec<VmValue> = this.keys().cloned().collect();
    Ok(VmValue::list(keys))
}

fn map_values(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_map_arg(args, 0)?;
    let values: Vec<VmValue> = this.values().cloned().collect();
    Ok(VmValue::list(values))
}

fn map_length(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_map_arg(args, 0)?;
    Ok(VmValue::Int(this.len() as i64))
}

fn map_is_empty(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_map_arg(args, 0)?;
    Ok(VmValue::Boolean(this.is_empty()))
}

// Mapping methods (VmObject with Mapping kind)

fn get_mapping_arg(args: &[VmValue], idx: usize) -> EvalResult<Arc<rpkl_runtime::VmObject>> {
    args.get(idx)
        .and_then(|v| {
            if let VmValue::Object(obj) = v {
                if obj.is_mapping() {
                    Some(Arc::clone(obj))
                } else {
                    None
                }
            } else {
                None
            }
        })
        .ok_or_else(|| {
            EvalError::type_error("Mapping", args.get(idx).map_or("none", |v| v.type_name()))
        })
}

fn mapping_to_map(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_mapping_arg(args, 0)?;
    let keys = obj.entry_keys();
    let mut map = IndexMap::new();

    for key in keys {
        if let Some(member) = obj.get_entry_member(&key) {
            let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
            map.insert(key, value);
        }
    }

    Ok(VmValue::Map(Arc::new(map)))
}

fn mapping_length(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_mapping_arg(args, 0)?;
    Ok(VmValue::Int(obj.entry_keys().len() as i64))
}

fn mapping_is_empty(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_mapping_arg(args, 0)?;
    Ok(VmValue::Boolean(obj.entry_keys().is_empty()))
}

fn mapping_contains_key(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_mapping_arg(args, 0)?;
    let key = args.get(1).ok_or(EvalError::WrongArgCount {
        expected: 2,
        actual: 1,
    })?;

    // Mapping keys are VmValue, so we need to check if the key matches
    let key_str = key.as_string().map(|s| VmValue::string(s.to_string()));
    let keys = obj.entry_keys();
    let found = if let Some(k) = key_str {
        keys.iter().any(|entry_key| entry_key == &k)
    } else {
        keys.iter().any(|entry_key| entry_key == key)
    };
    Ok(VmValue::Boolean(found))
}

fn mapping_get_or_null(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_mapping_arg(args, 0)?;
    let key = args.get(1).ok_or(EvalError::WrongArgCount {
        expected: 2,
        actual: 1,
    })?;

    // Try to find the key - it could be a string
    let key_to_find = if let Some(s) = key.as_string() {
        VmValue::string(s.to_string())
    } else {
        key.clone()
    };

    if let Some(member) = obj.get_entry_member(&key_to_find) {
        let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
        Ok(value)
    } else {
        Ok(VmValue::Null)
    }
}

fn listing_join(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_listing_arg(args, 0)?;
    let separator = args.get(1).and_then(|v| v.as_string()).unwrap_or("");

    let count = obj.element_count();
    let mut strings = Vec::with_capacity(count);

    for i in 0..count {
        if let Some(member) = obj.get_element_member(i) {
            let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
            let s = match &value {
                VmValue::String(s) => s.to_string(),
                _ => value.to_string(),
            };
            strings.push(s);
        }
    }

    Ok(VmValue::string(strings.join(separator)))
}

fn listing_map(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_listing_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    let count = obj.element_count();
    let mut result = Vec::with_capacity(count);

    for i in 0..count {
        if let Some(member) = obj.get_element_member(i) {
            let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
            let mapped = call_lambda(&func, vec![value], eval)?;
            result.push(mapped);
        }
    }

    Ok(VmValue::list(result))
}

// =============================================================================
// Additional List methods
// =============================================================================

fn list_add(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let element = args.get(1).ok_or(EvalError::WrongArgCount {
        expected: 2,
        actual: 1,
    })?;
    let mut result: Vec<VmValue> = this.iter().cloned().collect();
    result.push(element.clone());
    Ok(VmValue::list(result))
}

fn list_replace(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let index = get_int_arg(args, 1)?;
    let element = args.get(2).ok_or(EvalError::WrongArgCount {
        expected: 3,
        actual: 2,
    })?;

    if index < 0 || index as usize >= this.len() {
        return Err(EvalError::IndexOutOfBounds {
            index,
            length: this.len(),
        });
    }

    let mut result: Vec<VmValue> = this.iter().cloned().collect();
    result[index as usize] = element.clone();
    Ok(VmValue::list(result))
}

fn list_sort(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let mut result: Vec<VmValue> = this.iter().cloned().collect();

    // Sort by comparing as floats for numbers, strings for strings
    result.sort_by(|a, b| match (a.as_float(), b.as_float()) {
        (Some(af), Some(bf)) => af.partial_cmp(&bf).unwrap_or(std::cmp::Ordering::Equal),
        _ => match (a.as_string(), b.as_string()) {
            (Some(as_), Some(bs)) => as_.cmp(bs),
            _ => std::cmp::Ordering::Equal,
        },
    });

    Ok(VmValue::list(result))
}

fn list_sort_by(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    // Compute keys for all elements
    let mut keyed: Vec<(VmValue, VmValue)> = Vec::with_capacity(this.len());
    for item in this.iter() {
        let key = call_lambda(&func, vec![item.clone()], eval)?;
        keyed.push((key, item.clone()));
    }

    // Sort by keys
    keyed.sort_by(|(ka, _), (kb, _)| match (ka.as_float(), kb.as_float()) {
        (Some(af), Some(bf)) => af.partial_cmp(&bf).unwrap_or(std::cmp::Ordering::Equal),
        _ => match (ka.as_string(), kb.as_string()) {
            (Some(as_), Some(bs)) => as_.cmp(bs),
            _ => std::cmp::Ordering::Equal,
        },
    });

    Ok(VmValue::list(keyed.into_iter().map(|(_, v)| v).collect()))
}

fn list_map_indexed(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    let mut result = Vec::with_capacity(this.len());
    for (i, item) in this.iter().enumerate() {
        let mapped = call_lambda(&func, vec![VmValue::Int(i as i64), item.clone()], eval)?;
        result.push(mapped);
    }
    Ok(VmValue::list(result))
}

fn list_filter_indexed(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    let mut result = Vec::new();
    for (i, item) in this.iter().enumerate() {
        let keep = call_lambda(&func, vec![VmValue::Int(i as i64), item.clone()], eval)?;
        if keep.is_truthy() {
            result.push(item.clone());
        }
    }
    Ok(VmValue::list(result))
}

fn list_fold_indexed(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let initial = args.get(1).ok_or(EvalError::WrongArgCount {
        expected: 3,
        actual: args.len(),
    })?;
    let func = get_lambda_arg(args, 2)?;

    let mut acc = initial.clone();
    for (i, item) in this.iter().enumerate() {
        acc = call_lambda(&func, vec![VmValue::Int(i as i64), acc, item.clone()], eval)?;
    }
    Ok(acc)
}

fn list_zip(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let other = get_list_arg(args, 1)?;

    let result: Vec<VmValue> = this
        .iter()
        .zip(other.iter())
        .map(|(a, b)| VmValue::Pair(Arc::new((a.clone(), b.clone()))))
        .collect();

    Ok(VmValue::list(result))
}

fn list_min_by(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    if this.is_empty() {
        return Err(EvalError::InvalidOperation(
            "Cannot get min of empty list".to_string(),
        ));
    }

    let mut min_item = this[0].clone();
    let mut min_key = call_lambda(&func, vec![min_item.clone()], eval)?;

    for item in this.iter().skip(1) {
        let key = call_lambda(&func, vec![item.clone()], eval)?;
        let is_less = match (key.as_float(), min_key.as_float()) {
            (Some(a), Some(b)) => a < b,
            _ => match (key.as_string(), min_key.as_string()) {
                (Some(a), Some(b)) => a < b,
                _ => false,
            },
        };
        if is_less {
            min_item = item.clone();
            min_key = key;
        }
    }

    Ok(min_item)
}

fn list_max_by(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    if this.is_empty() {
        return Err(EvalError::InvalidOperation(
            "Cannot get max of empty list".to_string(),
        ));
    }

    let mut max_item = this[0].clone();
    let mut max_key = call_lambda(&func, vec![max_item.clone()], eval)?;

    for item in this.iter().skip(1) {
        let key = call_lambda(&func, vec![item.clone()], eval)?;
        let is_greater = match (key.as_float(), max_key.as_float()) {
            (Some(a), Some(b)) => a > b,
            _ => match (key.as_string(), max_key.as_string()) {
                (Some(a), Some(b)) => a > b,
                _ => false,
            },
        };
        if is_greater {
            max_item = item.clone();
            max_key = key;
        }
    }

    Ok(max_item)
}

fn list_split(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let index = get_int_arg(args, 1)?;

    if index < 0 || index as usize > this.len() {
        return Err(EvalError::IndexOutOfBounds {
            index,
            length: this.len(),
        });
    }

    let idx = index as usize;
    let first = VmValue::list(this[..idx].to_vec());
    let second = VmValue::list(this[idx..].to_vec());

    Ok(VmValue::Pair(Arc::new((first, second))))
}

fn list_find_last(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    for item in this.iter().rev() {
        let matches = call_lambda(&func, vec![item.clone()], eval)?;
        if matches.is_truthy() {
            return Ok(item.clone());
        }
    }

    Err(EvalError::InvalidOperation(
        "No element matches the predicate".to_string(),
    ))
}

fn list_find_index(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;

    for (i, item) in this.iter().enumerate() {
        let matches = call_lambda(&func, vec![item.clone()], eval)?;
        if matches.is_truthy() {
            return Ok(VmValue::Int(i as i64));
        }
    }

    Ok(VmValue::Int(-1))
}

fn list_repeat(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let count = get_int_arg(args, 1)?;

    if count < 0 {
        return Err(EvalError::InvalidOperation(
            "repeat count must be non-negative".to_string(),
        ));
    }

    let mut result = Vec::with_capacity(this.len() * count as usize);
    for _ in 0..count {
        result.extend(this.iter().cloned());
    }
    Ok(VmValue::list(result))
}

fn list_to_listing(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;

    // Create a new Listing object
    let obj = rpkl_runtime::VmObject::new_listing(Arc::clone(scope));

    for item in this.iter() {
        obj.add_element(rpkl_runtime::ObjectMember::with_value(item.clone()));
    }

    Ok(VmValue::Object(Arc::new(obj)))
}

fn list_to_map(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;

    // If no lambda args, treat as List<Pair> -> Map
    if args.len() <= 1 {
        let mut map = IndexMap::new();
        for item in this.iter() {
            if let VmValue::Pair(p) = item {
                map.insert(p.0.clone(), p.1.clone());
            } else {
                return Err(EvalError::type_error("Pair", item.type_name()));
            }
        }
        return Ok(VmValue::Map(Arc::new(map)));
    }

    let key_func = get_lambda_arg(args, 1)?;
    let value_func = get_lambda_arg(args, 2)?;

    let mut map = IndexMap::new();
    for item in this.iter() {
        let key = call_lambda(&key_func, vec![item.clone()], eval)?;
        let value = call_lambda(&value_func, vec![item.clone()], eval)?;
        map.insert(key, value);
    }

    Ok(VmValue::Map(Arc::new(map)))
}

// =============================================================================
// Listing filter and fold
// =============================================================================

fn listing_filter(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_listing_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;
    let count = obj.element_count();
    let mut result = Vec::new();
    for i in 0..count {
        if let Some(member) = obj.get_element_member(i) {
            let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
            let keep = call_lambda(&func, vec![value.clone()], eval)?;
            if keep.is_truthy() {
                result.push(value);
            }
        }
    }
    Ok(VmValue::list(result))
}

fn listing_fold(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_listing_arg(args, 0)?;
    let mut acc = args.get(1).cloned().ok_or(EvalError::WrongArgCount { expected: 3, actual: 1 })?;
    let func = get_lambda_arg(args, 2)?;
    let count = obj.element_count();
    for i in 0..count {
        if let Some(member) = obj.get_element_member(i) {
            let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
            acc = call_lambda(&func, vec![acc, value], eval)?;
        }
    }
    Ok(acc)
}

fn listing_first(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_listing_arg(args, 0)?;
    if obj.element_count() == 0 {
        return Err(EvalError::InvalidOperation("Cannot get first of empty Listing".to_string()));
    }
    if let Some(member) = obj.get_element_member(0) {
        member.force(|expr, scope| eval.eval_expr(expr, scope))
    } else {
        Err(EvalError::InvalidOperation("Cannot get first of empty Listing".to_string()))
    }
}

fn listing_last(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_listing_arg(args, 0)?;
    let count = obj.element_count();
    if count == 0 {
        return Err(EvalError::InvalidOperation("Cannot get last of empty Listing".to_string()));
    }
    if let Some(member) = obj.get_element_member(count - 1) {
        member.force(|expr, scope| eval.eval_expr(expr, scope))
    } else {
        Err(EvalError::InvalidOperation("Cannot get last of empty Listing".to_string()))
    }
}

fn listing_rest(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_listing_arg(args, 0)?;
    let count = obj.element_count();
    let mut result = Vec::new();
    for i in 1..count {
        if let Some(member) = obj.get_element_member(i) {
            let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
            result.push(value);
        }
    }
    Ok(VmValue::list(result))
}

// =============================================================================
// Mapping filter, map, fold, keys, values
// =============================================================================

fn mapping_filter(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_mapping_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;
    let mut map = IndexMap::new();
    for key in obj.entry_keys() {
        if let Some(member) = obj.get_entry_member(&key) {
            let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
            let keep = call_lambda(&func, vec![key.clone(), value.clone()], eval)?;
            if keep.is_truthy() {
                map.insert(key, value);
            }
        }
    }
    Ok(VmValue::Map(Arc::new(map)))
}

fn mapping_map(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_mapping_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;
    let mut map = IndexMap::new();
    for key in obj.entry_keys() {
        if let Some(member) = obj.get_entry_member(&key) {
            let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
            let result = call_lambda(&func, vec![key.clone(), value], eval)?;
            if let VmValue::Pair(p) = result {
                map.insert(p.0.clone(), p.1.clone());
            } else {
                return Err(EvalError::type_error("Pair", result.type_name()));
            }
        }
    }
    Ok(VmValue::Map(Arc::new(map)))
}

fn mapping_fold(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_mapping_arg(args, 0)?;
    let mut acc = args.get(1).cloned().ok_or(EvalError::WrongArgCount { expected: 3, actual: 1 })?;
    let func = get_lambda_arg(args, 2)?;
    for key in obj.entry_keys() {
        if let Some(member) = obj.get_entry_member(&key) {
            let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
            acc = call_lambda(&func, vec![acc, key, value], eval)?;
        }
    }
    Ok(acc)
}

fn mapping_keys(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_mapping_arg(args, 0)?;
    let keys: Vec<VmValue> = obj.entry_keys();
    Ok(VmValue::list(keys))
}

fn mapping_values(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let obj = get_mapping_arg(args, 0)?;
    let mut values = Vec::new();
    for key in obj.entry_keys() {
        if let Some(member) = obj.get_entry_member(&key) {
            let value = member.force(|expr, scope| eval.eval_expr(expr, scope))?;
            values.push(value);
        }
    }
    Ok(VmValue::list(values))
}

// =============================================================================
// Set methods
// =============================================================================

fn get_set_arg(args: &[VmValue], idx: usize) -> EvalResult<Arc<IndexSet<VmValue>>> {
    args.get(idx)
        .and_then(|v| {
            if let VmValue::Set(s) = v {
                Some(Arc::clone(s))
            } else {
                None
            }
        })
        .ok_or_else(|| {
            EvalError::type_error("Set", args.get(idx).map_or("none", |v| v.type_name()))
        })
}

fn set_contains(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_set_arg(args, 0)?;
    let element = args.get(1).ok_or(EvalError::WrongArgCount { expected: 2, actual: 1 })?;
    Ok(VmValue::Boolean(this.contains(element)))
}

fn set_add(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_set_arg(args, 0)?;
    let element = args.get(1).ok_or(EvalError::WrongArgCount { expected: 2, actual: 1 })?;
    let mut new_set = (*this).clone();
    new_set.insert(element.clone());
    Ok(VmValue::Set(Arc::new(new_set)))
}

fn set_remove(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_set_arg(args, 0)?;
    let element = args.get(1).ok_or(EvalError::WrongArgCount { expected: 2, actual: 1 })?;
    let mut new_set = (*this).clone();
    new_set.shift_remove(element);
    Ok(VmValue::Set(Arc::new(new_set)))
}

fn set_intersect(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_set_arg(args, 0)?;
    let other = get_set_arg(args, 1)?;
    let result: IndexSet<VmValue> = this.iter().filter(|v| other.contains(*v)).cloned().collect();
    Ok(VmValue::Set(Arc::new(result)))
}

fn set_difference(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_set_arg(args, 0)?;
    let other = get_set_arg(args, 1)?;
    let result: IndexSet<VmValue> = this.iter().filter(|v| !other.contains(*v)).cloned().collect();
    Ok(VmValue::Set(Arc::new(result)))
}

fn set_union(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_set_arg(args, 0)?;
    let other = get_set_arg(args, 1)?;
    let mut result = (*this).clone();
    for v in other.iter() {
        result.insert(v.clone());
    }
    Ok(VmValue::Set(Arc::new(result)))
}

fn set_filter(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_set_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;
    let mut result = IndexSet::new();
    for item in this.iter() {
        let keep = call_lambda(&func, vec![item.clone()], eval)?;
        if keep.is_truthy() {
            result.insert(item.clone());
        }
    }
    Ok(VmValue::Set(Arc::new(result)))
}

fn set_map(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_set_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;
    let mut result = IndexSet::new();
    for item in this.iter() {
        let mapped = call_lambda(&func, vec![item.clone()], eval)?;
        result.insert(mapped);
    }
    Ok(VmValue::Set(Arc::new(result)))
}

fn set_flat_map(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_set_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;
    let mut result = IndexSet::new();
    for item in this.iter() {
        let mapped = call_lambda(&func, vec![item.clone()], eval)?;
        if let VmValue::Set(s) = mapped {
            for v in s.iter() {
                result.insert(v.clone());
            }
        } else if let VmValue::List(l) = mapped {
            for v in l.iter() {
                result.insert(v.clone());
            }
        } else {
            result.insert(mapped);
        }
    }
    Ok(VmValue::Set(Arc::new(result)))
}

fn set_fold(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_set_arg(args, 0)?;
    let mut acc = args.get(1).cloned().ok_or(EvalError::WrongArgCount { expected: 3, actual: 1 })?;
    let func = get_lambda_arg(args, 2)?;
    for item in this.iter() {
        acc = call_lambda(&func, vec![acc, item.clone()], eval)?;
    }
    Ok(acc)
}

fn set_any(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_set_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;
    for item in this.iter() {
        let result = call_lambda(&func, vec![item.clone()], eval)?;
        if result.is_truthy() {
            return Ok(VmValue::Boolean(true));
        }
    }
    Ok(VmValue::Boolean(false))
}

fn set_every(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_set_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;
    for item in this.iter() {
        let result = call_lambda(&func, vec![item.clone()], eval)?;
        if !result.is_truthy() {
            return Ok(VmValue::Boolean(false));
        }
    }
    Ok(VmValue::Boolean(true))
}

fn set_to_list(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_set_arg(args, 0)?;
    let result: Vec<VmValue> = this.iter().cloned().collect();
    Ok(VmValue::list(result))
}

fn set_to_set(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    Ok(args[0].clone())
}

// =============================================================================
// IntSeq methods
// =============================================================================

fn intseq_to_list(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let (start, end, step) = get_intseq_args(&args[0])?;
    let mut result = Vec::new();
    if step > 0 {
        let mut i = start;
        while i <= end {
            result.push(VmValue::Int(i));
            i += step;
        }
    } else if step < 0 {
        let mut i = start;
        while i >= end {
            result.push(VmValue::Int(i));
            i += step;
        }
    }
    Ok(VmValue::list(result))
}

fn intseq_map(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let (start, end, step) = get_intseq_args(&args[0])?;
    let func = get_lambda_arg(args, 1)?;
    let mut result = Vec::new();
    if step > 0 {
        let mut i = start;
        while i <= end {
            let mapped = call_lambda(&func, vec![VmValue::Int(i)], eval)?;
            result.push(mapped);
            i += step;
        }
    } else if step < 0 {
        let mut i = start;
        while i >= end {
            let mapped = call_lambda(&func, vec![VmValue::Int(i)], eval)?;
            result.push(mapped);
            i += step;
        }
    }
    Ok(VmValue::list(result))
}

fn intseq_step(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let (start, end, _) = get_intseq_args(&args[0])?;
    let new_step = args.get(1).and_then(|v| v.as_int()).ok_or_else(|| {
        EvalError::type_error("Int", args.get(1).map_or("none", |v| v.type_name()))
    })?;
    Ok(VmValue::IntSeq { start, end, step: new_step })
}

fn get_intseq_args(value: &VmValue) -> EvalResult<(i64, i64, i64)> {
    if let VmValue::IntSeq { start, end, step } = value {
        Ok((*start, *end, *step))
    } else {
        Err(EvalError::type_error("IntSeq", value.type_name()))
    }
}

// =============================================================================
// Map additional methods: put, filter, map, fold, any, every
// =============================================================================

fn map_put(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_map_arg(args, 0)?;
    let key = args.get(1).ok_or(EvalError::WrongArgCount { expected: 3, actual: 1 })?.clone();
    let value = args.get(2).ok_or(EvalError::WrongArgCount { expected: 3, actual: 2 })?.clone();
    let mut new_map = (*this).clone();
    new_map.insert(key, value);
    Ok(VmValue::Map(Arc::new(new_map)))
}

fn map_filter(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_map_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;
    let mut result = IndexMap::new();
    for (k, v) in this.iter() {
        let keep = call_lambda(&func, vec![k.clone(), v.clone()], eval)?;
        if keep.is_truthy() {
            result.insert(k.clone(), v.clone());
        }
    }
    Ok(VmValue::Map(Arc::new(result)))
}

fn map_map(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_map_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;
    let mut result = IndexMap::new();
    for (k, v) in this.iter() {
        let mapped = call_lambda(&func, vec![k.clone(), v.clone()], eval)?;
        if let VmValue::Pair(p) = mapped {
            result.insert(p.0.clone(), p.1.clone());
        } else {
            return Err(EvalError::type_error("Pair", mapped.type_name()));
        }
    }
    Ok(VmValue::Map(Arc::new(result)))
}

fn map_fold(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_map_arg(args, 0)?;
    let mut acc = args.get(1).cloned().ok_or(EvalError::WrongArgCount { expected: 3, actual: 1 })?;
    let func = get_lambda_arg(args, 2)?;
    for (k, v) in this.iter() {
        acc = call_lambda(&func, vec![acc, k.clone(), v.clone()], eval)?;
    }
    Ok(acc)
}

fn map_any(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_map_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;
    for (k, v) in this.iter() {
        let result = call_lambda(&func, vec![k.clone(), v.clone()], eval)?;
        if result.is_truthy() {
            return Ok(VmValue::Boolean(true));
        }
    }
    Ok(VmValue::Boolean(false))
}

fn map_every(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_map_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;
    for (k, v) in this.iter() {
        let result = call_lambda(&func, vec![k.clone(), v.clone()], eval)?;
        if !result.is_truthy() {
            return Ok(VmValue::Boolean(false));
        }
    }
    Ok(VmValue::Boolean(true))
}

// =============================================================================
// List additional methods: addAll, replaceRange, findLastIndex, takeWhile, etc.
// =============================================================================

fn list_add_all(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let other = get_list_arg(args, 1)?;
    let mut result: Vec<VmValue> = this.iter().cloned().collect();
    result.extend(other.iter().cloned());
    Ok(VmValue::list(result))
}

fn list_replace_range(
    args: &[VmValue],
    _eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let start = get_int_arg(args, 1)? as usize;
    let exclusive_end = get_int_arg(args, 2)? as usize;
    let replacement = get_list_arg(args, 3)?;
    let mut result: Vec<VmValue> = this.iter().cloned().collect();
    let end = exclusive_end.min(result.len());
    let start = start.min(result.len());
    result.splice(start..end, replacement.iter().cloned());
    Ok(VmValue::list(result))
}

fn list_find_last_index(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;
    for (i, item) in this.iter().enumerate().rev() {
        let result = call_lambda(&func, vec![item.clone()], eval)?;
        if result.is_truthy() {
            return Ok(VmValue::Int(i as i64));
        }
    }
    Ok(VmValue::Int(-1))
}

fn list_take_while(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;
    let mut result = Vec::new();
    for item in this.iter() {
        let keep = call_lambda(&func, vec![item.clone()], eval)?;
        if keep.is_truthy() {
            result.push(item.clone());
        } else {
            break;
        }
    }
    Ok(VmValue::list(result))
}

fn list_drop_while(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;
    let mut dropping = true;
    let mut result = Vec::new();
    for item in this.iter() {
        if dropping {
            let drop = call_lambda(&func, vec![item.clone()], eval)?;
            if !drop.is_truthy() {
                dropping = false;
                result.push(item.clone());
            }
        } else {
            result.push(item.clone());
        }
    }
    Ok(VmValue::list(result))
}

fn list_take_last_while(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;
    let mut count = 0;
    for item in this.iter().rev() {
        let keep = call_lambda(&func, vec![item.clone()], eval)?;
        if keep.is_truthy() {
            count += 1;
        } else {
            break;
        }
    }
    let start = this.len() - count;
    let result: Vec<VmValue> = this[start..].iter().cloned().collect();
    Ok(VmValue::list(result))
}

fn list_drop_last_while(
    args: &[VmValue],
    eval: &rpkl_runtime::Evaluator,
    _scope: &rpkl_runtime::ScopeRef,
) -> EvalResult<VmValue> {
    let this = get_list_arg(args, 0)?;
    let func = get_lambda_arg(args, 1)?;
    let mut count = 0;
    for item in this.iter().rev() {
        let drop = call_lambda(&func, vec![item.clone()], eval)?;
        if drop.is_truthy() {
            count += 1;
        } else {
            break;
        }
    }
    let end = this.len() - count;
    let result: Vec<VmValue> = this[..end].iter().cloned().collect();
    Ok(VmValue::list(result))
}
