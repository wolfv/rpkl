//! Runtime value types for PKL

use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use indexmap::{IndexMap, IndexSet};
use serde::ser::{SerializeMap, SerializeSeq};
use serde::{Serialize, Serializer};

use crate::object::VmObject;

/// Duration units
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DurationUnit {
    Nanoseconds,
    Microseconds,
    Milliseconds,
    Seconds,
    Minutes,
    Hours,
    Days,
}

impl DurationUnit {
    pub fn to_nanos_factor(self) -> f64 {
        match self {
            DurationUnit::Nanoseconds => 1.0,
            DurationUnit::Microseconds => 1_000.0,
            DurationUnit::Milliseconds => 1_000_000.0,
            DurationUnit::Seconds => 1_000_000_000.0,
            DurationUnit::Minutes => 60_000_000_000.0,
            DurationUnit::Hours => 3_600_000_000_000.0,
            DurationUnit::Days => 86_400_000_000_000.0,
        }
    }

    pub fn suffix(self) -> &'static str {
        match self {
            DurationUnit::Nanoseconds => "ns",
            DurationUnit::Microseconds => "us",
            DurationUnit::Milliseconds => "ms",
            DurationUnit::Seconds => "s",
            DurationUnit::Minutes => "min",
            DurationUnit::Hours => "h",
            DurationUnit::Days => "d",
        }
    }
}

/// DataSize units
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DataSizeUnit {
    Bytes,
    Kilobytes,
    Megabytes,
    Gigabytes,
    Terabytes,
    Petabytes,
    Kibibytes,
    Mebibytes,
    Gibibytes,
    Tebibytes,
    Pebibytes,
}

impl DataSizeUnit {
    pub fn to_bytes_factor(self) -> f64 {
        match self {
            DataSizeUnit::Bytes => 1.0,
            DataSizeUnit::Kilobytes => 1_000.0,
            DataSizeUnit::Megabytes => 1_000_000.0,
            DataSizeUnit::Gigabytes => 1_000_000_000.0,
            DataSizeUnit::Terabytes => 1_000_000_000_000.0,
            DataSizeUnit::Petabytes => 1_000_000_000_000_000.0,
            DataSizeUnit::Kibibytes => 1_024.0,
            DataSizeUnit::Mebibytes => 1_048_576.0,
            DataSizeUnit::Gibibytes => 1_073_741_824.0,
            DataSizeUnit::Tebibytes => 1_099_511_627_776.0,
            DataSizeUnit::Pebibytes => 1_125_899_906_842_624.0,
        }
    }

    pub fn suffix(self) -> &'static str {
        match self {
            DataSizeUnit::Bytes => "b",
            DataSizeUnit::Kilobytes => "kb",
            DataSizeUnit::Megabytes => "mb",
            DataSizeUnit::Gigabytes => "gb",
            DataSizeUnit::Terabytes => "tb",
            DataSizeUnit::Petabytes => "pb",
            DataSizeUnit::Kibibytes => "kib",
            DataSizeUnit::Mebibytes => "mib",
            DataSizeUnit::Gibibytes => "gib",
            DataSizeUnit::Tebibytes => "tib",
            DataSizeUnit::Pebibytes => "pib",
        }
    }
}

/// The core runtime value type
#[derive(Debug, Clone)]
pub enum VmValue {
    /// Null value
    Null,

    /// Boolean value
    Boolean(bool),

    /// 64-bit integer
    Int(i64),

    /// 64-bit floating point
    Float(f64),

    /// String (shared, immutable)
    String(Arc<str>),

    /// Duration with unit
    Duration { value: f64, unit: DurationUnit },

    /// Data size with unit
    DataSize { value: f64, unit: DataSizeUnit },

    /// Immutable list
    List(Arc<Vec<VmValue>>),

    /// Immutable set (preserves insertion order)
    Set(Arc<IndexSet<VmValue>>),

    /// Immutable map (preserves insertion order)
    Map(Arc<IndexMap<VmValue, VmValue>>),

    /// PKL object (Dynamic, Listing, Mapping, or typed)
    Object(Arc<VmObject>),

    /// Lambda/function closure
    Lambda(Arc<LambdaClosure>),

    /// Regex pattern
    Regex(Arc<RegexValue>),

    /// Integer sequence/range
    IntSeq { start: i64, end: i64, step: i64 },

    /// Pair (used internally)
    Pair(Arc<(VmValue, VmValue)>),
}

/// Lambda closure
#[derive(Debug)]
pub struct LambdaClosure {
    pub params: Vec<String>,
    pub body: rpkl_parser::Expr,
    pub captured_scope: crate::scope::ScopeRef,
}

/// Regex wrapper
#[derive(Debug, Clone)]
pub struct RegexValue {
    pub pattern: String,
    // We'll add actual regex later
}

impl VmValue {
    /// Create a string value
    pub fn string(s: impl Into<Arc<str>>) -> Self {
        VmValue::String(s.into())
    }

    /// Create a list value
    pub fn list(items: Vec<VmValue>) -> Self {
        VmValue::List(Arc::new(items))
    }

    /// Create a map value
    pub fn map(items: IndexMap<VmValue, VmValue>) -> Self {
        VmValue::Map(Arc::new(items))
    }

    /// Get the type name of this value
    pub fn type_name(&self) -> &'static str {
        match self {
            VmValue::Null => "Null",
            VmValue::Boolean(_) => "Boolean",
            VmValue::Int(_) => "Int",
            VmValue::Float(_) => "Float",
            VmValue::String(_) => "String",
            VmValue::Duration { .. } => "Duration",
            VmValue::DataSize { .. } => "DataSize",
            VmValue::List(_) => "List",
            VmValue::Set(_) => "Set",
            VmValue::Map(_) => "Map",
            VmValue::Object(obj) => obj.type_name(),
            VmValue::Lambda(_) => "Function",
            VmValue::Regex(_) => "Regex",
            VmValue::IntSeq { .. } => "IntSeq",
            VmValue::Pair(_) => "Pair",
        }
    }

    /// Check if this value is null
    pub fn is_null(&self) -> bool {
        matches!(self, VmValue::Null)
    }

    /// Try to get as bool
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            VmValue::Boolean(b) => Some(*b),
            _ => None,
        }
    }

    /// Try to get as i64
    pub fn as_int(&self) -> Option<i64> {
        match self {
            VmValue::Int(i) => Some(*i),
            _ => None,
        }
    }

    /// Try to get as f64
    pub fn as_float(&self) -> Option<f64> {
        match self {
            VmValue::Float(f) => Some(*f),
            VmValue::Int(i) => Some(*i as f64),
            _ => None,
        }
    }

    /// Try to get as string
    pub fn as_string(&self) -> Option<&str> {
        match self {
            VmValue::String(s) => Some(s),
            _ => None,
        }
    }

    /// Try to get as list
    pub fn as_list(&self) -> Option<&[VmValue]> {
        match self {
            VmValue::List(l) => Some(l),
            _ => None,
        }
    }

    /// Try to get as object
    pub fn as_object(&self) -> Option<&Arc<VmObject>> {
        match self {
            VmValue::Object(o) => Some(o),
            _ => None,
        }
    }

    /// Check truthiness for conditionals
    pub fn is_truthy(&self) -> bool {
        match self {
            VmValue::Null => false,
            VmValue::Boolean(b) => *b,
            _ => true,
        }
    }
}

impl fmt::Display for VmValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VmValue::Null => write!(f, "null"),
            VmValue::Boolean(b) => write!(f, "{}", b),
            VmValue::Int(i) => write!(f, "{}", i),
            VmValue::Float(n) => {
                if n.is_nan() {
                    write!(f, "NaN")
                } else if n.is_infinite() {
                    if *n > 0.0 {
                        write!(f, "Infinity")
                    } else {
                        write!(f, "-Infinity")
                    }
                } else if n.fract() == 0.0 {
                    write!(f, "{}.0", n)
                } else {
                    write!(f, "{}", n)
                }
            }
            VmValue::String(s) => write!(f, "\"{}\"", escape_string(s)),
            VmValue::Duration { value, unit } => write!(f, "{}{}", value, unit.suffix()),
            VmValue::DataSize { value, unit } => write!(f, "{}{}", value, unit.suffix()),
            VmValue::List(items) => {
                write!(f, "List(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
            VmValue::Set(items) => {
                write!(f, "Set(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
            VmValue::Map(items) => {
                write!(f, "Map(")?;
                for (i, (k, v)) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, ")")
            }
            VmValue::Object(obj) => write!(f, "{}", obj),
            VmValue::Lambda(_) => write!(f, "<function>"),
            VmValue::Regex(r) => write!(f, "Regex(\"{}\")", r.pattern),
            VmValue::IntSeq { start, end, step } => {
                write!(f, "IntSeq({}, {}, {})", start, end, step)
            }
            VmValue::Pair(p) => write!(f, "Pair({}, {})", p.0, p.1),
        }
    }
}

fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            c if c.is_control() => {
                result.push_str(&format!("\\u{{{:x}}}", c as u32));
            }
            c => result.push(c),
        }
    }
    result
}

// Implement PartialEq for VmValue
impl PartialEq for VmValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (VmValue::Null, VmValue::Null) => true,
            (VmValue::Boolean(a), VmValue::Boolean(b)) => a == b,
            (VmValue::Int(a), VmValue::Int(b)) => a == b,
            (VmValue::Float(a), VmValue::Float(b)) => {
                if a.is_nan() && b.is_nan() {
                    true
                } else {
                    a == b
                }
            }
            (VmValue::Int(a), VmValue::Float(b)) | (VmValue::Float(b), VmValue::Int(a)) => {
                (*a as f64) == *b
            }
            (VmValue::String(a), VmValue::String(b)) => a == b,
            (
                VmValue::Duration {
                    value: v1,
                    unit: u1,
                },
                VmValue::Duration {
                    value: v2,
                    unit: u2,
                },
            ) => (v1 * u1.to_nanos_factor()) == (v2 * u2.to_nanos_factor()),
            (
                VmValue::DataSize {
                    value: v1,
                    unit: u1,
                },
                VmValue::DataSize {
                    value: v2,
                    unit: u2,
                },
            ) => (v1 * u1.to_bytes_factor()) == (v2 * u2.to_bytes_factor()),
            (VmValue::List(a), VmValue::List(b)) => a == b,
            (VmValue::Set(a), VmValue::Set(b)) => a == b,
            (VmValue::Map(a), VmValue::Map(b)) => a == b,
            (VmValue::Object(a), VmValue::Object(b)) => Arc::ptr_eq(a, b),
            (VmValue::Lambda(a), VmValue::Lambda(b)) => Arc::ptr_eq(a, b),
            (VmValue::Regex(a), VmValue::Regex(b)) => a.pattern == b.pattern,
            (
                VmValue::IntSeq {
                    start: s1,
                    end: e1,
                    step: st1,
                },
                VmValue::IntSeq {
                    start: s2,
                    end: e2,
                    step: st2,
                },
            ) => s1 == s2 && e1 == e2 && st1 == st2,
            (VmValue::Pair(a), VmValue::Pair(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for VmValue {}

impl Hash for VmValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            VmValue::Null => {}
            VmValue::Boolean(b) => b.hash(state),
            VmValue::Int(i) => i.hash(state),
            VmValue::Float(f) => f.to_bits().hash(state),
            VmValue::String(s) => s.hash(state),
            VmValue::Duration { value, unit } => {
                value.to_bits().hash(state);
                unit.hash(state);
            }
            VmValue::DataSize { value, unit } => {
                value.to_bits().hash(state);
                unit.hash(state);
            }
            VmValue::List(l) => {
                for item in l.iter() {
                    item.hash(state);
                }
            }
            VmValue::Set(s) => {
                for item in s.iter() {
                    item.hash(state);
                }
            }
            VmValue::Map(m) => {
                for (k, v) in m.iter() {
                    k.hash(state);
                    v.hash(state);
                }
            }
            VmValue::Object(o) => Arc::as_ptr(o).hash(state),
            VmValue::Lambda(l) => Arc::as_ptr(l).hash(state),
            VmValue::Regex(r) => r.pattern.hash(state),
            VmValue::IntSeq { start, end, step } => {
                start.hash(state);
                end.hash(state);
                step.hash(state);
            }
            VmValue::Pair(p) => {
                p.0.hash(state);
                p.1.hash(state);
            }
        }
    }
}

impl PartialOrd for VmValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (VmValue::Int(a), VmValue::Int(b)) => a.partial_cmp(b),
            (VmValue::Float(a), VmValue::Float(b)) => a.partial_cmp(b),
            (VmValue::Int(a), VmValue::Float(b)) => (*a as f64).partial_cmp(b),
            (VmValue::Float(a), VmValue::Int(b)) => a.partial_cmp(&(*b as f64)),
            (VmValue::String(a), VmValue::String(b)) => a.partial_cmp(b),
            (
                VmValue::Duration {
                    value: v1,
                    unit: u1,
                },
                VmValue::Duration {
                    value: v2,
                    unit: u2,
                },
            ) => {
                let n1 = v1 * u1.to_nanos_factor();
                let n2 = v2 * u2.to_nanos_factor();
                n1.partial_cmp(&n2)
            }
            (
                VmValue::DataSize {
                    value: v1,
                    unit: u1,
                },
                VmValue::DataSize {
                    value: v2,
                    unit: u2,
                },
            ) => {
                let b1 = v1 * u1.to_bytes_factor();
                let b2 = v2 * u2.to_bytes_factor();
                b1.partial_cmp(&b2)
            }
            _ => None,
        }
    }
}

// Serde serialization
impl Serialize for VmValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            VmValue::Null => serializer.serialize_none(),
            VmValue::Boolean(b) => serializer.serialize_bool(*b),
            VmValue::Int(i) => serializer.serialize_i64(*i),
            VmValue::Float(f) => serializer.serialize_f64(*f),
            VmValue::String(s) => serializer.serialize_str(s),
            VmValue::Duration { value, unit } => {
                // Serialize as a string like "5.s" or as an object
                serializer.serialize_str(&format!("{}{}", value, unit.suffix()))
            }
            VmValue::DataSize { value, unit } => {
                serializer.serialize_str(&format!("{}{}", value, unit.suffix()))
            }
            VmValue::List(items) => {
                let mut seq = serializer.serialize_seq(Some(items.len()))?;
                for item in items.iter() {
                    seq.serialize_element(item)?;
                }
                seq.end()
            }
            VmValue::Set(items) => {
                let mut seq = serializer.serialize_seq(Some(items.len()))?;
                for item in items.iter() {
                    seq.serialize_element(item)?;
                }
                seq.end()
            }
            VmValue::Map(items) => {
                let mut map = serializer.serialize_map(Some(items.len()))?;
                for (k, v) in items.iter() {
                    // For map keys, we need string representation
                    let key_str = match k {
                        VmValue::String(s) => s.to_string(),
                        _ => k.to_string(),
                    };
                    map.serialize_entry(&key_str, v)?;
                }
                map.end()
            }
            VmValue::Object(obj) => obj.serialize(serializer),
            VmValue::Lambda(_) => serializer.serialize_str("<function>"),
            VmValue::Regex(r) => serializer.serialize_str(&r.pattern),
            VmValue::IntSeq { start, end, step } => {
                let items: Vec<i64> = (*start..*end).step_by(*step as usize).collect();
                let mut seq = serializer.serialize_seq(Some(items.len()))?;
                for item in items {
                    seq.serialize_element(&item)?;
                }
                seq.end()
            }
            VmValue::Pair(p) => {
                let mut seq = serializer.serialize_seq(Some(2))?;
                seq.serialize_element(&p.0)?;
                seq.serialize_element(&p.1)?;
                seq.end()
            }
        }
    }
}
