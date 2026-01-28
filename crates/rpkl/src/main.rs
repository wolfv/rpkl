//! PKL CLI - Command line interface for PKL
//!
//! # Usage
//!
//! ```text
//! rpkl eval <file>              Evaluate a PKL file and output as JSON
//! rpkl eval <file> -f yaml      Evaluate and output as YAML
//! rpkl eval <file> -f json      Evaluate and output as JSON (default)
//! ```

use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use clap::{Parser, Subcommand, ValueEnum};
use rpkl_parser::parse_module;
use rpkl_runtime::{Evaluator, VmValue};
use rpkl_stdlib::stdlib_registry;

#[derive(Parser)]
#[command(name = "rpkl")]
#[command(author, version, about = "PKL configuration language (Rust implementation)", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Evaluate a PKL file
    Eval {
        /// The PKL file to evaluate
        file: PathBuf,

        /// Output format
        #[arg(short, long, value_enum, default_value = "json")]
        format: OutputFormat,

        /// Pretty print output
        #[arg(short, long, default_value = "true")]
        pretty: bool,
    },

    /// Parse a PKL file and show AST (for debugging)
    Parse {
        /// The PKL file to parse
        file: PathBuf,
    },

    /// Evaluate a PKL expression
    Expr {
        /// The expression to evaluate
        expression: String,

        /// Output format
        #[arg(short, long, value_enum, default_value = "json")]
        format: OutputFormat,
    },
}

#[derive(Copy, Clone, PartialEq, Eq, ValueEnum)]
enum OutputFormat {
    Json,
    Yaml,
    Pcf,
}

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Eval {
            file,
            format,
            pretty,
        } => {
            let mut registry = stdlib_registry();
            // Register the _recordAccess external function for Variant.pkl
            // This is a pass-through that just returns the value (no tracking in CLI)
            registry.register_function(
                "Variant",
                "_recordAccess",
                Arc::new(|args, _eval, _scope| {
                    // Just return the second argument (the value)
                    let value = args.get(1).cloned().unwrap_or(VmValue::string(""));
                    Ok(value)
                }),
            );
            let evaluator = Evaluator::with_externals(registry);
            let result = evaluator
                .eval_file(&file)
                .map_err(|e| format!("Evaluation error: {}", e))?;

            // Force all lazy members before serialization
            evaluator
                .force_value(&result)
                .map_err(|e| format!("Evaluation error: {}", e))?;

            let output = match format {
                OutputFormat::Json => {
                    if pretty {
                        serde_json::to_string_pretty(&result)?
                    } else {
                        serde_json::to_string(&result)?
                    }
                }
                OutputFormat::Yaml => serde_yaml::to_string(&result)?,
                OutputFormat::Pcf => render_pcf(&result),
            };

            println!("{}", output);
        }

        Commands::Parse { file } => {
            let source = fs::read_to_string(&file)?;
            let module = parse_module(&source).map_err(|e| format!("Parse error: {}", e))?;
            println!("{:#?}", module);
        }

        Commands::Expr { expression, format } => {
            let expr = rpkl_parser::parse_expression(&expression)
                .map_err(|e| format!("Parse error: {}", e))?;

            let registry = stdlib_registry();
            let evaluator = Evaluator::with_externals(registry);
            let scope = rpkl_runtime::Scope::new();
            let result = evaluator
                .eval_expr(&expr, &scope)
                .map_err(|e| format!("Evaluation error: {}", e))?;

            let output = match format {
                OutputFormat::Json => serde_json::to_string_pretty(&result)?,
                OutputFormat::Yaml => serde_yaml::to_string(&result)?,
                OutputFormat::Pcf => render_pcf(&result),
            };

            println!("{}", output);
        }
    }

    Ok(())
}

/// Render value as PKL Configuration Format (PCF)
fn render_pcf(value: &rpkl_runtime::VmValue) -> String {
    render_pcf_value(value, 0)
}

fn render_pcf_value(value: &rpkl_runtime::VmValue, indent: usize) -> String {
    use rpkl_runtime::VmValue;

    let indent_str = "  ".repeat(indent);

    match value {
        VmValue::Null => "null".to_string(),
        VmValue::Boolean(b) => b.to_string(),
        VmValue::Int(i) => i.to_string(),
        VmValue::Float(f) => {
            if f.fract() == 0.0 {
                format!("{}.0", f)
            } else {
                f.to_string()
            }
        }
        VmValue::String(s) => format!("\"{}\"", escape_pcf_string(s)),
        VmValue::Duration { value, unit } => format!("{}{}", value, unit.suffix()),
        VmValue::DataSize { value, unit } => format!("{}{}", value, unit.suffix()),
        VmValue::List(items) => {
            if items.is_empty() {
                "List()".to_string()
            } else {
                let mut result = String::from("new Listing {\n");
                for item in items.iter() {
                    result.push_str(&indent_str);
                    result.push_str("  ");
                    result.push_str(&render_pcf_value(item, indent + 1));
                    result.push('\n');
                }
                result.push_str(&indent_str);
                result.push('}');
                result
            }
        }
        VmValue::Set(items) => {
            if items.is_empty() {
                "Set()".to_string()
            } else {
                let mut result = String::from("Set(");
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&render_pcf_value(item, indent));
                }
                result.push(')');
                result
            }
        }
        VmValue::Map(items) => {
            if items.is_empty() {
                "Map()".to_string()
            } else {
                let mut result = String::from("new Mapping {\n");
                for (k, v) in items.iter() {
                    result.push_str(&indent_str);
                    result.push_str("  [");
                    result.push_str(&render_pcf_value(k, indent + 1));
                    result.push_str("] = ");
                    result.push_str(&render_pcf_value(v, indent + 1));
                    result.push('\n');
                }
                result.push_str(&indent_str);
                result.push('}');
                result
            }
        }
        VmValue::Object(obj) => {
            // Use visible_property_names to filter out hidden properties
            let names = obj.visible_property_names();
            let element_count = obj.element_count();
            let entry_keys = obj.entry_keys();

            if names.is_empty() && element_count == 0 && entry_keys.is_empty() {
                "{}".to_string()
            } else {
                let mut result = String::from("{\n");

                // Properties
                for name in names {
                    if let Some(member) = obj.get_property_member(&name) {
                        if let Some(value) = member.get_if_evaluated() {
                            result.push_str(&indent_str);
                            result.push_str("  ");
                            result.push_str(&name);
                            result.push_str(" = ");
                            result.push_str(&render_pcf_value(&value, indent + 1));
                            result.push('\n');
                        }
                    }
                }

                // Elements
                for i in 0..element_count {
                    if let Some(member) = obj.get_element_member(i) {
                        if let Some(value) = member.get_if_evaluated() {
                            result.push_str(&indent_str);
                            result.push_str("  ");
                            result.push_str(&render_pcf_value(&value, indent + 1));
                            result.push('\n');
                        }
                    }
                }

                // Entries
                for key in entry_keys {
                    if let Some(member) = obj.get_entry_member(&key) {
                        if let Some(value) = member.get_if_evaluated() {
                            result.push_str(&indent_str);
                            result.push_str("  [");
                            result.push_str(&render_pcf_value(&key, indent + 1));
                            result.push_str("] = ");
                            result.push_str(&render_pcf_value(&value, indent + 1));
                            result.push('\n');
                        }
                    }
                }

                result.push_str(&indent_str);
                result.push('}');
                result
            }
        }
        VmValue::Lambda(_) => "<function>".to_string(),
        VmValue::Regex(r) => format!("Regex(\"{}\")", r.pattern),
        VmValue::IntSeq { start, end, step } => {
            format!("IntSeq({}, {}, {})", start, end, step)
        }
        VmValue::Pair(p) => {
            format!(
                "Pair({}, {})",
                render_pcf_value(&p.0, indent),
                render_pcf_value(&p.1, indent)
            )
        }
        VmValue::ExternalFunc { .. } => "<external function>".to_string(),
    }
}

fn escape_pcf_string(s: &str) -> String {
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
