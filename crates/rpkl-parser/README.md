# rpkl-parser

PKL parser for Rust, built with [pest](https://pest.rs/).

## Usage

```rust
use rpkl_parser::{parse_module, parse_expression};

// Parse a module
let module = parse_module(r#"
    name = "example"
    value = 42
"#)?;

// Parse a single expression
let expr = parse_expression("1 + 2 * 3")?;
```

## Supported Syntax

- Literals: null, booleans, integers, floats, strings (with interpolation)
- Operators: arithmetic, comparison, logical, null-coalescing
- Objects: dynamic, Listing, Mapping, typed
- Classes: with inheritance (`extends`)
- Modules: `amends`, `extends`, `import`
- Functions: `function` declarations and lambdas
- Type annotations and constraints
- Generators: `for`, `when`
- Duration and DataSize literals

## License

Apache-2.0
