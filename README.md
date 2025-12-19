# rpkl

A Rust implementation of the [PKL configuration language](https://pkl-lang.org/).

## Overview

PKL (Pickle) is a configuration language created by Apple that combines the simplicity of static configuration formats with the power of a programming language. This project provides a pure Rust implementation of PKL, including:

- **rpkl-parser** - PKL parser built with pest
- **rpkl-runtime** - Evaluation engine with lazy evaluation
- **rpkl-stdlib** - Standard library implementations
- **rpkl** - Command-line interface

## Installation

### From crates.io

```bash
cargo install rpkl
```

### From source

```bash
git clone https://github.com/prefix-dev/rpkl
cd rpkl
cargo install --path crates/rpkl
```

## Usage

### Evaluate a PKL file

```bash
# Output as JSON (default)
rpkl eval config.pkl

# Output as YAML
rpkl eval config.pkl -f yaml

# Output as PCF (PKL Configuration Format)
rpkl eval config.pkl -f pcf
```

### Example PKL file

```pkl
name = "my-application"
version = "1.0.0"
port = 8080

database {
  host = "localhost"
  port = 5432
  name = "myapp"
}

servers = new Listing {
  "server1.example.com"
  "server2.example.com"
}
```

### Evaluate an expression

```bash
rpkl expr "1 + 2 * 3"
```

## Features

- **Types**: Null, Boolean, Int, Float, String, Duration, DataSize, List, Set, Map, Objects
- **Classes**: Class definitions with inheritance (`extends`)
- **Module inheritance**: `amends` and `extends` for configuration layering
- **Type system**: `is` and `as` operators, nullable types, union types, constrained types
- **Lambdas**: First-class functions with `map`, `filter`, `fold`, etc.
- **Generators**: `for` and `when` generators in object bodies
- **String interpolation**: `"Hello, \(name)!"`
- **Standard library**: String, Int, Float, List, Map, and collection methods

## Library Usage

```rust
use rpkl_parser::parse_module;
use rpkl_runtime::Evaluator;
use rpkl_stdlib::stdlib_registry;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
        name = "example"
        value = 1 + 2
    "#;

    let module = parse_module(source)?;
    let registry = stdlib_registry();
    let evaluator = Evaluator::with_externals(registry);
    let result = evaluator.eval_module(&module)?;

    println!("{}", serde_json::to_string_pretty(&result)?);
    Ok(())
}
```

## License

Apache-2.0
