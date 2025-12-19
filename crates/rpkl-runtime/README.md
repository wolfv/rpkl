# rpkl-runtime

Runtime and evaluator for the PKL configuration language.

## Features

- Lazy evaluation of object properties
- External function registry for stdlib integration
- Module loading with import resolution
- Module inheritance (`amends`/`extends`)
- Serde serialization for output

## Usage

```rust
use rpkl_parser::parse_module;
use rpkl_runtime::Evaluator;

let source = r#"
    x = 10
    y = x * 2
"#;

let module = parse_module(source)?;
let evaluator = Evaluator::new();
let result = evaluator.eval_module(&module)?;

// Serialize to JSON
let json = serde_json::to_string(&result)?;
```

## With Standard Library

```rust
use rpkl_runtime::Evaluator;
use rpkl_stdlib::stdlib_registry;

let registry = stdlib_registry();
let evaluator = Evaluator::with_externals(registry);
```

## License

Apache-2.0
