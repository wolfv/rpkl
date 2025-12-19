# rpkl

Command-line interface for the PKL configuration language.

## Installation

```bash
cargo install rpkl
```

## Commands

### eval

Evaluate a PKL file and output the result.

```bash
# JSON output (default)
rpkl eval config.pkl

# YAML output
rpkl eval config.pkl -f yaml

# PCF output (PKL Configuration Format)
rpkl eval config.pkl -f pcf

# Compact JSON (no pretty printing)
rpkl eval config.pkl --pretty false
```

### expr

Evaluate a PKL expression.

```bash
rpkl expr "1 + 2 * 3"
rpkl expr '"hello".toUpperCase()'
```

### parse

Parse a PKL file and show the AST (for debugging).

```bash
rpkl parse config.pkl
```

## Examples

```pkl
// config.pkl
name = "my-app"
version = "1.0.0"
debug = true

database {
  host = "localhost"
  port = 5432
}
```

```bash
$ rpkl eval config.pkl
{
  "name": "my-app",
  "version": "1.0.0",
  "debug": true,
  "database": {
    "host": "localhost",
    "port": 5432
  }
}
```

## License

Apache-2.0
