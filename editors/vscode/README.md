# PKL Language Support for VS Code

This extension provides language support for the [PKL](https://pkl-lang.org) configuration language.

## Features

- Syntax highlighting
- Parse error diagnostics
- Document symbols (Outline view)
- Go to definition
- Hover information
- Code completion
- Semantic token highlighting

## Requirements

This extension requires the `rpkl-lsp` language server to be installed and available in your PATH.

### Building the Language Server

From the root of the rpkl repository:

```bash
cargo build --release -p rpkl-lsp
```

Then add the binary to your PATH, or configure the path in VS Code settings:

```json
{
  "pkl.server.path": "/path/to/rpkl-lsp"
}
```

## Extension Settings

- `pkl.server.path`: Path to the rpkl-lsp executable. If not set, will look for 'rpkl-lsp' in PATH.
- `pkl.trace.server`: Traces the communication between VS Code and the language server.

## Development

### Building the Extension

```bash
cd editors/vscode
npm install
npm run compile
```

### Packaging

```bash
npm run package
```

This creates a `.vsix` file that can be installed in VS Code.

### Testing Locally

1. Open the `editors/vscode` directory in VS Code
2. Press F5 to launch a new VS Code window with the extension loaded
3. Open a `.pkl` file to test the extension

## License

Apache-2.0
