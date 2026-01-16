import * as path from 'path';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind,
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export async function activate(context: vscode.ExtensionContext): Promise<void> {
    const outputChannel = vscode.window.createOutputChannel('PKL Language Server');

    // Get the path to the language server
    const serverPath = getServerPath(context);
    outputChannel.appendLine(`Using language server at: ${serverPath}`);

    // Server options - run the rpkl-lsp binary
    const serverOptions: ServerOptions = {
        run: {
            command: serverPath,
            transport: TransportKind.stdio,
            options: {
                env: {
                    ...process.env,
                    RUST_LOG: 'rpkl_lsp=debug',
                },
            },
        },
        debug: {
            command: serverPath,
            transport: TransportKind.stdio,
            options: {
                env: {
                    ...process.env,
                    RUST_LOG: 'rpkl_lsp=trace',
                },
            },
        },
    };

    // Client options
    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'pkl' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.pkl'),
        },
        outputChannel,
        traceOutputChannel: outputChannel,
    };

    // Create the language client
    client = new LanguageClient(
        'pkl',
        'PKL Language Server',
        serverOptions,
        clientOptions
    );

    // Register restart command
    const restartCommand = vscode.commands.registerCommand('pkl.restartServer', async () => {
        if (client) {
            outputChannel.appendLine('Restarting language server...');
            await client.restart();
            outputChannel.appendLine('Language server restarted');
        }
    });
    context.subscriptions.push(restartCommand);

    // Start the client
    try {
        await client.start();
        outputChannel.appendLine('PKL Language Server started successfully');
    } catch (error) {
        outputChannel.appendLine(`Failed to start language server: ${error}`);
        vscode.window.showErrorMessage(
            `Failed to start PKL Language Server. Make sure 'rpkl-lsp' is installed and in your PATH.`
        );
    }
}

export async function deactivate(): Promise<void> {
    if (client) {
        await client.stop();
    }
}

function getServerPath(context: vscode.ExtensionContext): string {
    // Check if user has configured a custom path
    const config = vscode.workspace.getConfiguration('pkl');
    const customPath = config.get<string>('server.path');

    if (customPath && customPath.trim() !== '') {
        return customPath;
    }

    // Try to find the bundled server
    const bundledPath = context.asAbsolutePath(
        path.join('server', process.platform === 'win32' ? 'rpkl-lsp.exe' : 'rpkl-lsp')
    );

    // Check if bundled server exists
    try {
        require('fs').accessSync(bundledPath, require('fs').constants.X_OK);
        return bundledPath;
    } catch {
        // Fall back to looking in PATH
        return 'rpkl-lsp';
    }
}
