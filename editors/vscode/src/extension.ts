import * as path from 'path';
import * as fs from 'fs';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind,
    State,
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let statusBarItem: vscode.StatusBarItem | undefined;

export async function activate(context: vscode.ExtensionContext): Promise<void> {
    const outputChannel = vscode.window.createOutputChannel('PKL Language Server');

    // Create status bar item
    statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 0);
    statusBarItem.text = '$(loading~spin) PKL';
    statusBarItem.tooltip = 'PKL Language Server: Starting...';
    statusBarItem.command = 'pkl.showOutputChannel';
    statusBarItem.show();
    context.subscriptions.push(statusBarItem);

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

    // Track client state changes for status bar
    client.onDidChangeState((event) => {
        updateStatusBar(event.newState);
    });

    // Register commands
    context.subscriptions.push(
        vscode.commands.registerCommand('pkl.restartServer', async () => {
            if (client) {
                outputChannel.appendLine('Restarting language server...');
                updateStatusBar(State.Starting);
                await client.restart();
                outputChannel.appendLine('Language server restarted');
            }
        }),
        vscode.commands.registerCommand('pkl.showOutputChannel', () => {
            outputChannel.show();
        }),
        vscode.commands.registerCommand('pkl.stopServer', async () => {
            if (client) {
                await client.stop();
                outputChannel.appendLine('Language server stopped');
            }
        }),
    );

    // Start the client
    try {
        await client.start();
        outputChannel.appendLine('PKL Language Server started successfully');
        updateStatusBar(State.Running);
    } catch (error) {
        outputChannel.appendLine(`Failed to start language server: ${error}`);
        updateStatusBar(State.Stopped);
        vscode.window.showErrorMessage(
            `Failed to start PKL Language Server. Make sure 'rpkl-lsp' is installed and in your PATH. ` +
            `You can set a custom path in settings: pkl.server.path`
        );
    }
}

export async function deactivate(): Promise<void> {
    if (client) {
        await client.stop();
    }
}

function updateStatusBar(state: State): void {
    if (!statusBarItem) {
        return;
    }

    switch (state) {
        case State.Running:
            statusBarItem.text = '$(check) PKL';
            statusBarItem.tooltip = 'PKL Language Server: Running';
            statusBarItem.backgroundColor = undefined;
            break;
        case State.Starting:
            statusBarItem.text = '$(loading~spin) PKL';
            statusBarItem.tooltip = 'PKL Language Server: Starting...';
            statusBarItem.backgroundColor = undefined;
            break;
        case State.Stopped:
            statusBarItem.text = '$(error) PKL';
            statusBarItem.tooltip = 'PKL Language Server: Stopped. Click to view output.';
            statusBarItem.backgroundColor = new vscode.ThemeColor('statusBarItem.errorBackground');
            break;
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
        fs.accessSync(bundledPath, fs.constants.X_OK);
        return bundledPath;
    } catch {
        // Fall back to looking in PATH
        return 'rpkl-lsp';
    }
}
