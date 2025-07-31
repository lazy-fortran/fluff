import * as vscode from 'vscode';
import * as path from 'path';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
    console.log('Fluff Fortran extension is now active!');

    // Register commands
    registerCommands(context);
    
    // Start language server if available
    startLanguageServer(context);
    
    // Register document formatting provider
    registerFormattingProvider(context);
    
    // Setup file watchers
    setupFileWatchers(context);
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

function registerCommands(context: vscode.ExtensionContext) {
    // Check current file
    const checkCommand = vscode.commands.registerCommand('fluff.check', async (uri?: vscode.Uri) => {
        const targetUri = uri || vscode.window.activeTextEditor?.document.uri;
        if (!targetUri) {
            vscode.window.showErrorMessage('No Fortran file selected');
            return;
        }
        
        await runFluffCommand('check', [targetUri.fsPath]);
    });
    
    // Check workspace
    const checkWorkspaceCommand = vscode.commands.registerCommand('fluff.checkWorkspace', async () => {
        const workspaceFolder = vscode.workspace.workspaceFolders?.[0];
        if (!workspaceFolder) {
            vscode.window.showErrorMessage('No workspace folder found');
            return;
        }
        
        await runFluffCommand('check', [workspaceFolder.uri.fsPath]);
    });
    
    // Format current file
    const formatCommand = vscode.commands.registerCommand('fluff.format', async (uri?: vscode.Uri) => {
        const targetUri = uri || vscode.window.activeTextEditor?.document.uri;
        if (!targetUri) {
            vscode.window.showErrorMessage('No Fortran file selected');
            return;
        }
        
        await runFluffCommand('format', ['--fix', targetUri.fsPath]);
    });
    
    // Format workspace
    const formatWorkspaceCommand = vscode.commands.registerCommand('fluff.formatWorkspace', async () => {
        const workspaceFolder = vscode.workspace.workspaceFolders?.[0];
        if (!workspaceFolder) {
            vscode.window.showErrorMessage('No workspace folder found');
            return;
        }
        
        const result = await vscode.window.showWarningMessage(
            'This will format all Fortran files in the workspace. Continue?',
            'Yes', 'No'
        );
        
        if (result === 'Yes') {
            await runFluffCommand('format', ['--fix', workspaceFolder.uri.fsPath]);
        }
    });
    
    // Fix current file
    const fixCommand = vscode.commands.registerCommand('fluff.fix', async (uri?: vscode.Uri) => {
        const targetUri = uri || vscode.window.activeTextEditor?.document.uri;
        if (!targetUri) {
            vscode.window.showErrorMessage('No Fortran file selected');
            return;
        }
        
        await runFluffCommand('check', ['--fix', targetUri.fsPath]);
    });
    
    // Restart language server
    const restartCommand = vscode.commands.registerCommand('fluff.restart', async () => {
        if (client) {
            await client.stop();
            startLanguageServer(context);
            vscode.window.showInformationMessage('Fluff language server restarted');
        }
    });
    
    context.subscriptions.push(
        checkCommand,
        checkWorkspaceCommand,
        formatCommand,
        formatWorkspaceCommand,
        fixCommand,
        restartCommand
    );
}

async function runFluffCommand(command: string, args: string[]) {
    const config = vscode.workspace.getConfiguration('fluff');
    const executable = config.get<string>('executable', 'fluff');
    const configFile = config.get<string>('configFile', '');
    const showOutput = config.get<boolean>('showOutput', false);
    
    // Build command arguments
    const cmdArgs = [command];
    if (configFile) {
        cmdArgs.push('--config', configFile);
    }
    cmdArgs.push(...args);
    
    // Show progress
    return vscode.window.withProgress({
        location: vscode.ProgressLocation.Notification,
        title: `Running fluff ${command}...`,
        cancellable: true
    }, async (progress, token) => {
        try {
            const { spawn } = require('child_process');
            const process = spawn(executable, cmdArgs, {
                cwd: vscode.workspace.workspaceFolders?.[0]?.uri.fsPath
            });
            
            let stdout = '';
            let stderr = '';
            
            process.stdout.on('data', (data: Buffer) => {
                stdout += data.toString();
            });
            
            process.stderr.on('data', (data: Buffer) => {
                stderr += data.toString();
            });
            
            return new Promise<void>((resolve, reject) => {
                process.on('close', (code: number) => {
                    if (showOutput || code !== 0) {
                        const channel = vscode.window.createOutputChannel('Fluff');
                        channel.appendLine(`Command: ${executable} ${cmdArgs.join(' ')}`);
                        channel.appendLine(`Exit code: ${code}`);
                        channel.appendLine('--- STDOUT ---');
                        channel.appendLine(stdout);
                        channel.appendLine('--- STDERR ---');
                        channel.appendLine(stderr);
                        
                        if (code !== 0) {
                            channel.show();
                        }
                    }
                    
                    if (code === 0) {
                        vscode.window.showInformationMessage(`Fluff ${command} completed successfully`);
                        resolve();
                    } else {
                        vscode.window.showErrorMessage(`Fluff ${command} failed (exit code ${code})`);
                        reject(new Error(`Fluff ${command} failed`));
                    }
                });
                
                token.onCancellationRequested(() => {
                    process.kill();
                    reject(new Error('Operation cancelled'));
                });
            });
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to run fluff: ${error}`);
            throw error;
        }
    });
}

function startLanguageServer(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('fluff');
    const executable = config.get<string>('executable', 'fluff');
    
    // Check if fluff supports LSP mode
    const serverOptions: ServerOptions = {
        command: executable,
        args: ['lsp'],
        transport: TransportKind.stdio
    };
    
    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'fortran' },
            { scheme: 'file', language: 'fortran90' },
            { scheme: 'file', language: 'fortran-modern' },
            { scheme: 'file', pattern: '**/*.{f,f90,f95,f03,f08,F,F90,F95,F03,F08}' }
        ],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{f,f90,f95,f03,f08,F,F90,F95,F03,F08}'),
            configurationSection: 'fluff'
        }
    };
    
    try {
        client = new LanguageClient(
            'fluff-lsp',
            'Fluff Language Server',
            serverOptions,
            clientOptions
        );
        
        client.start();
        console.log('Fluff language server started');
    } catch (error) {
        console.log('Could not start fluff language server:', error);
        // LSP is optional, continue without it
    }
}

function registerFormattingProvider(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('fluff');
    
    if (!config.get<boolean>('formatting.provider', true)) {
        return;
    }
    
    const provider = vscode.languages.registerDocumentFormattingEditProvider(
        [
            { language: 'fortran' },
            { language: 'fortran90' },
            { language: 'fortran-modern' }
        ],
        {
            async provideDocumentFormattingEdits(document: vscode.TextDocument): Promise<vscode.TextEdit[]> {
                const config = vscode.workspace.getConfiguration('fluff');
                const executable = config.get<string>('executable', 'fluff');
                const configFile = config.get<string>('configFile', '');
                
                try {
                    const { spawn } = require('child_process');
                    const args = ['format'];
                    
                    if (configFile) {
                        args.push('--config', configFile);
                    }
                    
                    args.push('-'); // Read from stdin
                    
                    const process = spawn(executable, args);
                    
                    // Send document content to stdin
                    process.stdin.write(document.getText());
                    process.stdin.end();
                    
                    let stdout = '';
                    let stderr = '';
                    
                    process.stdout.on('data', (data: Buffer) => {
                        stdout += data.toString();
                    });
                    
                    process.stderr.on('data', (data: Buffer) => {
                        stderr += data.toString();
                    });
                    
                    return new Promise<vscode.TextEdit[]>((resolve, reject) => {
                        process.on('close', (code: number) => {
                            if (code === 0 && stdout.trim() !== document.getText().trim()) {
                                const fullRange = new vscode.Range(
                                    document.positionAt(0),
                                    document.positionAt(document.getText().length)
                                );
                                resolve([vscode.TextEdit.replace(fullRange, stdout)]);
                            } else if (code !== 0) {
                                console.error('Fluff formatting failed:', stderr);
                                resolve([]);
                            } else {
                                resolve([]);
                            }
                        });
                    });
                } catch (error) {
                    console.error('Failed to format with fluff:', error);
                    return [];
                }
            }
        }
    );
    
    context.subscriptions.push(provider);
}

function setupFileWatchers(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('fluff');
    
    // Watch for configuration changes
    const configWatcher = vscode.workspace.onDidChangeConfiguration(event => {
        if (event.affectsConfiguration('fluff')) {
            // Restart language server if config changed
            if (client) {
                client.stop().then(() => {
                    startLanguageServer(context);
                });
            }
        }
    });
    
    // Watch for file saves
    const saveWatcher = vscode.workspace.onDidSaveTextDocument(async document => {
        if (!isFortranFile(document)) {
            return;
        }
        
        const config = vscode.workspace.getConfiguration('fluff');
        
        // Run linting on save
        if (config.get<boolean>('linting.onSave', true)) {
            await runFluffCommand('check', [document.uri.fsPath]);
        }
        
        // Run formatting on save
        if (config.get<boolean>('formatting.onSave', false)) {
            await runFluffCommand('format', ['--fix', document.uri.fsPath]);
        }
    });
    
    context.subscriptions.push(configWatcher, saveWatcher);
}

function isFortranFile(document: vscode.TextDocument): boolean {
    const fortranExtensions = ['.f', '.f90', '.f95', '.f03', '.f08', '.F', '.F90', '.F95', '.F03', '.F08'];
    const ext = path.extname(document.fileName).toLowerCase();
    return fortranExtensions.includes(ext) || 
           ['fortran', 'fortran90', 'fortran-modern'].includes(document.languageId);
}