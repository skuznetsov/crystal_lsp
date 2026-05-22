const vscode = require('vscode');
const { LanguageClient, TransportKind, Trace, State } = require('vscode-languageclient/node');
const fs = require('fs');
const path = require('path');
const os = require('os');

let client;
let lspLogChannel;
let statusItem;

function workspaceRoot() {
    return vscode.workspace.workspaceFolders?.[0]?.uri?.fsPath;
}

function expandConfiguredPath(rawPath, defaultBase) {
    if (typeof rawPath !== 'string' || rawPath.trim().length === 0) {
        return '';
    }

    const trimmed = rawPath.trim();
    let expanded = trimmed.startsWith('~')
        ? path.join(process.env.HOME || '', trimmed.slice(1))
        : trimmed;

    if (!path.isAbsolute(expanded)) {
        expanded = path.join(defaultBase || process.cwd(), expanded);
    }

    return expanded;
}

function executableNames(name) {
    if (process.platform !== 'win32') {
        return [name];
    }

    const extensions = (process.env.PATHEXT || '.EXE;.CMD;.BAT;.COM')
        .split(';')
        .filter((ext) => ext.length > 0);
    if (path.extname(name).length > 0) {
        return [name];
    }
    return extensions.map((ext) => `${name}${ext.toLowerCase()}`)
        .concat(extensions.map((ext) => `${name}${ext.toUpperCase()}`));
}

function findExecutableOnPath(name) {
    const pathEntries = (process.env.PATH || '').split(path.delimiter).filter((entry) => entry.length > 0);
    for (const entry of pathEntries) {
        for (const executableName of executableNames(name)) {
            const candidate = path.join(entry, executableName);
            try {
                fs.accessSync(candidate, fs.constants.X_OK);
                return candidate;
            } catch (_) {
                // Continue searching PATH.
            }
        }
    }
    return undefined;
}

function isExecutableFile(filePath) {
    try {
        const stat = fs.statSync(filePath);
        if (!stat.isFile()) {
            return false;
        }
        fs.accessSync(filePath, fs.constants.X_OK);
        return true;
    } catch (_) {
        return false;
    }
}

function resolveServerLaunch(config, workspaceBase) {
    const configuredServerPathRaw = config.get('lsp.serverPath') || '';
    const configuredServerPath = typeof configuredServerPathRaw === 'string' ? configuredServerPathRaw : '';
    const configuredServerArgs = config.get('lsp.serverArgs') || [];
    const userArgs = Array.isArray(configuredServerArgs)
        ? configuredServerArgs.filter((arg) => typeof arg === 'string')
        : [];

    if (configuredServerPath.trim().length > 0) {
        const command = expandConfiguredPath(configuredServerPath, workspaceBase);
        return {
            command,
            args: userArgs,
            source: 'settings',
            missing: !isExecutableFile(command)
        };
    }

    if (process.env.CRYSTAL_V2_LSP_SERVER && process.env.CRYSTAL_V2_LSP_SERVER.trim().length > 0) {
        const command = process.env.CRYSTAL_V2_LSP_SERVER.trim();
        return {
            command,
            args: userArgs,
            source: 'CRYSTAL_V2_LSP_SERVER',
            missing: !isExecutableFile(command)
        };
    }

    const compilerCandidates = ['crystal2', 'crystal_v2'];
    for (const candidateName of compilerCandidates) {
        const command = findExecutableOnPath(candidateName);
        if (command) {
            return {
                command,
                args: ['tool', 'lsp'].concat(userArgs),
                source: `PATH:${candidateName}`
            };
        }
    }

    const standalone = findExecutableOnPath('crystal_v2_lsp');
    if (standalone) {
        return {
            command: standalone,
            args: userArgs,
            source: 'PATH:crystal_v2_lsp'
        };
    }

    return undefined;
}

function activate(context) {
    console.log('Crystal V2 LSP extension is now active');

    // Server options
    // Allow user to configure debug logging via VS Code settings.
    // When crystalV2.lsp.debugLogPath is set, pass CRYSTALV2_LSP_CONFIG pointing to a temp JSON
    // so the server writes detailed logs (including semantic token samples) to that path.
    const config = vscode.workspace.getConfiguration('crystalv2');
    if (config.get('lsp.enabled') === false) {
        console.log('Crystal V2 LSP extension is disabled by configuration');
        return;
    }

    const serverLaunch = resolveServerLaunch(config, workspaceRoot());
    if (!serverLaunch) {
        const message = 'Crystal V2 LSP server not found. Install crystal2 with `crystal2 tool lsp`, or set crystalv2.lsp.serverPath.';
        vscode.window.showWarningMessage(message, 'Open Settings').then((selection) => {
            if (selection === 'Open Settings') {
                vscode.commands.executeCommand('workbench.action.openSettings', 'crystalv2.lsp.serverPath');
            }
        });
        console.warn(message);
        return;
    }
    if (serverLaunch.missing) {
        const message = `Crystal V2 LSP server path does not exist or is not executable: ${serverLaunch.command}`;
        vscode.window.showErrorMessage(message, 'Open Settings').then((selection) => {
            if (selection === 'Open Settings') {
                vscode.commands.executeCommand('workbench.action.openSettings', 'crystalv2.lsp.serverPath');
            }
        });
        console.error(message);
        return;
    }
    const serverPath = serverLaunch.command;
    const serverArgs = serverLaunch.args;
    const debugLogPathRaw = config.get('lsp.debugLogPath');

    const env = { ...process.env };
    if (debugLogPathRaw && debugLogPathRaw.trim().length > 0) {         
        // Inline JSON config via env var; server already understands CRYSTALV2_LSP_CONFIG
        const tmpConfigPath = path.join(os.tmpdir(), `crystal_v2_lsp_config_${process.pid}.json`);
        try {
            const base = path.dirname(debugLogPathRaw.trim()) === '.'
                ? path.join(workspaceRoot() || process.cwd(), 'logs')
                : workspaceRoot();
            let expanded = expandConfiguredPath(debugLogPathRaw, base);
            const dir = path.dirname(expanded);
            fs.mkdirSync(dir, { recursive: true });
            fs.writeFileSync(tmpConfigPath, JSON.stringify({ debug_log_path: expanded }));
            env['LSP_DEBUG_LOG'] = expanded; // legacy env for direct path
            env['CRYSTALV2_LSP_CONFIG'] = tmpConfigPath;
        } catch (err) {
            console.warn('Failed to write LSP debug config', err);
        }   
        env['LSP_DEBUG'] = '1';
    }

    const serverOptions = {
        command: serverPath,
        args: serverArgs,
        options: { env },
        transport: TransportKind.stdio
    };

    const traceChannel = vscode.window.createOutputChannel('Crystal V2 LSP Trace');
    lspLogChannel = vscode.window.createOutputChannel('Crystal V2 LSP Messages');
    const traceSetting = config.get('lsp.trace.server') || 'off';
    const traceMap = {
        off: Trace.Off,
        messages: Trace.Messages,
        verbose: Trace.Verbose,
    };
    const traceLevel = traceMap[traceSetting] ?? Trace.Off;
    traceChannel.appendLine(`[client] LSP server command: ${serverPath} ${serverArgs.join(' ')} (${serverLaunch.source})`);

    // Client options
    const clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'crystal' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.cr')
        },      
        traceOutputChannel: traceChannel,
        middleware: {
            didSendRequest: (data) => {
                if (data && data.type === 1) { // RequestMessage
                    try {
                        const body = JSON.parse(data.message);
                        lspLogChannel.appendLine(`--> ${body.method} (${body.id ?? "n/a"})`);
                    } catch (err) {
                        lspLogChannel.appendLine(`--> send (unparsed): ${String(data.message)}`);
                    }
                }
            }
        },
    };  

    // Create and start the language client
    client = new LanguageClient(
        'crystalv2-lsp',
        'Crystal V2 Language Server',
        serverOptions,
        clientOptions
    );

    if (typeof client.onDidChangeState === 'function') {
        client.onDidChangeState((event) => {
            if (event.newState === State.Running) {
                if (typeof client.setTrace === 'function') {
                    client.setTrace(traceLevel);
                } else {
                    try { client.trace = traceLevel; } catch (_) { /* noop */ }
                }
                traceChannel.appendLine(`[client] LSP trace set to ${traceSetting} (state change)`);
                if (statusItem) {
                    statusItem.text = 'Crystal V2 LSP: Ready';
                    statusItem.show();
                }
            }
        });
    } else {
        traceChannel.appendLine('[client] onDidChangeState unavailable; trace channel active but trace level may remain default');
    }

    client.onNotification('crystal/indexing', (params) => {
        const message = params && params.message ? params.message : 'Indexing…';
        if (!statusItem) {
            statusItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 10);
            statusItem.command = undefined;
        }
        statusItem.text = `Crystal V2 LSP: ${message}`;
        statusItem.show();
    });

    client.onNotification('crystal/indexed', () => {
        if (!statusItem) {
            statusItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 10);
        }
        statusItem.text = 'Crystal V2 LSP: Ready';
        statusItem.show();
    });

    client.onNotification((method, params) => {
        // Log all incoming notifications to the message channel
        lspLogChannel.appendLine(`<-- ${method}`);
    });

    // Start the client (this will also launch the server)
    client.start();

    console.log('Crystal V2 LSP client started');
}

function deactivate() {
    if (statusItem) {
        statusItem.dispose();
        statusItem = undefined;
    }
    if (lspLogChannel) {
        lspLogChannel.dispose();
        lspLogChannel = undefined;
    }
    if (!client) {
        return undefined;
    }
    return client.stop();
}

module.exports = {
    activate,
    deactivate
};
