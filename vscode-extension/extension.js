const vscode = require('vscode');
const { LanguageClient, TransportKind } = require('vscode-languageclient/node');

let client;

function activate(context) {
    console.log('Crystal V2 LSP extension is now active');

    // Path to the LSP server executable
    const serverPath = context.asAbsolutePath('../bin/crystal_v2_lsp');

    // Server options
    // Allow user to configure debug logging via VS Code settings.
    // When crystalV2.lsp.debugLogPath is set, pass CRYSTALV2_LSP_CONFIG pointing to a temp JSON
    // so the server writes detailed logs (including semantic token samples) to that path.
    const config = vscode.workspace.getConfiguration('crystalV2');
    const debugLogPathRaw = config.get('lsp.debugLogPath');

    const env = { ...process.env };
    if (debugLogPathRaw && debugLogPathRaw.trim().length > 0) {
        // Inline JSON config via env var; server already understands CRYSTALV2_LSP_CONFIG
        const tmpConfigPath = `/tmp/crystal_v2_lsp_config_${process.pid}.json`;
        const fs = require('fs');
        const path = require('path');
        try {
            const expanded = debugLogPathRaw.startsWith('~')
                ? path.join(process.env.HOME || '', debugLogPathRaw.slice(1))
                : debugLogPathRaw;
            const dir = path.dirname(expanded);
            fs.mkdirSync(dir, { recursive: true });
            fs.writeFileSync(tmpConfigPath, JSON.stringify({ debug_log_path: debugLogPath }));
            env['LSP_DEBUG_LOG'] = expanded; // legacy env for direct path
            env['CRYSTALV2_LSP_CONFIG'] = tmpConfigPath;
        } catch (err) {
            console.warn('Failed to write LSP debug config', err);
        }
        env['LSP_DEBUG'] = '1';
    }

    const serverOptions = {
        command: serverPath,
        args: [],
        options: { env },
        transport: TransportKind.stdio
    };

    // Client options
    const clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'crystal' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.cr')
        }
    };

    // Create and start the language client
    client = new LanguageClient(
        'crystalv2-lsp',
        'Crystal V2 Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client (this will also launch the server)
    client.start();

    console.log('Crystal V2 LSP client started');
}

function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

module.exports = {
    activate,
    deactivate
};
