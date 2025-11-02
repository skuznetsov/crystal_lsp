const vscode = require('vscode');
const { LanguageClient, TransportKind } = require('vscode-languageclient/node');

let client;

function activate(context) {
    console.log('Crystal V2 LSP extension is now active');

    // Path to the LSP server executable
    const serverPath = context.asAbsolutePath('../bin/crystal_v2_lsp');

    // Server options
    const serverOptions = {
        command: serverPath,
        args: [],
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
