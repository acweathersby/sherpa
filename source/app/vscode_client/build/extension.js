"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.deactivate = exports.activate = void 0;
const path = require("path");
const vscode_1 = require("vscode");
const node_1 = require("vscode-languageclient/node");
let defaultClient;
const clients = new Map();
let _sortedWorkspaceFolders;
function sortedWorkspaceFolders() {
    if (_sortedWorkspaceFolders === void 0) {
        _sortedWorkspaceFolders = vscode_1.workspace.workspaceFolders ? vscode_1.workspace.workspaceFolders.map(folder => {
            let result = folder.uri.toString();
            if (result.charAt(result.length - 1) !== '/') {
                result = result + '/';
            }
            return result;
        }).sort((a, b) => {
            return a.length - b.length;
        }) : [];
    }
    return _sortedWorkspaceFolders;
}
vscode_1.workspace.onDidChangeWorkspaceFolders(() => _sortedWorkspaceFolders = undefined);
function getOuterMostWorkspaceFolder(folder) {
    const sorted = sortedWorkspaceFolders();
    for (const element of sorted) {
        let uri = folder.uri.toString();
        if (uri.charAt(uri.length - 1) !== '/') {
            uri = uri + '/';
        }
        if (uri.startsWith(element)) {
            return vscode_1.workspace.getWorkspaceFolder(vscode_1.Uri.parse(element));
        }
    }
    return folder;
}
function activate(context) {
    console.log("HCG CLient Activated!");
    const module = context.asAbsolutePath(path.join('server', 'out', 'server.js'));
    const outputChannel = vscode_1.window.createOutputChannel('hcg-lsp-client');
    function didOpenTextDocument(document) {
        // We are only interested in language mode text
        if (document.languageId !== 'hydrocarbon-grammar' || (document.uri.scheme !== 'file' && document.uri.scheme !== 'untitled')) {
            return;
        }
        const uri = document.uri.toString();
        const serverOptions = {
            run: {
                command: "/home/work/projects/lib_hctoolkit/bin/hc-ls",
                args: [uri],
                options: {
                    cwd: "",
                    detached: false,
                    shell: false
                },
                transport: node_1.TransportKind.stdio
            },
            debug: {
                command: "/home/work/projects/lib_hctoolkit/bin/hc-ls",
                args: [uri],
                options: {
                    cwd: "",
                    detached: true,
                    shell: false
                },
                transport: node_1.TransportKind.stdio
            }
        };
        const clientOptions = {
            documentSelector: [
                { language: 'hydrocarbon-grammar' }
            ],
            diagnosticCollectionName: 'hcg-lsp-client',
            outputChannel: outputChannel
        };
        defaultClient = new node_1.LanguageClient('hcg-lsp-client', 'LSP Multi Server Example', serverOptions, clientOptions);
        defaultClient.start();
        return;
    }
    // Attach document open event with the current client
    vscode_1.workspace.onDidOpenTextDocument(didOpenTextDocument);
    //Handle Existing Documents were opened before this client came online.
    vscode_1.workspace.textDocuments.forEach(didOpenTextDocument);
    vscode_1.workspace.onDidChangeWorkspaceFolders((event) => {
        for (const folder of event.removed) {
            const client = clients.get(folder.uri.toString());
            if (client) {
                clients.delete(folder.uri.toString());
                client.stop();
            }
        }
    });
}
exports.activate = activate;
function deactivate() {
    const promises = [];
    if (defaultClient) {
        promises.push(defaultClient.stop());
    }
    for (const client of clients.values()) {
        promises.push(client.stop());
    }
    return Promise.all(promises).then(() => undefined);
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map