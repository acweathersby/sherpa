import * as path from 'path';
import {
    workspace as Workspace, window as Window, ExtensionContext, TextDocument, OutputChannel, WorkspaceFolder, Uri
} from 'vscode';

import {
    Executable,
    LanguageClient, LanguageClientOptions, ServerOptions, TransportKind
} from 'vscode-languageclient/node';

let defaultClient: LanguageClient;
const clients: Map<string, LanguageClient> = new Map();

let _sortedWorkspaceFolders: string[] | undefined;

function sortedWorkspaceFolders(): string[] {

    if (_sortedWorkspaceFolders === void 0) {
        _sortedWorkspaceFolders = Workspace.workspaceFolders ? Workspace.workspaceFolders.map(folder => {
            let result = folder.uri.toString();
            if (result.charAt(result.length - 1) !== '/') {
                result = result + '/';
            }
            return result;
        }).sort(
            (a, b) => {
                return a.length - b.length;
            }
        ) : [];
    }
    return _sortedWorkspaceFolders;
}
Workspace.onDidChangeWorkspaceFolders(() => _sortedWorkspaceFolders = undefined);

function getOuterMostWorkspaceFolder(folder: WorkspaceFolder): WorkspaceFolder {
    const sorted = sortedWorkspaceFolders();
    for (const element of sorted) {
        let uri = folder.uri.toString();
        if (uri.charAt(uri.length - 1) !== '/') {
            uri = uri + '/';
        }
        if (uri.startsWith(element)) {
            return Workspace.getWorkspaceFolder(Uri.parse(element))!;
        }
    }
    return folder;
}

export function activate(context: ExtensionContext) {

    console.log("HCG CLient Activated!");

    const module = context.asAbsolutePath(path.join('server', 'out', 'server.js'));
    const outputChannel: OutputChannel = Window.createOutputChannel('hcg-lsp-client');

    function didOpenTextDocument(document: TextDocument): void {

        // We are only interested in language mode text
        if (document.languageId !== 'hydrocarbon-grammar' || (document.uri.scheme !== 'file' && document.uri.scheme !== 'untitled')) {
            return;
        }

        const uri = document.uri.toString();
        const serverOptions: ServerOptions = {
            run: <Executable>{
                command: "/home/work/projects/lib_hctoolkit/bin/hc-ls",
                args: [uri],
                options: {
                    cwd: "",
                    detached: false,
                    shell: false
                },
                transport: TransportKind.stdio
            },
            debug: <Executable>{
                command: "/home/work/projects/lib_hctoolkit/bin/hc-ls",
                args: [uri],
                options: {
                    cwd: "",
                    detached: true,
                    shell: false
                },
                transport: TransportKind.stdio
            }
        };
        const clientOptions: LanguageClientOptions = {
            documentSelector: [
                { language: 'hydrocarbon-grammar' }
            ],
            diagnosticCollectionName: 'hcg-lsp-client',
            outputChannel: outputChannel
        };
        defaultClient = new LanguageClient('hcg-lsp-client', 'LSP Multi Server Example', serverOptions, clientOptions);
        defaultClient.start();
        return;
    }

    // Attach document open event with the current client
    Workspace.onDidOpenTextDocument(didOpenTextDocument);

    //Handle Existing Documents were opened before this client came online.
    Workspace.textDocuments.forEach(didOpenTextDocument);

    Workspace.onDidChangeWorkspaceFolders((event) => {
        for (const folder of event.removed) {
            const client = clients.get(folder.uri.toString());
            if (client) {
                clients.delete(folder.uri.toString());
                client.stop();
            }
        }
    });
}

export function deactivate(): Thenable<void> {
    const promises: Thenable<void>[] = [];
    if (defaultClient) {
        promises.push(defaultClient.stop());
    }
    for (const client of clients.values()) {
        promises.push(client.stop());
    }
    return Promise.all(promises).then(() => undefined);
}