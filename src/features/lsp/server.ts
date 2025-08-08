// src/features/lsp/server.ts
// Main entry point for the Prolog LSP server, modular provider registration

import { TextDocument } from 'vscode-languageserver-textdocument';
import {
  CodeActionKind,
  createConnection,
  ProposedFeatures,
  TextDocuments
} from 'vscode-languageserver/node';
import { PrologBackend } from '../../prologBackend';
import { ConfigurationManager } from './configurationManager';
import * as Providers from './index';
import { LSPContext, semanticTokensLegend } from './types';

const connection = createConnection(ProposedFeatures.all);
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);
const configurationManager = new ConfigurationManager(connection);

// State
let hasWorkspaceFolderCapability = false;
let prologBackend: PrologBackend | null = null;

const createLSPContext = (): LSPContext => ({
  prologBackend,
  getDocumentSettings: (resource: string) => configurationManager.getDocumentSettings(resource),
  getGlobalSettings: () => configurationManager.getGlobalSettings(),
  documents: new Map(documents.all().map(doc => [doc.uri, doc])),
  hasConfigurationCapability: configurationManager.getHasConfigurationCapability(),
  hasWorkspaceFolderCapability,
});

connection.onInitialize((params) => {
  const capabilities = params.capabilities;
  const hasConfigurationCapability = !!(capabilities.workspace && !!capabilities.workspace.configuration);
  configurationManager.setConfigurationCapability(hasConfigurationCapability);
  hasWorkspaceFolderCapability = !!(capabilities.workspace && !!capabilities.workspace.workspaceFolders);
  return {
    capabilities: Providers.getServerCapabilities(CodeActionKind, semanticTokensLegend, hasWorkspaceFolderCapability)
  };
});

connection.onInitialized(() => {
  configurationManager.registerForConfigurationChanges();
  if (hasWorkspaceFolderCapability) {
    connection.workspace.onDidChangeWorkspaceFolders(_event => {
      connection.console.log('Workspace folder change event received.');
    });
  }
  initializePrologBackend();
});

async function initializePrologBackend() {
  try {
    const settings = await configurationManager.getGlobalSettings();
    prologBackend = new PrologBackend({
      swiplPath: settings.executablePath,
      port: 3061,
      streamingEnabled: true,
      maxResultsPerChunk: 50,
    });
    prologBackend.on('ready', () => connection.console.log('Prolog backend ready for LSP server'));
    prologBackend.on('error', (error: any) => connection.console.error(`Prolog backend error: ${error}`));
    prologBackend.start();
  } catch (error) {
    connection.console.error(`Failed to initialize Prolog backend: ${error}`);
  }
}

// Register all providers
Providers.registerAll(connection, documents, createLSPContext);

documents.listen(connection);
connection.listen();

export { connection, documents, prologBackend };

