
import { TextDocument } from 'vscode-languageserver-textdocument';
import type {
  CodeActionParams,
  DefinitionParams,
  DocumentFormattingParams,
  DocumentHighlightParams,
  DocumentRangeFormattingParams,
  DocumentSymbolParams,
  ExecuteCommandParams,
  FoldingRangeParams,
  HoverParams,
  InitializeParams,
  InitializeResult,
  PrepareRenameParams,
  ReferenceParams,
  RenameParams,
  SemanticTokensParams,
  SignatureHelpParams,
  TextDocumentPositionParams,
  WorkspaceSymbolParams
} from 'vscode-languageserver/node';
import { CodeActionKind, CompletionItem, createConnection, TextDocuments, TextDocumentSyncKind } from 'vscode-languageserver/node';
import { PrologBackend } from '../prologBackend.js';
import { PrologCodeActionsProvider } from './lsp/codeActionsProvider.js';
import { PrologCompletionProvider } from './lsp/completionProvider.js';
import { ConfigurationManager } from './lsp/configurationManager.js';
import { PrologDefinitionProvider } from './lsp/definitionProvider.js';
import { PrologExecuteCommandHandler } from './lsp/executeCommandHandler.js';
import { PrologFoldingProvider } from './lsp/foldingProvider.js';
import { PrologFormattingProvider } from './lsp/formattingProvider.js';
import { PrologHoverProvider } from './lsp/hoverProvider.js';
import { PrologReferencesProvider } from './lsp/referencesProvider.js';
import { PrologRenameProvider } from './lsp/renameProvider.js';
import { PrologSemanticTokensProvider, semanticTokensLegend } from './lsp/semanticTokensProvider.js';
import { PrologSignatureProvider } from './lsp/signatureProvider.js';
import { PrologSymbolProvider } from './lsp/symbolProvider.js';
import type { LSPContext } from './lsp/types.js';
import { PrologValidationProvider } from './lsp/validationProvider.js';

const connection = createConnection();
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

// Initialize all providers and managers
const configurationManager = new ConfigurationManager(connection);
const validationProvider = new PrologValidationProvider();
const completionProvider = new PrologCompletionProvider();
const hoverProvider = new PrologHoverProvider();
const codeActionsProvider = new PrologCodeActionsProvider();
const executeCommandHandler = new PrologExecuteCommandHandler();
const definitionProvider = new PrologDefinitionProvider();
const symbolProvider = new PrologSymbolProvider();
const referencesProvider = new PrologReferencesProvider();
const signatureProvider = new PrologSignatureProvider();
const formattingProvider = new PrologFormattingProvider();
const renameProvider = new PrologRenameProvider();
const foldingProvider = new PrologFoldingProvider();
const semanticTokensProvider = new PrologSemanticTokensProvider();

// State management
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;

// Prolog backend instance
let prologBackend: PrologBackend | null = null;

// Create LSP context
const createLSPContext = (): LSPContext => ({
  prologBackend,
  getDocumentSettings: (resource: string) => configurationManager.getDocumentSettings(resource),
  getGlobalSettings: () => configurationManager.getGlobalSettings(),
  documents: new Map(documents.all().map((doc: TextDocument) => [doc.uri, doc])),
  hasConfigurationCapability: configurationManager.getHasConfigurationCapability(),
  hasWorkspaceFolderCapability,
});



connection.onInitialize((params: InitializeParams) => {
  const capabilities = params.capabilities;

  // Set configuration capability
  const hasConfigurationCapability = !!(
    capabilities.workspace && !!capabilities.workspace.configuration
  );
  configurationManager.setConfigurationCapability(hasConfigurationCapability);

  hasWorkspaceFolderCapability = !!(
    capabilities.workspace && !!capabilities.workspace.workspaceFolders
  );
  hasDiagnosticRelatedInformationCapability = !!(
    capabilities.textDocument?.publishDiagnostics &&
    capabilities.textDocument.publishDiagnostics.relatedInformation
  );

  const result: InitializeResult = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      completionProvider: {
        resolveProvider: true,
        triggerCharacters: ['(', ',', ' ', '.', ':', '-', '_'],
      },
      hoverProvider: true,
      codeActionProvider: {
        codeActionKinds: [
          CodeActionKind.QuickFix,
          CodeActionKind.Refactor,
          CodeActionKind.Source,
          CodeActionKind.SourceOrganizeImports,
        ],
      },
      executeCommandProvider: {
        commands: [
          'prolog.executeQuery',
          'prolog.consultFile',
          'prolog.getHelp',
          'prolog.runN3Diagnostics',
          'prolog.formatDocument',
          'prolog.organizeImports',
        ],
      },
      definitionProvider: true,
      documentSymbolProvider: true,
      workspaceSymbolProvider: true,
      referencesProvider: true,
      documentHighlightProvider: true,
      signatureHelpProvider: {
        triggerCharacters: ['(', ','],
      },
      documentFormattingProvider: true,
      documentRangeFormattingProvider: true,
      renameProvider: {
        prepareProvider: true,
      },
      foldingRangeProvider: true,
      semanticTokensProvider: {
        legend: semanticTokensLegend,
        range: true,
        full: {
          delta: true,
        },
      },
      colorProvider: true,
      callHierarchyProvider: true,
      linkedEditingRangeProvider: true,
      monikerProvider: true,
      typeDefinitionProvider: true,
      implementationProvider: true,
      declarationProvider: true,
      selectionRangeProvider: true,
    },
  };

  if (hasWorkspaceFolderCapability) {
    result.capabilities.workspace = {
      workspaceFolders: {
        supported: true,
      },
    };
  }

  return result;
});

connection.onInitialized(() => {
  configurationManager.registerForConfigurationChanges();

  if (hasWorkspaceFolderCapability) {
    connection.workspace.onDidChangeWorkspaceFolders((_event: any) => {
      connection.console.log('Workspace folder change event received.');
    });
  }

  // Initialize Prolog backend
  initializePrologBackend();
});

async function initializePrologBackend() {
  try {
    const settings = await configurationManager.getGlobalSettings();
    prologBackend = new PrologBackend({
      swiplPath: settings.executablePath,
      port: 3061, // Different port for LSP server
      streamingEnabled: true,
      maxResultsPerChunk: 50,
    });

    prologBackend.on('ready', () => {
      connection.console.log('Prolog backend ready for LSP server');
    });

    prologBackend.on('error', error => {
      connection.console.error(`Prolog backend error: ${error}`);
    });

    prologBackend.start();
  } catch (error: unknown) {
    connection.console.error(`Failed to initialize Prolog backend: ${error}`);
  }
}

// Configuration changes
connection.onDidChangeConfiguration((change: any) => {
  configurationManager.onDidChangeConfiguration(change);

  // Revalidate all open text documents
  documents.all().forEach(validateTextDocument);
});

// Document lifecycle
documents.onDidClose((e: any) => {
  configurationManager.clearDocumentSettings(e.document.uri);
});

documents.onDidChangeContent((change: any) => {
  validateTextDocument(change.document);
});

async function validateTextDocument(textDocument: TextDocument): Promise<void> {
  try {
    const context = createLSPContext();
    const diagnostics = await validationProvider.validateTextDocument(textDocument, context);
    connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
  } catch (error: unknown) {
    connection.console.error(`Validation error: ${error}`);
  }
}

// LSP feature implementations

// Completion provider
connection.onCompletion(
  async (_textDocumentPosition: TextDocumentPositionParams): Promise<CompletionItem[]> => {
    const document = documents.get(_textDocumentPosition.textDocument.uri);
    if (!document) {
      return [];
    }

    try {
      const context = createLSPContext();
      return await completionProvider.provideCompletions(
        document,
        _textDocumentPosition.position,
        context
      );
    } catch (error: unknown) {
      connection.console.error(`Completion error: ${error}`);
      return [];
    }
  }
);

connection.onCompletionResolve((item: CompletionItem): CompletionItem => {
  // Add more detailed documentation if needed
  return item;
});

// Hover provider
connection.onHover(async (params: HoverParams) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) {
    return null;
  }

  try {
    const context = createLSPContext();
    return await hoverProvider.provideHover(document, params.position, context);
  } catch (error: unknown) {
    connection.console.error(`Hover error: ${error}`);
    return null;
  }
});

// Code actions provider
connection.onCodeAction(async (params: CodeActionParams) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) {
    return [];
  }

  try {
    const context = createLSPContext();
    return await codeActionsProvider.provideCodeActions(
      document,
      params.range,
      params.context.diagnostics,
      context
    );
  } catch (error: unknown) {
    connection.console.error(`Code actions error: ${error}`);
    return [];
  }
});

// Execute command handler
connection.onExecuteCommand(async (params: ExecuteCommandParams) => {
  try {
    const context = createLSPContext();
    const result = await executeCommandHandler.executeCommand(
      params.command,
      params.arguments || [],
      context
    );

    // Send appropriate notifications based on result
    if (result.success) {
      connection.window.showInformationMessage(result.message);
    } else {
      connection.window.showErrorMessage(result.message);
    }

    return result;
  } catch (error: unknown) {
    connection.console.error(`Execute command error: ${error}`);
    connection.window.showErrorMessage(`Command execution failed: ${error}`);
    return null;
  }
});

// Definition provider
connection.onDefinition(async (params: DefinitionParams) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) {
    return null;
  }

  try {
    const context = createLSPContext();
    return await definitionProvider.provideDefinition(document, params.position, context);
  } catch (error: unknown) {
    connection.console.error(`Definition error: ${error}`);
    return null;
  }
});

// Document symbol provider
connection.onDocumentSymbol(async (params: DocumentSymbolParams) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) {
    return [];
  }

  try {
    const context = createLSPContext();
    return await symbolProvider.provideDocumentSymbols(document, context);
  } catch (error: unknown) {
    connection.console.error(`Document symbols error: ${error}`);
    return [];
  }
});

// Workspace symbol provider
connection.onWorkspaceSymbol(async (params: WorkspaceSymbolParams) => {
  try {
    const context = createLSPContext();
    const allDocuments = documents.all();
    return await symbolProvider.provideWorkspaceSymbols(params.query, allDocuments, context);
  } catch (error: unknown) {
    connection.console.error(`Workspace symbols error: ${error}`);
    return [];
  }
});

// References provider
connection.onReferences(async (params: ReferenceParams) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) {
    return [];
  }

  try {
    const context = createLSPContext();
    return await referencesProvider.provideReferences(
      document,
      params.position,
      params.context.includeDeclaration,
      context
    );
  } catch (error: unknown) {
    connection.console.error(`References error: ${error}`);
    return [];
  }
});

// Document highlight provider
connection.onDocumentHighlight(async (params: DocumentHighlightParams) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) {
    return [];
  }

  try {
    const context = createLSPContext();
    return await referencesProvider.provideDocumentHighlights(document, params.position, context);
  } catch (error: unknown) {
    connection.console.error(`Document highlight error: ${error}`);
    return [];
  }
});

// Signature help provider
connection.onSignatureHelp(async (params: SignatureHelpParams) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) {
    return null;
  }

  try {
    const context = createLSPContext();
    return await signatureProvider.provideSignatureHelp(document, params.position, context);
  } catch (error: unknown) {
    connection.console.error(`Signature help error: ${error}`);
    return null;
  }
});

// Document formatting provider
connection.onDocumentFormatting(async (params: DocumentFormattingParams) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) {
    return [];
  }

  try {
    const context = createLSPContext();
    return await formattingProvider.formatDocument(document, context);
  } catch (error: unknown) {
    connection.console.error(`Document formatting error: ${error}`);
    return [];
  }
});

connection.onDocumentRangeFormatting(async (params: DocumentRangeFormattingParams) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) {
    return [];
  }

  try {
    const context = createLSPContext();
    return await formattingProvider.formatRange(document, params.range, context);
  } catch (error: unknown) {
    connection.console.error(`Document range formatting error: ${error}`);
    return [];
  }
});

// Rename provider
connection.onPrepareRename(async (params: PrepareRenameParams) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) {
    return null;
  }

  try {
    const context = createLSPContext();
    return await renameProvider.prepareRename(document, params.position, context);
  } catch (error: unknown) {
    connection.console.error(`Prepare rename error: ${error}`);
    return null;
  }
});

connection.onRenameRequest(async (params: RenameParams) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) {
    return null;
  }

  try {
    const context = createLSPContext();
    return await renameProvider.provideRename(document, params.position, params.newName, context);
  } catch (error: unknown) {
    connection.console.error(`Rename error: ${error}`);
    return null;
  }
});

// Folding range provider
connection.onFoldingRanges(async (params: FoldingRangeParams) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) {
    return [];
  }

  try {
    const context = createLSPContext();
    return await foldingProvider.provideFoldingRanges(document, context);
  } catch (error: unknown) {
    connection.console.error(`Folding ranges error: ${error}`);
    return [];
  }
});

// Semantic tokens provider
connection.onRequest('textDocument/semanticTokens/full', async (params: SemanticTokensParams) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) {
    return { data: [] };
  }

  try {
    const context = createLSPContext();
    return await semanticTokensProvider.provideSemanticTokens(document, context);
  } catch (error: unknown) {
    connection.console.error(`Semantic tokens error: ${error}`);
    return { data: [] };
  }
});

// Make the text document manager listen on the connection
documents.listen(connection);

// Listen on the connection
connection.listen();

export { connection, documents, prologBackend };
