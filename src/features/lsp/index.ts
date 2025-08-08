// --- Modular LSP server registration and capabilities ---
import {
  CodeActionParams,
  DefinitionParams,
  DocumentFormattingParams,
  DocumentHighlightParams,
  DocumentRangeFormattingParams,
  DocumentSymbolParams,
  ExecuteCommandParams,
  FoldingRangeParams,
  HoverParams,
  PrepareRenameParams,
  ReferenceParams,
  RenameParams,
  SemanticTokensParams,
  ServerCapabilities,
  SignatureHelpParams,
  TextDocuments,
  TextDocumentSyncKind,
  WorkspaceSymbolParams
} from 'vscode-languageserver/node';

// Returns the full capabilities object for the LSP server
export function getServerCapabilities(CodeActionKind: any, semanticTokensLegend: any, hasWorkspaceFolderCapability: boolean): ServerCapabilities {
  const base = {
    textDocumentSync: TextDocumentSyncKind.Incremental as typeof TextDocumentSyncKind.Incremental,
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
  };
  if (hasWorkspaceFolderCapability) {
    return {
      ...base,
      workspace: {
        workspaceFolders: {
          supported: true,
        },
      },
    };
  }
  return base;
}

// Registers all LSP providers with the connection
export function registerAll(connection: any, documents: TextDocuments<any>, createLSPContext: () => any) {
  const providers = createLSPProviders();

  // Validation on content change
  documents.onDidChangeContent(change => {
    validateTextDocument(change.document);
  });
  async function validateTextDocument(textDocument: any) {
    try {
      const context = createLSPContext();
      const diagnostics = await providers.validationProvider.validateTextDocument(textDocument, context);
      connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
    } catch (error: any) {
      connection.console.error(`Validation error: ${error}`);
    }
  }

  // Completion
  connection.onCompletion(async (params: any) => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return [];
    try {
      const context = createLSPContext();
      return await providers.completionProvider.provideCompletions(document, params.position, context);
    } catch (error: any) {
      connection.console.error(`Completion error: ${error}`);
      return [];
    }
  });

  // Hover
  connection.onHover(async (params: HoverParams) => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return null;
    try {
      const context = createLSPContext();
      return await providers.hoverProvider.provideHover(document, params.position, context);
    } catch (error: any) {
      connection.console.error(`Hover error: ${error}`);
      return null;
    }
  });

  // Code Actions
  connection.onCodeAction(async (params: CodeActionParams) => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return [];
    try {
      const context = createLSPContext();
      return await providers.codeActionsProvider.provideCodeActions(document, params.range, params.context.diagnostics, context);
    } catch (error: any) {
      connection.console.error(`Code actions error: ${error}`);
      return [];
    }
  });

  // Execute Command
  connection.onExecuteCommand(async (params: ExecuteCommandParams) => {
    try {
      const context = createLSPContext();
      const result = await providers.executeCommandHandler.executeCommand(params.command, params.arguments || [], context);
      if (result.success) {
        connection.window.showInformationMessage(result.message);
      } else {
        connection.window.showErrorMessage(result.message);
      }
      return result;
    } catch (error: any) {
      connection.console.error(`Execute command error: ${error}`);
      connection.window.showErrorMessage(`Command execution failed: ${error}`);
      return null;
    }
  });

  // Definition
  connection.onDefinition(async (params: DefinitionParams) => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return null;
    try {
      const context = createLSPContext();
      return await providers.definitionProvider.provideDefinition(document, params.position, context);
    } catch (error: any) {
      connection.console.error(`Definition error: ${error}`);
      return null;
    }
  });

  // Document Symbol
  connection.onDocumentSymbol(async (params: DocumentSymbolParams) => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return [];
    try {
      const context = createLSPContext();
      return await providers.symbolProvider.provideDocumentSymbols(document, context);
    } catch (error: any) {
      connection.console.error(`Document symbols error: ${error}`);
      return [];
    }
  });

  // Workspace Symbol
  connection.onWorkspaceSymbol(async (params: WorkspaceSymbolParams) => {
    try {
      const context = createLSPContext();
      const allDocuments = documents.all();
      return await providers.symbolProvider.provideWorkspaceSymbols(params.query, allDocuments, context);
    } catch (error: any) {
      connection.console.error(`Workspace symbols error: ${error}`);
      return [];
    }
  });

  // References
  connection.onReferences(async (params: ReferenceParams) => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return [];
    try {
      const context = createLSPContext();
      return await providers.referencesProvider.provideReferences(document, params.position, params.context.includeDeclaration, context);
    } catch (error: any) {
      connection.console.error(`References error: ${error}`);
      return [];
    }
  });

  // Document Highlight
  connection.onDocumentHighlight(async (params: DocumentHighlightParams) => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return [];
    try {
      const context = createLSPContext();
      return await providers.referencesProvider.provideDocumentHighlights(document, params.position, context);
    } catch (error: any) {
      connection.console.error(`Document highlight error: ${error}`);
      return [];
    }
  });

  // Signature Help
  connection.onSignatureHelp(async (params: SignatureHelpParams) => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return null;
    try {
      const context = createLSPContext();
      return await providers.signatureProvider.provideSignatureHelp(document, params.position, context);
    } catch (error: any) {
      connection.console.error(`Signature help error: ${error}`);
      return null;
    }
  });

  // Document Formatting
  connection.onDocumentFormatting(async (params: DocumentFormattingParams) => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return [];
    try {
      const context = createLSPContext();
      return await providers.formattingProvider.formatDocument(document, context);
    } catch (error: any) {
      connection.console.error(`Document formatting error: ${error}`);
      return [];
    }
  });

  connection.onDocumentRangeFormatting(async (params: DocumentRangeFormattingParams) => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return [];
    try {
      const context = createLSPContext();
      return await providers.formattingProvider.formatRange(document, params.range, context);
    } catch (error: any) {
      connection.console.error(`Document range formatting error: ${error}`);
      return [];
    }
  });

  // Rename
  connection.onPrepareRename(async (params: PrepareRenameParams) => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return null;
    try {
      const context = createLSPContext();
      return await providers.renameProvider.prepareRename(document, params.position, context);
    } catch (error: any) {
      connection.console.error(`Prepare rename error: ${error}`);
      return null;
    }
  });

  connection.onRenameRequest(async (params: RenameParams) => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return null;
    try {
      const context = createLSPContext();
      return await providers.renameProvider.provideRename(document, params.position, params.newName, context);
    } catch (error: any) {
      connection.console.error(`Rename error: ${error}`);
      return null;
    }
  });

  // Folding Range
  connection.onFoldingRanges(async (params: FoldingRangeParams) => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return [];
    try {
      const context = createLSPContext();
      return await providers.foldingProvider.provideFoldingRanges(document, context);
    } catch (error: any) {
      connection.console.error(`Folding ranges error: ${error}`);
      return [];
    }
  });

  // Semantic Tokens
  connection.onRequest('textDocument/semanticTokens/full', async (params: SemanticTokensParams) => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return { data: [] };
    try {
      const context = createLSPContext();
      return await providers.semanticTokensProvider.provideSemanticTokens(document, context);
    } catch (error: any) {
      connection.console.error(`Semantic tokens error: ${error}`);
      return { data: [] };
    }
  });

  // Document close: clear settings
  documents.onDidClose(e => {
    // If you have per-document settings, clear them here
    // configurationManager.clearDocumentSettings(e.document.uri);
  });
}
// Import all providers for internal use
import { PrologCodeActionsProvider } from './codeActionsProvider';
import { PrologCompletionProvider } from './completionProvider';
import { PrologDefinitionProvider } from './definitionProvider';
import { PrologExecuteCommandHandler } from './executeCommandHandler';
import { PrologFoldingProvider } from './foldingProvider';
import { PrologFormattingProvider } from './formattingProvider';
import { PrologHoverProvider } from './hoverProvider';
import { PrologReferencesProvider } from './referencesProvider';
import { PrologRenameProvider } from './renameProvider';
import { PrologSemanticTokensProvider } from './semanticTokensProvider';
import { PrologSignatureProvider } from './signatureProvider';
import { PrologSymbolProvider } from './symbolProvider';
import { PrologValidationProvider } from './validationProvider';

// Export all types and interfaces
export * from './types';

// Export all providers
export { PrologCodeActionsProvider } from './codeActionsProvider';
export { PrologCompletionProvider } from './completionProvider';
export { ConfigurationManager } from './configurationManager';
export { PrologDefinitionProvider } from './definitionProvider';
export { PrologExecuteCommandHandler } from './executeCommandHandler';
export { PrologFoldingProvider } from './foldingProvider';
export { PrologFormattingProvider } from './formattingProvider';
export { PrologHoverProvider } from './hoverProvider';
export { PrologReferencesProvider } from './referencesProvider';
export { PrologRenameProvider } from './renameProvider';
export { PrologSemanticTokensProvider } from './semanticTokensProvider';
export { PrologSignatureProvider } from './signatureProvider';
export { PrologSymbolProvider } from './symbolProvider';
export { PrologValidationProvider } from './validationProvider';

// Convenience factory function to create all providers
export function createLSPProviders() {
  return {
    validationProvider: new PrologValidationProvider(),
    completionProvider: new PrologCompletionProvider(),
    hoverProvider: new PrologHoverProvider(),
    codeActionsProvider: new PrologCodeActionsProvider(),
    executeCommandHandler: new PrologExecuteCommandHandler(),
    definitionProvider: new PrologDefinitionProvider(),
    symbolProvider: new PrologSymbolProvider(),
    referencesProvider: new PrologReferencesProvider(),
    signatureProvider: new PrologSignatureProvider(),
    formattingProvider: new PrologFormattingProvider(),
    renameProvider: new PrologRenameProvider(),
    foldingProvider: new PrologFoldingProvider(),
    semanticTokensProvider: new PrologSemanticTokensProvider(),
  };
}

// Version information
export const LSP_MODULE_VERSION = '1.0.0';