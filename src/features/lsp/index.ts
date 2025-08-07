// Import all providers for internal use
import { PrologCodeActionsProvider } from './codeActionsProvider.js';
import { PrologCompletionProvider } from './completionProvider.js';
import { PrologDefinitionProvider } from './definitionProvider.js';
import { PrologExecuteCommandHandler } from './executeCommandHandler.js';
import { PrologFoldingProvider } from './foldingProvider.js';
import { PrologFormattingProvider } from './formattingProvider.js';
import { PrologHoverProvider } from './hoverProvider.js';
import { PrologReferencesProvider } from './referencesProvider.js';
import { PrologRenameProvider } from './renameProvider.js';
import { PrologSemanticTokensProvider } from './semanticTokensProvider.js';
import { PrologSignatureProvider } from './signatureProvider.js';
import { PrologSymbolProvider } from './symbolProvider.js';
import { PrologValidationProvider } from './validationProvider.js';

// Export all types and interfaces
export * from './types.js';

// Export all providers
export { PrologCodeActionsProvider } from './codeActionsProvider.js';
export { PrologCompletionProvider } from './completionProvider.js';
export { ConfigurationManager } from './configurationManager.js';
export { PrologDefinitionProvider } from './definitionProvider.js';
export { PrologExecuteCommandHandler } from './executeCommandHandler.js';
export { PrologFoldingProvider } from './foldingProvider.js';
export { PrologFormattingProvider } from './formattingProvider.js';
export { PrologHoverProvider } from './hoverProvider.js';
export { PrologReferencesProvider } from './referencesProvider.js';
export { PrologRenameProvider } from './renameProvider.js';
export { PrologSemanticTokensProvider } from './semanticTokensProvider.js';
export { PrologSignatureProvider } from './signatureProvider.js';
export { PrologSymbolProvider } from './symbolProvider.js';
export { PrologValidationProvider } from './validationProvider.js';

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
