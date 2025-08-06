// Import all providers for internal use
import { ConfigurationManager } from './configurationManager';
import { PrologValidationProvider } from './validationProvider';
import { PrologCompletionProvider } from './completionProvider';
import { PrologHoverProvider } from './hoverProvider';
import { PrologCodeActionsProvider } from './codeActionsProvider';
import { PrologExecuteCommandHandler } from './executeCommandHandler';
import { PrologDefinitionProvider } from './definitionProvider';
import { PrologSymbolProvider } from './symbolProvider';
import { PrologReferencesProvider } from './referencesProvider';
import { PrologSignatureProvider } from './signatureProvider';
import { PrologFormattingProvider } from './formattingProvider';
import { PrologRenameProvider } from './renameProvider';
import { PrologFoldingProvider } from './foldingProvider';
import { PrologSemanticTokensProvider } from './semanticTokensProvider';

// Export all types and interfaces
export * from './types';

// Export all providers
export { ConfigurationManager } from './configurationManager';
export { PrologValidationProvider } from './validationProvider';
export { PrologCompletionProvider } from './completionProvider';
export { PrologHoverProvider } from './hoverProvider';
export { PrologCodeActionsProvider } from './codeActionsProvider';
export { PrologExecuteCommandHandler } from './executeCommandHandler';
export { PrologDefinitionProvider } from './definitionProvider';
export { PrologSymbolProvider } from './symbolProvider';
export { PrologReferencesProvider } from './referencesProvider';
export { PrologSignatureProvider } from './signatureProvider';
export { PrologFormattingProvider } from './formattingProvider';
export { PrologRenameProvider } from './renameProvider';
export { PrologFoldingProvider } from './foldingProvider';
export { PrologSemanticTokensProvider } from './semanticTokensProvider';

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