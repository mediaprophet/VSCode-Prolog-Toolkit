// Export all linting-related modules and interfaces
export * from './codeActionProvider.js';
export * from './commandManager.js';
export * from './configurationManager.js';
export * from './diagnosticParser.js';
export * from './interfaces.js';
export * from './navigationProvider.js';
export * from './processExecutor.js';
export { default as PrologLinter } from './prologLinter.js';

// Re-export the main linter as default
export { default } from './prologLinter.js';
