// Export all linting-related modules and interfaces
export * from './interfaces';
export * from './configurationManager';
export * from './processExecutor';
export * from './diagnosticParser';
export * from './codeActionProvider';
export * from './navigationProvider';
export * from './commandManager';
export { default as PrologLinter } from './prologLinter';

// Re-export the main linter as default
export { default } from './prologLinter';