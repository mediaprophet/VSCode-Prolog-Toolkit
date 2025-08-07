import type {
  CodeAction,
  CompletionItem,
  Definition,
  Diagnostic,
  DocumentHighlight,
  DocumentSymbol,
  FoldingRange,
  Hover,
  Location,
  Position,
  Range,
  SemanticTokens,
  SemanticTokensLegend,
  SignatureHelp,
  SymbolInformation,
  TextDocument,
  TextEdit,
  WorkspaceEdit,
} from 'vscode-languageserver';
import { PrologBackend } from '../../prologBackend.js';

// Configuration interfaces
export interface PrologSettings {
  executablePath: string;
  dialect: 'swi' | 'ecl';
  linter: {
    run: 'onSave' | 'onType' | 'never';
    delay: number;
    enableMsgInOutput: boolean;
  };
  format: {
    addSpace: boolean;
  };
  terminal: {
    runtimeArgs: string[];
  };
}

export const defaultSettings: PrologSettings = {
  executablePath: 'swipl',
  dialect: 'swi',
  linter: {
    run: 'onType',
    delay: 500,
    enableMsgInOutput: false,
  },
  format: {
    addSpace: true,
  },
  terminal: {
    runtimeArgs: [],
  },
};

// LSP Provider context interface
export interface LSPContext {
  prologBackend: PrologBackend | null;
  getDocumentSettings: (resource: string) => Promise<PrologSettings>;
  getGlobalSettings: () => Promise<PrologSettings>;
  documents: Map<string, TextDocument>;
  hasConfigurationCapability: boolean;
  hasWorkspaceFolderCapability: boolean;
}

// Provider interfaces
export interface ValidationProvider {
  validateTextDocument(textDocument: TextDocument, context: LSPContext): Promise<Diagnostic[]>;
}

export interface CompletionProvider {
  provideCompletions(
    document: TextDocument,
    position: Position,
    context: LSPContext
  ): Promise<CompletionItem[]>;
}

export interface HoverProvider {
  provideHover(
    document: TextDocument,
    position: Position,
    context: LSPContext
  ): Promise<Hover | null>;
}

export interface CodeActionProvider {
  provideCodeActions(
    document: TextDocument,
    range: Range,
    diagnostics: Diagnostic[],
    context: LSPContext
  ): Promise<CodeAction[]>;
}

export interface DefinitionProvider {
  provideDefinition(
    document: TextDocument,
    position: Position,
    context: LSPContext
  ): Promise<Definition | null>;
}

export interface SymbolProvider {
  provideDocumentSymbols(document: TextDocument, context: LSPContext): Promise<DocumentSymbol[]>;
  provideWorkspaceSymbols(
    query: string,
    documents: TextDocument[],
    context: LSPContext
  ): Promise<SymbolInformation[]>;
}

export interface ReferencesProvider {
  provideReferences(
    document: TextDocument,
    position: Position,
    includeDeclaration: boolean,
    context: LSPContext
  ): Promise<Location[]>;
  provideDocumentHighlights(
    document: TextDocument,
    position: Position,
    context: LSPContext
  ): Promise<DocumentHighlight[]>;
}

export interface SignatureProvider {
  provideSignatureHelp(
    document: TextDocument,
    position: Position,
    context: LSPContext
  ): Promise<SignatureHelp | null>;
}

export interface FormattingProvider {
  formatDocument(document: TextDocument, context: LSPContext): Promise<TextEdit[]>;
  formatRange(document: TextDocument, range: Range, context: LSPContext): Promise<TextEdit[]>;
}

export interface RenameProvider {
  prepareRename(
    document: TextDocument,
    position: Position,
    context: LSPContext
  ): Promise<Range | null>;
  provideRename(
    document: TextDocument,
    position: Position,
    newName: string,
    context: LSPContext
  ): Promise<WorkspaceEdit | null>;
}

export interface FoldingProvider {
  provideFoldingRanges(document: TextDocument, context: LSPContext): Promise<FoldingRange[]>;
}

export interface SemanticTokensProvider {
  provideSemanticTokens(document: TextDocument, context: LSPContext): Promise<SemanticTokens>;
}

export interface ExecuteCommandHandler {
  executeCommand(command: string, args: any[], context: LSPContext): Promise<any>;
}

// Semantic tokens legend
export const semanticTokensLegend: SemanticTokensLegend = {
  tokenTypes: [
    'namespace',
    'type',
    'class',
    'enum',
    'interface',
    'struct',
    'typeParameter',
    'parameter',
    'variable',
    'property',
    'enumMember',
    'event',
    'function',
    'method',
    'macro',
    'keyword',
    'modifier',
    'comment',
    'string',
    'number',
    'regexp',
    'operator',
    'decorator',
  ],
  tokenModifiers: [
    'declaration',
    'definition',
    'readonly',
    'static',
    'deprecated',
    'abstract',
    'async',
    'modification',
    'documentation',
    'defaultLibrary',
  ],
};

// Utility types
export interface PredicateInfo {
  name: string;
  arity: number;
  description?: string;
}

export interface N3CompletionInfo {
  name: string;
  detail?: string;
}

export interface HelpDocumentation {
  name: string;
  arity: number;
  summary?: string;
  args?: Array<{ name: string; description: string }>;
  examples?: string[];
}

export interface BackendResponse {
  status: 'ok' | 'error';
  results?: any[];
  completions?: any[];
  doc?: HelpDocumentation;
  locations?: Array<{ uri: string; line: number; character: number }>;
  errors?: Array<{ line?: number; column?: number; length?: number; message?: string }>;
  error?: string;
}
