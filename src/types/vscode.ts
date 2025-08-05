/**
 * VSCode-specific type definitions for extension functionality
 */

import {
  ExtensionContext,
  TextDocument,
  Position,
  Range,
  Location,
  Hover,
  DocumentHighlight,
  DocumentHighlightKind,
  CompletionItem,
  CompletionItemKind,
  CodeAction,
  CodeActionKind,
  Diagnostic,
  DiagnosticSeverity,
  DiagnosticRelatedInformation,
  TextEdit,
  WorkspaceEdit,
  ChatRequest,
  ChatResponseStream,
  CancellationToken,
  ChatResult,
  Terminal,
  TerminalExitStatus,
  TextEditor,
  Uri,
  MarkdownString,
  MarkedString,
  SnippetString,
  Command
} from 'vscode';

// Extension Context and Lifecycle
export interface ExtensionState {
  context: ExtensionContext;
  isActive: boolean;
  activatedAt?: Date;
  version: string;
  configuration: ExtensionConfiguration;
}

export interface ExtensionConfiguration {
  executablePath: string;
  dialect: 'swi' | 'ecl';
  linter: {
    run: 'onSave' | 'onType' | 'never';
    delay: number;
    enableMsgInOutput: boolean;
  };
  format: {
    enabled: boolean;
    addSpace: boolean;
  };
  terminal: {
    runtimeArgs: string[];
  };
  telemetry: {
    enabled: boolean;
  };
  apiServer: {
    enabled: boolean;
    port: number;
    host: string;
    corsOrigins: string[];
    maxConnections: number;
    requestTimeout: number;
    rateLimiting: {
      enabled: boolean;
      requestsPerMinute: number;
      burstLimit: number;
    };
    auth: {
      method: 'local_only' | 'api_key' | 'jwt_token' | 'oauth2';
      apiKeys: Record<string, {
        role: 'admin' | 'agent' | 'readonly' | 'limited';
        permissions: string[];
      }>;
      jwtSecret: string;
      jwtExpiration: string;
    };
  };
  webSocketServer: {
    enabled: boolean;
    port: number;
    maxConnections: number;
    heartbeatInterval: number;
  };
}

// Language Provider Types
export interface PrologDocument {
  document: TextDocument;
  uri: Uri;
  languageId: 'prolog';
  version: number;
  getText(): string;
  getWordRangeAtPosition(position: Position): Range | undefined;
  lineAt(line: number): {
    text: string;
    range: Range;
    rangeIncludingLineBreak: Range;
    firstNonWhitespaceCharacterIndex: number;
    isEmptyOrWhitespace: boolean;
  };
}

export interface PrologPosition extends Position {
  line: number;
  character: number;
}

export interface PrologRange extends Range {
  start: PrologPosition;
  end: PrologPosition;
}

export interface PrologLocation extends Location {
  uri: Uri;
  range: PrologRange;
}

// Hover Provider Types
export interface PrologHoverInfo {
  predicate: string;
  arity: number;
  module?: string;
  documentation?: string;
  signature?: string;
  examples?: string[];
  seeAlso?: string[];
  isBuiltin: boolean;
  isDeterministic?: boolean;
}

export interface PrologHover extends Hover {
  contents: (MarkdownString | MarkedString)[];
  range?: PrologRange;
}

// Definition Provider Types
export interface PrologDefinition {
  predicate: string;
  arity: number;
  file: string;
  line: number;
  column: number;
  clause?: string;
  isExported?: boolean;
  module?: string;
}

// Reference Provider Types
export interface PrologReference {
  predicate: string;
  arity: number;
  file: string;
  line: number;
  column: number;
  context: 'definition' | 'call' | 'export' | 'import';
  clause?: string;
}

// Document Highlight Provider Types
export interface PrologDocumentHighlight extends DocumentHighlight {
  range: PrologRange;
  kind?: DocumentHighlightKind;
}

// Completion Provider Types
export interface PrologCompletionItem extends CompletionItem {
  label: string;
  kind: CompletionItemKind;
  detail?: string;
  documentation?: string | MarkdownString;
  insertText?: string | SnippetString;
  predicate?: string;
  arity?: number;
  module?: string;
  isBuiltin?: boolean;
}

// Code Action Provider Types
export interface PrologCodeAction extends CodeAction {
  title: string;
  kind?: CodeActionKind;
  diagnostics?: Diagnostic[];
  edit?: WorkspaceEdit;
  command?: Command;
}

// Diagnostic Types
export interface PrologDiagnostic extends Diagnostic {
  range: PrologRange;
  message: string;
  severity: DiagnosticSeverity;
  source?: string;
  code?: string | number;
  relatedInformation?: DiagnosticRelatedInformation[];
}

// Formatter Types
export interface PrologFormattingOptions {
  tabSize: number;
  insertSpaces: boolean;
  addSpaceAfterCommas: boolean;
  indentClauses: boolean;
  alignParameters: boolean;
  maxLineLength?: number;
}

export interface PrologTextEdit extends TextEdit {
  range: PrologRange;
  newText: string;
}

// Linter Types
export interface PrologLinterOptions {
  trigger: 'onSave' | 'onType' | 'never';
  delay: number;
  enableOutput: boolean;
  executablePath: string;
  additionalArgs: string[];
}

export interface PrologLinterResult {
  diagnostics: PrologDiagnostic[];
  success: boolean;
  output?: string;
  error?: string;
  executionTime: number;
}

// Terminal Types
export interface PrologTerminalOptions {
  name: string;
  shellPath: string;
  shellArgs: string[];
  cwd?: string;
  env?: Record<string, string>;
}

export interface PrologTerminal extends Omit<Terminal, 'exitStatus'> {
  name: string;
  processId: Promise<number | undefined>;
  creationOptions: PrologTerminalOptions;
  exitStatus: Promise<TerminalExitStatus | undefined>;
  sendText(text: string, addNewLine?: boolean): void;
  show(preserveFocus?: boolean): void;
  hide(): void;
  dispose(): void;
}

// Chat Integration Types
export interface PrologChatContext {
  request: ChatRequest;
  stream: ChatResponseStream;
  token: CancellationToken;
  command?: string;
  args?: string[];
}

export interface PrologChatResult extends ChatResult {
  metadata?: {
    command?: string;
    success?: boolean;
    executionTime?: number;
    resultCount?: number;
    error?: string;
  };
}

export interface PrologChatCommand {
  name: string;
  description: string;
  handler: (context: PrologChatContext) => Promise<void>;
  examples?: string[];
  parameters?: Array<{
    name: string;
    description: string;
    required: boolean;
    type: 'string' | 'number' | 'boolean';
  }>;
}

// Telemetry Types
export interface TelemetryData {
  command: string;
  success: boolean;
  error?: string;
  timestamp: number;
  executionTime?: number;
  resultCount?: number;
  userId?: string;
  sessionId?: string;
}

export interface TelemetryCollector {
  enabled: boolean;
  collect(data: TelemetryData): void;
  getStats(): {
    totalCommands: number;
    commands: Record<string, number>;
    successRate: number;
    averageExecutionTime: number;
  } | null;
  clear(): void;
}

// Debug Types
export interface PrologDebugConfiguration {
  type: 'prolog';
  request: 'launch';
  name: string;
  program: string;
  startupQuery: string;
  stopOnEntry: boolean;
  cwd: string;
  env: Record<string, string>;
  runtimeExecutable: string;
  runtimeArgs: string[];
  traceCmds: {
    continue: string[];
    stepover: string[];
    stepinto: string[];
    stepout: string[];
  };
}

export interface PrologDebugSession {
  id: string;
  configuration: PrologDebugConfiguration;
  isRunning: boolean;
  currentFrame?: {
    file: string;
    line: number;
    predicate: string;
    variables: Record<string, any>;
  };
}

// Snippet Types
export interface PrologSnippet {
  name: string;
  prefix: string;
  body: string | string[];
  description: string;
  scope?: string;
  predicate?: string;
  arity?: number;
  module?: string;
}

export interface SnippetCollection {
  [key: string]: PrologSnippet;
}

// Refactoring Types
export interface PrologRefactorAction {
  type: 'rename' | 'extract' | 'inline' | 'move';
  title: string;
  description: string;
  predicate: string;
  arity: number;
  range: PrologRange;
  newName?: string;
  targetFile?: string;
}

// Workspace Types
export interface PrologWorkspace {
  rootPath: string;
  folders: Array<{
    uri: Uri;
    name: string;
    index: number;
  }>;
  prologFiles: Uri[];
  configuration: ExtensionConfiguration;
}

// Event Types
export interface ExtensionEventMap {
  activated: (context: ExtensionContext) => void;
  deactivated: () => void;
  configurationChanged: (config: ExtensionConfiguration) => void;
  documentOpened: (document: PrologDocument) => void;
  documentClosed: (document: PrologDocument) => void;
  documentSaved: (document: PrologDocument) => void;
  terminalCreated: (terminal: PrologTerminal) => void;
  terminalClosed: (terminal: PrologTerminal) => void;
  debugSessionStarted: (session: PrologDebugSession) => void;
  debugSessionEnded: (session: PrologDebugSession) => void;
}

// Command Types
export interface PrologCommand {
  command: string;
  title: string;
  category?: string;
  handler: (...args: any[]) => any;
  when?: string;
}

// Settings WebView Types
export interface SettingsWebViewMessage {
  type: 'getSetting' | 'setSetting' | 'resetSetting' | 'exportSettings' | 'importSettings';
  key?: string;
  value?: any;
  settings?: Record<string, any>;
}

export interface SettingsWebViewState {
  settings: ExtensionConfiguration;
  isDirty: boolean;
  lastSaved?: Date;
}

// Multi-IDE Support Types
export interface IDEConfiguration {
  name: string;
  configFiles: Array<{
    path: string;
    content: string;
    description: string;
  }>;
  launchConfigurations?: Array<{
    name: string;
    type: string;
    request: string;
    [key: string]: any;
  }>;
}

export interface SupportedIDE {
  name: string;
  detected: boolean;
  version?: string;
  configPath?: string;
  executable?: string;
}