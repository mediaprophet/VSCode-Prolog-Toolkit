import type {
  CancellationToken,
  CodeActionContext,
  Command,
  Diagnostic,
  DiagnosticCollection,
  DiagnosticSeverity,
  Position,
  Range,
  TextDocument,
  Uri,
} from 'vscode';

/**
 * Enum for different triggers that can run the linter
 */
export enum RunTrigger {
  onType,
  onSave,
  never,
}

/**
 * Interface for linter configuration
 */
export interface ILinterConfiguration {
  executable: string;
  trigger: RunTrigger;
  delay: number;
  enableOutput: boolean;
}

/**
 * Interface for diagnostic information
 */
export interface IDiagnosticInfo {
  fileName: string;
  severity: DiagnosticSeverity;
  line: number;
  fromCol: number;
  toCol: number;
  message: string;
}

/**
 * Interface for process execution result
 */
export interface IProcessResult {
  stdout: string;
  stderr: string;
  exitCode: number;
}

/**
 * Interface for configuration manager
 */
export interface IConfigurationManager {
  loadConfiguration(): Promise<ILinterConfiguration>;
  resolveExecutablePath(): Promise<string>;
  getDelay(): number;
  getTrigger(): RunTrigger;
  isOutputEnabled(): boolean;
  getCurrentConfiguration(): ILinterConfiguration | null;
}

/**
 * Interface for process executor
 */
export interface IProcessExecutor {
  executeProlog(document: TextDocument, config: ILinterConfiguration): Promise<IProcessResult>;
  buildArgumentsForDialect(trigger: RunTrigger, fileName: string, documentText: string): {
    args: string[];
    goals?: string;
  };
}

/**
 * Interface for diagnostic parser
 */
export interface IDiagnosticParser {
  parseIssue(issue: string, filePathIds: { [id: string]: string }): IDiagnosticInfo | null;
  parsePrologOutput(
    output: string,
    errorOutput: string,
    textDocument: TextDocument
  ): IDiagnosticInfo[];
  convertToDiagnostic(info: IDiagnosticInfo): Diagnostic;
  groupDiagnosticsByFile(diagnostics: IDiagnosticInfo[]): { [fileName: string]: IDiagnosticInfo[] };
}

/**
 * Interface for code action provider
 */
export interface ICodeActionProvider {
  provideCodeActions(
    document: TextDocument,
    range: Range,
    context: CodeActionContext,
    token: CancellationToken
  ): Command[] | Promise<Command[]>;
}

/**
 * Interface for command manager
 */
export interface ICommandManager {
  registerCommands(): void;
  dispose(): void;
  addDynamicDirective(
    doc: TextDocument,
    predicate: string,
    uri: Uri,
    range: Range
  ): Promise<boolean>;
  addUseModule(
    doc: TextDocument,
    predicate: string,
    module: string,
    uri: Uri,
    range: Range
  ): Promise<boolean>;
}

/**
 * Interface for navigation provider
 */
export interface INavigationProvider {
  gotoNextError(): void;
  gotoPrevError(): void;
  gotoErrorLine(direction: number): void;
  updateSortedDiagnosticIndex(documentUri: string, diagnostics: Diagnostic[]): void;
  displayAllDiagnostics(documentPath: string, diagnostics: Diagnostic[]): void;
  clearNavigationState(documentUri: string): void;
  isNavigationAvailable(): boolean;
  getDiagnosticCount(documentUri: string): { errors: number; warnings: number; total: number };
}

/**
 * Interface for diagnostic manager
 */
export interface IDiagnosticManager {
  getDiagnosticCollection(): DiagnosticCollection;
  updateDiagnostics(document: TextDocument, diagnostics: Diagnostic[]): void;
  clearDiagnostics(document: TextDocument): void;
  getSortedDiagnostics(documentUri: string): number[];
}