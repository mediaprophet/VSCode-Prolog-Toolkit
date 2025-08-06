import {
  CancellationToken,
  CodeActionContext,
  CodeActionProvider,
  Command,
  commands,
  Diagnostic,
  DiagnosticCollection,
  Disposable,
  ExtensionContext,
  languages,
  OutputChannel,
  Range,
  TextDocument,
  Uri,
  window,
  workspace,
} from 'vscode';
import { InstallationGuide } from '../installationGuide';
import { ConfigurationManager } from './configurationManager';
import { ProcessExecutor } from './processExecutor';
import { DiagnosticParser } from './diagnosticParser';
import { CodeActionProvider as PrologCodeActionProvider } from './codeActionProvider';
import { NavigationProvider } from './navigationProvider';
import { CommandManager } from './commandManager';
import {
  IConfigurationManager,
  IProcessExecutor,
  IDiagnosticParser,
  ICodeActionProvider,
  INavigationProvider,
  ICommandManager,
  RunTrigger,
} from './interfaces';

/**
 * Main Prolog Linter class that orchestrates all linting functionality
 * Now refactored into a modular architecture with separated concerns
 */
export default class PrologLinter implements CodeActionProvider {
  // Core modules
  private configurationManager: IConfigurationManager;
  private processExecutor: IProcessExecutor;
  private diagnosticParser: IDiagnosticParser;
  private codeActionProvider: ICodeActionProvider;
  private navigationProvider: INavigationProvider;
  private commandManager: ICommandManager;

  // VS Code integration
  private diagnosticCollection: DiagnosticCollection;
  private diagnostics: { [docName: string]: Diagnostic[] } = {};
  private outputChannel: OutputChannel | null = null;

  // Document listeners
  private documentListener: Disposable;
  private openDocumentListener: Disposable;
  private timer: ReturnType<typeof setTimeout> | null = null;

  constructor(private context: ExtensionContext) {
    // Initialize all modules
    this.configurationManager = new ConfigurationManager();
    this.processExecutor = new ProcessExecutor(context);
    this.diagnosticParser = new DiagnosticParser();
    this.codeActionProvider = new PrologCodeActionProvider();
    
    // Initialize output channel
    this.initializeOutputChannel();
    
    // Initialize navigation and command managers (they need the diagnostic collection)
    this.diagnosticCollection = languages.createDiagnosticCollection();
    this.navigationProvider = new NavigationProvider(this.diagnosticCollection, this.outputChannel!);
    this.commandManager = new CommandManager();
  }

  /**
   * Initialize the output channel
   */
  private initializeOutputChannel(): void {
    if (this.outputChannel === null) {
      this.outputChannel = window.createOutputChannel('PrologLinter');
      this.outputChannel.clear();
    }
  }

  /**
   * Implementation of CodeActionProvider interface method
   */
  public provideCodeActions(
    document: TextDocument,
    range: Range,
    context: CodeActionContext,
    token: CancellationToken
  ): Command[] | Promise<Command[]> {
    return this.codeActionProvider.provideCodeActions(document, range, context, token);
  }

  /**
   * Perform linting of a Prolog file
   */
  private async doPlint(textDocument: TextDocument): Promise<void> {
    // Check if the language of the document is Prolog
    if (textDocument.languageId !== 'prolog') {
      return;
    }

    // Clear existing diagnostics
    this.diagnostics = {};
    this.diagnosticCollection.delete(textDocument.uri);

    try {
      // Load configuration
      const config = await this.configurationManager.loadConfiguration();

      // Clear output if enabled
      if (config.enableOutput && this.outputChannel) {
        this.outputChannel.clear();
      }

      // Execute Prolog process
      const result = await this.processExecutor.executeProlog(textDocument, config);

      // Parse diagnostics
      const diagnosticInfos = this.diagnosticParser.parsePrologOutput(
        result.stdout,
        result.stderr,
        textDocument
      );

      // Convert to VS Code diagnostics and group by file
      const groupedDiagnostics = this.diagnosticParser.groupDiagnosticsByFile(diagnosticInfos);

      // Update diagnostic collection
      for (const [fileName, infos] of Object.entries(groupedDiagnostics)) {
        const diagnostics = infos.map(info => this.diagnosticParser.convertToDiagnostic(info));
        this.diagnostics[fileName] = diagnostics;
        this.diagnosticCollection.set(Uri.file(fileName), diagnostics);

        // Update navigation provider
        this.navigationProvider.updateSortedDiagnosticIndex(fileName, diagnostics);

        // Display diagnostics in output if enabled
        if (config.enableOutput && this.outputChannel) {
          this.navigationProvider.displayAllDiagnostics(fileName, diagnostics);
        }
      }
    } catch (error) {
      await this.handleLintingError(error);
    }
  }

  /**
   * Handle linting errors with enhanced error messages and installation guidance
   */
  private async handleLintingError(error: any): Promise<void> {
    let message: string | null = null;

    if (error.message === 'PROLOG_EXECUTABLE_NOT_FOUND' || error.code === 'ENOENT') {
      message =
        "Cannot lint the prolog file. The Prolog executable was not found. Use the 'prolog.executablePath' setting to configure";

      // Show enhanced error message with installation guidance
      const action = await window.showErrorMessage(
        'SWI-Prolog executable not found. The linter requires SWI-Prolog to check your code for errors and warnings.',
        'Install SWI-Prolog',
        'Setup Wizard',
        'Configure Path',
        'Dismiss'
      );

      const installationGuide = InstallationGuide.getInstance();
      switch (action) {
        case 'Install SWI-Prolog': {
          await installationGuide.showInstallationGuideDialog();
          break;
        }
        case 'Setup Wizard': {
          await commands.executeCommand('prolog.setupWizard');
          break;
        }
        case 'Configure Path': {
          await commands.executeCommand('workbench.action.openSettings', 'prolog.executablePath');
          break;
        }
        default:
          break;
      }
    } else {
      message = error.message
        ? error.message
        : `Failed to run prolog executable. Reason is unknown.`;
    }

    this.outputMsg(message);
  }

  /**
   * Load configuration settings and set up listeners
   */
  private async loadConfiguration(): Promise<void> {
    try {
      const config = await this.configurationManager.loadConfiguration();

      // Dispose existing listeners
      if (this.documentListener) {
        this.documentListener.dispose();
      }
      if (this.openDocumentListener) {
        this.openDocumentListener.dispose();
      }

      // Set up open document listener
      this.openDocumentListener = workspace.onDidOpenTextDocument(e => {
        this.triggerLinter(e);
      });

      // Set up document change/save listeners based on trigger
      if (config.trigger === RunTrigger.onType) {
        this.documentListener = workspace.onDidChangeTextDocument(e => {
          this.triggerLinter(e.document);
        });
      } else if (config.trigger === RunTrigger.onSave) {
        if (this.timer) {
          clearTimeout(this.timer);
        }
        this.documentListener = workspace.onDidSaveTextDocument(this.doPlint, this);
      }

      // Trigger linting for existing documents
      workspace.textDocuments.forEach(this.triggerLinter, this);
    } catch (error) {
      console.error('Failed to load linter configuration:', error);
      this.outputMsg(`Configuration error: ${error.message || error}`);
    }
  }

  /**
   * Trigger linting based on configuration
   */
  private triggerLinter(textDocument: TextDocument): void {
    // Check if the document is a Prolog source file
    if (textDocument.languageId !== 'prolog') {
      return;
    }

    const config = this.configurationManager.getCurrentConfiguration();
    if (!config) {
      return;
    }

    // If the linter is set to trigger on type with delay, use a timer
    if (config.trigger === RunTrigger.onType) {
      if (this.timer) {
        clearTimeout(this.timer);
      }
      this.timer = setTimeout(() => {
        this.doPlint(textDocument);
      }, config.delay);
    } else if (config.trigger !== RunTrigger.never) {
      // Trigger linting immediately for onSave
      this.doPlint(textDocument);
    }
  }

  /**
   * Activate the linter
   */
  public activate(): void {
    const subscriptions: Disposable[] = this.context.subscriptions;

    // Register commands
    this.commandManager.registerCommands();

    // Register configuration change listener
    workspace.onDidChangeConfiguration(() => this.loadConfiguration(), this, subscriptions);

    // Load initial configuration
    this.loadConfiguration();

    // Register listeners based on trigger
    workspace.onDidCloseTextDocument(
      textDocument => {
        this.diagnosticCollection.delete(textDocument.uri);
        this.navigationProvider.clearNavigationState(textDocument.uri.fsPath);
      },
      null,
      subscriptions
    );
  }

  /**
   * Output message to console
   */
  private outputMsg(msg: string): void {
    if (this.outputChannel) {
      this.outputChannel.append(msg + '\n');
      this.outputChannel.show(true);
    }
  }

  /**
   * Navigate to next error line
   */
  public nextErrLine(): void {
    this.navigationProvider.gotoNextError();
  }

  /**
   * Navigate to previous error line
   */
  public prevErrLine(): void {
    this.navigationProvider.gotoPrevError();
  }

  /**
   * Get diagnostic collection for external use
   */
  public getDiagnosticCollection(): DiagnosticCollection {
    return this.diagnosticCollection;
  }

  /**
   * Check if navigation is available
   */
  public isNavigationAvailable(): boolean {
    return this.navigationProvider.isNavigationAvailable();
  }

  /**
   * Get diagnostic count for current document
   */
  public getDiagnosticCount(): { errors: number; warnings: number; total: number } {
    const editor = window.activeTextEditor;
    if (!editor) {
      return { errors: 0, warnings: 0, total: 0 };
    }
    return this.navigationProvider.getDiagnosticCount(editor.document.uri.fsPath);
  }

  /**
   * Cleanup method to dispose of resources when the extension is deactivated
   */
  public dispose(): void {
    if (this.documentListener) {
      this.documentListener.dispose();
    }
    if (this.openDocumentListener) {
      this.openDocumentListener.dispose();
    }
    if (this.diagnosticCollection) {
      this.diagnosticCollection.clear();
      this.diagnosticCollection.dispose();
    }
    if (this.commandManager) {
      this.commandManager.dispose();
    }
    if (this.timer) {
      clearTimeout(this.timer);
    }
  }
}