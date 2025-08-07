'use strict';

import * as path from 'path';
import {
  type CancellationToken,
  chat,
  type ChatRequest,
  type ChatResponseStream,
  type ChatResult,
  commands,
  type DocumentFilter,
  type ExtensionContext,
  languages,
  ThemeIcon,
  window,
  workspace,
  WorkspaceEdit,
} from 'vscode';
import type { ApiServerConfig } from '../features/apiServer.js';
import { ApiServer } from '../features/apiServer.js';
import { PrologDefinitionProvider } from '../features/definitionProvider.js';
import PrologDocumentHighlightProvider from '../features/documentHighlightProvider.js';
import { loadEditHelpers } from '../features/editHelpers.js';
import type { ExternalWebSocketConfig } from '../features/externalWebSocketManager.js';
import { ExternalWebSocketManager } from '../features/externalWebSocketManager.js';
import PrologHoverProvider from '../features/hoverProvider.js';
import PrologLinter from '../features/linting/prologLinter.js';
import { MultiIDESupport } from '../features/multiIDESupport.js';
import { PrologActivityProvider } from '../features/prologActivityProvider.js';
import { PrologDashboardProvider } from '../features/prologDashboardProvider.js';
import { PrologFormatter } from '../features/prologFormatter.js';
import { PrologLSPClient } from '../features/prologLSPClient.js';
import { PrologLSPExtension } from '../features/prologLSPExtension.js';
import { PrologRefactor } from '../features/prologRefactor.js';
import PrologTerminal from '../features/prologTerminal.js';
import { PrologReferenceProvider } from '../features/referenceProvider.js';
import { SettingsWebviewProvider } from '../features/settingsWebviewProvider.js';
import {
  PrologCompletionProvider,
  SnippetUpdater,
  SnippetUpdaterController,
} from '../features/updateSnippets.js';
import { PrologBackend } from '../prologBackend.js';
import { Utils } from '../utils/utils.js';
import { ChatHandler } from './chatHandler.js';
import { ConfigurationManager } from './configurationManager.js';
import { InstallationManager } from './installationManager.js';
import { TelemetryCollector } from './telemetryCollector.js';

export class ExtensionManager {
  private static instance: ExtensionManager;

  // Global instances
  private prologBackend: PrologBackend | null = null;
  private prologLSPClient: PrologLSPClient | null = null;
  private apiServer: ApiServer | null = null;
  private externalWebSocketManager: ExternalWebSocketManager | null = null;

  // Module instances
  private telemetry: TelemetryCollector;
  private chatHandler: ChatHandler;
  private installationManager: InstallationManager;
  private configurationManager: ConfigurationManager;

  private constructor() {
    this.telemetry = new TelemetryCollector();
    this.chatHandler = new ChatHandler(null, this.telemetry);
    this.installationManager = InstallationManager.getInstance();
    this.configurationManager = ConfigurationManager.getInstance();
  }

  static getInstance(): ExtensionManager {
    if (!ExtensionManager.instance) {
      ExtensionManager.instance = new ExtensionManager();
    }
    return ExtensionManager.instance;
  }

  // Main activation method
  async activate(context: ExtensionContext): Promise<void> {
    console.log('Congratulations, your extension "vsc-prolog" is now active! :)');

    // Check SWI-Prolog installation before proceeding
    await this.installationManager.checkAndHandleInstallation(context);

    // Initialize workspace for dialect
    await this.configurationManager.initForDialect(context);

    // Initialize core extension features
    await this.initializeCoreFeatures(context);

    // Initialize backend and services
    await this.initializeBackendServices(context);

    // Register providers and UI components
    this.registerProvidersAndUI(context);

    // Start backend automatically (with error handling)
    this.startBackend();
  }

  // Initialize core extension features
  private async initializeCoreFeatures(context: ExtensionContext): Promise<void> {
    // Filter the files to process
    const PROLOG_MODE: DocumentFilter = { language: 'prolog', scheme: 'file' };

    // Initialize utils class and load snippets file with its predicates
    Utils.init(context);

    // Automatic indent on change
    loadEditHelpers(context.subscriptions);

    // Register extension commands
    this.registerExtensionCommands(context);

    // Register installation commands
    this.installationManager.registerInstallationCommands(context);

    // Initialize linter if enabled
    this.initializeLinter(context, PROLOG_MODE);

    // Register language providers
    this.registerLanguageProviders(context, PROLOG_MODE);

    // Initialize terminal and snippets
    this.initializeTerminalAndSnippets(context, PROLOG_MODE);
  }

  // Register extension-specific commands
  private registerExtensionCommands(context: ExtensionContext): void {
    const myCommands = [
      {
        command: 'prolog.load.document',
        callback: () => {
          PrologTerminal.loadDocument();
        },
      },
      {
        command: 'prolog.query.goal',
        callback: () => {
          PrologTerminal.queryGoalUnderCursor();
        },
      },
      {
        command: 'prolog.refactorPredicate',
        callback: () => {
          new PrologRefactor().refactorPredUnderCursor();
        },
      },
      {
        command: 'prolog.openSettings',
        callback: () => {
          // This will be handled by the webview view registration
        },
      },
      {
        command: 'prolog.newFile',
        callback: async () => {
          await this.createNewPrologFile();
        },
      },
      {
        command: 'prolog.rerunQuery',
        callback: async (query: string) => {
          await this.rerunQuery(query);
        },
      },
      {
        command: 'prolog.clearQueryHistory',
        callback: async () => {
          window.showInformationMessage('Query history cleared');
        },
      },
      {
        command: 'prolog.viewLogs',
        callback: () => {
          commands.executeCommand('workbench.action.showLogs');
        },
      },
      {
        command: 'prolog.reportIssue',
        callback: () => {
          const issueUrl = 'https://github.com/mediaprophet/VSCode-Prolog-Toolkit/issues/new';
          commands.executeCommand('vscode.open', issueUrl);
        },
      },
    ];

    // Register commands
    myCommands.forEach(command => {
      context.subscriptions.push(commands.registerCommand(command.command, command.callback));
    });
  }

  // Initialize linter if enabled
  private initializeLinter(context: ExtensionContext, PROLOG_MODE: DocumentFilter): void {
    let linter: PrologLinter | undefined;
    if (Utils.LINTERTRIGGER !== 'never') {
      linter = new PrologLinter(context);
      linter.activate();

      // Register linter commands
      const linterCommands = [
        {
          command: 'prolog.linter.nextErrLine',
          callback: () => {
            linter?.nextErrLine();
          },
        },
        {
          command: 'prolog.linter.prevErrLine',
          callback: () => {
            linter?.prevErrLine();
          },
        },
      ];

      linterCommands.forEach(command => {
        context.subscriptions.push(commands.registerCommand(command.command, command.callback));
      });

      // Register code actions provider
      context.subscriptions.push(languages.registerCodeActionsProvider(PROLOG_MODE, linter));
    }
  }

  // Register language providers
  private registerLanguageProviders(context: ExtensionContext, PROLOG_MODE: DocumentFilter): void {
    // Hover provider
    context.subscriptions.push(
      languages.registerHoverProvider(PROLOG_MODE, new PrologHoverProvider())
    );

    // Highlight provider
    context.subscriptions.push(
      languages.registerDocumentHighlightProvider(
        PROLOG_MODE,
        new PrologDocumentHighlightProvider()
      )
    );

    // Definition provider (go to definition command)
    context.subscriptions.push(
      languages.registerDefinitionProvider(PROLOG_MODE, new PrologDefinitionProvider())
    );

    // Reference provider (find all references command)
    context.subscriptions.push(
      languages.registerReferenceProvider(PROLOG_MODE, new PrologReferenceProvider())
    );

    // Auto completion provider
    context.subscriptions.push(
      languages.registerCompletionItemProvider(PROLOG_MODE, new PrologCompletionProvider())
    );

    // File formatting provider
    context.subscriptions.push(
      languages.registerDocumentRangeFormattingEditProvider(PROLOG_MODE, new PrologFormatter())
    );
    context.subscriptions.push(
      languages.registerDocumentFormattingEditProvider(PROLOG_MODE, new PrologFormatter())
    );
  }

  // Initialize terminal and snippets
  private initializeTerminalAndSnippets(
    context: ExtensionContext,
    PROLOG_MODE: DocumentFilter
  ): void {
    // Create prolog terminal (load file command)
    context.subscriptions.push(PrologTerminal.init());

    // Add created predicate to the snippet
    const snippetUpdater = new SnippetUpdater();
    context.subscriptions.push(new SnippetUpdaterController(snippetUpdater));
    context.subscriptions.push(snippetUpdater);
  }

  // Initialize backend and services
  private async initializeBackendServices(context: ExtensionContext): Promise<void> {
    const config = workspace.getConfiguration('prolog');
    const swiplPath = config.get('executablePath', 'swipl') as string;

    // Initialize Prolog backend
    this.prologBackend = new PrologBackend({
      swiplPath,
      port: 3060,
      streamingEnabled: true,
      maxResultsPerChunk: 50,
    });

    // Update chat handler with backend reference
    this.chatHandler.updateBackend(this.prologBackend);

    // Initialize API server if enabled
    await this.initializeApiServer(config);

    // Initialize LSP services
    await this.initializeLSPServices(context);

    // Generate multi-IDE configurations
    await this.initializeMultiIDESupport();

    // Set up backend event handlers
    this.setupBackendEventHandlers();
  }

  // Initialize API server if enabled
  private async initializeApiServer(config: any): Promise<void> {
    const apiServerEnabled = config.get('apiServer.enabled', false) as boolean;
    if (apiServerEnabled && this.prologBackend) {
      try {
        const apiServerConfig: ApiServerConfig = {
          enabled: true,
          port: config.get('apiServer.port', 8080) as number,
          host: config.get('apiServer.host', 'localhost') as string,
          corsOrigins: config.get('apiServer.corsOrigins', ['http://localhost:*']) as string[],
          maxConnections: config.get('apiServer.maxConnections', 100) as number,
          requestTimeout: config.get('apiServer.requestTimeout', 60000) as number,
          rateLimiting: {
            enabled: config.get('apiServer.rateLimiting.enabled', true) as boolean,
            requestsPerMinute: config.get('apiServer.rateLimiting.requestsPerMinute', 60) as number,
            burstLimit: config.get('apiServer.rateLimiting.burstLimit', 10) as number,
          },
          auth: this.configurationManager.createAuthConfig(config),
        };

        this.apiServer = new ApiServer({
          config: apiServerConfig,
          prologBackend: this.prologBackend,
        });

        // Initialize external WebSocket manager if enabled
        const wsEnabled = config.get('webSocketServer.enabled', true) as boolean;
        if (wsEnabled) {
          const wsConfig: ExternalWebSocketConfig = {
            enabled: true,
            port: config.get('webSocketServer.port', 8081) as number,
            maxConnections: config.get('webSocketServer.maxConnections', 50) as number,
            heartbeatInterval: config.get('webSocketServer.heartbeatInterval', 30) as number,
            auth: apiServerConfig.auth,
          };

          this.externalWebSocketManager = new ExternalWebSocketManager(
            wsConfig,
            this.prologBackend.getNotificationManager()
          );
        }

        console.log('[Extension] API server and WebSocket manager initialized');
      } catch (error) {
        console.error('[Extension] Failed to initialize API server:', error);
        window.showErrorMessage(`Failed to initialize API server: ${error}`);
      }
    }
  }

  // Initialize LSP services
  private async initializeLSPServices(context: ExtensionContext): Promise<void> {
    if (!this.prologBackend) return;

    // Initialize LSP Extension (legacy support)
    const lspExtension = new PrologLSPExtension(context, this.prologBackend);
    lspExtension.registerFeatures();
    context.subscriptions.push(lspExtension);

    // Initialize full LSP Client
    this.prologLSPClient = new PrologLSPClient(context);

    // Start LSP client
    try {
      await this.prologLSPClient.start();
      console.log('[Extension] Prolog LSP Client started successfully');
    } catch (error) {
      console.error('[Extension] Failed to start Prolog LSP Client:', error);
      window.showWarningMessage(
        'Prolog LSP Client failed to start. Some features may not be available.'
      );
    }
  }

  // Initialize multi-IDE support
  private async initializeMultiIDESupport(): Promise<void> {
    const workspaceFolder = workspace.workspaceFolders?.[0];
    if (workspaceFolder) {
      try {
        await MultiIDESupport.generateIDEConfigurations(workspaceFolder.uri.fsPath);
        MultiIDESupport.generateLaunchConfigurations(workspaceFolder.uri.fsPath);

        // Detect available IDEs
        const availableIDEs = await MultiIDESupport.detectAvailableIDEs();
        if (availableIDEs.length > 1) {
          console.log('[Extension] Detected IDEs:', availableIDEs.join(', '));
        }
      } catch (error) {
        console.error('[Extension] Failed to generate multi-IDE configurations:', error);
      }
    }
  }

  // Set up backend event handlers
  private setupBackendEventHandlers(): void {
    if (!this.prologBackend) return;

    this.prologBackend.on('ready', () => {
      console.log('[Extension] Prolog backend ready');
      window.showInformationMessage('Prolog backend started successfully');
    });

    this.prologBackend.on('stopped', () => {
      console.log('[Extension] Prolog backend stopped');
      window.showWarningMessage('Prolog backend stopped');
    });

    this.prologBackend.on('restarted', () => {
      console.log('[Extension] Prolog backend restarted');
      window.showInformationMessage('Prolog backend restarted successfully');
    });

    this.prologBackend.on('error', error => {
      console.error('[Extension] Prolog backend error:', error);
      window.showErrorMessage(`Prolog backend error: ${error}`);
    });
  }

  // Register providers and UI components
  private registerProvidersAndUI(context: ExtensionContext): void {
    // Register chat participant with enhanced followup provider
    const chatParticipant = chat.createChatParticipant(
      'prolog',
      (request: ChatRequest, context: any, stream: ChatResponseStream, token: CancellationToken) =>
        this.chatHandler.handleChatRequest(request, context, stream, token)
    );
    chatParticipant.iconPath = new ThemeIcon('symbol-class');
    chatParticipant.followupProvider = {
      provideFollowups(result: ChatResult, _context: any, __token: CancellationToken) {
        return ExtensionManager.getInstance().getChatFollowups(result);
      },
    };
    context.subscriptions.push(chatParticipant);

    // Register settings webview provider
    const settingsProvider = new SettingsWebviewProvider(context.extensionUri);
    context.subscriptions.push(
      window.registerWebviewViewProvider(SettingsWebviewProvider.viewType, settingsProvider)
    );

    // Register activity bar providers
    const prologActivityProvider = new PrologActivityProvider(context);
    const prologDashboardProvider = new PrologDashboardProvider(context.extensionUri);

    context.subscriptions.push(
      window.registerTreeDataProvider('prologActivity', prologActivityProvider),
      window.registerTreeDataProvider('prologQueries', prologActivityProvider),
      window.registerTreeDataProvider('prologFiles', prologActivityProvider),
      window.registerWebviewViewProvider(PrologDashboardProvider.viewType, prologDashboardProvider)
    );

    // Refresh activity bar when installation status changes
    const refreshActivityBar = () => {
      prologActivityProvider.refresh();
    };

    // Listen for configuration changes to refresh activity bar
    context.subscriptions.push(
      workspace.onDidChangeConfiguration(event => {
        if (event.affectsConfiguration('prolog')) {
          refreshActivityBar();
        }
      })
    );
  }

  // Get chat followups based on result
  private getChatFollowups(result: ChatResult): any[] {
    const followups = [];

    // Context-aware followups based on command type
    if (result.metadata?.command === 'query') {
      followups.push({
        prompt: '/status',
        label: '🔧 Check backend status',
      });
      followups.push({
        prompt: '/help findall/3',
        label: '📖 Learn about findall/3',
      });
    }

    if (result.metadata?.command === 'consult') {
      followups.push({
        prompt: '/query',
        label: '🔍 Run a query',
      });
    }

    if (result.metadata?.command === 'error') {
      followups.push({
        prompt: '/status',
        label: '🚨 Check what went wrong',
      });
      followups.push({
        prompt: '/help',
        label: '❓ Show help',
      });
    }

    if (result.metadata?.command === 'n3_load') {
      followups.push({
        prompt: '/n3_list --limit 10',
        label: '📋 List loaded triples',
      });
      followups.push({
        prompt: '/n3_reason',
        label: '🧠 Start reasoning',
      });
    }

    // Always include help as fallback
    followups.push({
      prompt: '/help',
      label: '💡 Show all commands',
    });

    return followups;
  }

  // Start backend
  private startBackend(): void {
    try {
      this.prologBackend?.start();
    } catch (error) {
      console.error('[Extension] Failed to start Prolog backend:', error);
      // Don't show error immediately - let user trigger it via chat if needed
    }
  }

  // Helper method to create new Prolog file
  private async createNewPrologFile(): Promise<void> {
    const fileName = await window.showInputBox({
      prompt: 'Enter the name for the new Prolog file',
      value: 'untitled.pl',
      validateInput: value => {
        if (!value) return 'File name cannot be empty';
        if (!value.match(/\.(pl|pro|prolog|plt|ecl)$/)) {
          return 'File must have a Prolog extension (.pl, .pro, .prolog, .plt, .ecl)';
        }
        return null;
      },
    });

    if (fileName) {
      const workspaceFolder = workspace.workspaceFolders?.[0];
      if (workspaceFolder) {
        const uri = window.activeTextEditor?.document.uri || workspaceFolder.uri;
        const newFileUri = uri.with({ path: path.join(path.dirname(uri.fsPath), fileName) });
        const edit = new WorkspaceEdit();
        edit.createFile(newFileUri, { ignoreIfExists: false });
        await Promise.resolve(workspace.applyEdit(edit));
        await window.showTextDocument(newFileUri);
      } else {
        const doc = await workspace.openTextDocument({
          language: 'prolog',
          content: `% ${fileName}\n% New Prolog file\n\n`,
        });
        await window.showTextDocument(doc);
      }
    }
  }

  // Helper method to rerun query
  private async rerunQuery(query: string): Promise<void> {
    if (query && this.prologBackend?.isRunning()) {
      try {
        const response = await this.prologBackend.sendRequest('query', { goal: query });
        if (response.status === 'ok') {
          window.showInformationMessage(`Query executed: ${query}`);
        } else {
          window.showErrorMessage(`Query failed: ${response.error}`);
        }
      } catch (error) {
        window.showErrorMessage(`Query error: ${error}`);
      }
    }
  }

  // Deactivation method
  async deactivate(): Promise<void> {
    // Stop API server
    if (this.apiServer) {
      try {
        await this.apiServer.stop();
        console.log('[Extension] API server stopped');
      } catch (error) {
        console.error('[Extension] Error stopping API server:', error);
      }
      this.apiServer = null;
    }

    // Stop external WebSocket manager
    if (this.externalWebSocketManager) {
      try {
        await this.externalWebSocketManager.stop();
        console.log('[Extension] External WebSocket manager stopped');
      } catch (error) {
        console.error('[Extension] Error stopping WebSocket manager:', error);
      }
      this.externalWebSocketManager = null;
    }

    // Stop LSP client
    if (this.prologLSPClient) {
      try {
        await this.prologLSPClient.stop();
        console.log('[Extension] Prolog LSP Client stopped');
      } catch (error) {
        console.error('[Extension] Error stopping LSP Client:', error);
      }
      this.prologLSPClient = null;
    }

    // Stop backend
    if (this.prologBackend) {
      this.prologBackend.stop(true);
      this.prologBackend = null;
    }
  }
}
