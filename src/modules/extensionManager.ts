
'use strict';

'use strict';

'use strict';

import * as vscode from 'vscode';
import { InstallationChecker } from '../features/installation/InstallationChecker';
import { InstallationGuide } from '../features/installation/InstallationGuide';
import { QueryHistoryOrchestrator } from '../features/queryHistoryManager/QueryHistoryOrchestrator';
import { ChatHandler } from './chatHandler.js';
import { ConfigurationManager } from './configurationManager.js';
import { BackendService } from './extensionManager/backendService';
import { ChatService } from './extensionManager/chatService';
import { FileService } from './extensionManager/fileService';
import { LinterService } from './extensionManager/linterService';
import { UIService } from './extensionManager/uiService';

import { TelemetryCollector } from './telemetryCollector';

export class ExtensionManager {
  private static instance: ExtensionManager;
  private telemetry: TelemetryCollector;
  private chatHandler: ChatHandler;
  private installationGuide: InstallationGuide;
  private installationChecker: InstallationChecker;
  private configurationManager: ConfigurationManager;
  private queryHistoryOrchestrator: QueryHistoryOrchestrator;
  private uiService: UIService;
  private linterService: LinterService;
  private fileService: FileService;
  private chatService: ChatService;
  private backendService: BackendService;

  private constructor() {
    this.telemetry = new TelemetryCollector();
    this.chatHandler = new ChatHandler(null, this.telemetry);
    this.installationGuide = InstallationGuide.getInstance();
    this.installationChecker = InstallationChecker.getInstance();
    this.configurationManager = ConfigurationManager.getInstance();
    this.queryHistoryOrchestrator = new QueryHistoryOrchestrator();
    this.uiService = new UIService();
    this.linterService = new LinterService();
    this.fileService = new FileService();
    this.chatService = new ChatService(this.chatHandler);
    this.backendService = new BackendService();
  }

  // Filter and tree view commands are now handled by UIService

  static getInstance(): ExtensionManager {
    if (!ExtensionManager.instance) {
      ExtensionManager.instance = new ExtensionManager();
    }
    return ExtensionManager.instance;
  }

  // Main activation method
  async activate(context: vscode.ExtensionContext): Promise<void> {
    console.log('Congratulations, your extension "vsc-prolog" is now active! :)');
    // Run installation check and show guide/wizard if needed
    const installationStatus = await this.installationChecker.checkSwiplInstallation();
    if (!installationStatus.isInstalled) {
      await this.installationGuide.runSetupWizard();
    }
    // Register the settings webview provider
    const { registerSettingsWebview } = await import('../features/registerSettingsWebview');
    registerSettingsWebview(context);
    // Register installation-related commands
    context.subscriptions.push(
      vscode.commands.registerCommand('prolog.setupWizard', async () => {
        await this.installationGuide.runSetupWizard();
      })
    );
    context.subscriptions.push(
      vscode.commands.registerCommand('prolog.showInstallationGuide', async () => {
        await this.installationGuide.showInstallationGuideDialog();
      })
    );
    const PROLOG_MODE: vscode.DocumentFilter = { language: 'prolog', scheme: 'file' };
    this.uiService.registerProvidersAndUI(context, this.queryHistoryOrchestrator, this.backendService.prologBackend);
    this.linterService.initializeLinter(context, this.configurationManager, PROLOG_MODE);
    this.linterService.registerLanguageProviders(context, PROLOG_MODE);
    await this.backendService.initializeBackendServices(context, this.configurationManager, this.chatHandler);
  }

  // Extension-specific commands can be moved to a CommandService if needed

  // Linter and language providers are now handled by LinterService

  // Terminal and snippet logic can be moved to a dedicated service if needed

  // Backend and services are now handled by BackendService

  // API server logic is now handled by BackendService

  // LSP logic is now handled by BackendService

  // Multi-IDE support is now handled by BackendService

  // Backend event handlers are now handled by BackendService

  // Providers and UI are now handled by UIService

  // Chat followups logic can be moved to ChatService if needed

  // Backend start logic is now handled by BackendService

  // File creation logic is now handled by FileService

  // Query rerun logic can be moved to BackendService or ChatService

  // Deactivation logic is now handled by BackendService
}
