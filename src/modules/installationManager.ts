'use strict';

import type { ExtensionContext } from 'vscode';
import { commands, window, workspace } from 'vscode';
import { ConfigurationMigration } from '../features/configurationMigration.js';
import { InstallationChecker } from '../features/installationChecker.js';
import { InstallationGuide } from '../features/installationGuide/index.js';

export class InstallationManager {
  private static instance: InstallationManager;
  private installationChecker: InstallationChecker;
  private installationGuide: InstallationGuide;
  private configurationMigration: ConfigurationMigration;

  private constructor() {
    this.installationChecker = InstallationChecker.getInstance();
    this.installationGuide = InstallationGuide.getInstance();
    this.configurationMigration = ConfigurationMigration.getInstance();
  }

  static getInstance(): InstallationManager {
    if (!InstallationManager.instance) {
      InstallationManager.instance = new InstallationManager();
    }
    return InstallationManager.instance;
  }

  // Installation checking and handling
  async checkAndHandleInstallation(context: ExtensionContext): Promise<void> {
    try {
      // Set extension context for configuration migration
      this.configurationMigration.setExtensionContext(context);

      // Check for configuration migration needs first
      await this.configurationMigration.performComprehensiveMigration();

      const installationStatus = await this.installationChecker.checkSwiplInstallation();

      if (!installationStatus.isInstalled) {
        // SWI-Prolog not found - show user-friendly guidance
        const action = await window.showWarningMessage(
          'SWI-Prolog is not installed or not found in the configured path. The Prolog extension requires SWI-Prolog to function properly.',
          'Install SWI-Prolog',
          'Setup Wizard',
          'Configure Path',
          'Continue Anyway'
        );

        switch (action) {
          case 'Install SWI-Prolog':
            await this.showInstallationInstructions();
            break;
          case 'Setup Wizard':
            await commands.executeCommand('prolog.setupWizard');
            break;
          case 'Configure Path':
            await commands.executeCommand('workbench.action.openSettings', 'prolog.executablePath');
            break;
          case 'Continue Anyway':
            window.showInformationMessage(
              'Some Prolog features may not work without SWI-Prolog installed.'
            );
            break;
          default:
            break;
        }
      } else if (installationStatus.issues && installationStatus.issues.length > 0) {
        // Installation found but has issues
        const issueMessage = installationStatus.issues.join('; ');
        const action = await window.showInformationMessage(
          `SWI-Prolog found at ${installationStatus.path} (version ${installationStatus.version}), but there are configuration issues: ${issueMessage}`,
          'Fix Configuration',
          'Ignore'
        );

        if (action === 'Fix Configuration') {
          const updateResult = await this.installationChecker.validateAndUpdateConfiguration();
          if (updateResult.updated) {
            window.showInformationMessage(
              `Configuration updated: SWI-Prolog path changed from '${updateResult.oldPath}' to '${updateResult.newPath}'`
            );
          }
        }
      } else {
        // Installation is good - show success message only in development mode
        console.log(
          `SWI-Prolog found: ${installationStatus.path} (version ${installationStatus.version})`
        );
      }
    } catch (error) {
      console.error('Error checking SWI-Prolog installation:', error);
      window.showErrorMessage(
        'Failed to check SWI-Prolog installation. Some features may not work properly.'
      );
    }
  }

  // Show platform-specific installation instructions using InstallationGuide
  private async showInstallationInstructions(): Promise<void> {
    await this.installationGuide.showInstallationGuideDialog();
  }

  // Register installation-related commands
  registerInstallationCommands(context: ExtensionContext): void {
    const installationCommands = [
      {
        command: 'prolog.setupWizard',
        callback: async () => {
          await this.installationGuide.runSetupWizard();
        },
      },
      {
        command: 'prolog.refreshInstallation',
        callback: async () => {
          await this.installationChecker.checkSwiplInstallation();
          window.showInformationMessage('Installation status refreshed');
        },
      },
      {
        command: 'prolog.testInstallation',
        callback: async () => {
          const config = workspace.getConfiguration('prolog');
          const executablePath = config.get<string>('executablePath', 'swipl');
          const isValid = await this.installationChecker.validateSwiplPath(executablePath);
          if (isValid) {
            const version = await this.installationChecker.getSwiplVersion(executablePath);
            window.showInformationMessage(`SWI-Prolog is working correctly (version ${version})`);
          } else {
            window.showErrorMessage('SWI-Prolog executable is not valid or not accessible');
          }
        },
      },
      {
        command: 'prolog.autoDetectPath',
        callback: async () => {
          const foundPath = await this.installationChecker.findSwiplExecutable();
          if (foundPath) {
            const config = workspace.getConfiguration('prolog');
            await config.update('executablePath', foundPath, true);
            window.showInformationMessage(
              `SWI-Prolog path auto-detected and updated: ${foundPath}`
            );
          } else {
            window.showWarningMessage('SWI-Prolog not found in common locations');
          }
        },
      },
    ];

    installationCommands.forEach(cmd => {
      context.subscriptions.push(commands.registerCommand(cmd.command, cmd.callback));
    });
  }
}
