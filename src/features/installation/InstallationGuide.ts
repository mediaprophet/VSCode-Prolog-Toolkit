import * as vscode from 'vscode';
import { InstallationChecker } from './InstallationChecker.js';
import { PackageManagerIntegration } from './PackageManagerIntegration.js';
import { SetupWizardWebview } from './SetupWizardWebview';

export interface InstallationGuideOptions {
  showDetailedInstructions?: boolean;
  allowSkip?: boolean;
  context?: vscode.ExtensionContext;
}

export class InstallationGuide {
  private static instance: InstallationGuide;
  private installationChecker: InstallationChecker;
  private packageManager: PackageManagerIntegration;

  private constructor() {
    this.installationChecker = InstallationChecker.getInstance();
    this.packageManager = PackageManagerIntegration.getInstance();
  }

  public static getInstance(): InstallationGuide {
    if (!InstallationGuide.instance) {
      InstallationGuide.instance = new InstallationGuide();
    }
    return InstallationGuide.instance;
  }

  /**
   * Show a comprehensive installation guide dialog (platform-specific)
   */
  public async showInstallationGuideDialog(): Promise<void> {
    const platform = process.platform;
    let instructions = '';
    let url = '';
    let command = '';
    if (platform === 'win32') {
      url = 'https://www.swi-prolog.org/download/stable';
      instructions = 'Download and run the Windows installer from the official SWI-Prolog website.';
    } else if (platform === 'darwin') {
      command = 'brew install swi-prolog';
      url = 'https://brew.sh';
      instructions = 'Install Homebrew if not already installed, then run the following command:';
    } else if (platform === 'linux') {
      // Try to detect distro for best command
      const distro = await this.packageManager.detectLinuxDistro();
      switch (distro) {
        case 'debian':
        case 'ubuntu':
          command = 'sudo apt install swi-prolog';
          break;
        case 'fedora':
          command = 'sudo dnf install pl';
          break;
        case 'arch':
          command = 'sudo pacman -S swi-prolog';
          break;
        case 'opensuse':
          command = 'sudo zypper install swi-prolog';
          break;
        default:
          command = 'sudo apt install swi-prolog';
      }
      instructions = 'Run the following command in your terminal:';
    }

    const actions = [];
    if (command) actions.push('Copy Command');
    if (url) actions.push('Open Download Page');
    actions.push('OK');

    const message = `${instructions}${command ? `\n\n${command}` : ''}`;
    const choice = await vscode.window.showInformationMessage(message, ...actions);

    if (choice === 'Copy Command' && command) {
      await vscode.env.clipboard.writeText(command);
      vscode.window.showInformationMessage('Command copied to clipboard.');
    } else if (choice === 'Open Download Page' && url) {
      vscode.env.openExternal(vscode.Uri.parse(url));
    }
  }

  /**
   * Show a robust, interactive setup wizard for SWI-Prolog installation/configuration
   */
  public async runSetupWizard(): Promise<void> {
    // Launch the new webview-based setup wizard
    SetupWizardWebview.getInstance().show(vscode.extensions.getExtension('mediaprophet.vscode-prolog-toolkit')?.extensionPath as any);
  }
}
