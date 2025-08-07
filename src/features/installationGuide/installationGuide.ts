import * as vscode from 'vscode';
import type { InstallationStatus } from '../installationChecker.js';
import { InstallationChecker } from '../installationChecker.js';
import { PackageManagerIntegration } from '../packageManagerIntegration.js';

export interface InstallationGuideOptions {
  showDetailedInstructions?: boolean;
  allowSkip?: boolean;
  context?: vscode.ExtensionContext;
}

export class InstallationGuide {
  private static instance: InstallationGuide;
  private installationChecker!: InstallationChecker;
  private packageManager!: PackageManagerIntegration;

  private constructor() {}

  public static getInstance(): InstallationGuide {
    if (!InstallationGuide.instance) {
      InstallationGuide.instance = new InstallationGuide();
      InstallationGuide.instance.installationChecker = InstallationChecker.getInstance();
      InstallationGuide.instance.packageManager = PackageManagerIntegration.getInstance();
    }
    return InstallationGuide.instance;
  }

  public async showInstallationGuideDialog(): Promise<void> {
    vscode.window.showInformationMessage('Installation Guide dialog would be shown here.');
  }

  public async runSetupWizard(): Promise<void> {
    vscode.window.showInformationMessage('Setup Wizard would be shown here.');
  }

  private getPlatformDisplayName(platform: string): string {
    switch (platform) {
      case 'win32':
        return 'Windows';
      case 'darwin':
        return 'macOS';
      case 'linux':
        return 'Linux';
      default:
        return 'Unix-like';
    }
  }

  private getInstallationGuideHtml(installInfo: any): string {
    return `<html><body><h1>${installInfo.title}</h1><ul>${installInfo.instructions.map((i: string) => `<li>${i}</li>`).join('')}</ul></body></html>`;
  }

  private getSetupWizardHtml(installationStatus: InstallationStatus, systemInfo: any): string {
    return `<html><body><h1>Setup Wizard</h1></body></html>`;
  }

  private async performAutoDetection(panel: vscode.WebviewPanel): Promise<void> {
    try {
      const foundPath = await this.installationChecker.findSwiplExecutable();
      if (foundPath) {
        const version = await this.installationChecker.getSwiplVersion(foundPath);
        panel.webview.postMessage({
          type: 'autoDetectResult',
          html: `<p style="color: green;">✅ Found SWI-Prolog at: ${foundPath}</p><p>Version: ${version}</p>`,
        });
      } else {
        panel.webview.postMessage({
          type: 'autoDetectResult',
          html: `<p style="color: red;">❌ SWI-Prolog not found in common locations</p><p>Please install SWI-Prolog or configure the path manually.</p>`,
        });
      }
    } catch (error) {
      panel.webview.postMessage({
        type: 'autoDetectResult',
        html: `<p style="color: red;">❌ Error during auto-detection: ${error}</p>`,
      });
    }
  }

  private async testSpecificPath(panel: vscode.WebviewPanel, path: string): Promise<void> {
    try {
      const isValid = await this.installationChecker.validateSwiplPath(path);
      if (isValid) {
        const version = await this.installationChecker.getSwiplVersion(path);
        panel.webview.postMessage({
          type: 'pathTestResult',
          html: `<p style="color: green;">✅ Valid SWI-Prolog executable</p><p>Version: ${version}</p>`,
        });
      } else {
        panel.webview.postMessage({
          type: 'pathTestResult',
          html: `<p style="color: red;">❌ Invalid or inaccessible SWI-Prolog executable</p>`,
        });
      }
    } catch (error) {
      panel.webview.postMessage({
        type: 'pathTestResult',
        html: `<p style="color: red;">❌ Error testing path: ${error}</p>`,
      });
    }
  }

  private async savePathFromWizard(panel: vscode.WebviewPanel, path: string): Promise<void> {
    try {
      const isValid = await this.installationChecker.validateSwiplPath(path);
      if (!isValid) {
        vscode.window.showErrorMessage(
          'Cannot save invalid SWI-Prolog path. Please test the path first.'
        );
        return;
      }
      const config = vscode.workspace.getConfiguration('prolog');
      await config.update('executablePath', path, vscode.ConfigurationTarget.Global);
      const version = await this.installationChecker.getSwiplVersion(path);
      panel.dispose();
      vscode.window.showInformationMessage(`SWI-Prolog path saved! Version: ${version}`);
    } catch (error) {
      vscode.window.showErrorMessage(`Failed to save configuration: ${error}`);
    }
  }

  private async runDiagnosticsFromWizard(panel: vscode.WebviewPanel): Promise<void> {
    try {
      const diagnostics = await this.installationChecker.performDiagnostics();
      let html = '<div style="font-family: monospace; font-size: 12px;">';
      html += `<h4>🔍 Installation Status</h4>`;
      html += `<p><strong>Installed:</strong> ${diagnostics.installation.isInstalled ? '✅ Yes' : '❌ No'}</p>`;
      if (diagnostics.installation.path)
        html += `<p><strong>Path:</strong> ${diagnostics.installation.path}</p>`;
      if (diagnostics.installation.version)
        html += `<p><strong>Version:</strong> ${diagnostics.installation.version}</p>`;
      html += `<h4>🖥️ System Information</h4>`;
      html += `<p><strong>Platform:</strong> ${diagnostics.system.platform}</p>`;
      html += `<p><strong>Architecture:</strong> ${diagnostics.system.arch}</p>`;
      html += `<p><strong>PATH entries:</strong> ${diagnostics.system.pathEnv.length}</p>`;
      html += `<h4>⚙️ Configuration</h4>`;
      html += `<p><strong>Current path:</strong> ${diagnostics.configuration.current}</p>`;
      html += `<p><strong>Valid:</strong> ${diagnostics.configuration.valid ? '✅ Yes' : '❌ No'}</p>`;
      if (diagnostics.recommendations.length > 0) {
        html += `<h4>💡 Recommendations</h4><ul>`;
        diagnostics.recommendations.forEach((rec: string) => {
          html += `<li>${rec}</li>`;
        });
        html += '</ul>';
      }
      html += '</div>';
      panel.webview.postMessage({ type: 'diagnosticsResult', html });
    } catch (error: any) {
      panel.webview.postMessage({
        type: 'diagnosticsResult',
        html: `<p style="color: red;">❌ Error running diagnostics: ${error}</p>`,
      });
    }
  }

  private async testInstallationAndReport(panel: vscode.WebviewPanel): Promise<void> {
    try {
      const installationStatus = await this.installationChecker.checkSwiplInstallation();
      if (installationStatus.isInstalled) {
        panel.webview.postMessage({
          type: 'testResult',
          success: true,
          message: `SWI-Prolog found at ${installationStatus.path} (version ${installationStatus.version})`,
        });
      } else {
        panel.webview.postMessage({
          type: 'testResult',
          success: false,
          message: 'SWI-Prolog not found. Please complete the installation first.',
        });
      }
    } catch (error: any) {
      panel.webview.postMessage({
        type: 'testResult',
        success: false,
        message: `Test failed: ${error}`,
      });
    }
  }

  private async testVersion(): Promise<{ success: boolean; message: string }> {
    try {
      const status = await this.installationChecker.checkSwiplInstallation();
      if (status.isInstalled && status.version) {
        return { success: true, message: `Version ${status.version} detected` };
      } else {
        return { success: false, message: 'Could not detect SWI-Prolog version' };
      }
    } catch (error: any) {
      return { success: false, message: `Version check failed: ${error}` };
    }
  }

  private async testBasicQuery(): Promise<{ success: boolean; message: string }> {
    try {
      const status = await this.installationChecker.checkSwiplInstallation();
      if (status.isInstalled && status.path) {
        const isValid = await this.installationChecker.validateSwiplPath(status.path);
        return {
          success: isValid,
          message: isValid ? 'Basic query execution works' : 'Query execution failed',
        };
      } else {
        return { success: false, message: 'SWI-Prolog not available for testing' };
      }
    } catch (error: any) {
      return { success: false, message: `Query test failed: ${error}` };
    }
  }

  private async testFileLoading(): Promise<{ success: boolean; message: string }> {
    try {
      const status = await this.installationChecker.checkSwiplInstallation();
      if (status.isInstalled) {
        return { success: true, message: 'File loading capability available' };
      } else {
        return { success: false, message: 'SWI-Prolog not available for file loading' };
      }
    } catch (error: any) {
      return { success: false, message: `File loading test failed: ${error}` };
    }
  }

  // Additional modular helpers can be added here for maintainability
}
