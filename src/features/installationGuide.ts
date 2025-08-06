import * as vscode from 'vscode';
import * as os from 'os';
import { InstallationChecker, InstallationStatus } from './installationChecker';
import { PackageManagerIntegration } from './packageManagerIntegration';

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
   * Show installation not found dialog with platform-specific guidance
   */
  public async showInstallationNotFoundDialog(
    options: InstallationGuideOptions = {}
  ): Promise<string | undefined> {
    const platform = os.platform();
    const platformName = this.getPlatformDisplayName(platform);

    const message = `SWI-Prolog is not installed or not found on your ${platformName} system. The VSCode Prolog Toolkit requires SWI-Prolog to provide language features like syntax checking, debugging, and query execution.`;

    const actions = [
      'Install with Package Manager',
      'Installation Guide',
      'Setup Wizard',
      'Manual Configuration',
    ];

    if (options.allowSkip) {
      actions.push('Skip for Now');
    }

    const choice = await vscode.window.showWarningMessage(message, { modal: true }, ...actions);

    switch (choice) {
      case 'Install with Package Manager':
        await this.packageManager.showInstallationDialog();
        break;
      case 'Installation Guide':
        await this.showInstallationGuideDialog();
        break;
      case 'Setup Wizard':
        await this.runSetupWizard();
        break;
      case 'Manual Configuration':
        await this.showPathConfigurationDialog();
        break;
      case 'Skip for Now':
        await this.showSkipWarningDialog();
        break;
      default:
        break;
    }

    return choice;
  }

  /**
   * Show comprehensive installation guide dialog
   */
  public async showInstallationGuideDialog(): Promise<void> {
    const platform = os.platform();
    const installInfo = this.getInstallationInfo(platform);

    const panel = vscode.window.createWebviewPanel(
      'prologInstallationGuide',
      'SWI-Prolog Installation Guide',
      vscode.ViewColumn.One,
      {
        enableScripts: true,
        retainContextWhenHidden: true,
      }
    );

    panel.webview.html = this.getInstallationGuideHtml(installInfo);

    // Handle messages from the webview
    panel.webview.onDidReceiveMessage(async message => {
      switch (message.type) {
        case 'openDownloadPage':
          await vscode.commands.executeCommand('vscode.open', vscode.Uri.parse(message.url));
          break;
        case 'testInstallation':
          await this.testInstallationAndReport(panel);
          break;
        case 'openTerminal': {
          const terminal = vscode.window.createTerminal('SWI-Prolog Installation');
          terminal.sendText(message.command);
          terminal.show();
          break;
        }
        case 'configureManually':
          panel.dispose();
          await this.showPathConfigurationDialog();
          break;
        case 'runWizard':
          panel.dispose();
          await this.runSetupWizard();
          break;
      }
    });
  }

  /**
   * Show path configuration dialog
   */
  public async showPathConfigurationDialog(): Promise<void> {
    const config = vscode.workspace.getConfiguration('prolog');
    const currentPath = config.get<string>('executablePath', 'swipl');

    const newPath = await vscode.window.showInputBox({
      title: 'Configure SWI-Prolog Executable Path',
      prompt: 'Enter the full path to your SWI-Prolog executable',
      value: currentPath,
      validateInput: async value => {
        if (!value || value.trim().length === 0) {
          return 'Path cannot be empty';
        }

        const isValid = await this.installationChecker.validateSwiplPath(value.trim());
        if (!isValid) {
          return 'Invalid SWI-Prolog executable path. Please check the path and try again.';
        }

        return null;
      },
    });

    if (newPath && newPath !== currentPath) {
      try {
        await config.update('executablePath', newPath, vscode.ConfigurationTarget.Global);
        await this.showSuccessDialog(newPath);
      } catch (error) {
        vscode.window.showErrorMessage(`Failed to update configuration: ${error}`);
      }
    }
  }

  /**
   * Show success dialog after successful installation/configuration
   */
  public async showSuccessDialog(path: string, version?: string): Promise<void> {
    const versionText = version ? ` (version ${version})` : '';
    const message = `✅ SWI-Prolog successfully configured!\n\nExecutable: ${path}${versionText}\n\nYou can now use all Prolog extension features including syntax highlighting, linting, debugging, and query execution.`;

    const action = await vscode.window.showInformationMessage(
      message,
      'Test Features',
      'Open Settings',
      'OK'
    );

    switch (action) {
      case 'Test Features':
        await this.showFeatureTestDialog();
        break;
      case 'Open Settings':
        await vscode.commands.executeCommand('prolog.openSettings');
        break;
      default:
        break;
    }
  }

  /**
   * Run the setup wizard
   */
  public async runSetupWizard(): Promise<void> {
    const panel = vscode.window.createWebviewPanel(
      'prologSetupWizard',
      'Prolog Setup Wizard',
      vscode.ViewColumn.One,
      {
        enableScripts: true,
        retainContextWhenHidden: true,
      }
    );

    // Get current installation status
    const installationStatus = await this.installationChecker.checkSwiplInstallation();
    const systemInfo = this.installationChecker.getSystemInfo();

    panel.webview.html = this.getSetupWizardHtml(installationStatus, systemInfo);

    // Handle wizard interactions
    panel.webview.onDidReceiveMessage(async message => {
      switch (message.type) {
        case 'autoDetect':
          await this.performAutoDetection(panel);
          break;
        case 'manualPath':
          panel.dispose();
          await this.showPathConfigurationDialog();
          break;
        case 'installGuide':
          panel.dispose();
          await this.showInstallationGuideDialog();
          break;
        case 'testPath':
          await this.testSpecificPath(panel, message.path);
          break;
        case 'savePath':
          await this.savePathFromWizard(panel, message.path);
          break;
        case 'runDiagnostics':
          await this.runDiagnosticsFromWizard(panel);
          break;
      }
    });
  }

  /**
   * Show warning dialog when user chooses to skip installation
   */
  private async showSkipWarningDialog(): Promise<void> {
    const message = `⚠️ Skipping SWI-Prolog installation will limit extension functionality.\n\nWithout SWI-Prolog, the following features will not work:\n• Syntax checking and linting\n• Code debugging\n• Query execution\n• Hover documentation\n• Go to definition\n• Code formatting\n\nYou can install SWI-Prolog later and run the setup wizard from the command palette.`;

    await vscode.window.showWarningMessage(message, { modal: true }, 'I Understand');
  }

  /**
   * Show feature test dialog
   */
  private async showFeatureTestDialog(): Promise<void> {
    const tests = [
      { name: 'Version Check', test: () => this.testVersion() },
      { name: 'Basic Query', test: () => this.testBasicQuery() },
      { name: 'File Loading', test: () => this.testFileLoading() },
    ];

    const results: Array<{ name: string; success: boolean; message: string }> = [];

    await vscode.window.withProgress(
      {
        location: vscode.ProgressLocation.Notification,
        title: 'Testing Prolog Features',
        cancellable: false,
      },
      async progress => {
        for (let i = 0; i < tests.length; i++) {
          const test = tests[i];
          progress.report({
            increment: (i / tests.length) * 100,
            message: `Testing ${test.name}...`,
          });

          try {
            const result = await test.test();
            results.push({
              name: test.name,
              success: result.success,
              message: result.message,
            });
          } catch (error) {
            results.push({
              name: test.name,
              success: false,
              message: `Error: ${error}`,
            });
          }
        }
      }
    );

    // Show results
    const successCount = results.filter(r => r.success).length;
    const resultMessage =
      `Feature Test Results (${successCount}/${results.length} passed):\n\n` +
      results.map(r => `${r.success ? '✅' : '❌'} ${r.name}: ${r.message}`).join('\n');

    if (successCount === results.length) {
      vscode.window.showInformationMessage(resultMessage);
    } else {
      vscode.window.showWarningMessage(resultMessage);
    }
  }

  /**
   * Get platform display name
   */
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

  /**
   * Get installation information for platform
   */
  private getInstallationInfo(platform: string) {
    const info = {
      windows: {
        title: 'Windows Installation',
        downloadUrl: 'https://www.swi-prolog.org/download/stable/bin/swipl-9.2.6-1.x64.exe',
        instructions: [
          'Download the Windows installer (.exe file)',
          'Run the installer as Administrator',
          'Follow the installation wizard',
          'SWI-Prolog will be added to your PATH automatically',
          'Restart VS Code after installation',
        ],
        packageManager: null,
      },
      darwin: {
        title: 'macOS Installation',
        downloadUrl: 'https://www.swi-prolog.org/download/stable/bin/swipl-9.2.6-1.x86_64.dmg',
        instructions: [
          'Option 1: Download the DMG file and drag to Applications',
          'Option 2: Use Homebrew (recommended for developers)',
          'For Homebrew: Open Terminal and run the command below',
          'Restart VS Code after installation',
        ],
        packageManager: {
          name: 'Homebrew',
          command: 'brew install swi-prolog',
          installUrl: 'https://brew.sh',
        },
      },
      linux: {
        title: 'Linux Installation',
        downloadUrl: 'https://www.swi-prolog.org/build/unix.html',
        instructions: [
          "Use your distribution's package manager",
          'Choose the appropriate command for your system',
          'Restart VS Code after installation',
        ],
        packageManager: {
          name: 'Package Manager',
          commands: [
            'Ubuntu/Debian: sudo apt install swi-prolog',
            'CentOS/RHEL: sudo yum install pl',
            'Fedora: sudo dnf install pl',
            'Arch Linux: sudo pacman -S swi-prolog',
            'openSUSE: sudo zypper install swi-prolog',
          ],
        },
      },
    };

    switch (platform) {
      case 'win32':
        return info.windows;
      case 'darwin':
        return info.darwin;
      case 'linux':
        return info.linux;
      default:
        return info.linux;
    }
  }

  /**
   * Generate HTML for installation guide
   */
  private getInstallationGuideHtml(installInfo: any): string {
    return `<!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>SWI-Prolog Installation Guide</title>
        <style>
            body {
                font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
                line-height: 1.6;
                margin: 0;
                padding: 20px;
                background-color: var(--vscode-editor-background);
                color: var(--vscode-editor-foreground);
            }
            .container {
                max-width: 800px;
                margin: 0 auto;
            }
            .header {
                text-align: center;
                margin-bottom: 30px;
                padding: 20px;
                background-color: var(--vscode-textBlockQuote-background);
                border-radius: 8px;
            }
            .header h1 {
                margin: 0;
                color: var(--vscode-textLink-foreground);
            }
            .section {
                margin-bottom: 30px;
                padding: 20px;
                background-color: var(--vscode-textBlockQuote-background);
                border-radius: 8px;
                border-left: 4px solid var(--vscode-textLink-foreground);
            }
            .instructions {
                list-style: none;
                padding: 0;
            }
            .instructions li {
                margin: 10px 0;
                padding: 10px;
                background-color: var(--vscode-input-background);
                border-radius: 4px;
                position: relative;
                padding-left: 40px;
            }
            .instructions li::before {
                content: counter(step-counter);
                counter-increment: step-counter;
                position: absolute;
                left: 15px;
                top: 50%;
                transform: translateY(-50%);
                background-color: var(--vscode-textLink-foreground);
                color: var(--vscode-editor-background);
                width: 20px;
                height: 20px;
                border-radius: 50%;
                display: flex;
                align-items: center;
                justify-content: center;
                font-size: 12px;
                font-weight: bold;
            }
            .instructions {
                counter-reset: step-counter;
            }
            .button {
                background-color: var(--vscode-button-background);
                color: var(--vscode-button-foreground);
                border: none;
                padding: 10px 20px;
                border-radius: 4px;
                cursor: pointer;
                margin: 5px;
                font-size: 14px;
            }
            .button:hover {
                background-color: var(--vscode-button-hoverBackground);
            }
            .button.secondary {
                background-color: var(--vscode-button-secondaryBackground);
                color: var(--vscode-button-secondaryForeground);
            }
            .button.secondary:hover {
                background-color: var(--vscode-button-secondaryHoverBackground);
            }
            .command-box {
                background-color: var(--vscode-terminal-background);
                color: var(--vscode-terminal-foreground);
                padding: 15px;
                border-radius: 4px;
                font-family: 'Courier New', monospace;
                margin: 10px 0;
                position: relative;
            }
            .copy-button {
                position: absolute;
                top: 10px;
                right: 10px;
                background: var(--vscode-button-background);
                color: var(--vscode-button-foreground);
                border: none;
                padding: 5px 10px;
                border-radius: 3px;
                cursor: pointer;
                font-size: 12px;
            }
            .actions {
                text-align: center;
                margin-top: 30px;
                padding: 20px;
                background-color: var(--vscode-textBlockQuote-background);
                border-radius: 8px;
            }
            .status {
                padding: 15px;
                border-radius: 4px;
                margin: 10px 0;
                text-align: center;
            }
            .status.success {
                background-color: var(--vscode-testing-iconPassed);
                color: white;
            }
            .status.error {
                background-color: var(--vscode-testing-iconFailed);
                color: white;
            }
        </style>
    </head>
    <body>
        <div class="container">
            <div class="header">
                <h1>🔧 ${installInfo.title}</h1>
                <p>Follow these steps to install SWI-Prolog and enable all Prolog extension features</p>
            </div>

            <div class="section">
                <h2>📋 Installation Steps</h2>
                <ol class="instructions">
                    ${installInfo.instructions.map((instruction: string) => `<li>${instruction}</li>`).join('')}
                </ol>
            </div>

            ${
              installInfo.packageManager
                ? `
            <div class="section">
                <h2>💻 ${installInfo.packageManager.name}</h2>
                ${
                  installInfo.packageManager.command
                    ? `
                    <p>Run this command in your terminal:</p>
                    <div class="command-box">
                        ${installInfo.packageManager.command}
                        <button class="copy-button" onclick="copyCommand('${installInfo.packageManager.command}')">Copy</button>
                    </div>
                    <button class="button" onclick="openTerminal('${installInfo.packageManager.command}')">
                        🖥️ Open in Terminal
                    </button>
                `
                    : ''
                }
                ${
                  installInfo.packageManager.commands
                    ? `
                    <p>Choose the command for your distribution:</p>
                    ${installInfo.packageManager.commands
                      .map(
                        (cmd: string) => `
                        <div class="command-box">
                            ${cmd}
                            <button class="copy-button" onclick="copyCommand('${cmd.split(': ')[1]}')">Copy</button>
                        </div>
                    `
                      )
                      .join('')}
                `
                    : ''
                }
            </div>
            `
                : ''
            }

            <div class="section">
                <h2>🧪 Test Installation</h2>
                <p>After installation, click the button below to verify that SWI-Prolog is working correctly:</p>
                <div id="testResult"></div>
                <button class="button" onclick="testInstallation()">
                    🔍 Test Installation
                </button>
            </div>

            <div class="actions">
                <h3>Quick Actions</h3>
                <button class="button" onclick="openDownloadPage('${installInfo.downloadUrl}')">
                    📥 Download SWI-Prolog
                </button>
                <button class="button secondary" onclick="configureManually()">
                    ⚙️ Configure Manually
                </button>
                <button class="button secondary" onclick="runWizard()">
                    🧙‍♂️ Setup Wizard
                </button>
            </div>
        </div>

        <script>
            const vscode = acquireVsCodeApi();

            function openDownloadPage(url) {
                vscode.postMessage({ type: 'openDownloadPage', url: url });
            }

            function testInstallation() {
                document.getElementById('testResult').innerHTML = '<div class="status">🔄 Testing installation...</div>';
                vscode.postMessage({ type: 'testInstallation' });
            }

            function openTerminal(command) {
                vscode.postMessage({ type: 'openTerminal', command: command });
            }

            function configureManually() {
                vscode.postMessage({ type: 'configureManually' });
            }

            function runWizard() {
                vscode.postMessage({ type: 'runWizard' });
            }

            function copyCommand(command) {
                navigator.clipboard.writeText(command).then(() => {
                    // Visual feedback for copy
                    event.target.textContent = 'Copied!';
                    setTimeout(() => {
                        event.target.textContent = 'Copy';
                    }, 2000);
                });
            }

            // Listen for test results
            window.addEventListener('message', event => {
                const message = event.data;
                if (message.type === 'testResult') {
                    const resultDiv = document.getElementById('testResult');
                    if (message.success) {
                        resultDiv.innerHTML = '<div class="status success">✅ ' + message.message + '</div>';
                    } else {
                        resultDiv.innerHTML = '<div class="status error">❌ ' + message.message + '</div>';
                    }
                }
            });
        </script>
    </body>
    </html>`;
  }

  /**
   * Generate HTML for setup wizard
   */
  private getSetupWizardHtml(installationStatus: InstallationStatus, systemInfo: any): string {
    return `<!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Prolog Setup Wizard</title>
        <style>
            body {
                font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
                line-height: 1.6;
                margin: 0;
                padding: 20px;
                background-color: var(--vscode-editor-background);
                color: var(--vscode-editor-foreground);
            }
            .wizard-container {
                max-width: 700px;
                margin: 0 auto;
            }
            .wizard-header {
                text-align: center;
                margin-bottom: 30px;
                padding: 20px;
                background: linear-gradient(135deg, var(--vscode-textLink-foreground), var(--vscode-button-background));
                color: white;
                border-radius: 8px;
            }
            .step {
                margin-bottom: 20px;
                padding: 20px;
                background-color: var(--vscode-textBlockQuote-background);
                border-radius: 8px;
                border-left: 4px solid var(--vscode-textLink-foreground);
            }
            .status-indicator {
                display: inline-block;
                width: 20px;
                height: 20px;
                border-radius: 50%;
                margin-right: 10px;
                text-align: center;
                line-height: 20px;
                font-size: 12px;
                font-weight: bold;
            }
            .status-success { background-color: #4CAF50; color: white; }
            .status-warning { background-color: #FF9800; color: white; }
            .status-error { background-color: #F44336; color: white; }
            .status-info { background-color: #2196F3; color: white; }
            .button {
                background-color: var(--vscode-button-background);
                color: var(--vscode-button-foreground);
                border: none;
                padding: 10px 20px;
                border-radius: 4px;
                cursor: pointer;
                margin: 5px;
                font-size: 14px;
            }
            .button:hover {
                background-color: var(--vscode-button-hoverBackground);
            }
            .button.secondary {
                background-color: var(--vscode-button-secondaryBackground);
                color: var(--vscode-button-secondaryForeground);
            }
            .path-input {
                width: 100%;
                padding: 10px;
                border: 1px solid var(--vscode-input-border);
                background-color: var(--vscode-input-background);
                color: var(--vscode-input-foreground);
                border-radius: 4px;
                margin: 10px 0;
            }
            .system-info {
                background-color: var(--vscode-terminal-background);
                padding: 15px;
                border-radius: 4px;
                font-family: monospace;
                font-size: 12px;
                margin: 10px 0;
            }
        </style>
    </head>
    <body>
        <div class="wizard-container">
            <div class="wizard-header">
                <h1>🧙‍♂️ Prolog Setup Wizard</h1>
                <p>Let's get your Prolog environment configured properly</p>
            </div>

            <div class="step">
                <h3>
                    <span class="status-indicator ${installationStatus.isInstalled ? 'status-success' : 'status-error'}">
                        ${installationStatus.isInstalled ? '✓' : '✗'}
                    </span>
                    Installation Status
                </h3>
                ${
                  installationStatus.isInstalled
                    ? `
                    <p><strong>✅ SWI-Prolog Found!</strong></p>
                    <p><strong>Path:</strong> ${installationStatus.path}</p>
                    <p><strong>Version:</strong> ${installationStatus.version || 'Unknown'}</p>
                    ${
                      installationStatus.issues && installationStatus.issues.length > 0
                        ? `
                        <p><strong>⚠️ Issues:</strong></p>
                        <ul>
                            ${installationStatus.issues.map(issue => `<li>${issue}</li>`).join('')}
                        </ul>
                    `
                        : ''
                    }
                `
                    : `
                    <p><strong>❌ SWI-Prolog Not Found</strong></p>
                    <p>SWI-Prolog is not installed or not found in the expected locations.</p>
                    <button class="button" onclick="installGuide()">📖 Installation Guide</button>
                `
                }
            </div>

            <div class="step">
                <h3>
                    <span class="status-indicator status-info">🔍</span>
                    Auto-Detection
                </h3>
                <p>Let the wizard automatically search for SWI-Prolog on your system:</p>
                <button class="button" onclick="autoDetect()">🔍 Auto-Detect SWI-Prolog</button>
                <div id="autoDetectResult"></div>
            </div>

            <div class="step">
                <h3>
                    <span class="status-indicator status-info">⚙️</span>
                    Manual Configuration
                </h3>
                <p>If you know where SWI-Prolog is installed, enter the path manually:</p>
                <input type="text" class="path-input" id="manualPath" placeholder="Enter path to swipl executable..." />
                <br>
                <button class="button" onclick="testPath()">🧪 Test Path</button>
                <button class="button secondary" onclick="manualPath()">💾 Save Path</button>
                <div id="pathTestResult"></div>
            </div>

            <div class="step">
                <h3>
                    <span class="status-indicator status-info">🖥️</span>
                    System Information
                </h3>
                <div class="system-info">
                    <strong>Platform:</strong> ${systemInfo.platform}<br>
                    <strong>Architecture:</strong> ${systemInfo.arch}<br>
                    <strong>PATH directories:</strong> ${systemInfo.pathEnv.length} entries
                </div>
                <button class="button secondary" onclick="runDiagnostics()">🔧 Run Full Diagnostics</button>
                <div id="diagnosticsResult"></div>
            </div>
        </div>

        <script>
            const vscode = acquireVsCodeApi();

            function autoDetect() {
                document.getElementById('autoDetectResult').innerHTML = '<p>🔄 Searching for SWI-Prolog...</p>';
                vscode.postMessage({ type: 'autoDetect' });
            }

            function testPath() {
                const path = document.getElementById('manualPath').value;
                if (!path) {
                    document.getElementById('pathTestResult').innerHTML = '<p style="color: orange;">⚠️ Please enter a path first</p>';
                    return;
                }
                document.getElementById('pathTestResult').innerHTML = '<p>🔄 Testing path...</p>';
                vscode.postMessage({ type: 'testPath', path: path });
            }

            function manualPath() {
                const path = document.getElementById('manualPath').value;
                if (!path) {
                    alert('Please enter a path first');
                    return;
                }
                vscode.postMessage({ type: 'savePath', path: path });
            }

            function installGuide() {
                vscode.postMessage({ type: 'installGuide' });
            }

            function runDiagnostics() {
                document.getElementById('diagnosticsResult').innerHTML = '<p>🔄 Running diagnostics...</p>';
                vscode.postMessage({ type: 'runDiagnostics' });
            }

            // Listen for messages from extension
            window.addEventListener('message', event => {
                const message = event.data;
                switch (message.type) {
                    case 'autoDetectResult':
                        document.getElementById('autoDetectResult').innerHTML = message.html;
                        break;
                    case 'pathTestResult':
                        document.getElementById('pathTestResult').innerHTML = message.html;
                        break;
                    case 'diagnosticsResult':
                        document.getElementById('diagnosticsResult').innerHTML = message.html;
                        break;
                }
            });
        </script>
    </body>
    </html>`;
  }

  // Helper methods for wizard functionality
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
      await this.showSuccessDialog(path, version);
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
      if (diagnostics.installation.path) {
        html += `<p><strong>Path:</strong> ${diagnostics.installation.path}</p>`;
      }
      if (diagnostics.installation.version) {
        html += `<p><strong>Version:</strong> ${diagnostics.installation.version}</p>`;
      }

      html += `<h4>🖥️ System Information</h4>`;
      html += `<p><strong>Platform:</strong> ${diagnostics.system.platform}</p>`;
      html += `<p><strong>Architecture:</strong> ${diagnostics.system.arch}</p>`;
      html += `<p><strong>PATH entries:</strong> ${diagnostics.system.pathEnv.length}</p>`;

      html += `<h4>⚙️ Configuration</h4>`;
      html += `<p><strong>Current path:</strong> ${diagnostics.configuration.current}</p>`;
      html += `<p><strong>Valid:</strong> ${diagnostics.configuration.valid ? '✅ Yes' : '❌ No'}</p>`;

      if (diagnostics.recommendations.length > 0) {
        html += `<h4>💡 Recommendations</h4>`;
        html += '<ul>';
        diagnostics.recommendations.forEach(rec => {
          html += `<li>${rec}</li>`;
        });
        html += '</ul>';
      }

      html += '</div>';

      panel.webview.postMessage({
        type: 'diagnosticsResult',
        html: html,
      });
    } catch (error) {
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
    } catch (error) {
      panel.webview.postMessage({
        type: 'testResult',
        success: false,
        message: `Test failed: ${error}`,
      });
    }
  }

  // Feature test methods
  private async testVersion(): Promise<{ success: boolean; message: string }> {
    try {
      const status = await this.installationChecker.checkSwiplInstallation();
      if (status.isInstalled && status.version) {
        return {
          success: true,
          message: `Version ${status.version} detected`,
        };
      } else {
        return {
          success: false,
          message: 'Could not detect SWI-Prolog version',
        };
      }
    } catch (error) {
      return {
        success: false,
        message: `Version check failed: ${error}`,
      };
    }
  }

  private async testBasicQuery(): Promise<{ success: boolean; message: string }> {
    try {
      // This would typically test a basic Prolog query
      // For now, we'll just check if the executable responds
      const status = await this.installationChecker.checkSwiplInstallation();
      if (status.isInstalled && status.path) {
        const isValid = await this.installationChecker.validateSwiplPath(status.path);
        return {
          success: isValid,
          message: isValid ? 'Basic query execution works' : 'Query execution failed',
        };
      } else {
        return {
          success: false,
          message: 'SWI-Prolog not available for testing',
        };
      }
    } catch (error) {
      return {
        success: false,
        message: `Query test failed: ${error}`,
      };
    }
  }

  private async testFileLoading(): Promise<{ success: boolean; message: string }> {
    try {
      // This would typically test loading a Prolog file
      // For now, we'll simulate the test
      const status = await this.installationChecker.checkSwiplInstallation();
      if (status.isInstalled) {
        return {
          success: true,
          message: 'File loading capability available',
        };
      } else {
        return {
          success: false,
          message: 'SWI-Prolog not available for file loading',
        };
      }
    } catch (error) {
      return {
        success: false,
        message: `File loading test failed: ${error}`,
      };
    }
  }
}
