import * as vscode from 'vscode';
import { InstallationChecker } from './InstallationChecker';

export class SetupWizardWebview {
  private static instance: SetupWizardWebview;
  private panel: vscode.WebviewPanel | undefined;
  private installationChecker: InstallationChecker;

  private constructor() {
    this.installationChecker = InstallationChecker.getInstance();
  }

  public static getInstance(): SetupWizardWebview {
    if (!SetupWizardWebview.instance) {
      SetupWizardWebview.instance = new SetupWizardWebview();
    }
    return SetupWizardWebview.instance;
  }

  public show(context: vscode.ExtensionContext): void {
    if (this.panel) {
      this.panel.reveal();
      return;
    }
    this.panel = vscode.window.createWebviewPanel(
      'prologSetupWizard',
      'Prolog Setup Wizard',
      vscode.ViewColumn.One,
      {
        enableScripts: true,
        retainContextWhenHidden: true,
      }
    );
    this.panel.webview.html = this.getHtmlForWebview();
    this.panel.onDidDispose(() => {
      this.panel = undefined;
    });
    this.panel.webview.onDidReceiveMessage(async message => {
      if (message.command === 'openSettings') {
        // Open the settings webview view
        await vscode.commands.executeCommand('workbench.view.extension.prologSettings');
      }
      if (message.command === 'diagnostics') {
        const diagnostics = await this.installationChecker.performDiagnostics();
        this.panel?.webview.postMessage({ command: 'diagnosticsResult', diagnostics });
      }
      if (message.command === 'getPlatformInstructions') {
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
          command = 'sudo apt install swi-prolog';
          instructions = 'Run the following command in your terminal:';
        }
        this.panel?.webview.postMessage({ command: 'platformInstructions', instructions, url, installCommand: command });
      }
      if (message.command === 'reportIssue') {
        // Pre-fill diagnostics for user support
        const diagnostics = await this.installationChecker.performDiagnostics();
        const body = encodeURIComponent(
          '### Prolog Toolkit Setup Issue\n' +
          '#### Diagnostics\n' +
          '```json\n' + JSON.stringify(diagnostics, null, 2) + '\n```\n' +
          '#### Describe your issue here:\n'
        );
        const url = `https://github.com/mediaprophet/VSCode-Prolog-Toolkit/issues/new?body=${body}`;
        vscode.env.openExternal(vscode.Uri.parse(url));
      }

      // HTTP backend setup/diagnostics
      if (message.command === 'checkHttpBackend') {
        // Check VS Code settings for apiServer.enabled and port
        const config = vscode.workspace.getConfiguration('prolog');
        const enabled = config.get<boolean>('apiServer.enabled', false);
        const port = config.get<number>('apiServer.port', 3060);
        // Try to connect to the backend if enabled
        if (enabled) {
          try {
            const res = await fetch(`http://localhost:${port}/api/status`, { method: 'GET' });
            if (res.ok) {
              this.panel?.webview.postMessage({ command: 'httpBackendStatus', enabled: true, port });
            } else {
              this.panel?.webview.postMessage({ command: 'httpBackendStatus', enabled: false, port });
            }
          } catch (e) {
            this.panel?.webview.postMessage({ command: 'httpBackendStatus', enabled: false, port });
          }
        } else {
          this.panel?.webview.postMessage({ command: 'httpBackendStatus', enabled: false, port });
        }
      }
      if (message.command === 'testHttpBackend') {
        const config = vscode.workspace.getConfiguration('prolog');
        const port = config.get<number>('apiServer.port', 3060);
        try {
          const res = await fetch(`http://localhost:${port}/api/status`, { method: 'GET' });
          if (res.ok) {
            this.panel?.webview.postMessage({ command: 'httpBackendTestResult', success: true, port });
          } else {
            this.panel?.webview.postMessage({ command: 'httpBackendTestResult', success: false, port, error: 'Non-OK response' });
          }
        } catch (e: any) {
          this.panel?.webview.postMessage({ command: 'httpBackendTestResult', success: false, port, error: e.message });
        }
      }
      if (message.command === 'enableHttpBackend') {
        // Set apiServer.enabled to true in settings
        const config = vscode.workspace.getConfiguration('prolog');
        await config.update('apiServer.enabled', true, vscode.ConfigurationTarget.Global);
        // Optionally, prompt user to reload or restart backend
        this.panel?.webview.postMessage({ command: 'httpBackendStatus', enabled: true, port: config.get<number>('apiServer.port', 3060) });
      }
    });
  }

  private getHtmlForWebview(): string {
    // Enhanced multi-step wizard UI
    return `
      <!DOCTYPE html>
      <html lang="en">
      <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Prolog Setup Wizard</title>
        <style>
          body { font-family: sans-serif; margin: 0; padding: 0; }
          .wizard-step { display: none; }
          .wizard-step.active { display: block; }
          .wizard-nav { margin-top: 20px; }
          button { margin-right: 8px; }
          .recommendations { color: #1565c0; margin-top: 10px; }
          .error { color: #c62828; }
        </style>
      </head>
      <body>
        <h1>Prolog Setup Wizard</h1>
        <div id="step1" class="wizard-step active">
          <h2>Welcome</h2>
          <p>This wizard will help you set up SWI-Prolog for VS Code.</p>
          <button id="startWizardBtn">Start</button>
          <button id="openSettingsBtn" type="button">Open Settings</button>
        </div>
        <div id="step2" class="wizard-step">
          <h2>Diagnostics</h2>
          <p>Checking your system for SWI-Prolog...</p>
          <pre id="diagnosticsResult">(Running...)</pre>
          <div id="recommendations" class="recommendations"></div>
          <div class="wizard-nav">
            <button onclick="prevStep()">Back</button>
            <button onclick="nextStep()">Next</button>
            <button onclick="reportIssue()">Report Issue</button>
          </div>
        </div>
        <div id="step3" class="wizard-step">
          <h2>Platform-Specific Installation</h2>
          <p id="platformInstructions">Loading instructions...</p>
          <button id="copyInstallCommand" style="display:none">Copy Install Command</button>
          <button id="openDownloadPage" style="display:none">Open Download Page</button>
          <div class="wizard-nav">
            <button onclick="prevStep()">Back</button>
            <button onclick="nextStep()">Next</button>
          </div>
        </div>

        <div id="step4" class="wizard-step">
          <h2>HTTP Backend Configuration</h2>
          <div id="httpBackendStatus">Checking HTTP backend status...</div>
          <button id="enableHttpBackendBtn" style="display:none">Enable HTTP Backend</button>
          <button id="testHttpBackendBtn">Test HTTP Connectivity</button>
          <button id="showHttpHelpBtn" type="button">Show Troubleshooting Tips</button>
          <div id="httpHelpSection" style="display:none; margin-top:16px; border:1px solid #ccc; padding:12px; background:#f9f9f9;">
            <h3>Troubleshooting HTTP Backend</h3>
            <ul>
              <li><b>Backend not started:</b> Ensure SWI-Prolog is installed and the HTTP server is enabled in settings (<code>apiServer.enabled</code>).</li>
              <li><b>Port in use:</b> The default port (3060) may be in use. Change <code>apiServer.port</code> in settings or stop the conflicting service.</li>
              <li><b>Firewall/Antivirus:</b> Make sure your firewall or antivirus is not blocking port 3060 (or your configured port).</li>
              <li><b>Extension not running backend:</b> Try reloading VS Code or restarting your computer.</li>
              <li><b>Advanced:</b> Open a browser and visit <code>http://localhost:3060/api/status</code> (replace 3060 with your port). If you see a status message, the backend is running.</li>
            </ul>
            <p>See the <a href="https://github.com/mediaprophet/VSCode-Prolog-Toolkit/wiki/HTTP-Backend-Troubleshooting" target="_blank">HTTP Backend Troubleshooting Guide</a> for more help.</p>
          </div>
          <div class="wizard-nav">
            <button onclick="prevStep()">Back</button>
            <button onclick="nextStep()">Next</button>
          </div>
        </div>
        <div id="step5" class="wizard-step">
          <h2>Finish</h2>
          <div id="finishStatus"></div>
          <button onclick="window.close()">Close</button>
        </div>
  <script>
          let currentStep = 1;
          let lastDiagnostics = null;
          let httpBackendEnabled = false;
          function showStep(step) {
            document.querySelectorAll('.wizard-step').forEach((el, idx) => {
              el.classList.toggle('active', idx === step - 1);
            });
            if (step === 2) {
              window.acquireVsCodeApi().postMessage({ command: 'diagnostics' });
            }
            if (step === 3) {
              window.acquireVsCodeApi().postMessage({ command: 'getPlatformInstructions' });
            }
            if (step === 4) {
              window.acquireVsCodeApi().postMessage({ command: 'checkHttpBackend' });
            }
            if (step === 5) {
              // Show success/failure based on diagnostics and HTTP backend
              const finishStatus = document.getElementById('finishStatus');
              if (lastDiagnostics && lastDiagnostics.installation && lastDiagnostics.installation.isInstalled && httpBackendEnabled) {
                finishStatus.innerHTML = '<span>✅ SWI-Prolog and HTTP backend are ready!</span>';
              } else {
                finishStatus.innerHTML = '<span class="error">❌ SWI-Prolog or HTTP backend is not configured correctly.</span>';
              }
            }
          }
          function nextStep() {
            if (currentStep < 5) {
              currentStep++;
              showStep(currentStep);
            }
          }
          // Start button triggers backend check before advancing
          document.addEventListener('DOMContentLoaded', function() {
            document.getElementById('startWizardBtn').onclick = function() {
              // Show loading in diagnostics step
              currentStep = 2;
              showStep(currentStep);
              // Wait for diagnostics to complete before allowing next
            };
            document.getElementById('openSettingsBtn').onclick = function() {
              window.acquireVsCodeApi().postMessage({ command: 'openSettings' });
            };
          }, { passive: true });
          function prevStep() {
            if (currentStep > 1) {
              currentStep--;
              showStep(currentStep);
            }
          }
          function reportIssue() {
            window.acquireVsCodeApi().postMessage({ command: 'reportIssue' });
          }
          window.addEventListener('DOMContentLoaded', function() {
            document.getElementById('testHttpBackendBtn').onclick = function() {
              window.acquireVsCodeApi().postMessage({ command: 'testHttpBackend' });
            };
            document.getElementById('enableHttpBackendBtn').onclick = function() {
              window.acquireVsCodeApi().postMessage({ command: 'enableHttpBackend' });
            };
            document.getElementById('showHttpHelpBtn').onclick = function() {
              const help = document.getElementById('httpHelpSection');
              if (help.style.display === 'none') {
                help.style.display = '';
                this.textContent = 'Hide Troubleshooting Tips';
              } else {
                help.style.display = 'none';
                this.textContent = 'Show Troubleshooting Tips';
              }
            };
          }, { passive: true });
          window.addEventListener('message', event => {
            const msg = event.data;
            if (msg.command === 'diagnosticsResult') {
              lastDiagnostics = msg.diagnostics;
              document.getElementById('diagnosticsResult').textContent = JSON.stringify(msg.diagnostics, null, 2);
              // Show actionable recommendations
              const recDiv = document.getElementById('recommendations');
              if (msg.diagnostics && msg.diagnostics.recommendations && msg.diagnostics.recommendations.length > 0) {
                recDiv.innerHTML = '<b>Recommendations:</b><ul>' + msg.diagnostics.recommendations.map(function(r) { return '<li>' + r + '</li>'; }).join('') + '</ul>';
              } else {
                recDiv.innerHTML = '';
              }
            }
            if (msg.command === 'platformInstructions') {
              const el = document.getElementById('platformInstructions');
              el.textContent = msg.instructions + (msg.installCommand ? '\n\n' + msg.installCommand : '');
              // Show/hide buttons
              const copyBtn = document.getElementById('copyInstallCommand');
              const openBtn = document.getElementById('openDownloadPage');
              if (msg.installCommand) {
                copyBtn.style.display = '';
                copyBtn.onclick = function() { navigator.clipboard.writeText(msg.installCommand); };
              } else {
                copyBtn.style.display = 'none';
              }
              if (msg.url) {
                openBtn.style.display = '';
                openBtn.onclick = function() { window.open(msg.url, '_blank'); };
              } else {
                openBtn.style.display = 'none';
              }
            }
            if (msg.command === 'httpBackendStatus') {
              const statusDiv = document.getElementById('httpBackendStatus');
              const enableBtn = document.getElementById('enableHttpBackendBtn');
              httpBackendEnabled = msg.enabled;
              if (msg.enabled) {
                statusDiv.innerHTML = '<span>✅ HTTP backend is enabled and reachable on port ' + msg.port + '.</span>';
                enableBtn.style.display = 'none';
              } else {
                statusDiv.innerHTML = '<span class="error">❌ HTTP backend is not enabled or not reachable.</span>';
                enableBtn.style.display = '';
              }
            }
            if (msg.command === 'httpBackendTestResult') {
              const statusDiv = document.getElementById('httpBackendStatus');
              if (msg.success) {
                statusDiv.innerHTML = '<span>✅ Successfully connected to HTTP backend on port ' + msg.port + '.</span>';
                httpBackendEnabled = true;
              } else {
                statusDiv.innerHTML = '<span class="error">❌ Failed to connect to HTTP backend: ' + msg.error + '</span>';
                httpBackendEnabled = false;
              }
            }
          });
        </script>
      </body>
      </html>
    `;
  }
}
