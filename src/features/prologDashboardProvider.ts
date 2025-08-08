import * as path from 'path';
import * as vscode from 'vscode';
import { InstallationChecker } from './installationChecker';
import { QueryHistoryOrchestrator } from './queryHistoryManager/QueryHistoryOrchestrator';

export class PrologDashboardProvider implements vscode.WebviewViewProvider {
  public static readonly viewType = 'prologDashboard';
  private _view?: vscode.WebviewView;
  private installationChecker: InstallationChecker;
  private queryHistory: QueryHistoryOrchestrator;

  constructor(private readonly _extensionUri: vscode.Uri) {
    this.installationChecker = InstallationChecker.getInstance();
    this.queryHistory = new QueryHistoryOrchestrator();
  }

  public resolveWebviewView(
    webviewView: vscode.WebviewView,
    context: vscode.WebviewViewResolveContext,
    _token: vscode.CancellationToken
  ) {
    this._view = webviewView;

    webviewView.webview.options = {
      enableScripts: true,
      localResourceRoots: [this._extensionUri],
    };

    webviewView.webview.html = this._getHtmlForWebview(webviewView.webview);

    // Handle messages from the webview
    webviewView.webview.onDidReceiveMessage(
      async message => {
        switch (message.type) {
          case 'executeQuery':
            await this._executeQuery(message.query);
            break;
          case 'openSettings':
            vscode.commands.executeCommand('prolog.openSettings');
            break;
          case 'setupWizard':
            vscode.commands.executeCommand('prolog.setupWizard');
            break;
          case 'refreshStatus':
            await this._refreshStatus();
            break;
          case 'openFile':
            if (message.filePath) {
              const uri = vscode.Uri.file(message.filePath);
              await vscode.window.showTextDocument(uri);
            }
            break;
          case 'newFile':
            await this._createNewFile();
            break;
          case 'clearHistory':
            await this._clearHistory();
            break;
        }
      },
      undefined,
      []
    );

    // Initial data load
    this._refreshStatus();
  }

  private async _executeQuery(query: string): Promise<void> {
    try {
      await vscode.commands.executeCommand('prolog.lsp.executeQuery', query);
      this._refreshStatus(); // Refresh to update history
    } catch (error) {
      vscode.window.showErrorMessage(`Failed to execute query: ${error}`);
    }
  }

  private async _refreshStatus(): Promise<void> {
    if (!this._view) {
      return;
    }

    try {
      // Get installation status
      const installationStatus = await this.installationChecker.checkSwiplInstallation();

      // Get recent queries
      const recentQueries: any[] = await this.queryHistory.getHistory({ limit: 5 });

      // Get workspace Prolog files
      const prologFiles = vscode.workspace.workspaceFolders
        ? await vscode.workspace.findFiles('**/*.{pl,pro,prolog,plt,ecl}', '**/node_modules/**', 10)
        : [];

      // Get debug session status
      const debugSession = vscode.debug.activeDebugSession;
      const isDebugging = debugSession && debugSession.type === 'prolog';

      const statusData = {
        installation: {
          isInstalled: installationStatus.isInstalled,
          version: installationStatus.version || 'Unknown',
          path: installationStatus.path || 'Not found',
          issues: installationStatus.issues || [],
        },
        queries: recentQueries.map((q: any) => ({
          query: q.query,
          success: q.success,
          timestamp: q.timestamp,
          result: q.result,
        })),
        files: prologFiles.map(file => ({
          name: path.basename(file.fsPath),
          path: file.fsPath,
          relativePath: vscode.workspace.asRelativePath(file),
        })),
        debugging: {
          isActive: isDebugging,
          sessionName: debugSession?.name || null,
        },
        workspace: {
          hasWorkspace: !!vscode.workspace.workspaceFolders,
          workspaceName: vscode.workspace.workspaceFolders?.[0]?.name || null,
        },
      };

      this._view.webview.postMessage({
        type: 'statusUpdate',
        data: statusData,
      });
    } catch (error) {
      console.error('Error refreshing dashboard status:', error);
      this._view.webview.postMessage({
        type: 'error',
        message: `Failed to refresh status: ${error}`,
      });
    }
  }

  private async _createNewFile(): Promise<void> {
    try {
      const fileName = await vscode.window.showInputBox({
        prompt: 'Enter the name for the new Prolog file',
        value: 'untitled.pl',
        validateInput: value => {
          if (!value) {
            return 'File name cannot be empty';
          }
          if (!value.match(/\.(pl|pro|prolog|plt|ecl)$/)) {
            return 'File must have a Prolog extension (.pl, .pro, .prolog, .plt, .ecl)';
          }
          return null;
        },
      });

      if (fileName) {
        const workspaceFolder = vscode.workspace.workspaceFolders?.[0];
        if (workspaceFolder) {
          const filePath = vscode.Uri.joinPath(workspaceFolder.uri, fileName);
          const edit = new vscode.WorkspaceEdit();
          edit.createFile(filePath, { ignoreIfExists: false });
          await Promise.resolve(vscode.workspace.applyEdit(edit));
          await vscode.window.showTextDocument(filePath);
        } else {
          // Create untitled document
          const doc = await vscode.workspace.openTextDocument({
            language: 'prolog',
            content: `% ${fileName}\n% New Prolog file\n\n`,
          });
          await vscode.window.showTextDocument(doc);
        }
      }
    } catch (error) {
      vscode.window.showErrorMessage(`Failed to create new file: ${error}`);
    }
  }

  private async _clearHistory(): Promise<void> {
    try {
      // fallback: mark all as deleted
      const history = await this.queryHistory.getHistory({});
      for (const q of history) {
        await this.queryHistory.updateQuery(q.id, { deleted: true });
      }
      vscode.window.showInformationMessage('Query history cleared');
      this._refreshStatus();
    } catch (error) {
      vscode.window.showErrorMessage(`Failed to clear history: ${error}`);
    }
  }

  private _getHtmlForWebview(webview: vscode.Webview): string {
    const scriptUri = webview.asWebviewUri(
      vscode.Uri.joinPath(this._extensionUri, 'out', 'pub', 'media', 'dashboard.js')
    );
    const styleUri = webview.asWebviewUri(
      vscode.Uri.joinPath(this._extensionUri, 'out', 'pub', 'media', 'dashboard.css')
    );
    return `<!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <link href="${styleUri}" rel="stylesheet">
      <title>Prolog Dashboard</title>
    </head>
    <body>
      <div class="dashboard-container">
        <div class="dashboard-header">
          <h1>🔧 Prolog Toolkit</h1>
          <button id="refreshBtn" class="action-btn" title="Refresh Status">🔄</button>
        </div>
        <!-- Installation Status -->
        <div class="dashboard-section">
          <h2>📦 Installation Status</h2>
          <div id="installationStatus" class="status-card">
            <div class="status-indicator loading">
              <span class="status-icon">🔄</span>
              <span class="status-text">Checking installation...</span>
            </div>
            <div class="status-details" id="installationDetails" style="display: none;">
              <div class="detail-item">
                <strong>Version:</strong> <span id="installationVersion">-</span>
              </div>
              <div class="detail-item">
                <strong>Path:</strong> <span id="installationPath">-</span>
              </div>
            </div>
            <div class="status-actions">
              <button class="action-btn" id="setupWizardBtn">🧙‍♂️ Setup Wizard</button>
              <button class="action-btn" id="openSettingsBtn">⚙️ Settings</button>
            </div>
          </div>
        </div>
        <!-- Quick Query -->
        <div class="dashboard-section">
          <h2>⚡ Quick Query</h2>
          <div class="query-card">
            <div class="query-input-container">
              <input type="text" id="queryInput" placeholder="Enter Prolog query (e.g., member(X, [1,2,3]))" />
              <button id="executeBtn" class="action-btn primary">▶️ Execute</button>
            </div>
            <div class="query-examples">
              <span class="example-label">Examples:</span>
              <button class="example-btn" data-query="member(X, [1,2,3])">member(X, [1,2,3])</button>
              <button class="example-btn" data-query="append([1,2], [3,4], X)">append([1,2], [3,4], X)</button>
              <button class="example-btn" data-query="length([a,b,c], X)">length([a,b,c], X)</button>
            </div>
          </div>
        </div>
        <!-- Recent Queries -->
        <div class="dashboard-section">
          <h2>📜 Recent Queries</h2>
          <div id="recentQueries" class="queries-card">
            <div class="loading-message">Loading recent queries...</div>
          </div>
          <div class="queries-actions">
            <button class="action-btn" id="clearHistoryBtn">🗑️ Clear History</button>
          </div>
        </div>
        <!-- Workspace Files -->
        <div class="dashboard-section">
          <h2>📁 Prolog Files</h2>
          <div id="workspaceFiles" class="files-card">
            <div class="loading-message">Loading workspace files...</div>
          </div>
          <div class="files-actions">
            <button class="action-btn" id="newFileBtn">➕ New File</button>
          </div>
        </div>
        <!-- Debug Status -->
        <div class="dashboard-section">
          <h2>🐛 Debug Status</h2>
          <div id="debugStatus" class="debug-card">
            <div class="status-indicator">
              <span class="status-icon">⏸️</span>
              <span class="status-text">No active debug session</span>
            </div>
          </div>
        </div>
        <!-- Quick Actions -->
        <div class="dashboard-section">
          <h2>🚀 Quick Actions</h2>
          <div class="actions-grid">
            <button class="quick-action-btn" onclick="vscode.postMessage({type: 'openSettings'})">
              <span class="action-icon">⚙️</span>
              <span class="action-label">Settings</span>
            </button>
            <button class="quick-action-btn" onclick="vscode.postMessage({type: 'setupWizard'})">
              <span class="action-icon">🧙‍♂️</span>
              <span class="action-label">Setup</span>
            </button>
            <button class="quick-action-btn" onclick="vscode.postMessage({type: 'newFile'})">
              <span class="action-icon">📄</span>
              <span class="action-label">New File</span>
            </button>
            <button class="quick-action-btn" onclick="window.open('https://github.com/mediaprophet/VSCode-Prolog-Toolkit')">
              <span class="action-icon">📚</span>
              <span class="action-label">Docs</span>
            </button>
          </div>
        </div>
      </div>
      <script src="${scriptUri}"></script>
    </body>
    </html>`;
  }
}
