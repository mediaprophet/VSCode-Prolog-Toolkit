import * as vscode from 'vscode';
import { QueryHistoryOrchestrator } from '../../features/queryHistoryManager/QueryHistoryOrchestrator';
import { PrologFilesProvider } from '../../features/tree/PrologFilesProvider';
import { QueryHistoryProvider } from '../../features/tree/QueryHistoryProvider';

export class UIService {
  registerProvidersAndUI(context: vscode.ExtensionContext, orchestrator: QueryHistoryOrchestrator, prologBackend: any) {
    // Register Query History Webview
    const registerQueryHistoryWebview = require('../../features/registerQueryHistoryWebview').registerQueryHistoryWebview;
    registerQueryHistoryWebview(context, orchestrator);
    // Register 'prolog.rerunQuery' command (robust, only once)
    if (!vscode.commands.getCommands().then(cmds => cmds.includes('prolog.rerunQuery'))) {
      const rerunQueryDisposable = vscode.commands.registerCommand('prolog.rerunQuery', async (query: string) => {
        if (!query) {
          vscode.window.showErrorMessage('No query provided to rerun.');
          return;
        }
        // Use the backend to rerun the query
        if (prologBackend && typeof prologBackend.sendRequest === 'function') {
          try {
            const response = await prologBackend.sendRequest('query', { goal: query, timeoutMs: 10000 });
            if (response.status === 'ok') {
              vscode.window.showInformationMessage('Query rerun successfully.');
            } else {
              vscode.window.showErrorMessage(`Query failed: ${response.error}`);
            }
          } catch (err) {
            vscode.window.showErrorMessage(`Error rerunning query: ${err}`);
          }
        } else {
          vscode.window.showErrorMessage('Prolog backend is not available.');
        }
      });
      context.subscriptions.push(rerunQueryDisposable);
    }

    // Register new, specialized tree providers for each panel, respecting user customization
    const workspaceRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath || '';
    const { PrologExplorerProvider } = require('../../features/tree/PrologExplorerProvider');
    const { QueryHistoryProvider, QueryHistoryItem } = require('../../features/tree/QueryHistoryProvider');
    const { PrologFilesProvider } = require('../../features/tree/PrologFilesProvider');
    const { PrologDashboardProvider } = require('../../features/prologDashboardProvider');

    const getHistory = async () => {
      const history = await orchestrator.getHistory();
      return (history || []).map((entry: any) =>
        new QueryHistoryItem(
          entry.label || entry.query || 'Query',
          vscode.TreeItemCollapsibleState.None,
          entry.status === 'success' ? 'success' : 'error',
          entry.query || ''
        )
      );
    };
    const queryHistoryProvider = new QueryHistoryProvider(getHistory);
    const prologFilesProvider = new PrologFilesProvider(workspaceRoot);
    const prologExplorerProvider = new PrologExplorerProvider(workspaceRoot);
    const prologDashboardProvider = new PrologDashboardProvider(context.extensionUri, prologBackend);

    // Register tree data providers for built-in views
    context.subscriptions.push(
      vscode.window.registerTreeDataProvider('prologActivity', prologExplorerProvider),
      vscode.window.registerTreeDataProvider('prologQueries', queryHistoryProvider),
      vscode.window.registerTreeDataProvider('prologFiles', prologFilesProvider),
      vscode.window.registerWebviewViewProvider('prologDashboard', prologDashboardProvider)
    );

    // Register filter commands (should be implemented in this class or delegated)
    if (typeof this.registerFilterCommands === 'function') {
      this.registerFilterCommands(context, queryHistoryProvider, prologFilesProvider);
    }

    // Register context menu command handlers (should be implemented in this class or delegated)
    if (typeof this.registerTreeViewCommands === 'function') {
      this.registerTreeViewCommands(context);
    }

    // Listen for configuration changes to refresh all panels
    context.subscriptions.push(
      vscode.workspace.onDidChangeConfiguration(event => {
        if (event.affectsConfiguration('prolog')) {
          prologExplorerProvider.refresh();
          queryHistoryProvider.refresh();
          prologFilesProvider.refresh();
        }
      })
    );
  }

  registerFilterCommands(context: vscode.ExtensionContext, queryHistoryProvider: QueryHistoryProvider, prologFilesProvider: PrologFilesProvider) {
    // ...existing code for filter commands...
  }

  registerTreeViewCommands(context: vscode.ExtensionContext) {
    // ...existing code for tree view commands...
  }
}
