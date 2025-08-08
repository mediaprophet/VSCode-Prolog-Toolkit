import * as path from 'path';
import * as vscode from 'vscode';
import { ErrorHandler, PrologError } from './errorHandler';
import { InstallationChecker } from './installationChecker';
import { QueryHistoryOrchestrator } from './queryHistoryManager/QueryHistoryOrchestrator';
import { QueryNotificationManager } from './queryNotificationManager';

export interface PrologTreeItem {
  id: string;
  label: string;
  description?: string;
  tooltip?: string;
  iconPath?: vscode.ThemeIcon | { light: vscode.Uri; dark: vscode.Uri };
  contextValue?: string;
  command?: vscode.Command;
  children?: PrologTreeItem[];
}

export class PrologActivityProvider implements vscode.TreeDataProvider<PrologTreeItem> {
  private _onDidChangeTreeData: vscode.EventEmitter<PrologTreeItem | undefined | null | void> =
    new vscode.EventEmitter<PrologTreeItem | undefined | null | void>();
  readonly onDidChangeTreeData: vscode.Event<PrologTreeItem | undefined | null | void> =
    this._onDidChangeTreeData.event;

  private installationChecker: InstallationChecker;
  private queryHistory: QueryHistoryOrchestrator;
  private queryNotificationManager: QueryNotificationManager;
  private errorHandler: ErrorHandler;
  private recentErrors: PrologError[] = [];
  private isInstalled: boolean = false;
  private currentVersion: string = 'Unknown';

  /**
   * Add an error to the recent errors list
   */
  addError(error: PrologError): void {
    // Add to the beginning of the array
    this.recentErrors.unshift(error);

    // Keep only the last 10 errors
    if (this.recentErrors.length > 10) {
      this.recentErrors = this.recentErrors.slice(0, 10);
    }

    // Refresh the tree view
    this.refresh();
  }

  constructor(private context: vscode.ExtensionContext) {
    this.installationChecker = InstallationChecker.getInstance();
    this.queryHistory = new QueryHistoryOrchestrator();
    this.queryNotificationManager = new QueryNotificationManager();
    this.errorHandler = new ErrorHandler();
    this.checkInstallation();
  }

  private async checkInstallation(): Promise<void> {
    try {
      const status = await this.installationChecker.checkSwiplInstallation();
      this.isInstalled = status.isInstalled;
      this.currentVersion = status.version || 'Unknown';
      this.refresh();
    } catch (error) {
      console.error('Error checking Prolog installation:', error);
    }
  }

  refresh(): void {
    this._onDidChangeTreeData.fire();
  }

  getTreeItem(element: PrologTreeItem): vscode.TreeItem {
    const treeItem = new vscode.TreeItem(
      element.label,
      element.children
        ? vscode.TreeItemCollapsibleState.Expanded
        : vscode.TreeItemCollapsibleState.None
    );

    treeItem.id = element.id;
    if (typeof element.description !== 'undefined') {
      treeItem.description = element.description;
    }
    treeItem.tooltip = element.tooltip;
    if (typeof element.iconPath !== 'undefined') {
      treeItem.iconPath = element.iconPath;
    }
    if (typeof element.contextValue !== 'undefined') {
      treeItem.contextValue = element.contextValue;
    }
    if (typeof element.command !== 'undefined') {
      treeItem.command = element.command;
    }

    return treeItem;
  }

  async getChildren(element?: PrologTreeItem): Promise<PrologTreeItem[]> {
    if (!element) {
      return this.getRootItems();
    }

    switch (element.id) {
      case 'installation':
        return this.getInstallationItems();
      case 'queries':
        return this.getQueryItems();
      case 'files':
        return this.getFileItems();
      case 'debugging':
        return this.getDebuggingItems();
      case 'settings':
        return this.getSettingsItems();
      case 'notifications-errors':
        return this.getNotificationsAndErrorsItems();
      default:
        return element.children || [];
    }
  }

  private getRootItems(): PrologTreeItem[] {
    return [
      {
        id: 'installation',
        label: 'Installation',
        description: this.isInstalled ? `✓ ${this.currentVersion}` : '✗ Not Found',
        tooltip: this.isInstalled
          ? `SWI-Prolog ${this.currentVersion} is installed`
          : 'SWI-Prolog not found',
        iconPath: new vscode.ThemeIcon(this.isInstalled ? 'check' : 'error'),
        contextValue: 'installation',
        children: [],
      },
      {
        id: 'queries',
        label: 'Query History',
        description: 'Recent queries',
        tooltip: 'View and manage recent Prolog queries',
        iconPath: new vscode.ThemeIcon('history'),
        contextValue: 'queries',
        children: [],
      },
      {
        id: 'files',
        label: 'Prolog Files',
        description: 'Workspace files',
        tooltip: 'Prolog files in current workspace',
        iconPath: new vscode.ThemeIcon('files'),
        contextValue: 'files',
        children: [],
      },
      {
        id: 'debugging',
        label: 'Debug Sessions',
        description: 'Active sessions',
        tooltip: 'Manage debug sessions',
        iconPath: new vscode.ThemeIcon('debug'),
        contextValue: 'debugging',
        children: [],
      },
      {
        id: 'settings',
        label: 'Settings',
        description: 'Configuration',
        tooltip: 'Extension settings and configuration',
        iconPath: new vscode.ThemeIcon('settings-gear'),
        contextValue: 'settings',
        children: [],
      },
      {
        id: 'notifications-errors',
        label: 'Notifications & Errors',
        description: 'System status',
        tooltip: 'View notifications and errors',
        iconPath: new vscode.ThemeIcon('bell'),
        contextValue: 'notifications-errors',
        children: [],
      },
    ];
  }

  private getInstallationItems(): PrologTreeItem[] {
    const items: PrologTreeItem[] = [];

    if (this.isInstalled) {
      items.push({
        id: 'installation-status',
        label: `SWI-Prolog ${this.currentVersion}`,
        description: 'Installed',
        tooltip: 'SWI-Prolog is properly installed',
        iconPath: new vscode.ThemeIcon('check'),
        contextValue: 'installation-status',
      });

      items.push({
        id: 'test-installation',
        label: 'Test Installation',
        tooltip: 'Test SWI-Prolog installation',
        iconPath: new vscode.ThemeIcon('beaker'),
        contextValue: 'test-installation',
        command: {
          command: 'prolog.testInstallation',
          title: 'Test Installation',
        },
      });
    } else {
      items.push({
        id: 'installation-missing',
        label: 'SWI-Prolog Not Found',
        description: 'Click to install',
        tooltip: 'SWI-Prolog is not installed or not found in PATH',
        iconPath: new vscode.ThemeIcon('error'),
        contextValue: 'installation-missing',
        command: {
          command: 'prolog.setupWizard',
          title: 'Setup Wizard',
        },
      });

      items.push({
        id: 'auto-detect',
        label: 'Auto-Detect Path',
        tooltip: 'Automatically detect SWI-Prolog installation',
        iconPath: new vscode.ThemeIcon('search'),
        contextValue: 'auto-detect',
        command: {
          command: 'prolog.autoDetectPath',
          title: 'Auto-Detect Path',
        },
      });
    }

    items.push({
      id: 'refresh-installation',
      label: 'Refresh Status',
      tooltip: 'Refresh installation status',
      iconPath: new vscode.ThemeIcon('refresh'),
      contextValue: 'refresh-installation',
      command: {
        command: 'prolog.refreshInstallation',
        title: 'Refresh Installation',
      },
    });

    return items;
  }

  private async getQueryItems(): Promise<PrologTreeItem[]> {
    const items: PrologTreeItem[] = [];

    try {
      const history = await this.queryHistory.getHistory({ limit: 10 });

      if (!Array.isArray(history) || history.length === 0) {
        items.push({
          id: 'no-queries',
          label: 'No recent queries',
          description: 'Execute a query to see history',
          tooltip: 'Query history will appear here',
          iconPath: new vscode.ThemeIcon('info'),
          contextValue: 'no-queries',
        });
      } else {
        history.forEach((query: any, index: number) => {
          items.push({
            id: `query-${index}`,
            label: typeof query.cmd === 'string' && query.cmd.length > 30 ? `${query.cmd.substring(0, 30)}...` : query.cmd,
            description: query.status === 'completed' ? '✓' : '✗',
            tooltip: `Query: ${query.cmd}\nResult: ${query.status === 'completed' ? 'Success' : 'Failed'}\nTime: ${query.startTime ? new Date(query.startTime).toLocaleString() : ''}`,
            iconPath: new vscode.ThemeIcon(query.status === 'completed' ? 'check' : 'error'),
            contextValue: 'query-item',
            command: {
              command: 'prolog.rerunQuery',
              title: 'Rerun Query',
              arguments: [query.cmd],
            },
          });
        });
      }

      items.push({
        id: 'clear-history',
        label: 'Clear History',
        tooltip: 'Clear query history',
        iconPath: new vscode.ThemeIcon('trash'),
        contextValue: 'clear-history',
        command: {
          command: 'prolog.clearQueryHistory',
          title: 'Clear History',
        },
      });
    } catch (_error) {
      items.push({
        id: 'query-error',
        label: 'Error loading history',
        description: 'Click to retry',
        tooltip: 'Failed to load query history',
        iconPath: new vscode.ThemeIcon('error'),
        contextValue: 'query-error',
      });
    }

    return items;
  }

  private async getFileItems(): Promise<PrologTreeItem[]> {
    const items: PrologTreeItem[] = [];

    if (!vscode.workspace.workspaceFolders) {
      items.push({
        id: 'no-workspace',
        label: 'No workspace open',
        description: 'Open a folder to see Prolog files',
        tooltip: 'Open a workspace to see Prolog files',
        iconPath: new vscode.ThemeIcon('folder'),
        contextValue: 'no-workspace',
      });
      return items;
    }

    try {
      const prologFiles = await vscode.workspace.findFiles(
        '**/*.{pl,pro,prolog,plt,ecl}',
        '**/node_modules/**',
        50
      );

      if (prologFiles.length === 0) {
        items.push({
          id: 'no-files',
          label: 'No Prolog files found',
          description: 'Create a .pl file to get started',
          tooltip: 'No Prolog files found in workspace',
          iconPath: new vscode.ThemeIcon('file'),
          contextValue: 'no-files',
        });
      } else {
        prologFiles.forEach((file, index) => {
          const fileName = path.basename(file.fsPath);
          const relativePath = vscode.workspace.asRelativePath(file);

          items.push({
            id: `file-${index}`,
            label: fileName,
            description: path.dirname(relativePath),
            tooltip: `Open ${relativePath}`,
            iconPath: new vscode.ThemeIcon('file-code'),
            contextValue: 'prolog-file',
            command: {
              command: 'vscode.open',
              title: 'Open File',
              arguments: [file],
            },
          });
        });
      }

      items.push({
        id: 'new-file',
        label: 'New Prolog File',
        tooltip: 'Create a new Prolog file',
        iconPath: new vscode.ThemeIcon('file-add'),
        contextValue: 'new-file',
        command: {
          command: 'prolog.newFile',
          title: 'New Prolog File',
        },
      });
    } catch (_error) {
      items.push({
        id: 'file-error',
        label: 'Error loading files',
        description: 'Click to retry',
        tooltip: 'Failed to load Prolog files',
        iconPath: new vscode.ThemeIcon('error'),
        contextValue: 'file-error',
      });
    }

    return items;
  }

  private getDebuggingItems(): PrologTreeItem[] {
    const items: PrologTreeItem[] = [];

    // Check if there are active debug sessions
    const activeSessions = vscode.debug.activeDebugSession;

    if (activeSessions && activeSessions.type === 'prolog') {
      items.push({
        id: 'active-session',
        label: `Active: ${activeSessions.name}`,
        description: 'Running',
        tooltip: 'Active Prolog debug session',
        iconPath: new vscode.ThemeIcon('debug-alt'),
        contextValue: 'active-session',
      });

      items.push({
        id: 'stop-debugging',
        label: 'Stop Debugging',
        tooltip: 'Stop current debug session',
        iconPath: new vscode.ThemeIcon('debug-stop'),
        contextValue: 'stop-debugging',
        command: {
          command: 'workbench.action.debug.stop',
          title: 'Stop Debugging',
        },
      });
    } else {
      items.push({
        id: 'no-sessions',
        label: 'No active sessions',
        description: 'Start debugging to see sessions',
        tooltip: 'No active debug sessions',
        iconPath: new vscode.ThemeIcon('debug'),
        contextValue: 'no-sessions',
      });

      items.push({
        id: 'start-debugging',
        label: 'Start Debugging',
        tooltip: 'Start a new debug session',
        iconPath: new vscode.ThemeIcon('debug-start'),
        contextValue: 'start-debugging',
        command: {
          command: 'workbench.action.debug.start',
          title: 'Start Debugging',
        },
      });
    }

    items.push({
      id: 'debug-config',
      label: 'Debug Configuration',
      tooltip: 'Configure debug settings',
      iconPath: new vscode.ThemeIcon('gear'),
      contextValue: 'debug-config',
      command: {
        command: 'workbench.action.debug.configure',
        title: 'Configure Debug',
      },
    });

    return items;
  }

  private getSettingsItems(): PrologTreeItem[] {
    return [
      {
        id: 'open-settings',
        label: 'Open Settings',
        tooltip: 'Open Prolog extension settings',
        iconPath: new vscode.ThemeIcon('settings'),
        contextValue: 'open-settings',
        command: {
          command: 'prolog.openSettings',
          title: 'Open Settings',
        },
      },
      {
        id: 'setup-wizard',
        label: 'Setup Wizard',
        tooltip: 'Run the setup wizard',
        iconPath: new vscode.ThemeIcon('wand'),
        contextValue: 'setup-wizard',
        command: {
          command: 'prolog.setupWizard',
          title: 'Setup Wizard',
        },
      },
      {
        id: 'view-logs',
        label: 'View Logs',
        tooltip: 'View extension logs',
        iconPath: new vscode.ThemeIcon('output'),
        contextValue: 'view-logs',
        command: {
          command: 'prolog.viewLogs',
          title: 'View Logs',
        },
      },
      {
        id: 'report-issue',
        label: 'Report Issue',
        tooltip: 'Report an issue on GitHub',
        iconPath: new vscode.ThemeIcon('bug'),
        contextValue: 'report-issue',
        command: {
          command: 'prolog.reportIssue',
          title: 'Report Issue',
        },
      },
    ];
  }

  private getNotificationsAndErrorsItems(): PrologTreeItem[] {
    const items: PrologTreeItem[] = [];

    // Get active queries from QueryNotificationManager
    try {
      const activeQueries = this.queryNotificationManager.getActiveQueries();
      const allQueries = this.queryNotificationManager.getAllQueries();

      // Add active queries section
      if (activeQueries.length > 0) {
        items.push({
          id: 'active-queries-header',
          label: 'Active Queries',
          description: `${activeQueries.length} running`,
          tooltip: 'Currently running queries',
          iconPath: new vscode.ThemeIcon('sync'),
          contextValue: 'active-queries-header',
          children: [],
        });

        activeQueries.forEach((query, index) => {
          const statusIcon =
            query.status === 'running' ? 'sync' : query.status === 'pending' ? 'clock' : 'question';
          const statusText = query.status.charAt(0).toUpperCase() + query.status.slice(1);

          items.push({
            id: `active-query-${query.id}`,
            label: query.message || `Query ${index + 1}`,
            description: statusText,
            tooltip: `Status: ${statusText}\nStart time: ${new Date(query.startTime).toLocaleString()}`,
            iconPath: new vscode.ThemeIcon(statusIcon),
            contextValue: 'active-query',
          });
        });
      } else {
        items.push({
          id: 'no-active-queries',
          label: 'No Active Queries',
          description: 'All queries completed',
          tooltip: 'No currently running queries',
          iconPath: new vscode.ThemeIcon('check'),
          contextValue: 'no-active-queries',
        });
      }

      // Add recent errors section
      items.push({
        id: 'recent-errors-header',
        label: 'Recent Errors',
        description: `${this.recentErrors.length} errors`,
        tooltip: 'Recently encountered errors',
        iconPath: new vscode.ThemeIcon('error'),
        contextValue: 'recent-errors-header',
        children: [],
      });

      if (this.recentErrors.length > 0) {
        // Show only the most recent 5 errors
        const errorsToShow = this.recentErrors.slice(0, 5);
        errorsToShow.forEach((error, index) => {
          const severityIcon =
            error.severity === 'error'
              ? 'error'
              : error.severity === 'warning'
                ? 'warning'
                : 'info';
          const severityText = error.severity.charAt(0).toUpperCase() + error.severity.slice(1);

          items.push({
            id: `recent-error-${index}`,
            label:
              error.message.length > 50 ? `${error.message.substring(0, 50)}...` : error.message,
            description: severityText,
            tooltip: `Severity: ${severityText}\nType: ${error.type}\nCode: ${error.code}\nMessage: ${error.message}`,
            iconPath: new vscode.ThemeIcon(severityIcon),
            contextValue: 'recent-error',
          });
        });

        if (this.recentErrors.length > 5) {
          items.push({
            id: 'more-errors',
            label: 'More errors...',
            description: `${this.recentErrors.length - 5} more`,
            tooltip: `View all ${this.recentErrors.length} errors`,
            iconPath: new vscode.ThemeIcon('more'),
            contextValue: 'more-errors',
          });
        }
      } else {
        items.push({
          id: 'no-recent-errors',
          label: 'No Recent Errors',
          description: 'All clear',
          tooltip: 'No recent errors to display',
          iconPath: new vscode.ThemeIcon('check'),
          contextValue: 'no-recent-errors',
        });
      }

      // Add statistics
      const stats = this.queryNotificationManager.getStatistics();
      items.push({
        id: 'notification-stats',
        label: 'Statistics',
        description: `${stats.total} total`,
        tooltip: `Total: ${stats.total}\nPending: ${stats.pending}\nRunning: ${stats.running}\nCompleted: ${stats.completed}\nErrors: ${stats.error}\nCancelled: ${stats.cancelled}\nTimeouts: ${stats.timeout}`,
        iconPath: new vscode.ThemeIcon('graph'),
        contextValue: 'notification-stats',
      });
    } catch (_error) {
      items.push({
        id: 'notifications-error',
        label: 'Error loading notifications',
        description: 'Click to retry',
        tooltip: 'Failed to load notifications and errors',
        iconPath: new vscode.ThemeIcon('error'),
        contextValue: 'notifications-error',
      });
    }

    return items;
  }
}
