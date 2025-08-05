import * as vscode from 'vscode';
import * as path from 'path';
import { InstallationChecker } from './installationChecker';
import { QueryHistoryManager } from './queryHistoryManager';

export interface PrologTreeItem {
    id: string;
    label: string;
    description?: string;
    tooltip?: string;
    iconPath?: vscode.ThemeIcon | { light: string; dark: string };
    contextValue?: string;
    command?: vscode.Command;
    children?: PrologTreeItem[];
}

export class PrologActivityProvider implements vscode.TreeDataProvider<PrologTreeItem> {
    private _onDidChangeTreeData: vscode.EventEmitter<PrologTreeItem | undefined | null | void> = new vscode.EventEmitter<PrologTreeItem | undefined | null | void>();
    readonly onDidChangeTreeData: vscode.Event<PrologTreeItem | undefined | null | void> = this._onDidChangeTreeData.event;

    private installationChecker: InstallationChecker;
    private queryHistory: QueryHistoryManager;
    private isInstalled: boolean = false;
    private currentVersion: string = 'Unknown';

    constructor(private context: vscode.ExtensionContext) {
        this.installationChecker = InstallationChecker.getInstance();
        this.queryHistory = QueryHistoryManager.getInstance();
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
        const treeItem = new vscode.TreeItem(element.label, element.children ? vscode.TreeItemCollapsibleState.Expanded : vscode.TreeItemCollapsibleState.None);
        
        treeItem.id = element.id;
        treeItem.description = element.description;
        treeItem.tooltip = element.tooltip;
        treeItem.iconPath = element.iconPath;
        treeItem.contextValue = element.contextValue;
        treeItem.command = element.command;

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
                tooltip: this.isInstalled ? `SWI-Prolog ${this.currentVersion} is installed` : 'SWI-Prolog not found',
                iconPath: new vscode.ThemeIcon(this.isInstalled ? 'check' : 'error'),
                contextValue: 'installation',
                children: []
            },
            {
                id: 'queries',
                label: 'Query History',
                description: 'Recent queries',
                tooltip: 'View and manage recent Prolog queries',
                iconPath: new vscode.ThemeIcon('history'),
                contextValue: 'queries',
                children: []
            },
            {
                id: 'files',
                label: 'Prolog Files',
                description: 'Workspace files',
                tooltip: 'Prolog files in current workspace',
                iconPath: new vscode.ThemeIcon('files'),
                contextValue: 'files',
                children: []
            },
            {
                id: 'debugging',
                label: 'Debug Sessions',
                description: 'Active sessions',
                tooltip: 'Manage debug sessions',
                iconPath: new vscode.ThemeIcon('debug'),
                contextValue: 'debugging',
                children: []
            },
            {
                id: 'settings',
                label: 'Settings',
                description: 'Configuration',
                tooltip: 'Extension settings and configuration',
                iconPath: new vscode.ThemeIcon('settings-gear'),
                contextValue: 'settings',
                children: []
            }
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
                contextValue: 'installation-status'
            });

            items.push({
                id: 'test-installation',
                label: 'Test Installation',
                tooltip: 'Test SWI-Prolog installation',
                iconPath: new vscode.ThemeIcon('beaker'),
                contextValue: 'test-installation',
                command: {
                    command: 'prolog.testInstallation',
                    title: 'Test Installation'
                }
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
                    title: 'Setup Wizard'
                }
            });

            items.push({
                id: 'auto-detect',
                label: 'Auto-Detect Path',
                tooltip: 'Automatically detect SWI-Prolog installation',
                iconPath: new vscode.ThemeIcon('search'),
                contextValue: 'auto-detect',
                command: {
                    command: 'prolog.autoDetectPath',
                    title: 'Auto-Detect Path'
                }
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
                title: 'Refresh Installation'
            }
        });

        return items;
    }

    private async getQueryItems(): Promise<PrologTreeItem[]> {
        const items: PrologTreeItem[] = [];

        try {
            const history = await this.queryHistory.getRecentQueries(10);
            
            if (history.length === 0) {
                items.push({
                    id: 'no-queries',
                    label: 'No recent queries',
                    description: 'Execute a query to see history',
                    tooltip: 'Query history will appear here',
                    iconPath: new vscode.ThemeIcon('info'),
                    contextValue: 'no-queries'
                });
            } else {
                history.forEach((query, index) => {
                    items.push({
                        id: `query-${index}`,
                        label: query.query.length > 30 ? `${query.query.substring(0, 30)}...` : query.query,
                        description: query.success ? '✓' : '✗',
                        tooltip: `Query: ${query.query}\nResult: ${query.success ? 'Success' : 'Failed'}\nTime: ${new Date(query.timestamp).toLocaleString()}`,
                        iconPath: new vscode.ThemeIcon(query.success ? 'check' : 'error'),
                        contextValue: 'query-item',
                        command: {
                            command: 'prolog.rerunQuery',
                            title: 'Rerun Query',
                            arguments: [query.query]
                        }
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
                    title: 'Clear History'
                }
            });

        } catch (error) {
            items.push({
                id: 'query-error',
                label: 'Error loading history',
                description: 'Click to retry',
                tooltip: 'Failed to load query history',
                iconPath: new vscode.ThemeIcon('error'),
                contextValue: 'query-error'
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
                contextValue: 'no-workspace'
            });
            return items;
        }

        try {
            const prologFiles = await vscode.workspace.findFiles('**/*.{pl,pro,prolog,plt,ecl}', '**/node_modules/**', 50);
            
            if (prologFiles.length === 0) {
                items.push({
                    id: 'no-files',
                    label: 'No Prolog files found',
                    description: 'Create a .pl file to get started',
                    tooltip: 'No Prolog files found in workspace',
                    iconPath: new vscode.ThemeIcon('file'),
                    contextValue: 'no-files'
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
                            arguments: [file]
                        }
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
                    title: 'New Prolog File'
                }
            });

        } catch (error) {
            items.push({
                id: 'file-error',
                label: 'Error loading files',
                description: 'Click to retry',
                tooltip: 'Failed to load Prolog files',
                iconPath: new vscode.ThemeIcon('error'),
                contextValue: 'file-error'
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
                contextValue: 'active-session'
            });

            items.push({
                id: 'stop-debugging',
                label: 'Stop Debugging',
                tooltip: 'Stop current debug session',
                iconPath: new vscode.ThemeIcon('debug-stop'),
                contextValue: 'stop-debugging',
                command: {
                    command: 'workbench.action.debug.stop',
                    title: 'Stop Debugging'
                }
            });
        } else {
            items.push({
                id: 'no-sessions',
                label: 'No active sessions',
                description: 'Start debugging to see sessions',
                tooltip: 'No active debug sessions',
                iconPath: new vscode.ThemeIcon('debug'),
                contextValue: 'no-sessions'
            });

            items.push({
                id: 'start-debugging',
                label: 'Start Debugging',
                tooltip: 'Start a new debug session',
                iconPath: new vscode.ThemeIcon('debug-start'),
                contextValue: 'start-debugging',
                command: {
                    command: 'workbench.action.debug.start',
                    title: 'Start Debugging'
                }
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
                title: 'Configure Debug'
            }
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
                    title: 'Open Settings'
                }
            },
            {
                id: 'setup-wizard',
                label: 'Setup Wizard',
                tooltip: 'Run the setup wizard',
                iconPath: new vscode.ThemeIcon('wand'),
                contextValue: 'setup-wizard',
                command: {
                    command: 'prolog.setupWizard',
                    title: 'Setup Wizard'
                }
            },
            {
                id: 'view-logs',
                label: 'View Logs',
                tooltip: 'View extension logs',
                iconPath: new vscode.ThemeIcon('output'),
                contextValue: 'view-logs',
                command: {
                    command: 'prolog.viewLogs',
                    title: 'View Logs'
                }
            },
            {
                id: 'report-issue',
                label: 'Report Issue',
                tooltip: 'Report an issue on GitHub',
                iconPath: new vscode.ThemeIcon('bug'),
                contextValue: 'report-issue',
                command: {
                    command: 'prolog.reportIssue',
                    title: 'Report Issue'
                }
            }
        ];
    }
}