import * as path from 'path';
import * as vscode from 'vscode';

export class PrologFilesProvider implements vscode.TreeDataProvider<vscode.TreeItem> {
  private filterText = '';
  private _onDidChangeTreeData: vscode.EventEmitter<vscode.TreeItem | undefined | void> = new vscode.EventEmitter();
  readonly onDidChangeTreeData: vscode.Event<vscode.TreeItem | undefined | void> = this._onDidChangeTreeData.event;
  private watcher: vscode.FileSystemWatcher | undefined;
  private refreshTimeout: NodeJS.Timeout | undefined;
  private filePatterns: string[];

  constructor(private workspaceRoot: string) {
    // Read user setting for file patterns, fallback to default
    this.filePatterns = vscode.workspace.getConfiguration('prolog').get<string[]>('files.customPatterns') || ['**/*.pl', '**/*.pro', '**/*.prolog', '**/*.plt', '**/*.ecl'];
    if (workspaceRoot) {
      // Watch all patterns
      for (const pattern of this.filePatterns) {
        const relPattern = new vscode.RelativePattern(workspaceRoot, pattern.replace(/^\*\*\//, ''));
        const watcher = vscode.workspace.createFileSystemWatcher(relPattern);
        watcher.onDidChange(() => this.debouncedRefresh());
        watcher.onDidCreate(() => this.debouncedRefresh());
        watcher.onDidDelete(() => this.debouncedRefresh());
      }
    }
  }

  private debouncedRefresh(delay = 200) {
    if (this.refreshTimeout) clearTimeout(this.refreshTimeout);
    this.refreshTimeout = setTimeout(() => this.refresh(), delay);
  }

  // For badge support
  public get onDidChangeTreeDataEvent() {
    return this._onDidChangeTreeData.event;
  }

  refresh(): void {
    this._onDidChangeTreeData.fire();
  }

  getTreeItem(element: vscode.TreeItem): vscode.TreeItem {
    return element || new vscode.TreeItem('');
  }

  async getChildren(element?: vscode.TreeItem): Promise<vscode.TreeItem[]> {
    if (!this.workspaceRoot) {
      return [new vscode.TreeItem('No workspace open')];
    }
    const items: vscode.TreeItem[] = [];
    const fg = require('fast-glob');
    // Find all files matching any pattern
    const files = fg.sync(this.filePatterns, { cwd: this.workspaceRoot, onlyFiles: true });
    for (const file of files) {
      const filePath = path.join(this.workspaceRoot, file);
      const fileItem = new vscode.TreeItem(file, vscode.TreeItemCollapsibleState.None);
      fileItem.resourceUri = vscode.Uri.file(filePath);
      // Demo: randomly assign warning overlay for demo
      const hasWarning = Math.random() < 0.2;
      fileItem.iconPath = hasWarning
        ? new vscode.ThemeIcon('file-code', new vscode.ThemeColor('problemsWarningIcon.foreground'))
        : new vscode.ThemeIcon('file-code');
      fileItem.contextValue = 'prologFile';
      items.push(fileItem);
    }
    if (!this.filterText) return items;
    const filter = this.filterText.toLowerCase();
    return items.filter(item => {
      if (!item.label) return false;
      if (typeof item.label === 'string') {
        return item.label.toLowerCase().includes(filter);
      } else if (typeof item.label === 'object' && 'label' in item.label && typeof item.label.label === 'string') {
        return item.label.label.toLowerCase().includes(filter);
      }
      return false;
    });
  }

  public setFilterText(text: string) {
    this.filterText = text;
    this.refresh();
  }
}
