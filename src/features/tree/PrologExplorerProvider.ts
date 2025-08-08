
import * as fs from 'fs';
import * as path from 'path';
import * as vscode from 'vscode';

export class PrologExplorerProvider implements vscode.TreeDataProvider<vscode.TreeItem> {

  private _onDidChangeTreeData: vscode.EventEmitter<vscode.TreeItem | undefined | void> = new vscode.EventEmitter();
  readonly onDidChangeTreeData: vscode.Event<vscode.TreeItem | undefined | void> = this._onDidChangeTreeData.event;

  private watcher: vscode.FileSystemWatcher | undefined;
  private refreshTimeout: NodeJS.Timeout | undefined;

  constructor(private workspaceRoot: string) {
    if (workspaceRoot) {
      this.watcher = vscode.workspace.createFileSystemWatcher(new vscode.RelativePattern(workspaceRoot, '**/*.{pl,prolog,pro}'));
      this.watcher.onDidChange(() => this.debouncedRefresh());
      this.watcher.onDidCreate(() => this.debouncedRefresh());
      this.watcher.onDidDelete(() => this.debouncedRefresh());
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
    return element;
  }

  async getChildren(element?: vscode.TreeItem): Promise<vscode.TreeItem[]> {
    if (!this.workspaceRoot) {
      return [new vscode.TreeItem('No workspace open')];
    }
    const items: vscode.TreeItem[] = [];
    if (!element) {
      // Top-level: show folders and files in workspace root
      const dirents = fs.readdirSync(this.workspaceRoot, { withFileTypes: true });
      for (const dirent of dirents) {
        if (dirent.isDirectory()) {
          const folderPath = path.join(this.workspaceRoot, dirent.name);
          const folderItem = new vscode.TreeItem(dirent.name, vscode.TreeItemCollapsibleState.Collapsed);
          folderItem.iconPath = new vscode.ThemeIcon('folder');
          folderItem.contextValue = 'prologFolder';
          folderItem.resourceUri = vscode.Uri.file(folderPath);
          items.push(folderItem);
        } else if (dirent.isFile() && /\.(pl|prolog|pro)$/i.test(dirent.name)) {
          const filePath = path.join(this.workspaceRoot, dirent.name);
          const fileItem = new vscode.TreeItem(dirent.name, vscode.TreeItemCollapsibleState.None);
          fileItem.resourceUri = vscode.Uri.file(filePath);
          // Demo: randomly assign warning overlay for demo
          const hasWarning = Math.random() < 0.2;
          fileItem.iconPath = hasWarning
            ? new vscode.ThemeIcon('file-code', new vscode.ThemeColor('problemsWarningIcon.foreground'))
            : new vscode.ThemeIcon('file-code');
          fileItem.contextValue = 'prologFile';
          items.push(fileItem);
        }
      }
    } else if (element.resourceUri && fs.lstatSync(element.resourceUri.fsPath).isDirectory()) {
      // Show files in the selected folder
      const dirents = fs.readdirSync(element.resourceUri.fsPath, { withFileTypes: true });
      for (const dirent of dirents) {
        if (dirent.isFile() && /\.(pl|prolog|pro)$/i.test(dirent.name)) {
          const filePath = path.join(element.resourceUri.fsPath, dirent.name);
          const fileItem = new vscode.TreeItem(dirent.name, vscode.TreeItemCollapsibleState.None);
          fileItem.resourceUri = vscode.Uri.file(filePath);
          // Demo: randomly assign error overlay for demo
          const hasError = Math.random() < 0.1;
          fileItem.iconPath = hasError
            ? new vscode.ThemeIcon('file-code', new vscode.ThemeColor('problemsErrorIcon.foreground'))
            : new vscode.ThemeIcon('file-code');
          fileItem.contextValue = 'prologFile';
          items.push(fileItem);
        }
      }
    }
    return items;
  }
}
