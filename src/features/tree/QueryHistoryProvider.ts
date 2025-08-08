import * as vscode from 'vscode';

export class QueryHistoryProvider implements vscode.TreeDataProvider<QueryHistoryItem> {

  private filterText: string = '';

  private _onDidChangeTreeData: vscode.EventEmitter<QueryHistoryItem | undefined | void> = new vscode.EventEmitter();
  readonly onDidChangeTreeData: vscode.Event<QueryHistoryItem | undefined | void> = this._onDidChangeTreeData.event;

  constructor(private getHistory: () => Promise<QueryHistoryItem[]>) { }

  // For badge support
  public get onDidChangeTreeDataEvent() {
    return this._onDidChangeTreeData.event;
  }

  refresh(): void {
    this._onDidChangeTreeData.fire();
  }

  // Allow external trigger for live updates
  public triggerRefresh(): void {
    this.refresh();
  }

  getTreeItem(element: QueryHistoryItem): vscode.TreeItem {
    return element;
  }

  async getChildren(element?: QueryHistoryItem): Promise<QueryHistoryItem[]> {
    if (element) return [];
    const all = await this.getHistory();
    if (!this.filterText) return all;
    const filter = this.filterText.toLowerCase();
    return all.filter(item =>
      item.label.toLowerCase().includes(filter) ||
      (item.query && item.query.toLowerCase().includes(filter))
    );
  }

  public setFilterText(text: string) {
    this.filterText = text;
    this.refresh();
  }

  // Clear the query history (UI only; actual clearing logic can be implemented as needed)
  public async clearHistory() {
    // TODO: implement actual clearing if persistent storage is used
    this.setFilterText('');
    this.refresh();
  }
}

export class QueryHistoryItem extends vscode.TreeItem {
  constructor(
    override readonly label: string,
    override readonly collapsibleState: vscode.TreeItemCollapsibleState,
    public readonly status: 'success' | 'error' | 'running',
    public readonly query: string
  ) {
    super(label, collapsibleState);
    let iconId = 'question';
    if (status === 'success') iconId = 'check';
    else if (status === 'error') iconId = 'error';
    else if (status === 'running') iconId = 'sync';
    this.iconPath = new vscode.ThemeIcon(iconId);
    this.contextValue = 'queryHistoryItem';
    this.tooltip = query;
  }
}
