import * as vscode from 'vscode';
import { QueryHistoryWebviewProvider } from '../../webview-ui/QueryHistoryWebviewProvider';
import { QueryHistoryOrchestrator } from './queryHistoryManager/QueryHistoryOrchestrator';

export function registerQueryHistoryWebview(context: vscode.ExtensionContext, orchestrator: QueryHistoryOrchestrator) {
  const provider = new QueryHistoryWebviewProvider(context.extensionUri, orchestrator);
  context.subscriptions.push(
    vscode.window.registerWebviewViewProvider(QueryHistoryWebviewProvider.viewType, provider)
  );
}
