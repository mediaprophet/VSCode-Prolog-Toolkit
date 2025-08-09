import * as vscode from 'vscode';
import { SettingsWebviewProvider } from './settingsWebviewProvider';

export function registerSettingsWebview(context: vscode.ExtensionContext) {
  const provider = new SettingsWebviewProvider(context.extensionUri);
  context.subscriptions.push(
    vscode.window.registerWebviewViewProvider(SettingsWebviewProvider.viewType, provider)
  );
}
