'use strict';

import { ExtensionContext } from 'vscode';
import { ExtensionManager } from './modules/extensionManager';

// Global extension manager instance
let extensionManager: ExtensionManager | null = null;

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export async function activate(context: ExtensionContext) {
  try {
    // Get the singleton extension manager instance
    extensionManager = ExtensionManager.getInstance();
    // Activate the extension through the manager
    await extensionManager.activate(context);

    // Register Prolog terminal profile provider
    const vscode = await import('vscode');
    context.subscriptions.push(
      vscode.window.registerTerminalProfileProvider('prolog', {
        provideTerminalProfile(token) {
          const config = vscode.workspace.getConfiguration('prolog');
          const exe = config.get('executablePath', 'swipl');
          const args = config.get('terminal.runtimeArgs', []);
          return new vscode.TerminalProfile({
            name: 'Prolog REPL',
            shellPath: exe,
            shellArgs: args,
          });
        },
      })
    );
  } catch (error) {
    console.error('[Extension] Failed to activate extension:', error);
    throw error;
  }
}

// This method is called when your extension is deactivated
export async function deactivate() {
  if (extensionManager) {
    try {
      await extensionManager.deactivate();
    } catch (error) {
      console.error('[Extension] Error during deactivation:', error);
    } finally {
      extensionManager = null;
    }
  }
}