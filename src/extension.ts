'use strict';

import { ExtensionContext } from 'vscode';
import { ExtensionManager } from './modules/extensionManager';

// Global extension manager instance
let extensionManager: ExtensionManager | null = null;

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export async function activate(context: ExtensionContext) {
  try {
    console.log('[Prolog Toolkit] Extension activation started.');
    // Get the singleton extension manager instance
    extensionManager = ExtensionManager.getInstance();
    console.log('[Prolog Toolkit] ExtensionManager instance obtained.');
    // Activate the extension through the manager
    await extensionManager.activate(context);
    console.log('[Prolog Toolkit] ExtensionManager activated.');

    // Register Prolog terminal profile provider
    const vscode = await import('vscode');
    context.subscriptions.push(
      vscode.window.registerTerminalProfileProvider('prolog', {
        provideTerminalProfile(token) {
          const config = vscode.workspace.getConfiguration('prolog');
          const exe = config.get('executablePath', 'swipl');
          const args = config.get('terminal.runtimeArgs', []);
          console.log('[Prolog Toolkit] Providing Prolog terminal profile. exe:', exe, 'args:', args);
          return new vscode.TerminalProfile({
            name: 'Prolog REPL',
            shellPath: exe,
            shellArgs: args,
          });
        },
      })
    );
    console.log('[Prolog Toolkit] Terminal profile provider registered.');

    // Log UI initialization
    console.log('[Prolog Toolkit] Initiating UI registration and providers.');
    // If UIService or similar is used, add logs there as well (see uiService.ts)
  } catch (error) {
    console.error('[Extension] Failed to activate extension:', error);
    throw error;
  }
}

// This method is called when your extension is deactivated
export async function deactivate() {
  if (extensionManager) {
    try {
      console.log('[Prolog Toolkit] Deactivating extension...');
      // No deactivate method on ExtensionManager; just cleanup reference
      extensionManager = null;
      console.log('[Prolog Toolkit] Extension deactivated (reference cleared).');
    } catch (error) {
      console.error('[Extension] Error during deactivation:', error);
    }
  }
}