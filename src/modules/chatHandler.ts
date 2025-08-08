'use strict';

import { CancellationToken, ChatRequest, ChatResponseStream, ChatResult, window, workspace } from 'vscode';
import { PrologBackend } from '../prologBackend';
import { ChatCommandContext } from './chat-commands/types';
import { ChatCommandRegistry } from './chatCommandRegistry';
import { TelemetryCollector } from './telemetryCollector';

export class ChatHandler {
  private prologBackend: PrologBackend | null = null;
  private telemetry: TelemetryCollector;
  private registry: ChatCommandRegistry;
  private chatHistory: Array<{ prompt: string; response: string; timestamp: number }> = [];

  constructor(prologBackend: PrologBackend | null, telemetry: TelemetryCollector) {
    this.prologBackend = prologBackend;
    this.telemetry = telemetry;
    this.registry = new ChatCommandRegistry();
  }

  updateBackend(backend: PrologBackend | null) {
    this.prologBackend = backend;
  }

  async handleChatRequest(
    request: ChatRequest,
    context: any,
    stream: ChatResponseStream,
    token: CancellationToken
  ): Promise<ChatResult> {
    const startTime = Date.now();
    let command = 'unknown';
    try {
      if (!this.prologBackend) {
        stream.markdown('⚠️ **Backend Error**: Prolog backend not initialized. Please restart VS Code.');
        this.telemetry.collect({ command: 'init_error', success: false, error: 'backend_not_initialized', timestamp: startTime });
        return { metadata: { command: 'error' } };
      }

      if (!this.prologBackend.isRunning()) {
        stream.markdown('🔄 **Starting Prolog backend...**\n\n');
        // Optionally, implement backend startup logic here
      }

      const message = request.prompt.trim();
      const parts = message.split(/\s+/);
      command = parts[0]?.toLowerCase() || 'unknown';
      const args = parts.slice(1).join(' ');

      if (token.isCancellationRequested) {
        stream.markdown('❌ **Request cancelled**');
        this.telemetry.collect({ command, success: false, error: 'cancelled', timestamp: startTime });
        return { metadata: { command: 'cancelled' } };
      }

      // Modular command dispatch
      const handler = this.registry.findHandler(command);
      // Gather active file and workspace context
      let activeFile;
      const editor = window.activeTextEditor;
      if (editor) {
        activeFile = {
          fileName: editor.document.fileName,
          languageId: editor.document.languageId,
          isDirty: editor.document.isDirty,
          lineCount: editor.document.lineCount,
          selection: editor.selection,
          content: editor.document.getText(),
        };
      }
      const workspaceFolders = workspace.workspaceFolders?.map(f => f.uri.fsPath) || [];
      const settings = workspace.getConfiguration('prolog');
      const chatContext: ChatCommandContext = {
        prologBackend: this.prologBackend,
        telemetry: this.telemetry,
        activeFile,
        workspaceFolders,
        settings,
        chatHistory: this.chatHistory.slice(-20), // last 20 exchanges
      };
      let responseText = '';
      if (handler) {
        // Call the handler's handle method (args, request, stream, token, context)
        await handler.handle(args, request, stream, token, chatContext);
        responseText = '[handled by command]';
        this.telemetry.collect({ command, success: true, timestamp: startTime });
        this.chatHistory.push({ prompt: request.prompt, response: responseText, timestamp: Date.now() });
        return { metadata: { command } };
      } else {
        const unknownMsg = 'Type /help for available commands.';
        stream.markdown(unknownMsg);
        this.telemetry.collect({ command, success: false, error: 'unknown_command', timestamp: startTime });
        this.chatHistory.push({ prompt: request.prompt, response: unknownMsg, timestamp: Date.now() });
        return { metadata: { command: 'unknown' } };
      }
    } catch (error) {
      const errorMsg = error instanceof Error ? error.message : String(error);
      stream.markdown(`❌ **Error**: ${errorMsg}`);
      this.telemetry.collect({ command, success: false, error: errorMsg, timestamp: startTime });
      return { metadata: { command: 'error' } };
    }
  }
}