import { CancellationToken, ChatRequest, ChatResponseStream, commands } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class RestartDebugCommand implements ChatCommand {
  name = '/restart_debug';
  description = 'Restart the current debug session.';
  arguments = [];
  canHandle(command: string): boolean {
    return command === 'restart_debug' || command === '/restart_debug';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    try {
      await commands.executeCommand('workbench.action.debug.restart');
      stream.markdown('Debug session restart triggered.');
    } catch (err) {
      stream.markdown('Failed to restart debug session: ' + err);
    }
  }
}
