import { CancellationToken, ChatRequest, ChatResponseStream, commands } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class ReloadBackendCommand implements ChatCommand {
  name = '/reload_backend';
  description = 'Reload the Prolog backend service.';
  arguments = [];
  canHandle(command: string): boolean {
    return command === 'reload_backend' || command === '/reload_backend';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    try {
      await commands.executeCommand('vscode-prolog-toolkit.reloadBackend');
      stream.markdown('Prolog backend reload triggered.');
    } catch (err) {
      stream.markdown('Failed to reload backend: ' + err);
    }
  }
}
