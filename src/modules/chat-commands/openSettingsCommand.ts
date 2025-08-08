import { CancellationToken, ChatRequest, ChatResponseStream, commands } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class OpenSettingsCommand implements ChatCommand {
  name = '/open_settings';
  description = 'Open the VS Code settings UI.';
  arguments = [];
  canHandle(command: string): boolean {
    return command === 'open_settings' || command === '/open_settings';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    try {
      await commands.executeCommand('workbench.action.openSettings');
      stream.markdown('Settings UI opened.');
    } catch (err) {
      stream.markdown('Failed to open settings: ' + err);
    }
  }
}
