import { CancellationToken, ChatRequest, ChatResponseStream, commands } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class InstallCommand implements ChatCommand {
  name = '/install';
  description = 'Install a VS Code extension by ID or name.';
  arguments = [
    {
      name: 'extension',
      type: 'string',
      description: 'The extension ID or name to install (e.g. ms-python.python).',
      example: 'ms-python.python',
      required: true
    }
  ];
  canHandle(command: string): boolean {
    return command === 'install' || command === '/install';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    const extId = args.trim();
    if (!extId) {
      stream.markdown('Please provide an extension ID or name.');
      return;
    }
    try {
      await commands.executeCommand('workbench.extensions.installExtension', extId);
      stream.markdown(`Extension **${extId}** installation triggered.`);
    } catch (err) {
      stream.markdown('Failed to install extension: ' + err);
    }
  }
}
