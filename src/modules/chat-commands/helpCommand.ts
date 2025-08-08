import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class HelpCommand implements ChatCommand {
  name = '/help';
  description = 'Show help for chat commands.';
  arguments = [
    {
      name: 'command',
      type: 'string',
      description: 'The command to show help for (e.g. /query).',
      example: '/query',
      required: false
    }
  ];

  canHandle(command: string) {
    return command === '/help' || command === 'help';
  }

  async handle(
    args: string,
    request: ChatRequest,
    stream: ChatResponseStream,
    token: CancellationToken,
    context: ChatCommandContext
  ) {
    stream.markdown('Help command handler (stub)');
  }
}
