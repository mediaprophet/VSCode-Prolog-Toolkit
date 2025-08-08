import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class N3LoadCommand implements ChatCommand {
  name = '/n3_load';
  description = 'Load an N3 file into the backend.';
  arguments = [
    {
      name: 'file',
      type: 'file',
      description: 'The N3 or Turtle file to load (e.g. data.n3).',
      example: 'data.n3',
      required: true
    }
  ];
  canHandle(command: string) {
    return command === '/n3_load' || command === 'n3_load';
  }

  async handle(
    args: string,
    request: ChatRequest,
    stream: ChatResponseStream,
    token: CancellationToken,
    context: ChatCommandContext
  ) {
    stream.markdown('N3 load command handler (stub)');
  }
}
