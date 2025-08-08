import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class ConsultCommand implements ChatCommand {
  name = '/consult';
  description = 'Consult a Prolog file in the backend.';
  arguments = [
    {
      name: 'file',
      type: 'file',
      description: 'The Prolog file to consult (e.g. family.pl).',
      example: 'family.pl',
      required: true
    }
  ];

  canHandle(command: string) {
    return command === '/consult' || command === 'consult';
  }

  async handle(
    args: string,
    request: ChatRequest,
    stream: ChatResponseStream,
    token: CancellationToken,
    context: ChatCommandContext
  ) {
    stream.markdown('Consult command handler (stub)');
  }
}
