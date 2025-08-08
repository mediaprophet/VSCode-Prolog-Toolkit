import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class StatusCommand implements ChatCommand {
  name = '/status';
  description = 'Show the current status of the Prolog backend.';
  arguments = [];

  canHandle(command: string) {
    return command === '/status' || command === 'status';
  }

  async handle(
    args: string,
    request: ChatRequest,
    stream: ChatResponseStream,
    token: CancellationToken,
    context: ChatCommandContext
  ) {
    stream.markdown('Status command handler (stub)');
  }
}
