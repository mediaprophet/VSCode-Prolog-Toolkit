// ...existing code...
import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class N3ListCommand implements ChatCommand {
  name = '/n3_list';
  description = 'List loaded N3 files or graphs.';
  arguments = [];

  canHandle(command: string): boolean {
    return command === '/n3_list' || command === 'n3_list';
  }

  async handle(
    args: string,
    request: ChatRequest,
    stream: ChatResponseStream,
    token: CancellationToken,
    context: ChatCommandContext
  ): Promise<void> {
    stream.markdown('N3 list command handler (stub)');
  }
}

