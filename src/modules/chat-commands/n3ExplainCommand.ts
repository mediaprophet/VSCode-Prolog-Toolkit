import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class N3ExplainCommand implements ChatCommand {
  name = '/n3_explain';
  description = 'Explain N3 reasoning results.';
  arguments = [];

  canHandle(command: string): boolean {
    return command === '/n3_explain' || command === 'n3_explain';
  }

  async handle(
    args: string,
    request: ChatRequest,
    stream: ChatResponseStream,
    token: CancellationToken,
    context: ChatCommandContext
  ): Promise<void> {
    stream.markdown('N3 explain command handler (stub)');
  }
}
