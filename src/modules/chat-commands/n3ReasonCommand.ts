import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class N3ReasonCommand implements ChatCommand {
  name = '/n3_reason';
  description = 'Run N3 reasoning on loaded data.';
  arguments = [];

  canHandle(command: string): boolean {
    return command === '/n3_reason' || command === 'n3_reason';
  }

  async handle(
    args: string,
    request: ChatRequest,
    stream: ChatResponseStream,
    token: CancellationToken,
    context: ChatCommandContext
  ): Promise<void> {
    stream.markdown('N3 reason command handler (stub)');
  }
}
