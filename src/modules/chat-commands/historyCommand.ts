import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class HistoryCommand implements ChatCommand {
  name = '/history';
  description = 'Show recent Prolog query history.';
  arguments = [];

  canHandle(command: string) {
    return command === '/history' || command === 'history';
  }

  async handle(
    args: string,
    request: ChatRequest,
    stream: ChatResponseStream,
    token: CancellationToken,
    context: ChatCommandContext
  ) {
    const backend = context.prologBackend;
    if (!backend) {
      stream.markdown('⚠️ **Backend Error**: Prolog backend not initialized.');
      return;
    }
    try {
      const result = await backend.sendRequest('query_history', {});
      if (result && Array.isArray(result.history)) {
        stream.markdown('**Recent Query History:**\n' +
          result.history.map((q: any, i: number) => `- ${q.query} (${q.success ? '✅' : '❌'})`).join('\n')
        );
      } else {
        stream.markdown('No query history found.');
      }
    } catch (err) {
      stream.markdown('❌ **Error fetching query history:** ' + (err instanceof Error ? err.message : String(err)));
    }
  }
}
