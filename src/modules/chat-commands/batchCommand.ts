import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class BatchCommand implements ChatCommand {
  name = '/batch';
  description = 'Run a batch of Prolog queries.';
  arguments = [
    {
      name: 'queries',
      type: 'string',
      description: 'Batch queries (semicolon-separated)',
      example: 'member(X,[1,2,3]);length([a,b],X)',
      required: true
    }
  ];

  canHandle(command: string) {
    return command === '/batch' || command === 'batch';
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
    const queries = args.split(';').map(q => q.trim()).filter(q => q.length > 0);
    try {
      const result = await backend.sendRequest('batch_query_execution', {
        queries
      });
      stream.markdown('**Batch Query Results:**\n' + '```json\n' + JSON.stringify(result, null, 2) + '\n```');
    } catch (err) {
      stream.markdown('❌ **Error running batch queries:** ' + (err instanceof Error ? err.message : String(err)));
    }
  }
}
