import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class QueryCommand implements ChatCommand {
  name = '/query';
  description = 'Run a Prolog query in the backend.';
  arguments = [
    {
      name: 'goal',
      type: 'string',
      description: 'The Prolog goal to execute (e.g. member(X, [1,2,3])).',
      example: 'member(X, [1,2,3])',
      required: true
    }
  ];

  canHandle(command: string) {
    return command === '/query' || command === 'query';
  }

  async handle(
    args: string,
    request: ChatRequest,
    stream: ChatResponseStream,
    token: CancellationToken,
    context: ChatCommandContext
  ) {
    // Parse the Prolog goal from args
    const goal = args.trim();
    if (!goal) {
      stream.markdown('❌ **Error:** No Prolog goal provided. Usage: `/query member(X, [1,2,3])`');
      return;
    }

    const backend = context.prologBackend;
    if (!backend) {
      stream.markdown('⚠️ **Backend Error**: Prolog backend not initialized.');
      return;
    }

    // Streaming handler for chunked results
    let isFirstChunk = true;
    let cancelled = false;
    try {
      await backend.sendStreamingRequest(
        'query',
        { goal },
        (chunk: any, isFirst: boolean, isLast: boolean) => {
          if (token.isCancellationRequested) {
            cancelled = true;
            backend.cancelQuery(chunk.id || chunk.queryId || '');
            stream.markdown('❌ **Query cancelled by user.**');
            return;
          }
          // Stream each chunk as partial result
          if (isFirst) {
            stream.markdown('**Prolog Query Results:**\n');
          }
          if (chunk && chunk.results && Array.isArray(chunk.results) && chunk.results.length > 0) {
            const formatted = chunk.results.map((r: any, i: number) => `- ${JSON.stringify(r)}`).join('\n');
            stream.markdown(formatted);
          }
          if (isLast) {
            stream.markdown('\n✅ **Query complete.**');
          }
        }
      );
    } catch (err) {
      if (cancelled || token.isCancellationRequested) {
        stream.markdown('❌ **Query cancelled.**');
        return;
      }
      const msg = err instanceof Error ? err.message : String(err);
      stream.markdown(`❌ **Error running query:** ${msg}`);
    }
  }
}
