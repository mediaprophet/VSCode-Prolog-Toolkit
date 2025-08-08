import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class ProbabilisticCommand implements ChatCommand {
  name = '/probabilistic';
  description = 'Run probabilistic inference.';
  arguments = [
    {
      name: 'query',
      type: 'string',
      description: 'Query for probabilistic inference',
      example: 'rain',
      required: true
    },
    {
      name: 'facts',
      type: 'string',
      description: 'Probabilistic facts (comma-separated, e.g. rain:0.3,sprinkler:0.5)',
      example: 'rain:0.3,sprinkler:0.5',
      required: false
    },
    {
      name: 'samples',
      type: 'number',
      description: 'Number of Monte Carlo samples',
      example: '1000',
      required: false
    }
  ];

  canHandle(command: string) {
    return command === '/probabilistic' || command === 'probabilistic';
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
    // Parse args: query | facts | samples
    const [query, factsRaw, samplesRaw] = args.split('|').map(s => s.trim());
    const facts = factsRaw ? factsRaw.split(',').map(f => {
      const [fact, prob] = f.split(':');
      if (typeof fact === 'string' && fact.trim().length > 0 && typeof prob === 'string' && !isNaN(Number(prob))) {
        return { fact: fact.trim(), probability: parseFloat(prob) };
      }
      return undefined;
    }).filter((item): item is { fact: string; probability: number } => !!item) : [];
    const samples = samplesRaw ? parseInt(samplesRaw, 10) : 1000;
    try {
      const result = await backend.sendRequest('probabilistic_inference', {
        query,
        facts,
        samples
      });
      stream.markdown('**Probabilistic Inference Result:**\n' + '```json\n' + JSON.stringify(result, null, 2) + '\n```');
    } catch (err) {
      stream.markdown('❌ **Error running probabilistic inference:** ' + (err instanceof Error ? err.message : String(err)));
    }
  }
}
