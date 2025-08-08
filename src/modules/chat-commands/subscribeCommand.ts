import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

// In-memory subscription registry (for demo; production should persist or use event emitter)
const subscribers = new Set<string>();

export class SubscribeCommand implements ChatCommand {
  name = '/subscribe';
  description = 'Subscribe the user/session to backend events (demo only).';
  arguments = [];
  canHandle(command: string): boolean {
    return command === 'subscribe' || command === '/subscribe';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    // For demo: subscribe user/session to backend events
    // Use sessionId or fallback to 'anonymous'
    const user = (request as any)?.sessionId || 'anonymous';
    subscribers.add(user);
    stream.markdown(`Subscribed **${user}** to backend events. (Demo only)`);
  }
}
