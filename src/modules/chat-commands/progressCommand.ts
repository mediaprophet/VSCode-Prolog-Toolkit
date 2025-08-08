import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class ProgressCommand implements ChatCommand {
  name = '/progress';
  description = 'Show backend and task progress status.';
  arguments = [];
  canHandle(command: string): boolean {
    return command === 'progress' || command === '/progress';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    // Example: Show status of backend and any known long-running tasks
    let progressInfo = '**Progress Status:**\n';
    if (context.prologBackend) {
      progressInfo += `- Backend running: ${context.prologBackend.isRunning ? context.prologBackend.isRunning() : 'unknown'}\n`;
    } else {
      progressInfo += '- Backend: unknown\n';
    }
    // Add more task status as needed
    stream.markdown(progressInfo);
  }
}
