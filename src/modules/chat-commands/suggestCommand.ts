import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class SuggestCommand implements ChatCommand {
  name = '/suggest';
  description = 'Suggest next steps or commands based on context.';
  arguments = [];
  canHandle(command: string): boolean {
    return command === 'suggest' || command === '/suggest';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    // Suggest next steps based on last error or context
    let suggestion = 'No suggestions available.';
    if (context.telemetry) {
      const stats = context.telemetry.getStats();
      if (stats && stats.totalCommands > 0) {
        suggestion = 'Try /debug, /logs, or /troubleshoot for more information.';
      }
    }
    stream.markdown('**Next Step Suggestion:** ' + suggestion);
  }
}
