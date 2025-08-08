import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class TroubleshootCommand implements ChatCommand {
  name = '/troubleshoot';
  description = 'Run troubleshooting diagnostics and suggestions.';
  arguments = [];

  canHandle(command: string): boolean {
    return command === 'troubleshoot' || command === '/troubleshoot';
  }

  async handle(
    args: string,
    request: ChatRequest,
    stream: ChatResponseStream,
    token: CancellationToken,
    context: ChatCommandContext
  ): Promise<void> {
    // Provide actionable suggestions and health checks
    const suggestions = [
      'Restart the Prolog backend if queries are not responding.',
      'Check SWI-Prolog is installed and available in PATH.',
      'Use /logs to view recent extension logs.',
      'Use /debug for diagnostics.',
      'Consult the README for setup instructions.',
    ];
    stream.markdown('**Troubleshooting Suggestions:**\n');
    suggestions.forEach((s, i) => stream.markdown(`${i + 1}. ${s}`));
  }
}
