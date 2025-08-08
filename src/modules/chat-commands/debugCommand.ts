import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class DebugCommand implements ChatCommand {
  name = '/debug';
  description = 'Show extension and backend debug diagnostics.';
  arguments = [];
  canHandle(command: string): boolean {
    return command === 'debug' || command === '/debug';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    // Basic diagnostics: extension status, backend status, memory, etc.
    const diagnostics = {
      extensionActive: true,
      backendStatus: context.prologBackend?.isRunning ? context.prologBackend.isRunning() : 'unknown',
      memoryUsage: process.memoryUsage(),
      platform: process.platform,
      nodeVersion: process.version,
      cwd: process.cwd(),
      timestamp: new Date().toISOString(),
    };
    stream.markdown('**Debug Diagnostics:**\n');
    stream.markdown('```json\n' + JSON.stringify(diagnostics, null, 2) + '\n```');
  }
}
