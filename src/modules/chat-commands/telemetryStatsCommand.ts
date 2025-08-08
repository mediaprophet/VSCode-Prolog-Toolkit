import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class TelemetryStatsCommand implements ChatCommand {
  name = '/telemetry_stats';
  description = 'Show telemetry statistics collected by the extension.';
  arguments = [];
  canHandle(command: string): boolean {
    return command === 'telemetry_stats' || command === '/telemetry_stats';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    if (!context.telemetry) {
      stream.markdown('Telemetry is not available.');
      return;
    }
    const stats = context.telemetry.getStats();
    if (!stats) {
      stream.markdown('Telemetry is disabled or no data collected.');
      return;
    }
    stream.markdown('**Telemetry Stats:**\n');
    stream.markdown('```json\n' + JSON.stringify(stats, null, 2) + '\n```');
  }
}
