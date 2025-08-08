import { CancellationToken, ChatRequest, ChatResponseStream, languages } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class ProblemsCommand implements ChatCommand {
  name = '/problems';
  description = 'Show current diagnostics from the Problems panel.';
  arguments = [];
  canHandle(command: string): boolean {
    return command === 'problems' || command === '/problems';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    // Show current diagnostics from Problems panel
    const allDiagnostics = languages.getDiagnostics();
    if (!allDiagnostics.length) {
      stream.markdown('No problems found.');
      return;
    }
    let output = '**Problems Panel Diagnostics:**\n';
    for (const [uri, diags] of allDiagnostics) {
      if (!diags.length) continue;
      output += `- **${uri.fsPath}**\n`;
      for (const d of diags) {
        output += `  - [${d.severity}] ${d.message} (Line ${d.range.start.line + 1})\n`;
      }
    }
    stream.markdown(output);
  }
}
