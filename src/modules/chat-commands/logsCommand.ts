import * as fs from 'fs';
import * as path from 'path';
import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class LogsCommand implements ChatCommand {
  name = '/logs';
  description = 'Show the most recent log file from test/logs or logs/.';
  arguments = [];
  canHandle(command: string): boolean {
    return command === 'logs' || command === '/logs';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    // Look for recent log files in test/logs or logs/
    const logDirs = [
      path.join(process.cwd(), 'test', 'logs'),
      path.join(process.cwd(), 'logs'),
    ];
    let foundLogs: string[] = [];
    for (const dir of logDirs) {
      if (fs.existsSync(dir)) {
        const files = fs.readdirSync(dir).filter(f => f.endsWith('.log'));
        foundLogs = foundLogs.concat(files.map(f => path.join(dir, f)));
      }
    }
    if (foundLogs.length === 0) {
      stream.markdown('No log files found.');
      return;
    }
    // Show the most recent log (by mtime)
    const sorted = foundLogs.map(f => ({ f, mtime: fs.statSync(f).mtime })).sort((a, b) => b.mtime.getTime() - a.mtime.getTime());
    if (sorted.length === 0) {
      stream.markdown('No log files found.');
      return;
    }
    const latestLog = sorted[0];
    if (!latestLog) {
      stream.markdown('No log files found.');
      return;
    }
    const logContent = fs.readFileSync(latestLog.f, 'utf8');
    stream.markdown(`**Latest Log File:** ${path.basename(latestLog.f)}\n`);
    stream.markdown('```log\n' + logContent.slice(-2000) + '\n```');
  }
}
