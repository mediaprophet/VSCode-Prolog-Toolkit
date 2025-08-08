import * as fs from 'fs';
import * as path from 'path';
import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class ErrorsCommand implements ChatCommand {
  name = '/errors';
  description = 'Show the most recent problems/errors file from test/logs or logs/.';
  arguments = [];

  canHandle(command: string): boolean {
    return command === '/errors' || command === 'errors';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    // Look for recent problems files in test/logs or logs/
    const logDirs = [
      path.join(process.cwd(), 'test', 'logs'),
      path.join(process.cwd(), 'logs'),
    ];
    let foundProblems: string[] = [];
    for (const dir of logDirs) {
      if (fs.existsSync(dir)) {
        const files = fs.readdirSync(dir).filter(f => f.endsWith('.problems.json'));
        foundProblems = foundProblems.concat(files.map(f => path.join(dir, f)));
      }
    }
    if (foundProblems.length === 0) {
      stream.markdown('No error/problem files found.');
      return;
    }
    // Show the most recent problems file (by mtime)
    const sorted = foundProblems.map(f => ({ f, mtime: fs.statSync(f).mtime })).sort((a, b) => b.mtime.getTime() - a.mtime.getTime());
    if (sorted.length === 0) {
      stream.markdown('No error/problem files found.');
      return;
    }
    const latestProblems = sorted[0];
    if (!latestProblems) {
      stream.markdown('No error/problem files found.');
      return;
    }
    const problemsContent = fs.readFileSync(latestProblems.f, 'utf8');
    stream.markdown(`**Latest Problems File:** ${path.basename(latestProblems.f)}\n`);
    stream.markdown('```json\n' + problemsContent.slice(0, 2000) + '\n```');
  }
}
