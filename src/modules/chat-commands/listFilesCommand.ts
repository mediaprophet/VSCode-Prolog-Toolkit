import * as fs from 'fs';
import * as path from 'path';
import { CancellationToken, ChatRequest, ChatResponseStream, workspace } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class ListFilesCommand implements ChatCommand {
  name = '/list_files';
  description = 'List files in the workspace (depth 2).';
  arguments = [];
  canHandle(command: string): boolean {
    return command === 'list_files';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    try {
      const folders = workspace.workspaceFolders?.map(f => f.uri.fsPath) || [];
      let files: string[] = [];
      for (const folder of folders) {
        files = files.concat(await this.listFilesRecursive(folder, 2)); // limit depth for performance
      }
      stream.markdown('**Workspace Files (depth 2):**\n');
      stream.markdown('```json\n' + JSON.stringify(files, null, 2) + '\n```');
    } catch (err) {
      stream.markdown('Failed to list files: ' + err);
      stream.markdown('Try `/troubleshoot`, `/logs`, or see the [README](https://github.com/mediaprophet/VSCode-Prolog-Toolkit#readme) for help.');
    }
  }

  private async listFilesRecursive(dir: string, depth: number): Promise<string[]> {
    if (depth < 0) return [];
    let results: string[] = [];
    const entries = await fs.promises.readdir(dir, { withFileTypes: true });
    for (const entry of entries) {
      const fullPath = path.join(dir, entry.name);
      results.push(fullPath);
      if (entry.isDirectory()) {
        results = results.concat(await this.listFilesRecursive(fullPath, depth - 1));
      }
    }
    return results;
  }
}
