import { CancellationToken, ChatRequest, ChatResponseStream, workspace } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class EnvCommand implements ChatCommand {
  name = '/env';
  description = 'Show environment and workspace context.';
  arguments = [];
  canHandle(command: string): boolean {
    return command === 'env';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    try {
      const folders = workspace.workspaceFolders?.map(f => f.uri.fsPath) || [];
      const envInfo = {
        workspaceFolders: folders,
        platform: process.platform,
        nodeVersion: process.version,
        cwd: process.cwd(),
        env: process.env,
      };
      stream.markdown('**Environment Context:**\n');
      stream.markdown('```json\n' + JSON.stringify(envInfo, null, 2) + '\n```');
    } catch (err) {
      stream.markdown('Failed to fetch environment context: ' + err);
      stream.markdown('Try `/troubleshoot`, `/logs`, or see the [README](https://github.com/mediaprophet/VSCode-Prolog-Toolkit#readme) for help.');
    }
  }
}
