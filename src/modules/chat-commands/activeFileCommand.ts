import { CancellationToken, ChatRequest, ChatResponseStream, window } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class ActiveFileCommand implements ChatCommand {
  name = '/active_file';
  description = 'Show or operate on the active file in the editor.';
  arguments = [];

  canHandle(command: string): boolean {
    return command === '/active_file' || command === 'active_file';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    try {
      const editor = window.activeTextEditor;
      if (!editor) {
        stream.markdown('No active file.');
        return;
      }
      const fileInfo = {
        fileName: editor.document.fileName,
        languageId: editor.document.languageId,
        isDirty: editor.document.isDirty,
        lineCount: editor.document.lineCount,
        selection: editor.selection,
      };
      stream.markdown('**Active File Info:**\n');
      stream.markdown('```json\n' + JSON.stringify(fileInfo, null, 2) + '\n```');
    } catch (err) {
      stream.markdown('Failed to fetch active file info: ' + err);
      stream.markdown('Try `/troubleshoot`, `/logs`, or see the [README](https://github.com/mediaprophet/VSCode-Prolog-Toolkit#readme) for help.');
    }
  }
}
