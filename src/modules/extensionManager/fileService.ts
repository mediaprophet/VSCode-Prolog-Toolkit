import * as path from 'path';
import * as vscode from 'vscode';

export class FileService {
  async createNewPrologFile(window: typeof vscode.window, workspace: typeof vscode.workspace): Promise<void> {
    const fileName = await window.showInputBox({
      prompt: 'Enter the name for the new Prolog file',
      value: 'untitled.pl',
      validateInput: value => {
        if (!value) return 'File name cannot be empty';
        if (!value.match(/\.(pl|pro|prolog|plt|ecl)$/)) {
          return 'File must have a Prolog extension (.pl, .pro, .prolog, .plt, .ecl)';
        }
        return null;
      },
    });

    if (fileName) {
      const workspaceFolder = workspace.workspaceFolders?.[0];
      if (workspaceFolder) {
        const uri = window.activeTextEditor?.document.uri || workspaceFolder.uri;
        const newFileUri = uri.with({ path: path.join(path.dirname(uri.fsPath), fileName) });
        const edit = new vscode.WorkspaceEdit();
        edit.createFile(newFileUri, { ignoreIfExists: false });
        await Promise.resolve(workspace.applyEdit(edit));
        await window.showTextDocument(newFileUri);
      } else {
        const doc = await workspace.openTextDocument({
          language: 'prolog',
          content: `% ${fileName}\n% New Prolog file\n\n`,
        });
        await window.showTextDocument(doc);
      }
    }
  }
}
