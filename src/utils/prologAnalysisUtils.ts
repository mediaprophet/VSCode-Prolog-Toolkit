import { RelativePattern, Uri, workspace, WorkspaceEdit } from 'vscode';

/**
 * Utility methods for advanced Prolog code analysis, including cross-file predicate resolution and refactoring helpers.
 */
export class PrologAnalysisUtils {
  /**
   * Find all files in the workspace that define a given predicate (by functor/arity).
   * Returns an array of {uri, line, predicate} objects.
   */
  static async findPredicateDefinitions(functor: string, arity: number): Promise<Array<{ uri: Uri, line: number, predicate: string }>> {
    const results: Array<{ uri: Uri, line: number, predicate: string }> = [];
    const folders = workspace.workspaceFolders;
    if (!folders) return results;
    for (const folder of folders) {
      const files = await workspace.findFiles(new RelativePattern(folder, '**/*.pl'));
      for (const file of files) {
        const doc = await workspace.openTextDocument(file);
        const text = doc.getText();
        const regex = new RegExp(`(^|\n)\s*${functor}\\s*\\(([^)]*)\\)`, 'g');
        let match;
        while ((match = regex.exec(text)) !== null) {
          if (match[2]) {
            const params = match[2].split(',').map(s => s.trim()).filter(Boolean);
            if (params.length === arity) {
              const line = doc.positionAt(match.index).line;
              results.push({ uri: file, line, predicate: `${functor}/${arity}` });
            }
          }
        }
      }
    }
    return results;
  }

  /**
   * Refactor all occurrences of a predicate (functor/arity) to a new name in the workspace.
   * Returns a list of files and line numbers changed.
   */
  static async refactorPredicateName(oldFunctor: string, oldArity: number, newFunctor: string): Promise<Array<{ uri: Uri, line: number }>> {
    const changes: Array<{ uri: Uri, line: number }> = [];
    const defs = await this.findPredicateDefinitions(oldFunctor, oldArity);
    for (const def of defs) {
      const doc = await workspace.openTextDocument(def.uri);
      const edit = new WorkspaceEdit();
      const lineText = doc.lineAt(def.line).text;
      const newLine = lineText.replace(new RegExp(`\\b${oldFunctor}\\b`), newFunctor);
      edit.replace(def.uri, doc.lineAt(def.line).range, newLine);
      await workspace.applyEdit(edit);
      changes.push({ uri: def.uri, line: def.line });
    }
    return changes;
  }
}
