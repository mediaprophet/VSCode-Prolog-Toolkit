import { TextDocument } from 'vscode-languageserver-textdocument';
import { WorkspaceEdit, Range, Position } from 'vscode-languageserver/node';
import { RenameProvider, LSPContext } from './types';

export class PrologRenameProvider implements RenameProvider {
  async prepareRename(document: TextDocument, position: Position, context: LSPContext): Promise<Range | null> {
    const word = this.getWordAtPosition(document.getText(), position);

    if (!word || !this.isValidPredicateName(word)) {
      return null;
    }

    const line = document.getText().split('\n')[position.line];
    const start = line.indexOf(word, position.character - word.length);

    if (start === -1) {
      return null;
    }

    return {
      start: { line: position.line, character: start },
      end: { line: position.line, character: start + word.length },
    };
  }

  async provideRename(
    document: TextDocument, 
    position: Position, 
    newName: string, 
    context: LSPContext
  ): Promise<WorkspaceEdit | null> {
    const oldName = this.getWordAtPosition(document.getText(), position);

    if (!oldName || !newName || !this.isValidPredicateName(newName)) {
      return null;
    }

    // Validate that the new name is a valid Prolog identifier
    if (!this.isValidPredicateName(newName)) {
      return null;
    }

    // Find all occurrences of the predicate in the document
    const changes = this.findAndReplacePredicateOccurrences(document, oldName, newName);

    if (changes.length === 0) {
      return null;
    }

    return {
      changes: {
        [document.uri]: changes,
      },
    };
  }

  private getWordAtPosition(text: string, position: Position): string | null {
    const lines = text.split('\n');
    const line = lines[position.line];
    if (!line) {
      return null;
    }

    const char = position.character;
    let start = char;
    let end = char;

    // Find word boundaries
    while (start > 0 && /[a-zA-Z0-9_]/.test(line[start - 1])) {
      start--;
    }
    while (end < line.length && /[a-zA-Z0-9_]/.test(line[end])) {
      end++;
    }

    return start < end ? line.substring(start, end) : null;
  }

  private isValidPredicateName(name: string): boolean {
    // Prolog predicate names must start with lowercase letter and contain only letters, digits, and underscores
    return /^[a-z][a-zA-Z0-9_]*$/.test(name);
  }

  private findAndReplacePredicateOccurrences(
    document: TextDocument, 
    oldName: string, 
    newName: string
  ): Array<{range: Range, newText: string}> {
    const text = document.getText();
    const lines = text.split('\n');
    const changes: Array<{range: Range, newText: string}> = [];

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      
      // Find all occurrences of the predicate in this line
      const regex = new RegExp(`\\b${this.escapeRegex(oldName)}\\b`, 'g');
      let match;

      while ((match = regex.exec(line)) !== null) {
        // Check if this occurrence should be renamed
        if (this.shouldRenameOccurrence(line, match.index, oldName)) {
          changes.push({
            range: {
              start: { line: i, character: match.index },
              end: { line: i, character: match.index + oldName.length },
            },
            newText: newName,
          });
        }
      }
    }

    return changes;
  }

  private shouldRenameOccurrence(line: string, index: number, predicateName: string): boolean {
    // Don't rename if it's inside a comment
    const beforeMatch = line.substring(0, index);
    if (beforeMatch.includes('%')) {
      return false;
    }

    // Don't rename if it's inside a string
    if (this.isInsideString(line, index)) {
      return false;
    }

    // Check context to determine if this is a predicate reference
    const afterPredicate = line.substring(index + predicateName.length);
    
    // It's likely a predicate if:
    // 1. It's followed by an opening parenthesis (predicate call or definition)
    // 2. It's at the start of a line (fact or rule head)
    // 3. It's preceded by common Prolog constructs
    
    const followedByParen = afterPredicate.trim().startsWith('(');
    const startsLine = beforeMatch.trim() === '' || beforeMatch.trim() === ':-';
    
    return followedByParen || startsLine;
  }

  private isInsideString(line: string, index: number): boolean {
    let inString = false;
    let stringChar = '';

    for (let i = 0; i < index; i++) {
      const char = line[i];
      
      if (!inString && (char === '"' || char === "'")) {
        inString = true;
        stringChar = char;
      } else if (inString && char === stringChar && line[i - 1] !== '\\') {
        inString = false;
      }
    }

    return inString;
  }

  private escapeRegex(str: string): string {
    return str.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  }

  // Additional utility methods

  // Rename across multiple documents (for workspace-wide rename)
  public async provideWorkspaceRename(
    documents: TextDocument[],
    originalDocument: TextDocument,
    position: Position,
    newName: string,
    context: LSPContext
  ): Promise<WorkspaceEdit | null> {
    const oldName = this.getWordAtPosition(originalDocument.getText(), position);

    if (!oldName || !newName || !this.isValidPredicateName(newName)) {
      return null;
    }

    const allChanges: { [uri: string]: Array<{range: Range, newText: string}> } = {};

    // Process each document
    for (const document of documents) {
      const changes = this.findAndReplacePredicateOccurrences(document, oldName, newName);
      if (changes.length > 0) {
        allChanges[document.uri] = changes;
      }
    }

    if (Object.keys(allChanges).length === 0) {
      return null;
    }

    return { changes: allChanges };
  }

  // Check if rename is safe (no conflicts)
  public checkRenameSafety(
    document: TextDocument,
    oldName: string,
    newName: string
  ): { safe: boolean; conflicts: string[] } {
    const conflicts: string[] = [];

    // Check if the new name conflicts with existing predicates
    const existingPredicates = this.findExistingPredicates(document);
    
    if (existingPredicates.includes(newName)) {
      conflicts.push(`Predicate '${newName}' already exists`);
    }

    // Check if the new name conflicts with built-in predicates
    if (this.isBuiltinPredicate(newName)) {
      conflicts.push(`'${newName}' is a built-in predicate`);
    }

    // Check if the new name follows Prolog naming conventions
    if (!this.followsNamingConventions(newName)) {
      conflicts.push(`'${newName}' does not follow Prolog naming conventions`);
    }

    return {
      safe: conflicts.length === 0,
      conflicts,
    };
  }

  private findExistingPredicates(document: TextDocument): string[] {
    const text = document.getText();
    const lines = text.split('\n');
    const predicates: string[] = [];

    for (const line of lines) {
      const match = line.match(/^([a-z][a-zA-Z0-9_]*)\s*\(/);
      if (match) {
        const predicateName = match[1];
        if (!predicates.includes(predicateName)) {
          predicates.push(predicateName);
        }
      }
    }

    return predicates;
  }

  private isBuiltinPredicate(name: string): boolean {
    const builtins = [
      'member', 'append', 'length', 'reverse', 'sort', 'findall', 'bagof', 'setof',
      'assert', 'retract', 'asserta', 'assertz', 'retractall',
      'write', 'writeln', 'read', 'get', 'put', 'nl', 'tab',
      'is', 'var', 'nonvar', 'atom', 'number', 'compound', 'atomic',
      'functor', 'arg', 'univ', 'copy_term', 'numbervars',
      'call', 'once', 'ignore', 'forall', 'between', 'succ',
      'true', 'fail', 'halt', 'abort', 'trace', 'notrace', 'spy', 'nospy',
      'cut', 'if', 'then', 'else'
    ];

    return builtins.includes(name);
  }

  private followsNamingConventions(name: string): boolean {
    // Check basic Prolog naming rules
    if (!this.isValidPredicateName(name)) {
      return false;
    }

    // Additional convention checks
    // Predicates should not be too short (except common ones)
    if (name.length === 1 && !['a', 'b', 'c', 'p', 'q', 'r', 's', 't'].includes(name)) {
      return false;
    }

    // Predicates should not be all uppercase (that's for variables)
    if (name === name.toUpperCase()) {
      return false;
    }

    return true;
  }

  // Get preview of rename changes
  public getRenamePreview(
    document: TextDocument,
    oldName: string,
    newName: string
  ): Array<{ line: number; oldText: string; newText: string }> {
    const changes = this.findAndReplacePredicateOccurrences(document, oldName, newName);
    const lines = document.getText().split('\n');
    const preview: Array<{ line: number; oldText: string; newText: string }> = [];

    for (const change of changes) {
      const lineNumber = change.range.start.line;
      const oldText = lines[lineNumber];
      const newText = oldText.substring(0, change.range.start.character) +
                     change.newText +
                     oldText.substring(change.range.end.character);
      
      preview.push({
        line: lineNumber,
        oldText,
        newText,
      });
    }

    return preview;
  }
}