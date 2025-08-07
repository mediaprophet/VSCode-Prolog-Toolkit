import {
  commands,
  Disposable,
  Position,
  Range,
  type TextDocument,
  Uri,
  workspace,
  WorkspaceEdit,
} from 'vscode';
import type { ICommandManager } from './interfaces.js';

/**
 * Manages VS Code commands for Prolog linting functionality
 */
export class CommandManager implements ICommandManager {
  private commandAddDynamic!: Disposable;
  private commandAddUseModule!: Disposable;
  private commandAddDynamicId: string;
  private commandAddUseModuleId: string;

  constructor() {
    this.commandAddDynamicId = 'prolog.addDynamicDirective';
    this.commandAddUseModuleId = 'prolog.addUseModule';
  }

  /**
   * Register all commands
   */
  public registerCommands(): void {
    // Register command to dynamically declare a predicate as dynamic in Prolog code
    this.commandAddDynamic = commands.registerCommand(
      this.commandAddDynamicId,
      this.addDynamicDirective,
      this
    );

    // Register command to include a Prolog module in the code
    this.commandAddUseModule = commands.registerCommand(
      this.commandAddUseModuleId,
      this.addUseModule,
      this
    );
  }

  /**
   * Dispose of all registered commands
   */
  public dispose(): void {
    if (this.commandAddDynamic) {
      this.commandAddDynamic.dispose();
    }
    if (this.commandAddUseModule) {
      this.commandAddUseModule.dispose();
    }
  }

  /**
   * Add a dynamic directive to the document
   */
  public async addDynamicDirective(
    doc: TextDocument,
    predicate: string,
    uri: Uri,
    range: Range
  ): Promise<boolean> {
    const edit = new WorkspaceEdit();
    const line = this.getDirectiveLines(doc, 'dynamic', range)[0];

    const text = doc.lineAt(line || 0).text;
    let pos: Position;

    // Check if an existing 'dynamic' declaration is present
    if (/:-\s+\(?dynamic/.test(text)) {
      // If present, find the start character of the list and position the cursor after it
      const startChar = text.indexOf('dynamic') + 7;
      pos = new Position(line || 0, startChar);
      edit.insert(uri, pos, ' ' + predicate + ',');
    } else {
      // If not present, insert a new 'dynamic' declaration with the specified predicate
      pos = new Position(line || 0, 0);
      edit.insert(uri, pos, ':- dynamic ' + predicate + '.\n');
    }

    // Apply the WorkspaceEdit to the document
    try {
      const result = await Promise.resolve(workspace.applyEdit(edit));
      return result;
    } catch (e) {
      console.log('Error in add dynamic declaration: ' + e);
      return false;
    }
  }

  /**
   * Add a "use_module" directive to the document
   */
  public async addUseModule(
    doc: TextDocument,
    predicate: string,
    module: string,
    uri: Uri,
    range: Range
  ): Promise<boolean> {
    const edit = new WorkspaceEdit();
    const lines = this.getDirectiveLines(doc, 'use_module', range);
    const re = new RegExp('^:-\\s+use_module\\s*\\(\\s*.+\\b' + module + '\\b');
    let directiveLine: number = -1;
    let pos: Position;

    // Iterate through the lines to find an existing 'use_module' directive with the specified module
    lines.forEach(line => {
      if (re.test(doc.lineAt(line).text)) {
        directiveLine = line;
      }
    });

    // If an existing directive is found, add the predicate to the existing list
    if (directiveLine >= 0) {
      let line = directiveLine;
      // Move down to find the position where the predicate should be added
      while (doc.lineAt(line).text.indexOf('[') < 0) line++;
      // Find the start character of the list and position the cursor after it
      const startChar = doc.lineAt(line).text.indexOf('[');
      pos = new Position(line, startChar + 1);
      edit.insert(uri, pos, predicate + ',');
    } else {
      // If no existing directive is found, add a new 'use_module' directive
      const lastLine = lines[lines.length - 1] ?? 0;
      pos = new Position(lastLine, 0);
      edit.insert(uri, pos, `:- use_module(library(${module}), [${predicate}]).\n`);
    }

    // Apply the WorkspaceEdit to the document
    try {
      const result = await Promise.resolve(workspace.applyEdit(edit));
      return result;
    } catch (e) {
      console.log('Error in add use_module declaration: ' + e);
      return false;
    }
  }

  /**
   * Get lines where a directive should be added based on predicate and range
   */
  private getDirectiveLines(
    doc: TextDocument,
    declarativePredicate: string,
    range: Range
  ): number[] {
    const textlines: string[] = doc.getText().split('\n');
    const re = new RegExp('^\\s*:-\\s+\\(?\\s*' + declarativePredicate + '\\b');
    let lines: number[] = [];
    let line = 0;

    while (line < textlines.length) {
      if (re.test(textlines[line] || '')) {
        lines = lines.concat(line);
      }
      line++;
    }

    // If lines with the directive are found, return the array of line numbers
    if (lines.length > 0) {
      return lines;
    }

    line = -1;
    // Find the first line starting with ":-"
    textlines.filter((item, index) => {
      if (/^\s*:-/.test(item)) {
        line = index;
        return true;
      }
      return false;
    });

    // Continue iterating to find the end of the directive or the end of the document
    while (line >= 0 && line < textlines.length && !/.+\.(\s*%.*)*/.test(textlines[line] || '')) {
      line++;
    }

    // If the line is before the specified range, return an array with the next line
    if (line >= 0 && line < range.start.line) {
      return [++line];
    }

    line = 0;
    // Check for the presence of a comment block at the beginning of the document
    let inComment = /\s*\/\*/.test(textlines[0] || '');

    // Continue iterating until the end of the comment block is found
    while (inComment && line < textlines.length) {
      if (/\*\//.test(textlines[line] || '')) {
        inComment = false;
        line++;
        break;
      }
      line++;
    }

    // Return an array with the line number after the comment block
    return [line];
  }

  /**
   * Get command IDs
   */
  public getCommandIds(): { addDynamic: string; addUseModule: string } {
    return {
      addDynamic: this.commandAddDynamicId,
      addUseModule: this.commandAddUseModuleId,
    };
  }

  /**
   * Check if a directive exists in the document
   */
  public hasDirective(doc: TextDocument, directive: string): boolean {
    const text = doc.getText();
    const regex = new RegExp(`:-\\s*${directive}\\b`);
    return regex.test(text);
  }

  /**
   * Find existing use_module directives for a module
   */
  public findUseModuleDirectives(doc: TextDocument, module: string): number[] {
    const lines: number[] = [];
    const textlines = doc.getText().split('\n');
    const regex = new RegExp(`:-\\s*use_module\\s*\\([^)]*\\b${module}\\b`);

    textlines.forEach((line, index) => {
      if (regex.test(line)) {
        lines.push(index);
      }
    });

    return lines;
  }

  /**
   * Extract predicates from a use_module directive line
   */
  public extractPredicatesFromUseModule(lineText: string): string[] {
    const match = lineText.match(/\[([^\]]+)\]/);
    if (!match || typeof match[1] !== 'string') {
      return [];
    }

    return match[1]
      .split(',')
      .map(p => p.trim())
      .filter(p => p.length > 0);
  }

  /**
   * Check if a predicate is already in a use_module directive
   */
  public isPredicateInUseModule(doc: TextDocument, module: string, predicate: string): boolean {
    const lines = this.findUseModuleDirectives(doc, module);
    for (const lineIndex of lines) {
      const lineText = doc.lineAt(lineIndex).text;
      const predicates = this.extractPredicatesFromUseModule(lineText);
      if (predicates.includes(predicate)) {
        return true;
      }
    }
    return false;
  }
}
