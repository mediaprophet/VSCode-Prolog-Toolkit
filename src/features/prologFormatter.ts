import {
  CancellationToken,
  commands,
  DocumentFormattingEditProvider,
  DocumentRangeFormattingEditProvider,
  FormattingOptions,
  OutputChannel,
  ProviderResult,
  Range,
  TextDocument,
  TextEdit,
  window,
  workspace,
} from 'vscode';
import { ExecutableFinder } from '../utils/executableFinder';
import { PlatformUtils } from '../utils/platformUtils';
import { InstallationGuide } from './installation/InstallationGuide';

export class PrologFormatter
  implements DocumentRangeFormattingEditProvider, DocumentFormattingEditProvider {
  private _section: ReturnType<typeof workspace.getConfiguration>;
  private _tabSize!: number;
  private _insertSpaces!: boolean;
  private _tabDistance!: number;
  private _executable: string;
  private _args: string[];
  private _outputChannel: OutputChannel;
  private _textEdits: TextEdit[] = [];
  private _startChars!: number;

  // Constructor for the PrologFormatter class
  constructor() {
    this._section = workspace.getConfiguration('prolog');
    const execPath = this._section.get('executablePath', PlatformUtils.getDefaultExecutablePath());
    this._executable = PlatformUtils.normalizePath(execPath);
    this._args = [];
    this._outputChannel = window.createOutputChannel('PrologFormatter');

    // Initialize with better executable resolution
    this.initializeExecutable();
  }

  /**
   * Initialize executable with enhanced detection and permission checking
   */
  private async initializeExecutable(): Promise<void> {
    const configuredPath = this._section.get<string>(
      'executablePath',
      PlatformUtils.getDefaultExecutablePath()
    );

    // Check if configured path exists and is executable
    if (await PlatformUtils.pathExists(configuredPath)) {
      if (await PlatformUtils.isExecutable(configuredPath)) {
        this._executable = PlatformUtils.normalizePath(configuredPath);
        return;
      } else {
        // Path exists but lacks execute permissions
        const platform = PlatformUtils.getPlatform();
        if (platform !== 'windows') {
          window
            .showWarningMessage(
              `SWI-Prolog executable at '${configuredPath}' lacks execute permissions. Try: chmod +x "${configuredPath}"`,
              'Fix Permissions'
            )
            .then(action => {
              if (action === 'Fix Permissions') {
                const terminal = window.createTerminal('Fix Prolog Permissions');
                terminal.sendText(`chmod +x "${configuredPath}"`);
                terminal.show();
              }
            });
        }
      }
    }

    // Try to find executable using comprehensive detection
    const executableFinder = new ExecutableFinder();
    const detectionResult = await executableFinder.findSwiplExecutable();

    if (detectionResult.found && detectionResult.path) {
      if (detectionResult.permissions?.executable) {
        this._executable = detectionResult.path;

        // Update configuration if we found a different path
        if (detectionResult.path !== configuredPath) {
          window
            .showInformationMessage(
              `Found SWI-Prolog at '${detectionResult.path}' via ${detectionResult.detectionMethod}. Update configuration?`,
              'Yes',
              'No'
            )
            .then(action => {
              if (action === 'Yes') {
                this._section.update('executablePath', detectionResult.path, true);
              }
            });
        }
      } else {
        // Found executable but has permission issues
        const platform = PlatformUtils.getPlatform();
        if (platform !== 'windows') {
          window
            .showWarningMessage(
              `Found SWI-Prolog at '${detectionResult.path}' but it lacks execute permissions. Try: chmod +x "${detectionResult.path}"`,
              'Fix Permissions'
            )
            .then(action => {
              if (action === 'Fix Permissions') {
                const terminal = window.createTerminal('Fix Prolog Permissions');
                terminal.sendText(`chmod +x "${detectionResult.path}"`);
                terminal.show();
              }
            });
        }
        this._executable = detectionResult.path; // Use it anyway, will fail with better error
      }
    }
  }

  // Enhanced error handling for SWI-Prolog executable issues
  private async handleExecutableError(error: any): Promise<void> {
    if (error.code === 'ENOENT') {
      const action = await window.showErrorMessage(
        'SWI-Prolog executable not found. The formatter requires SWI-Prolog to format your code properly.',
        'Install SWI-Prolog',
        'Setup Wizard',
        'Configure Path',
        'Dismiss'
      );

      const installationGuide = InstallationGuide.getInstance();
      switch (action) {
        case 'Install SWI-Prolog':
          await installationGuide.showInstallationGuideDialog();
          break;
        case 'Setup Wizard':
          await commands.executeCommand('prolog.setupWizard');
          break;
        case 'Configure Path':
          await commands.executeCommand('workbench.action.openSettings', 'prolog.executablePath');
          break;
        default:
          break;
      }
    } else {
      const message =
        error.message || `Failed to run SWI-Prolog formatter using path: ${this._executable}`;
      window.showErrorMessage(`Prolog formatting failed: ${message}`);
    }
  }
  public provideDocumentRangeFormattingEdits(
    doc: TextDocument,
    range: Range,
    _options: FormattingOptions,
    _token: CancellationToken
  ): ProviderResult<TextEdit[]> {
    const docContent = doc.getText(range); // Get the content of the document;
    const offset = doc.getText(new Range(doc.positionAt(0), range.start)).length;
    let regexp = /%.*|\/\*[\w\W]*?\*\//gm; // Define regular expression for matching comments
    const array = [...docContent.matchAll(regexp)]; // Match all occurrences of comments in the substring
    // Replace comments with placeholder characters in the clause
    let clauseComment = docContent;
    array.forEach(Comment => {
      clauseComment = clauseComment.replace(Comment[0], new Array(Comment[0].length + 1).join('☻'));
    });
    regexp = /^\s*([a-z][a-zA-Z0-9_]*)/gm; // Define a regular expression for identifying Prolog clauses
    const arrayStart = [...clauseComment.matchAll(regexp)]; // Match all occurrences of Prolog clauses in the document
    regexp = /\.\s*$/gm; // Define a regular expression for identifying Prolog clauses end
    const arrayEnd = [...clauseComment.matchAll(regexp)]; // Match all occurrences of Prolog clauses in the document
    let min = 0;
    const clausesArray = [];
    for (let i = 0; i < arrayStart.length; i++) {
      const start = arrayStart[i];
      if (start !== undefined && typeof start.index === 'number' && start.index >= min) {
        for (let j = 0; j < arrayEnd.length; j++) {
          const end = arrayEnd[j];
          if (
            end !== undefined &&
            typeof end.index === 'number' &&
            end.index > start.index
          ) {
            min = end.index;
            clausesArray.push([
              start.index + (typeof offset === 'number' ? offset : 0),
              end.index + (typeof offset === 'number' ? offset : 0),
            ]);
            break;
          }
        }
      }
    }
    let result: TextEdit[] = [];
    // Iterate over each matched clause and format it
    clausesArray.forEach(clause => {
      const clauseArray = this.getClauseString(doc, clause);
      clauseArray[0] = this.formatClause(clauseArray[0]);
      result = result.concat(TextEdit.replace(clauseArray[1], clauseArray[0]));
    });

    return result; // Return the formatted result
  }

  // Implementation of the provideDocumentFormattingEdits method required by DocumentFormattingEditProvider
  public provideDocumentFormattingEdits(
    document: TextDocument,
    _options: FormattingOptions,
    _token: CancellationToken
  ): ProviderResult<TextEdit[]> {
    const docContent = document.getText(); // Get the content of the document
    let regexp = /%.*|\/\*[\w\W]*?\*\//gm; // Define regular expression for matching comments
    const array = [...docContent.matchAll(regexp)]; // Match all occurrences of comments in the substring
    // Replace comments with placeholder characters in the clause
    let clauseComment = docContent;
    array.forEach(Comment => {
      clauseComment = clauseComment.replace(Comment[0], new Array(Comment[0].length + 1).join('☻'));
    });
    regexp = /^\s*([a-z][a-zA-Z0-9_]*)/gm; // Define a regular expression for identifying Prolog clauses
    const arrayStart = [...clauseComment.matchAll(regexp)]; // Match all occurrences of Prolog clauses in the document
    regexp = /\.\s*$/gm; // Define a regular expression for identifying Prolog clauses end
    const arrayEnd = [...clauseComment.matchAll(regexp)]; // Match all occurrences of Prolog clauses in the document
    let min = 0;
    const clausesArray = [];

    for (let i = 0; i < arrayStart.length; i++) {
      const start = arrayStart[i];
      if (start !== undefined && typeof start.index === 'number' && start.index >= min) {
        for (let j = 0; j < arrayEnd.length; j++) {
          const end = arrayEnd[j];
          if (
            end !== undefined &&
            typeof end.index === 'number' &&
            end.index > start.index
          ) {
            min = end.index;
            clausesArray.push([
              start.index,
              end.index,
            ]);
            break;
          }
        }
      }
    }
    let result: TextEdit[] = [];
    // Iterate over each matched clause and format it
    clausesArray.forEach(clause => {
      const clauseArray = this.getClauseString(document, clause);
      clauseArray[0] = this.formatClause(clauseArray[0]);
      result = result.concat(TextEdit.replace(clauseArray[1], clauseArray[0]));
    });

    return result; // Return the formatted result
  }

  // Helper method to get the clause string and its range from the document
  private getClauseString(doc: TextDocument, range: number[]): [string, Range] {
    const docContent = doc.getText();
    if (
      Array.isArray(range) &&
      range.length === 2 &&
      typeof range[0] === 'number' &&
      typeof range[1] === 'number'
    ) {
      let sub = docContent.substring(range[0], range[1] + 1); // Extract the substring from the starting position to the end of the document
      const regexp = /^\s+/gm; // Define regular expression for matching comments
      const array = [...sub.matchAll(regexp)]; // Match all occurrences of starting spaces in the substring
      if (array.length !== 0 && array[0]) {
        sub = sub.slice(array[0][0].length);
        return [
          sub,
          new Range(doc.positionAt(range[0] + array[0][0].length), doc.positionAt(range[1] + 1)),
        ]; // Return the clause string and its range
      }
      return [sub, new Range(doc.positionAt(range[0]), doc.positionAt(range[1] + 1))]; // Return the clause string and its range
    }
    // fallback for invalid range
    return ['', new Range(0, 0, 0, 0)];
  }

  // Helper method to format a Prolog clause
  private formatClause(clause: string): string {
    // COMMENT
    let regexpComment = /%.*|\/\*[\w\W]*?\*\//gm;
    let arrayComment = [...clause.matchAll(regexpComment)];
    let clauseComment = clause;
    // Replace comments with placeholder characters in the clause
    arrayComment.forEach(Comment => {
      clauseComment = clauseComment.replace(Comment[0], new Array(Comment[0].length).join('☻') + '♥');
    });
    // STRING
    let regexpString = /\"((\\\(?:[^"])*\")/gm;
    let arrayString = [...clauseComment.matchAll(regexpString)];
    // Replace strings with placeholder characters in the clause
    arrayString.forEach(String => {
      clauseComment = clauseComment.replace(String[0], new Array(String[0].length + 1).join('☺'));
    });
    //EXTRACT HEAD
    let regexpHead = /^\s*[a-z][a-zA-Z0-9_]*(\\\(?([^.]|\.[^\s])*?(:-|=>|-->)|[^(,\n]*?(\.\s*?$)|\\\(([^.]*|\.[^\s]*?)\\\)\s*\.\s*?)$/gm;
    let arrayHead = [...clauseComment.matchAll(regexpHead)];
    if (arrayHead.length === 0 || !arrayHead[0]) {
      return clause; // Return original clause if no head found
    }
    let headComment = arrayHead[0][0];
    let head = clause.slice(0, (arrayHead[0].index ?? 0) + arrayHead[0][0].length);
    // Remove head from the clause and clauseComment
    clause = clause.slice((arrayHead[0].index ?? 0) + arrayHead[0][0].length);
    clauseComment = clauseComment.replace(headComment, '');
    //CONDENSATE
    let regexp = /(?<!\sis|mod|div|rem|xor|rdiv|in)\s(?!is\s|:-|mod|div|rem|xor|rdiv|in)/gm;
    let array = [...clauseComment.matchAll(regexp)];
    let offset = 0;
    // Remove unnecessary spaces in the clause and clauseComment
    array.forEach(space => {
      clause = [
        clause.slice(0, space.index + offset),
        clause.slice(space.index + space[0].length + offset),
      ].join('');
      clauseComment = [
        clauseComment.slice(0, space.index + offset),
        clauseComment.slice(space.index + space[0].length + offset),
      ].join('');
      offset -= space[0].length;
    });
    array = [...headComment.matchAll(regexp)];
    offset = 0;
    array.forEach(space => {
      head = [
        head.slice(0, space.index + offset),
        head.slice(space.index + space[0].length + offset),
      ].join('');
      headComment = [
        headComment.slice(0, space.index + offset),
        headComment.slice(space.index + space[0].length + offset),
      ].join('');
      offset -= space[0].length;
    });

    //OPERATOR
    let regexpOperator = /(?<=[\]\)\}])ins|(?<=[\]\)\}])in|[-*]?->|=>|\?-|:-|=?:=|\\\+|(?:<|=|@|@=||:|>:)<|(?:\\?)(?<![><#])=(?:\.\.|@=|=|\\=|)|@?>(?:=|>|)|:|\+|-|\\\/|\/\\|#=|#>|#\\=|#<==>|#/gm;
    let arrayOperator = [...clauseComment.matchAll(regexpOperator)];
    offset = 0;
    // Add spaces around operators in the clause and clauseComment
    arrayOperator.forEach(operator => {
      clause = [
        clause.slice(0, operator.index + offset),
        ' ' + operator[0] + ' ',
        clause.slice(operator.index + operator[0].length + offset),
      ].join('');
      clauseComment = [
        clauseComment.slice(0, operator.index + offset),
        ' ' + operator[0] + ' ',
        clauseComment.slice(operator.index + operator[0].length + offset),
      ].join('');
      offset += 2;
    });
    // Special case for '->'
    regexp = /^(\s*).*->\s/gm;
    array = [...clauseComment.matchAll(regexp)];
    offset = 0;
    // Split lines with '->' and add indentation
    array.forEach(l => {
      regexp = /->\s/gm;
      const array2 = [...l[0].matchAll(regexp)];
      array2.forEach(f => {
        const indentLength = l[1]?.length ?? 0;
        clause = [
          clause.slice(0, (l.index ?? 0) + (f.index ?? 0) + offset),
          '->\n' + new Array(indentLength + 2).join('\t'),
          clause.slice((l.index ?? 0) + (f.index ?? 0) + f[0].length + offset),
        ].join('');
        clauseComment = [
          clauseComment.slice(0, (l.index ?? 0) + (f.index ?? 0) + offset),
          '->\n' + new Array(indentLength + 2).join('\t'),
          clauseComment.slice((l.index ?? 0) + (f.index ?? 0) + f[0].length + offset),
        ].join('');
        offset += indentLength + 1;
      });
    });
    // Special case for ';'
    regexp = /^(\s*).*;(?=\S)/gm;
    array = [...clauseComment.matchAll(regexp)];
    offset = 0;
    array.forEach(l => {
      regexp = /;/gm;
      const array2 = [...l[0].matchAll(regexp)];
      array2.forEach(f => {
        const indentLength = l[1]?.length ?? 0;
        clause = [
          clause.slice(0, (l.index ?? 0) + (f.index ?? 0) + offset),
          ';\n' + new Array(indentLength + 1).join('\t'),
          clause.slice((l.index ?? 0) + (f.index ?? 0) + f[0].length + offset),
        ].join('');
        clauseComment = [
          clauseComment.slice(0, (l.index ?? 0) + (f.index ?? 0) + offset),
          ';\n' + new Array(indentLength + 1).join('\t'),
          clauseComment.slice((l.index ?? 0) + (f.index ?? 0) + f[0].length + offset),
        ].join('');
        offset += indentLength + 1;
      });
    });
    //NESTED
    const result = this.formatNested(clause, clauseComment);
    clause = result[0];
    clauseComment = result[1];
    //COMMAS
    const formatConfig = this._section.get('format') as { addSpace?: boolean } | undefined;
    if (formatConfig?.addSpace) {
      regexp = /,(?!\s)/gm;
      array = [...clauseComment.matchAll(regexp)];
      offset = 0;
      // Add space after commas in the clause and clauseComment
      array.forEach(comma => {
        clause = [
          clause.slice(0, comma.index + offset),
          comma[0] + ' ',
          clause.slice(comma.index + comma[0].length + offset),
        ].join('');
        clauseComment = [
          clauseComment.slice(0, comma.index + offset),
          comma[0] + ' ',
          clauseComment.slice(comma.index + comma[0].length + offset),
        ].join('');
        offset += 1;
      });

      array = [...headComment.matchAll(regexp)];
      offset = 0;
      array.forEach(comma => {
        head = [
          head.slice(0, comma.index + offset),
          comma[0] + ' ',
          head.slice(comma.index + comma[0].length + offset),
        ].join('');
        headComment = [
          headComment.slice(0, comma.index + offset),
          comma[0] + ' ',
          headComment.slice(comma.index + comma[0].length + offset),
        ].join('');
        offset += 1;
      });
    }
    //REPLACE COMMENT
    regexp = /^(\s*).*(♥)/gm;
    array = [...clauseComment.matchAll(regexp)];
    let offset2 = 0;
    array.forEach(Comment => {
      regexp = /♥/gm;
      const array2 = [...Comment[0].matchAll(regexp)];
      // Add newline and indentation after comments in the clause
      array2.forEach(h => {
        clause = [
          clause.slice(0, (Comment.index ?? 0) + (h.index ?? 0) + 1 + offset2),
          '\n' + new Array((Comment[1]?.length ?? 0) + 1).join('\t'),
          clause.slice((Comment.index ?? 0) + (h.index ?? 0) + 1 + offset2),
        ].join('');
        offset2 += 1 + (Comment[1]?.length ?? 0);
      });
    });

    array = [...headComment.matchAll(regexp)];
    let offset3 = 0;
    array.forEach(Comment => {
      regexp = /♥/gm;
      const array2 = [...Comment[0].matchAll(regexp)];
      // Add newline and indentation after comments in the clause
      array2.forEach(h => {
        head = [
          head.slice(0, (Comment.index ?? 0) + (h.index ?? 0) + 1 + offset3),
          '\n' + new Array((Comment[1]?.length ?? 0) + 1).join(' '),
          head.slice((Comment.index ?? 0) + (h.index ?? 0) + 1 + offset3),
        ].join('');
        offset3 += 1 + (Comment[1]?.length ?? 0);
      });
    });
    if (clause !== '') {
      head = head + '\n\t';
      return head + clause; // Return the formatted clause
    } else {
      return head; // Return the formatted clause
    }
  }

  // Helper method to format nested expressions within a Prolog clause
  private formatNested(clause: string, clauseComment: string): [string, string] {
    let regexpNested = new RegExp('\\[[^\\[\\]]*?\\]|\\([^()]*?\\)|{\\|[\\w\\W]*?\\|}', 'gm'); // Define regular expression to find 0 deep expressions
    const array0deep = [...clauseComment.matchAll(regexpNested)]; // Find all occurrences of 0 deep expressions
    regexpNested = new RegExp('.(?=},?)|,|{|\\[', 'gm'); // Define regular expression to find 0 deep expressions
    const endLine = [...clauseComment.matchAll(regexpNested)]; // Find all end line

    let deep = 1;
    let offset = 0;
    const arrayDeep = [];

    // Deepness calculation per index
    for (let i = 0; i < clauseComment.length; i++) {
      const char = clauseComment[i];
      if (char && ['{', '[', '('].includes(char)) {
        deep = deep + 1;
      }
      if (char && ['}', ']', ')'].includes(char)) {
        deep = deep - 1;
      }
      arrayDeep.push(deep);
    }
    // For each line end verify if it's inside a 0 deep element
    for (let i = 0; i < endLine.length; i++) {
      let verif = true;
      for (let j = 0; j < array0deep.length; j++) {
        const endLineIndex = endLine[i]?.index ?? 0;
        const endLineLength = endLine[i]?.[0]?.length ?? 0;
        const array0deepIndex = array0deep[j]?.index ?? 0;
        const array0deepLength = array0deep[j]?.[0]?.length ?? 0;

        if (
          endLineIndex >= array0deepIndex &&
          endLineIndex + endLineLength < array0deepIndex + array0deepLength
        ) {
          verif = false;
        }
      }
      // If verif add line breaks and indentation depending of the deepness
      if (verif) {
        const endLineIndex = endLine[i]?.index ?? 0;
        const endLineLength = endLine[i]?.[0]?.length ?? 0;
        deep = arrayDeep[endLineIndex] ?? 1;

        clauseComment = [
          clauseComment.slice(0, endLineIndex + endLineLength + offset),
          '\n' + new Array(deep + 1).join('\t'),
          clauseComment.slice(endLineIndex + endLineLength + offset),
        ].join('');
        clause = [
          clause.slice(0, endLineIndex + endLineLength + offset),
          '\n' + new Array(deep + 1).join('\t'),
          clause.slice(endLineIndex + endLineLength + offset),
        ].join('');

        offset += 1 + deep;
      }
    }
    return [clause, clauseComment]; // Return the formatted clause and clauseComment
  }
}
