

import { off } from 'process';
import {
  languages,
  TextEdit,
  DocumentFormattingEditProvider,
  DocumentRangeFormattingEditProvider,
  TextDocument,
  FormattingOptions,
  Range,
  CancellationToken,
  ProviderResult,
  Position,
  workspace,
  WorkspaceConfiguration,
  OutputChannel,
  window,
  commands
}
  from 'vscode';
import { InstallationGuide } from "./installationGuide";
import { PlatformUtils } from "../utils/platformUtils";
import { ExecutableFinder } from "../utils/executableFinder";

export class PrologFormatter implements
  DocumentRangeFormattingEditProvider,
  DocumentFormattingEditProvider {
  private _section: WorkspaceConfiguration;
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
    this._section = workspace.getConfiguration("prolog");
    let execPath = this._section.get("executablePath", PlatformUtils.getDefaultExecutablePath());
    this._executable = PlatformUtils.normalizePath(execPath);
    this._args = [];
    this._outputChannel = window.createOutputChannel("PrologFormatter");
    
    // Initialize with better executable resolution
    this.initializeExecutable();
  }

  /**
   * Initialize executable with enhanced detection and permission checking
   */
  private async initializeExecutable(): Promise<void> {
    const configuredPath = this._section.get<string>("executablePath", PlatformUtils.getDefaultExecutablePath());
    
    // Check if configured path exists and is executable
    if (await PlatformUtils.pathExists(configuredPath)) {
      if (await PlatformUtils.isExecutable(configuredPath)) {
        this._executable = PlatformUtils.normalizePath(configuredPath);
        return;
      } else {
        // Path exists but lacks execute permissions
        const platform = PlatformUtils.getPlatform();
        if (platform !== 'windows') {
          window.showWarningMessage(
            `SWI-Prolog executable at '${configuredPath}' lacks execute permissions. Try: chmod +x "${configuredPath}"`,
            'Fix Permissions'
          ).then((action) => {
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
          window.showInformationMessage(
            `Found SWI-Prolog at '${detectionResult.path}' via ${detectionResult.detectionMethod}. Update configuration?`,
            'Yes', 'No'
          ).then((action) => {
            if (action === 'Yes') {
              this._section.update('executablePath', detectionResult.path, true);
            }
          });
        }
      } else {
        // Found executable but has permission issues
        const platform = PlatformUtils.getPlatform();
        if (platform !== 'windows') {
          window.showWarningMessage(
            `Found SWI-Prolog at '${detectionResult.path}' but it lacks execute permissions. Try: chmod +x "${detectionResult.path}"`,
            'Fix Permissions'
          ).then((action) => {
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
    if (error.code === "ENOENT") {
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
      }
    } else {
      const message = error.message || `Failed to run SWI-Prolog formatter using path: ${this._executable}`;
      window.showErrorMessage(`Prolog formatting failed: ${message}`);
    }
  }
  public provideDocumentRangeFormattingEdits(
    doc: TextDocument,
    range: Range,
    options: FormattingOptions,
    token: CancellationToken
  ): ProviderResult<TextEdit[]> {
    let docContent = doc.getText(range); // Get the content of the document;
    var offset = doc.getText(new Range(doc.positionAt(0),range.start)).length;
    var regexp = /%.*|\/\*[\w\W]*?\*\//gm; // Define regular expression for matching comments
    var array = [...docContent.matchAll(regexp)]; // Match all occurrences of comments in the substring
    // Replace comments with placeholder characters in the clause
    var clauseComment = docContent;
    array.forEach(Comment => {
      clauseComment = clauseComment.replace(Comment[0], new Array(Comment[0].length + 1).join("☻"))
    });
    regexp = /^\s*([a-z][a-zA-Z0-9_]*)/gm;// Define a regular expression for identifying Prolog clauses
    var arrayStart = [...clauseComment.matchAll(regexp)];// Match all occurrences of Prolog clauses in the document
    regexp = /\.\s*$/gm; // Define a regular expression for identifying Prolog clauses end
    var arrayEnd = [...clauseComment.matchAll(regexp)];// Match all occurrences of Prolog clauses in the document
    var min = 0;
    var clausesArray = [];
    for (let i = 0; i < arrayStart.length; i++) {
      if (arrayStart[i].index !== undefined && arrayStart[i].index >= min) {
        for (let j = 0; j < arrayEnd.length; j++) {
          if (arrayEnd[j].index !== undefined && arrayEnd[j].index > arrayStart[i].index) {
            min = arrayEnd[j].index;
            clausesArray.push([arrayStart[i].index+offset, arrayEnd[j].index+offset]);
            break;
          }
        }
      }
    }
    var result: TextEdit[] = []
    // Iterate over each matched clause and format it
    clausesArray.forEach((clause) => {
      var clauseArray = this.getClauseString(doc, clause);
      clauseArray[0] = this.formatClause(clauseArray[0]);
      result = result.concat(TextEdit.replace(clauseArray[1], clauseArray[0]));
    })

    return result;// Return the formatted result
  }

  // Implementation of the provideDocumentFormattingEdits method required by DocumentFormattingEditProvider
  public provideDocumentFormattingEdits(document: TextDocument, _options: FormattingOptions, _token: CancellationToken): ProviderResult<TextEdit[]> {
    let docContent = document.getText(); // Get the content of the document
    var regexp = /%.*|\/\*[\w\W]*?\*\//gm; // Define regular expression for matching comments
    var array = [...docContent.matchAll(regexp)]; // Match all occurrences of comments in the substring
    // Replace comments with placeholder characters in the clause
    var clauseComment = docContent;
    array.forEach(Comment => {
      clauseComment = clauseComment.replace(Comment[0], new Array(Comment[0].length + 1).join("☻"))
    });
    regexp = /^\s*([a-z][a-zA-Z0-9_]*)/gm;// Define a regular expression for identifying Prolog clauses
    var arrayStart = [...clauseComment.matchAll(regexp)];// Match all occurrences of Prolog clauses in the document
    regexp = /\.\s*$/gm; // Define a regular expression for identifying Prolog clauses end
    var arrayEnd = [...clauseComment.matchAll(regexp)];// Match all occurrences of Prolog clauses in the document
    var min = 0;
    var clausesArray = [];

    for (let i = 0; i < arrayStart.length; i++) {
      if (arrayStart[i].index !== undefined && arrayStart[i].index >= min) {
        for (let j = 0; j < arrayEnd.length; j++) {
          if (arrayEnd[j].index !== undefined && arrayEnd[j].index > arrayStart[i].index) {
            min = arrayEnd[j].index;
            clausesArray.push([arrayStart[i].index, arrayEnd[j].index]);
            break;
          }
        }
      }
    }
    var result: TextEdit[] = []
    // Iterate over each matched clause and format it
    clausesArray.forEach((clause) => {
      var clauseArray = this.getClauseString(document, clause);
      clauseArray[0] = this.formatClause(clauseArray[0]);
      result = result.concat(TextEdit.replace(clauseArray[1], clauseArray[0]));
    })

    return result;// Return the formatted result
  }

  // Helper method to get the clause string and its range from the document
  private getClauseString(doc: TextDocument, range: number[]): [string, Range] {
    let docContent = doc.getText();
    var sub = docContent.substring(range[0], range[1] + 1); // Extract the substring from the starting position to the end of the document
    var regexp = /^\s+/gm; // Define regular expression for matching comments
    var array = [...sub.matchAll(regexp)]; // Match all occurrences of starting spaces in the substring
    if (array.length != 0 && array[0]) {
      sub = sub.slice(array[0][0].length);
      return [sub, new Range(doc.positionAt(range[0] + array[0][0].length), doc.positionAt(range[1] + 1))];// Return the clause string and its range
    }

    return [sub, new Range(doc.positionAt(range[0]), doc.positionAt(range[1] + 1))];// Return the clause string and its range
  }

  // Helper method to format a Prolog clause
  private formatClause(clause: string): string {
    // COMMENT
    var regexp = /%.*|\/\*[\w\W]*?\*\//gm;
    var array = [...clause.matchAll(regexp)];
    var clauseComment = clause;
    // Replace comments with placeholder characters in the clause
    array.forEach(Comment => {
      clauseComment = clauseComment.replace(Comment[0], new Array(Comment[0].length).join("☻") + "♥")
    });
    // STRING
    var regexp = /(\")((?:[^\"])*)(\")|(\')((?:[^\'])*)(\')/gm;
    var array = [...clauseComment.matchAll(regexp)];
    // Replace strings with placeholder characters in the clause
    array.forEach(String => {
      clauseComment = clauseComment.replace(String[0], new Array(String[0].length + 1).join("☺"))
    });
    //EXTRACT HEAD
    regexp = /^\s*[a-z][a-zA-Z0-9_]*(\(?([^.]|\.[^\s])*?(:-|=>|-->)|[^\(,\n]*?(\.\s*?$)|\(([^.]*|\.[^\s]*?)\)\s*\.\s*?$)/gm;
    array = [...clauseComment.matchAll(regexp)];
    var headComment = array[0][0];
    var head = clause.slice(0, array[0].index + array[0][0].length)
    // Remove head from the clause and clauseComment
    clause = clause.slice(array[0].index + array[0][0].length)
    clauseComment = clauseComment.replace(headComment, "")
    //CONDENSATE
    regexp = /(?<!\sis|mod|div|rem|xor|rdiv|in)\s(?!is\s|:-|mod|div|rem|xor|rdiv|in)/gm;
    array = [...clauseComment.matchAll(regexp)];
    offset = 0
    // Remove unnecessary spaces in the clause and clauseComment
    array.forEach(space => {
      clause = [clause.slice(0, space.index + offset), clause.slice(space.index + space[0].length + offset)].join('');
      clauseComment = [clauseComment.slice(0, space.index + offset), clauseComment.slice(space.index + space[0].length + offset)].join('');
      offset -= space[0].length;
    });
    array = [...headComment.matchAll(regexp)];
    offset = 0
    array.forEach(space => {
      head = [head.slice(0, space.index + offset), head.slice(space.index + space[0].length + offset)].join('');
      headComment = [headComment.slice(0, space.index + offset), headComment.slice(space.index + space[0].length + offset)].join('');
      offset -= space[0].length;
    });

    //OPERATOR
    regexp = /(?<=[\]\)}])ins|(?<=[]\)}])in|[-*]?->|=>|\?-|:-|=?:=|\\\+|(?:<|=|@|@=||:|>:)<|(?:\\?)(?<![><#])=(?:\.\.|@=|=|\\=|)|@?>(?:=|>|)|:|\+|-|\\\/|\/\\|#=|#>|#\\=|#<==>|#</gm;
    array = [...clauseComment.matchAll(regexp)];
    offset = 0
    // Add spaces around operators in the clause and clauseComment
    array.forEach(operator => {
      clause = [clause.slice(0, operator.index + offset), " " + operator[0] + " ", clause.slice(operator.index + operator[0].length + offset)].join('');
      clauseComment = [clauseComment.slice(0, operator.index + offset), " " + operator[0] + " ", clauseComment.slice(operator.index + operator[0].length + offset)].join('');
      offset += 2;
    });
    // Special case for '->'
    regexp = /^(\s*).*->\s/gm;
    array = [...clauseComment.matchAll(regexp)];
    offset = 0
    // Split lines with '->' and add indentation
    array.forEach(l => {
      regexp = /->\s/gm;
      const array2 = [...l[0].matchAll(regexp)];
      array2.forEach(f => {
        clause = [clause.slice(0, l.index + f.index + offset), "->\n" + new Array(l[1].length + 2).join("\t"), clause.slice(l.index + f.index + f[0].length + offset)].join('');
        clauseComment = [clauseComment.slice(0, l.index + f.index + offset), "->\n" + new Array(l[1].length + 2).join("\t"), clauseComment.slice(l.index + f.index + f[0].length + offset)].join('');
        offset += l[1].length + 1;
      });
    });
    // Special case for ';'
    regexp = /^(\s*).*;(?=\S)/gm;
    array = [...clauseComment.matchAll(regexp)];
    offset = 0
    array.forEach(l => {
      regexp = /;/gm;
      const array2 = [...l[0].matchAll(regexp)];
      array2.forEach(f => {
        clause = [clause.slice(0, l.index + f.index + offset), ";\n" + new Array(l[1].length + 1).join("\t"), clause.slice(l.index + f.index + f[0].length + offset)].join('');
        clauseComment = [clauseComment.slice(0, l.index + f.index + offset), ";\n" + new Array(l[1].length + 1).join("\t"), clauseComment.slice(l.index + f.index + f[0].length + offset)].join('');
        offset += l[1].length + 1;
      });
    });
    //NESTED
    var result = this.formatNested(clause, clauseComment);
    clause = result[0];
    clauseComment = result[1];
    //COMMAS
    if (this._section.format.addSpace) {
      regexp = /,(?!\s)/gm;
      array = [...clauseComment.matchAll(regexp)];
      offset = 0
      // Add space after commas in the clause and clauseComment
      array.forEach(comma => {
        clause = [clause.slice(0, comma.index + offset), comma[0] + " ", clause.slice(comma.index + comma[0].length + offset)].join('');
        clauseComment = [clauseComment.slice(0, comma.index + offset), comma[0] + " ", clauseComment.slice(comma.index + comma[0].length + offset)].join('');
        offset += 1;
      });

      array = [...headComment.matchAll(regexp)];
      offset = 0
      array.forEach(comma => {
        head = [head.slice(0, comma.index + offset), comma[0] + " ", head.slice(comma.index + comma[0].length + offset)].join('');
        headComment = [headComment.slice(0, comma.index + offset), comma[0] + " ", headComment.slice(comma.index + comma[0].length + offset)].join('');
        offset += 1;
      });
    }
    //REPLACE COMMENT
    regexp = /^(\s*).*(♥)/gm;
    array = [...clauseComment.matchAll(regexp)];
    var offset = 0;
    array.forEach(Comment => {
      regexp = /♥/gm;
      const array2 = [...Comment[0].matchAll(regexp)];
      // Add newline and indentation after comments in the clause
      array2.forEach(h => {
        clause = [clause.slice(0, Comment.index + h.index + 1 + offset), "\n" + new Array(Comment[1].length + 1).join("\t"), clause.slice(Comment.index + h.index + 1 + offset)].join('');
        offset += 1 + Comment[1].length;
      });
    });

    array = [...headComment.matchAll(regexp)];
    var offset = 0;
    array.forEach(Comment => {
      regexp = /♥/gm;
      const array2 = [...Comment[0].matchAll(regexp)];
      // Add newline and indentation after comments in the clause
      array2.forEach(h => {
        head = [head.slice(0, Comment.index + h.index + 1 + offset), "\n" + new Array(Comment[1].length + 1).join(" "), head.slice(Comment.index + h.index + 1 + offset)].join('');
        offset += 1 + Comment[1].length;
      });
    });
    if (clause != "") {
      head = head + "\n\t";
      return head + clause; // Return the formatted clause
    } else {
      return head; // Return the formatted clause
    }

  }

  // Helper method to format nested expressions within a Prolog clause
  private formatNested(clause: string, clauseComment: string): [string, string] {
    var regexp = new RegExp("\\[[^\\[\\]]*?\\]|\\([^\\(]*?\\)|{\\|[\\w\\W]*?\\|}", "gm");// Define regular expression to find 0 deep expressions
    var array0deep = [...clauseComment.matchAll(regexp)];// Find all occurrences of 0 deep expressions 
    regexp = new RegExp(".(?=},?)|,|{|\\[", "gm");// Define regular expression to find 0 deep expressions
    var endLine = [...clauseComment.matchAll(regexp)];// Find all end line

    var deep = 1;
    var offset = 0;
    var arrayDeep = [];

    // Deepness calculation per index
    for (let i = 0; i < clauseComment.length; i++) {
      if (['{', '[', '('].includes(clauseComment[i])) {
        deep = deep + 1;
      }
      if (['}', ']', ')'].includes(clauseComment[i])) {
        deep = deep - 1;
      }
      arrayDeep.push(deep);
    }
    // For each line end verify if it's inside a 0 deep element
    for (let i = 0; i < endLine.length; i++) {
      var verif = true;
      for (let j = 0; j < array0deep.length; j++) {
        if (endLine[i].index >= array0deep[j].index && endLine[i].index + endLine[i][0].length < array0deep[j].index + array0deep[j][0].length) {
          verif = false;
        }
      }
      // If verif add line breaks and indentation depending of the deepness
      if (verif) {
        deep = arrayDeep[endLine[i].index];

        clauseComment = [clauseComment.slice(0, endLine[i].index + endLine[i][0].length + offset), "\n" + new Array(deep + 1).join("\t"), clauseComment.slice(endLine[i].index + endLine[i][0].length + offset)].join('');
        clause = [clause.slice(0, endLine[i].index + endLine[i][0].length + offset), "\n" + new Array(deep + 1).join("\t"), clause.slice(endLine[i].index + endLine[i][0].length + offset)].join('');

        offset += 1 + deep;
      }
    }
    return [clause, clauseComment]// Return the formatted clause and clauseComment
  }

}