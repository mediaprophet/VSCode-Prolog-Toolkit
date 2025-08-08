import fg from 'fast-glob';
import { Diagnostic, DiagnosticSeverity, Position, Range, TextDocument, workspace } from 'vscode';
import { PrologExecUtils } from '../../utils/utils';
import { IDiagnosticInfo, IDiagnosticParser } from './interfaces';

/**
 * Handles parsing of Prolog compiler output into diagnostic information
 */
export class DiagnosticParser implements IDiagnosticParser {
  // Regular expression to parse SWI-Prolog errors and warnings
  private readonly swiRegex = /([^:]+?):\s*(.+?):(\d+):((\d+):)?((\d+):)?\s*([\s\S]*)/;

  /**
   * Parse a single issue string from linter output
   */
  public parseIssue(issue: string, filePathIds: { [id: string]: string }): IDiagnosticInfo | null {
    // Use regular expression to match different components of the issue
    const match = issue.match(this.swiRegex);

    // Check if a match is found
    if (match == null) return null;

    // Extract relevant information from the match
    const fileName = filePathIds[match[2]] ? filePathIds[match[2]] : match[2];
    let severity: DiagnosticSeverity;

    // Determine severity based on the issue type
    if (match[1] === 'ERROR') {
      severity = DiagnosticSeverity.Error;
    } else if (match[1] === 'Warning') {
      severity = DiagnosticSeverity.Warning;
    } else {
      severity = DiagnosticSeverity.Information;
    }

    // Parse line, column, and error message information
    const line = parseInt(match[3]) - 1; // Convert to 0-based indexing
    let fromCol = match[5] ? parseInt(match[5]) : 0;
    fromCol = fromCol < 0 ? 0 : fromCol;
    const toCol = match[7] ? parseInt(match[7]) : 200;
    const message = match[8];

    return {
      fileName,
      severity,
      line,
      fromCol,
      toCol,
      message,
    };
  }

  /**
   * Parse Prolog output and extract diagnostic information
   */
  public parsePrologOutput(
    output: string,
    errorOutput: string,
    textDocument: TextDocument
  ): IDiagnosticInfo[] {
    const diagnostics: IDiagnosticInfo[] = [];
    const filePathIds: { [id: string]: string } = {};
    let lineErr = '';

    // Process standard output
    if (output) {
      this.processStdout(output, textDocument, diagnostics, filePathIds, lineErr);
    }

    // Process standard error
    if (errorOutput) {
      this.processStderr(errorOutput, textDocument, diagnostics, filePathIds, lineErr);
    }

    return diagnostics;
  }

  /**
   * Process standard output for different dialects
   */
  private processStdout(
    output: string,
    textDocument: TextDocument,
    diagnostics: IDiagnosticInfo[],
    filePathIds: { [id: string]: string },
    lineErr: string
  ): void {
    if (PrologExecUtils.DIALECT === 'ecl' && !/checking completed/.test(output)) {
      const lines = output.split('\n');

      for (const line of lines) {
        if (/^File\s*/.test(line)) {
          if (lineErr) {
            const diagnostic = this.parseIssue(lineErr + '\n', filePathIds);
            if (diagnostic) diagnostics.push(diagnostic);
            lineErr = '';
          }

          const match = line.match(/File\s*([^,]+),.*line\s*(\d+):\s*(.*)/);
          if (match) {
            let fullName: string;
            if (match[1] === 'string') {
              fullName = textDocument.fileName;
            } else {
              // Use fast-glob to find the file matching the pattern
              const files = fg.sync([`**/${match[1]}`], {
                cwd: workspace.workspaceFolders?.[0]?.uri.fsPath,
                absolute: true,
              });
              fullName = files.length > 0 ? files[0] : match[1];
            }
            lineErr = 'Warning:' + fullName + ':' + match[2] + ':' + match[3];
          }
        } else if (/^\|/.test(line)) {
          lineErr += line;
        }
      }
    }
  }

  /**
   * Process standard error for different dialects
   */
  private processStderr(
    errorOutput: string,
    textDocument: TextDocument,
    diagnostics: IDiagnosticInfo[],
    filePathIds: { [id: string]: string },
    lineErr: string
  ): void {
    const lines = errorOutput.split('\n');

    switch (PrologExecUtils.DIALECT) {
      case 'swi':
        this.processSwiStderr(lines, diagnostics, filePathIds, lineErr);
        break;
      case 'ecl':
        this.processEclStderr(lines, textDocument, diagnostics, filePathIds, lineErr);
        break;
      default:
        break;
    }
  }

  /**
   * Process SWI-Prolog stderr output
   */
  private processSwiStderr(
    lines: string[],
    diagnostics: IDiagnosticInfo[],
    filePathIds: { [id: string]: string },
    lineErr: string
  ): void {
    for (const line of lines) {
      if (/which is referenced by/.test(line)) {
        const regex = /Warning:\s*(.+),/;
        const match = line.match(regex);
        if (match) {
          lineErr = ' Predicate ' + match[1] + ' not defined';
        }
      } else if (/clause of /.test(line)) {
        const regex = /^(Warning:\s*(.+?):)(\d+):(\d+)?/;
        const match = line.match(regex);
        if (match) {
          const lineNum = parseInt(match[3]);
          const char = match[4] ? parseInt(match[4]) : 0;
          const rangeStr = lineNum + ':' + char + ':200: ';
          const lineMsg = match[1] + rangeStr + lineErr;
          const diagnostic = this.parseIssue(lineMsg + '\n', filePathIds);
          if (diagnostic) diagnostics.push(diagnostic);
        }
      } else if (/:\s*$/.test(line)) {
        lineErr = line;
      } else {
        if (line.startsWith('ERROR') || line.startsWith('Warning')) {
          lineErr = line;
        } else {
          lineErr = lineErr.concat(line);
        }
        const diagnostic = this.parseIssue(lineErr + '\n', filePathIds);
        if (diagnostic) diagnostics.push(diagnostic);
        lineErr = '';
      }
    }
  }

  /**
   * Process ECLiPSe stderr output
   */
  private processEclStderr(
    lines: string[],
    textDocument: TextDocument,
    diagnostics: IDiagnosticInfo[],
    filePathIds: { [id: string]: string },
    lineErr: string
  ): void {
    for (const line of lines) {
      if (/^[fF]ile|^string stream|^Stream/.test(line)) {
        if (lineErr !== '') {
          const diagnostic = this.parseIssue(lineErr + '\n', filePathIds);
          if (diagnostic) diagnostics.push(diagnostic);
          lineErr = '';
        }

        let fullName: string, lineNum: string, msg: string;
        let match = line.match(/[fF]ile\s*([^,]+),\s*line\s*(\d+):\s*(.*)/);

        if (match) {
          // Use fast-glob to find the file matching the pattern
          const files = fg.sync([`**/${match[1]}`], {
            cwd: workspace.workspaceFolders?.[0]?.uri.fsPath,
            absolute: true,
          });
          fullName = files.length > 0 ? files[0] : match[1];
          lineNum = match[2];
          msg = match[3];
        } else {
          fullName = textDocument.fileName;
          match = line.match(/line\s*(\d+):\s*(.*)/);
          if (!match) {
            match = line.match(/:(\d+):\s*(.*)/);
          }
          if (match) {
            lineNum = match[1];
            msg = match[2];
          } else {
            continue;
          }
        }

        const msgType = /error:|[sS]tream/.test(lineErr) ? 'ERROR:' : 'WARNING:';
        lineErr = msgType + fullName + ':' + lineNum + ':' + msg;
      } else if (!/^\s*$/.test(line)) {
        lineErr += '\n' + line;
      }
    }

    // Handle any remaining error
    if (lineErr !== '') {
      const diagnostic = this.parseIssue(lineErr + '\n', filePathIds);
      if (diagnostic) diagnostics.push(diagnostic);
    }
  }

  /**
   * Convert IDiagnosticInfo to VS Code Diagnostic
   */
  public convertToDiagnostic(info: IDiagnosticInfo): Diagnostic {
    const fromPos = new Position(info.line, info.fromCol);
    const toPos = new Position(info.line, info.toCol);
    const range = new Range(fromPos, toPos);
    return new Diagnostic(range, info.message, info.severity);
  }

  /**
   * Group diagnostics by file name
   */
  public groupDiagnosticsByFile(diagnostics: IDiagnosticInfo[]): { [fileName: string]: IDiagnosticInfo[] } {
    const grouped: { [fileName: string]: IDiagnosticInfo[] } = {};

    for (const diagnostic of diagnostics) {
      if (!grouped[diagnostic.fileName]) {
        grouped[diagnostic.fileName] = [];
      }
      grouped[diagnostic.fileName].push(diagnostic);
    }

    return grouped;
  }
}