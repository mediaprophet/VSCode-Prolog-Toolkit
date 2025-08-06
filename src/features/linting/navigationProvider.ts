import type {
  DiagnosticCollection,
  OutputChannel,
  Position,
} from 'vscode';
import {
  Diagnostic,
  DiagnosticSeverity,
  Selection,
  TextEditorRevealType,
  window,
} from 'vscode';
import * as path from 'path';
import type { INavigationProvider } from './interfaces.js';

/**
 * Provides navigation functionality for linting errors and warnings
 */
export class NavigationProvider implements INavigationProvider {
  private diagnosticCollection: DiagnosticCollection;
  private sortedDiagIndex: { [docName: string]: number[] } = {};
  private outputChannel: OutputChannel;

  constructor(diagnosticCollection: DiagnosticCollection, outputChannel: OutputChannel) {
    this.diagnosticCollection = diagnosticCollection;
    this.outputChannel = outputChannel;
  }

  /**
   * Navigate to the next error or warning line
   */
  public gotoNextError(): void {
    this.gotoErrorLine(0);
  }

  /**
   * Navigate to the previous error or warning line
   */
  public gotoPrevError(): void {
    this.gotoErrorLine(1);
  }

  /**
   * Navigate to the error or warning line based on direction
   * @param direction 0: next, 1: previous
   */
  public gotoErrorLine(direction: number): void {
    const editor = window.activeTextEditor;
    if (!editor) {
      this.showMessage('No active editor found');
      return;
    }

    const diagnostics = this.diagnosticCollection.get(editor.document.uri);

    // If there are no diagnostics, display a message and return
    if (!diagnostics || diagnostics.length === 0) {
      this.showMessage('No errors or warnings :)');
      return;
    }

    this.outputChannel.clear();
    const activeLine = editor.selection.active.line;
    let position: Position | undefined = undefined;
    let selectedIndex: number;
    const sortedIndices = this.sortedDiagIndex[editor.document.uri.fsPath];

    if (!sortedIndices || sortedIndices.length === 0) {
      this.showMessage('No diagnostic index found for this document');
      return;
    }

    // Determine the navigation direction (next or previous)
    if (direction === 0) {
      // Next error
      selectedIndex = 0;
      // If the active line is greater than or equal to the last diagnostic's start line,
      // navigate to the start of the first diagnostic; otherwise, find the next diagnostic
      const lastIndex = sortedIndices[sortedIndices.length - 1];
      if (lastIndex !== undefined && diagnostics[lastIndex] && activeLine >= diagnostics[lastIndex].range.start.line) {
        const firstIndex = sortedIndices[0];
        if (firstIndex !== undefined && diagnostics[firstIndex]) {
          position = diagnostics[firstIndex].range.start;
          selectedIndex = 0;
        }
      } else {
        // Find the next diagnostic based on the active line
        while (selectedIndex < sortedIndices.length) {
          const currentIndex = sortedIndices[selectedIndex];
          if (currentIndex === undefined || !diagnostics[currentIndex] || diagnostics[currentIndex].range.start.line > activeLine) {
            break;
          }
          selectedIndex++;
        }
        if (selectedIndex >= sortedIndices.length) {
          selectedIndex = 0;
        }
        const targetIndex = sortedIndices[selectedIndex];
        if (targetIndex !== undefined && diagnostics[targetIndex]) {
          position = diagnostics[targetIndex].range.start;
        }
      }
    } else {
      // Previous error
      selectedIndex = sortedIndices.length - 1;
      // If the active line is less than or equal to the first diagnostic's start line,
      // navigate to the start of the last diagnostic; otherwise, find the previous diagnostic
      const firstIndex = sortedIndices[0];
      if (firstIndex !== undefined && diagnostics[firstIndex] && activeLine <= diagnostics[firstIndex].range.start.line) {
        const lastIndex = sortedIndices[selectedIndex];
        if (lastIndex !== undefined && diagnostics[lastIndex]) {
          position = diagnostics[lastIndex].range.start;
        }
      } else {
        // Find the previous diagnostic based on the active line
        while (selectedIndex >= 0) {
          const currentIndex = sortedIndices[selectedIndex];
          if (currentIndex === undefined || !diagnostics[currentIndex] || diagnostics[currentIndex].range.start.line < activeLine) {
            break;
          }
          selectedIndex--;
        }
        if (selectedIndex < 0) {
          selectedIndex = sortedIndices.length - 1;
        }
        const targetIndex = sortedIndices[selectedIndex];
        if (targetIndex !== undefined && diagnostics[targetIndex]) {
          position = diagnostics[targetIndex].range.start;
        }
      }
    }

    // Check if we have a valid position
    if (!position) {
      this.showMessage('Unable to navigate to diagnostic position');
      return;
    }

    // Reveal the range of the diagnostic in the editor
    const finalIndex = sortedIndices[selectedIndex];
    if (finalIndex !== undefined && diagnostics[finalIndex]) {
      editor.revealRange(diagnostics[finalIndex].range, TextEditorRevealType.InCenter);
    }

    // Display the diagnostic message in the output channel
    this.displayDiagnosticMessage(diagnostics, position);

    // Set the editor selection to the diagnostic's start position
    editor.selection = new Selection(position, position);
    this.outputChannel.show(true);
  }

  /**
   * Update sorted diagnostic indices for a document
   */
  public updateSortedDiagnosticIndex(documentUri: string, diagnostics: Diagnostic[]): void {
    const index = diagnostics
      .map((diag, i) => {
        return [diag.range.start.line, i] as [number, number];
      })
      .sort((a, b) => {
        return a[0] - b[0];
      });

    this.sortedDiagIndex[documentUri] = index.map(item => item[1]);
  }

  /**
   * Display diagnostic message in output channel
   */
  private displayDiagnosticMessage(diagnostics: readonly Diagnostic[], position: Position): void {
    diagnostics.forEach(item => {
      if (item.range.start.line === position.line) {
        const severity = item.severity === DiagnosticSeverity.Error ? 'ERROR:\t\t' : 'Warning:\t';
        this.outputChannel.append(severity + item.message + '\n');
      }
    });
  }

  /**
   * Show message in output channel
   */
  private showMessage(message: string): void {
    this.outputChannel.clear();
    this.outputChannel.append(message + '\n');
    this.outputChannel.show(true);
  }

  /**
   * Display all diagnostics for a document in output channel
   */
  public displayAllDiagnostics(documentPath: string, diagnostics: Diagnostic[]): void {
    if (!diagnostics || diagnostics.length === 0) {
      return;
    }

    const sortedIndices = this.sortedDiagIndex[documentPath];
    if (!sortedIndices) {
      return;
    }

    this.outputChannel.clear();

    for (let i = 0; i < sortedIndices.length; i++) {
      const diagIndex = sortedIndices[i];
      if (diagIndex !== undefined && diagIndex < diagnostics.length) {
        const diag = diagnostics[diagIndex];
        if (diag) {
          const severity = diag.severity === DiagnosticSeverity.Error ? 'ERROR' : 'Warning';
          const msg = `${path.basename(documentPath)}:${diag.range.start.line + 1}:\t${severity}:\t${diag.message}\n`;
          this.outputChannel.append(msg);
        }
      }
    }

    if (sortedIndices.length > 0) {
      this.outputChannel.show(true);
    }
  }

  /**
   * Get diagnostic count for a document
   */
  public getDiagnosticCount(documentUri: string): { errors: number; warnings: number; total: number } {
    const editor = window.activeTextEditor;
    if (!editor) {
      return { errors: 0, warnings: 0, total: 0 };
    }
    
    const diagnostics = this.diagnosticCollection.get(editor.document.uri);
    if (!diagnostics) {
      return { errors: 0, warnings: 0, total: 0 };
    }

    let errors = 0;
    let warnings = 0;

    diagnostics.forEach(diagnostic => {
      if (diagnostic.severity === DiagnosticSeverity.Error) {
        errors++;
      } else if (diagnostic.severity === DiagnosticSeverity.Warning) {
        warnings++;
      }
    });

    return { errors, warnings, total: errors + warnings };
  }

  /**
   * Clear navigation state for a document
   */
  public clearNavigationState(documentUri: string): void {
    delete this.sortedDiagIndex[documentUri];
  }

  /**
   * Check if navigation is available for current document
   */
  public isNavigationAvailable(): boolean {
    const editor = window.activeTextEditor;
    if (!editor) {
      return false;
    }

    const diagnostics = this.diagnosticCollection.get(editor.document.uri);
    return diagnostics !== undefined && diagnostics.length > 0;
  }
}