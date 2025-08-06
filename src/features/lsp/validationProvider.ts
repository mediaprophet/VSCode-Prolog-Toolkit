import { TextDocument } from 'vscode-languageserver-textdocument';
import { Diagnostic, DiagnosticSeverity } from 'vscode-languageserver/node';
import { ValidationProvider, LSPContext, BackendResponse } from './types';

export class PrologValidationProvider implements ValidationProvider {
  async validateTextDocument(textDocument: TextDocument, context: LSPContext): Promise<Diagnostic[]> {
    const settings = await context.getDocumentSettings(textDocument.uri);

    if (settings.linter.run === 'never') {
      return [];
    }

    const diagnostics: Diagnostic[] = [];
    const text = textDocument.getText();
    const lines = text.split('\n');

    // Basic Prolog syntax validation
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const trimmedLine = line.trim();

      if (trimmedLine === '' || trimmedLine.startsWith('%')) {
        continue; // Skip empty lines and comments
      }

      // Check for common syntax errors
      const diagnosticsForLine = this.validatePrologLine(line, i, textDocument);
      diagnostics.push(...diagnosticsForLine);
    }

    // Advanced validation using Prolog backend
    if (context.prologBackend?.isRunning()) {
      try {
        const backendDiagnostics = await this.validateWithBackend(textDocument, context);
        diagnostics.push(...backendDiagnostics);
      } catch (error: unknown) {
        console.error(`Backend validation error: ${error}`);
      }
    }

    return diagnostics;
  }

  private validatePrologLine(
    line: string,
    lineNumber: number,
    _document: TextDocument
  ): Diagnostic[] {
    const diagnostics: Diagnostic[] = [];
    const trimmedLine = line.trim();

    // Check for unmatched parentheses
    const openParens = (line.match(/\(/g) || []).length;
    const closeParens = (line.match(/\)/g) || []).length;

    if (openParens !== closeParens) {
      const diagnostic: Diagnostic = {
        severity: DiagnosticSeverity.Error,
        range: {
          start: { line: lineNumber, character: 0 },
          end: { line: lineNumber, character: line.length },
        },
        message: `Unmatched parentheses: ${openParens} opening, ${closeParens} closing`,
        source: 'prolog-lsp',
      };
      diagnostics.push(diagnostic);
    }

    // Check for missing periods at end of clauses
    if (
      trimmedLine.length > 0 &&
      !trimmedLine.endsWith('.') &&
      !trimmedLine.endsWith(':-') &&
      !trimmedLine.startsWith('?-') &&
      !trimmedLine.startsWith(':-')
    ) {
      const diagnostic: Diagnostic = {
        severity: DiagnosticSeverity.Warning,
        range: {
          start: { line: lineNumber, character: line.length - 1 },
          end: { line: lineNumber, character: line.length },
        },
        message: 'Clause should end with a period',
        source: 'prolog-lsp',
      };
      diagnostics.push(diagnostic);
    }

    // Check for undefined predicates (basic heuristic)
    const predicateMatch = trimmedLine.match(/^([a-z][a-zA-Z0-9_]*)\s*\(/);
    if (predicateMatch) {
      const predicate = predicateMatch[1];
      if (this.isLikelyUndefinedPredicate(predicate)) {
        const diagnostic: Diagnostic = {
          severity: DiagnosticSeverity.Information,
          range: {
            start: { line: lineNumber, character: line.indexOf(predicate) },
            end: { line: lineNumber, character: line.indexOf(predicate) + predicate.length },
          },
          message: `Predicate '${predicate}' may be undefined`,
          source: 'prolog-lsp',
        };
        diagnostics.push(diagnostic);
      }
    }

    return diagnostics;
  }

  private isLikelyUndefinedPredicate(predicate: string): boolean {
    // List of common built-in predicates
    const builtins = [
      'member',
      'append',
      'length',
      'reverse',
      'sort',
      'findall',
      'bagof',
      'setof',
      'assert',
      'retract',
      'asserta',
      'assertz',
      'retracta',
      'retractall',
      'write',
      'writeln',
      'read',
      'get',
      'put',
      'nl',
      'tab',
      'is',
      'var',
      'nonvar',
      'atom',
      'number',
      'compound',
      'atomic',
      'functor',
      'arg',
      'univ',
      'copy_term',
      'numbervars',
      'call',
      'once',
      'ignore',
      'forall',
      'between',
      'succ',
      'true',
      'fail',
      'halt',
      'abort',
      'trace',
      'notrace',
      'spy',
      'nospy',
    ];

    return !builtins.includes(predicate);
  }

  private async validateWithBackend(document: TextDocument, context: LSPContext): Promise<Diagnostic[]> {
    if (!context.prologBackend?.isRunning()) {
      return [];
    }

    try {
      // Try to consult the document content
      const response: BackendResponse = await context.prologBackend.sendRequest('validate', {
        content: document.getText(),
        uri: document.uri,
        timeoutMs: 5000,
      });

      if (response.status === 'error' && response.errors) {
        return response.errors.map(
          (error: { line?: number; column?: number; length?: number; message?: string }) => ({
            severity: DiagnosticSeverity.Error,
            range: {
              start: { line: error.line || 0, character: error.column || 0 },
              end: { line: error.line || 0, character: (error.column || 0) + (error.length || 1) },
            },
            message: error.message || 'Validation error',
            source: 'prolog-backend',
          })
        );
      }
    } catch (_error: unknown) {
      // Backend validation failed, return empty array
    }

    return [];
  }
}