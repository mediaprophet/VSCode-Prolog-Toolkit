import { TextDocument } from 'vscode-languageserver-textdocument';
import { CodeAction, CodeActionKind, Diagnostic, Range } from 'vscode-languageserver/node';
import { CodeActionProvider, LSPContext } from './types';

export class PrologCodeActionsProvider implements CodeActionProvider {
  async provideCodeActions(
    document: TextDocument, 
    range: Range, 
    diagnostics: Diagnostic[], 
    context: LSPContext
  ): Promise<CodeAction[]> {
    const actions: CodeAction[] = [];
    const selectedText = document.getText(range);

    // Quick fix actions for diagnostics
    for (const diagnostic of diagnostics) {
      if (diagnostic.source === 'prolog-lsp') {
        if (diagnostic.message.includes('missing period')) {
          const fix: CodeAction = {
            title: 'Add missing period',
            kind: CodeActionKind.QuickFix,
            diagnostics: [diagnostic],
            edit: {
              changes: {
                [document.uri]: [
                  {
                    range: {
                      start: diagnostic.range.end,
                      end: diagnostic.range.end,
                    },
                    newText: '.',
                  },
                ],
              },
            },
          };
          actions.push(fix);
        }

        if (diagnostic.message.includes('Unmatched parentheses')) {
          const fix: CodeAction = {
            title: 'Fix unmatched parentheses',
            kind: CodeActionKind.QuickFix,
            diagnostics: [diagnostic],
            edit: {
              changes: {
                [document.uri]: [
                  {
                    range: {
                      start: diagnostic.range.end,
                      end: diagnostic.range.end,
                    },
                    newText: ')',
                  },
                ],
              },
            },
          };
          actions.push(fix);
        }
      }
    }

    // Refactor actions
    if (selectedText.trim()) {
      // Execute as query
      const executeAction: CodeAction = {
        title: 'Execute as Prolog query',
        kind: CodeActionKind.Source,
        command: {
          title: 'Execute Query',
          command: 'prolog.executeQuery',
          arguments: [selectedText.trim()],
        },
      };
      actions.push(executeAction);

      // Get help for predicate
      const predicateMatch = selectedText.match(/([a-z][a-zA-Z0-9_]*)/);
      if (predicateMatch) {
        const helpAction: CodeAction = {
          title: `Get help for ${predicateMatch[1]}`,
          kind: CodeActionKind.Source,
          command: {
            title: 'Get Help',
            command: 'prolog.getHelp',
            arguments: [predicateMatch[1]],
          },
        };
        actions.push(helpAction);
      }

      // Extract predicate
      const extractPredicateAction: CodeAction = {
        title: 'Extract as new predicate',
        kind: CodeActionKind.Refactor,
        edit: {
          changes: {
            [document.uri]: [
              {
                range: range,
                newText: 'new_predicate',
              },
              {
                range: {
                  start: { line: 0, character: 0 },
                  end: { line: 0, character: 0 },
                },
                newText: `new_predicate :- ${selectedText.trim()}.\n\n`,
              },
            ],
          },
        },
      };
      actions.push(extractPredicateAction);
    }

    // Source actions
    const formatAction: CodeAction = {
      title: 'Format document',
      kind: CodeActionKind.SourceOrganizeImports,
      command: {
        title: 'Format Document',
        command: 'prolog.formatDocument',
        arguments: [document.uri],
      },
    };
    actions.push(formatAction);

    const organizeImportsAction: CodeAction = {
      title: 'Organize imports',
      kind: CodeActionKind.SourceOrganizeImports,
      command: {
        title: 'Organize Imports',
        command: 'prolog.organizeImports',
        arguments: [document.uri],
      },
    };
    actions.push(organizeImportsAction);

    // Add N3 diagnostics action if in N3 context
    if (this.isN3Context(document.getText())) {
      const n3DiagnosticsAction: CodeAction = {
        title: 'Run N3 diagnostics',
        kind: CodeActionKind.Source,
        command: {
          title: 'Run N3 Diagnostics',
          command: 'prolog.runN3Diagnostics',
          arguments: [document.uri],
        },
      };
      actions.push(n3DiagnosticsAction);
    }

    // Context-specific actions based on cursor position
    const line = document.getText({
      start: { line: range.start.line, character: 0 },
      end: { line: range.start.line + 1, character: 0 },
    });

    // If cursor is on a rule head, offer to add body
    if (line.trim().match(/^[a-z][a-zA-Z0-9_]*\([^)]*\)\s*\.$/)) {
      const addBodyAction: CodeAction = {
        title: 'Convert fact to rule',
        kind: CodeActionKind.Refactor,
        edit: {
          changes: {
            [document.uri]: [
              {
                range: {
                  start: { line: range.start.line, character: line.lastIndexOf('.') },
                  end: { line: range.start.line, character: line.lastIndexOf('.') + 1 },
                },
                newText: ' :- true.',
              },
            ],
          },
        },
      };
      actions.push(addBodyAction);
    }

    // If cursor is on a directive, offer to comment it out
    if (line.trim().startsWith(':-')) {
      const commentDirectiveAction: CodeAction = {
        title: 'Comment out directive',
        kind: CodeActionKind.Refactor,
        edit: {
          changes: {
            [document.uri]: [
              {
                range: {
                  start: { line: range.start.line, character: 0 },
                  end: { line: range.start.line, character: 0 },
                },
                newText: '% ',
              },
            ],
          },
        },
      };
      actions.push(commentDirectiveAction);
    }

    return actions;
  }

  private isN3Context(text: string): boolean {
    return (
      text.includes('@prefix') ||
      text.includes('@base') ||
      text.includes('rdf:') ||
      text.includes('rdfs:') ||
      text.includes('owl:')
    );
  }
}