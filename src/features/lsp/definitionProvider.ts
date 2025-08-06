import { TextDocument } from 'vscode-languageserver-textdocument';
import { Definition, Location, Position } from 'vscode-languageserver/node';
import { DefinitionProvider, LSPContext, BackendResponse } from './types';

export class PrologDefinitionProvider implements DefinitionProvider {
  async provideDefinition(document: TextDocument, position: Position, context: LSPContext): Promise<Definition | null> {
    const word = this.getWordAtPosition(document.getText(), position);

    if (!word) {
      return null;
    }

    // Try to find definition using backend
    if (context.prologBackend?.isRunning()) {
      try {
        const response: BackendResponse = await context.prologBackend.sendRequest('definition', {
          predicate: word,
          uri: document.uri,
          line: position.line,
          character: position.character,
          timeoutMs: 3000,
        });

        if (response.status === 'ok' && response.locations) {
          return response.locations.map((loc: { uri: string; line: number; character: number }) => ({
            uri: loc.uri,
            range: {
              start: { line: loc.line, character: loc.character },
              end: { line: loc.line, character: loc.character + word.length },
            },
          }));
        }
      } catch (_error: unknown) {
        // Fall back to local search
      }
    }

    // Local definition search
    return this.findLocalDefinition(document, word);
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

  private findLocalDefinition(document: TextDocument, predicate: string): Location[] {
    const text = document.getText();
    const lines = text.split('\n');
    const locations: Location[] = [];

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      
      // Look for predicate definitions (both facts and rule heads)
      const patterns = [
        // Rule definition: predicate(...) :-
        new RegExp(`^\\s*${this.escapeRegex(predicate)}\\s*\\(`),
        // Fact definition: predicate(...).
        new RegExp(`^\\s*${this.escapeRegex(predicate)}\\s*\\([^)]*\\)\\s*\\.`),
        // Directive definition: :- predicate(...)
        new RegExp(`^\\s*:-\\s*${this.escapeRegex(predicate)}\\s*\\(`),
      ];

      for (const pattern of patterns) {
        const match = pattern.exec(line);
        if (match) {
          const startIndex = line.indexOf(predicate);
          if (startIndex !== -1) {
            locations.push({
              uri: document.uri,
              range: {
                start: { line: i, character: startIndex },
                end: { line: i, character: startIndex + predicate.length },
              },
            });
          }
        }
      }
    }

    return locations;
  }

  private escapeRegex(str: string): string {
    return str.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  }

  // Additional method to find all references to a predicate (used by references provider)
  public findAllReferences(document: TextDocument, predicate: string, includeDeclaration = true): Location[] {
    const text = document.getText();
    const lines = text.split('\n');
    const locations: Location[] = [];

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      
      // Find all occurrences of the predicate
      const regex = new RegExp(`\\b${this.escapeRegex(predicate)}\\b`, 'g');
      let match;

      while ((match = regex.exec(line)) !== null) {
        const isDeclaration = this.isPredicateDeclaration(line, match.index, predicate);
        
        // Include this occurrence if we want declarations or it's not a declaration
        if (includeDeclaration || !isDeclaration) {
          locations.push({
            uri: document.uri,
            range: {
              start: { line: i, character: match.index },
              end: { line: i, character: match.index + predicate.length },
            },
          });
        }
      }
    }

    return locations;
  }

  private isPredicateDeclaration(line: string, predicateIndex: number, predicate: string): boolean {
    const beforePredicate = line.substring(0, predicateIndex).trim();
    const afterPredicate = line.substring(predicateIndex + predicate.length);
    
    // It's a declaration if:
    // 1. It starts the line (possibly after whitespace or :-)
    // 2. It's followed by an opening parenthesis
    // 3. The line contains :- (rule) or ends with . (fact)
    const startsLine = beforePredicate === '' || beforePredicate === ':-';
    const followedByParen = afterPredicate.trim().startsWith('(');
    const isRuleOrFact = line.includes(':-') || line.trim().endsWith('.');
    
    return startsLine && followedByParen && isRuleOrFact;
  }

  // Method to find predicate calls (excluding declarations)
  public findPredicateCalls(document: TextDocument, predicate: string): Location[] {
    return this.findAllReferences(document, predicate, false);
  }

  // Method to find predicate declarations only
  public findPredicateDeclarations(document: TextDocument, predicate: string): Location[] {
    const text = document.getText();
    const lines = text.split('\n');
    const locations: Location[] = [];

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      
      // Find all occurrences of the predicate
      const regex = new RegExp(`\\b${this.escapeRegex(predicate)}\\b`, 'g');
      let match;

      while ((match = regex.exec(line)) !== null) {
        if (this.isPredicateDeclaration(line, match.index, predicate)) {
          locations.push({
            uri: document.uri,
            range: {
              start: { line: i, character: match.index },
              end: { line: i, character: match.index + predicate.length },
            },
          });
        }
      }
    }

    return locations;
  }
}