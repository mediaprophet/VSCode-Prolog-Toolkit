import { TextDocument } from 'vscode-languageserver-textdocument';
import type { DocumentHighlight, Location, Position } from 'vscode-languageserver-types';
import { DocumentHighlightKind } from 'vscode-languageserver-types';
import { PrologDefinitionProvider } from './definitionProvider.js';
import type { LSPContext, ReferencesProvider } from './types.js';
// DocumentHighlight and DocumentHighlightKind are not used in the provided code, remove them if not needed.

export class PrologReferencesProvider implements ReferencesProvider {
  private definitionProvider: PrologDefinitionProvider;

  constructor() {
    this.definitionProvider = new PrologDefinitionProvider();
  }

  async provideReferences(
    document: TextDocument,
    position: Position,
    includeDeclaration: boolean,
    _context: LSPContext
  ): Promise<Location[]> {
    const word = this.getWordAtPosition(document.getText(), position);

    if (!word) {
      return [];
    }

    const locations: Location[] = [];

    // Search in current document
    const currentDocReferences = this.findReferencesInDocument(document, word, includeDeclaration);
    locations.push(...currentDocReferences);

    // TODO: Search in other documents in the workspace
    // This would require access to all documents in the workspace
    // For now, we only search in the current document

    return locations;
  }

  async provideDocumentHighlights(
    document: TextDocument,
    position: Position,
    _context: LSPContext
  ): Promise<DocumentHighlight[]> {
    const word = this.getWordAtPosition(document.getText(), position);

    if (!word) {
      return [];
    }

    const highlights: DocumentHighlight[] = [];
    const text = document.getText();
    const lines = text.split('\n');

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      if (typeof line !== 'string') continue;
      const regex = new RegExp(`\\b${this.escapeRegex(word)}\\b`, 'g');
      let match;

      while ((match = regex.exec(line)) !== null) {
        const kind = this.getHighlightKind(line, match.index, word);

        highlights.push({
          range: {
            start: { line: i, character: match.index },
            end: { line: i, character: match.index + word.length },
          },
          kind,
        });
      }
    }

    return highlights;
  }

  private getWordAtPosition(text: string, position: Position): string | null {
    const lines = text.split('\n');
    const line = lines[position.line];
    if (typeof line !== 'string') {
      return null;
    }

    const char = position.character;
    let start = char;
    let end = char;

    // Find word boundaries
    while (start > 0) {
      const prevChar = line[start - 1];
      if (typeof prevChar === 'string' && /[a-zA-Z0-9_]/.test(prevChar)) {
        start--;
      } else {
        break;
      }
    }
    while (end < line.length) {
      const nextChar = line[end];
      if (typeof nextChar === 'string' && /[a-zA-Z0-9_]/.test(nextChar)) {
        end++;
      } else {
        break;
      }
    }

    return start < end ? line.substring(start, end) : null;
  }

  private findReferencesInDocument(
    document: TextDocument,
    word: string,
    includeDeclaration: boolean
  ): Location[] {
    const text = document.getText();
    const lines = text.split('\n');
    const locations: Location[] = [];

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      if (typeof line !== 'string') continue;
      const regex = new RegExp(`\\b${this.escapeRegex(word)}\\b`, 'g');
      let match;

      while ((match = regex.exec(line)) !== null) {
        if (!match || typeof match.index !== 'number') continue;
        const isDeclaration = this.isPredicateDeclaration(line, match.index, word);

        // Include this occurrence if we want declarations or it's not a declaration
        if (includeDeclaration || !isDeclaration) {
          locations.push({
            uri: document.uri,
            range: {
              start: { line: i, character: match.index },
              end: { line: i, character: match.index + word.length },
            },
          });
        }
      }
    }

    return locations;
  }

  private isPredicateDeclaration(line: string, predicateIndex: number, predicate: string): boolean {
    // It's a declaration if:
    // 1. It starts the line (possibly after whitespace or :-)
    // 2. It's followed by an opening parenthesis
    // 3. The line contains :- (rule) or ends with . (fact)
    const afterPredicate = line.substring(predicateIndex + predicate.length);
    const startsLine =
      line.substring(0, predicateIndex).trim() === '' ||
      line.substring(0, predicateIndex).trim() === ':-';
    const followedByParen =
      typeof afterPredicate === 'string' && afterPredicate.trim().startsWith('(');
    const isRuleOrFact = line.includes(':-') || line.trim().endsWith('.');
    return startsLine && followedByParen && isRuleOrFact;
  }

  private getHighlightKind(
    line: string,
    predicateIndex: number,
    predicate: string
  ): DocumentHighlightKind {
    // Determine the kind of highlight based on context
    if (this.isPredicateDeclaration(line, predicateIndex, predicate)) {
      return DocumentHighlightKind.Write; // Declaration/definition
    }

    // Check if it's in a read context (being called)
    // ...existing code...
    const afterPredicate = line.substring(predicateIndex + predicate.length);

    // If followed by opening parenthesis, it's likely a call
    if (typeof afterPredicate === 'string' && afterPredicate.trim().startsWith('(')) {
      return DocumentHighlightKind.Read;
    }

    // Default to text highlight
    return DocumentHighlightKind.Text;
  }

  private escapeRegex(str: string): string {
    return str.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  }

  // Additional utility methods

  // Find all references to a predicate across multiple documents
  public async findReferencesInDocuments(
    documents: TextDocument[],
    predicate: string,
    includeDeclaration = true
  ): Promise<Location[]> {
    const allLocations: Location[] = [];

    for (const document of documents) {
      const locations = this.findReferencesInDocument(document, predicate, includeDeclaration);
      allLocations.push(...locations);
    }

    return allLocations;
  }

  // Find unused predicates (declared but never called)
  public findUnusedPredicates(document: TextDocument): string[] {
    const unusedPredicates: string[] = [];
    const text = document.getText();
    const lines = text.split('\n');

    // Find all predicate declarations
    const declarations = new Set<string>();
    const usages = new Set<string>();

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      if (typeof line !== 'string') continue;
      // Find predicate declarations
      const declarationMatch = line.match(/^([a-z][a-zA-Z0-9_]*)\s*\(/);
      if (declarationMatch && typeof declarationMatch[1] === 'string') {
        declarations.add(declarationMatch[1]);
      }
      // Find predicate usages (not at start of line)
      const usageMatches = line.match(/\b[a-z][a-zA-Z0-9_]*\s*\(/g);
      if (usageMatches) {
        for (const match of usageMatches) {
          const predicateName = match.replace(/\s*\($/, '');
          // Only count as usage if not at the start of the line (not a declaration)
          if (!line.trim().startsWith(match)) {
            usages.add(predicateName);
          }
        }
      }
    }

    // Find declared predicates that are never used
    Array.from(declarations).forEach(declared => {
      if (!usages.has(declared)) {
        unusedPredicates.push(declared);
      }
    });

    return unusedPredicates;
  }

  // Find predicates that are called but never defined
  public findUndefinedPredicates(document: TextDocument): string[] {
    const undefinedPredicates: string[] = [];
    const text = document.getText();
    const lines = text.split('\n');

    const declarations = new Set<string>();
    const usages = new Set<string>();

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      if (typeof line !== 'string') continue;

      // Find predicate declarations
      const declarationMatch = line.match(/^([a-z][a-zA-Z0-9_]*)\s*\(/);
      if (declarationMatch && typeof declarationMatch[1] === 'string') {
        declarations.add(declarationMatch[1]);
      }

      // Find predicate usages
      const usageMatches = line.match(/\b[a-z][a-zA-Z0-9_]*\s*\(/g);
      if (usageMatches) {
        for (const match of usageMatches) {
          const predicateName = match.replace(/\s*\($/, '');
          // Only count as usage if not at the start of the line
          if (!line.trim().startsWith(match)) {
            usages.add(predicateName);
          }
        }
      }
    }

    // Find used predicates that are never declared (excluding built-ins)
    const builtins = new Set([
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
      'write',
      'writeln',
      'nl',
      'is',
      'var',
      'nonvar',
      'atom',
      'number',
      'compound',
      'functor',
      'arg',
      'univ',
      'call',
      'once',
      'forall',
      'between',
      'succ',
      'true',
      'fail',
      'cut',
    ]);

    Array.from(usages).forEach(used => {
      if (!declarations.has(used) && !builtins.has(used)) {
        undefinedPredicates.push(used);
      }
    });

    return undefinedPredicates;
  }
}
