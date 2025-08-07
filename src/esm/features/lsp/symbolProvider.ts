import { TextDocument } from 'vscode-languageserver-textdocument';
import type { LSPContext, SymbolProvider } from './types.js';
type DocumentSymbol = any;
type SymbolInformation = any;
enum SymbolKind {
  File = 17,
  Module = 1,
  Namespace = 2,
  Package = 3,
  Method = 6,
  Function = 12,
  Operator = 25,
}
const SymbolKindEnum = SymbolKind;

export class PrologSymbolProvider implements SymbolProvider {
  async provideDocumentSymbols(
    document: TextDocument,
    _context: LSPContext
  ): Promise<DocumentSymbol[]> {
    const symbols: DocumentSymbol[] = [];
    const text = document.getText();
    const lines = text.split('\n');

    for (let i = 0; i < lines.length; i++) {
      const lineRaw = lines[i];
      if (typeof lineRaw !== 'string') continue;
      const line = lineRaw.trim();
      if (line.startsWith('%') || line === '') {
        continue;
      }

      // Match predicate definitions
      const predicateMatch = line.match(/^([a-z][a-zA-Z0-9_]*)\s*\(/);
      if (predicateMatch && typeof predicateMatch[1] === 'string') {
        const name = predicateMatch[1];
        const arity = this.countArity(line);
        symbols.push({
          name: `${name}/${arity}`,
          kind: SymbolKindEnum.Function,
          range: {
            start: { line: i, character: 0 },
            end: { line: i, character: line.length },
          },
          selectionRange: {
            start: { line: i, character: 0 },
            end: { line: i, character: name.length },
          },
        });
      }

      // Match directives
      const directiveMatch = line.match(/^:-\s*(.+)\./);
      if (directiveMatch && typeof directiveMatch[1] === 'string') {
        const content = directiveMatch[1];
        symbols.push({
          name: `:- ${content}`,
          kind: SymbolKindEnum.Namespace,
          range: {
            start: { line: i, character: 0 },
            end: { line: i, character: line.length },
          },
          selectionRange: {
            start: { line: i, character: 0 },
            end: { line: i, character: 2 },
          },
        });
      }

      // Match module declarations
      const moduleMatch = line.match(/^:-\s*module\s*\(\s*([a-z][a-zA-Z0-9_]*)\s*,/);
      if (moduleMatch && typeof moduleMatch[1] === 'string') {
        const moduleName = moduleMatch[1];
        symbols.push({
          name: `module: ${moduleName}`,
          kind: SymbolKindEnum.Module,
          range: {
            start: { line: i, character: 0 },
            end: { line: i, character: line.length },
          },
          selectionRange: {
            start: { line: i, character: line.indexOf(moduleName) },
            end: { line: i, character: line.indexOf(moduleName) + moduleName.length },
          },
        });
      }

      // Match use_module declarations
      const useModuleMatch = line.match(/^:-\s*use_module\s*\(\s*([^)]+)\s*\)/);
      if (useModuleMatch && typeof useModuleMatch[1] === 'string') {
        const moduleRef = useModuleMatch[1];
        symbols.push({
          name: `use_module: ${moduleRef}`,
          kind: SymbolKindEnum.Package,
          range: {
            start: { line: i, character: 0 },
            end: { line: i, character: line.length },
          },
          selectionRange: {
            start: { line: i, character: line.indexOf(moduleRef) },
            end: { line: i, character: line.indexOf(moduleRef) + moduleRef.length },
          },
        });
      }

      // Match include declarations
      const includeMatch = line.match(/^:-\s*include\s*\(\s*([^)]+)\s*\)/);
      if (includeMatch && typeof includeMatch[1] === 'string') {
        const includeRef = includeMatch[1];
        symbols.push({
          name: `include: ${includeRef}`,
          kind: SymbolKindEnum.File,
          range: {
            start: { line: i, character: 0 },
            end: { line: i, character: line.length },
          },
          selectionRange: {
            start: { line: i, character: line.indexOf(includeRef) },
            end: { line: i, character: line.indexOf(includeRef) + includeRef.length },
          },
        });
      }

      // Match DCG rules (using -->)
      const dcgMatch = line.match(/^([a-z][a-zA-Z0-9_]*)\s*(\([^)]*\))?\s*-->/);
      if (dcgMatch && typeof dcgMatch[1] === 'string') {
        const name = dcgMatch[1];
        const params = typeof dcgMatch[2] === 'string' ? dcgMatch[2] : '()';
        const arity = this.countArity(params);
        symbols.push({
          name: `${name}//${arity} (DCG)`,
          kind: SymbolKindEnum.Method,
          range: {
            start: { line: i, character: 0 },
            end: { line: i, character: line.length },
          },
          selectionRange: {
            start: { line: i, character: 0 },
            end: { line: i, character: name.length },
          },
        });
      }

      // Match operators
      const operatorMatch = line.match(/^:-\s*op\s*\(\s*(\d+)\s*,\s*([^,]+)\s*,\s*([^)]+)\s*\)/);
      if (
        operatorMatch &&
        typeof operatorMatch[1] === 'string' &&
        typeof operatorMatch[2] === 'string' &&
        typeof operatorMatch[3] === 'string'
      ) {
        const precedence = operatorMatch[1];
        const associativity = operatorMatch[2].trim();
        const operator = operatorMatch[3].trim();
        symbols.push({
          name: `op ${precedence} ${associativity} ${operator}`,
          kind: SymbolKindEnum.Operator,
          range: {
            start: { line: i, character: 0 },
            end: { line: i, character: line.length },
          },
          selectionRange: {
            start: { line: i, character: line.indexOf(operator) },
            end: { line: i, character: line.indexOf(operator) + operator.length },
          },
        });
      }
    }

    return symbols;
  }

  async provideWorkspaceSymbols(
    query: string,
    documents: TextDocument[],
    context: LSPContext
  ): Promise<SymbolInformation[]> {
    const symbols: SymbolInformation[] = [];
    const queryLower = query.toLowerCase();

    // Search through all documents
    for (const document of documents) {
      const documentSymbols: DocumentSymbol[] = await this.provideDocumentSymbols(
        document,
        context
      );

      for (const symbol of documentSymbols) {
        if (symbol.name.toLowerCase().includes(queryLower)) {
          symbols.push({
            name: symbol.name,
            kind: symbol.kind,
            location: {
              uri: document.uri,
              range: symbol.range,
            },
            containerName: this.getContainerName(document.uri),
          });
        }
      }
    }

    // Sort symbols by relevance (exact matches first, then partial matches)
    symbols.sort((a, b) => {
      const aExact = a.name.toLowerCase() === queryLower;
      const bExact = b.name.toLowerCase() === queryLower;

      if (aExact && !bExact) return -1;
      if (!aExact && bExact) return 1;

      const aStarts = a.name.toLowerCase().startsWith(queryLower);
      const bStarts = b.name.toLowerCase().startsWith(queryLower);

      if (aStarts && !bStarts) return -1;
      if (!aStarts && bStarts) return 1;

      return a.name.localeCompare(b.name);
    });

    return symbols;
  }

  private countArity(line: string): number {
    const match = line.match(/\(([^)]*)\)/);
    if (!match || typeof match[1] !== 'string') return 0;

    const args = match[1].trim();
    if (args === '') return 0;

    // Simple arity counting (doesn't handle nested structures perfectly)
    return args.split(',').length;
  }

  private getContainerName(uri: string): string | undefined {
    const parts = uri.split('/');
    const fileName = parts[parts.length - 1];
    if (fileName) {
      return fileName.replace(/\.[^.]*$/, ''); // Remove extension
    }
    return undefined;
  }

  // Additional utility method to get symbols by kind
  public async getSymbolsByKind(
    document: TextDocument,
    kind: SymbolKind,
    context: LSPContext
  ): Promise<DocumentSymbol[]> {
    const allSymbols = await this.provideDocumentSymbols(document, context);
    return allSymbols.filter(symbol => symbol.kind === kind);
  }

  // Method to get predicates only
  public async getPredicates(
    document: TextDocument,
    context: LSPContext
  ): Promise<DocumentSymbol[]> {
    return this.getSymbolsByKind(document, SymbolKind.Function, context);
  }

  // Method to get directives only
  public async getDirectives(
    document: TextDocument,
    context: LSPContext
  ): Promise<DocumentSymbol[]> {
    return this.getSymbolsByKind(document, SymbolKind.Namespace, context);
  }

  // Method to get modules only
  public async getModules(document: TextDocument, context: LSPContext): Promise<DocumentSymbol[]> {
    return this.getSymbolsByKind(document, SymbolKind.Module, context);
  }

  // Method to find symbol at position
  public findSymbolAtPosition(
    document: TextDocument,
    line: number,
    character: number
  ): DocumentSymbol | null {
    // Not implemented: requires async call to provideDocumentSymbols
    return null;
  }
}
