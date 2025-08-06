import { TextDocument } from 'vscode-languageserver-textdocument';
import { DocumentSymbol, SymbolInformation, SymbolKind } from 'vscode-languageserver/node';
import { SymbolProvider, LSPContext } from './types';

export class PrologSymbolProvider implements SymbolProvider {
  async provideDocumentSymbols(document: TextDocument, context: LSPContext): Promise<DocumentSymbol[]> {
    const symbols: DocumentSymbol[] = [];
    const text = document.getText();
    const lines = text.split('\n');

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();

      if (line.startsWith('%') || line === '') {
        continue;
      }

      // Match predicate definitions
      const predicateMatch = line.match(/^([a-z][a-zA-Z0-9_]*)\s*\(/);
      if (predicateMatch) {
        const name = predicateMatch[1];
        const arity = this.countArity(line);

        symbols.push({
          name: `${name}/${arity}`,
          kind: SymbolKind.Function,
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
      if (directiveMatch) {
        const content = directiveMatch[1];
        symbols.push({
          name: `:- ${content}`,
          kind: SymbolKind.Namespace,
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
      if (moduleMatch) {
        const moduleName = moduleMatch[1];
        symbols.push({
          name: `module: ${moduleName}`,
          kind: SymbolKind.Module,
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
      if (useModuleMatch) {
        const moduleRef = useModuleMatch[1];
        symbols.push({
          name: `use_module: ${moduleRef}`,
          kind: SymbolKind.Package,
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
      if (includeMatch) {
        const includeRef = includeMatch[1];
        symbols.push({
          name: `include: ${includeRef}`,
          kind: SymbolKind.File,
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
      if (dcgMatch) {
        const name = dcgMatch[1];
        const params = dcgMatch[2] || '()';
        const arity = this.countArity(params);
        
        symbols.push({
          name: `${name}//${arity} (DCG)`,
          kind: SymbolKind.Method,
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
      if (operatorMatch) {
        const precedence = operatorMatch[1];
        const associativity = operatorMatch[2].trim();
        const operator = operatorMatch[3].trim();
        
        symbols.push({
          name: `op ${precedence} ${associativity} ${operator}`,
          kind: SymbolKind.Operator,
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

  async provideWorkspaceSymbols(query: string, documents: TextDocument[], context: LSPContext): Promise<SymbolInformation[]> {
    const symbols: SymbolInformation[] = [];
    const queryLower = query.toLowerCase();

    // Search through all documents
    for (const document of documents) {
      const documentSymbols: DocumentSymbol[] = await this.provideDocumentSymbols(document, context);

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
    if (!match) return 0;

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
  public async getSymbolsByKind(document: TextDocument, kind: SymbolKind, context: LSPContext): Promise<DocumentSymbol[]> {
    const allSymbols = await this.provideDocumentSymbols(document, context);
    return allSymbols.filter(symbol => symbol.kind === kind);
  }

  // Method to get predicates only
  public async getPredicates(document: TextDocument, context: LSPContext): Promise<DocumentSymbol[]> {
    return this.getSymbolsByKind(document, SymbolKind.Function, context);
  }

  // Method to get directives only
  public async getDirectives(document: TextDocument, context: LSPContext): Promise<DocumentSymbol[]> {
    return this.getSymbolsByKind(document, SymbolKind.Namespace, context);
  }

  // Method to get modules only
  public async getModules(document: TextDocument, context: LSPContext): Promise<DocumentSymbol[]> {
    return this.getSymbolsByKind(document, SymbolKind.Module, context);
  }

  // Method to find symbol at position
  public findSymbolAtPosition(document: TextDocument, line: number, character: number): DocumentSymbol | null {
    // This would need to be implemented with the document symbols
    // For now, return null as it requires async call to provideDocumentSymbols
    return null;
  }
}