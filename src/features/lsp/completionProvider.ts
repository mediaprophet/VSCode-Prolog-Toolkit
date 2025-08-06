import { TextDocument } from 'vscode-languageserver-textdocument';
import { CompletionItem, CompletionItemKind, Position } from 'vscode-languageserver/node';
import { CompletionProvider, LSPContext, PredicateInfo, N3CompletionInfo, BackendResponse } from './types';

export class PrologCompletionProvider implements CompletionProvider {
  async provideCompletions(document: TextDocument, position: Position, context: LSPContext): Promise<CompletionItem[]> {
    const text = document.getText();
    const lines = text.split('\n');
    const currentLine = lines[position.line] || '';
    const _prefix = currentLine.substring(0, position.character);

    const completions: CompletionItem[] = [];

    // Built-in predicates
    const builtinCompletions = this.getBuiltinPredicateCompletions();
    completions.push(...builtinCompletions);

    // N3/RDF completions if in N3 context
    if (this.isN3Context(text)) {
      const n3Completions = this.getN3Completions();
      completions.push(...n3Completions);
    }

    // Dynamic completions from backend
    if (context.prologBackend?.isRunning()) {
      try {
        const dynamicCompletions = await this.getDynamicCompletions(document, position, context);
        completions.push(...dynamicCompletions);
      } catch (error: unknown) {
        // Ignore errors in dynamic completion
      }
    }

    return completions;
  }

  private getBuiltinPredicateCompletions(): CompletionItem[] {
    const builtinPredicates: PredicateInfo[] = [
      { name: 'member', arity: 2, description: 'True if Elem is a member of List' },
      {
        name: 'append',
        arity: 3,
        description: 'True if List3 is the concatenation of List1 and List2',
      },
      { name: 'length', arity: 2, description: 'True if Length is the length of List' },
      { name: 'reverse', arity: 2, description: 'True if List2 is the reverse of List1' },
      { name: 'sort', arity: 2, description: 'True if Sorted is the sorted version of List' },
      { name: 'findall', arity: 3, description: 'Find all solutions to Goal' },
      { name: 'bagof', arity: 3, description: 'Collect solutions to Goal' },
      { name: 'setof', arity: 3, description: 'Collect unique solutions to Goal' },
      { name: 'assert', arity: 1, description: 'Add clause to database' },
      { name: 'retract', arity: 1, description: 'Remove clause from database' },
      { name: 'write', arity: 1, description: 'Write term to output' },
      { name: 'writeln', arity: 1, description: 'Write term followed by newline' },
      { name: 'nl', arity: 0, description: 'Write newline to output' },
      { name: 'is', arity: 2, description: 'Arithmetic evaluation' },
      { name: 'var', arity: 1, description: 'True if argument is unbound variable' },
      { name: 'nonvar', arity: 1, description: 'True if argument is not unbound variable' },
      { name: 'atom', arity: 1, description: 'True if argument is an atom' },
      { name: 'number', arity: 1, description: 'True if argument is a number' },
      { name: 'compound', arity: 1, description: 'True if argument is a compound term' },
      { name: 'functor', arity: 3, description: 'Relate compound term to functor name and arity' },
      { name: 'arg', arity: 3, description: 'Extract argument from compound term' },
      { name: 'univ', arity: 2, description: 'Convert between term and list representation' },
      { name: 'call', arity: 1, description: 'Call goal dynamically' },
      { name: 'once', arity: 1, description: 'Succeed at most once' },
      {
        name: 'forall',
        arity: 2,
        description: 'For all solutions of Condition, Action must succeed',
      },
      { name: 'between', arity: 3, description: 'Generate integers between bounds' },
      { name: 'succ', arity: 2, description: 'Successor relation for integers' },
    ];

    return builtinPredicates.map(pred => ({
      label: pred.name,
      kind: CompletionItemKind.Function,
      detail: `${pred.name}/${pred.arity}`,
      documentation: pred.description,
      insertText: pred.arity > 0 ? `${pred.name}(` : pred.name,
    }));
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

  private getN3Completions(): CompletionItem[] {
    const completions: CompletionItem[] = [];

    // N3 prefixes
    const prefixes: N3CompletionInfo[] = [
      { name: '@prefix rdf:', detail: '<http://www.w3.org/1999/02/22-rdf-syntax-ns#>' },
      { name: '@prefix rdfs:', detail: '<http://www.w3.org/2000/01/rdf-schema#>' },
      { name: '@prefix owl:', detail: '<http://www.w3.org/2002/07/owl#>' },
      { name: '@prefix xsd:', detail: '<http://www.w3.org/2001/XMLSchema#>' },
      { name: '@prefix foaf:', detail: '<http://xmlns.com/foaf/0.1/>' },
      { name: '@prefix dc:', detail: '<http://purl.org/dc/elements/1.1/>' },
    ];

    prefixes.forEach(prefix => {
      completions.push({
        label: prefix.name,
        kind: CompletionItemKind.Keyword,
        detail: prefix.detail,
        insertText: `${prefix.name} ${prefix.detail} .`,
      });
    });

    // Common RDF properties
    const properties = [
      'rdf:type',
      'rdfs:label',
      'rdfs:comment',
      'rdfs:seeAlso',
      'rdfs:isDefinedBy',
      'owl:sameAs',
      'owl:differentFrom',
      'owl:equivalentClass',
      'owl:equivalentProperty',
      'foaf:name',
      'foaf:knows',
      'foaf:mbox',
      'dc:title',
      'dc:creator',
      'dc:date',
    ];

    properties.forEach(prop => {
      completions.push({
        label: prop,
        kind: CompletionItemKind.Property,
        detail: 'RDF property',
      });
    });

    return completions;
  }

  private async getDynamicCompletions(
    document: TextDocument,
    position: Position,
    context: LSPContext
  ): Promise<CompletionItem[]> {
    if (!context.prologBackend?.isRunning()) {
      return [];
    }

    try {
      const response: BackendResponse = await context.prologBackend.sendRequest('completions', {
        uri: document.uri,
        content: document.getText(),
        line: position.line,
        character: position.character,
        timeoutMs: 2000,
      });

      if (response.status === 'ok' && response.completions) {
        return response.completions.map(
          (comp: {
            label: string;
            kind?: string;
            detail?: string;
            documentation?: string;
            insertText?: string;
          }) => ({
            label: comp.label,
            kind: this.getCompletionItemKind(comp.kind),
            detail: comp.detail,
            documentation: comp.documentation,
            insertText: comp.insertText || comp.label,
          })
        );
      }
    } catch (error: unknown) {
      // Ignore errors
    }

    return [];
  }

  private getCompletionItemKind(kind?: string): CompletionItemKind {
    switch (kind) {
      case 'predicate': {
        return CompletionItemKind.Function;
      }
      case 'variable': {
        return CompletionItemKind.Variable;
      }
      case 'atom': {
        return CompletionItemKind.Constant;
      }
      case 'module': {
        return CompletionItemKind.Module;
      }
      case 'operator': {
        return CompletionItemKind.Operator;
      }
      default: {
        return CompletionItemKind.Text;
      }
    }
  }
}