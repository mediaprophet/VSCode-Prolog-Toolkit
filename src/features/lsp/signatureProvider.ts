import { TextDocument } from 'vscode-languageserver-textdocument';
import type { LSPContext, SignatureProvider } from './types.js';
type SignatureHelp = any;
type SignatureInformation = any;
type Position = { line: number; character: number };

export class PrologSignatureProvider implements SignatureProvider {
  async provideSignatureHelp(
    document: TextDocument,
    position: Position,
    _context: LSPContext
  ): Promise<SignatureHelp | null> {
    const text = document.getText();
    const lines = text.split('\n');
    const line = lines[position.line];
    if (typeof line !== 'string') return null;
    // Find the predicate being called
    const beforeCursor = line.substring(0, position.character);
    const predicateMatch = beforeCursor.match(/([a-z][a-zA-Z0-9_]*)\s*\([^)]*$/);
    if (!predicateMatch) {
      return null;
    }
    const predicate = predicateMatch[1];
    // Get signature information
    if (!predicate) {
      return null;
    }
    const signature = this.getPredicateSignature(predicate);
    if (!signature) {
      return null;
    }
    // Count current parameter
    const openParen = beforeCursor.lastIndexOf('(');
    const args = beforeCursor.substring(openParen + 1);
    const parameterIndex = this.countParameters(args);
    return {
      signatures: [signature],
      activeSignature: 0,
      activeParameter: Math.min(parameterIndex, signature.parameters.length - 1),
    };
  }

  private countParameters(args: string): number {
    if (!args.trim()) {
      return 0;
    }

    // Simple parameter counting - split by commas but account for nested structures
    let depth = 0;
    let paramCount = 0;
    let inString = false;
    let stringChar = '';

    for (let i = 0; i < args.length; i++) {
      const char = args[i];

      // Handle strings
      if (!inString && (char === '"' || char === "'")) {
        inString = true;
        stringChar = char;
        continue;
      }

      if (inString) {
        if (char === stringChar && args[i - 1] !== '\\') {
          inString = false;
        }
        continue;
      }

      // Handle nesting
      if (char === '(' || char === '[' || char === '{') {
        depth++;
      } else if (char === ')' || char === ']' || char === '}') {
        depth--;
      } else if (char === ',' && depth === 0) {
        paramCount++;
      }
    }

    return paramCount;
  }

  private getPredicateSignature(predicate: string): SignatureInformation | null {
    const signatures: Record<string, SignatureInformation> = {
      member: {
        label: 'member(Elem, List)',
        documentation: 'True if Elem is a member of List',
        parameters: [
          { label: 'Elem', documentation: 'Element to check' },
          { label: 'List', documentation: 'List to search in' },
        ],
      },
      append: {
        label: 'append(List1, List2, List3)',
        documentation: 'True if List3 is the concatenation of List1 and List2',
        parameters: [
          { label: 'List1', documentation: 'First list' },
          { label: 'List2', documentation: 'Second list' },
          { label: 'List3', documentation: 'Concatenated result' },
        ],
      },
      length: {
        label: 'length(List, Length)',
        documentation: 'True if Length is the length of List',
        parameters: [
          { label: 'List', documentation: 'The list to measure' },
          { label: 'Length', documentation: 'The length of the list' },
        ],
      },
      reverse: {
        label: 'reverse(List, Reversed)',
        documentation: 'True if Reversed is the reverse of List',
        parameters: [
          { label: 'List', documentation: 'The original list' },
          { label: 'Reversed', documentation: 'The reversed list' },
        ],
      },
      sort: {
        label: 'sort(List, Sorted)',
        documentation: 'True if Sorted is the sorted version of List',
        parameters: [
          { label: 'List', documentation: 'The list to sort' },
          { label: 'Sorted', documentation: 'The sorted list' },
        ],
      },
      findall: {
        label: 'findall(Template, Goal, List)',
        documentation: 'Find all solutions to Goal',
        parameters: [
          { label: 'Template', documentation: 'Template for solutions' },
          { label: 'Goal', documentation: 'Goal to solve' },
          { label: 'List', documentation: 'List of solutions' },
        ],
      },
      bagof: {
        label: 'bagof(Template, Goal, List)',
        documentation: 'Collect solutions to Goal (may fail if no solutions)',
        parameters: [
          { label: 'Template', documentation: 'Template for solutions' },
          { label: 'Goal', documentation: 'Goal to solve' },
          { label: 'List', documentation: 'List of solutions' },
        ],
      },
      setof: {
        label: 'setof(Template, Goal, List)',
        documentation: 'Collect unique sorted solutions to Goal',
        parameters: [
          { label: 'Template', documentation: 'Template for solutions' },
          { label: 'Goal', documentation: 'Goal to solve' },
          { label: 'List', documentation: 'Sorted list of unique solutions' },
        ],
      },
      assert: {
        label: 'assert(Clause)',
        documentation: 'Add clause to database',
        parameters: [{ label: 'Clause', documentation: 'The clause to assert' }],
      },
      retract: {
        label: 'retract(Clause)',
        documentation: 'Remove clause from database',
        parameters: [{ label: 'Clause', documentation: 'The clause to retract' }],
      },
      asserta: {
        label: 'asserta(Clause)',
        documentation: 'Add clause to beginning of database',
        parameters: [{ label: 'Clause', documentation: 'The clause to assert at beginning' }],
      },
      assertz: {
        label: 'assertz(Clause)',
        documentation: 'Add clause to end of database',
        parameters: [{ label: 'Clause', documentation: 'The clause to assert at end' }],
      },
      retractall: {
        label: 'retractall(Goal)',
        documentation: 'Remove all clauses matching Goal',
        parameters: [{ label: 'Goal', documentation: 'Pattern to match for retraction' }],
      },
      write: {
        label: 'write(Term)',
        documentation: 'Write term to output',
        parameters: [{ label: 'Term', documentation: 'Term to write' }],
      },
      writeln: {
        label: 'writeln(Term)',
        documentation: 'Write term followed by newline',
        parameters: [{ label: 'Term', documentation: 'Term to write' }],
      },
      is: {
        label: 'is(Result, Expression)',
        documentation: 'Arithmetic evaluation',
        parameters: [
          { label: 'Result', documentation: 'Variable to unify with result' },
          { label: 'Expression', documentation: 'Arithmetic expression to evaluate' },
        ],
      },
      var: {
        label: 'var(Term)',
        documentation: 'True if Term is an unbound variable',
        parameters: [{ label: 'Term', documentation: 'Term to test' }],
      },
      nonvar: {
        label: 'nonvar(Term)',
        documentation: 'True if Term is not an unbound variable',
        parameters: [{ label: 'Term', documentation: 'Term to test' }],
      },
      atom: {
        label: 'atom(Term)',
        documentation: 'True if Term is an atom',
        parameters: [{ label: 'Term', documentation: 'Term to test' }],
      },
      number: {
        label: 'number(Term)',
        documentation: 'True if Term is a number',
        parameters: [{ label: 'Term', documentation: 'Term to test' }],
      },
      compound: {
        label: 'compound(Term)',
        documentation: 'True if Term is a compound term',
        parameters: [{ label: 'Term', documentation: 'Term to test' }],
      },
      functor: {
        label: 'functor(Term, Functor, Arity)',
        documentation: 'Relate compound term to its functor name and arity',
        parameters: [
          { label: 'Term', documentation: 'The compound term' },
          { label: 'Functor', documentation: 'The functor name' },
          { label: 'Arity', documentation: 'The arity (number of arguments)' },
        ],
      },
      arg: {
        label: 'arg(N, Term, Arg)',
        documentation: 'Extract Nth argument from compound term',
        parameters: [
          { label: 'N', documentation: 'Argument position (1-based)' },
          { label: 'Term', documentation: 'The compound term' },
          { label: 'Arg', documentation: 'The extracted argument' },
        ],
      },
      univ: {
        label: 'univ(Term, List)',
        documentation: 'Convert between term and list representation (=..)',
        parameters: [
          { label: 'Term', documentation: 'The term' },
          { label: 'List', documentation: 'List representation [functor|args]' },
        ],
      },
      call: {
        label: 'call(Goal)',
        documentation: 'Call goal dynamically',
        parameters: [{ label: 'Goal', documentation: 'The goal to call' }],
      },
      once: {
        label: 'once(Goal)',
        documentation: 'Succeed at most once',
        parameters: [{ label: 'Goal', documentation: 'The goal to call once' }],
      },
      forall: {
        label: 'forall(Condition, Action)',
        documentation: 'For all solutions of Condition, Action must succeed',
        parameters: [
          { label: 'Condition', documentation: 'The condition to test' },
          { label: 'Action', documentation: 'Action to execute for each solution' },
        ],
      },
      between: {
        label: 'between(Low, High, Value)',
        documentation: 'Generate integers between bounds',
        parameters: [
          { label: 'Low', documentation: 'Lower bound (inclusive)' },
          { label: 'High', documentation: 'Upper bound (inclusive)' },
          { label: 'Value', documentation: 'Generated integer value' },
        ],
      },
      succ: {
        label: 'succ(Int1, Int2)',
        documentation: 'Successor relation for integers',
        parameters: [
          { label: 'Int1', documentation: 'First integer' },
          { label: 'Int2', documentation: 'Successor integer' },
        ],
      },
    };

    return signatures[predicate] || null;
  }

  // Method to get signature for custom predicates (could be extended to parse from comments)
  private getCustomPredicateSignature(
    predicate: string,
    document: TextDocument
  ): SignatureInformation | null {
    const text = document.getText();
    const lines = text.split('\n');
    // Look for predicate definition with comment above it
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      if (typeof line !== 'string') continue;
      const predicatePattern = new RegExp(`^\\s*${predicate}\\s*\\(`);
      if (predicatePattern.test(line)) {
        // Check previous lines for documentation comment
        let docComment = '';
        let j = i - 1;
        while (j >= 0 && typeof lines[j] === 'string' && lines[j]?.trim()?.startsWith('%')) {
          docComment = (lines[j]?.trim().substring(1).trim() ?? '') + '\n' + docComment;
          j--;
        }
        if (docComment) {
          // Parse the line to extract parameters
          const match = line.match(/\(([^)]*)\)/);
          const parameters = [];
          if (match && typeof match[1] === 'string' && match[1].trim()) {
            const args = match[1].split(',');
            for (let k = 0; k < args.length; k++) {
              const arg = args[k]?.trim?.() ?? '';
              parameters.push({
                label: arg,
                documentation: `Parameter ${k + 1}`,
              });
            }
          }
          return {
            label: line.trim(),
            documentation: docComment.trim(),
            parameters,
          };
        }
      }
    }
    return null;
  }
}
