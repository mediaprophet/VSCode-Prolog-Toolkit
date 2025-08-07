import { TextDocument } from 'vscode-languageserver-textdocument';
import type { BackendResponse, HelpDocumentation, HoverProvider, LSPContext } from './types.js';
type Hover = any;
enum MarkupKind {
  PlainText = 'plaintext',
  Markdown = 'markdown',
}
type Position = { line: number; character: number };

export class PrologHoverProvider implements HoverProvider {
  async provideHover(
    document: TextDocument,
    position: Position,
    _context: LSPContext
  ): Promise<Hover | null> {
    const text = document.getText();
    const word = this.getWordAtPosition(text, position);

    if (!word) {
      return null;
    }

    // Try to get help from backend
    if (_context.prologBackend?.isRunning()) {
      try {
        const response: BackendResponse = await _context.prologBackend.sendRequest('help', {
          predicate: word,
          timeoutMs: 3000,
        });

        if (response.status === 'ok' && response.doc) {
          const doc = response.doc;
          const markdown = this.formatHelpAsMarkdown(doc);

          return {
            contents: {
              kind: MarkupKind.Markdown,
              value: markdown,
            },
          };
        }
      } catch (error) {
        // Fall back to static help
      }
    }

    // Static help for common predicates
    const staticHelp = this.getStaticHelp(word);
    if (staticHelp) {
      return {
        contents: {
          kind: MarkupKind.Markdown,
          value: staticHelp,
        },
      };
    }

    return null;
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
    while (start > 0 && /[a-zA-Z0-9_]/.test(line[start - 1] ?? '')) {
      start--;
    }
    while (end < line.length && /[a-zA-Z0-9_]/.test(line[end] ?? '')) {
      end++;
    }
    return start < end ? line.substring(start, end) : null;
  }

  private formatHelpAsMarkdown(doc: HelpDocumentation): string {
    let markdown = `# ${doc.name}/${doc.arity}\n\n`;

    if (doc.summary) {
      markdown += `${doc.summary}\n\n`;
    }

    if (doc.args && doc.args.length > 0) {
      markdown += '## Arguments\n\n';
      doc.args.forEach((arg: any, index: number) => {
        markdown += `**${index + 1}. ${arg.name}** - ${arg.description}\n\n`;
      });
    }

    if (doc.examples && doc.examples.length > 0) {
      markdown += '## Examples\n\n';
      doc.examples.forEach((example: string) => {
        markdown += `\`\`\`prolog\n${example}\n\`\`\`\n\n`;
      });
    }

    return markdown;
  }

  private getStaticHelp(word: string): string | null {
    const staticHelp: Record<string, string> = {
      member:
        '# member/2\n\nTrue if Elem is a member of List.\n\n```prolog\nmember(X, [1,2,3]).\n```',
      append:
        '# append/3\n\nTrue if List3 is the concatenation of List1 and List2.\n\n```prolog\nappend([1,2], [3,4], X).\n```',
      length:
        '# length/2\n\nTrue if Length is the length of List.\n\n```prolog\nlength([1,2,3], X).\n```',
      findall:
        '# findall/3\n\nFind all solutions to Goal.\n\n```prolog\nfindall(X, member(X, [1,2,3]), L).\n```',
      reverse:
        '# reverse/2\n\nTrue if List2 is the reverse of List1.\n\n```prolog\nreverse([1,2,3], X).\n```',
      sort: '# sort/2\n\nTrue if Sorted is the sorted version of List.\n\n```prolog\nsort([3,1,2], X).\n```',
      bagof:
        '# bagof/3\n\nCollect solutions to Goal.\n\n```prolog\nbagof(X, member(X, [1,2,3]), L).\n```',
      setof:
        '# setof/3\n\nCollect unique solutions to Goal.\n\n```prolog\nsetof(X, member(X, [1,2,1]), L).\n```',
      assert: '# assert/1\n\nAdd clause to database.\n\n```prolog\nassert(fact(a)).\n```',
      retract: '# retract/1\n\nRemove clause from database.\n\n```prolog\nretract(fact(X)).\n```',
      write: '# write/1\n\nWrite term to output.\n\n```prolog\nwrite(hello).\n```',
      writeln: '# writeln/1\n\nWrite term followed by newline.\n\n```prolog\nwriteln(hello).\n```',
      nl: '# nl/0\n\nWrite newline to output.\n\n```prolog\nnl.\n```',
      is: '# is/2\n\nArithmetic evaluation.\n\n```prolog\nX is 2 + 3.\n```',
      var: '# var/1\n\nTrue if argument is unbound variable.\n\n```prolog\nvar(X).\n```',
      nonvar:
        '# nonvar/1\n\nTrue if argument is not unbound variable.\n\n```prolog\nnonvar(hello).\n```',
      atom: '# atom/1\n\nTrue if argument is an atom.\n\n```prolog\natom(hello).\n```',
      number: '# number/1\n\nTrue if argument is a number.\n\n```prolog\nnumber(42).\n```',
      compound:
        '# compound/1\n\nTrue if argument is a compound term.\n\n```prolog\ncompound(f(a)).\n```',
      functor:
        '# functor/3\n\nRelate compound term to functor name and arity.\n\n```prolog\nfunctor(f(a,b), F, A).\n```',
      arg: '# arg/3\n\nExtract argument from compound term.\n\n```prolog\narg(1, f(a,b), X).\n```',
      univ: '# univ/2\n\nConvert between term and list representation.\n\n```prolog\nf(a,b) =.. L.\n```',
      call: '# call/1\n\nCall goal dynamically.\n\n```prolog\ncall(member(X, [1,2,3])).\n```',
      once: '# once/1\n\nSucceed at most once.\n\n```prolog\nonce(member(X, [1,2,3])).\n```',
      forall:
        '# forall/2\n\nFor all solutions of Condition, Action must succeed.\n\n```prolog\nforall(member(X, [1,2,3]), write(X)).\n```',
      between:
        '# between/3\n\nGenerate integers between bounds.\n\n```prolog\nbetween(1, 5, X).\n```',
      succ: '# succ/2\n\nSuccessor relation for integers.\n\n```prolog\nsucc(X, 5).\n```',
      true: '# true/0\n\nAlways succeeds.\n\n```prolog\ntrue.\n```',
      fail: '# fail/0\n\nAlways fails.\n\n```prolog\nfail.\n```',
    };

    return staticHelp[word] || null;
  }
}
