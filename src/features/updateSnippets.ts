import * as fs from 'fs';
import type { CancellationToken, CompletionContext, Position, TextDocument } from 'vscode';
import {
  CompletionItem,
  CompletionItemKind,
  Disposable,
  MarkdownString,
  SnippetString,
  window,
  workspace,
} from 'vscode';
import { Utils } from '../utils/utils.js';

export class SnippetUpdater {
  public updateSnippet() {
    const editor = window.activeTextEditor;
    if (!editor) {
      return;
    }
    const doc = editor.document;
    if (doc.languageId === 'prolog') {
      const predicats = this._getPredicat(doc);
      const already: string[] = [];
      const description: boolean[] = [];
      if (Utils.snippets) {
        Object.keys(Utils.snippets as object).forEach(elem => {
          if (elem.includes(':')) {
            if (elem.includes(':-')) {
              already.push(elem.replace(':- ', ''));
            } else {
              const part = elem.split(':')[1];
              if (part !== undefined) {
                already.push(part);
              }
            }
          } else {
            already.push(elem);
          }
          if (
            Utils.snippets &&
            Utils.snippets[elem]?.description?.includes('\ncustom predicate\n')
          ) {
            description.push(false);
          } else {
            description.push(true);
          }
        });
        predicats.forEach((elem: [string, string, string?]) => {
          const num = elem[1]?.split(',').length ?? 0;
          const key = elem[0] + '/' + num.toString();
          if (!already.includes(key)) {
            if (elem[2] == null) {
              if (Utils.snippets) {
                Utils.snippets[key] = {
                  prefix: elem[0] ?? '',
                  body: [''],
                  description:
                    (elem[0]?.toString() ?? '') +
                    '(' +
                    (elem[1]?.toString() ?? '') +
                    ')\ncustom predicate\n\n',
                };
              }
            } else {
              if (Utils.snippets) {
                Utils.snippets[key] = {
                  prefix: elem[0] ?? '',
                  body: [''],
                  description:
                    (elem[0]?.toString() ?? '') +
                    '(' +
                    (elem[1]?.toString() ?? '') +
                    ')\n' +
                    (elem[2] ?? '') +
                    '\n',
                };
              }
            }
            Utils.newsnippets.push(elem);
          } else if (elem[2] != null) {
            if (Utils.snippets) {
              const idx = already.indexOf(key);
              const keys = Object.keys(Utils.snippets as object);
              const delKey = keys[idx];
              if (delKey !== undefined) {
                delete Utils.snippets[delKey];
              }
              Utils.snippets[key] = {
                prefix: elem[0] ?? '',
                body: [''],
                description:
                  (elem[0]?.toString() ?? '') +
                  '(' +
                  (elem[1]?.toString() ?? '') +
                  ')\n' +
                  (elem[2] ?? '') +
                  '\n',
              };
            }
            for (let i = 0; i < Utils.newsnippets.length; i++) {
              if (Utils.newsnippets[i][0] == elem[0] && Utils.newsnippets[i][1] == elem[1]) {
                Utils.newsnippets.splice(i, 1);
                Utils.newsnippets.push(elem);
                break;
              }
            }
          }
        });
        if (Utils.CONTEXT) {
          Utils.genPredicateModules(Utils.CONTEXT);
        }
      }
    }
  }
  private _getPredicat(doc: TextDocument) {
    const docContent = doc.getText();
    const regexp = /(^\s*)([a-z][a-zA-Z0-9_]*)\(([a-zA-Z0-9_\-, ]*)\)(?=.*(:-|=>|-->).*)/gm;
    const regexpModule = /^\s*:-\s*use_module\(([a-z][a-zA-Z0-9_/]*)\s*(,|\)\s*\.)/gm;
    const regexpComment = /^\s*(%(?!!)|%!|\*(?!\/)|\/\*\*)(\s*)(.*)/gm;
    const arrayModule = [...docContent.matchAll(regexpModule)];
    const prolog = doc.fileName.split('.')[1];
    let predicats: [string, string, string?][] = [];
    for (let i = 0; i < arrayModule.length; i++) {
      let text = '';
      try {
        let wsPath = '';
        if (workspace.workspaceFolders && workspace.workspaceFolders[0]) {
          wsPath = workspace.workspaceFolders[0].uri.fsPath;
        }
        const mod = arrayModule[i];
        if (mod !== undefined && Array.isArray(mod) && typeof mod[1] === 'string') {
          text = fs.readFileSync(wsPath + '/' + mod[1] + '.' + prolog, 'utf8');
        }
      } catch (error) {
        console.error('Error reading file:', error);
      }
      const array2 = [...text.matchAll(regexp)];
      array2.forEach(elem => {
        const idx =
          typeof elem.index === 'number' && typeof elem[1] === 'string'
            ? elem.index + elem[1].length
            : 0;
        const lineCol = Utils.findLineColForByte(text, idx);
        let nbline = (lineCol?.line ?? 1) - 1;
        const lines = text.split(/\n|\r/);
        let verif = true;
        let comment = '';
        while (verif && nbline > -1) {
          const res = lines[nbline]?.matchAll(regexpComment).next();
          if (res && res.value) {
            if (res.value[1] !== '*/' && res.value[1] !== '/**') {
              if (comment === '') {
                comment = res.value[3] ?? '';
              } else if (res.value[3] && res.value[3] !== '') {
                comment = res.value[3] + '\n' + comment;
              }
            }
            if (res.value[1] === '%!' || res.value[1] === '/**') {
              verif = false;
            }
            nbline = nbline - 1;
          } else {
            comment = '';
            verif = false;
          }
        }
        predicats.push([
          typeof elem[2] === 'string' ? elem[2] : '',
          typeof elem[3] === 'string' ? elem[3] : '',
          comment,
        ]);
      });
    }
    const array = [...docContent.matchAll(regexp)];
    array.forEach(elem => {
      const idx =
        typeof elem.index === 'number' && typeof elem[1] === 'string'
          ? elem.index + elem[1].length
          : 0;
      const lineCol = Utils.findLineColForByte(docContent, idx);
      let nbline = (lineCol?.line ?? 1) - 1;
      const lines = docContent.split('\n');
      let verif = true;
      let comment = '';
      while (verif && nbline > -1) {
        const res = lines[nbline]?.matchAll(regexpComment).next();
        if (res && res.value) {
          if (res.value[1] !== '*/' && res.value[1] !== '/**') {
            if (comment === '') {
              comment = res.value[3] ?? '';
            } else if (res.value[3] && res.value[3] !== '') {
              comment = res.value[3] + '\n' + comment;
            }
          }
          if (res.value[1] === '%!' || res.value[1] === '/**') {
            verif = false;
          }
          nbline = nbline - 1;
        } else {
          comment = '';
          verif = false;
        }
      }
      predicats.push([
        typeof elem[2] === 'string' ? elem[2] : '',
        typeof elem[3] === 'string' ? elem[3] : '',
        comment,
      ]);
    });
    predicats = predicats.filter(function (predicat) {
      return predicat[0] != 'test';
    });
    return predicats;
  }

  public dispose() {
    // No resources to dispose
  }
}

export class SnippetUpdaterController {
  private snippetUpdater: SnippetUpdater;
  private _disposable: Disposable;

  constructor(snippetUpdater: SnippetUpdater) {
    this.snippetUpdater = snippetUpdater;
    this.snippetUpdater.updateSnippet();
    const subscriptions: Disposable[] = [];
    workspace.onDidSaveTextDocument(this._onEvent, this, subscriptions);
    window.onDidChangeActiveTextEditor(this._onEvent, this, subscriptions);
    this.snippetUpdater.updateSnippet();
    this._disposable = Disposable.from(...subscriptions);
  }

  dispose() {
    this._disposable.dispose();
  }

  private _onEvent() {
    this.snippetUpdater.updateSnippet();
  }
}

export class PrologCompletionProvider {
  // Provides completion items for Prolog code (auto completion)
  public provideCompletionItems(
    _document: TextDocument,
    _position: Position,
    _token: CancellationToken,
    _context: CompletionContext
  ) {
    // Array to store completion items
    const snippetCompletion: CompletionItem[] = [];
    // Iterate through new snippets and create completion items
    Utils.newsnippets.forEach((elem: [string, string, string?]) => {
      const params = elem[1].split(','); // Split parameters of the snippet
      const completionItem = new CompletionItem(
        elem[0] + '/' + params.length,
        CompletionItemKind.Function
      ); // Create a new CompletionItem for each snippet
      // Construct the snippet text with placeholders for parameters
      let str = elem[0].toString() + '(';
      let str2 = '';
      for (let i = 0; i < params.length; i++) {
        str = str + '${' + (i + 2).toString() + ':' + params[i] + '}';
        str2 = str2 + '<span style="color:#ff7878;">' + params[i] + '</span>';
        if (i != params.length - 1) {
          str = str + ',';
          str2 = str2 + ',';
        }
      }
      str = str + ')$0';
      // Set the insert text for the completion item as a SnippetString
      completionItem.insertText = new SnippetString(str);
      // Set documentation for the completion item
      const docs: any = new MarkdownString();
      docs.supportHtml = true;
      if (elem[2] == null) {
        docs.appendMarkdown(
          '<span style="color:#8da9fc;">' +
            elem[0].toString() +
            '</span>(' +
            str2 +
            ')</br>Custom predicate'
        );
      } else {
        docs.appendMarkdown(
          '<span style="color:#8da9fc;">' +
            elem[0].toString() +
            '</span>(' +
            str2 +
            ')</br>' +
            elem[2].replace('\n', '</br>')
        );
      }
      completionItem.documentation = docs;
      completionItem.detail = elem[0] + '/' + params.length; // Set additional details for the completion item
      snippetCompletion.push(completionItem); // Add the completion item to the array
    });
    return snippetCompletion; // Return the array of completion items
  }
}
