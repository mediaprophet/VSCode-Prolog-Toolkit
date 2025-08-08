import * as fs from 'fs';
import {
  CancellationToken,
  CompletionContext,
  Disposable,
  Position,
  TextDocument,
  window,
  workspace
} from 'vscode';
import { PositionUtils } from '../utils/positionUtils';
import { SnippetUtils } from '../utils/utils';

// Class responsible for updating snippets based on prolog files
export class SnippetUpdater {
  // Update snippets based on new predicates create by the user in the document
  public updateSnippet() {
    // Get the currently active text editor
    const editor = window.activeTextEditor;
    if (!editor) {
      return;
    }

    const doc = editor.document;
    // Update only if the document is a prolog file
    if (doc.languageId === 'prolog') {
      // Retrieve predicates from the document and check against existing snippets
      const predicats = this._getPredicat(doc);
      const already: string[] = [];
      const description: boolean[] = [];
      // Extract existing snippets' names for comparison
      Object.keys(SnippetUtils.snippets ?? {}).forEach(elem => {
        if (elem.includes(':')) {
          if (elem.includes(':-')) {
            already.push(elem.replace(':- ', ''));
          } else {
            const split = elem.split(':');
            if (split.length > 1 && typeof split[1] === 'string') {
              already.push(split[1]);
            }
          }
        } else {
          already.push(elem);
        }
        if (SnippetUtils.snippets?.[elem]?.description && typeof SnippetUtils.snippets[elem].description === 'string' && SnippetUtils.snippets[elem].description.includes('\ncustom predicate\n')) {
          description.push(false);
        } else {
          description.push(true);
        }
      });
      // Update snippets based on new predicates in the document
      predicats.forEach(elem => {
        const num = elem[1].split(',').length;
        const snippetKey = elem[0] + '/' + num.toString();
        if (!already.includes(snippetKey)) {
          if (elem[2] == null) {
            SnippetUtils.snippets![snippetKey] = {
              prefix: elem[0],
              body: [''],
              description:
                elem[0].toString() + '(' + elem[1].toString() + ')\ncustom predicate\n\n',
            };
          } else {
            SnippetUtils.snippets![snippetKey] = {
              prefix: elem[0],
              body: [''],
              description: elem[0].toString() + '(' + elem[1].toString() + ')\n' + elem[2] + '\n',
            };
          }
        } else if (elem[2] != null) {
          const idx = already.indexOf(snippetKey);
          const keyToDelete = Object.keys(SnippetUtils.snippets!)[idx];
          if (keyToDelete !== undefined) {
            delete SnippetUtils.snippets![keyToDelete];
          }
          SnippetUtils.snippets![snippetKey] = {
            prefix: elem[0],
            body: [''],
            description: elem[0].toString() + '(' + elem[1].toString() + ')\n' + elem[2] + '\n',
          };
        }
      });
      // Generate predicate modules based on the updated context
      SnippetUtils.genPredicateModules();
    }
  }

  // Extracts predicates from the given TextDocument
  public _getPredicat(doc: TextDocument) {
    const docContent = doc.getText(); // Get the content of the document
    const regexp = /(^\s*)([a-z][a-zA-Z0-9_]*)\(([a-zA-Z0-9_\-, ]*)\)(?=.*(:-|=>|-->).*)/gm; // Regular expression for matching Prolog predicates
    const regexpModule = /^\s*:-\s*use_module\(([a-z][a-zA-Z0-9_/]*)\s*(,|\)\s*\.)/gm; // Regular expression for matching Prolog use_module directives
    const regexpComment = /^\s*(%(?!!)|%!|\*(?!\/)|\/\*\*)(\s*)(.*)/gm; // Regular expression for matching Prolog comments
    const arrayModule = [...docContent.matchAll(regexpModule)]; // Extract all use_module directives from the document
    const prolog = doc.fileName.split('.')[1]; // Get the Prolog extension from the document's file name
    let predicats: any[] = [];

    // Loop through each use_module directive
    for (let i = 0; i < arrayModule.length; i++) {
      // Read the content of the referenced module
      let text = '';
      try {
        const wsFolders = workspace.workspaceFolders;
        const moduleArr = arrayModule[i];
        const moduleName = Array.isArray(moduleArr) && moduleArr.length > 1 ? moduleArr[1] : undefined;
        if (wsFolders && wsFolders.length > 0 && wsFolders[0]?.uri?.fsPath && typeof moduleName === 'string') {
          text = fs.readFileSync(
            wsFolders[0].uri.fsPath + '/' + moduleName + '.' + prolog,
            'utf8'
          );
        }
      } catch (error) {
        console.error('Error reading file:', error);
      }

      // Extract predicates from the referenced module's content
      const array2 = [...text.matchAll(regexp)];
      array2.forEach(elem => {
        if (!elem || typeof elem.index !== 'number' || typeof elem[1] !== 'string') return;
        const pos = PositionUtils.findLineColForByte(text, elem.index + elem[1].length);
        let nbline = pos ? pos.line - 1 : -1;
        const lines = text.split(/\n|\r/);
        let verif = true;
        let comment: string | null = '';
        while (verif && nbline > -1) {
          const res = lines[nbline]?.matchAll(regexpComment).next();
          if (res && res.value) {
            if (res.value[1] !== '*/' && res.value[1] !== '/**') {
              if (comment === '' && typeof res.value[3] === 'string') {
                comment = res.value[3];
              } else if (res.value[3]) {
                comment = res.value[3] + '\n' + comment;
              }
            }
            if (res.value[1] === '%!' || res.value[1] === '/**') {
              verif = false;
            }
            nbline = nbline - 1;
          } else {
            comment = null;
            verif = false;
          }
        }
        predicats.push([elem[2], elem[3], comment ?? '']);
      });
    }
    // Extract predicates from the current document
    const array = [...docContent.matchAll(regexp)];
    // Search for definition comments
    array.forEach(elem => {
      if (!elem || typeof elem.index !== 'number' || typeof elem[1] !== 'string') return;
      const pos = PositionUtils.findLineColForByte(docContent, elem.index + elem[1].length);
      let nbline = pos ? pos.line - 1 : -1;
      const lines = docContent.split('\n');
      let verif = true;
      let comment: string | null = '';
      while (verif && nbline > -1) {
        const res = lines[nbline]?.matchAll(regexpComment).next();
        if (res && res.value) {
          if (res.value[1] !== '*/' && res.value[1] !== '/**') {
            if (comment === '' && typeof res.value[3] === 'string') {
              comment = res.value[3];
            } else if (res.value[3]) {
              comment = res.value[3] + '\n' + comment;
            }
          }
          if (res.value[1] === '%!' || res.value[1] === '/**') {
            verif = false;
          }
          nbline = nbline - 1;
        } else {
          comment = null;
          verif = false;
        }
      }
      predicats.push([elem[2], elem[3], comment ?? '']);
    });
    // Filter out a specific predicate named "test"
    predicats = predicats.filter(function (predicat) {
      return predicat[0] != 'test';
    });
    return predicats;
  }
  dispose() {
    // No resources to dispose
  }
}

// Class responsible for managing the SnippetUpdater and subscribing to relevant events
export class SnippetUpdaterController {
  private snippetUpdater: SnippetUpdater;
  private _disposable: Disposable;

  constructor(snippetUpdater: SnippetUpdater) {
    this.snippetUpdater = snippetUpdater;
    this.snippetUpdater.updateSnippet(); // Update snippets initially

    // subscribe to selection change and editor activation events
    const subscriptions: Disposable[] = [];
    workspace.onDidSaveTextDocument(this._onEvent, this, subscriptions);
    window.onDidChangeActiveTextEditor(this._onEvent, this, subscriptions);

    // update the counter for the current file
    this.snippetUpdater.updateSnippet();

    // create a combined disposable from both event subscriptions
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
    document: TextDocument,
    position: Position,
    token: CancellationToken,
    context: CompletionContext
  ) {
    // Array to store completion items
    const snippetCompletion: any[] = [];
    // Iterate through new snippets and create completion items
    // Removed legacy Utils.newsnippets usage (no longer present in utils)
    return snippetCompletion; // Return the array of completion items
  }
}
