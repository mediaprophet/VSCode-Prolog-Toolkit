import { Position, TextDocument } from 'vscode';
// Snippet and predicate utilities extracted from utils.ts
import * as fs from 'fs';
import { ExtensionContext } from 'vscode';

export interface ISnippet {
  [predIndicator: string]: {
    prefix: string;
    body: string[];
    description: string;
  };
}
export interface IPredModule {
  [predicate: string]: string[];
}
export interface IPredicate {
  wholePred: string;
  pi: string;
  functor: string;
  arity: number;
  params: string;
  module: string;
}

export class SnippetUtils {
  /**
   * Extracts the predicate under the cursor from a VSCode TextDocument and Position.
   * Returns an IPredicate or null if not found.
   */
  static getPredicateUnderCursor(doc: TextDocument, position: Position): IPredicate | null {
    const wordRange = doc.getWordRangeAtPosition(position, /[a-zA-Z0-9_:\/-]+/);
    if (!wordRange) return null;
    const word = doc.getText(wordRange);
    // Try to extract arity from the text after the word (e.g., foo/2)
    const line = doc.lineAt(position.line).text;
    const afterWord = line.slice(wordRange.end.character);
    let arity = 0;
    const arityMatch = afterWord.match(/^\s*\/(\d+)/);
    if (arityMatch && typeof arityMatch[1] === 'string') {
      arity = parseInt(arityMatch[1], 10);
    } else {
      // Fallback: count commas in params if present in line
      const paramsMatch = line.match(/\(([^)]*)\)/);
      if (paramsMatch && typeof paramsMatch[1] === 'string') {
        arity = paramsMatch[1].split(',').filter((s: string) => s.trim().length > 0).length;
      }
    }
    // Try to extract module if present (mod:pred)
    let mod = '';
    let functor = word;
    let pi = word;
    if (word.includes(':')) {
      const parts = word.split(':');
      mod = parts[0] ?? '';
      functor = parts[1] ?? '';
      pi = word;
    }
    return {
      wholePred: word,
      pi,
      functor,
      arity,
      params: '',
      module: mod,
    };
  }
  static snippets: ISnippet | null = null;
  static predModules: IPredModule | null = null;

  static loadSnippets(context: ExtensionContext) {
    if (this.snippets) return;
    const snippetsPath = context.extensionPath + '/snippets/prolog.json';
    try {
      // For very large files, use a stream and incremental JSON parse if needed
      const stats = fs.statSync(snippetsPath);
      if (stats.size > 2 * 1024 * 1024) { // >2MB, stream and parse
        let data = '';
        const stream = fs.createReadStream(snippetsPath, { encoding: 'utf8' });
        stream.on('data', chunk => { data += chunk; });
        stream.on('end', () => {
          try {
            this.snippets = JSON.parse(data);
            console.log(`[SnippetUtils] Loaded large snippets file (${stats.size} bytes)`);
          } catch (err) {
            console.error('[SnippetUtils] Error parsing large snippets file:', err);
            this.snippets = {};
          }
        });
        stream.on('error', err => {
          console.error('[SnippetUtils] Error reading large snippets file:', err);
          this.snippets = {};
        });
      } else {
        const snippets = fs.readFileSync(snippetsPath, 'utf8').toString();
        this.snippets = JSON.parse(snippets);
        console.log(`[SnippetUtils] Loaded snippets file (${stats.size} bytes)`);
      }
    } catch (err) {
      console.error('[SnippetUtils] Error loading snippets:', err);
      this.snippets = {};
    }
  }

  static genPredicateModules() {
    this.predModules = {};
    if (!this.snippets) return;
    for (const p in this.snippets) {
      if (p.indexOf(':') > 0) {
        const [mod, pred] = p.split(':');
        if (pred && typeof mod === 'string' && mod.length > 0) {
          if (Object.prototype.hasOwnProperty.call(this.predModules, pred) && this.predModules[pred]) {
            this.predModules[pred] = this.predModules[pred]!.concat(mod);
          } else {
            this.predModules[pred] = [mod];
          }
        }
      }
    }
  }

  static getPredDescriptions(pred: string): string {
    if (this.snippets?.[pred]) return this.snippets[pred].description;
    return '';
  }

  static getPredModules(pred1: string): string[] {
    const pred = pred1.indexOf(':') > -1 ? pred1.split(':')[1] : pred1;
    if (!this.predModules || !pred) return [];
    return Object.prototype.hasOwnProperty.call(this.predModules, pred) && this.predModules[pred] ? this.predModules[pred]! : [];
  }

  static getBuiltinNames(): string[] {
    let builtins: string[] = Object.getOwnPropertyNames(this.snippets || {});
    builtins = builtins.filter(name => !/:/.test(name) && /\//.test(name));
    builtins = builtins.map(name => name.match(/(.+)\//)?.[1] || name);
    builtins = builtins.filter((item, index, original) => !/\W/.test(item) && original.indexOf(item) == index);
    return builtins;
  }
}
