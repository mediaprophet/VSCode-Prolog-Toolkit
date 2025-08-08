import * as cp from 'child_process';
import jsesc from 'jsesc';
import * as path from 'path';
import {
  CancellationToken,
  DefinitionProvider,
  Location,
  Position,
  TextDocument,
  Uri,
  workspace,
} from 'vscode';
import { PrologExecUtils, SnippetUtils } from '../utils/utils';
export class PrologDefinitionProvider implements DefinitionProvider {
  // Implement the provideDefinition method required by DefinitionProvider interface
  public provideDefinition(
    doc: TextDocument,
    position: Position,
    _token: CancellationToken
  ): Location | undefined | Promise<Location | undefined> {
    let location: Location | undefined = undefined;
    const pred = SnippetUtils.getPredicateUnderCursor(doc, position); // Get the predicate under the cursor using utility function
    // Return early if no predicate is found
    if (!pred) {
      return undefined;
    }
    // Initialize variables for Prolog execution
    // const _exec = Utils.RUNTIMEPATH;
    let args: string[] = [],
      prologCode: string,
      result: string[] = [],
      predToFind: string,
      runOptions: cp.SpawnSyncOptions;
    const fileLineRe = /File:(.+);Line:(\d+)/;
    // Switch based on the Prolog dialect (e.g., "swi" or "ecl")
    switch (PrologExecUtils.DIALECT) {
      case 'swi':
        // Construct a predicate with void arguments (e.g., pred_void = pred.functor(_,_,_,...,_) )
        var pred_void = pred.functor + '(';
        for (let i = 0; i < pred.arity; i++) {
          pred_void = pred_void + '_';
          if (i < pred.arity - 1) {
            pred_void = pred_void + ',';
          }
        }
        pred_void = pred_void + ')';
        args = ['-q', doc.fileName];
        prologCode = `
        source_location:-
          predicate_property(${pred_void}, file(File)),
          predicate_property(${pred_void}, line_count(Line)),
          format("File:~s;Line:~d~n", [File, Line]).
          `;
        // Check if the document is dirty (unsaved) and save it if needed
        if (doc.isDirty) {
          doc.save().then(_ => {
            result = PrologExecUtils.execPrologSync(args, prologCode, 'source_location', '', fileLineRe);
          });
        } else {
          result = PrologExecUtils.execPrologSync(args, prologCode, 'source_location', '', fileLineRe);
        }
        break;

      case 'ecl': {
        args = [];
        const lc = path.resolve(`${__dirname}/locate_clause`);
        const piParts = pred.pi.split(':');
        predToFind = piParts.length > 1 ? piParts[1]! : pred.pi;
        if (!predToFind) {
          return undefined;
        }

        if (!workspace.workspaceFolders || workspace.workspaceFolders.length === 0) {
          return undefined;
        }

        prologCode = `ensure_loaded(['${lc}']),
          source_location('${jsesc(doc.fileName)}', ${predToFind}).
          `;
        runOptions = {
          cwd: workspace.workspaceFolders[0]!.uri.fsPath,
          encoding: 'utf8',
          input: prologCode,
        };
        // Check if the document is dirty (unsaved) and save it if needed
        if (!PrologExecUtils.RUNTIMEPATH) {
          return undefined;
        }

        if (doc.isDirty) {
          doc.save().then(_ => {
            const syncPro = cp.spawnSync(PrologExecUtils.RUNTIMEPATH!, args, runOptions);
            if (syncPro.status === 0) {
              const matchResult = syncPro.stdout.toString().match(fileLineRe);
              result = matchResult ? Array.from(matchResult) : [];
            }
          });
        } else {
          const syncPro = cp.spawnSync(PrologExecUtils.RUNTIMEPATH, args, runOptions);
          if (syncPro.status === 0) {
            const matchResult = syncPro.stdout.toString().match(fileLineRe);
            result = matchResult ? Array.from(matchResult) : [];
          }
        }
        break;
      }

      default:
        // Handle other Prolog dialects if needed
        break;
    }
    // If result is obtained, create a Location object with file and line information
    if (result && result.length >= 3 && result[1] && result[2]) {
      const fileName: string = result[1];
      const lineNum: number = parseInt(result[2]);
      location = new Location(Uri.file(fileName), new Position(lineNum - 1, 0));
    }

    return location; // Return the obtained location
  }
}
