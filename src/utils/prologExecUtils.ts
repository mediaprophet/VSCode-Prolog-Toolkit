// Prolog execution helpers extracted from utils.ts
import * as cp from 'child_process';
import jsesc from 'jsesc';
import { workspace } from 'vscode';

export class PrologExecUtils {
  static DIALECT: string | null = null;
  static RUNTIMEPATH: string | null = null;

  static execPrologSync(
    args: string[],
    clause: string,
    call: string,
    inputTerm: string,
    resultReg: RegExp
  ): string[] | null {
    const plCode = jsesc(clause, { quotes: 'double' });
    let input: string,
      prologProcess: cp.SpawnSyncReturns<string | Buffer>,
      runOptions: cp.SpawnSyncOptions;
    switch (this.DIALECT) {
      case 'swi': {
        input = `\n          open_string(\"${plCode}\", Stream), \n          load_files(runprolog, [stream(Stream)]).\n          ${call}. \n          ${inputTerm}.\n          halt.\n        `;
        runOptions = {
          cwd: workspace.workspaceFolders?.[0]?.uri.fsPath || process.cwd(),
          encoding: 'utf8',
          input: input,
        };
        prologProcess = cp.spawnSync(this.RUNTIMEPATH || '', args, runOptions);
        break;
      }
      case 'ecl': {
        input = `${inputTerm}.`;
        args = args.concat([
          '-e',
          `open(string(\"${plCode}\n\"), read, S),compile(stream(S)),close(S),call(${call}).`,
        ]);
        runOptions = {
          cwd: workspace.workspaceFolders?.[0]?.uri.fsPath || process.cwd(),
          encoding: 'utf8',
          input: input,
        };
        prologProcess = cp.spawnSync(this.RUNTIMEPATH || '', args, runOptions);
        break;
      }
      default:
        return null;
    }
    if (prologProcess && prologProcess.status === 0) {
      const output = prologProcess.stdout ? prologProcess.stdout.toString() : '';
      const err = prologProcess.stderr ? prologProcess.stderr.toString() : '';
      if (err.trim()) {
        console.debug('[PrologExecUtils] Prolog process stderr:', err);
      }
      const match = output.match(resultReg);
      return match ? match : null;
    } else {
      console.log(
        'PrologExecSyncError: ' +
        (prologProcess && prologProcess.stderr
          ? prologProcess.stderr.toString()
          : 'Unknown error')
      );
      return null;
    }
  }
}
