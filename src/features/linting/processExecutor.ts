import * as path from 'path';
import jsesc from 'jsesc';
import { spawn } from 'process-promises';
import { TextDocument, workspace } from 'vscode';
import { Utils } from '../../utils/utils';
import { PlatformUtils } from '../../utils/platformUtils';
import { IProcessExecutor, IProcessResult, ILinterConfiguration, RunTrigger } from './interfaces';

/**
 * Handles execution of Prolog processes with dialect-specific configurations
 */
export class ProcessExecutor implements IProcessExecutor {
  private context: any; // Extension context

  constructor(context: any) {
    this.context = context;
  }

  /**
   * Execute Prolog process for linting a document
   */
  public async executeProlog(
    document: TextDocument,
    config: ILinterConfiguration
  ): Promise<IProcessResult> {
    // Configure options for executing Prolog
    const options = workspace.workspaceFolders?.[0]?.uri.fsPath
      ? { cwd: workspace.workspaceFolders[0].uri.fsPath }
      : undefined;

    const docTxt = document.getText();
    const docTxtEsced = jsesc(docTxt, { quotes: 'double' });
    const fname = jsesc(PlatformUtils.toAbsolute(document.fileName));

    // Build arguments based on dialect and trigger
    const { args, goals } = this.buildArgumentsForDialect(config.trigger, fname, docTxtEsced);

    return new Promise((resolve, reject) => {
      let stdoutData = '';
      let stderrData = '';

      // Execute the Prolog process
      spawn(config.executable, args, options)
        .on('process', process => {
          // Handle the Prolog process
          if (process.pid) {
            if (config.trigger === RunTrigger.onType && goals) {
              process.stdin.write(goals);
              process.stdin.end();
            }
          }
        })
        .on('stdout', (out: string) => {
          stdoutData += out;
        })
        .on('stderr', (err: string) => {
          stderrData += err;
        })
        .then(_result => {
          resolve({
            stdout: stdoutData,
            stderr: stderrData,
            exitCode: 0,
          });
        })
        .catch(error => {
          if (error.code === 'ENOENT') {
            reject(new Error('PROLOG_EXECUTABLE_NOT_FOUND'));
          } else {
            reject(error);
          }
        });
    });
  }

  /**
   * Build arguments for different Prolog dialects and triggers
   */
  public buildArgumentsForDialect(
    trigger: RunTrigger,
    fileName: string,
    documentText: string
  ): { args: string[]; goals?: string } {
    let args: string[] = [];
    let goals: string = '';

    // Determine Prolog dialect and set arguments accordingly
    switch (Utils.DIALECT) {
      case 'swi': {
        if (trigger === RunTrigger.onSave) {
          args = ['-g', 'halt', fileName];
        }
        if (trigger === RunTrigger.onType) {
          args = ['-q'];
          goals = `
            open_string("${documentText}", S),
            load_files('${fileName}', [stream(S),if(true)]).
            list_undefined.
          `;
        }
        break;
      }
      case 'ecl': {
        const dir = jsesc(path.resolve(`${this.context.extensionPath}/out/src/features`));
        if (trigger === RunTrigger.onSave) {
          const fdir = path.dirname(fileName);
          const file = path.basename(fileName);
          goals = `(cd("${dir}"),
          use_module('load_modules'),
          cd("${fdir}"),
          load_modules_from_file('${file}'),
          compile('${file}', [debug:off]),halt)`;
          args = ['-e', goals];
        }
        if (trigger === RunTrigger.onType) {
          goals = `(cd("${dir}"),
          use_module(load_modules),
          load_modules_from_text("${documentText}"),
          open(string("${documentText}"), read, S),
          compile(stream(S), [debug:off]),
          close(S),halt)`;
          args = ['-e', goals];
        }
        break;
      }
      default:
        break;
    }

    return { args, goals: trigger === RunTrigger.onType ? goals : undefined };
  }

  /**
   * Get execution options for the current workspace
   */
  private getExecutionOptions(): any {
    return workspace.workspaceFolders?.[0]?.uri.fsPath
      ? { cwd: workspace.workspaceFolders[0].uri.fsPath }
      : undefined;
  }

  /**
   * Check if the dialect is supported
   */
  public isDialectSupported(dialect: string): boolean {
    return ['swi', 'ecl'].includes(dialect);
  }

  /**
   * Get supported dialects
   */
  public getSupportedDialects(): string[] {
    return ['swi', 'ecl'];
  }
}