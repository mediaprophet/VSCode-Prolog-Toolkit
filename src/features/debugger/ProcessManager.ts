
import { EventEmitter } from 'events';
import { spawn, SpawnOptions } from 'process-promises';
import { PlatformUtils } from '../../utils/platformUtils';

export interface PrologProcessOptions {
  runtimeExecutable?: string;
  runtimeArgs?: string[];
  cwd: string;
  env?: { [key: string]: string };
  timeoutMs?: number; // Optional process start timeout
  maxRetries?: number; // Optional retry count
}

export interface PrologProcessEvents {
  stdout: (data: string) => void;
  stderr: (data: string) => void;
  exit: (code?: number) => void;
  error: (err: Error) => void;
  process: (proc: any) => void;
}

export class ProcessManager extends EventEmitter {

  private _prologProc: any = null;
  private _options: PrologProcessOptions;
  private _retryCount = 0;

  constructor(options: PrologProcessOptions) {
    super();
    this._options = options;
  }

  public async start(): Promise<void> {
    this.kill();
    const maxRetries = this._options.maxRetries ?? 2;
    const timeoutMs = this._options.timeoutMs ?? 10000;
    let lastError: Error | null = null;
    for (this._retryCount = 0; this._retryCount <= maxRetries; this._retryCount++) {
      try {
        const exec = PlatformUtils.normalizePath(this._options.runtimeExecutable || 'swipl');
        const args = (this._options.runtimeArgs || []).concat('-q');
        const spawnOpts: SpawnOptions = {
          cwd: PlatformUtils.normalizePath(this._options.cwd),
          env: this._options.env || process.env,
        };
        let timeoutHandle: NodeJS.Timeout | undefined;
        const procPromise = new Promise<void>((resolve, reject) => {
          let started = false;
          spawn(exec, args, spawnOpts)
            .on('process', (proc: any) => {
              this._prologProc = proc;
              this.emit('process', proc);
              started = true;
              console.log(`[ProcessManager] Prolog process started (pid: ${proc.pid})`);
              resolve();
            })
            .on('stdout', (data: string) => {
              this.emit('stdout', data);
              console.log(`[ProcessManager] stdout: ${data}`);
            })
            .on('stderr', (err: string) => {
              this.emit('stderr', err);
              console.error(`[ProcessManager] stderr: ${err}`);
            })
            .on('exit', (code: number) => {
              this.emit('exit', code);
              console.log(`[ProcessManager] Prolog process exited with code: ${code}`);
            })
            .then(() => { })
            .catch((error: unknown) => {
              const errObj = error instanceof Error ? error : new Error(String(error));
              this.emit('error', errObj);
              console.error('[ProcessManager] Error starting process:', errObj);
              reject(errObj);
            });
          timeoutHandle = setTimeout(() => {
            if (!started) {
              const err = new Error(`[ProcessManager] Prolog process start timed out after ${timeoutMs}ms`);
              this.emit('error', err);
              console.error(err);
              reject(err);
            }
          }, timeoutMs);
        });
        await procPromise;
        if (timeoutHandle) clearTimeout(timeoutHandle);
        return;
      } catch (err: any) {
        lastError = err instanceof Error ? err : new Error(String(err));
        if (this._retryCount < maxRetries) {
          console.warn(`[ProcessManager] Retry ${this._retryCount + 1} of ${maxRetries} after error:`, lastError);
        }
      }
    }
    if (lastError) {
      this.emit('error', lastError);
      console.error('[ProcessManager] All retries failed:', lastError);
    }
  }

  public kill(): void {
    if (this._prologProc && typeof this._prologProc.kill === 'function') {
      this._prologProc.kill();
      this._prologProc = null;
    }
  }

  public get pid(): number {
    return this._prologProc ? this._prologProc.pid : 0;
  }

  public get process(): any {
    return this._prologProc;
  }
}
