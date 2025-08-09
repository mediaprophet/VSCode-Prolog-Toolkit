

// Production-grade, modular backend manager for SWI-Prolog as a child process with JSON protocol
import { ChildProcess, spawn } from 'child_process';
import { EventEmitter } from 'events';
import * as os from 'os';

export interface PrologBackendOptions {
  executablePath?: string;
  args?: string[];
  port?: number;
  env?: NodeJS.ProcessEnv;
  onStdout?: (data: string) => void;
  onStderr?: (data: string) => void;
  onExit?: (code: number | null, signal: string | null) => void;
  timeoutMs?: number;
}

export class PrologBackend extends EventEmitter {
  private process: ChildProcess | null = null;
  private options: PrologBackendOptions;
  private isRunning = false;
  private lastError: Error | null = null;

  constructor(options: PrologBackendOptions = {}) {
    super();
    this.options = options;
  }

  public start(): Promise<void> {
    return new Promise((resolve, reject) => {
      if (this.isRunning) {
        return resolve();
      }
      const exe = this.options.executablePath || this.getDefaultExecutable();
      const args = this.options.args || [];
      const env = { ...process.env, ...this.options.env };
      this.process = spawn(exe, args, { env });
      this.isRunning = true;
      this.lastError = null;
      this.process.stdout?.on('data', (data) => {
        this.options.onStdout?.(data.toString());
        this.emit('stdout', data.toString());
      });
      this.process.stderr?.on('data', (data) => {
        this.options.onStderr?.(data.toString());
        this.emit('stderr', data.toString());
      });
      this.process.on('exit', (code, signal) => {
        this.isRunning = false;
        this.options.onExit?.(code, signal);
        this.emit('exit', code, signal);
      });
      this.process.on('error', (err) => {
        this.lastError = err;
        this.isRunning = false;
        this.emit('error', err);
        reject(err);
      });
      // Wait for process to be ready (could add handshake logic here)
      setTimeout(() => resolve(), 500);
    });
  }

  public stop(): void {
    if (this.process && this.isRunning) {
      this.process.kill();
      this.isRunning = false;
    }
  }

  public sendJSON(message: object): void {
    if (this.process && this.process.stdin && this.isRunning) {
      this.process.stdin.write(JSON.stringify(message) + '\n');
    }
  }

  public getLastError(): Error | null {
    return this.lastError;
  }

  public isAlive(): boolean {
    return this.isRunning;
  }

  private getDefaultExecutable(): string {
    switch (os.platform()) {
      case 'win32':
        return 'swipl.exe';
      case 'darwin':
        return '/usr/local/bin/swipl';
      default:
        return 'swipl';
    }
  }
}

