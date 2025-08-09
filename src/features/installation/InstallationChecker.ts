
import { execFile } from 'child_process';
import * as os from 'os';
import { promisify } from 'util';
import * as vscode from 'vscode';
const execFileAsync = promisify(execFile);

export interface InstallationStatus {
  isInstalled: boolean;
  path?: string | undefined;
  version?: string | undefined;
  issues?: string[];
}

export interface SystemInfo {
  platform: string;
  arch: string;
  pathEnv: string[];
}

export interface Diagnostics {
  installation: InstallationStatus;
  system: SystemInfo;
  configuration: { current: string; valid: boolean };
  recommendations: string[];
}

export class InstallationChecker {
  private static instance: InstallationChecker;

  private constructor() { }

  public static getInstance(): InstallationChecker {
    if (!InstallationChecker.instance) {
      InstallationChecker.instance = new InstallationChecker();
    }
    return InstallationChecker.instance;
  }

  public async checkSwiplInstallation(): Promise<InstallationStatus> {
    const config = vscode.workspace.getConfiguration('prolog');
    const configuredPath = config.get<string>('executablePath', 'swipl');
    const result: InstallationStatus = {
      isInstalled: false,
      path: undefined,
      version: undefined,
      issues: [],
    };
    try {
      const version = await this.getSwiplVersion(configuredPath);
      result.isInstalled = true;
      result.path = configuredPath;
      result.version = version;
    } catch (e) {
      result.issues?.push('SWI-Prolog not found or not executable at configured path.');
    }
    return result;
  }

  public async validateSwiplPath(path: string): Promise<boolean> {
    try {
      await this.getSwiplVersion(path);
      return true;
    } catch {
      return false;
    }
  }

  public async getSwiplVersion(path: string): Promise<string> {
    try {
      const { stdout } = await execFileAsync(path, ['--version']);
      const match = stdout.match(/SWI-Prolog\s+([\d.]+)/);
      if (match && match[1]) {
        return match[1];
      }
      throw new Error('Could not parse SWI-Prolog version');
    } catch (e) {
      throw new Error('SWI-Prolog not found or not executable');
    }
  }

  public async findSwiplExecutable(): Promise<string | undefined> {
    // Try configured path first
    const config = vscode.workspace.getConfiguration('prolog');
    const configuredPath = config.get<string>('executablePath', 'swipl');
    if (await this.validateSwiplPath(configuredPath)) {
      return configuredPath;
    }
    // Try common locations
    const candidates = os.platform() === 'win32'
      ? ['C:/Program Files/swipl/bin/swipl.exe', 'C:/swipl/bin/swipl.exe', 'swipl']
      : ['/usr/bin/swipl', '/usr/local/bin/swipl', 'swipl'];
    for (const candidate of candidates) {
      if (await this.validateSwiplPath(candidate)) {
        return candidate;
      }
    }
    return undefined;
  }

  public getSystemInfo(): SystemInfo {
    return {
      platform: os.platform(),
      arch: os.arch(),
      pathEnv: (process.env.PATH || '').split(os.platform() === 'win32' ? ';' : ':'),
    };
  }

  public async performDiagnostics(): Promise<Diagnostics> {
    const installation = await this.checkSwiplInstallation();
    const system = this.getSystemInfo();
    const config = vscode.workspace.getConfiguration('prolog');
    const current = config.get<string>('executablePath', 'swipl');
    const valid = await this.validateSwiplPath(current);
    const recommendations: string[] = [];
    if (!installation.isInstalled) {
      recommendations.push('Install SWI-Prolog and configure the correct path.');
    }
    if (!valid) {
      recommendations.push('Check the configured SWI-Prolog path in settings.');
    }
    return {
      installation,
      system,
      configuration: { current, valid },
      recommendations,
    };
  }
}
