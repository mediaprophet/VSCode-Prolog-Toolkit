// Platform/path/environment variable utilities extracted from utils.ts
import { workspace } from 'vscode';
import { PlatformUtils, getPlatformDefaults } from './platformUtils';

export class PlatformEnvUtils {
  static initializePlatformSettings() {
    const config = workspace.getConfiguration('prolog');
    const autoDetect = config.get<boolean>('platform.autoDetect', true);
    if (autoDetect) {
      const platformDefaults = getPlatformDefaults();
      const currentExecPath = config.get<string>('executablePath');
      if (!currentExecPath || currentExecPath === '/usr/bin/swipl') {
        const platformExecPath = platformDefaults.defaultExecutablePath;
        if (platformExecPath !== currentExecPath) {
          console.log(`[Platform Utils] Auto-detected executable path: ${platformExecPath}`);
        }
      }
      const currentRuntimeArgs = config.get<string[]>('terminal.runtimeArgs');
      if (!currentRuntimeArgs || currentRuntimeArgs.length === 0) {
        const platformRuntimeArgs = platformDefaults.defaultRuntimeArgs;
        if (platformRuntimeArgs.length > 0) {
          console.log(`[Platform Utils] Auto-detected runtime args: ${platformRuntimeArgs.join(' ')}`);
        }
      }
    }
  }

  static getPlatformExecutablePath(): string {
    const config = workspace.getConfiguration('prolog');
    let execPath = config.get<string>('executablePath', '');
    if (!execPath) execPath = getPlatformDefaults().defaultExecutablePath;
    return PlatformUtils.normalizePath(execPath);
  }

  static getPlatformRuntimeArgs(): string[] {
    const config = workspace.getConfiguration('prolog');
    let runtimeArgs = config.get<string[]>('terminal.runtimeArgs', []);
    if (runtimeArgs.length === 0) runtimeArgs = getPlatformDefaults().defaultRuntimeArgs;
    return runtimeArgs;
  }

  static expandEnvironmentVariables(inputPath: string): string {
    return PlatformUtils.expandEnvironmentVariables(inputPath);
  }

  static getPlatformEnvironmentVariables(): Record<string, string> {
    const config = workspace.getConfiguration('prolog');
    const customEnvVars = config.get<Record<string, string>>('platform.environmentVariables', {});
    const platformEnvVars = PlatformUtils.getEnvironmentVariables();
    const result: Record<string, string> = {};
    platformEnvVars.crossPlatform.forEach(varName => {
      if (process.env[varName]) result[varName] = process.env[varName]!;
    });
    platformEnvVars.platformSpecific.forEach(varName => {
      if (process.env[varName]) result[varName] = process.env[varName]!;
    });
    Object.assign(result, customEnvVars);
    return result;
  }

  static createPlatformPath(...segments: string[]): string {
    return PlatformUtils.joinPath(...segments);
  }

  static resolvePlatformPath(basePath: string, ...segments: string[]): string {
    const normalizedBase = PlatformUtils.normalizePath(basePath);
    return PlatformUtils.resolvePath(normalizedBase, ...segments);
  }
}
