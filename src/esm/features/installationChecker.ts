import * as os from 'os';
import * as path from 'path';
import * as vscode from 'vscode';
import type { ExecutableDetectionResult } from '../utils/executableFinder.js';
import { ExecutableFinder } from '../utils/executableFinder.js';
import { PlatformUtils } from '../utils/platformUtils.js';
export interface InstallationStatus {
  isInstalled: boolean;
  path?: string;
  version?: string;
  issues?: string[];
  permissions?: {
    readable: boolean;
    writable: boolean;
    executable: boolean;
  };
  detectionMethod?: string;
  platformInfo?: {
    platform: string;
    architecture: string;
    osVersion: string;
  };
}

export class InstallationChecker {
  /**
   * Return a list of common SWI-Prolog installation paths for the current OS.
   * This is used for migration and validation of outdated configurations.
   */
  public static detectCommonInstallPaths(): string[] {
    const platform = os.platform();
    if (platform === 'win32') {
      return [
        'C:/Program Files/SWI-Prolog/bin/swipl.exe',
        'C:/Program Files (x86)/SWI-Prolog/bin/swipl.exe',
        'C:/SWI-Prolog/bin/swipl.exe',
        'C:/ProgramData/chocolatey/bin/swipl.exe',
        'C:/tools/swi-prolog/bin/swipl.exe',
        'C:/scoop/apps/swi-prolog/current/bin/swipl.exe',
      ];
    } else if (platform === 'darwin') {
      return [
        '/usr/local/bin/swipl',
        '/opt/homebrew/bin/swipl',
        '/opt/local/bin/swipl',
        '/Applications/SWI-Prolog.app/Contents/MacOS/swipl',
      ];
    } else if (platform === 'linux') {
      return [
        '/usr/bin/swipl',
        '/usr/local/bin/swipl',
        '/snap/bin/swipl',
        '/opt/swi-prolog/bin/swipl',
      ];
    }
    return [];
  }
  private static instance: InstallationChecker;
  private executableFinder: ExecutableFinder;

  public static getInstance(): InstallationChecker {
    if (!InstallationChecker.instance) {
      InstallationChecker.instance = new InstallationChecker();
    }
    return InstallationChecker.instance;
  }

  constructor() {
    this.executableFinder = new ExecutableFinder();
  }

  /**
   * Check SWI-Prolog installation status.
   * This is the primary method for verifying the SWI-Prolog installation.
   */
  public async checkSwiplInstallation(): Promise<InstallationStatus> {
    const config = vscode.workspace.getConfiguration('prolog');
    const configuredPath = config.get<string>(
      'executablePath',
      PlatformUtils.getDefaultExecutablePath()
    );
    const platformInfo = PlatformUtils.getPlatformInfo();

    // Helper to build the InstallationStatus object
    const buildStatus = (
      result: ExecutableDetectionResult,
      isInstalled: boolean,
      detectionMethod?: string
    ): InstallationStatus => {
      const status: InstallationStatus = {
        isInstalled,
        platformInfo: {
          platform: platformInfo.platform,
          architecture: platformInfo.architecture,
          osVersion: platformInfo.osVersion,
        },
        issues: result.issues || [],
      };

      if (result.path) {
        status.path = result.path;
      }
      if (result.version) {
        status.version = result.version;
      }
      if (result.permissions) {
        status.permissions = result.permissions;
      }
      const finalDetectionMethod = result.detectionMethod || detectionMethod;
      if (finalDetectionMethod) {
        status.detectionMethod = finalDetectionMethod;
      }

      return status;
    };

    // 1. Validate the user-configured path first
    if (configuredPath && configuredPath !== PlatformUtils.getDefaultExecutablePath()) {
      const configuredResult = await this.executableFinder.validateExecutable(configuredPath);
      if (configuredResult.found) {
        return buildStatus(configuredResult, true, 'User Configuration');
      }
    }

    // 2. Try automatic detection if user path is invalid or default
    const detectionResult = await this.executableFinder.findSwiplExecutable();
    if (detectionResult.found) {
      const status = buildStatus(detectionResult, true);
      if (configuredPath && configuredPath !== detectionResult.path) {
        status.issues?.unshift(
          `Configured path '${configuredPath}' is invalid, but found SWI-Prolog at '${detectionResult.path}'`
        );
      }
      return status;
    }

    // 3. Not found, return detailed failure information
    const installationSuggestions = await this.executableFinder.getInstallationSuggestions();
    const finalIssues = [
      'SWI-Prolog executable not found.',
      `Configured path '${configuredPath}' is not valid.`,
      ...installationSuggestions,
    ];
    if (detectionResult.issues) {
      finalIssues.push(...detectionResult.issues);
    }
    return {
      isInstalled: false,
      platformInfo: {
        platform: platformInfo.platform,
        architecture: platformInfo.architecture,
        osVersion: platformInfo.osVersion,
      },
      issues: finalIssues,
    };
  }

  /**
   * Find SWI-Prolog executable using the ExecutableFinder.
   */
  public async findSwiplExecutable(): Promise<string | null> {
    const result = await this.executableFinder.findSwiplExecutable();
    return result.path || null;
  }

  /**
   * Validate that a given path points to a valid SWI-Prolog executable.
   * Delegates to ExecutableFinder.
   */
  public async validateSwiplPath(execPath: string): Promise<boolean> {
    const result = await this.executableFinder.validateExecutable(execPath);
    return result.found;
  }

  /**
   * Get detailed validation information for an executable path.
   * Delegates to ExecutableFinder.
   */
  public async validateSwiplPathDetailed(execPath: string): Promise<ExecutableDetectionResult> {
    return this.executableFinder.validateExecutable(execPath);
  }

  /**
   * Get SWI-Prolog version from executable.
   * This is a convenience method that delegates to ExecutableFinder.
   */
  public async getSwiplVersion(execPath: string): Promise<string | null> {
    const result = await this.executableFinder.validateExecutable(execPath);
    return result.version || null;
  }

  /**
   * Check if the current configuration is valid and update if needed
   */
  public async validateAndUpdateConfiguration(): Promise<{
    updated: boolean;
    oldPath?: string;
    newPath?: string;
  }> {
    const config = vscode.workspace.getConfiguration('prolog');
    const currentPath = config.get<string>('executablePath', 'swipl');

    // If current path is valid, no update needed
    if (await this.validateSwiplPath(currentPath)) {
      return { updated: false };
    }

    // Try to find a valid path
    const foundPath = await this.findSwiplExecutable();
    if (foundPath) {
      try {
        await config.update('executablePath', foundPath, vscode.ConfigurationTarget.Global);
        return {
          updated: true,
          oldPath: currentPath,
          newPath: foundPath,
        };
      } catch (error) {
        console.error('Failed to update configuration:', error);
        return { updated: false };
      }
    }

    return { updated: false };
  }

  /**
   * Get detailed system information for troubleshooting
   */
  public getSystemInfo(): { platform: string; arch: string; pathEnv: string[] } {
    return {
      platform: os.platform(),
      arch: os.arch(),
      pathEnv: (process.env.PATH || '').split(path.delimiter).filter(p => p.length > 0),
    };
  }

  /**
   * Check if SWI-Prolog version meets minimum requirements.
   * This logic is now more robust and handles different version formats.
   */
  public checkVersionCompatibility(version: string): { compatible: boolean; message?: string } {
    if (!version || version === 'Unknown') {
      return {
        compatible: false,
        message: 'Unable to determine SWI-Prolog version.',
      };
    }

    const minVersion = '8.0.0';

    const parse = (v: string): number[] => {
      const parts = v.split('.').map(part => parseInt(part, 10));
      return parts.some(isNaN) ? [] : parts;
    };

    const minParts = parse(minVersion);
    const versionParts = parse(version);

    if (versionParts.length === 0 || minParts.length < 3) {
      return { compatible: false, message: `Invalid version format: ${version}` };
    }

    const major = versionParts[0];
    const minor = versionParts[1] ?? 0;
    const patch = versionParts[2] ?? 0;

    const minMajor = minParts[0];
    const minMinor = minParts[1];
    const minPatch = minParts[2];

    if (
      typeof major !== 'number' ||
      typeof minMajor !== 'number' ||
      typeof minMinor !== 'number' ||
      typeof minPatch !== 'number'
    ) {
      return { compatible: false, message: `Invalid version format: ${version}` };
    }

    if (major > minMajor) return { compatible: true };
    if (major === minMajor && minor > minMinor) return { compatible: true };
    if (major === minMajor && minor === minMinor && patch >= minPatch) {
      return { compatible: true };
    }

    return {
      compatible: false,
      message: `SWI-Prolog version ${version} is below the minimum required version ${minVersion}.`,
    };
  }

  /**
   * Perform comprehensive installation diagnostics.
   * This is the main entry point for gathering detailed troubleshooting information.
   */
  public async performDiagnostics(): Promise<{
    installation: InstallationStatus;
    system: { platform: string; arch: string; pathEnv: string[]; platformInfo: any };
    configuration: {
      current: string;
      valid: boolean;
      detailedValidation?: ExecutableDetectionResult;
    };
    recommendations: string[];
    permissionIssues?: string[];
  }> {
    const installation = await this.checkSwiplInstallation();
    const system = this.getSystemInfo();
    const platformInfo = PlatformUtils.getPlatformInfo();
    const config = vscode.workspace.getConfiguration('prolog');
    const currentPath = config.get<string>(
      'executablePath',
      PlatformUtils.getDefaultExecutablePath()
    );
    const detailedValidation = await this.validateSwiplPathDetailed(currentPath);

    const recommendations: string[] = [];
    const permissionIssues: string[] = [];

    // Helper to add platform-specific troubleshooting tips
    const addTroubleshootingTips = () => {
      switch (platformInfo.platform) {
        case 'windows':
          recommendations.push(
            'Windows Troubleshooting: Check if Antivirus/Firewall is blocking the executable.'
          );
          break;
        case 'macos':
          recommendations.push(
            'macOS Troubleshooting: Check Gatekeeper settings in System Preferences > Security & Privacy.'
          );
          break;
        case 'linux':
          recommendations.push(
            'Linux Troubleshooting: Ensure all required libraries are installed (use `ldd` on the executable).'
          );
          break;
      }
    };

    if (installation.isInstalled) {
      // Check version compatibility
      if (installation.version) {
        const compatibility = this.checkVersionCompatibility(installation.version);
        if (!compatibility.compatible) {
          recommendations.push(`Update SWI-Prolog: ${compatibility.message}`);
        }
      }

      // Check permissions
      if (installation.permissions) {
        if (!installation.permissions.executable) {
          const message = `Executable file lacks execute permissions: ${installation.path}`;
          permissionIssues.push(message);
          if (PlatformUtils.getPlatform() !== 'windows') {
            recommendations.push(`To fix, run: chmod +x "${installation.path}"`);
          }
        }
        if (!installation.permissions.readable) {
          permissionIssues.push(`Cannot read executable file: ${installation.path}`);
        }
      }

      // Check if configuration is pointing to the found installation
      if (!detailedValidation.found && installation.path) {
        recommendations.push(
          `Configuration mismatch. Update 'prolog.executablePath' to the found path: ${installation.path}`
        );
      }
    } else {
      // Not installed: provide installation and troubleshooting guidance
      recommendations.push(...(installation.issues || []));

      // Attempt to add package manager suggestions
      try {
        const { PackageManagerIntegration } = await import('./packageManagerIntegration.js');
        const packageManager = PackageManagerIntegration.getInstance();
        if ((await packageManager.detectAvailableManagers()).length > 0) {
          recommendations.push('Quick Install: Use the "Prolog: Install SWI-Prolog" command.');
        }
      } catch (error) {
        console.warn('Package manager integration check failed:', error);
      }

      addTroubleshootingTips();
    }

    return {
      installation,
      system: { ...system, platformInfo },
      configuration: {
        current: currentPath,
        valid: detailedValidation.found,
        detailedValidation,
      },
      recommendations,
      ...(permissionIssues.length > 0 && { permissionIssues }),
    };
  }
}
