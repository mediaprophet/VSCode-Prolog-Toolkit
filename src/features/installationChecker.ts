import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { spawn } from 'child_process';
import * as which from 'which';
import { ExecutableFinder, ExecutableDetectionResult } from '../utils/executableFinder';
import { PlatformUtils } from '../utils/platformUtils';

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
   * Check SWI-Prolog installation status
   */
  public async checkSwiplInstallation(): Promise<InstallationStatus> {
    const config = vscode.workspace.getConfiguration('prolog');
    const configuredPath = config.get<string>('executablePath', PlatformUtils.getDefaultExecutablePath());
    const platformInfo = PlatformUtils.getPlatformInfo();
    
    // First, try the configured path if it's not the default
    if (configuredPath && configuredPath !== PlatformUtils.getDefaultExecutablePath()) {
      const configuredResult = await this.executableFinder.validateExecutable(configuredPath);
      if (configuredResult.found) {
        return {
          isInstalled: true,
          path: configuredResult.path,
          version: configuredResult.version,
          permissions: configuredResult.permissions,
          detectionMethod: 'User Configuration',
          platformInfo: {
            platform: platformInfo.platform,
            architecture: platformInfo.architecture,
            osVersion: platformInfo.osVersion
          },
          issues: configuredResult.issues
        };
      }
    }

    // Try to find SWI-Prolog automatically using comprehensive detection
    const detectionResult = await this.executableFinder.findSwiplExecutable();
    if (detectionResult.found) {
      const issues: string[] = [];
      if (configuredPath !== detectionResult.path) {
        issues.push(`Configured path '${configuredPath}' is invalid, but found SWI-Prolog at '${detectionResult.path}' via ${detectionResult.detectionMethod}`);
      }
      if (detectionResult.issues) {
        issues.push(...detectionResult.issues);
      }

      return {
        isInstalled: true,
        path: detectionResult.path,
        version: detectionResult.version,
        permissions: detectionResult.permissions,
        detectionMethod: detectionResult.detectionMethod,
        platformInfo: {
          platform: platformInfo.platform,
          architecture: platformInfo.architecture,
          osVersion: platformInfo.osVersion
        },
        issues: issues.length > 0 ? issues : undefined
      };
    }

    // Not found - provide platform-specific guidance
    const installationSuggestions = this.executableFinder.getInstallationSuggestions();
    return {
      isInstalled: false,
      platformInfo: {
        platform: platformInfo.platform,
        architecture: platformInfo.architecture,
        osVersion: platformInfo.osVersion
      },
      issues: [
        'SWI-Prolog executable not found',
        `Configured path '${configuredPath}' is not valid`,
        ...installationSuggestions,
        ...(detectionResult.issues || [])
      ]
    };
  }

  /**
   * Find SWI-Prolog executable in common locations
   */
  public async findSwiplExecutable(): Promise<string | null> {
    const result = await this.executableFinder.findSwiplExecutable();
    return result.found ? result.path || null : null;
  }

  /**
   * Validate that a given path points to a valid SWI-Prolog executable
   */
  public async validateSwiplPath(execPath: string): Promise<boolean> {
    const result = await this.executableFinder.validateExecutable(execPath);
    return result.found;
  }

  /**
   * Get detailed validation information for an executable path
   */
  public async validateSwiplPathDetailed(execPath: string): Promise<ExecutableDetectionResult> {
    return await this.executableFinder.validateExecutable(execPath);
  }

  /**
   * Get SWI-Prolog version from executable
   */
  public async getSwiplVersion(execPath: string): Promise<string | null> {
    if (!execPath) {
      return null;
    }

    return new Promise<string | null>((resolve) => {
      const process = spawn(execPath, ['--version'], {
        stdio: ['ignore', 'pipe', 'pipe'],
        timeout: 5000
      });

      let output = '';
      process.stdout?.on('data', (data) => {
        output += data.toString();
      });

      process.on('close', (code) => {
        if (code === 0) {
          // Extract version from output
          // SWI-Prolog version output typically looks like:
          // "SWI-Prolog version 9.2.6 for x86_64-linux"
          const versionMatch = output.match(/SWI-Prolog version (\d+\.\d+\.\d+)/i);
          if (versionMatch) {
            resolve(versionMatch[1]);
          } else {
            // Fallback: try to extract any version-like pattern
            const fallbackMatch = output.match(/(\d+\.\d+\.\d+)/);
            resolve(fallbackMatch ? fallbackMatch[1] : 'Unknown');
          }
        } else {
          resolve(null);
        }
      });

      process.on('error', () => {
        resolve(null);
      });

      // Timeout fallback
      setTimeout(() => {
        process.kill();
        resolve(null);
      }, 5000);
    });
  }

  /**
   * Get platform-specific common installation paths
   */
  public detectCommonInstallPaths(): string[] {
    return PlatformUtils.getExecutablePaths();
  }

  /**
   * Check if the current configuration is valid and update if needed
   */
  public async validateAndUpdateConfiguration(): Promise<{updated: boolean, oldPath?: string, newPath?: string}> {
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
          newPath: foundPath
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
  public getSystemInfo(): {platform: string, arch: string, pathEnv: string[]} {
    return {
      platform: os.platform(),
      arch: os.arch(),
      pathEnv: (process.env.PATH || '').split(path.delimiter).filter(p => p.length > 0)
    };
  }

  /**
   * Check if SWI-Prolog version meets minimum requirements
   */
  public checkVersionCompatibility(version: string): {compatible: boolean, message?: string} {
    if (!version || version === 'Unknown') {
      return {
        compatible: false,
        message: 'Unable to determine SWI-Prolog version'
      };
    }

    try {
      const versionParts = version.split('.').map(v => parseInt(v, 10));
      const [major, minor, patch] = versionParts;

      // Minimum version requirements (adjust as needed)
      const minMajor = 8;
      const minMinor = 0;
      const minPatch = 0;

      if (major > minMajor || 
          (major === minMajor && minor > minMinor) ||
          (major === minMajor && minor === minMinor && patch >= minPatch)) {
        return { compatible: true };
      } else {
        return {
          compatible: false,
          message: `SWI-Prolog version ${version} is below minimum required version ${minMajor}.${minMinor}.${minPatch}`
        };
      }
    } catch (error) {
      return {
        compatible: false,
        message: `Invalid version format: ${version}`
      };
    }
  }

  /**
   * Perform comprehensive installation diagnostics
   */
  public async performDiagnostics(): Promise<{
    installation: InstallationStatus;
    system: {platform: string, arch: string, pathEnv: string[], platformInfo: any};
    configuration: {current: string, valid: boolean, detailedValidation?: ExecutableDetectionResult};
    recommendations: string[];
    permissionIssues?: string[];
  }> {
    const installation = await this.checkSwiplInstallation();
    const system = this.getSystemInfo();
    const platformInfo = PlatformUtils.getPlatformInfo();
    const config = vscode.workspace.getConfiguration('prolog');
    const currentPath = config.get<string>('executablePath', PlatformUtils.getDefaultExecutablePath());
    const configValid = await this.validateSwiplPath(currentPath);
    const detailedValidation = await this.validateSwiplPathDetailed(currentPath);

    const recommendations: string[] = [];
    const permissionIssues: string[] = [];

    if (!installation.isInstalled) {
      const installationSuggestions = await this.executableFinder.getInstallationSuggestions();
      recommendations.push(...installationSuggestions);
      
      // Add package manager integration
      try {
        const { PackageManagerIntegration } = await import('./packageManagerIntegration');
        const packageManager = PackageManagerIntegration.getInstance();
        const availableManagers = await packageManager.detectAvailableManagers();
        
        if (availableManagers.length > 0) {
          recommendations.push('');
          recommendations.push('Quick installation options:');
          recommendations.push('  • Use the "Install SWI-Prolog" command in VS Code');
          recommendations.push('  • Run the setup wizard: Ctrl+Shift+P → "Prolog: Setup Wizard"');
        }
      } catch (error) {
        // Package manager integration failed, continue with basic recommendations
      }
    } else {
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
          permissionIssues.push(`Executable file lacks execute permissions: ${installation.path}`);
          if (PlatformUtils.getPlatform() !== 'windows') {
            recommendations.push(`Fix permissions with: chmod +x "${installation.path}"`);
          }
        }
        if (!installation.permissions.readable) {
          permissionIssues.push(`Cannot read executable file: ${installation.path}`);
        }
      }
    }

    if (!configValid && installation.isInstalled && installation.path) {
      recommendations.push(`Update configuration to use found installation: ${installation.path}`);
    }

    // Add platform-specific troubleshooting
    if (!installation.isInstalled) {
      switch (platformInfo.platform) {
        case 'windows':
          recommendations.push('Windows troubleshooting: Check if Windows Defender or antivirus is blocking the executable');
          break;
        case 'macos':
          recommendations.push('macOS troubleshooting: Check if Gatekeeper is blocking the executable (System Preferences > Security & Privacy)');
          break;
        case 'linux':
          recommendations.push('Linux troubleshooting: Ensure the executable has proper permissions and required libraries are installed');
          break;
      }
    }

    return {
      installation,
      system: {
        ...system,
        platformInfo
      },
      configuration: {
        current: currentPath,
        valid: configValid,
        detailedValidation
      },
      recommendations,
      permissionIssues: permissionIssues.length > 0 ? permissionIssues : undefined
    };
  }
}