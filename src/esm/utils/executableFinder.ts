import { spawn } from 'node:child_process';
import * as fs from 'node:fs';
import * as os from 'node:os';
import * as path from 'node:path';
import which from 'which';
import type { PlatformType } from './platformUtils.js';
import { PlatformUtils } from './platformUtils.js';

/**
 * Executable detection result interface
 */
export interface ExecutableDetectionResult {
  found: boolean;
  path?: string;
  version?: string;
  permissions?: {
    readable: boolean;
    writable: boolean;
    executable: boolean;
  };
  issues?: string[];
  detectionMethod?: string;
}

/**
 * Platform-specific detection strategy interface
 */
export interface DetectionStrategy {
  name: string;
  priority: number;
  detect(): Promise<string | null>;
}

/**
 * Comprehensive executable finder with platform-specific detection strategies
 */
export class ExecutableFinder {
  private platform: PlatformType;
  private strategies: DetectionStrategy[] = [];

  constructor() {
    this.platform = PlatformUtils.getPlatform();
    this.initializeStrategies();
  }

  /**
   * Initialize platform-specific detection strategies
   */
  private initializeStrategies(): void {
    switch (this.platform) {
      case 'windows': {
        this.strategies = [
          new WindowsPathStrategy(),
          new WindowsProgramFilesStrategy(),
          new WindowsRegistryStrategy(),
          new WindowsEnvironmentStrategy(),
        ];
        break;
      }
      case 'macos': {
        this.strategies = [
          new MacOSWhichStrategy(),
          new MacOSHomebrewStrategy(),
          new MacOSApplicationsStrategy(),
          new MacOSMacPortsStrategy(),
          new MacOSStandardPathsStrategy(),
        ];
        break;
      }
      case 'linux': {
        this.strategies = [
          new LinuxWhichStrategy(),
          new LinuxWhereisStrategy(),
          new LinuxStandardPathsStrategy(),
          new LinuxPackageManagerStrategy(),
          new LinuxSnapFlatpakStrategy(),
          new LinuxUserLocalStrategy(),
        ];
        break;
      }
    }

    // Sort strategies by priority (higher priority first)
    this.strategies.sort((a, b) => b.priority - a.priority);
  }

  /**
   * Find SWI-Prolog executable using all available strategies
   */
  public async findSwiplExecutable(): Promise<ExecutableDetectionResult> {
    const issues: string[] = [];

    for (const strategy of this.strategies) {
      try {
        const foundPath = await strategy.detect();
        if (foundPath) {
          const validation = await this.validateExecutable(foundPath);
          if (validation.found) {
            return {
              ...validation,
              detectionMethod: strategy.name,
            };
          } else {
            issues.push(
              `Found potential executable at '${foundPath}' via ${strategy.name}, but validation failed: ${validation.issues?.join(', ')}`
            );
          }
        }
      } catch (error) {
        issues.push(
          `Strategy '${strategy.name}' failed: ${error instanceof Error ? error.message : 'Unknown error'}`
        );
      }
    }

    return {
      found: false,
      issues:
        issues.length > 0
          ? issues
          : ['No SWI-Prolog executable found using any detection strategy'],
    };
  }

  /**
   * Validate an executable path and get detailed information
   */
  public async validateExecutable(execPath: string): Promise<ExecutableDetectionResult> {
    if (!execPath) {
      return {
        found: false,
        issues: ['Empty executable path provided'],
      };
    }

    const normalizedPath = PlatformUtils.normalizePath(execPath);
    const issues: string[] = [];

    try {
      // Check if file exists
      const stats = await fs.promises.stat(normalizedPath);
      if (!stats.isFile()) {
        return {
          found: false,
          path: normalizedPath,
          issues: ['Path exists but is not a file'],
        };
      }

      // Check permissions
      const permissions = await this.checkPermissions(normalizedPath);
      if (!permissions.executable) {
        issues.push('File is not executable');
      }

      // Try to execute and get version
      const versionResult = await this.getExecutableVersion(normalizedPath);
      if (!versionResult.success) {
        issues.push(`Failed to execute: ${versionResult.error}`);
        return {
          found: false,
          path: normalizedPath,
          permissions,
          issues,
        };
      }

      // Validate that it's actually SWI-Prolog
      if (!this.isSwiplOutput(versionResult.output ?? '')) {
        issues.push('Executable does not appear to be SWI-Prolog');
        return {
          found: false,
          path: normalizedPath,
          permissions,
          issues,
        };
      }

      return {
        found: true,
        path: normalizedPath,
        version: this.extractVersion(versionResult.output ?? ''),
        permissions,
        issues: issues.length > 0 ? issues : [],
      };
    } catch (error) {
      return {
        found: false,
        path: normalizedPath,
        issues: [`File system error: ${error instanceof Error ? error.message : 'Unknown error'}`],
      };
    }
  }

  /**
   * Check file permissions (Unix-style and Windows)
   */
  private async checkPermissions(
    filePath: string
  ): Promise<{ readable: boolean; writable: boolean; executable: boolean }> {
    const permissions = {
      readable: false,
      writable: false,
      executable: false,
    };

    try {
      // Check readable
      await fs.promises.access(filePath, fs.constants.R_OK);
      permissions.readable = true;
    } catch {
      // Not readable
    }

    try {
      // Check writable
      await fs.promises.access(filePath, fs.constants.W_OK);
      permissions.writable = true;
    } catch {
      // Not writable (this is normal for system executables)
    }

    try {
      // Check executable
      await fs.promises.access(filePath, fs.constants.X_OK);
      permissions.executable = true;
    } catch {
      // Not executable - this is a problem for executables
    }

    return permissions;
  }

  /**
   * Execute the binary to get version information
   */
  private async getExecutableVersion(
    execPath: string
  ): Promise<{ success: boolean; output?: string; error?: string }> {
    return new Promise(resolve => {
      const process = spawn(execPath, ['--version'], {
        stdio: ['ignore', 'pipe', 'pipe'],
        timeout: 10000,
      });

      let stdout = '';
      let stderr = '';

      process.stdout?.on('data', data => {
        stdout += data.toString();
      });

      process.stderr?.on('data', data => {
        stderr += data.toString();
      });

      process.on('close', code => {
        if (code === 0) {
          resolve({ success: true, output: stdout });
        } else {
          resolve({ success: false, error: `Process exited with code ${code}. stderr: ${stderr}` });
        }
      });

      process.on('error', error => {
        resolve({ success: false, error: error.message });
      });

      // Timeout fallback
      setTimeout(() => {
        process.kill();
        resolve({ success: false, error: 'Process timed out after 10 seconds' });
      }, 10000);
    });
  }

  /**
   * Check if output is from SWI-Prolog
   */
  private isSwiplOutput(output: string): boolean {
    return /SWI-Prolog/i.test(output);
  }

  /**
   * Extract version number from SWI-Prolog output
   */
  private extractVersion(output: string): string {
    const versionMatch = output.match(/SWI-Prolog version (\d+\.\d+\.\d+)/i);
    if (versionMatch) {
      return versionMatch[1] ?? 'Unknown';
    }

    // Fallback: try to extract any version-like pattern
    const fallbackMatch = output.match(/(\d+\.\d+\.\d+)/);
    return fallbackMatch ? (fallbackMatch[1] ?? 'Unknown') : 'Unknown';
  }

  /**
   * Get platform-specific installation suggestions
   */
  public async getInstallationSuggestions(): Promise<string[]> {
    // Import here to avoid circular dependencies
    const { PackageManagerIntegration } = await import('../features/packageManagerIntegration.js');
    const packageManager = PackageManagerIntegration.getInstance();

    try {
      return await packageManager.getInstallationSuggestions();
    } catch (error) {
      // Fallback to basic suggestions if package manager integration fails
      const errorMsg = error instanceof Error ? error.message : String(error);
      console.warn(
        '[ExecutableFinder] Package manager integration failed, using fallback suggestions:',
        errorMsg
      );
      const suggestions: string[] = [];
      suggestions.push(`Install SWI-Prolog from https://www.swi-prolog.org/download/stable`);

      switch (this.platform) {
        case 'windows': {
          suggestions.push('Windows installation options:');
          suggestions.push('  • Download and run the .exe installer from the official website');
          suggestions.push('  • Use Chocolatey: choco install swi-prolog');
          suggestions.push('  • Use Winget: winget install SWI.SWI-Prolog');
          suggestions.push('  • Use Scoop: scoop install swi-prolog');
          break;
        }
        case 'macos': {
          suggestions.push('macOS installation options:');
          suggestions.push('  • Use Homebrew: brew install swi-prolog');
          suggestions.push('  • Use MacPorts: sudo port install swi-prolog');
          suggestions.push('  • Download and install the .dmg file from the official website');
          break;
        }
        case 'linux': {
          suggestions.push('Linux installation options:');
          suggestions.push('  • Ubuntu/Debian: sudo apt install swi-prolog');
          suggestions.push('  • CentOS/RHEL: sudo yum install pl');
          suggestions.push('  • Fedora: sudo dnf install pl');
          suggestions.push('  • Arch Linux: sudo pacman -S swi-prolog');
          suggestions.push('  • openSUSE: sudo zypper install swi-prolog');
          suggestions.push('  • Snap: sudo snap install swi-prolog');
          suggestions.push('  • Flatpak: flatpak install org.swi_prolog.SWI-Prolog');
          break;
        }
      }

      return suggestions;
    }
  }
}

// Windows-specific detection strategies
class WindowsPathStrategy implements DetectionStrategy {
  name = 'Windows PATH Environment';
  priority = 100;

  async detect(): Promise<string | null> {
    try {
      const result = await which('swipl.exe');
      return result || null;
    } catch {
      try {
        const result = await which('swipl');
        return result || null;
      } catch {
        return null;
      }
    }
  }
}

class WindowsProgramFilesStrategy implements DetectionStrategy {
  name = 'Windows Program Files';
  priority = 90;

  async detect(): Promise<string | null> {
    const paths = [
      'C:\\Program Files\\swipl\\bin\\swipl.exe',
      'C:\\Program Files (x86)\\swipl\\bin\\swipl.exe',
      'C:\\swipl\\bin\\swipl.exe',
    ];

    if (process.env.ProgramFiles) {
      paths.push(path.join(process.env.ProgramFiles, 'swipl', 'bin', 'swipl.exe'));
    }
    if (process.env['ProgramFiles(x86)']) {
      paths.push(path.join(process.env['ProgramFiles(x86)'], 'swipl', 'bin', 'swipl.exe'));
    }

    for (const execPath of paths) {
      try {
        await fs.promises.access(execPath, fs.constants.F_OK);
        return execPath;
      } catch {
        continue;
      }
    }

    return null;
  }
}

class WindowsRegistryStrategy implements DetectionStrategy {
  name = 'Windows Registry';
  priority = 80;

  async detect(): Promise<string | null> {
    // Note: This is a simplified registry check
    // In a full implementation, you might use a Windows registry library
    // For now, we'll check common registry-based installation paths
    const registryPaths = [
      path.join(os.homedir(), 'AppData', 'Local', 'swipl', 'bin', 'swipl.exe'),
      path.join(os.homedir(), 'AppData', 'Roaming', 'swipl', 'bin', 'swipl.exe'),
    ];

    for (const execPath of registryPaths) {
      try {
        await fs.promises.access(execPath, fs.constants.F_OK);
        return execPath;
      } catch {
        continue;
      }
    }

    return null;
  }
}

class WindowsEnvironmentStrategy implements DetectionStrategy {
  name = 'Windows Environment Variables';
  priority = 70;

  async detect(): Promise<string | null> {
    const swiplHome = process.env.SWIPL_HOME;
    if (swiplHome) {
      const execPath = path.join(swiplHome, 'bin', 'swipl.exe');
      try {
        await fs.promises.access(execPath, fs.constants.F_OK);
        return execPath;
      } catch {
        // Continue
      }
    }

    return null;
  }
}

// macOS-specific detection strategies
class MacOSWhichStrategy implements DetectionStrategy {
  name = 'macOS which command';
  priority = 100;

  async detect(): Promise<string | null> {
    try {
      const result = await which('swipl');
      return result || null;
    } catch {
      return null;
    }
  }
}

class MacOSHomebrewStrategy implements DetectionStrategy {
  name = 'macOS Homebrew';
  priority = 90;

  async detect(): Promise<string | null> {
    const homebrewPaths = [
      '/usr/local/bin/swipl', // Intel Macs
      '/opt/homebrew/bin/swipl', // Apple Silicon Macs
    ];

    for (const execPath of homebrewPaths) {
      try {
        await fs.promises.access(execPath, fs.constants.F_OK);
        return execPath;
      } catch {
        continue;
      }
    }

    return null;
  }
}

class MacOSApplicationsStrategy implements DetectionStrategy {
  name = 'macOS Applications';
  priority = 80;

  async detect(): Promise<string | null> {
    const appPath = '/Applications/SWI-Prolog.app/Contents/MacOS/swipl';
    try {
      await fs.promises.access(appPath, fs.constants.F_OK);
      return appPath;
    } catch {
      return null;
    }
  }
}

class MacOSMacPortsStrategy implements DetectionStrategy {
  name = 'macOS MacPorts';
  priority = 70;

  async detect(): Promise<string | null> {
    const macPortsPaths = [
      '/opt/local/bin/swipl',
      '/sw/bin/swipl', // Fink
    ];

    for (const execPath of macPortsPaths) {
      try {
        await fs.promises.access(execPath, fs.constants.F_OK);
        return execPath;
      } catch {
        continue;
      }
    }

    return null;
  }
}

class MacOSStandardPathsStrategy implements DetectionStrategy {
  name = 'macOS Standard Paths';
  priority = 60;

  async detect(): Promise<string | null> {
    const standardPaths = ['/usr/bin/swipl', path.join(os.homedir(), '.local', 'bin', 'swipl')];

    for (const execPath of standardPaths) {
      try {
        await fs.promises.access(execPath, fs.constants.F_OK);
        return execPath;
      } catch {
        continue;
      }
    }

    return null;
  }
}

// Linux-specific detection strategies
class LinuxWhichStrategy implements DetectionStrategy {
  name = 'Linux which command';
  priority = 100;

  async detect(): Promise<string | null> {
    try {
      const result = await which('swipl');
      return result || null;
    } catch {
      return null;
    }
  }
}

class LinuxWhereisStrategy implements DetectionStrategy {
  name = 'Linux whereis command';
  priority = 95;

  async detect(): Promise<string | null> {
    return new Promise(resolve => {
      const process = spawn('whereis', ['swipl'], {
        stdio: ['ignore', 'pipe', 'pipe'],
        timeout: 5000,
      });

      let output = '';
      process.stdout?.on('data', data => {
        output += data.toString();
      });

      process.on('close', code => {
        if (code === 0) {
          // whereis output format: "swipl: /usr/bin/swipl /usr/share/man/man1/swipl.1.gz"
          const match = output.match(/swipl:\s+([^\s]+)/);
          if (match?.[1] && !match[1].includes('.gz')) {
            resolve(match[1]);
            return;
          }
        }
        resolve(null);
      });

      process.on('error', () => {
        resolve(null);
      });

      setTimeout(() => {
        process.kill();
        resolve(null);
      }, 5000);
    });
  }
}

class LinuxStandardPathsStrategy implements DetectionStrategy {
  name = 'Linux Standard Paths';
  priority = 90;

  async detect(): Promise<string | null> {
    const standardPaths = ['/usr/bin/swipl', '/usr/local/bin/swipl', '/opt/swipl/bin/swipl'];

    for (const execPath of standardPaths) {
      try {
        await fs.promises.access(execPath, fs.constants.F_OK);
        return execPath;
      } catch {
        continue;
      }
    }

    return null;
  }
}

class LinuxPackageManagerStrategy implements DetectionStrategy {
  name = 'Linux Package Manager Paths';
  priority = 80;

  async detect(): Promise<string | null> {
    const packagePaths = [
      '/usr/lib/swi-prolog/bin/x86_64-linux/swipl', // Debian/Ubuntu specific
      '/usr/lib64/swi-prolog/bin/x86_64-linux/swipl', // 64-bit systems
    ];

    for (const execPath of packagePaths) {
      try {
        await fs.promises.access(execPath, fs.constants.F_OK);
        return execPath;
      } catch {
        continue;
      }
    }

    return null;
  }
}

class LinuxSnapFlatpakStrategy implements DetectionStrategy {
  name = 'Linux Snap/Flatpak';
  priority = 70;

  async detect(): Promise<string | null> {
    const snapFlatpakPaths = [
      '/snap/bin/swi-prolog',
      '/var/lib/flatpak/exports/bin/org.swi_prolog.SWI-Prolog',
      path.join(
        os.homedir(),
        '.local',
        'share',
        'flatpak',
        'exports',
        'bin',
        'org.swi_prolog.SWI-Prolog'
      ),
    ];

    for (const execPath of snapFlatpakPaths) {
      try {
        await fs.promises.access(execPath, fs.constants.F_OK);
        return execPath;
      } catch {
        continue;
      }
    }

    return null;
  }
}

class LinuxUserLocalStrategy implements DetectionStrategy {
  name = 'Linux User Local';
  priority = 60;

  async detect(): Promise<string | null> {
    const userPaths = [
      path.join(os.homedir(), '.local', 'bin', 'swipl'),
      path.join(os.homedir(), 'bin', 'swipl'),
    ];

    for (const execPath of userPaths) {
      try {
        await fs.promises.access(execPath, fs.constants.F_OK);
        return execPath;
      } catch {
        continue;
      }
    }

    return null;
  }
}
