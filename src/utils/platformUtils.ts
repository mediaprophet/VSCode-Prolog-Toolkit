import * as os from 'os';
import * as path from 'path';
import * as fs from 'fs';

/**
 * Platform configuration interface defining platform-specific settings
 */
export interface PlatformConfig {
  executablePaths: string[];
  packageManager: string;
  installCommands: string[];
  pathSeparator: string;
  executableExtension: string;
  defaultExecutablePath: string;
  defaultRuntimeArgs: string[];
  configurationLocation: string;
  tempDirectory: string;
  homeDirectory: string;
}

/**
 * Supported platform types
 */
export type PlatformType = 'windows' | 'macos' | 'linux';

/**
 * Supported architecture types
 */
export type ArchitectureType = 'x64' | 'arm64' | 'x86' | 'x32';

/**
 * Platform-specific configurations
 */
const PLATFORM_CONFIGS: Record<PlatformType, PlatformConfig> = {
  windows: {
    executablePaths: [
      'C:\\Program Files\\swipl\\bin\\swipl.exe',
      'C:\\swipl\\bin\\swipl.exe',
      'swipl.exe',
    ],
    packageManager: 'chocolatey',
    installCommands: ['choco install swi-prolog', 'winget install SWI.SWI-Prolog'],
    pathSeparator: '\\',
    executableExtension: '.exe',
    defaultExecutablePath: 'swipl',
    defaultRuntimeArgs: [],
    configurationLocation: '%APPDATA%\\Code\\User\\settings.json',
    tempDirectory: process.env.TEMP || process.env.TMP || 'C:\\temp',
    homeDirectory: process.env.USERPROFILE || 'C:\\Users\\Default',
  },
  macos: {
    executablePaths: [
      '/usr/local/bin/swipl',
      '/opt/homebrew/bin/swipl',
      '/Applications/SWI-Prolog.app/Contents/MacOS/swipl',
      '/usr/bin/swipl',
    ],
    packageManager: 'homebrew',
    installCommands: ['brew install swi-prolog', 'port install swi-prolog'],
    pathSeparator: '/',
    executableExtension: '',
    defaultExecutablePath: '/usr/local/bin/swipl',
    defaultRuntimeArgs: ['--quiet'],
    configurationLocation: '~/Library/Application Support/Code/User/settings.json',
    tempDirectory: process.env.TMPDIR || '/tmp',
    homeDirectory: process.env.HOME || '/Users/unknown',
  },
  linux: {
    executablePaths: [
      '/usr/bin/swipl',
      '/usr/local/bin/swipl',
      '/opt/swipl/bin/swipl',
      '~/.local/bin/swipl',
    ],
    packageManager: 'apt',
    installCommands: [
      'sudo apt install swi-prolog',
      'sudo yum install pl',
      'sudo dnf install pl',
      'sudo pacman -S swi-prolog',
    ],
    pathSeparator: '/',
    executableExtension: '',
    defaultExecutablePath: '/usr/bin/swipl',
    defaultRuntimeArgs: ['--quiet'],
    configurationLocation: '~/.config/Code/User/settings.json',
    tempDirectory: process.env.TMPDIR || '/tmp',
    homeDirectory: process.env.HOME || '/home/unknown',
  },
};

/**
 * Environment variable mappings for different platforms
 */
export interface EnvironmentVariables {
  crossPlatform: string[];
  platformSpecific: string[];
}

const ENVIRONMENT_VARIABLES: Record<PlatformType, EnvironmentVariables> = {
  windows: {
    crossPlatform: ['SWIPL_HOME', 'PROLOG_PATH', 'PATH'],
    platformSpecific: ['PROGRAMFILES', 'LOCALAPPDATA', 'APPDATA', 'USERPROFILE', 'TEMP', 'TMP'],
  },
  macos: {
    crossPlatform: ['SWIPL_HOME', 'PROLOG_PATH', 'PATH'],
    platformSpecific: ['HOME', 'XDG_CONFIG_HOME', 'XDG_DATA_HOME', 'TMPDIR'],
  },
  linux: {
    crossPlatform: ['SWIPL_HOME', 'PROLOG_PATH', 'PATH'],
    platformSpecific: ['HOME', 'XDG_CONFIG_HOME', 'XDG_DATA_HOME', 'TMPDIR'],
  },
};

/**
 * Utility class for platform detection and configuration
 */
export class PlatformUtils {
  private static _platform: PlatformType | null = null;
  private static _architecture: ArchitectureType | null = null;
  private static _config: PlatformConfig | null = null;

  /**
   * Get the current platform type
   */
  public static getPlatform(): PlatformType {
    if (this._platform === null) {
      const platform = os.platform();
      switch (platform) {
        case 'win32': {
          this._platform = 'windows';
          break;
        }
        case 'darwin': {
          this._platform = 'macos';
          break;
        }
        case 'linux': {
          this._platform = 'linux';
          break;
        }
        default:
          // Default to linux for unknown platforms
          this._platform = 'linux';
          break;
      }
    }
    return this._platform;
  }

  /**
   * Get the current architecture type
   */
  public static getArchitecture(): ArchitectureType {
    if (this._architecture === null) {
      const arch = os.arch();
      switch (arch) {
        case 'x64': {
          this._architecture = 'x64';
          break;
        }
        case 'arm64': {
          this._architecture = 'arm64';
          break;
        }
        case 'ia32': {
          this._architecture = 'x32';
          break;
        }
        default:
          // Default to x64 for unknown architectures
          this._architecture = 'x64';
          break;
      }
    }
    return this._architecture;
  }

  /**
   * Get platform-specific default configuration
   */
  public static getPlatformDefaults(): PlatformConfig {
    if (this._config === null) {
      const platform = this.getPlatform();
      this._config = { ...PLATFORM_CONFIGS[platform] };

      // Expand environment variables in paths
      this._config.tempDirectory = this.expandEnvironmentVariables(this._config.tempDirectory);
      this._config.homeDirectory = this.expandEnvironmentVariables(this._config.homeDirectory);
      this._config.configurationLocation = this.expandEnvironmentVariables(
        this._config.configurationLocation
      );
    }
    return this._config;
  }

  /**
   * Normalize a file path for the current platform
   */
  public static normalizePath(inputPath: string): string {
    if (!inputPath) {
      return '';
    }

    // Expand environment variables first
    let normalizedPath = this.expandEnvironmentVariables(inputPath);

    // Handle home directory expansion (~)
    if (normalizedPath.startsWith('~')) {
      const homeDir = this.getHomeDirectory();
      normalizedPath = normalizedPath.replace(/^~/, homeDir);
    }

    // Normalize path separators
    normalizedPath = path.normalize(normalizedPath);

    // Handle Windows UNC paths
    if (this.getPlatform() === 'windows' && normalizedPath.startsWith('\\\\')) {
      return normalizedPath;
    }

    return normalizedPath;
  }

  /**
   * Get the executable extension for the current platform
   */
  public static getExecutableExtension(): string {
    return this.getPlatformDefaults().executableExtension;
  }

  /**
   * Get the path separator for the current platform
   */
  public static getPathSeparator(): string {
    return this.getPlatformDefaults().pathSeparator;
  }

  /**
   * Get the home directory for the current platform
   */
  public static getHomeDirectory(): string {
    return this.getPlatformDefaults().homeDirectory;
  }

  /**
   * Get the temporary directory for the current platform
   */
  public static getTempDirectory(): string {
    return this.getPlatformDefaults().tempDirectory;
  }

  /**
   * Get the configuration file location for the current platform
   */
  public static getConfigurationLocation(): string {
    return this.getPlatformDefaults().configurationLocation;
  }

  /**
   * Get platform-specific environment variables
   */
  public static getEnvironmentVariables(): EnvironmentVariables {
    const platform = this.getPlatform();
    return ENVIRONMENT_VARIABLES[platform];
  }

  /**
   * Expand environment variables in a path string
   */
  public static expandEnvironmentVariables(inputPath: string): string {
    if (!inputPath) {
      return '';
    }

    let expandedPath = inputPath;
    const platform = this.getPlatform();

    if (platform === 'windows') {
      // Windows-style environment variable expansion (%VAR%)
      expandedPath = expandedPath.replace(/%([^%]+)%/g, (match, varName) => {
        return process.env[varName] || match;
      });
    } else {
      // Unix-style environment variable expansion ($VAR or ${VAR})
      expandedPath = expandedPath.replace(/\$\{([^}]+)\}/g, (match, varName) => {
        return process.env[varName] || match;
      });
      expandedPath = expandedPath.replace(/\$([A-Za-z_][A-Za-z0-9_]*)/g, (match, varName) => {
        return process.env[varName] || match;
      });
    }

    return expandedPath;
  }

  /**
   * Check if a path exists and is accessible
   */
  public static async pathExists(filePath: string): Promise<boolean> {
    try {
      const normalizedPath = this.normalizePath(filePath);
      await fs.promises.access(normalizedPath, fs.constants.F_OK);
      return true;
    } catch {
      return false;
    }
  }

  /**
   * Check if a path is executable
   */
  public static async isExecutable(filePath: string): Promise<boolean> {
    try {
      const normalizedPath = this.normalizePath(filePath);
      await fs.promises.access(normalizedPath, fs.constants.F_OK | fs.constants.X_OK);
      return true;
    } catch {
      return false;
    }
  }

  /**
   * Get platform-specific executable paths for SWI-Prolog
   */
  public static getExecutablePaths(): string[] {
    return this.getPlatformDefaults().executablePaths;
  }

  /**
   * Get the default executable path for the current platform
   */
  public static getDefaultExecutablePath(): string {
    return this.getPlatformDefaults().defaultExecutablePath;
  }

  /**
   * Get default runtime arguments for the current platform
   */
  public static getDefaultRuntimeArgs(): string[] {
    return this.getPlatformDefaults().defaultRuntimeArgs;
  }

  /**
   * Get package manager information for the current platform
   */
  public static getPackageManager(): string {
    return this.getPlatformDefaults().packageManager;
  }

  /**
   * Get installation commands for the current platform
   */
  public static getInstallCommands(): string[] {
    return this.getPlatformDefaults().installCommands;
  }

  /**
   * Create a platform-appropriate file path
   */
  public static joinPath(...segments: string[]): string {
    return path.join(...segments);
  }

  /**
   * Resolve a path relative to another path
   */
  public static resolvePath(basePath: string, ...segments: string[]): string {
    const normalizedBase = this.normalizePath(basePath);
    return path.resolve(normalizedBase, ...segments);
  }

  /**
   * Get the directory name from a path
   */
  public static dirname(filePath: string): string {
    return path.dirname(this.normalizePath(filePath));
  }

  /**
   * Get the base name from a path
   */
  public static basename(filePath: string, ext?: string): string {
    return path.basename(this.normalizePath(filePath), ext);
  }

  /**
   * Get the file extension from a path
   */
  public static extname(filePath: string): string {
    return path.extname(this.normalizePath(filePath));
  }

  /**
   * Check if a path is absolute
   */
  public static isAbsolute(filePath: string): boolean {
    return path.isAbsolute(this.normalizePath(filePath));
  }

  /**
   * Convert a relative path to absolute
   */
  public static toAbsolute(filePath: string, basePath?: string): string {
    const normalizedPath = this.normalizePath(filePath);
    if (this.isAbsolute(normalizedPath)) {
      return normalizedPath;
    }

    const base = basePath ? this.normalizePath(basePath) : process.cwd();
    return path.resolve(base, normalizedPath);
  }

  /**
   * Get platform information summary
   */
  public static getPlatformInfo(): {
    platform: PlatformType;
    architecture: ArchitectureType;
    osVersion: string;
    nodeVersion: string;
    homeDirectory: string;
    tempDirectory: string;
    pathSeparator: string;
    executableExtension: string;
  } {
    return {
      platform: this.getPlatform(),
      architecture: this.getArchitecture(),
      osVersion: os.release(),
      nodeVersion: process.version,
      homeDirectory: this.getHomeDirectory(),
      tempDirectory: this.getTempDirectory(),
      pathSeparator: this.getPathSeparator(),
      executableExtension: this.getExecutableExtension(),
    };
  }

  /**
   * Reset cached platform information (useful for testing)
   */
  public static resetCache(): void {
    this._platform = null;
    this._architecture = null;
    this._config = null;
  }
}

/**
 * Convenience functions for common operations
 */

/**
 * Get the current platform type
 */
export function getPlatform(): PlatformType {
  return PlatformUtils.getPlatform();
}

/**
 * Get the current architecture type
 */
export function getArchitecture(): ArchitectureType {
  return PlatformUtils.getArchitecture();
}

/**
 * Get platform-specific default configuration
 */
export function getPlatformDefaults(): PlatformConfig {
  return PlatformUtils.getPlatformDefaults();
}

/**
 * Normalize a file path for the current platform
 */
export function normalizePath(inputPath: string): string {
  return PlatformUtils.normalizePath(inputPath);
}

/**
 * Get the executable extension for the current platform
 */
export function getExecutableExtension(): string {
  return PlatformUtils.getExecutableExtension();
}

/**
 * Get the path separator for the current platform
 */
export function getPathSeparator(): string {
  return PlatformUtils.getPathSeparator();
}

/**
 * Get the home directory for the current platform
 */
export function getHomeDirectory(): string {
  return PlatformUtils.getHomeDirectory();
}

/**
 * Get the temporary directory for the current platform
 */
export function getTempDirectory(): string {
  return PlatformUtils.getTempDirectory();
}
