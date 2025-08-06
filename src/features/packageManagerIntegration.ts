import { spawn } from 'child_process';
import * as vscode from 'vscode';
import { PlatformType, PlatformUtils } from '../utils/platformUtils';

/**
 * Package manager information interface
 */
export interface PackageManagerInfo {
  name: string;
  displayName: string;
  checkCommand: string;
  installCommand: string;
  packageName: string;
  isAvailable: boolean;
  version?: string;
  priority: number; // Higher priority = preferred
}

/**
 * Installation result interface
 */
export interface InstallationResult {
  success: boolean;
  packageManager: string;
  command: string;
  output?: string;
  error?: string;
  requiresElevation?: boolean;
}

/**
 * Package manager integration for SWI-Prolog installation
 */
export class PackageManagerIntegration {
  private static instance: PackageManagerIntegration;
  private platform: PlatformType;
  private availableManagers: Map<string, PackageManagerInfo> = new Map();
  private detectionCache: Map<string, boolean> = new Map();

  public static getInstance(): PackageManagerIntegration {
    if (!PackageManagerIntegration.instance) {
      PackageManagerIntegration.instance = new PackageManagerIntegration();
    }
    return PackageManagerIntegration.instance;
  }

  constructor() {
    this.platform = PlatformUtils.getPlatform();
    this.initializePackageManagers();
  }

  /**
   * Initialize platform-specific package managers
   */
  private initializePackageManagers(): void {
    switch (this.platform) {
      case 'windows':
        this.initializeWindowsManagers();
        break;
      case 'macos':
        this.initializeMacOSManagers();
        break;
      case 'linux':
        this.initializeLinuxManagers();
        break;
      default:
        break;
    }
  }

  /**
   * Initialize Windows package managers
   */
  private initializeWindowsManagers(): void {
    const managers: PackageManagerInfo[] = [
      {
        name: 'winget',
        displayName: 'Windows Package Manager (winget)',
        checkCommand: 'winget --version',
        installCommand: 'winget install SWI.SWI-Prolog',
        packageName: 'SWI.SWI-Prolog',
        isAvailable: false,
        priority: 100,
      },
      {
        name: 'chocolatey',
        displayName: 'Chocolatey',
        checkCommand: 'choco --version',
        installCommand: 'choco install swi-prolog',
        packageName: 'swi-prolog',
        isAvailable: false,
        priority: 90,
      },
      {
        name: 'scoop',
        displayName: 'Scoop',
        checkCommand: 'scoop --version',
        installCommand: 'scoop install swi-prolog',
        packageName: 'swi-prolog',
        isAvailable: false,
        priority: 80,
      },
    ];

    managers.forEach(manager => {
      this.availableManagers.set(manager.name, manager);
    });
  }

  /**
   * Initialize macOS package managers
   */
  private initializeMacOSManagers(): void {
    const managers: PackageManagerInfo[] = [
      {
        name: 'homebrew',
        displayName: 'Homebrew',
        checkCommand: 'brew --version',
        installCommand: 'brew install swi-prolog',
        packageName: 'swi-prolog',
        isAvailable: false,
        priority: 100,
      },
      {
        name: 'macports',
        displayName: 'MacPorts',
        checkCommand: 'port version',
        installCommand: 'sudo port install swi-prolog',
        packageName: 'swi-prolog',
        isAvailable: false,
        priority: 90,
      },
    ];

    managers.forEach(manager => {
      this.availableManagers.set(manager.name, manager);
    });
  }

  /**
   * Initialize Linux package managers
   */
  private initializeLinuxManagers(): void {
    const managers: PackageManagerInfo[] = [
      {
        name: 'apt',
        displayName: 'APT (Ubuntu/Debian)',
        checkCommand: 'apt --version',
        installCommand: 'sudo apt update && sudo apt install swi-prolog',
        packageName: 'swi-prolog',
        isAvailable: false,
        priority: 100,
      },
      {
        name: 'dnf',
        displayName: 'DNF (Fedora)',
        checkCommand: 'dnf --version',
        installCommand: 'sudo dnf install pl',
        packageName: 'pl',
        isAvailable: false,
        priority: 95,
      },
      {
        name: 'yum',
        displayName: 'YUM (CentOS/RHEL)',
        checkCommand: 'yum --version',
        installCommand: 'sudo yum install pl',
        packageName: 'pl',
        isAvailable: false,
        priority: 90,
      },
      {
        name: 'pacman',
        displayName: 'Pacman (Arch Linux)',
        checkCommand: 'pacman --version',
        installCommand: 'sudo pacman -S swi-prolog',
        packageName: 'swi-prolog',
        isAvailable: false,
        priority: 85,
      },
      {
        name: 'zypper',
        displayName: 'Zypper (openSUSE)',
        checkCommand: 'zypper --version',
        installCommand: 'sudo zypper install swi-prolog',
        packageName: 'swi-prolog',
        isAvailable: false,
        priority: 80,
      },
      {
        name: 'snap',
        displayName: 'Snap',
        checkCommand: 'snap --version',
        installCommand: 'sudo snap install swi-prolog',
        packageName: 'swi-prolog',
        isAvailable: false,
        priority: 70,
      },
      {
        name: 'flatpak',
        displayName: 'Flatpak',
        checkCommand: 'flatpak --version',
        installCommand: 'flatpak install flathub org.swi_prolog.SWI-Prolog',
        packageName: 'org.swi_prolog.SWI-Prolog',
        isAvailable: false,
        priority: 60,
      },
    ];

    managers.forEach(manager => {
      this.availableManagers.set(manager.name, manager);
    });
  }

  /**
   * Detect available package managers on the system
   */
  public async detectAvailableManagers(): Promise<PackageManagerInfo[]> {
    const available: PackageManagerInfo[] = [];

    for (const [name, manager] of this.availableManagers) {
      // Check cache first
      if (this.detectionCache.has(name)) {
        manager.isAvailable = this.detectionCache.get(name)!;
        if (manager.isAvailable) {
          available.push(manager);
        }
        continue;
      }

      try {
        const isAvailable = await this.checkPackageManagerAvailability(manager);
        manager.isAvailable = isAvailable;
        this.detectionCache.set(name, isAvailable);

        if (isAvailable) {
          available.push(manager);
        }
      } catch (_error) {
        manager.isAvailable = false;
        this.detectionCache.set(name, false);
      }
    }

    // Sort by priority (higher priority first)
    return available.sort((a, b) => b.priority - a.priority);
  }

  /**
   * Check if a specific package manager is available
   */
  private async checkPackageManagerAvailability(manager: PackageManagerInfo): Promise<boolean> {
    return new Promise(resolve => {
      const [command, ...args] = manager.checkCommand.split(' ');
      if (!command) {
        resolve(false);
        return;
      }
      const process: import('child_process').ChildProcess = spawn(command, args, {
        stdio: ['ignore', 'pipe', 'pipe'],
        timeout: 5000,
      });

      let hasOutput = false;

      process.stdout?.on('data', data => {
        const output = data.toString();
        hasOutput = true;

        // Try to extract version information
        const versionMatch = output.match(/(\d+\.\d+(?:\.\d+)?)/);
        if (versionMatch) {
          manager.version = versionMatch[1];
        }
      });

      process.on('close', code => {
        // Most package managers return 0 for version commands
        resolve(code === 0 && hasOutput);
      });

      process.on('error', () => {
        resolve(false);
      });

      // Timeout fallback
      setTimeout(() => {
        process.kill();
        resolve(false);
      }, 5000);
    });
  }

  /**
   * Check if SWI-Prolog is already installed via package managers
   */
  public async checkExistingInstallation(): Promise<{
    isInstalled: boolean;
    packageManager?: string;
    version?: string;
    packageName?: string;
  }> {
    const availableManagers = await this.detectAvailableManagers();

    for (const manager of availableManagers) {
      try {
        const isInstalled = await this.checkPackageInstallation(manager);
        if (isInstalled.installed) {
          const result: {
            isInstalled: boolean;
            packageManager: string;
            version?: string;
            packageName: string;
          } = {
            isInstalled: true,
            packageManager: manager.name,
            packageName: manager.packageName,
          };
          if (isInstalled.version !== undefined) {
            result.version = isInstalled.version;
          }
          return result;
        }
      } catch (_error) {
        // Continue checking other managers
        continue;
      }
    }

    return { isInstalled: false };
  }

  /**
   * Check if a package is installed via a specific package manager
   */
  private async checkPackageInstallation(manager: PackageManagerInfo): Promise<{
    installed: boolean;
    version?: string;
  }> {
    return new Promise(resolve => {
      let checkCommand: string;
      let expectedOutput: RegExp;

      switch (manager.name) {
        case 'winget':
          checkCommand = `winget list ${manager.packageName}`;
          expectedOutput = /SWI\.SWI-Prolog/i;
          break;
        case 'chocolatey':
          checkCommand = `choco list ${manager.packageName} --local-only`;
          expectedOutput = /swi-prolog/i;
          break;
        case 'scoop':
          checkCommand = `scoop list ${manager.packageName}`;
          expectedOutput = /swi-prolog/i;
          break;
        case 'homebrew':
          checkCommand = `brew list ${manager.packageName}`;
          expectedOutput = /swi-prolog/i;
          break;
        case 'macports':
          checkCommand = `port installed ${manager.packageName}`;
          expectedOutput = /swi-prolog/i;
          break;
        case 'apt':
          checkCommand = `dpkg -l ${manager.packageName}`;
          expectedOutput = /^ii\s+swi-prolog/m;
          break;
        case 'dnf':
        case 'yum':
          checkCommand = `rpm -q ${manager.packageName}`;
          expectedOutput = /^pl-/m;
          break;
        case 'pacman':
          checkCommand = `pacman -Q ${manager.packageName}`;
          expectedOutput = /swi-prolog/i;
          break;
        case 'zypper':
          checkCommand = `zypper search --installed-only ${manager.packageName}`;
          expectedOutput = /swi-prolog/i;
          break;
        case 'snap':
          checkCommand = `snap list ${manager.packageName}`;
          expectedOutput = /swi-prolog/i;
          break;
        case 'flatpak':
          checkCommand = `flatpak list | grep ${manager.packageName}`;
          expectedOutput = /org\.swi_prolog\.SWI-Prolog/i;
          break;
        default:
          resolve({ installed: false });
          return;
      }

      const [command, ...args] = checkCommand.split(' ');
      if (typeof command !== 'string' || !command.trim()) {
        resolve({ installed: false });
        return;
      }
      // Ensure command is a non-empty string and all args are strings
      const safeCommand: string = command.trim();
      const safeArgs: string[] = args.filter((arg): arg is string => typeof arg === 'string');
      if (!safeCommand) {
        resolve({ installed: false });
        return;
      }
      const process: import('child_process').ChildProcess = spawn(safeCommand, safeArgs, {
        stdio: ['ignore', 'pipe', 'pipe'],
        timeout: 10000,
      });

      let output = '';
      process.stdout?.on('data', data => {
        output += data.toString();
      });

      process.on('close', code => {
        if (code === 0 && expectedOutput.test(output)) {
          // Try to extract version
          const versionMatch = output.match(/(\d+\.\d+(?:\.\d+)?)/);
          if (versionMatch && versionMatch[1]) {
            resolve({
              installed: true,
              version: versionMatch[1],
            });
          } else {
            resolve({
              installed: true
            });
          }
        } else {
          resolve({ installed: false });
        }
      });

      process.on('error', () => {
        resolve({ installed: false });
      });

      setTimeout(() => {
        process.kill();
        resolve({ installed: false });
      }, 10000);
    });
  }

  /**
   * Install SWI-Prolog using the best available package manager
   */
  public async installSwiplProlog(preferredManager?: string): Promise<InstallationResult> {
    const availableManagers = await this.detectAvailableManagers();

    if (availableManagers.length === 0) {
      return {
        success: false,
        packageManager: 'none',
        command: '',
        error: 'No package managers found on this system',
      };
    }

    // Use preferred manager if specified and available
    let selectedManager = availableManagers[0]; // Default to highest priority
    if (preferredManager) {
      const preferred = availableManagers.find(m => m.name === preferredManager);
      if (preferred) {
        selectedManager = preferred;
      }
    }

    if (!selectedManager) {
      return {
        success: false,
        packageManager: 'none',
        command: '',
        error: 'No valid package manager selected',
      };
    }
    return await this.executeInstallation(selectedManager);
  }

  /**
   * Execute the installation using a specific package manager
   */
  private async executeInstallation(manager: PackageManagerInfo): Promise<InstallationResult> {
    return new Promise(resolve => {
      const [command, ...args] = manager.installCommand.split(' ');
      if (!command) {
        throw new Error('Install command must be a string');
      }
      // Check if command requires elevation
      const requiresElevation =
        manager.installCommand.includes('sudo') ||
        (this.platform === 'windows' && ['choco', 'winget'].includes(manager.name));

      const process: import('child_process').ChildProcess = spawn(command, args, {
        stdio: ['pipe', 'pipe', 'pipe'],
        timeout: 300000, // 5 minutes timeout for installation
        shell: true, // Use shell for complex commands
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
        const success = code === 0;
        if (success) {
          resolve({
            success,
            packageManager: manager.name,
            command: manager.installCommand,
            output: stdout,
            requiresElevation,
          });
        } else {
          resolve({
            success,
            packageManager: manager.name,
            command: manager.installCommand,
            output: stdout,
            error: stderr,
            requiresElevation,
          });
        }
      });

      process.on('error', error => {
        resolve({
          success: false,
          packageManager: manager.name,
          command: manager.installCommand,
          error: error.message,
          requiresElevation,
        });
      });

      // Timeout fallback
      setTimeout(() => {
        process.kill();
        resolve({
          success: false,
          packageManager: manager.name,
          command: manager.installCommand,
          error: 'Installation timed out after 5 minutes',
          requiresElevation,
        });
      }, 300000);
    });
  }

  /**
   * Show installation dialog with package manager options
   */
  public async showInstallationDialog(): Promise<InstallationResult | null> {
    const availableManagers = await this.detectAvailableManagers();

    if (availableManagers.length === 0) {
      vscode.window.showErrorMessage(
        'No package managers found on your system. Please install SWI-Prolog manually from https://www.swi-prolog.org/download/stable'
      );
      return null;
    }

    // Create quick pick items
    const items = availableManagers.map(manager => ({
      label: manager.displayName,
      description: `Install using: ${manager.installCommand}`,
      detail: manager.version ? `Version: ${manager.version}` : 'Available',
      manager: manager.name,
    }));

    items.push({
      label: 'Manual Installation',
      description: 'Download and install manually',
      detail: 'Opens the official SWI-Prolog download page',
      manager: 'manual',
    });

    const selected = await vscode.window.showQuickPick(items, {
      title: 'Install SWI-Prolog',
      placeHolder: 'Choose an installation method',
    });

    if (!selected) {
      return null; // User cancelled
    }

    if (selected.manager === 'manual') {
      vscode.env.openExternal(vscode.Uri.parse('https://www.swi-prolog.org/download/stable'));
      return null;
    }

    // Show confirmation dialog
    const manager = availableManagers.find(m => m.name === selected.manager)!;
    const requiresElevation =
      manager.installCommand.includes('sudo') ||
      (this.platform === 'windows' && ['choco', 'winget'].includes(manager.name));

    let confirmMessage = `Install SWI-Prolog using ${manager.displayName}?\n\nCommand: ${manager.installCommand}`;
    if (requiresElevation) {
      confirmMessage += '\n\nNote: This command requires administrator/sudo privileges.';
    }

    const confirm = await vscode.window.showInformationMessage(
      confirmMessage,
      { modal: true },
      'Install',
      'Cancel'
    );

    if (confirm !== 'Install') {
      return null;
    }

    // Show progress and execute installation
    return vscode.window.withProgress(
      {
        location: vscode.ProgressLocation.Notification,
        title: `Installing SWI-Prolog via ${manager.displayName}`,
        cancellable: false,
      },
      async progress => {
        progress.report({ message: 'Starting installation...' });

        const result = await this.executeInstallation(manager);

        if (result.success) {
          progress.report({ message: 'Installation completed successfully!' });
          vscode.window.showInformationMessage(
            `SWI-Prolog has been installed successfully using ${manager.displayName}!`
          );
        } else {
          let errorMessage = `Installation failed using ${manager.displayName}.`;
          if (result.requiresElevation) {
            errorMessage += ' Make sure you have administrator/sudo privileges.';
          }
          if (result.error) {
            errorMessage += `\n\nError: ${result.error}`;
          }

          vscode.window.showErrorMessage(errorMessage);
        }

        return result;
      }
    );
  }

  /**
   * Get installation suggestions for the current platform
   */
  public async getInstallationSuggestions(): Promise<string[]> {
    const availableManagers = await this.detectAvailableManagers();
    const suggestions: string[] = [];

    suggestions.push('Install SWI-Prolog using one of these methods:');
    suggestions.push('');

    if (availableManagers.length > 0) {
      suggestions.push('Available package managers:');
      availableManagers.forEach(manager => {
        suggestions.push(`  • ${manager.displayName}: ${manager.installCommand}`);
      });
      suggestions.push('');
    }

    // Add manual installation option
    suggestions.push('Manual installation:');
    suggestions.push('  • Download from: https://www.swi-prolog.org/download/stable');

    // Add platform-specific notes
    switch (this.platform) {
      case 'windows':
        suggestions.push('');
        suggestions.push('Windows notes:');
        suggestions.push('  • Run terminal as Administrator for package manager installations');
        suggestions.push('  • Add SWI-Prolog to PATH after manual installation');
        break;
      case 'macos':
        suggestions.push('');
        suggestions.push('macOS notes:');
        suggestions.push('  • Homebrew is recommended for easy installation and updates');
        suggestions.push('  • For Apple Silicon Macs, ensure you use the arm64 version');
        break;
      case 'linux':
        suggestions.push('');
        suggestions.push('Linux notes:');
        suggestions.push('  • Package names may vary between distributions');
        suggestions.push('  • Some distributions use "pl" instead of "swi-prolog"');
        break;
    }

    return suggestions;
  }

  /**
   * Clear detection cache (useful for testing or after system changes)
   */
  public clearCache(): void {
    this.detectionCache.clear();
  }

  /**
   * Get all configured package managers for the current platform
   */
  public getConfiguredManagers(): PackageManagerInfo[] {
    return Array.from(this.availableManagers.values());
  }

  /**
   * Get platform-specific package manager recommendations
   */
  public getRecommendedManagers(): string[] {
    switch (this.platform) {
      case 'windows':
        return ['winget', 'chocolatey', 'scoop'];
      case 'macos':
        return ['homebrew', 'macports'];
      case 'linux':
        return ['apt', 'dnf', 'yum', 'pacman', 'zypper', 'snap', 'flatpak'];
      default:
        return [];
    }
  }
}
