import * as vscode from 'vscode';
import { InstallationChecker } from './installationChecker.js';

export interface MigrationResult {
  migrated: boolean;
  oldPath?: string;
  newPath?: string;
  backupCreated?: boolean;
  issues?: string[];
}

export interface ConfigurationBackup {
  timestamp: string;
  configuration: any;
  reason: string;
}

export class ConfigurationMigration {
  private static instance: ConfigurationMigration;
  private installationChecker: InstallationChecker;

  private constructor() {
    this.installationChecker = InstallationChecker.getInstance();
  }

  public static getInstance(): ConfigurationMigration {
    if (!ConfigurationMigration.instance) {
      ConfigurationMigration.instance = new ConfigurationMigration();
    }
    return ConfigurationMigration.instance;
  }

  /**
   * Perform automatic configuration migration
   */
  public async performMigration(): Promise<MigrationResult> {
    const config = vscode.workspace.getConfiguration('prolog');
    const currentPath = config.get<string>('executablePath', 'swipl');

    // Check if current path is valid
    const isCurrentValid = await this.installationChecker.validateSwiplPath(currentPath);

    if (isCurrentValid) {
      return {
        migrated: false,
        issues: [],
      };
    }

    // Current path is invalid, try to find a valid one
    const foundPath = await this.installationChecker.findSwiplExecutable();

    if (!foundPath) {
      return {
        migrated: false,
        oldPath: currentPath,
        issues: ['No valid SWI-Prolog installation found for migration'],
      };
    }

    // Create backup before migration
    const backupResult = await this.createConfigurationBackup('automatic_migration');

    try {
      // Update configuration
      await config.update('executablePath', foundPath, vscode.ConfigurationTarget.Global);

      return {
        migrated: true,
        oldPath: currentPath,
        newPath: foundPath,
        backupCreated: backupResult,
        issues: [],
      };
    } catch (error) {
      return {
        migrated: false,
        oldPath: currentPath,
        backupCreated: backupResult,
        issues: [`Failed to update configuration: ${error}`],
      };
    }
  }

  /**
   * Detect and handle outdated or invalid paths
   */
  public async detectOutdatedPaths(): Promise<{
    hasOutdatedPaths: boolean;
    invalidPaths: string[];
    suggestions: Array<{ path: string; version?: string }>;
  }> {
    const config = vscode.workspace.getConfiguration('prolog');
    const currentPath = config.get<string>('executablePath', 'swipl');
    const invalidPaths: string[] = [];
    const suggestions: Array<{ path: string; version?: string }> = [];

    // Check current executable path
    const isCurrentValid = await this.installationChecker.validateSwiplPath(currentPath);
    if (!isCurrentValid) {
      invalidPaths.push(currentPath);
    }

    // Check for common outdated paths
    const commonOutdatedPaths = this.getCommonOutdatedPaths();
    for (const outdatedPath of commonOutdatedPaths) {
      if (currentPath.includes(outdatedPath)) {
        invalidPaths.push(currentPath);
        break;
      }
    }

    // Find valid alternatives
    const foundPath = await this.installationChecker.findSwiplExecutable();
    if (typeof foundPath === 'string' && foundPath !== null) {
      const version = await this.installationChecker.getSwiplVersion(foundPath);
      if (foundPath !== null) {
        if (typeof version === 'string') {
          suggestions.push({ path: foundPath as string, version });
        } else {
          suggestions.push({ path: foundPath as string });
        }
      }
    }

    return {
      hasOutdatedPaths: invalidPaths.length > 0,
      invalidPaths,
      suggestions,
    };
  }

  /**
   * Attempt to find new valid paths for invalid configurations
   */
  public async findNewValidPaths(): Promise<Array<{ path: string; version?: string }>> {
    const validPaths: Array<{ path: string; version?: string }> = [];
    const commonPaths = InstallationChecker.detectCommonInstallPaths();

    for (const path of commonPaths) {
      if (typeof path === 'string' && path !== null) {
        const isValid = await this.installationChecker.validateSwiplPath(path);
        if (isValid) {
          const version = await this.installationChecker.getSwiplVersion(path);
          if (path !== null) {
            if (typeof version === 'string') {
              validPaths.push({ path: path as string, version });
            } else {
              validPaths.push({ path: path as string });
            }
          }
        }
      }
    }

    return validPaths;
  }

  /**
   * Create a backup of current configuration
   */
  public async createConfigurationBackup(reason: string): Promise<boolean> {
    try {
      const config = vscode.workspace.getConfiguration('prolog');
      const backup: ConfigurationBackup = {
        timestamp: new Date().toISOString(),
        reason,
        configuration: {
          executablePath: config.get('executablePath'),
          dialect: config.get('dialect'),
          'linter.run': config.get('linter.run'),
          'linter.delay': config.get('linter.delay'),
          'linter.enableMsgInOutput': config.get('linter.enableMsgInOutput'),
          'format.addSpace': config.get('format.addSpace'),
          'terminal.runtimeArgs': config.get('terminal.runtimeArgs'),
          'telemetry.enabled': config.get('telemetry.enabled'),
        },
      };

      // Store backup in global state (VS Code's storage)
      const context = this.getExtensionContext();
      if (context) {
        let existingBackups = context.globalState.get<ConfigurationBackup[]>(
          'prologConfigBackups',
          []
        );
        if (!existingBackups) {
          existingBackups = [];
        }
        existingBackups.push(backup);
        // Keep only last 10 backups
        if (existingBackups.length > 10) {
          existingBackups.splice(0, existingBackups.length - 10);
        }
        await context.globalState.update('prologConfigBackups', existingBackups);
        return true;
      }
      return false;
    } catch (error) {
      console.error('Failed to create configuration backup:', error);
      return false;
    }
  }

  /**
   * Restore configuration from backup
   */
  public async restoreConfigurationBackup(backupIndex: number = 0): Promise<boolean> {
    try {
      const context = this.getExtensionContext();
      if (!context) {
        return false;
      }

      const backups = context.globalState.get<ConfigurationBackup[]>('prologConfigBackups', []);
      if (backups.length === 0 || backupIndex >= backups.length) {
        return false;
      }

      const backup = backups[backups.length - 1 - backupIndex]; // Most recent first
      const config = vscode.workspace.getConfiguration('prolog');

      // Restore each configuration value
      if (backup && backup.configuration) {
        for (const [key, value] of Object.entries(backup.configuration)) {
          if (value !== undefined) {
            await config.update(key, value, vscode.ConfigurationTarget.Global);
          }
        }
      }

      return true;
    } catch (error) {
      console.error('Failed to restore configuration backup:', error);
      return false;
    }
  }

  /**
   * Get list of available backups
   */
  public getConfigurationBackups(): ConfigurationBackup[] {
    const context = this.getExtensionContext();
    if (!context) {
      return [];
    }

    return context.globalState.get<ConfigurationBackup[]>('prologConfigBackups', []);
  }

  /**
   * Handle migration of different SWI-Prolog versions
   */
  public async handleVersionMigration(
    oldVersion: string,
    newVersion: string
  ): Promise<{
    compatibilityIssues: string[];
    recommendations: string[];
  }> {
    const compatibilityIssues: string[] = [];
    const recommendations: string[] = [];

    try {
      const oldVersionParts = oldVersion.split('.').map(v => parseInt(v, 10));
      const newVersionParts = newVersion.split('.').map(v => parseInt(v, 10));

      // Check for major version changes
      if (oldVersionParts[0] !== newVersionParts[0]) {
        compatibilityIssues.push(
          `Major version change from ${oldVersionParts[0]} to ${newVersionParts[0]} may affect compatibility`
        );
        recommendations.push(
          'Review your Prolog code for compatibility with the new major version'
        );
      }

      // Check for specific version-related issues
      if (
        Array.isArray(oldVersionParts) &&
        Array.isArray(newVersionParts) &&
        oldVersionParts.length > 0 &&
        newVersionParts.length > 0 &&
        typeof oldVersionParts[0] === 'number' &&
        typeof newVersionParts[0] === 'number'
      ) {
        if (oldVersionParts[0] < 8 && newVersionParts[0] >= 8) {
          recommendations.push('SWI-Prolog 8.x introduced new features and some syntax changes');
          recommendations.push(
            'Consider updating your code to use new string syntax if applicable'
          );
        }
        if (oldVersionParts[0] < 9 && newVersionParts[0] >= 9) {
          recommendations.push(
            'SWI-Prolog 9.x has improved performance and new built-in predicates'
          );
        }
      }
    } catch (_error) {
      compatibilityIssues.push('Unable to parse version numbers for compatibility check');
    }

    return {
      compatibilityIssues,
      recommendations,
    };
  }

  /**
   * Preserve user customizations during migration
   */
  public async preserveUserCustomizations(): Promise<{
    preserved: string[];
    issues: string[];
  }> {
    const preserved: string[] = [];
    const issues: string[] = [];

    try {
      const config = vscode.workspace.getConfiguration('prolog');

      // List of settings to preserve (non-path related)
      const settingsToPreserve = [
        'dialect',
        'linter.run',
        'linter.delay',
        'linter.enableMsgInOutput',
        'format.addSpace',
        'terminal.runtimeArgs',
        'telemetry.enabled',
      ];

      for (const setting of settingsToPreserve) {
        const value = config.get(setting);
        if (value !== undefined) {
          preserved.push(`${setting}: ${JSON.stringify(value)}`);
        }
      }
    } catch (error) {
      issues.push(`Failed to preserve customizations: ${error}`);
    }

    return {
      preserved,
      issues,
    };
  }

  /**
   * Show migration dialog to user
   */
  public async showMigrationDialog(migrationResult: MigrationResult): Promise<void> {
    if (migrationResult.migrated) {
      const action = await vscode.window.showInformationMessage(
        `Configuration migrated successfully!\n\nOld path: ${migrationResult.oldPath}\nNew path: ${migrationResult.newPath}`,
        'OK',
        'Undo Migration'
      );

      if (action === 'Undo Migration') {
        const restored = await this.restoreConfigurationBackup(0);
        if (restored) {
          vscode.window.showInformationMessage('Configuration restored from backup');
        } else {
          vscode.window.showErrorMessage('Failed to restore configuration backup');
        }
      }
    } else if (migrationResult.issues && migrationResult.issues.length > 0) {
      const issueMessage = migrationResult.issues.join('\n');
      await vscode.window.showWarningMessage(
        `Configuration migration failed:\n\n${issueMessage}`,
        'OK'
      );
    }
  }

  /**
   * Get common outdated paths that should be migrated
   */
  private getCommonOutdatedPaths(): string[] {
    return [
      '/usr/local/bin/pl', // Old SWI-Prolog executable name
      '/usr/bin/pl', // Old SWI-Prolog executable name
      'C:\\pl\\bin\\pl.exe', // Old Windows path
      '/opt/pl/', // Old installation directory
    ];
  }

  /**
   * Get extension context (this would need to be set by the extension)
   */
  private getExtensionContext(): vscode.ExtensionContext | undefined {
    // This would be set by the extension when initializing the migration system
    return (global as any).prologExtensionContext;
  }

  /**
   * Set extension context for storage operations
   */
  public setExtensionContext(context: vscode.ExtensionContext): void {
    (global as any).prologExtensionContext = context;
  }

  /**
   * Perform comprehensive migration check and handle user interaction
   */
  public async performComprehensiveMigration(): Promise<void> {
    try {
      // Check for outdated paths
      const outdatedCheck = await this.detectOutdatedPaths();

      if (outdatedCheck.hasOutdatedPaths) {
        const action = await vscode.window.showWarningMessage(
          `Outdated SWI-Prolog configuration detected.\n\nInvalid paths: ${outdatedCheck.invalidPaths.join(', ')}\n\nWould you like to automatically migrate to a valid installation?`,
          'Migrate Now',
          'Show Suggestions',
          'Skip'
        );

        switch (action) {
          case 'Migrate Now': {
            const migrationResult = await this.performMigration();
            await this.showMigrationDialog(migrationResult);
            break;
          }

          case 'Show Suggestions':
            await this.showSuggestionsDialog(outdatedCheck.suggestions);
            break;
        }
      }
    } catch (error) {
      console.error('Error during comprehensive migration:', error);
      vscode.window.showErrorMessage(`Migration check failed: ${error}`);
    }
  }

  /**
   * Show suggestions dialog for manual path selection
   */
  private async showSuggestionsDialog(
    suggestions: Array<{ path: string; version?: string }>
  ): Promise<void> {
    if (suggestions.length === 0) {
      vscode.window.showWarningMessage('No valid SWI-Prolog installations found for migration.');
      return;
    }

    const items = suggestions.map(suggestion => ({
      label: suggestion.path,
      description: suggestion.version ? `Version ${suggestion.version}` : 'Version unknown',
      path: suggestion.path,
    }));

    const selected = await vscode.window.showQuickPick(items, {
      placeHolder: 'Select a SWI-Prolog installation to use',
      ignoreFocusOut: true,
    });

    if (selected) {
      const config = vscode.workspace.getConfiguration('prolog');
      await this.createConfigurationBackup('manual_path_selection');
      await config.update('executablePath', selected.path, vscode.ConfigurationTarget.Global);
      vscode.window.showInformationMessage(`Configuration updated to use: ${selected.path}`);
    }
  }
}
