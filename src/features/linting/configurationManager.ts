import { workspace } from 'vscode';
import * as which from 'which';
import { ExecutableFinder } from '../../utils/executableFinder';
import { PlatformUtils } from '../../utils/platformUtils';
import { IConfigurationManager, ILinterConfiguration, RunTrigger } from './interfaces';

/**
 * Manages linter configuration, executable path resolution, and settings
 */
export class ConfigurationManager implements IConfigurationManager {
  private _configuration: ILinterConfiguration | null = null;

  /**
   * Load configuration settings from VS Code workspace
   */
  public async loadConfiguration(): Promise<ILinterConfiguration> {
    const section = workspace.getConfiguration('prolog');

    if (!section) {
      throw new Error('Prolog configuration section not found');
    }

    const executable = await this.resolveExecutablePath();
    const trigger = this.determineTrigger();
    const delay = section.get<number>('linter.delay', 500);
    const enableOutput = section.get<boolean>('linter.enableMsgInOutput', false);

    this._configuration = {
      executable,
      trigger,
      delay,
      enableOutput,
    };

    return this._configuration;
  }

  /**
   * Resolve the executable path with enhanced validation and permission checking
   */
  public async resolveExecutablePath(): Promise<string> {
    const section = workspace.getConfiguration('prolog');
    const configuredPath = section.get<string>(
      'executablePath',
      PlatformUtils.getDefaultExecutablePath()
    );

    // First try the configured path
    if (configuredPath && configuredPath !== PlatformUtils.getDefaultExecutablePath()) {
      const normalizedPath = PlatformUtils.normalizePath(configuredPath);

      // Check if the configured path exists and has proper permissions
      if (await PlatformUtils.pathExists(normalizedPath)) {
        if (await PlatformUtils.isExecutable(normalizedPath)) {
          return normalizedPath;
        } else {
          // Path exists but is not executable - this will be handled by the main linter
          console.warn(`[ConfigurationManager] Path exists but lacks execute permissions: ${normalizedPath}`);
        }
      }
    }

    // Try to find executable using comprehensive detection
    const executableFinder = new ExecutableFinder();
    const detectionResult = await executableFinder.findSwiplExecutable();

    if (detectionResult.found && detectionResult.path) {
      // Check permissions on the found executable
      if (detectionResult.permissions?.executable) {
        return detectionResult.path;
      } else {
        // Found executable but has permission issues
        console.warn(`[ConfigurationManager] Found executable with permission issues: ${detectionResult.path}`);
        return detectionResult.path; // Return the path anyway, the spawn will fail with a better error
      }
    }

    // Fallback to the configured path or default, even if it might not work
    try {
      const fallbackPath = which.sync(configuredPath);
      return fallbackPath;
    } catch (_e) {
      return PlatformUtils.normalizePath(configuredPath);
    }
  }

  /**
   * Determine the trigger type for the linter
   */
  private determineTrigger(): RunTrigger {
    // TODO: Refactor trigger logic to use new modular config if needed
    return RunTrigger.never;
  }

  /**
   * Get the current delay setting
   */
  public getDelay(): number {
    return this._configuration?.delay ?? 500;
  }

  /**
   * Get the current trigger setting
   */
  public getTrigger(): RunTrigger {
    return this._configuration?.trigger ?? RunTrigger.never;
  }

  /**
   * Check if output is enabled
   */
  public isOutputEnabled(): boolean {
    return this._configuration?.enableOutput ?? false;
  }

  /**
   * Get current configuration (readonly)
   */
  public getCurrentConfiguration(): ILinterConfiguration | null {
    return this._configuration;
  }

  /**
   * Reload configuration when settings change
   */
  public async reloadConfiguration(): Promise<ILinterConfiguration> {
    this._configuration = null;
    return this.loadConfiguration();
  }
}