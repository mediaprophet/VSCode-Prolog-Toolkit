import * as fs from 'fs';
import * as path from 'path';

export interface LocalizationStrings {
  [key: string]: string | LocalizationStrings;
}

export interface LocaleInfo {
  code: string;
  name: string;
  nativeName: string;
  direction: 'ltr' | 'rtl';
}

export class LocalizationManager {
  private currentLocale: string = 'en';
  private fallbackLocale: string = 'en';
  private strings: Map<string, LocalizationStrings> = new Map();
  private availableLocales: Map<string, LocaleInfo> = new Map();
  private extensionPath: string;

  constructor(extensionPath: string) {
    this.extensionPath = extensionPath;
    this.initializeAvailableLocales();
    this.loadDefaultStrings();
  }

  /**
   * Initialize available locales
   */
  private initializeAvailableLocales(): void {
    this.availableLocales.set('en', {
      code: 'en',
      name: 'English',
      nativeName: 'English',
      direction: 'ltr',
    });

    this.availableLocales.set('es', {
      code: 'es',
      name: 'Spanish',
      nativeName: 'Español',
      direction: 'ltr',
    });

    this.availableLocales.set('fr', {
      code: 'fr',
      name: 'French',
      nativeName: 'Français',
      direction: 'ltr',
    });

    this.availableLocales.set('de', {
      code: 'de',
      name: 'German',
      nativeName: 'Deutsch',
      direction: 'ltr',
    });

    this.availableLocales.set('it', {
      code: 'it',
      name: 'Italian',
      nativeName: 'Italiano',
      direction: 'ltr',
    });

    this.availableLocales.set('pt', {
      code: 'pt',
      name: 'Portuguese',
      nativeName: 'Português',
      direction: 'ltr',
    });

    this.availableLocales.set('ja', {
      code: 'ja',
      name: 'Japanese',
      nativeName: '日本語',
      direction: 'ltr',
    });

    this.availableLocales.set('zh', {
      code: 'zh',
      name: 'Chinese',
      nativeName: '中文',
      direction: 'ltr',
    });
  }

  /**
   * Load default English strings
   */
  private loadDefaultStrings(): void {
    const defaultStrings: LocalizationStrings = {
      // General UI
      general: {
        yes: 'Yes',
        no: 'No',
        ok: 'OK',
        cancel: 'Cancel',
        error: 'Error',
        warning: 'Warning',
        info: 'Information',
        success: 'Success',
        loading: 'Loading...',
        processing: 'Processing...',
        complete: 'Complete',
        failed: 'Failed',
      },

      // Query results
      query: {
        succeeded: 'Query succeeded',
        failed: 'Query failed',
        error: 'Query error',
        noResults: 'No results found',
        multipleResults: 'Multiple results found',
        solution: 'Solution',
        solutions: 'solutions',
        variable: 'Variable',
        variables: 'variables',
        binding: 'binding',
        bindings: 'bindings',
        noVariables: 'no variables',
        noBindings: 'no variable bindings',
        streamingResults: 'Streaming results',
        streamingComplete: 'Streaming complete',
        loadingMore: 'Loading more results...',
        outputTruncated: '... (output truncated)',
      },

      // Package management
      packages: {
        availablePacks: 'Available Packs',
        installedPacks: 'Installed Packs',
        outdatedPacks: 'Outdated Packs',
        packInstalled: 'Pack installed successfully',
        packUninstalled: 'Pack uninstalled successfully',
        packUpdated: 'Pack updated successfully',
        packNotFound: 'Pack not found',
        installationFailed: 'Installation failed',
        uninstallationFailed: 'Uninstallation failed',
        updateFailed: 'Update failed',
        searchResults: 'Search Results',
        noPacksFound: 'No packs found',
        packInfo: 'Pack Information',
        dependencies: 'Dependencies',
        conflicts: 'Conflicts',
        securityWarning: 'Security Warning',
        untrustedSource: 'This pack is from an untrusted source',
        proceedAnyway: 'Install Anyway',
        configuredServers: 'Configured Pack Servers',
        addServer: 'Add Server',
        removeServer: 'Remove Server',
        defaultServer: 'default',
      },

      // Error messages
      errors: {
        syntaxError: 'Syntax error in Prolog code',
        runtimeError: 'Runtime error occurred',
        systemError: 'System error occurred',
        networkError: 'Network error occurred',
        permissionError: 'Permission denied',
        resourceError: 'Resource limit exceeded',
        backendNotAvailable: 'Prolog backend is not available',
        backendTimeout: 'Backend request timed out',
        connectionFailed: 'Failed to connect to Prolog server',
        invalidCommand: 'Invalid or unrecognized command',
        missingArgument: 'Required argument is missing',
        invalidFilePath: 'Invalid file path specified',
        unknownError: 'An unknown error occurred',
      },

      // Error suggestions
      suggestions: {
        checkSyntax:
          'Check your Prolog syntax for missing periods, unmatched parentheses, or incorrect operators',
        checkTypes: 'Check the types of your arguments using var/1, atom/1, number/1, etc.',
        checkPermissions: 'Check file permissions or predicate access rights',
        restartBackend: 'Try restarting the extension or check your SWI-Prolog installation',
        checkConnection: 'Check your internet connection and proxy settings',
        checkCommand: 'Check the command syntax. Use /help to see available commands',
        checkFilePath: 'Check if the file path exists and is accessible',
        simplifyQuery: 'Try simplifying your query or using call_with_time_limit/2',
        checkInstallation: 'Check your SWI-Prolog installation and version compatibility',
      },

      // Help system
      help: {
        helpFor: 'Help for',
        availableCommands: 'Available Commands',
        usage: 'Usage',
        examples: 'Examples',
        description: 'Description',
        parameters: 'Parameters',
        options: 'Options',
        seeAlso: 'See Also',
        documentation: 'Documentation',
        noHelpAvailable: 'No help available for this topic',
      },

      // Commands
      commands: {
        query: 'Execute a Prolog query',
        consult: 'Load a Prolog file',
        help: 'Get help on predicates or commands',
        pack: 'Manage Prolog packages',
        list: 'List items',
        install: 'Install a package',
        uninstall: 'Uninstall a package',
        update: 'Update packages',
        search: 'Search for packages',
        info: 'Get information about a package',
      },

      // Status messages
      status: {
        starting: 'Starting Prolog backend...',
        ready: 'Prolog backend is ready',
        stopping: 'Stopping Prolog backend...',
        stopped: 'Prolog backend stopped',
        restarting: 'Restarting Prolog backend...',
        connecting: 'Connecting to Prolog server...',
        connected: 'Connected to Prolog server',
        disconnected: 'Disconnected from Prolog server',
      },

      // Progress messages
      progress: {
        processing: 'Processing request...',
        loading: 'Loading data...',
        saving: 'Saving changes...',
        installing: 'Installing package...',
        uninstalling: 'Uninstalling package...',
        updating: 'Updating package...',
        searching: 'Searching packages...',
        analyzing: 'Analyzing results...',
      },
    };

    this.strings.set('en', defaultStrings);
  }

  /**
   * Set the current locale
   */
  async setLocale(locale: string): Promise<boolean> {
    if (!this.availableLocales.has(locale)) {
      console.warn(`Locale '${locale}' is not available`);
      return false;
    }

    this.currentLocale = locale;

    // Load locale strings if not already loaded
    if (!this.strings.has(locale) && locale !== 'en') {
      await this.loadLocaleStrings(locale);
    }

    return true;
  }

  /**
   * Load strings for a specific locale
   */
  private async loadLocaleStrings(locale: string): Promise<void> {
    try {
      const localeFile = path.join(this.extensionPath, 'locales', `${locale}.json`);

      if (fs.existsSync(localeFile)) {
        const content = fs.readFileSync(localeFile, 'utf8');
        const strings = JSON.parse(content) as LocalizationStrings;
        this.strings.set(locale, strings);
      } else {
        console.warn(`Locale file not found: ${localeFile}`);
      }
    } catch (error) {
      console.error(`Failed to load locale strings for '${locale}':`, error);
    }
  }

  /**
   * Get localized string
   */
  getString(key: string, ...args: any[]): string {
    const value =
      this.getStringValue(key, this.currentLocale) ||
      this.getStringValue(key, this.fallbackLocale) ||
      key;

    // Simple placeholder replacement
    if (args.length > 0) {
      return this.formatString(value, args);
    }

    return value;
  }

  /**
   * Get string value from nested object
   */
  private getStringValue(key: string, locale: string): string | undefined {
    const strings = this.strings.get(locale);
    if (!strings) {
      return undefined;
    }

    const keys = key.split('.');
    let current: any = strings;

    for (const k of keys) {
      if (current && typeof current === 'object' && k in current) {
        current = current[k];
      } else {
        return undefined;
      }
    }

    return typeof current === 'string' ? current : undefined;
  }

  /**
   * Format string with arguments
   */
  private formatString(template: string, args: any[]): string {
    return template.replace(/\{(\d+)\}/g, (match, index) => {
      const argIndex = parseInt(index, 10);
      return argIndex < args.length ? String(args[argIndex]) : match;
    });
  }

  /**
   * Get current locale
   */
  getCurrentLocale(): string {
    return this.currentLocale;
  }

  /**
   * Get available locales
   */
  getAvailableLocales(): LocaleInfo[] {
    return Array.from(this.availableLocales.values());
  }

  /**
   * Get locale info
   */
  getLocaleInfo(locale: string): LocaleInfo | undefined {
    return this.availableLocales.get(locale);
  }

  /**
   * Check if locale is available
   */
  isLocaleAvailable(locale: string): boolean {
    return this.availableLocales.has(locale);
  }

  /**
   * Get localized error message
   */
  getErrorMessage(errorCode: string, context?: any): string {
    const key = `errors.${errorCode.toLowerCase()}`;
    let message = this.getString(key);

    if (message === key) {
      // Fallback to generic error message
      message = this.getString('errors.unknownError');
    }

    if (context) {
      message = this.formatString(message, [context]);
    }

    return message;
  }

  /**
   * Get localized suggestion
   */
  getSuggestion(suggestionCode: string): string {
    const key = `suggestions.${suggestionCode.toLowerCase()}`;
    return this.getString(key);
  }

  /**
   * Get pluralized string
   */
  getPlural(key: string, count: number, ...args: any[]): string {
    const singularKey = `${key}`;
    const pluralKey = `${key}s`;

    const selectedKey = count === 1 ? singularKey : pluralKey;
    return this.getString(selectedKey, count, ...args);
  }

  /**
   * Format number according to locale
   */
  formatNumber(number: number): string {
    try {
      return new Intl.NumberFormat(this.currentLocale).format(number);
    } catch {
      return number.toString();
    }
  }

  /**
   * Format date according to locale
   */
  formatDate(date: Date): string {
    try {
      return new Intl.DateTimeFormat(this.currentLocale).format(date);
    } catch {
      return date.toISOString();
    }
  }

  /**
   * Get text direction for current locale
   */
  getTextDirection(): 'ltr' | 'rtl' {
    const localeInfo = this.availableLocales.get(this.currentLocale);
    return localeInfo?.direction || 'ltr';
  }

  /**
   * Add custom strings for a locale
   */
  addStrings(locale: string, strings: LocalizationStrings): void {
    const existing = this.strings.get(locale) || {};
    const merged = this.mergeStrings(existing, strings);
    this.strings.set(locale, merged);
  }

  /**
   * Merge string objects recursively
   */
  private mergeStrings(
    target: LocalizationStrings,
    source: LocalizationStrings
  ): LocalizationStrings {
    const result = { ...target };

    for (const [key, value] of Object.entries(source)) {
      if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
        result[key] = this.mergeStrings((result[key] as LocalizationStrings) || {}, value);
      } else {
        result[key] = value;
      }
    }

    return result;
  }

  /**
   * Export strings for a locale (for translation)
   */
  exportStrings(locale: string): LocalizationStrings | undefined {
    return this.strings.get(locale);
  }

  /**
   * Get missing translations for a locale
   */
  getMissingTranslations(locale: string): string[] {
    const baseStrings = this.strings.get(this.fallbackLocale);
    const localeStrings = this.strings.get(locale);

    if (!baseStrings || !localeStrings) {
      return [];
    }

    const missing: string[] = [];
    this.findMissingKeys(baseStrings, localeStrings, '', missing);
    return missing;
  }

  /**
   * Find missing keys recursively
   */
  private findMissingKeys(
    base: LocalizationStrings,
    target: LocalizationStrings,
    prefix: string,
    missing: string[]
  ): void {
    for (const [key, value] of Object.entries(base)) {
      const fullKey = prefix ? `${prefix}.${key}` : key;

      if (!(key in target)) {
        missing.push(fullKey);
      } else if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
        if (
          typeof target[key] === 'object' &&
          target[key] !== null &&
          !Array.isArray(target[key])
        ) {
          this.findMissingKeys(value, target[key], fullKey, missing);
        } else {
          missing.push(fullKey);
        }
      }
    }
  }
}

/**
 * Global localization manager instance
 */
let globalLocalizationManager: LocalizationManager | undefined;

/**
 * Initialize localization manager
 */
export function initializeLocalization(extensionPath: string): LocalizationManager {
  globalLocalizationManager = new LocalizationManager(extensionPath);
  return globalLocalizationManager;
}

/**
 * Get global localization manager
 */
export function getLocalizationManager(): LocalizationManager {
  if (!globalLocalizationManager) {
    throw new Error('Localization manager not initialized');
  }
  return globalLocalizationManager;
}

/**
 * Convenience function to get localized string
 */
export function t(key: string, ...args: any[]): string {
  return getLocalizationManager().getString(key, ...args);
}

/**
 * Convenience function to get pluralized string
 */
export function tp(key: string, count: number, ...args: any[]): string {
  return getLocalizationManager().getPlural(key, count, ...args);
}
