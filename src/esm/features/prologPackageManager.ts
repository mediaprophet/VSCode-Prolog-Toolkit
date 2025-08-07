import type { CancellationToken, Progress } from 'vscode';
import { ProgressLocation, window } from 'vscode';
import { PrologBackend } from '../prologBackend.js';

export interface PrologPack {
  name: string;
  version?: string;
  title?: string;
  description?: string;
  author?: string;
  home?: string;
  download?: string;
  requires?: string[];
  conflicts?: string[];
  installed?: boolean;
  outdated?: boolean;
}

export interface PackageOperationResult {
  success: boolean;
  message: string;
  details?: string;
}

export class PrologPackageManager {
  private backend: PrologBackend;
  private packServers: string[] = [
    'https://www.swi-prolog.org/pack/list',
    // Add custom pack servers here
  ];

  constructor(backend: PrologBackend) {
    this.backend = backend;
  }

  /**
   * List all available SWI-Prolog packs from configured servers
   */
  async listAvailablePacks(): Promise<PrologPack[]> {
    try {
      const response = await this.backend.sendRequest('query', {
        goal: 'findall(pack(Name, Title, Version, Author, Home, Download), (pack_search(., Packs), member(pack(Name, Title, Version, Author, Home, Download), Packs)), AvailablePacks).',
      });

      if (response.status === 'ok' && response.results) {
        return this.parsePackList(response.results);
      }
      return [];
    } catch (error) {
      window.showErrorMessage(`Failed to list available packs: ${error}`);
      return [];
    }
  }

  /**
   * List installed SWI-Prolog packs
   */
  async listInstalledPacks(): Promise<PrologPack[]> {
    try {
      const response = await this.backend.sendRequest('query', {
        goal: 'findall(pack(Name, Title, Version, Author, Home), (pack_list_installed(Packs), member(pack(Name, _, _), Packs), pack_info(Name, title(Title)), pack_info(Name, version(Version)), pack_info(Name, author(Author)), pack_info(Name, home(Home))), InstalledPacks).',
      });

      if (response.status === 'ok' && response.results) {
        return this.parsePackList(response.results).map(pack => ({ ...pack, installed: true }));
      }
      return [];
    } catch (error) {
      window.showErrorMessage(`Failed to list installed packs: ${error}`);
      return [];
    }
  }

  /**
   * Install a Prolog pack by name
   */
  async installPack(packName: string): Promise<PackageOperationResult> {
    if (!this.validatePackName(packName)) {
      return {
        success: false,
        message:
          'Invalid pack name. Pack names should contain only alphanumeric characters, underscores, and hyphens.',
      };
    }

    return window.withProgress(
      {
        location: ProgressLocation.Notification,
        title: `Installing pack: ${packName}`,
        cancellable: true,
      },
      async (
        progress: Progress<{ message?: string; increment?: number }>,
        token: CancellationToken
      ) => {
        try {
          progress.report({ message: 'Checking pack availability...' });

          // Check if pack exists
          const availablePacks = await this.listAvailablePacks();
          const packExists = availablePacks.some(pack => pack.name === packName);

          if (!packExists) {
            return {
              success: false,
              message: `Pack '${packName}' not found in available packs.`,
            };
          }

          progress.report({ message: 'Installing pack...', increment: 50 });

          const response = await this.backend.sendRequest('query', {
            goal: `pack_install(${packName}, [interactive(false), upgrade(true)]).`,
            timeoutMs: 60000, // 60 seconds timeout for installation
          });

          if (token.isCancellationRequested) {
            return {
              success: false,
              message: 'Installation cancelled by user.',
            };
          }

          progress.report({ message: 'Verifying installation...', increment: 90 });

          if (response.status === 'ok') {
            // Verify installation
            const installedPacks = await this.listInstalledPacks();
            const isInstalled = installedPacks.some(pack => pack.name === packName);

            if (isInstalled) {
              return {
                success: true,
                message: `Pack '${packName}' installed successfully.`,
              };
            } else {
              return {
                success: false,
                message: `Pack '${packName}' installation completed but verification failed.`,
              };
            }
          } else {
            return {
              success: false,
              message: `Failed to install pack '${packName}': ${response.error || 'Unknown error'}`,
              details: response.message,
            };
          }
        } catch (error) {
          return {
            success: false,
            message: `Installation failed: ${error}`,
            details: error instanceof Error ? error.stack : undefined,
          };
        }
      }
    );
  }

  /**
   * Uninstall a Prolog pack by name
   */
  async uninstallPack(packName: string): Promise<PackageOperationResult> {
    if (!this.validatePackName(packName)) {
      return {
        success: false,
        message: 'Invalid pack name.',
      };
    }

    return window.withProgress(
      {
        location: ProgressLocation.Notification,
        title: `Uninstalling pack: ${packName}`,
        cancellable: false,
      },
      async (progress: Progress<{ message?: string; increment?: number }>) => {
        try {
          progress.report({ message: 'Checking if pack is installed...' });

          // Check if pack is installed
          const installedPacks = await this.listInstalledPacks();
          const isInstalled = installedPacks.some(pack => pack.name === packName);

          if (!isInstalled) {
            return {
              success: false,
              message: `Pack '${packName}' is not installed.`,
            };
          }

          progress.report({ message: 'Uninstalling pack...', increment: 50 });

          const response = await this.backend.sendRequest('query', {
            goal: `pack_remove(${packName}).`,
            timeoutMs: 30000, // 30 seconds timeout
          });

          progress.report({ message: 'Verifying removal...', increment: 90 });

          if (response.status === 'ok') {
            // Verify removal
            const remainingPacks = await this.listInstalledPacks();
            const stillInstalled = remainingPacks.some(pack => pack.name === packName);

            if (!stillInstalled) {
              return {
                success: true,
                message: `Pack '${packName}' uninstalled successfully.`,
              };
            } else {
              return {
                success: false,
                message: `Pack '${packName}' removal completed but verification failed.`,
              };
            }
          } else {
            return {
              success: false,
              message: `Failed to uninstall pack '${packName}': ${response.error || 'Unknown error'}`,
              details: response.message,
            };
          }
        } catch (error) {
          return {
            success: false,
            message: `Uninstallation failed: ${error}`,
            details: error instanceof Error ? error.stack : undefined,
          };
        }
      }
    );
  }

  /**
   * Update a Prolog pack by name
   */
  async updatePack(packName: string): Promise<PackageOperationResult> {
    if (!this.validatePackName(packName)) {
      return {
        success: false,
        message: 'Invalid pack name.',
      };
    }

    return window.withProgress(
      {
        location: ProgressLocation.Notification,
        title: `Updating pack: ${packName}`,
        cancellable: true,
      },
      async (
        progress: Progress<{ message?: string; increment?: number }>,
        token: CancellationToken
      ) => {
        try {
          progress.report({ message: 'Checking for updates...' });

          const response = await this.backend.sendRequest('query', {
            goal: `pack_upgrade(${packName}).`,
            timeoutMs: 60000, // 60 seconds timeout
          });

          if (token.isCancellationRequested) {
            return {
              success: false,
              message: 'Update cancelled by user.',
            };
          }

          progress.report({ message: 'Update completed', increment: 100 });

          if (response.status === 'ok') {
            return {
              success: true,
              message: `Pack '${packName}' updated successfully.`,
            };
          } else {
            return {
              success: false,
              message: `Failed to update pack '${packName}': ${response.error || 'Unknown error'}`,
              details: response.message,
            };
          }
        } catch (error) {
          return {
            success: false,
            message: `Update failed: ${error}`,
            details: error instanceof Error ? error.stack : undefined,
          };
        }
      }
    );
  }

  /**
   * Get detailed information about a pack
   */
  async getPackInfo(packName: string): Promise<PrologPack | null> {
    if (!this.validatePackName(packName)) {
      return null;
    }

    try {
      const response = await this.backend.sendRequest('query', {
        goal: `pack_info(${packName}, Info), findall(Key-Value, member(Key(Value), Info), Details).`,
      });

      if (response.status === 'ok' && response.results && response.results.length > 0) {
        return this.parsePackInfo(packName, response.results[0]);
      }
      return null;
    } catch (error) {
      window.showErrorMessage(`Failed to get pack info: ${error}`);
      return null;
    }
  }

  /**
   * Search for packs by keyword
   */
  async searchPacks(keyword: string): Promise<PrologPack[]> {
    if (!keyword.trim()) {
      return [];
    }

    try {
      const response = await this.backend.sendRequest('query', {
        goal: `pack_search('${keyword}', Packs).`,
      });

      if (response.status === 'ok' && response.results) {
        return this.parsePackList(response.results);
      }
      return [];
    } catch (error) {
      window.showErrorMessage(`Pack search failed: ${error}`);
      return [];
    }
  }

  /**
   * Add a custom pack server
   */
  addPackServer(serverUrl: string): void {
    if (!this.packServers.includes(serverUrl)) {
      this.packServers.push(serverUrl);
    }
  }

  /**
   * Remove a custom pack server
   */
  removePackServer(serverUrl: string): void {
    const index = this.packServers.indexOf(serverUrl);
    if (index > 0) {
      // Don't remove the default server at index 0
      this.packServers.splice(index, 1);
    }
  }

  /**
   * Get list of configured pack servers
   */
  getPackServers(): string[] {
    return [...this.packServers];
  }

  /**
   * Check for outdated packs
   */
  async checkOutdatedPacks(): Promise<PrologPack[]> {
    try {
      const response = await this.backend.sendRequest('query', {
        goal: 'findall(pack(Name, CurrentVersion, LatestVersion), (pack_list_installed(Packs), member(pack(Name, _, _), Packs), pack_info(Name, version(CurrentVersion)), pack_property(Name, latest_version(LatestVersion)), CurrentVersion \\= LatestVersion), OutdatedPacks).',
      });

      if (response.status === 'ok' && response.results) {
        return this.parsePackList(response.results).map(pack => ({ ...pack, outdated: true }));
      }
      return [];
    } catch (error) {
      window.showErrorMessage(`Failed to check for outdated packs: ${error}`);
      return [];
    }
  }

  private validatePackName(packName: string): boolean {
    // Pack names should be alphanumeric with underscores and hyphens
    return /^[a-zA-Z0-9_-]+$/.test(packName);
  }

  private parsePackList(results: any[]): PrologPack[] {
    const packs: PrologPack[] = [];

    for (const result of results) {
      if (Array.isArray(result) && result.length > 0) {
        for (const packData of result) {
          const pack = this.parsePackData(packData);
          if (pack) {
            packs.push(pack);
          }
        }
      }
    }

    return packs;
  }

  private parsePackData(packData: any): PrologPack | null {
    try {
      if (typeof packData === 'object' && packData.functor === 'pack') {
        const args = packData.args || [];
        return {
          name: args[0] || 'unknown',
          title: args[1] || '',
          version: args[2] || '',
          author: args[3] || '',
          home: args[4] || '',
          download: args[5] || '',
        };
      }
      return null;
    } catch (error) {
      console.warn('Failed to parse pack data:', packData, error);
      return null;
    }
  }

  private parsePackInfo(packName: string, infoData: any): PrologPack {
    const pack: PrologPack = { name: packName };

    try {
      if (Array.isArray(infoData)) {
        for (const item of infoData) {
          if (typeof item === 'object' && item.functor === '-') {
            const key = item.args[0];
            const value = item.args[1];

            switch (key) {
              case 'title':
                pack.title = value;
                break;
              case 'version':
                pack.version = value;
                break;
              case 'author':
                pack.author = value;
                break;
              case 'home':
                pack.home = value;
                break;
              case 'download':
                pack.download = value;
                break;
              case 'requires':
                pack.requires = Array.isArray(value) ? value : [value];
                break;
              case 'conflicts':
                pack.conflicts = Array.isArray(value) ? value : [value];
                break;
            }
          }
        }
      }
    } catch (error) {
      console.warn('Failed to parse pack info:', infoData, error);
    }

    return pack;
  }

  /**
   * Validate pack security and warn about untrusted packs
   */
  async validatePackSecurity(packName: string): Promise<{ safe: boolean; warnings: string[] }> {
    const warnings: string[] = [];
    let safe = true;

    try {
      const packInfo = await this.getPackInfo(packName);
      if (!packInfo) {
        warnings.push('Pack information not available');
        safe = false;
        return { safe, warnings };
      }

      // Check if pack is from official SWI-Prolog repository
      if (packInfo.home && !packInfo.home.includes('swi-prolog.org')) {
        warnings.push('Pack is not from the official SWI-Prolog repository');
      }

      // Check for HTTPS download URLs
      if (packInfo.download && !packInfo.download.startsWith('https://')) {
        warnings.push('Pack download URL is not using HTTPS');
        safe = false;
      }

      // Check for known deprecated packs (this would be maintained as a list)
      const deprecatedPacks = ['old_pack', 'deprecated_pack']; // Example list
      if (deprecatedPacks.includes(packName)) {
        warnings.push('This pack is deprecated and should not be used');
        safe = false;
      }
    } catch (error) {
      warnings.push(`Failed to validate pack security: ${error}`);
      safe = false;
    }

    return { safe, warnings };
  }
}
