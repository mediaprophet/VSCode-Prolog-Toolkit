import type { QuickPickItem } from 'vscode';
import { window } from 'vscode';
import type { PackageOperationResult, PrologPack } from './prologPackageManager.js';
import { PrologPackageManager } from './prologPackageManager.js';

export interface PackQuickPickItem extends QuickPickItem {
  pack: PrologPack;
}

export class PrologPackageCommands {
  private packageManager: PrologPackageManager;

  constructor(packageManager: PrologPackageManager) {
    this.packageManager = packageManager;
  }

  /**
   * Handle package management commands from chat
   */
  async handlePackageCommand(command: string, args: string[]): Promise<string> {
    try {
      switch (command.toLowerCase()) {
        case 'list':
          return await this.handleListCommand(args);
        case 'install':
          return await this.handleInstallCommand(args);
        case 'uninstall':
        case 'remove':
          return await this.handleUninstallCommand(args);
        case 'update':
        case 'upgrade':
          return await this.handleUpdateCommand(args);
        case 'info':
        case 'show':
          return await this.handleInfoCommand(args);
        case 'search':
          return await this.handleSearchCommand(args);
        case 'outdated':
          return await this.handleOutdatedCommand();
        case 'servers':
          return await this.handleServersCommand(args);
        default:
          return this.getHelpMessage();
      }
    } catch (error: unknown) {
      const errorMessage = error instanceof Error ? error.message : String(error);
      return `❌ Command failed: ${errorMessage}`;
    }
  }

  private async handleListCommand(args: string[]): Promise<string> {
    const listType = args[0]?.toLowerCase() || 'installed';

    if (listType === 'available' || listType === 'all') {
      const packs = await this.packageManager.listAvailablePacks();
      if (packs.length === 0) {
        return '📦 No available packs found.';
      }

      let result = `📦 **Available Packs** (${packs.length} found):\n\n`;
      for (const pack of packs.slice(0, 20)) {
        // Limit to first 20 for readability
        result += `• **${pack.name}** ${pack.version ? `v${pack.version}` : ''}\n`;
        if (pack.title) {
          result += `  ${pack.title}\n`;
        }
        if (pack.author) {
          result += `  👤 ${pack.author}\n`;
        }
        result += '\n';
      }

      if (packs.length > 20) {
        result += `... and ${packs.length - 20} more packs. Use \`/prolog pack search <keyword>\` to find specific packs.\n`;
      }

      return result;
    } else {
      const packs = await this.packageManager.listInstalledPacks();
      if (packs.length === 0) {
        return '📦 No packs are currently installed.';
      }

      let result = `📦 **Installed Packs** (${packs.length} found):\n\n`;
      for (const pack of packs) {
        result += `• **${pack.name}** ${pack.version ? `v${pack.version}` : ''}\n`;
        if (pack.title) {
          result += `  ${pack.title}\n`;
        }
        result += '\n';
      }

      return result;
    }
  }

  private async handleInstallCommand(args: string[]): Promise<string> {
    if (args.length === 0) {
      return '❌ Please specify a pack name to install.\nUsage: `/prolog pack install <pack_name>`';
    }

    const packName = args[0];

    // Validate pack security first
    if (!packName) {
      return '\u274c Invalid pack name.';
    }
    const securityCheck = await this.packageManager.validatePackSecurity(packName);
    if (!securityCheck.safe) {
      const proceed = await window.showWarningMessage(
        `Security warnings for pack '${packName}':\n${securityCheck.warnings.join('\n')}\n\nDo you want to proceed with installation?`,
        'Yes, Install Anyway',
        'Cancel'
      );

      if (proceed !== 'Yes, Install Anyway') {
        return `❌ Installation of '${packName}' cancelled due to security concerns.`;
      }
    } else if (securityCheck.warnings.length > 0) {
      window.showInformationMessage(
        `Pack '${packName}' has some warnings: ${securityCheck.warnings.join(', ')}`
      );
    }

    if (!packName) {
      return '\u274c Invalid pack name.';
    }
    const result = await this.packageManager.installPack(packName);
    return this.formatOperationResult('Installation', result);
  }

  private async handleUninstallCommand(args: string[]): Promise<string> {
    if (args.length === 0) {
      return '❌ Please specify a pack name to uninstall.\nUsage: `/prolog pack uninstall <pack_name>`';
    }

    const packName = args[0];

    // Confirm uninstallation
    const confirm = await window.showWarningMessage(
      `Are you sure you want to uninstall pack '${packName}'?`,
      'Yes, Uninstall',
      'Cancel'
    );

    if (confirm !== 'Yes, Uninstall') {
      return `❌ Uninstallation of '${packName}' cancelled.`;
    }

    if (!packName) {
      return '\u274c Invalid pack name.';
    }
    const result = await this.packageManager.uninstallPack(packName);
    return this.formatOperationResult('Uninstallation', result);
  }

  private async handleUpdateCommand(args: string[]): Promise<string> {
    if (args.length === 0) {
      // Update all outdated packs
      const outdatedPacks = await this.packageManager.checkOutdatedPacks();
      if (outdatedPacks.length === 0) {
        return '✅ All installed packs are up to date.';
      }

      const confirm = await window.showInformationMessage(
        `Found ${outdatedPacks.length} outdated pack(s). Update all?`,
        'Yes, Update All',
        'Cancel'
      );

      if (confirm !== 'Yes, Update All') {
        return '❌ Update cancelled.';
      }

      let results = `🔄 **Updating ${outdatedPacks.length} pack(s):**\n\n`;
      for (const pack of outdatedPacks) {
        const result = await this.packageManager.updatePack(pack.name || '');
        results += `• ${pack.name}: ${result.success ? '✅' : '❌'} ${result.message}\n`;
      }

      return results;
    } else {
      const packName = args[0];
      if (!packName) {
        return '\u274c Invalid pack name.';
      }
      const result = await this.packageManager.updatePack(packName);
      return this.formatOperationResult('Update', result);
    }
  }

  private async handleInfoCommand(args: string[]): Promise<string> {
    if (args.length === 0) {
      return '❌ Please specify a pack name to get information about.\nUsage: `/prolog pack info <pack_name>`';
    }

    const packName = args[0];
    if (!packName) {
      return '\u274c Invalid pack name.';
    }
    const packInfo = await this.packageManager.getPackInfo(packName);

    if (!packInfo) {
      return `❌ Pack '${packName}' not found or information unavailable.`;
    }

    let result = `📦 **Pack Information: ${packInfo.name}**\n\n`;

    if (packInfo.title) {
      result += `**Title:** ${packInfo.title}\n`;
    }
    if (packInfo.version) {
      result += `**Version:** ${packInfo.version}\n`;
    }
    if (packInfo.author) {
      result += `**Author:** ${packInfo.author}\n`;
    }
    if (packInfo.description) {
      result += `**Description:** ${packInfo.description}\n`;
    }
    if (packInfo.home) {
      result += `**Homepage:** ${packInfo.home}\n`;
    }
    if (packInfo.download) {
      result += `**Download:** ${packInfo.download}\n`;
    }
    if (packInfo.requires && packInfo.requires.length > 0) {
      result += `**Dependencies:** ${packInfo.requires.join(', ')}\n`;
    }
    if (packInfo.conflicts && packInfo.conflicts.length > 0) {
      result += `**Conflicts:** ${packInfo.conflicts.join(', ')}\n`;
    }

    result += `**Installed:** ${packInfo.installed ? '✅ Yes' : '❌ No'}\n`;

    if (packInfo.outdated) {
      result += `**Status:** ⚠️ Outdated (update available)\n`;
    }

    return result;
  }

  private async handleSearchCommand(args: string[]): Promise<string> {
    if (args.length === 0) {
      return '❌ Please specify a search keyword.\nUsage: `/prolog pack search <keyword>`';
    }

    const keyword = args.join(' ');
    const packs = await this.packageManager.searchPacks(keyword);

    if (packs.length === 0) {
      return `🔍 No packs found matching '${keyword}'.`;
    }

    let result = `🔍 **Search Results for '${keyword}'** (${packs.length} found):\n\n`;

    for (const pack of packs.slice(0, 10)) {
      // Limit to first 10 results
      result += `• **${pack.name}** ${pack.version ? `v${pack.version}` : ''}\n`;
      if (pack.title) {
        result += `  ${pack.title}\n`;
      }
      if (pack.author) {
        result += `  👤 ${pack.author}\n`;
      }
      result += '\n';
    }

    if (packs.length > 10) {
      result += `... and ${packs.length - 10} more results. Try a more specific search term.\n`;
    }

    return result;
  }

  private async handleOutdatedCommand(): Promise<string> {
    const outdatedPacks = await this.packageManager.checkOutdatedPacks();

    if (outdatedPacks.length === 0) {
      return '✅ All installed packs are up to date.';
    }

    let result = `⚠️ **Outdated Packs** (${outdatedPacks.length} found):\n\n`;

    for (const pack of outdatedPacks) {
      result += `• **${pack.name}** ${pack.version ? `v${pack.version}` : ''}\n`;
      if (pack.title) {
        result += `  ${pack.title}\n`;
      }
      result += '\n';
    }

    result += `\nUse \`/prolog pack update\` to update all outdated packs, or \`/prolog pack update <pack_name>\` to update a specific pack.\n`;

    return result;
  }

  private async handleServersCommand(args: string[]): Promise<string> {
    const subCommand = args[0]?.toLowerCase();

    if (subCommand === 'add' && args[1]) {
      const serverUrl = args[1];
      try {
        new URL(serverUrl); // Validate URL
        this.packageManager.addPackServer(serverUrl);
        return `✅ Added pack server: ${serverUrl}`;
      } catch {
        return `❌ Invalid URL: ${serverUrl}`;
      }
    } else if (subCommand === 'remove' && args[1]) {
      const serverUrl = args[1];
      this.packageManager.removePackServer(serverUrl);
      return `✅ Removed pack server: ${serverUrl}`;
    } else {
      const servers = this.packageManager.getPackServers();
      let result = `🌐 **Configured Pack Servers:**\n\n`;

      servers.forEach((server, index) => {
        result += `${index + 1}. ${server}${index === 0 ? ' (default)' : ''}\n`;
      });

      result += `\n**Usage:**\n`;
      result += `• \`/prolog pack servers add <url>\` - Add a custom server\n`;
      result += `• \`/prolog pack servers remove <url>\` - Remove a custom server\n`;

      return result;
    }
  }

  private formatOperationResult(operation: string, result: PackageOperationResult): string {
    const icon = result.success ? '✅' : '❌';
    let message = `${icon} ${operation}: ${result.message}`;

    if (result.details) {
      message += `\n\n**Details:**\n${result.details}`;
    }

    return message;
  }

  private getHelpMessage(): string {
    return `📦 **Prolog Package Manager Commands:**

**Basic Commands:**
• \`/prolog pack list\` - List installed packs
• \`/prolog pack list available\` - List all available packs
• \`/prolog pack install <name>\` - Install a pack
• \`/prolog pack uninstall <name>\` - Uninstall a pack
• \`/prolog pack update [name]\` - Update pack(s)

**Information Commands:**
• \`/prolog pack info <name>\` - Show pack details
• \`/prolog pack search <keyword>\` - Search for packs
• \`/prolog pack outdated\` - List outdated packs

**Server Management:**
• \`/prolog pack servers\` - List pack servers
• \`/prolog pack servers add <url>\` - Add custom server
• \`/prolog pack servers remove <url>\` - Remove custom server

**Examples:**
• \`/prolog pack install http\` - Install the HTTP pack
• \`/prolog pack search web\` - Search for web-related packs
• \`/prolog pack info clpfd\` - Get info about the CLPFD pack`;
  }

  /**
   * Show interactive pack picker for installation
   */
  async showPackPicker(): Promise<void> {
    const availablePacks = await this.packageManager.listAvailablePacks();
    const installedPacks = await this.packageManager.listInstalledPacks();
    const installedNames = new Set(installedPacks.map(p => p.name));

    const items: PackQuickPickItem[] = availablePacks
      .filter(pack => !installedNames.has(pack.name))
      .map(pack => ({
        label: pack.name,
        description: pack.version ? `v${pack.version}` : '',
        detail: pack.title || pack.description || 'No description available',
        pack,
      }));

    const selected = await window.showQuickPick(items, {
      placeHolder: 'Select a pack to install',
      matchOnDescription: true,
      matchOnDetail: true,
    });

    if (selected) {
      const result = await this.packageManager.installPack(selected.pack.name);
      if (result.success) {
        window.showInformationMessage(result.message);
      } else {
        window.showErrorMessage(result.message);
      }
    }
  }

  /**
   * Show interactive pack picker for uninstallation
   */
  async showUninstallPicker(): Promise<void> {
    const installedPacks = await this.packageManager.listInstalledPacks();

    if (installedPacks.length === 0) {
      window.showInformationMessage('No packs are currently installed.');
      return;
    }

    const items: PackQuickPickItem[] = installedPacks.map(pack => ({
      label: pack.name,
      description: pack.version ? `v${pack.version}` : '',
      detail: pack.title || pack.description || 'No description available',
      pack,
    }));

    const selected = await window.showQuickPick(items, {
      placeHolder: 'Select a pack to uninstall',
      matchOnDescription: true,
      matchOnDetail: true,
    });

    if (selected) {
      const confirm = await window.showWarningMessage(
        `Are you sure you want to uninstall '${selected.pack.name}'?`,
        'Yes, Uninstall',
        'Cancel'
      );

      if (confirm === 'Yes, Uninstall') {
        const result = await this.packageManager.uninstallPack(selected.pack.name);
        if (result.success) {
          window.showInformationMessage(result.message);
        } else {
          window.showErrorMessage(result.message);
        }
      }
    }
  }
}
