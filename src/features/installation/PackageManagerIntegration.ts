
import * as os from 'os';
import * as vscode from 'vscode';

export class PackageManagerIntegration {
  private static instance: PackageManagerIntegration;

  private constructor() { }

  public static getInstance(): PackageManagerIntegration {
    if (!PackageManagerIntegration.instance) {
      PackageManagerIntegration.instance = new PackageManagerIntegration();
    }
    return PackageManagerIntegration.instance;
  }

  public async showInstallationDialog(): Promise<void> {
    const platform = os.platform();
    let command: string | undefined;
    let instructions: string = '';
    let url: string | undefined;

    if (platform === 'win32') {
      url = 'https://www.swi-prolog.org/download/stable';
      instructions = 'Download and run the Windows installer from the official SWI-Prolog website.';
    } else if (platform === 'darwin') {
      command = 'brew install swi-prolog';
      url = 'https://brew.sh';
      instructions = 'Install Homebrew if not already installed, then run the following command:';
    } else if (platform === 'linux') {
      // Try to detect distro for best command
      const distro = await this.detectLinuxDistro();
      switch (distro) {
        case 'debian':
        case 'ubuntu':
          command = 'sudo apt install swi-prolog';
          break;
        case 'fedora':
          command = 'sudo dnf install pl';
          break;
        case 'arch':
          command = 'sudo pacman -S swi-prolog';
          break;
        case 'opensuse':
          command = 'sudo zypper install swi-prolog';
          break;
        default:
          command = 'sudo apt install swi-prolog';
      }
      instructions = 'Run the following command in your terminal:';
    }

    const actions = [];
    if (command) actions.push('Copy Command');
    if (url) actions.push('Open Download Page');
    actions.push('OK');

    const message = `${instructions}${command ? `\n\n${command}` : ''}`;
    const choice = await vscode.window.showInformationMessage(message, ...actions);

    if (choice === 'Copy Command' && command) {
      await vscode.env.clipboard.writeText(command);
      vscode.window.showInformationMessage('Command copied to clipboard.');
    } else if (choice === 'Open Download Page' && url) {
      vscode.env.openExternal(vscode.Uri.parse(url));
    }
  }

  public async detectLinuxDistro(): Promise<string> {
    // Try to detect Linux distro from /etc/os-release
    try {
      const fs = await import('fs/promises');
      const content = await fs.readFile('/etc/os-release', 'utf8');
      if (/ubuntu/i.test(content)) return 'ubuntu';
      if (/debian/i.test(content)) return 'debian';
      if (/fedora/i.test(content)) return 'fedora';
      if (/arch/i.test(content)) return 'arch';
      if (/opensuse/i.test(content)) return 'opensuse';
    } catch { }
    return 'unknown';
  }
}
