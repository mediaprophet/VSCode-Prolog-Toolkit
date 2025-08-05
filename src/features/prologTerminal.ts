"use strict";

import { Utils } from "../utils/utils";
import {
  Terminal,
  window,
  workspace,
  TextDocument,
  Disposable,
  OutputChannel,
  TextEditor,
  commands
} from "vscode";
import jsesc from "jsesc";
import { InstallationGuide } from "./installationGuide";
import { PlatformUtils, PlatformType } from "../utils/platformUtils";
import { ExecutableFinder } from "../utils/executableFinder";
import { spawn } from "child_process";
import * as os from "os";

/**
 * Shell information interface
 */
interface ShellInfo {
  name: string;
  path: string;
  args: string[];
  supportsColors: boolean;
  requiresQuoting: boolean;
}

/**
 * Terminal configuration interface
 */
interface TerminalConfig {
  executable: string;
  args: string[];
  shell?: ShellInfo;
  env?: Record<string, string>;
  cwd?: string;
}

export default class PrologTerminal {
  private static _terminal: Terminal;
  private static _document: TextDocument;
  private static _platform: PlatformType;
  private static _shellInfo: ShellInfo | null = null;

  constructor() {}
  // Initialize the Prolog terminal
  public static init(): Disposable {
    PrologTerminal._platform = PlatformUtils.getPlatform();
    return (<any>window).onDidCloseTerminal((terminal: any) => {
        PrologTerminal._terminal = null as any;
        terminal.dispose();
      });
  }

  /**
   * Detect the default shell for the current platform
   */
  private static async detectShell(): Promise<ShellInfo> {
    if (PrologTerminal._shellInfo) {
      return PrologTerminal._shellInfo;
    }

    let shellInfo: ShellInfo;

    switch (PrologTerminal._platform) {
      case 'windows':
        shellInfo = await PrologTerminal.detectWindowsShell();
        break;
      case 'macos':
        shellInfo = await PrologTerminal.detectMacOSShell();
        break;
      case 'linux':
        shellInfo = await PrologTerminal.detectLinuxShell();
        break;
      default:
        // Fallback to basic shell
        shellInfo = {
          name: 'sh',
          path: '/bin/sh',
          args: [],
          supportsColors: false,
          requiresQuoting: true
        };
    }

    PrologTerminal._shellInfo = shellInfo;
    return shellInfo;
  }

  /**
   * Detect Windows shell (PowerShell, Command Prompt, WSL)
   */
  private static async detectWindowsShell(): Promise<ShellInfo> {
    // Check for PowerShell Core first
    try {
      await PrologTerminal.testCommand('pwsh', ['--version']);
      return {
        name: 'PowerShell Core',
        path: 'pwsh',
        args: ['-NoLogo'],
        supportsColors: true,
        requiresQuoting: true
      };
    } catch {}

    // Check for Windows PowerShell
    try {
      await PrologTerminal.testCommand('powershell', ['-Version']);
      return {
        name: 'Windows PowerShell',
        path: 'powershell',
        args: ['-NoLogo'],
        supportsColors: true,
        requiresQuoting: true
      };
    } catch {}

    // Check for WSL
    try {
      await PrologTerminal.testCommand('wsl', ['--version']);
      return {
        name: 'WSL',
        path: 'wsl',
        args: [],
        supportsColors: true,
        requiresQuoting: true
      };
    } catch {}

    // Fallback to Command Prompt
    return {
      name: 'Command Prompt',
      path: 'cmd',
      args: ['/K'],
      supportsColors: false,
      requiresQuoting: true
    };
  }

  /**
   * Detect macOS shell (zsh, bash)
   */
  private static async detectMacOSShell(): Promise<ShellInfo> {
    // Check for zsh (default on macOS Catalina+)
    try {
      await PrologTerminal.testCommand('zsh', ['--version']);
      return {
        name: 'Zsh',
        path: '/bin/zsh',
        args: [],
        supportsColors: true,
        requiresQuoting: true
      };
    } catch {}

    // Check for bash
    try {
      await PrologTerminal.testCommand('bash', ['--version']);
      return {
        name: 'Bash',
        path: '/bin/bash',
        args: [],
        supportsColors: true,
        requiresQuoting: true
      };
    } catch {}

    // Fallback to sh
    return {
      name: 'Shell',
      path: '/bin/sh',
      args: [],
      supportsColors: false,
      requiresQuoting: true
    };
  }

  /**
   * Detect Linux shell (bash, zsh, fish, etc.)
   */
  private static async detectLinuxShell(): Promise<ShellInfo> {
    // Get user's default shell from environment
    const userShell = process.env.SHELL;
    if (userShell) {
      const shellName = PlatformUtils.basename(userShell);
      try {
        await PrologTerminal.testCommand(userShell, ['--version']);
        return {
          name: shellName,
          path: userShell,
          args: [],
          supportsColors: true,
          requiresQuoting: true
        };
      } catch {}
    }

    // Check for common shells
    const shells = [
      { name: 'bash', path: '/bin/bash' },
      { name: 'zsh', path: '/bin/zsh' },
      { name: 'fish', path: '/usr/bin/fish' },
      { name: 'dash', path: '/bin/dash' }
    ];

    for (const shell of shells) {
      try {
        await PrologTerminal.testCommand(shell.path, ['--version']);
        return {
          name: shell.name,
          path: shell.path,
          args: [],
          supportsColors: shell.name !== 'dash',
          requiresQuoting: true
        };
      } catch {}
    }

    // Fallback to sh
    return {
      name: 'Shell',
      path: '/bin/sh',
      args: [],
      supportsColors: false,
      requiresQuoting: true
    };
  }

  /**
   * Test if a command is available
   */
  private static async testCommand(command: string, args: string[]): Promise<boolean> {
    return new Promise((resolve) => {
      const process = spawn(command, args, {
        stdio: ['ignore', 'ignore', 'ignore'],
        timeout: 3000
      });

      process.on('close', (code) => {
        resolve(code === 0);
      });

      process.on('error', () => {
        resolve(false);
      });

      setTimeout(() => {
        process.kill();
        resolve(false);
      }, 3000);
    });
  }

  /**
   * Get platform-specific terminal configuration
   */
  private static async getTerminalConfig(): Promise<TerminalConfig> {
    const section = workspace.getConfiguration("prolog");
    let executable = section.get<string>("executablePath", PlatformUtils.getDefaultExecutablePath());
    
    // Enhanced executable resolution
    if (!await PlatformUtils.pathExists(executable) || !await PlatformUtils.isExecutable(executable)) {
      const executableFinder = new ExecutableFinder();
      const detectionResult = await executableFinder.findSwiplExecutable();
      if (detectionResult.found && detectionResult.path) {
        executable = detectionResult.path;
      }
    }

    executable = PlatformUtils.normalizePath(executable);
    
    // Get platform-specific runtime arguments
    let args = section.get<string[]>("terminal.runtimeArgs") || PlatformUtils.getDefaultRuntimeArgs();
    
    // Detect shell for better integration
    const shell = await PrologTerminal.detectShell();
    
    // Platform-specific environment variables
    const env: Record<string, string> = {};
    
    switch (PrologTerminal._platform) {
      case 'windows':
        // Windows-specific environment setup
        env.TERM = 'xterm-256color';
        break;
      case 'macos':
        // macOS-specific environment setup
        env.TERM = 'xterm-256color';
        env.LANG = process.env.LANG || 'en_US.UTF-8';
        break;
      case 'linux':
        // Linux-specific environment setup
        env.TERM = 'xterm-256color';
        env.LANG = process.env.LANG || 'en_US.UTF-8';
        break;
    }

    return {
      executable,
      args,
      shell,
      env,
      cwd: workspace.workspaceFolders?.[0]?.uri.fsPath || process.cwd()
    };
  }

  /**
   * Escape command arguments for the detected shell
   */
  private static escapeForShell(text: string, shell: ShellInfo): string {
    if (!shell.requiresQuoting) {
      return text;
    }

    switch (PrologTerminal._platform) {
      case 'windows':
        // Windows shell escaping
        if (shell.name.includes('PowerShell')) {
          // PowerShell escaping
          return `'${text.replace(/'/g, "''")}'`;
        } else {
          // Command Prompt escaping
          return `"${text.replace(/"/g, '""')}"`;
        }
      case 'macos':
      case 'linux':
        // Unix shell escaping
        if (text.includes(' ') || text.includes('"') || text.includes("'")) {
          return `'${text.replace(/'/g, "'\"'\"'")}'`;
        }
        return text;
      default:
        return text;
    }
  }

  // Create a Prolog terminal instance
  private static async createPrologTerm() {
    if (PrologTerminal._terminal) {
      return;
    }

    try {
      const config = await PrologTerminal.getTerminalConfig();
      const title = "Prolog";

      // Validate executable exists and has proper permissions
      if (!await PlatformUtils.pathExists(config.executable)) {
        throw new Error(`Executable not found: ${config.executable}`);
      }

      if (!await PlatformUtils.isExecutable(config.executable)) {
        const platform = PlatformUtils.getPlatform();
        let permissionError = `Executable lacks execute permissions: ${config.executable}`;
        if (platform !== 'windows') {
          permissionError += `\n\nTry fixing permissions with: chmod +x "${config.executable}"`;
        }
        throw new Error(permissionError);
      }

      // Create terminal with enhanced configuration
      const terminalOptions: any = {
        name: title,
        shellPath: config.executable,
        shellArgs: config.args,
        cwd: config.cwd,
        env: {
          ...process.env,
          ...config.env
        }
      };

      // Add platform-specific terminal options
      switch (PrologTerminal._platform) {
        case 'windows':
          // Windows-specific terminal options
          terminalOptions.hideFromUser = false;
          break;
        case 'macos':
          // macOS-specific terminal options
          terminalOptions.strictEnv = false;
          break;
        case 'linux':
          // Linux-specific terminal options
          terminalOptions.strictEnv = false;
          break;
      }

      PrologTerminal._terminal = window.createTerminal(terminalOptions);

      // Send initial setup commands based on platform and shell
      setTimeout(async () => {
        await PrologTerminal.sendInitialSetupCommands(config);
      }, 1000);

    } catch (error: any) {
      await PrologTerminal.handleTerminalCreationError(error);
      throw error;
    }
  }

  /**
   * Send initial setup commands to configure the terminal environment
   */
  private static async sendInitialSetupCommands(config: TerminalConfig) {
    if (!PrologTerminal._terminal) return;

    const shell = config.shell;
    if (!shell) return;

    // Platform-specific setup commands
    const setupCommands: string[] = [];

    switch (PrologTerminal._platform) {
      case 'windows':
        if (shell.name.includes('PowerShell')) {
          setupCommands.push('$Host.UI.RawUI.WindowTitle = "Prolog Terminal"');
          if (shell.supportsColors) {
            setupCommands.push('$PSStyle.OutputRendering = "Host"');
          }
        } else if (shell.name === 'Command Prompt') {
          setupCommands.push('title Prolog Terminal');
        }
        break;
      case 'macos':
      case 'linux':
        if (shell.supportsColors) {
          setupCommands.push('export TERM=xterm-256color');
        }
        // Set terminal title
        setupCommands.push('echo -ne "\\033]0;Prolog Terminal\\007"');
        break;
    }

    // Send setup commands
    for (const command of setupCommands) {
      PrologTerminal._terminal.sendText(command);
    }
  }

  /**
   * Handle terminal creation errors with enhanced error messages and recovery options
   */
  private static async handleTerminalCreationError(error: any) {
    let errorMessage = 'Failed to create Prolog terminal';
    let isExecutableError = false;

    if (error.code === "ENOENT" || error.message?.includes("not found")) {
      errorMessage = 'SWI-Prolog executable not found. The terminal requires SWI-Prolog to run interactive Prolog sessions.';
      isExecutableError = true;
    } else if (error.message?.includes("permission")) {
      errorMessage = `Permission error: ${error.message}`;
      isExecutableError = true;
    } else {
      errorMessage = `Failed to create Prolog terminal: ${error.message || error}`;
    }

    if (isExecutableError) {
      const action = await window.showErrorMessage(
        errorMessage,
        'Install with Package Manager',
        'Installation Guide',
        'Setup Wizard',
        'Configure Path',
        'Dismiss'
      );
      
      const installationGuide = InstallationGuide.getInstance();
      switch (action) {
        case 'Install with Package Manager':
          const { PackageManagerIntegration } = await import('./packageManagerIntegration');
          const packageManager = PackageManagerIntegration.getInstance();
          await packageManager.showInstallationDialog();
          break;
        case 'Installation Guide':
          await installationGuide.showInstallationGuideDialog();
          break;
        case 'Setup Wizard':
          await commands.executeCommand('prolog.setupWizard');
          break;
        case 'Configure Path':
          await commands.executeCommand('workbench.action.openSettings', 'prolog.executablePath');
          break;
      }
    } else {
      window.showErrorMessage(errorMessage);
    }
  }
  // Send a string to the Prolog terminal
  public static async sendString(text: string) {
    try {
      await PrologTerminal.createPrologTerm();
      
      // Get shell info for proper escaping
      const shell = await PrologTerminal.detectShell();
      
      // finish goal by .
      if (!text.endsWith(".")) {
        text += ".";
      }
      
      // Escape text for the detected shell if needed
      const escapedText = PrologTerminal.escapeForShell(text, shell);
      
      PrologTerminal._terminal.sendText(escapedText);
      PrologTerminal._terminal.show(false);
    } catch (error) {
      // Error already handled in createPrologTerm
      console.error('Failed to send string to Prolog terminal:', error);
    }
  }
  // load the prolog file
  public static async loadDocument() {
    if (!window.activeTextEditor) {
      return;
    }
    try {
      PrologTerminal._document = window.activeTextEditor.document;// Get the active Prolog document
      await PrologTerminal.createPrologTerm();// Create the Prolog terminal
      // Get the file name and escape it using jsesc
      let fname = jsesc(PlatformUtils.normalizePath(PrologTerminal._document.fileName), { quotes: "single" });
      let goals = `['${fname}']`;// Define the goals to load the Prolog file
      // load the file into swipl with a goal
      if (PrologTerminal._document.isDirty) {
        PrologTerminal._document.save().then(_ => {
          PrologTerminal.sendString(goals);
        });
      } else {
        await PrologTerminal.sendString(goals);
      }
    } catch (error) {
      // Error already handled in createPrologTerm
      console.error('Failed to load document in Prolog terminal:', error);
    }
  }
  // query the goal under the cursor command
  public static queryGoalUnderCursor() {
    // Get the active text editor and document
    if (!window.activeTextEditor) {
      return;
    }
    let editor: TextEditor = window.activeTextEditor;
    let doc: TextDocument = editor.document;
    let pred = Utils.getPredicateUnderCursor(doc, editor.selection.active);// Get the predicate under the cursor using utility function
    // if no predicate under cursor
    if (!pred) {
      return;
    }
    PrologTerminal.loadDocument();// Load the current Prolog document into the Prolog terminal
    let goal = pred?.wholePred || "";// Extract the goal from the predicate
    // Separate the module if present
    if (goal.indexOf(":") > -1) {
      const parts = goal.split(":");
      goal = parts.length > 1 ? parts[1] : goal;
    }
    PrologTerminal.sendString(goal);
  }
}
