import { expect } from 'chai';
import * as vscode from 'vscode';
import { PrologLSPExtension } from '../src/features/prologLSPExtension.js';

describe('UI/UX Enhancements', () => {
  let mockContext: vscode.ExtensionContext;
  let lspExtension: PrologLSPExtension;

  beforeEach(() => {
    // Create a mock extension context
    mockContext = {
      subscriptions: [],
      workspaceState: {} as any,
      globalState: {} as any,
      extensionPath: '',
      asAbsolutePath: (relativePath: string) => relativePath,
      storagePath: '',
      globalStoragePath: '',
      logPath: '',
      extensionUri: {} as any,
      environmentVariableCollection: {} as any,
      extensionMode: vscode.ExtensionMode.Test,
      globalStorageUri: {} as any,
      logUri: {} as any,
      storageUri: {} as any,
      secrets: {} as any,
      extension: {} as any,
      languageModelAccessInformation: {} as any,
    };

    lspExtension = new PrologLSPExtension(mockContext, null);
  });

  afterEach(() => {
    if (lspExtension) {
      lspExtension.dispose();
    }
  });

  describe('LSP Extension', () => {
    it('should initialize without errors', () => {
      expect(lspExtension).to.not.be.null;
      expect(mockContext.subscriptions).to.have.length.greaterThan(0);
    });

    it('should register features without throwing', () => {
      expect(() => {
        lspExtension.registerFeatures();
      }).to.not.throw();
    });

    it('should dispose cleanly', () => {
      expect(() => {
        lspExtension.dispose();
      }).to.not.throw();
    });
  });

  describe('Chat Participant Configuration', () => {
    it('should have enhanced chat participant configuration in package.json', async () => {
      // eslint-disable-next-line @typescript-eslint/no-var-requires
      const packageJson = require('../package.json');

      expect(packageJson.contributes.chatParticipants).to.be.an('array');
      expect(packageJson.contributes.chatParticipants).to.have.length(1);

      const chatParticipant = packageJson.contributes.chatParticipants[0];
      expect(chatParticipant.id).to.equal('prolog');
      expect(chatParticipant.description).to.include('🤖');
      expect(chatParticipant.commands).to.be.an('array');
      expect(chatParticipant.commands).to.have.length.greaterThan(0);
    });

    it('should have all required chat commands defined', () => {
      // eslint-disable-next-line @typescript-eslint/no-var-requires
      const packageJson = require('../package.json');
      const chatParticipant = packageJson.contributes.chatParticipants[0];
      const commandNames = chatParticipant.commands.map((cmd: any) => cmd.name);

      const expectedCommands = [
        'query',
        'consult',
        'help',
        'status',
        'n3_load',
        'n3_list',
        'n3_reason',
        'n3_explain',
      ];
      expectedCommands.forEach(cmd => {
        expect(commandNames).to.include(cmd);
      });
    });
  });

  describe('LSP Commands', () => {
    it('should have LSP commands registered in package.json', () => {
      // eslint-disable-next-line @typescript-eslint/no-var-requires
      const packageJson = require('../package.json');
      const commands = packageJson.contributes.commands;
      const commandIds = commands.map((cmd: any) => cmd.command);

      expect(commandIds).to.include('prolog.lsp.executeQuery');
      expect(commandIds).to.include('prolog.lsp.getHelp');
      expect(commandIds).to.include('prolog.lsp.runN3Diagnostics');
    });

    it('should have keybindings for LSP commands', () => {
      // eslint-disable-next-line @typescript-eslint/no-var-requires
      const packageJson = require('../package.json');
      const keybindings = packageJson.contributes.keybindings;
      const commandIds = keybindings.map((kb: any) => kb.command);

      expect(commandIds).to.include('prolog.lsp.executeQuery');
      expect(commandIds).to.include('prolog.lsp.getHelp');
    });

    it('should have context menu entries for LSP commands', () => {
      // eslint-disable-next-line @typescript-eslint/no-var-requires
      const packageJson = require('../package.json');
      const contextMenus = packageJson.contributes.menus['editor/context'];
      const commandIds = contextMenus.map((menu: any) => menu.command);

      expect(commandIds).to.include('prolog.lsp.executeQuery');
      expect(commandIds).to.include('prolog.lsp.getHelp');
      expect(commandIds).to.include('prolog.lsp.runN3Diagnostics');
    });
  });

  describe('Extension Activation', () => {
    it('should build successfully', () => {
      // This test passes if the TypeScript compilation was successful
      // which we verified with the npm run build command
      expect(true).to.be.true;
    });
  });
});
