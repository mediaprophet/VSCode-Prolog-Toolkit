import { expect } from 'chai';
import * as fs from 'fs';
import * as path from 'path';

describe('UI/UX Enhancements Validation', () => {
  let packageJson: any;

  before(() => {
    const packagePath = path.join(__dirname, '..', 'package.json');
    packageJson = JSON.parse(fs.readFileSync(packagePath, 'utf8'));
  });

  describe('Chat Participant Configuration', () => {
    it('should have enhanced chat participant configuration', () => {
      expect(packageJson.contributes.chatParticipants).to.be.an('array');
      expect(packageJson.contributes.chatParticipants).to.have.length(1);

      const chatParticipant = packageJson.contributes.chatParticipants[0];
      expect(chatParticipant.id).to.equal('prolog');
      expect(chatParticipant.description).to.include('🤖');
      expect(chatParticipant.commands).to.be.an('array');
      expect(chatParticipant.commands).to.have.length.greaterThan(0);
    });

    it('should have all required chat commands defined', () => {
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
        expect(commandNames).to.include(cmd, `Missing command: ${cmd}`);
      });
    });

    it('should have descriptive command descriptions', () => {
      const chatParticipant = packageJson.contributes.chatParticipants[0];
      chatParticipant.commands.forEach((cmd: any) => {
        expect(cmd.description).to.be.a('string');
        expect(cmd.description.length).to.be.greaterThan(
          10,
          `Command ${cmd.name} has too short description`
        );
      });
    });
  });

  describe('LSP Commands', () => {
    it('should have LSP commands registered', () => {
      const commands = packageJson.contributes.commands;
      const commandIds = commands.map((cmd: any) => cmd.command);

      expect(commandIds).to.include('prolog.lsp.executeQuery');
      expect(commandIds).to.include('prolog.lsp.getHelp');
      expect(commandIds).to.include('prolog.lsp.runN3Diagnostics');
    });

    it('should have proper command titles and categories', () => {
      const commands = packageJson.contributes.commands;
      const lspCommands = commands.filter((cmd: any) => cmd.command.startsWith('prolog.lsp.'));

      lspCommands.forEach((cmd: any) => {
        expect(cmd.title).to.be.a('string');
        expect(cmd.title.length).to.be.greaterThan(5);
        expect(cmd.category).to.equal('Prolog');
      });
    });

    it('should have keybindings for main LSP commands', () => {
      const keybindings = packageJson.contributes.keybindings;
      const commandIds = keybindings.map((kb: any) => kb.command);

      expect(commandIds).to.include('prolog.lsp.executeQuery');
      expect(commandIds).to.include('prolog.lsp.getHelp');
    });

    it('should have context menu entries for LSP commands', () => {
      const contextMenus = packageJson.contributes.menus['editor/context'];
      const commandIds = contextMenus.map((menu: any) => menu.command);

      expect(commandIds).to.include('prolog.lsp.executeQuery');
      expect(commandIds).to.include('prolog.lsp.getHelp');
      expect(commandIds).to.include('prolog.lsp.runN3Diagnostics');
    });

    it('should have proper when clauses for context menus', () => {
      const contextMenus = packageJson.contributes.menus['editor/context'];
      const lspMenus = contextMenus.filter((menu: any) => menu.command.startsWith('prolog.lsp.'));

      lspMenus.forEach((menu: any) => {
        expect(menu.when).to.equal('resourceLangId == prolog');
      });
    });
  });

  describe('File Structure', () => {
    it('should have LSP extension file', () => {
      const lspExtensionPath = path.join(
        __dirname,
        '..',
        'src',
        'features',
        'prologLSPExtension.ts'
      );
      expect(fs.existsSync(lspExtensionPath)).to.be.true;
    });

    it('should have enhanced extension file', () => {
      const extensionPath = path.join(__dirname, '..', 'src', 'extension.ts');
      expect(fs.existsSync(extensionPath)).to.be.true;

      const extensionContent = fs.readFileSync(extensionPath, 'utf8');
      expect(extensionContent).to.include('PrologLSPExtension');
      expect(extensionContent).to.include('lspExtension.registerFeatures()');
    });

    it('should have backup of original extension', () => {
      const backupPath = path.join(__dirname, '..', 'src', 'extension.ts');
      expect(fs.existsSync(backupPath)).to.be.true;
    });
  });

  describe('Build Validation', () => {
    it('should have compiled output files', () => {
      const outPath = path.join(__dirname, '..', 'out', 'pub', 'extension.js');
      expect(fs.existsSync(outPath)).to.be.true;
    });

    it('should have source maps', () => {
      const mapPath = path.join(__dirname, '..', 'out', 'pub', 'extension.js.map');
      expect(fs.existsSync(mapPath)).to.be.true;
    });
  });

  describe('Enhanced Help Message', () => {
    it('should have enhanced help content in extension', () => {
      const extensionPath = path.join(__dirname, '..', 'src', 'extension.ts');
      const extensionContent = fs.readFileSync(extensionPath, 'utf8');

      // Check for enhanced help message features
      expect(extensionContent).to.include('🤖 Prolog Assistant');
      expect(extensionContent).to.include('Pro Tips');
      expect(extensionContent).to.include('Quick Start Examples');
      expect(extensionContent).to.include('N3 Semantic Web Commands');
    });
  });

  describe('Enhanced Query Results', () => {
    it('should have enhanced query result formatting', () => {
      const extensionPath = path.join(__dirname, '..', 'src', 'extension.ts');
      const extensionContent = fs.readFileSync(extensionPath, 'utf8');

      // Check for enhanced formatting features
      expect(extensionContent).to.include('formatQueryResults');
      expect(extensionContent).to.include('solution');
      expect(extensionContent).to.include('variable binding');
    });
  });

  describe('Enhanced Followup Provider', () => {
    it('should have context-aware followup suggestions', () => {
      const extensionPath = path.join(__dirname, '..', 'src', 'extension.ts');
      const extensionContent = fs.readFileSync(extensionPath, 'utf8');

      // Check for enhanced followup provider
      expect(extensionContent).to.include('Context-aware followups');
      expect(extensionContent).to.include('result.metadata?.command');
      expect(extensionContent).to.include('Check backend status');
      expect(extensionContent).to.include('Start reasoning');
    });
  });
});
