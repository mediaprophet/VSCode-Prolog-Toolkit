import { expect } from 'chai';
import { describe, it, beforeEach, afterEach } from 'mocha';
import * as sinon from 'sinon';
import { PrologPackageCommands } from '../src/features/prologPackageCommands';
import { PrologPackageManager, PrologPack } from '../src/features/prologPackageManager';

describe('PrologPackageCommands', () => {
  let packageCommands: PrologPackageCommands;
  let mockPackageManager: sinon.SinonStubbedInstance<PrologPackageManager>;
  let windowStub: any;

  const mockPacks: PrologPack[] = [
    {
      name: 'http',
      title: 'HTTP client and server library',
      version: '1.0.0',
      author: 'Jan Wielemaker',
      home: 'https://swi-prolog.org',
      installed: true
    },
    {
      name: 'clpfd',
      title: 'Constraint Logic Programming over Finite Domains',
      version: '2.1.0',
      author: 'Markus Triska',
      home: 'https://swi-prolog.org',
      installed: false
    }
  ];

  beforeEach(() => {
    // Create mock package manager
    mockPackageManager = sinon.createStubInstance(PrologPackageManager);
    packageCommands = new PrologPackageCommands(mockPackageManager as any);

    // Mock VS Code window API
    windowStub = {
      showErrorMessage: sinon.stub(),
      showWarningMessage: sinon.stub(),
      showInformationMessage: sinon.stub(),
      showQuickPick: sinon.stub()
    };

    // Mock the VS Code module
    const vscode = require('vscode');
    if (vscode && vscode.window) {
      Object.assign(vscode.window, windowStub);
    }
  });

  afterEach(() => {
    sinon.restore();
  });

  describe('handlePackageCommand', () => {
    describe('list command', () => {
      it('should list installed packs by default', async () => {
        mockPackageManager.listInstalledPacks.resolves([mockPacks[0]]);

        const result = await packageCommands.handlePackageCommand('list', []);
        
        expect(result).to.include('Installed Packs');
        expect(result).to.include('http');
        expect(result).to.include('v1.0.0');
        expect(mockPackageManager.listInstalledPacks).to.have.been.calledOnce;
      });

      it('should list available packs when requested', async () => {
        mockPackageManager.listAvailablePacks.resolves(mockPacks);

        const result = await packageCommands.handlePackageCommand('list', ['available']);
        
        expect(result).to.include('Available Packs');
        expect(result).to.include('http');
        expect(result).to.include('clpfd');
        expect(mockPackageManager.listAvailablePacks).to.have.been.calledOnce;
      });

      it('should handle empty pack lists', async () => {
        mockPackageManager.listInstalledPacks.resolves([]);

        const result = await packageCommands.handlePackageCommand('list', []);
        
        expect(result).to.include('No packs are currently installed');
      });

      it('should limit available packs display to 20', async () => {
        const manyPacks = Array.from({ length: 25 }, (_, i) => ({
          name: `pack${i}`,
          version: '1.0.0',
          title: `Pack ${i}`,
          author: 'Test Author',
          installed: false
        }));
        
        mockPackageManager.listAvailablePacks.resolves(manyPacks);

        const result = await packageCommands.handlePackageCommand('list', ['available']);
        
        expect(result).to.include('... and 5 more packs');
      });
    });

    describe('install command', () => {
      it('should require a pack name', async () => {
        const result = await packageCommands.handlePackageCommand('install', []);
        
        expect(result).to.include('Please specify a pack name');
        expect(result).to.include('Usage:');
      });

      it('should install a pack successfully', async () => {
        mockPackageManager.validatePackSecurity.resolves({ safe: true, warnings: [] });
        mockPackageManager.installPack.resolves({ success: true, message: 'Pack installed successfully' });

        const result = await packageCommands.handlePackageCommand('install', ['http']);
        
        expect(result).to.include('✅');
        expect(result).to.include('Pack installed successfully');
        expect(mockPackageManager.validatePackSecurity).to.have.been.calledWith('http');
        expect(mockPackageManager.installPack).to.have.been.calledWith('http');
      });

      it('should handle security warnings', async () => {
        mockPackageManager.validatePackSecurity.resolves({ 
          safe: false, 
          warnings: ['Pack is not from official repository'] 
        });
        windowStub.showWarningMessage.resolves('Cancel');

        const result = await packageCommands.handlePackageCommand('install', ['unsafe_pack']);
        
        expect(result).to.include('cancelled due to security concerns');
        expect(mockPackageManager.installPack).to.not.have.been.called;
      });

      it('should proceed with installation despite warnings if user confirms', async () => {
        mockPackageManager.validatePackSecurity.resolves({ 
          safe: false, 
          warnings: ['Pack is not from official repository'] 
        });
        windowStub.showWarningMessage.resolves('Yes, Install Anyway');
        mockPackageManager.installPack.resolves({ success: true, message: 'Pack installed' });

        const result = await packageCommands.handlePackageCommand('install', ['unsafe_pack']);
        
        expect(result).to.include('✅');
        expect(mockPackageManager.installPack).to.have.been.calledWith('unsafe_pack');
      });

      it('should handle installation failures', async () => {
        mockPackageManager.validatePackSecurity.resolves({ safe: true, warnings: [] });
        mockPackageManager.installPack.resolves({ 
          success: false, 
          message: 'Installation failed',
          details: 'Network error'
        });

        const result = await packageCommands.handlePackageCommand('install', ['http']);
        
        expect(result).to.include('❌');
        expect(result).to.include('Installation failed');
        expect(result).to.include('Network error');
      });
    });

    describe('uninstall command', () => {
      it('should require a pack name', async () => {
        const result = await packageCommands.handlePackageCommand('uninstall', []);
        
        expect(result).to.include('Please specify a pack name');
      });

      it('should uninstall a pack with confirmation', async () => {
        windowStub.showWarningMessage.resolves('Yes, Uninstall');
        mockPackageManager.uninstallPack.resolves({ success: true, message: 'Pack uninstalled' });

        const result = await packageCommands.handlePackageCommand('uninstall', ['http']);
        
        expect(result).to.include('✅');
        expect(result).to.include('Pack uninstalled');
        expect(mockPackageManager.uninstallPack).to.have.been.calledWith('http');
      });

      it('should cancel uninstallation if user declines', async () => {
        windowStub.showWarningMessage.resolves('Cancel');

        const result = await packageCommands.handlePackageCommand('uninstall', ['http']);
        
        expect(result).to.include('cancelled');
        expect(mockPackageManager.uninstallPack).to.not.have.been.called;
      });

      it('should handle both uninstall and remove commands', async () => {
        windowStub.showWarningMessage.resolves('Yes, Uninstall');
        mockPackageManager.uninstallPack.resolves({ success: true, message: 'Pack removed' });

        const result = await packageCommands.handlePackageCommand('remove', ['http']);
        
        expect(result).to.include('✅');
        expect(mockPackageManager.uninstallPack).to.have.been.calledWith('http');
      });
    });

    describe('update command', () => {
      it('should update all outdated packs when no pack specified', async () => {
        const outdatedPacks = [
          { name: 'http', version: '1.0.0', outdated: true },
          { name: 'clpfd', version: '2.0.0', outdated: true }
        ];
        
        mockPackageManager.checkOutdatedPacks.resolves(outdatedPacks);
        windowStub.showInformationMessage.resolves('Yes, Update All');
        mockPackageManager.updatePack.onFirstCall().resolves({ success: true, message: 'Updated' });
        mockPackageManager.updatePack.onSecondCall().resolves({ success: true, message: 'Updated' });

        const result = await packageCommands.handlePackageCommand('update', []);
        
        expect(result).to.include('Updating 2 pack(s)');
        expect(result).to.include('http: ✅');
        expect(result).to.include('clpfd: ✅');
        expect(mockPackageManager.updatePack).to.have.been.calledTwice;
      });

      it('should report when all packs are up to date', async () => {
        mockPackageManager.checkOutdatedPacks.resolves([]);

        const result = await packageCommands.handlePackageCommand('update', []);
        
        expect(result).to.include('All installed packs are up to date');
      });

      it('should update a specific pack', async () => {
        mockPackageManager.updatePack.resolves({ success: true, message: 'Pack updated' });

        const result = await packageCommands.handlePackageCommand('update', ['http']);
        
        expect(result).to.include('✅');
        expect(result).to.include('Pack updated');
        expect(mockPackageManager.updatePack).to.have.been.calledWith('http');
      });

      it('should handle both update and upgrade commands', async () => {
        mockPackageManager.updatePack.resolves({ success: true, message: 'Pack upgraded' });

        const result = await packageCommands.handlePackageCommand('upgrade', ['http']);
        
        expect(result).to.include('✅');
        expect(mockPackageManager.updatePack).to.have.been.calledWith('http');
      });
    });

    describe('info command', () => {
      it('should require a pack name', async () => {
        const result = await packageCommands.handlePackageCommand('info', []);
        
        expect(result).to.include('Please specify a pack name');
      });

      it('should display pack information', async () => {
        const packInfo: PrologPack = {
          name: 'http',
          title: 'HTTP client and server library',
          version: '1.0.0',
          author: 'Jan Wielemaker',
          description: 'A comprehensive HTTP library',
          home: 'https://swi-prolog.org',
          requires: ['ssl', 'uri'],
          installed: true
        };
        
        mockPackageManager.getPackInfo.resolves(packInfo);

        const result = await packageCommands.handlePackageCommand('info', ['http']);
        
        expect(result).to.include('Pack Information: http');
        expect(result).to.include('HTTP client and server library');
        expect(result).to.include('v1.0.0');
        expect(result).to.include('Jan Wielemaker');
        expect(result).to.include('ssl, uri');
        expect(result).to.include('✅ Yes');
      });

      it('should handle pack not found', async () => {
        mockPackageManager.getPackInfo.resolves(null);

        const result = await packageCommands.handlePackageCommand('info', ['nonexistent']);
        
        expect(result).to.include('not found or information unavailable');
      });

      it('should handle both info and show commands', async () => {
        mockPackageManager.getPackInfo.resolves(mockPacks[0]);

        const result = await packageCommands.handlePackageCommand('show', ['http']);
        
        expect(result).to.include('Pack Information');
        expect(mockPackageManager.getPackInfo).to.have.been.calledWith('http');
      });
    });

    describe('search command', () => {
      it('should require a search keyword', async () => {
        const result = await packageCommands.handlePackageCommand('search', []);
        
        expect(result).to.include('Please specify a search keyword');
      });

      it('should return search results', async () => {
        mockPackageManager.searchPacks.resolves([mockPacks[0]]);

        const result = await packageCommands.handlePackageCommand('search', ['http']);
        
        expect(result).to.include('Search Results for \'http\'');
        expect(result).to.include('http');
        expect(result).to.include('v1.0.0');
        expect(mockPackageManager.searchPacks).to.have.been.calledWith('http');
      });

      it('should handle no search results', async () => {
        mockPackageManager.searchPacks.resolves([]);

        const result = await packageCommands.handlePackageCommand('search', ['nonexistent']);
        
        expect(result).to.include('No packs found matching');
      });

      it('should join multiple search terms', async () => {
        mockPackageManager.searchPacks.resolves([]);

        await packageCommands.handlePackageCommand('search', ['http', 'client']);
        
        expect(mockPackageManager.searchPacks).to.have.been.calledWith('http client');
      });

      it('should limit search results to 10', async () => {
        const manyPacks = Array.from({ length: 15 }, (_, i) => ({
          name: `pack${i}`,
          version: '1.0.0',
          title: `Pack ${i}`,
          author: 'Test Author',
          installed: false
        }));
        
        mockPackageManager.searchPacks.resolves(manyPacks);

        const result = await packageCommands.handlePackageCommand('search', ['test']);
        
        expect(result).to.include('... and 5 more results');
      });
    });

    describe('outdated command', () => {
      it('should list outdated packs', async () => {
        const outdatedPacks = [
          { name: 'http', version: '1.0.0', outdated: true, title: 'HTTP library' }
        ];
        
        mockPackageManager.checkOutdatedPacks.resolves(outdatedPacks);

        const result = await packageCommands.handlePackageCommand('outdated', []);
        
        expect(result).to.include('Outdated Packs');
        expect(result).to.include('http');
        expect(result).to.include('Use `/prolog pack update`');
      });

      it('should report when no packs are outdated', async () => {
        mockPackageManager.checkOutdatedPacks.resolves([]);

        const result = await packageCommands.handlePackageCommand('outdated', []);
        
        expect(result).to.include('All installed packs are up to date');
      });
    });

    describe('servers command', () => {
      it('should list configured servers', async () => {
        const servers = ['https://swi-prolog.org/pack/list', 'https://custom-server.com'];
        mockPackageManager.getPackServers.returns(servers);

        const result = await packageCommands.handlePackageCommand('servers', []);
        
        expect(result).to.include('Configured Pack Servers');
        expect(result).to.include('swi-prolog.org');
        expect(result).to.include('custom-server.com');
        expect(result).to.include('(default)');
      });

      it('should add a new server', async () => {
        const result = await packageCommands.handlePackageCommand('servers', ['add', 'https://new-server.com']);
        
        expect(result).to.include('Added pack server');
        expect(result).to.include('https://new-server.com');
        expect(mockPackageManager.addPackServer).to.have.been.calledWith('https://new-server.com');
      });

      it('should reject invalid URLs when adding', async () => {
        const result = await packageCommands.handlePackageCommand('servers', ['add', 'invalid-url']);
        
        expect(result).to.include('Invalid URL');
      });

      it('should remove a server', async () => {
        const result = await packageCommands.handlePackageCommand('servers', ['remove', 'https://old-server.com']);
        
        expect(result).to.include('Removed pack server');
        expect(mockPackageManager.removePackServer).to.have.been.calledWith('https://old-server.com');
      });
    });

    describe('help command', () => {
      it('should return help message for unknown commands', async () => {
        const result = await packageCommands.handlePackageCommand('unknown', []);
        
        expect(result).to.include('Prolog Package Manager Commands');
        expect(result).to.include('Basic Commands');
        expect(result).to.include('Examples');
      });
    });

    describe('error handling', () => {
      it('should handle exceptions gracefully', async () => {
        mockPackageManager.listInstalledPacks.rejects(new Error('Backend error'));

        const result = await packageCommands.handlePackageCommand('list', []);
        
        expect(result).to.include('❌ Command failed');
        expect(result).to.include('Backend error');
      });
    });
  });

  describe('showPackPicker', () => {
    it('should show available packs for installation', async () => {
      const availablePacks = [mockPacks[1]]; // clpfd (not installed)
      const installedPacks = [mockPacks[0]]; // http (installed)
      
      mockPackageManager.listAvailablePacks.resolves(mockPacks);
      mockPackageManager.listInstalledPacks.resolves(installedPacks);
      mockPackageManager.installPack.resolves({ success: true, message: 'Installed' });
      
      windowStub.showQuickPick.resolves({
        label: 'clpfd',
        pack: mockPacks[1]
      });

      await packageCommands.showPackPicker();
      
      expect(windowStub.showQuickPick).to.have.been.calledOnce;
      expect(mockPackageManager.installPack).to.have.been.calledWith('clpfd');
      expect(windowStub.showInformationMessage).to.have.been.calledWith('Installed');
    });

    it('should handle installation failure in picker', async () => {
      mockPackageManager.listAvailablePacks.resolves([mockPacks[1]]);
      mockPackageManager.listInstalledPacks.resolves([]);
      mockPackageManager.installPack.resolves({ success: false, message: 'Failed' });
      
      windowStub.showQuickPick.resolves({
        label: 'clpfd',
        pack: mockPacks[1]
      });

      await packageCommands.showPackPicker();
      
      expect(windowStub.showErrorMessage).to.have.been.calledWith('Failed');
    });

    it('should handle user cancellation', async () => {
      mockPackageManager.listAvailablePacks.resolves([mockPacks[1]]);
      mockPackageManager.listInstalledPacks.resolves([]);
      
      windowStub.showQuickPick.resolves(undefined); // User cancelled

      await packageCommands.showPackPicker();
      
      expect(mockPackageManager.installPack).to.not.have.been.called;
    });
  });

  describe('showUninstallPicker', () => {
    it('should show installed packs for uninstallation', async () => {
      mockPackageManager.listInstalledPacks.resolves([mockPacks[0]]);
      mockPackageManager.uninstallPack.resolves({ success: true, message: 'Uninstalled' });
      
      windowStub.showQuickPick.resolves({
        label: 'http',
        pack: mockPacks[0]
      });
      windowStub.showWarningMessage.resolves('Yes, Uninstall');

      await packageCommands.showUninstallPicker();
      
      expect(windowStub.showQuickPick).to.have.been.calledOnce;
      expect(mockPackageManager.uninstallPack).to.have.been.calledWith('http');
      expect(windowStub.showInformationMessage).to.have.been.calledWith('Uninstalled');
    });

    it('should handle no installed packs', async () => {
      mockPackageManager.listInstalledPacks.resolves([]);

      await packageCommands.showUninstallPicker();
      
      expect(windowStub.showInformationMessage).to.have.been.calledWith('No packs are currently installed.');
      expect(windowStub.showQuickPick).to.not.have.been.called;
    });

    it('should handle user declining uninstallation', async () => {
      mockPackageManager.listInstalledPacks.resolves([mockPacks[0]]);
      
      windowStub.showQuickPick.resolves({
        label: 'http',
        pack: mockPacks[0]
      });
      windowStub.showWarningMessage.resolves('Cancel');

      await packageCommands.showUninstallPicker();
      
      expect(mockPackageManager.uninstallPack).to.not.have.been.called;
    });
  });
});