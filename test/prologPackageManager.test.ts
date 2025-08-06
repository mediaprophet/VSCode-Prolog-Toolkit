import { expect } from 'chai';
import { afterEach, beforeEach, describe, it } from 'mocha';
import * as sinon from 'sinon';
import { PrologPack, PrologPackageManager } from '../src/features/prologPackageManager.js';
import { PrologBackend } from '../src/prologBackend.js';

describe('PrologPackageManager', () => {
  let packageManager: PrologPackageManager;
  let mockBackend: sinon.SinonStubbedInstance<PrologBackend>;
  let windowStub: {
    showErrorMessage: sinon.SinonStub;
    showWarningMessage: sinon.SinonStub;
    showInformationMessage: sinon.SinonStub;
    withProgress: sinon.SinonStub;
  };

  beforeEach(() => {
    // Create mock backend
    mockBackend = sinon.createStubInstance(PrologBackend);
    packageManager = new PrologPackageManager(mockBackend as unknown as PrologBackend);

    // Mock VS Code window API
    windowStub = {
      showErrorMessage: sinon.stub(),
      showWarningMessage: sinon.stub(),
      showInformationMessage: sinon.stub(),
      withProgress: sinon.stub()
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

  describe('listAvailablePacks', () => {
    it('should return empty array when no packs are available', async () => {
      mockBackend.sendRequest.resolves({
        status: 'ok',
        results: []
      });

      const packs = await packageManager.listAvailablePacks();
      expect(packs).to.be.an('array').that.is.empty;
    });

    it('should parse and return available packs', async () => {
      const mockResponse = {
        status: 'ok',
        results: [[
          {
            functor: 'pack',
            args: ['http', 'HTTP client and server library', '1.0.0', 'Jan Wielemaker', 'https://swi-prolog.org', 'https://github.com/SWI-Prolog/packages-http']
          },
          {
            functor: 'pack',
            args: ['clpfd', 'Constraint Logic Programming over Finite Domains', '2.1.0', 'Markus Triska', 'https://swi-prolog.org', 'https://github.com/triska/clpfd']
          }
        ]]
      };

      mockBackend.sendRequest.resolves(mockResponse);

      const packs = await packageManager.listAvailablePacks();
      expect(packs).to.have.length(2);
      expect(packs[0]).to.deep.include({
        name: 'http',
        title: 'HTTP client and server library',
        version: '1.0.0',
        author: 'Jan Wielemaker'
      });
      expect(packs[1]).to.deep.include({
        name: 'clpfd',
        title: 'Constraint Logic Programming over Finite Domains',
        version: '2.1.0',
        author: 'Markus Triska'
      });
    });

    it('should handle backend errors gracefully', async () => {
      mockBackend.sendRequest.rejects(new Error('Backend connection failed'));

      const packs = await packageManager.listAvailablePacks();
      expect(packs).to.be.an('array').that.is.empty;
      expect(windowStub.showErrorMessage).to.have.been.calledOnce;
    });
  });

  describe('listInstalledPacks', () => {
    it('should return installed packs with installed flag set to true', async () => {
      const mockResponse = {
        status: 'ok',
        results: [[
          {
            functor: 'pack',
            args: ['http', 'HTTP client and server library', '1.0.0', 'Jan Wielemaker', 'https://swi-prolog.org']
          }
        ]]
      };

      mockBackend.sendRequest.resolves(mockResponse);

      const packs = await packageManager.listInstalledPacks();
      expect(packs).to.have.length(1);
      expect(packs[0]).to.deep.include({
        name: 'http',
        installed: true
      });
    });
  });

  describe('installPack', () => {
    beforeEach(() => {
      // Mock withProgress to immediately call the callback
      windowStub.withProgress.callsFake((options: unknown, callback: (progress: { report: sinon.SinonStub }, token: { isCancellationRequested: boolean }) => unknown) => {
        const mockProgress = { report: sinon.stub() };
        const mockToken = { isCancellationRequested: false };
        return callback(mockProgress, mockToken);
      });
    });

    it('should reject invalid pack names', async () => {
      const result = await packageManager.installPack('invalid@pack!name');
      expect(result.success).to.be.false;
      expect(result.message).to.include('Invalid pack name');
    });

    it('should successfully install a valid pack', async () => {
      // Mock available packs check
      mockBackend.sendRequest.onFirstCall().resolves({
        status: 'ok',
        results: [[
          {
            functor: 'pack',
            args: ['http', 'HTTP client and server library', '1.0.0', 'Jan Wielemaker', 'https://swi-prolog.org', 'https://github.com/SWI-Prolog/packages-http']
          }
        ]]
      });

      // Mock installation
      mockBackend.sendRequest.onSecondCall().resolves({
        status: 'ok'
      });

      // Mock verification
      mockBackend.sendRequest.onThirdCall().resolves({
        status: 'ok',
        results: [[
          {
            functor: 'pack',
            args: ['http', 'HTTP client and server library', '1.0.0', 'Jan Wielemaker', 'https://swi-prolog.org']
          }
        ]]
      });

      const result = await packageManager.installPack('http');
      expect(result.success).to.be.true;
      expect(result.message).to.include('installed successfully');
    });

    it('should handle pack not found error', async () => {
      // Mock empty available packs
      mockBackend.sendRequest.resolves({
        status: 'ok',
        results: []
      });

      const result = await packageManager.installPack('nonexistent');
      expect(result.success).to.be.false;
      expect(result.message).to.include('not found in available packs');
    });

    it('should handle installation failure', async () => {
      // Mock available packs check
      mockBackend.sendRequest.onFirstCall().resolves({
        status: 'ok',
        results: [[
          {
            functor: 'pack',
            args: ['http', 'HTTP client and server library', '1.0.0', 'Jan Wielemaker', 'https://swi-prolog.org', 'https://github.com/SWI-Prolog/packages-http']
          }
        ]]
      });

      // Mock installation failure
      mockBackend.sendRequest.onSecondCall().resolves({
        status: 'error',
        error: 'Installation failed',
        message: 'Network error'
      });

      const result = await packageManager.installPack('http');
      expect(result.success).to.be.false;
      expect(result.message).to.include('Failed to install');
    });
  });

  describe('uninstallPack', () => {
    beforeEach(() => {
      windowStub.withProgress.callsFake((options: unknown, callback: (progress: { report: sinon.SinonStub }) => unknown) => {
        const mockProgress = { report: sinon.stub() };
        return callback(mockProgress);
      });
    });

    it('should successfully uninstall an installed pack', async () => {
      // Mock installed packs check
      mockBackend.sendRequest.onFirstCall().resolves({
        status: 'ok',
        results: [[
          {
            functor: 'pack',
            args: ['http', 'HTTP client and server library', '1.0.0', 'Jan Wielemaker', 'https://swi-prolog.org']
          }
        ]]
      });

      // Mock uninstallation
      mockBackend.sendRequest.onSecondCall().resolves({
        status: 'ok'
      });

      // Mock verification (empty results = pack removed)
      mockBackend.sendRequest.onThirdCall().resolves({
        status: 'ok',
        results: []
      });

      const result = await packageManager.uninstallPack('http');
      expect(result.success).to.be.true;
      expect(result.message).to.include('uninstalled successfully');
    });

    it('should handle pack not installed error', async () => {
      // Mock empty installed packs
      mockBackend.sendRequest.resolves({
        status: 'ok',
        results: []
      });

      const result = await packageManager.uninstallPack('nonexistent');
      expect(result.success).to.be.false;
      expect(result.message).to.include('is not installed');
    });
  });

  describe('updatePack', () => {
    beforeEach(() => {
      windowStub.withProgress.callsFake((options: unknown, callback: (progress: { report: sinon.SinonStub }, token: { isCancellationRequested: boolean }) => unknown) => {
        const mockProgress = { report: sinon.stub() };
        const mockToken = { isCancellationRequested: false };
        return callback(mockProgress, mockToken);
      });
    });

    it('should successfully update a pack', async () => {
      mockBackend.sendRequest.resolves({
        status: 'ok'
      });

      const result = await packageManager.updatePack('http');
      expect(result.success).to.be.true;
      expect(result.message).to.include('updated successfully');
    });

    it('should handle update failure', async () => {
      mockBackend.sendRequest.resolves({
        status: 'error',
        error: 'Update failed'
      });

      const result = await packageManager.updatePack('http');
      expect(result.success).to.be.false;
      expect(result.message).to.include('Failed to update');
    });
  });

  describe('getPackInfo', () => {
    it('should return detailed pack information', async () => {
      const mockResponse = {
        status: 'ok',
        results: [[
          { functor: '-', args: ['title', 'HTTP client and server library'] },
          { functor: '-', args: ['version', '1.0.0'] },
          { functor: '-', args: ['author', 'Jan Wielemaker'] },
          { functor: '-', args: ['home', 'https://swi-prolog.org'] },
          { functor: '-', args: ['requires', ['ssl', 'uri']] }
        ]]
      };

      mockBackend.sendRequest.resolves(mockResponse);

      const packInfo = await packageManager.getPackInfo('http');
      expect(packInfo).to.not.be.null;
      expect(packInfo!.name).to.equal('http');
      expect(packInfo!.title).to.equal('HTTP client and server library');
      expect(packInfo!.version).to.equal('1.0.0');
      expect(packInfo!.author).to.equal('Jan Wielemaker');
      expect(packInfo!.requires).to.deep.equal(['ssl', 'uri']);
    });

    it('should return null for non-existent pack', async () => {
      mockBackend.sendRequest.resolves({
        status: 'ok',
        results: []
      });

      const packInfo = await packageManager.getPackInfo('nonexistent');
      expect(packInfo).to.be.null;
    });
  });

  describe('searchPacks', () => {
    it('should return search results', async () => {
      const mockResponse = {
        status: 'ok',
        results: [[
          {
            functor: 'pack',
            args: ['http', 'HTTP client and server library', '1.0.0', 'Jan Wielemaker', 'https://swi-prolog.org', 'https://github.com/SWI-Prolog/packages-http']
          }
        ]]
      };

      mockBackend.sendRequest.resolves(mockResponse);

      const packs = await packageManager.searchPacks('http');
      expect(packs).to.have.length(1);
      expect(packs[0].name).to.equal('http');
    });

    it('should return empty array for empty search term', async () => {
      const packs = await packageManager.searchPacks('');
      expect(packs).to.be.an('array').that.is.empty;
    });
  });

  describe('checkOutdatedPacks', () => {
    it('should return outdated packs', async () => {
      const mockResponse = {
        status: 'ok',
        results: [[
          {
            functor: 'pack',
            args: ['http', '1.0.0', '1.1.0']
          }
        ]]
      };

      mockBackend.sendRequest.resolves(mockResponse);

      const outdatedPacks = await packageManager.checkOutdatedPacks();
      expect(outdatedPacks).to.have.length(1);
      expect(outdatedPacks[0].outdated).to.be.true;
    });
  });

  describe('validatePackSecurity', () => {
    it('should validate pack security and return warnings', async () => {
      // Mock pack info
      const mockPackInfo: PrologPack = {
        name: 'test_pack',
        home: 'http://example.com', // Non-HTTPS, non-official
        download: 'http://example.com/download' // Non-HTTPS
      };

      // Stub getPackInfo method
      sinon.stub(packageManager, 'getPackInfo').resolves(mockPackInfo);

      const result = await packageManager.validatePackSecurity('test_pack');
      expect(result.safe).to.be.false;
      expect(result.warnings).to.have.length.greaterThan(0);
      expect(result.warnings.some(w => w.includes('not from the official SWI-Prolog repository'))).to.be.true;
      expect(result.warnings.some(w => w.includes('not using HTTPS'))).to.be.true;
    });

    it('should mark official packs as safe', async () => {
      const mockPackInfo: PrologPack = {
        name: 'http',
        home: 'https://swi-prolog.org/pack/http',
        download: 'https://github.com/SWI-Prolog/packages-http'
      };

      sinon.stub(packageManager, 'getPackInfo').resolves(mockPackInfo);

      const result = await packageManager.validatePackSecurity('http');
      expect(result.safe).to.be.true;
      expect(result.warnings).to.have.length(0);
    });
  });

  describe('pack server management', () => {
    it('should add custom pack servers', () => {
      const initialServers = packageManager.getPackServers();
      const newServer = 'https://custom-pack-server.com';

      packageManager.addPackServer(newServer);
      const updatedServers = packageManager.getPackServers();

      expect(updatedServers).to.have.length(initialServers.length + 1);
      expect(updatedServers).to.include(newServer);
    });

    it('should not add duplicate servers', () => {
      const initialServers = packageManager.getPackServers();
      const existingServer = initialServers[0];

      packageManager.addPackServer(existingServer);
      const updatedServers = packageManager.getPackServers();

      expect(updatedServers).to.have.length(initialServers.length);
    });

    it('should remove custom pack servers but not default', () => {
      const customServer = 'https://custom-pack-server.com';
      packageManager.addPackServer(customServer);

      const beforeRemoval = packageManager.getPackServers();
      packageManager.removePackServer(customServer);
      const afterRemoval = packageManager.getPackServers();

      expect(afterRemoval).to.have.length(beforeRemoval.length - 1);
      expect(afterRemoval).to.not.include(customServer);

      // Try to remove default server (should not work)
      const defaultServer = afterRemoval[0];
      packageManager.removePackServer(defaultServer);
      const afterDefaultRemoval = packageManager.getPackServers();

      expect(afterDefaultRemoval).to.include(defaultServer);
    });
  });

  describe('pack name validation', () => {
    it('should accept valid pack names', () => {
      const validNames = ['http', 'clpfd', 'pack_name', 'pack-name', 'pack123'];

      for (const name of validNames) {
        const result = (packageManager as unknown as { validatePackName: (name: string) => boolean }).validatePackName(name);
        expect(result, `${name} should be valid`).to.be.true;
      }
    });

    it('should reject invalid pack names', () => {
      const invalidNames = ['pack@name', 'pack name', 'pack.name', 'pack/name', ''];

      for (const name of invalidNames) {
        const result = (packageManager as unknown as { validatePackName: (name: string) => boolean }).validatePackName(name);
        expect(result, `${name} should be invalid`).to.be.false;
      }
    });
  });
});