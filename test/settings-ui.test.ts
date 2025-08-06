import { expect } from 'chai';
import * as vscode from 'vscode';
import { SettingsWebviewProvider } from '../src/features/settingsWebviewProvider.js';

describe('Settings UI Tests', () => {
  let settingsProvider: SettingsWebviewProvider;
  let mockExtensionUri: vscode.Uri;

  before(() => {
    mockExtensionUri = vscode.Uri.file('/mock/extension/path');
    settingsProvider = new SettingsWebviewProvider(mockExtensionUri);
  });

  describe('SettingsWebviewProvider', () => {
    it('should create settings provider instance', () => {
      expect(settingsProvider).to.be.instanceOf(SettingsWebviewProvider);
    });

    it('should have correct viewType', () => {
      expect(SettingsWebviewProvider.viewType).to.equal('prologSettings');
    });

    it('should handle webview resolution', () => {
      // Mock webview view
      const mockWebviewView = {
        webview: {
          options: {},
          html: '',
          onDidReceiveMessage: () => ({ dispose: () => { } }),
          postMessage: () => Promise.resolve(true),
          asWebviewUri: (uri: vscode.Uri) => uri
        }
      } as any;

      // This should not throw
      expect(() => {
        settingsProvider.resolveWebviewView(
          mockWebviewView,
          {} as any,
          {} as any
        );
      }).to.not.throw();
    });
  });

  describe('Settings Configuration', () => {
    it('should have all required configuration properties', () => {
      const config = vscode.workspace.getConfiguration('prolog');

      // Test core settings
      expect(config.has('executablePath')).to.be.true;
      expect(config.has('dialect')).to.be.true;

      // Test linter settings
      expect(config.has('linter.run')).to.be.true;
      expect(config.has('linter.delay')).to.be.true;
      expect(config.has('linter.enableMsgInOutput')).to.be.true;

      // Test formatter settings
      expect(config.has('format.addSpace')).to.be.true;

      // Test API server settings
      expect(config.has('apiServer.enabled')).to.be.true;
      expect(config.has('apiServer.port')).to.be.true;
      expect(config.has('apiServer.host')).to.be.true;

      // Test WebSocket server settings
      expect(config.has('webSocketServer.enabled')).to.be.true;
      expect(config.has('webSocketServer.port')).to.be.true;

      // Test telemetry settings
      expect(config.has('telemetry.enabled')).to.be.true;
    });

    it('should have correct default values', () => {
      const config = vscode.workspace.getConfiguration('prolog');

      expect(config.get('executablePath')).to.equal('/usr/bin/swipl');
      expect(config.get('dialect')).to.equal('swi');
      expect(config.get('linter.run')).to.equal('onType');
      expect(config.get('linter.delay')).to.equal(500);
      expect(config.get('format.addSpace')).to.equal(true);
      expect(config.get('apiServer.enabled')).to.equal(false);
      expect(config.get('apiServer.port')).to.equal(8080);
      expect(config.get('webSocketServer.enabled')).to.equal(true);
      expect(config.get('webSocketServer.port')).to.equal(8081);
      expect(config.get('telemetry.enabled')).to.equal(false);
    });
  });

  describe('Settings Validation', () => {
    it('should validate port numbers correctly', () => {
      // Valid ports
      expect(isValidPort(8080)).to.be.true;
      expect(isValidPort(1024)).to.be.true;
      expect(isValidPort(65535)).to.be.true;

      // Invalid ports
      expect(isValidPort(1023)).to.be.false;
      expect(isValidPort(65536)).to.be.false;
      expect(isValidPort(-1)).to.be.false;
    });

    it('should validate positive numbers correctly', () => {
      expect(isPositiveNumber(1)).to.be.true;
      expect(isPositiveNumber(500)).to.be.true;
      expect(isPositiveNumber(0)).to.be.false;
      expect(isPositiveNumber(-1)).to.be.false;
    });

    it('should validate JSON arrays correctly', () => {
      expect(isValidJSON('[]')).to.be.true;
      expect(isValidJSON('["item1", "item2"]')).to.be.true;
      expect(isValidJSON('{"key": "value"}')).to.be.true;
      expect(isValidJSON('invalid json')).to.be.false;
      expect(isValidJSON('[invalid')).to.be.false;
    });
  });

  describe('Settings Commands', () => {
    it('should register prolog.openSettings command', async () => {
      const commands = await vscode.commands.getCommands();
      expect(commands).to.include('prolog.openSettings');
    });
  });
});

// Helper validation functions (these would normally be in the main code)
function isValidPort(port: number): boolean {
  return port >= 1024 && port <= 65535;
}

function isPositiveNumber(value: number): boolean {
  return value > 0;
}

function isValidJSON(value: string): boolean {
  try {
    JSON.parse(value);
    return true;
  } catch (e) {
    return false;
  }
}