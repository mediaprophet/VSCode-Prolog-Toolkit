import type { PrologSettings } from './types.js';
import { defaultSettings } from './types.js';
// Use type-only import for createConnection type
import type { Connection } from 'vscode-languageserver';
// DidChangeConfigurationNotification is a value, import from 'vscode-languageserver' (ESM-compatible)
import { DidChangeConfigurationNotification } from 'vscode-languageserver';

export class ConfigurationManager {
  private globalSettings: PrologSettings = defaultSettings;
  private documentSettings: Map<string, Promise<PrologSettings>> = new Map();
  private hasConfigurationCapability = false;
  private connection: Connection;

  constructor(connection: Connection) {
    this.connection = connection;
  }

  public setConfigurationCapability(hasCapability: boolean): void {
    this.hasConfigurationCapability = hasCapability;
  }

  public getHasConfigurationCapability(): boolean {
    return this.hasConfigurationCapability;
  }

  public registerForConfigurationChanges(): void {
    if (this.hasConfigurationCapability) {
      // Register for all configuration changes.
      this.connection.client.register(DidChangeConfigurationNotification.type, undefined);
    }
  }

  public onDidChangeConfiguration(change: any): void {
    if (this.hasConfigurationCapability) {
      // Reset all cached document settings
      this.documentSettings.clear();
    } else {
      this.globalSettings = <PrologSettings>(change.settings.prolog || defaultSettings);
    }
  }

  public getDocumentSettings(resource: string): Promise<PrologSettings> {
    if (!this.hasConfigurationCapability) {
      return Promise.resolve(this.globalSettings);
    }
    let result = this.documentSettings.get(resource);
    if (!result) {
      result = this.connection.workspace.getConfiguration({
        scopeUri: resource,
        section: 'prolog',
      });
      this.documentSettings.set(resource, result);
    }
    // Always return a Promise<PrologSettings>
    return result ?? Promise.resolve(this.globalSettings);
  }

  public getGlobalSettings(): Promise<PrologSettings> {
    if (!this.hasConfigurationCapability) {
      return Promise.resolve(this.globalSettings);
    }
    return this.connection.workspace.getConfiguration('prolog');
  }

  public clearDocumentSettings(uri: string): void {
    this.documentSettings.delete(uri);
  }

  public clearAllDocumentSettings(): void {
    this.documentSettings.clear();
  }
}
