import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { InstallationChecker, InstallationStatus } from './installationChecker';
import { InstallationGuide } from './installationGuide';
import { PlatformUtils } from '../utils/platformUtils';

export class SettingsWebviewProvider implements vscode.WebviewViewProvider {
  public static readonly viewType = 'prologSettings';
  private _view?: vscode.WebviewView;
  private installationChecker: InstallationChecker;
  private installationGuide: InstallationGuide;

  constructor(private readonly _extensionUri: vscode.Uri) {
    this.installationChecker = InstallationChecker.getInstance();
    this.installationGuide = InstallationGuide.getInstance();
  }

  public resolveWebviewView(
    webviewView: vscode.WebviewView,
    context: vscode.WebviewViewResolveContext,
    _token: vscode.CancellationToken
  ) {
    this._view = webviewView;

    webviewView.webview.options = {
      enableScripts: true,
      localResourceRoots: [this._extensionUri],
    };

    webviewView.webview.html = this._getHtmlForWebview(webviewView.webview);

    // Handle messages from the webview
    webviewView.webview.onDidReceiveMessage(
      async message => {
        switch (message.type) {
          case 'updateSetting': {
            this._updateSetting(message.key, message.value);
            break;
          }
          case 'resetSettings': {
            this._resetSettings();
            break;
          }
          case 'exportSettings': {
            this._exportSettings();
            break;
          }
          case 'importSettings': {
            this._importSettings();
            break;
          }
          case 'validateSetting': {
            this._validateSetting(message.key, message.value);
            break;
          }
          case 'getSettings': {
            this._sendCurrentSettings();
            break;
          }
          case 'checkInstallation': {
            this._checkInstallation();
            break;
          }
          case 'autoDetectPath': {
            this._autoDetectPath();
            break;
          }
          case 'testInstallation': {
            this._testInstallation();
            break;
          }
          case 'runSetupWizard': {
            await this.installationGuide.runSetupWizard();
            break;
          }
          case 'showInstallationGuide': {
            await this.installationGuide.showInstallationGuideDialog();
            break;
          }
        }
      },
      undefined,
      []
    );

    // Send initial settings
    this._sendCurrentSettings();
  }

  private _updateSetting(key: string, value: any) {
    const config = vscode.workspace.getConfiguration('prolog');
    config.update(key, value, vscode.ConfigurationTarget.Global).then(
      () => {
        vscode.window.showInformationMessage(`Setting '${key}' updated successfully`);
        this._sendCurrentSettings(); // Refresh the UI
      },
      error => {
        vscode.window.showErrorMessage(`Failed to update setting '${key}': ${error}`);
      }
    );
  }

  private _resetSettings() {
    vscode.window
      .showWarningMessage(
        'Are you sure you want to reset all Prolog settings to their default values?',
        'Yes',
        'No'
      )
      .then(selection => {
        if (selection === 'Yes') {
          const config = vscode.workspace.getConfiguration('prolog');
          const settingsToReset = [
            'executablePath',
            'dialect',
            'linter.run',
            'format.addSpace',
            'linter.delay',
            'linter.enableMsgInOutput',
            'terminal.runtimeArgs',
            'telemetry.enabled',
            'apiServer.enabled',
            'apiServer.port',
            'apiServer.host',
            'apiServer.corsOrigins',
            'apiServer.maxConnections',
            'apiServer.requestTimeout',
            'apiServer.rateLimiting.enabled',
            'apiServer.rateLimiting.requestsPerMinute',
            'apiServer.rateLimiting.burstLimit',
            'apiServer.auth.method',
            'apiServer.auth.apiKeys',
            'apiServer.auth.jwtSecret',
            'apiServer.auth.jwtExpiration',
            'webSocketServer.enabled',
            'webSocketServer.port',
            'webSocketServer.maxConnections',
            'webSocketServer.heartbeatInterval',
          ];

          Promise.all(
            settingsToReset.map(key =>
              config.update(key, undefined, vscode.ConfigurationTarget.Global)
            )
          ).then(() => {
            vscode.window.showInformationMessage('All Prolog settings have been reset to defaults');
            this._sendCurrentSettings();
          });
        }
      });
  }

  private async _exportSettings() {
    const config = vscode.workspace.getConfiguration('prolog');
    const settings = {
      executablePath: config.get('executablePath'),
      dialect: config.get('dialect'),
      linter: {
        run: config.get('linter.run'),
        delay: config.get('linter.delay'),
        enableMsgInOutput: config.get('linter.enableMsgInOutput'),
      },
      format: {
        addSpace: config.get('format.addSpace'),
      },
      terminal: {
        runtimeArgs: config.get('terminal.runtimeArgs'),
      },
      telemetry: {
        enabled: config.get('telemetry.enabled'),
      },
      apiServer: {
        enabled: config.get('apiServer.enabled'),
        port: config.get('apiServer.port'),
        host: config.get('apiServer.host'),
        corsOrigins: config.get('apiServer.corsOrigins'),
        maxConnections: config.get('apiServer.maxConnections'),
        requestTimeout: config.get('apiServer.requestTimeout'),
        rateLimiting: {
          enabled: config.get('apiServer.rateLimiting.enabled'),
          requestsPerMinute: config.get('apiServer.rateLimiting.requestsPerMinute'),
          burstLimit: config.get('apiServer.rateLimiting.burstLimit'),
        },
        auth: {
          method: config.get('apiServer.auth.method'),
          jwtExpiration: config.get('apiServer.auth.jwtExpiration'),
        },
      },
      webSocketServer: {
        enabled: config.get('webSocketServer.enabled'),
        port: config.get('webSocketServer.port'),
        maxConnections: config.get('webSocketServer.maxConnections'),
        heartbeatInterval: config.get('webSocketServer.heartbeatInterval'),
      },
    };

    const uri = await vscode.window.showSaveDialog({
      defaultUri: vscode.Uri.file('prolog-settings.json'),
      filters: {
        'JSON files': ['json'],
      },
    });

    if (uri) {
      const normalizedPath = PlatformUtils.normalizePath(uri.fsPath);
      fs.writeFileSync(normalizedPath, JSON.stringify(settings, null, 2));
      vscode.window.showInformationMessage(`Settings exported to ${normalizedPath}`);
    }
  }

  private async _importSettings() {
    const uri = await vscode.window.showOpenDialog({
      canSelectFiles: true,
      canSelectFolders: false,
      canSelectMany: false,
      filters: {
        'JSON files': ['json'],
      },
    });

    if (uri?.[0]) {
      try {
        const normalizedPath = PlatformUtils.normalizePath(uri[0].fsPath);
        const settingsJson = fs.readFileSync(normalizedPath, 'utf8');
        const settings = JSON.parse(settingsJson);

        const config = vscode.workspace.getConfiguration('prolog');

        // Apply imported settings
        const promises = [];
        if (settings.executablePath)
          promises.push(
            config.update(
              'executablePath',
              settings.executablePath,
              vscode.ConfigurationTarget.Global
            )
          );
        if (settings.dialect)
          promises.push(
            config.update('dialect', settings.dialect, vscode.ConfigurationTarget.Global)
          );
        if (settings.linter?.run)
          promises.push(
            config.update('linter.run', settings.linter.run, vscode.ConfigurationTarget.Global)
          );
        if (settings.linter?.delay)
          promises.push(
            config.update('linter.delay', settings.linter.delay, vscode.ConfigurationTarget.Global)
          );
        if (settings.linter?.enableMsgInOutput !== undefined)
          promises.push(
            config.update(
              'linter.enableMsgInOutput',
              settings.linter.enableMsgInOutput,
              vscode.ConfigurationTarget.Global
            )
          );
        if (settings.format?.addSpace !== undefined)
          promises.push(
            config.update(
              'format.addSpace',
              settings.format.addSpace,
              vscode.ConfigurationTarget.Global
            )
          );
        if (settings.terminal?.runtimeArgs)
          promises.push(
            config.update(
              'terminal.runtimeArgs',
              settings.terminal.runtimeArgs,
              vscode.ConfigurationTarget.Global
            )
          );
        if (settings.telemetry?.enabled !== undefined)
          promises.push(
            config.update(
              'telemetry.enabled',
              settings.telemetry.enabled,
              vscode.ConfigurationTarget.Global
            )
          );

        // API Server settings
        if (settings.apiServer) {
          if (settings.apiServer.enabled !== undefined)
            promises.push(
              config.update(
                'apiServer.enabled',
                settings.apiServer.enabled,
                vscode.ConfigurationTarget.Global
              )
            );
          if (settings.apiServer.port)
            promises.push(
              config.update(
                'apiServer.port',
                settings.apiServer.port,
                vscode.ConfigurationTarget.Global
              )
            );
          if (settings.apiServer.host)
            promises.push(
              config.update(
                'apiServer.host',
                settings.apiServer.host,
                vscode.ConfigurationTarget.Global
              )
            );
          if (settings.apiServer.corsOrigins)
            promises.push(
              config.update(
                'apiServer.corsOrigins',
                settings.apiServer.corsOrigins,
                vscode.ConfigurationTarget.Global
              )
            );
          if (settings.apiServer.maxConnections)
            promises.push(
              config.update(
                'apiServer.maxConnections',
                settings.apiServer.maxConnections,
                vscode.ConfigurationTarget.Global
              )
            );
          if (settings.apiServer.requestTimeout)
            promises.push(
              config.update(
                'apiServer.requestTimeout',
                settings.apiServer.requestTimeout,
                vscode.ConfigurationTarget.Global
              )
            );

          if (settings.apiServer.rateLimiting) {
            if (settings.apiServer.rateLimiting.enabled !== undefined)
              promises.push(
                config.update(
                  'apiServer.rateLimiting.enabled',
                  settings.apiServer.rateLimiting.enabled,
                  vscode.ConfigurationTarget.Global
                )
              );
            if (settings.apiServer.rateLimiting.requestsPerMinute)
              promises.push(
                config.update(
                  'apiServer.rateLimiting.requestsPerMinute',
                  settings.apiServer.rateLimiting.requestsPerMinute,
                  vscode.ConfigurationTarget.Global
                )
              );
            if (settings.apiServer.rateLimiting.burstLimit)
              promises.push(
                config.update(
                  'apiServer.rateLimiting.burstLimit',
                  settings.apiServer.rateLimiting.burstLimit,
                  vscode.ConfigurationTarget.Global
                )
              );
          }

          if (settings.apiServer.auth) {
            if (settings.apiServer.auth.method)
              promises.push(
                config.update(
                  'apiServer.auth.method',
                  settings.apiServer.auth.method,
                  vscode.ConfigurationTarget.Global
                )
              );
            if (settings.apiServer.auth.jwtExpiration)
              promises.push(
                config.update(
                  'apiServer.auth.jwtExpiration',
                  settings.apiServer.auth.jwtExpiration,
                  vscode.ConfigurationTarget.Global
                )
              );
          }
        }

        // WebSocket Server settings
        if (settings.webSocketServer) {
          if (settings.webSocketServer.enabled !== undefined)
            promises.push(
              config.update(
                'webSocketServer.enabled',
                settings.webSocketServer.enabled,
                vscode.ConfigurationTarget.Global
              )
            );
          if (settings.webSocketServer.port)
            promises.push(
              config.update(
                'webSocketServer.port',
                settings.webSocketServer.port,
                vscode.ConfigurationTarget.Global
              )
            );
          if (settings.webSocketServer.maxConnections)
            promises.push(
              config.update(
                'webSocketServer.maxConnections',
                settings.webSocketServer.maxConnections,
                vscode.ConfigurationTarget.Global
              )
            );
          if (settings.webSocketServer.heartbeatInterval)
            promises.push(
              config.update(
                'webSocketServer.heartbeatInterval',
                settings.webSocketServer.heartbeatInterval,
                vscode.ConfigurationTarget.Global
              )
            );
        }

        await Promise.all(promises);
        vscode.window.showInformationMessage('Settings imported successfully');
        this._sendCurrentSettings();
      } catch (error) {
        vscode.window.showErrorMessage(`Failed to import settings: ${error}`);
      }
    }
  }

  private _validateSetting(key: string, value: any): boolean {
    let isValid = true;
    let errorMessage = '';

    switch (key) {
      case 'apiServer.port':
      case 'webSocketServer.port': {
        if (typeof value !== 'number' || value < 1024 || value > 65535) {
          isValid = false;
          errorMessage = 'Port must be a number between 1024 and 65535';
        }
        break;
      }
      case 'linter.delay': {
        if (typeof value !== 'number' || value < 0) {
          isValid = false;
          errorMessage = 'Delay must be a positive number';
        }
        break;
      }
      case 'apiServer.maxConnections':
      case 'webSocketServer.maxConnections': {
        if (typeof value !== 'number' || value < 1 || value > 1000) {
          isValid = false;
          errorMessage = 'Max connections must be between 1 and 1000';
        }
        break;
      }
      case 'apiServer.requestTimeout': {
        if (typeof value !== 'number' || value < 1000 || value > 300000) {
          isValid = false;
          errorMessage = 'Request timeout must be between 1000 and 300000 milliseconds';
        }
        break;
      }
      case 'apiServer.rateLimiting.requestsPerMinute': {
        if (typeof value !== 'number' || value < 1 || value > 1000) {
          isValid = false;
          errorMessage = 'Requests per minute must be between 1 and 1000';
        }
        break;
      }
      case 'apiServer.rateLimiting.burstLimit': {
        if (typeof value !== 'number' || value < 1 || value > 100) {
          isValid = false;
          errorMessage = 'Burst limit must be between 1 and 100';
        }
        break;
      }
      case 'webSocketServer.heartbeatInterval': {
        if (typeof value !== 'number' || value < 10 || value > 300) {
          isValid = false;
          errorMessage = 'Heartbeat interval must be between 10 and 300 seconds';
        }
        break;
      }
    }

    if (this._view) {
      this._view.webview.postMessage({
        type: 'validationResult',
        key: key,
        isValid: isValid,
        errorMessage: errorMessage,
      });
    }

    return isValid;
  }

  private async _sendCurrentSettings() {
    if (!this._view) {
      return;
    }

    const config = vscode.workspace.getConfiguration('prolog');

    // Check installation status
    const installationStatus = await this.installationChecker.checkSwiplInstallation();

    const settings = {
      executablePath: config.get('executablePath', '/usr/bin/swipl'),
      dialect: config.get('dialect', 'swi'),
      linter: {
        run: config.get('linter.run', 'onType'),
        delay: config.get('linter.delay', 500),
        enableMsgInOutput: config.get('linter.enableMsgInOutput', false),
      },
      format: {
        addSpace: config.get('format.addSpace', true),
      },
      terminal: {
        runtimeArgs: config.get('terminal.runtimeArgs', []),
      },
      telemetry: {
        enabled: config.get('telemetry.enabled', false),
      },
      apiServer: {
        enabled: config.get('apiServer.enabled', false),
        port: config.get('apiServer.port', 8080),
        host: config.get('apiServer.host', 'localhost'),
        corsOrigins: config.get('apiServer.corsOrigins', ['http://localhost:*']),
        maxConnections: config.get('apiServer.maxConnections', 100),
        requestTimeout: config.get('apiServer.requestTimeout', 60000),
        rateLimiting: {
          enabled: config.get('apiServer.rateLimiting.enabled', true),
          requestsPerMinute: config.get('apiServer.rateLimiting.requestsPerMinute', 60),
          burstLimit: config.get('apiServer.rateLimiting.burstLimit', 10),
        },
        auth: {
          method: config.get('apiServer.auth.method', 'local_only'),
          apiKeys: config.get('apiServer.auth.apiKeys', {}),
          jwtSecret: config.get('apiServer.auth.jwtSecret', ''),
          jwtExpiration: config.get('apiServer.auth.jwtExpiration', '1h'),
        },
      },
      webSocketServer: {
        enabled: config.get('webSocketServer.enabled', true),
        port: config.get('webSocketServer.port', 8081),
        maxConnections: config.get('webSocketServer.maxConnections', 50),
        heartbeatInterval: config.get('webSocketServer.heartbeatInterval', 30),
      },
      installation: installationStatus,
    };

    this._view.webview.postMessage({
      type: 'settingsData',
      settings: settings,
    });
  }

  private async _checkInstallation() {
    if (!this._view) {
      return;
    }

    try {
      const installationStatus = await this.installationChecker.checkSwiplInstallation();
      this._view.webview.postMessage({
        type: 'installationStatus',
        status: installationStatus,
      });
    } catch (error) {
      this._view.webview.postMessage({
        type: 'installationStatus',
        status: {
          isInstalled: false,
          issues: [`Error checking installation: ${error}`],
        },
      });
    }
  }

  private async _autoDetectPath() {
    if (!this._view) {
      return;
    }

    try {
      const foundPath = await this.installationChecker.findSwiplExecutable();
      if (foundPath) {
        const version = await this.installationChecker.getSwiplVersion(foundPath);

        // Update configuration
        const config = vscode.workspace.getConfiguration('prolog');
        await config.update('executablePath', foundPath, vscode.ConfigurationTarget.Global);

        this._view.webview.postMessage({
          type: 'autoDetectResult',
          success: true,
          path: foundPath,
          version: version,
        });

        // Refresh settings
        await this._sendCurrentSettings();
      } else {
        this._view.webview.postMessage({
          type: 'autoDetectResult',
          success: false,
          message: 'SWI-Prolog not found in common locations',
        });
      }
    } catch (error) {
      this._view.webview.postMessage({
        type: 'autoDetectResult',
        success: false,
        message: `Auto-detection failed: ${error}`,
      });
    }
  }

  private async _testInstallation() {
    if (!this._view) {
      return;
    }

    try {
      const config = vscode.workspace.getConfiguration('prolog');
      const executablePath = config.get<string>('executablePath', 'swipl');

      const isValid = await this.installationChecker.validateSwiplPath(executablePath);
      if (isValid) {
        const version = await this.installationChecker.getSwiplVersion(executablePath);
        this._view.webview.postMessage({
          type: 'testResult',
          success: true,
          message: `SWI-Prolog is working correctly (version ${version})`,
        });
      } else {
        this._view.webview.postMessage({
          type: 'testResult',
          success: false,
          message: 'SWI-Prolog executable is not valid or not accessible',
        });
      }
    } catch (error) {
      this._view.webview.postMessage({
        type: 'testResult',
        success: false,
        message: `Test failed: ${error}`,
      });
    }
  }

  private _getHtmlForWebview(webview: vscode.Webview): string {
    const scriptUri = webview.asWebviewUri(
      vscode.Uri.joinPath(this._extensionUri, 'media', 'settings.js')
    );
    const styleUri = webview.asWebviewUri(
      vscode.Uri.joinPath(this._extensionUri, 'media', 'settings.css')
    );

    return `<!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <link href="${styleUri}" rel="stylesheet">
            <title>Prolog Settings</title>
        </head>
        <body>
            <div class="settings-container">
                <div class="settings-header">
                    <h1>🔧 Prolog Extension Settings</h1>
                    <div class="settings-actions">
                        <button id="searchBtn" class="action-btn" title="Search Settings">🔍</button>
                        <button id="exportBtn" class="action-btn" title="Export Settings">📤</button>
                        <button id="importBtn" class="action-btn" title="Import Settings">📥</button>
                        <button id="resetBtn" class="action-btn danger" title="Reset All Settings">🔄</button>
                    </div>
                </div>

                <div class="search-container" id="searchContainer" style="display: none;">
                    <input type="text" id="searchInput" placeholder="Search settings..." />
                    <button id="clearSearch">✕</button>
                </div>

                <div class="settings-content">
                    <!-- Installation Status -->
                    <div class="settings-section" data-category="installation">
                        <h2 class="section-title">
                            <span class="section-icon">🔧</span>
                            SWI-Prolog Installation
                            <button class="section-toggle">−</button>
                        </h2>
                        <div class="section-content">
                            <div class="installation-status" id="installationStatus">
                                <div class="status-indicator" id="statusIndicator">
                                    <span class="status-icon">🔄</span>
                                    <span class="status-text">Checking installation...</span>
                                </div>
                                <div class="installation-details" id="installationDetails" style="display: none;">
                                    <div class="detail-item">
                                        <strong>Path:</strong> <span id="installationPath">-</span>
                                    </div>
                                    <div class="detail-item">
                                        <strong>Version:</strong> <span id="installationVersion">-</span>
                                    </div>
                                    <div class="detail-item" id="installationIssues" style="display: none;">
                                        <strong>Issues:</strong>
                                        <ul id="issuesList"></ul>
                                    </div>
                                </div>
                                <div class="installation-actions">
                                    <button class="action-btn" id="autoDetectBtn" title="Auto-detect SWI-Prolog">🔍 Auto-Detect</button>
                                    <button class="action-btn" id="testInstallationBtn" title="Test Installation">🧪 Test</button>
                                    <button class="action-btn" id="setupWizardBtn" title="Setup Wizard">🧙‍♂️ Setup Wizard</button>
                                    <button class="action-btn" id="installationGuideBtn" title="Installation Guide">📖 Install Guide</button>
                                </div>
                            </div>
                        </div>
                    </div>

                    <!-- Core Settings -->
                    <div class="settings-section" data-category="core">
                        <h2 class="section-title">
                            <span class="section-icon">⚙️</span>
                            Core Settings
                            <button class="section-toggle">−</button>
                        </h2>
                        <div class="section-content">
                            <div class="setting-group">
                                <label for="executablePath">Prolog Executable Path</label>
                                <input type="text" id="executablePath" data-key="executablePath" placeholder="/usr/bin/swipl" />
                                <div class="setting-description">Path to the Prolog executable (SWI-Prolog)</div>
                                <div class="validation-message" id="executablePath-validation"></div>
                            </div>

                            <div class="setting-group">
                                <label for="dialect">Prolog Dialect</label>
                                <select id="dialect" data-key="dialect">
                                    <option value="swi">SWI-Prolog</option>
                                    <option value="ecl">ECLiPSe</option>
                                </select>
                                <div class="setting-description">Choose your Prolog dialect</div>
                            </div>

                            <div class="setting-group">
                                <label for="terminalRuntimeArgs">Terminal Runtime Arguments</label>
                                <textarea id="terminalRuntimeArgs" data-key="terminal.runtimeArgs" placeholder='["--quiet", "--nosignals"]' rows="3"></textarea>
                                <div class="setting-description">JSON array of arguments passed to Prolog executable in terminal</div>
                                <div class="validation-message" id="terminalRuntimeArgs-validation"></div>
                            </div>
                        </div>
                    </div>

                    <!-- Linter Settings -->
                    <div class="settings-section" data-category="linter">
                        <h2 class="section-title">
                            <span class="section-icon">🔍</span>
                            Linter Settings
                            <button class="section-toggle">−</button>
                        </h2>
                        <div class="section-content">
                            <div class="setting-group">
                                <label for="linterRun">Linter Trigger</label>
                                <select id="linterRun" data-key="linter.run">
                                    <option value="onSave">On Save</option>
                                    <option value="onType">On Type</option>
                                    <option value="never">Never</option>
                                </select>
                                <div class="setting-description">When to trigger the linter</div>
                            </div>

                            <div class="setting-group">
                                <label for="linterDelay">Linter Delay (ms)</label>
                                <input type="number" id="linterDelay" data-key="linter.delay" min="0" max="5000" step="100" />
                                <div class="setting-description">Delay in milliseconds when using onType trigger</div>
                                <div class="validation-message" id="linterDelay-validation"></div>
                            </div>

                            <div class="setting-group">
                                <label class="checkbox-label">
                                    <input type="checkbox" id="linterEnableMsgInOutput" data-key="linter.enableMsgInOutput" />
                                    <span class="checkmark"></span>
                                    Enable Messages in Output Channel
                                </label>
                                <div class="setting-description">Show linter errors and warnings in OUTPUT channel</div>
                            </div>
                        </div>
                    </div>

                    <!-- Formatter Settings -->
                    <div class="settings-section" data-category="formatter">
                        <h2 class="section-title">
                            <span class="section-icon">📝</span>
                            Formatter Settings
                            <button class="section-toggle">−</button>
                        </h2>
                        <div class="section-content">
                            <div class="setting-group">
                                <label class="checkbox-label">
                                    <input type="checkbox" id="formatAddSpace" data-key="format.addSpace" />
                                    <span class="checkmark"></span>
                                    Add Space After Commas
                                </label>
                                <div class="setting-description">Automatically add spaces after commas during formatting</div>
                            </div>
                        </div>
                    </div>

                    <!-- API Server Settings -->
                    <div class="settings-section" data-category="api">
                        <h2 class="section-title">
                            <span class="section-icon">🌐</span>
                            API Server Settings
                            <button class="section-toggle">−</button>
                        </h2>
                        <div class="section-content">
                            <div class="setting-group">
                                <label class="checkbox-label">
                                    <input type="checkbox" id="apiServerEnabled" data-key="apiServer.enabled" />
                                    <span class="checkmark"></span>
                                    Enable API Server
                                </label>
                                <div class="setting-description">Enable HTTP API server for external AI agent access</div>
                            </div>

                            <div class="setting-group api-dependent">
                                <label for="apiServerPort">API Server Port</label>
                                <input type="number" id="apiServerPort" data-key="apiServer.port" min="1024" max="65535" />
                                <div class="setting-description">Port number for the HTTP API server</div>
                                <div class="validation-message" id="apiServerPort-validation"></div>
                            </div>

                            <div class="setting-group api-dependent">
                                <label for="apiServerHost">API Server Host</label>
                                <input type="text" id="apiServerHost" data-key="apiServer.host" placeholder="localhost" />
                                <div class="setting-description">Host address for the HTTP API server</div>
                            </div>

                            <div class="setting-group api-dependent">
                                <label for="apiServerCorsOrigins">CORS Origins</label>
                                <textarea id="apiServerCorsOrigins" data-key="apiServer.corsOrigins" placeholder='["http://localhost:*"]' rows="3"></textarea>
                                <div class="setting-description">JSON array of allowed CORS origins for API requests</div>
                                <div class="validation-message" id="apiServerCorsOrigins-validation"></div>
                            </div>

                            <div class="setting-group api-dependent">
                                <label for="apiServerMaxConnections">Max Connections</label>
                                <input type="number" id="apiServerMaxConnections" data-key="apiServer.maxConnections" min="1" max="1000" />
                                <div class="setting-description">Maximum number of concurrent connections</div>
                                <div class="validation-message" id="apiServerMaxConnections-validation"></div>
                            </div>

                            <div class="setting-group api-dependent">
                                <label for="apiServerRequestTimeout">Request Timeout (ms)</label>
                                <input type="number" id="apiServerRequestTimeout" data-key="apiServer.requestTimeout" min="1000" max="300000" step="1000" />
                                <div class="setting-description">Request timeout in milliseconds</div>
                                <div class="validation-message" id="apiServerRequestTimeout-validation"></div>
                            </div>

                            <!-- Rate Limiting -->
                            <div class="setting-subgroup api-dependent">
                                <h3>Rate Limiting</h3>
                                <div class="setting-group">
                                    <label class="checkbox-label">
                                        <input type="checkbox" id="apiServerRateLimitingEnabled" data-key="apiServer.rateLimiting.enabled" />
                                        <span class="checkmark"></span>
                                        Enable Rate Limiting
                                    </label>
                                    <div class="setting-description">Enable rate limiting for API requests</div>
                                </div>

                                <div class="setting-group rate-limiting-dependent">
                                    <label for="apiServerRateLimitingRequestsPerMinute">Requests Per Minute</label>
                                    <input type="number" id="apiServerRateLimitingRequestsPerMinute" data-key="apiServer.rateLimiting.requestsPerMinute" min="1" max="1000" />
                                    <div class="setting-description">Maximum requests per minute per client</div>
                                    <div class="validation-message" id="apiServerRateLimitingRequestsPerMinute-validation"></div>
                                </div>

                                <div class="setting-group rate-limiting-dependent">
                                    <label for="apiServerRateLimitingBurstLimit">Burst Limit</label>
                                    <input type="number" id="apiServerRateLimitingBurstLimit" data-key="apiServer.rateLimiting.burstLimit" min="1" max="100" />
                                    <div class="setting-description">Maximum burst requests allowed</div>
                                    <div class="validation-message" id="apiServerRateLimitingBurstLimit-validation"></div>
                                </div>
                            </div>

                            <!-- Authentication -->
                            <div class="setting-subgroup api-dependent">
                                <h3>Authentication</h3>
                                <div class="setting-group">
                                    <label for="apiServerAuthMethod">Authentication Method</label>
                                    <select id="apiServerAuthMethod" data-key="apiServer.auth.method">
                                        <option value="local_only">Local Only</option>
                                        <option value="api_key">API Key</option>
                                        <option value="jwt_token">JWT Token</option>
                                        <option value="oauth2">OAuth2</option>
                                    </select>
                                    <div class="setting-description">Authentication method for API access</div>
                                </div>

                                <div class="setting-group jwt-dependent">
                                    <label for="apiServerAuthJwtExpiration">JWT Token Expiration</label>
                                    <input type="text" id="apiServerAuthJwtExpiration" data-key="apiServer.auth.jwtExpiration" placeholder="1h" />
                                    <div class="setting-description">JWT token expiration time (e.g., '1h', '30m', '7d')</div>
                                </div>
                            </div>
                        </div>
                    </div>

                    <!-- WebSocket Server Settings -->
                    <div class="settings-section" data-category="websocket">
                        <h2 class="section-title">
                            <span class="section-icon">🔌</span>
                            WebSocket Server Settings
                            <button class="section-toggle">−</button>
                        </h2>
                        <div class="section-content">
                            <div class="setting-group">
                                <label class="checkbox-label">
                                    <input type="checkbox" id="webSocketServerEnabled" data-key="webSocketServer.enabled" />
                                    <span class="checkmark"></span>
                                    Enable WebSocket Server
                                </label>
                                <div class="setting-description">Enable WebSocket server for real-time notifications</div>
                            </div>

                            <div class="setting-group websocket-dependent">
                                <label for="webSocketServerPort">WebSocket Server Port</label>
                                <input type="number" id="webSocketServerPort" data-key="webSocketServer.port" min="1024" max="65535" />
                                <div class="setting-description">Port number for the WebSocket server</div>
                                <div class="validation-message" id="webSocketServerPort-validation"></div>
                            </div>

                            <div class="setting-group websocket-dependent">
                                <label for="webSocketServerMaxConnections">Max WebSocket Connections</label>
                                <input type="number" id="webSocketServerMaxConnections" data-key="webSocketServer.maxConnections" min="1" max="500" />
                                <div class="setting-description">Maximum number of concurrent WebSocket connections</div>
                                <div class="validation-message" id="webSocketServerMaxConnections-validation"></div>
                            </div>

                            <div class="setting-group websocket-dependent">
                                <label for="webSocketServerHeartbeatInterval">Heartbeat Interval (seconds)</label>
                                <input type="number" id="webSocketServerHeartbeatInterval" data-key="webSocketServer.heartbeatInterval" min="10" max="300" />
                                <div class="setting-description">WebSocket heartbeat interval in seconds</div>
                                <div class="validation-message" id="webSocketServerHeartbeatInterval-validation"></div>
                            </div>
                        </div>
                    </div>

                    <!-- Privacy & Telemetry Settings -->
                    <div class="settings-section" data-category="privacy">
                        <h2 class="section-title">
                            <span class="section-icon">🔒</span>
                            Privacy & Telemetry Settings
                            <button class="section-toggle">−</button>
                        </h2>
                        <div class="section-content">
                            <div class="setting-group">
                                <label class="checkbox-label">
                                    <input type="checkbox" id="telemetryEnabled" data-key="telemetry.enabled" />
                                    <span class="checkmark"></span>
                                    Enable Privacy-Respecting Telemetry
                                </label>
                                <div class="setting-description">Enable local usage analytics (data stays on your machine)</div>
                            </div>
                        </div>
                    </div>
                </div>

                <div class="settings-footer">
                    <div class="footer-info">
                        <p>💡 <strong>Tip:</strong> Changes are applied immediately. Restart VS Code if you experience issues.</p>
                        <p>📚 For more information, visit the <a href="https://github.com/mediaprophet/VSCode-Prolog-Toolkit" target="_blank">documentation</a>.</p>
                    </div>
                </div>
            </div>

            <script src="${scriptUri}"></script>
        </body>
        </html>`;
  }
}
