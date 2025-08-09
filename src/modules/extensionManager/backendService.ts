import * as vscode from 'vscode';
import { ApiServer, ApiServerConfig } from '../../features/apiServer';
import { ExternalWebSocketConfig, ExternalWebSocketManager } from '../../features/externalWebSocketManager';
import { MultiIDESupport } from '../../features/multiIDESupport';
import { PrologLSPClient } from '../../features/prologLSPClient';
import { PrologBackend, PrologBackendOptions } from '../../prologBackend/PrologBackend';

export class BackendService {
  prologBackend: PrologBackend | null = null;
  prologLSPClient: PrologLSPClient | null = null;
  apiServer: ApiServer | null = null;
  externalWebSocketManager: ExternalWebSocketManager | null = null;
  queryNotificationManager: import('../../features/queryNotificationManager').QueryNotificationManager | null = null;

  async initializeBackendServices(context: vscode.ExtensionContext, configurationManager: any, chatHandler: any): Promise<void> {
    const config = vscode.workspace.getConfiguration('prolog');
    const swiplPath = config.get('executablePath', 'swipl') as string;

    // Initialize Prolog backend
    const backendOptions: PrologBackendOptions = {
      swiplPath,
      port: 3060,
      streamingEnabled: true,
      maxResultsPerChunk: 50,
    };
    this.prologBackend = new PrologBackend(backendOptions);

    // Update chat handler with backend reference
    if (chatHandler && typeof chatHandler.updateBackend === 'function') {
      chatHandler.updateBackend(this.prologBackend);
    }

    // Initialize API server if enabled
    const apiServerEnabled = config.get('apiServer.enabled', false) as boolean;
    if (apiServerEnabled && this.prologBackend) {
      try {
        const apiServerConfig: ApiServerConfig = {
          enabled: true,
          port: config.get('apiServer.port', 8080) as number,
          host: config.get('apiServer.host', 'localhost') as string,
          corsOrigins: config.get('apiServer.corsOrigins', ['http://localhost:*']) as string[],
          maxConnections: config.get('apiServer.maxConnections', 100) as number,
          requestTimeout: config.get('apiServer.requestTimeout', 60000) as number,
          rateLimiting: {
            enabled: config.get('apiServer.rateLimiting.enabled', true) as boolean,
            requestsPerMinute: config.get('apiServer.rateLimiting.requestsPerMinute', 60) as number,
            burstLimit: config.get('apiServer.rateLimiting.burstLimit', 10) as number,
          },
          auth: configurationManager.createAuthConfig(config),
        };

        this.apiServer = new ApiServer({
          config: apiServerConfig,
          prologBackend: this.prologBackend,
        });


        // Initialize external WebSocket manager if enabled
        const wsEnabled = config.get('webSocketServer.enabled', true) as boolean;
        if (wsEnabled) {
          const wsConfig: ExternalWebSocketConfig = {
            enabled: true,
            port: config.get('webSocketServer.port', 8081) as number,
            maxConnections: config.get('webSocketServer.maxConnections', 50) as number,
            heartbeatInterval: config.get('webSocketServer.heartbeatInterval', 30) as number,
            auth: apiServerConfig.auth,
          };

          // Use QueryNotificationManager for external WebSocket notifications
          const { QueryNotificationManager } = await import('../../features/queryNotificationManager');
          this.queryNotificationManager = new QueryNotificationManager({
            enableWebSocket: true,
            webSocketPort: wsConfig.port
          });
          this.externalWebSocketManager = new ExternalWebSocketManager(
            wsConfig,
            this.queryNotificationManager
          );
        }

        console.log('[Extension] API server and WebSocket manager initialized');
      } catch (error) {
        console.error('[Extension] Failed to initialize API server:', error);
        vscode.window.showErrorMessage(`Failed to initialize API server: ${error}`);
      }
    }

    // Initialize LSP services
    if (this.prologBackend) {
      this.prologLSPClient = new PrologLSPClient(context);
      try {
        await this.prologLSPClient.start();
        console.log('[Extension] Prolog LSP Client started successfully');
      } catch (error) {
        console.error('[Extension] Failed to start Prolog LSP Client:', error);
        vscode.window.showWarningMessage(
          'Prolog LSP Client failed to start. Some features may not be available.'
        );
      }
    }

    // Generate multi-IDE configurations
    const workspaceFolder = vscode.workspace.workspaceFolders?.[0];
    if (workspaceFolder) {
      try {
        await MultiIDESupport.generateIDEConfigurations(workspaceFolder.uri.fsPath);
        MultiIDESupport.generateLaunchConfigurations(workspaceFolder.uri.fsPath);

        // Detect available IDEs
        const availableIDEs = await MultiIDESupport.detectAvailableIDEs();
        if (availableIDEs.length > 1) {
          console.log('[Extension] Detected IDEs:', availableIDEs.join(', '));
        }
      } catch (error) {
        console.error('[Extension] Failed to generate multi-IDE configurations:', error);
      }
    }

    // Set up backend event handlers
    if (this.prologBackend) {
      this.prologBackend.on('ready', () => {
        try {
          console.log('[Extension] Prolog backend ready');
          vscode.window.showInformationMessage('Prolog backend started successfully');
        } catch (err) {
          console.error('[Extension] Event handler error (ready):', err);
        }
      });

      this.prologBackend.on('stopped', () => {
        try {
          console.log('[Extension] Prolog backend stopped');
          vscode.window.showWarningMessage('Prolog backend stopped');
        } catch (err) {
          console.error('[Extension] Event handler error (stopped):', err);
        }
      });

      this.prologBackend.on('restarted', () => {
        try {
          console.log('[Extension] Prolog backend restarted');
          vscode.window.showInformationMessage('Prolog backend restarted successfully');
        } catch (err) {
          console.error('[Extension] Event handler error (restarted):', err);
        }
      });

      this.prologBackend.on('error', error => {
        try {
          console.error('[Extension] Prolog backend error:', error);
          vscode.window.showErrorMessage(`Prolog backend error: ${error && error.stack ? error.stack : error}`);
        } catch (err) {
          console.error('[Extension] Event handler error (error):', err);
        }
      });

      this.prologBackend.on('backendStartupFailed', ({ error, troubleshooting }) => {
        try {
          console.error('[Extension] Prolog backend startup failed:', error);
          if (Array.isArray(troubleshooting)) {
            troubleshooting.forEach(msg => console.error('[Troubleshooting]', msg));
          }
          vscode.window.showErrorMessage(`Prolog backend startup failed: ${error}`);
        } catch (err) {
          console.error('[Extension] Event handler error (backendStartupFailed):', err);
        }
      });
    }
  }

  startBackend(): void {
    try {
      this.prologBackend?.start();
    } catch (error) {
      console.error('[Extension] Failed to start Prolog backend:', error);
      // Don't show error immediately - let user trigger it via chat if needed
    }
  }

  stopBackend(): void {
    if (this.apiServer) {
      try {
        this.apiServer.stop();
        console.log('[Extension] API server stopped');
      } catch (error) {
        console.error('[Extension] Error stopping API server:', error);
      }
      this.apiServer = null;
    }

    if (this.externalWebSocketManager) {
      try {
        this.externalWebSocketManager.stop();
        console.log('[Extension] External WebSocket manager stopped');
      } catch (error) {
        console.error('[Extension] Error stopping WebSocket manager:', error);
      }
      this.externalWebSocketManager = null;
    }

    if (this.prologLSPClient) {
      try {
        this.prologLSPClient.stop();
        console.log('[Extension] Prolog LSP Client stopped');
      } catch (error) {
        console.error('[Extension] Error stopping LSP Client:', error);
      }
      this.prologLSPClient = null;
    }

    if (this.prologBackend) {
      this.prologBackend.stop();
      this.prologBackend = null;
    }
  }
}
