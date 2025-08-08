// Main orchestrator for modular Prolog backend
import { PrologConcurrencyManager } from './concurrencyManager';
import { PrologHistoryManager } from './historyManager';
import { PrologNotificationManager } from './notificationManager';
import { PrologProcessManager } from './processManager';
import { PrologRequestManager } from './requestManager';
import { PrologSessionManager } from './sessionManager';

export interface ModularPrologBackendOptions {
  swiplPath?: string;
  args?: string[];
  cwd?: string;
  port?: number;
  maxResultsPerChunk?: number;
  streamingEnabled?: boolean;
  // ...other config fields...
}

export class ModularPrologBackend {
  public readonly processManager: PrologProcessManager;
  public readonly requestManager: PrologRequestManager;
  public readonly notificationManager: PrologNotificationManager;
  public readonly concurrencyManager: PrologConcurrencyManager;
  public readonly sessionManager: PrologSessionManager;
  public readonly historyManager: PrologHistoryManager;

  constructor(options: ModularPrologBackendOptions) {
    this.processManager = new PrologProcessManager(options);
    this.requestManager = new PrologRequestManager({
      port: options.port || 3060,
      maxResultsPerChunk: options.maxResultsPerChunk || 50,
      streamingEnabled: options.streamingEnabled ?? true,
    });
    this.notificationManager = new PrologNotificationManager({
      enableWebSocket: true,
      webSocketPort: (options.port || 3060) + 2,
    });
    this.concurrencyManager = new PrologConcurrencyManager({});
    this.sessionManager = new PrologSessionManager({});
    this.historyManager = new PrologHistoryManager({});
  }
  // ...compose and expose API...
}
