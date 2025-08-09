import { EventEmitter } from 'events';
import { PrologConcurrencyManager } from './concurrencyManager';
import { PrologHistoryManager } from './historyManager';
import { PrologNotificationManager } from './notificationManager';
import { PrologProcessManager } from './processManager';
import { PrologRequestManager } from './requestManager';
import { PrologSessionManager } from './sessionManager';

// Unified options for all sub-managers (all properties optional, flattened)
export interface PrologBackendOptions {
  swiplPath?: string;
  args?: string[];
  cwd?: string;
  port?: number;
  maxResultsPerChunk?: number;
  streamingEnabled?: boolean;
  onReady?: () => void;
  onExit?: (code: number | null, signal: NodeJS.Signals | null) => void;
  logger?: (msg: string) => void;
  enableWebSocket?: boolean;
  webSocketPort?: number;
  resourceQuota?: any;
  // ...add any other sub-manager options as needed
}

export class PrologBackend extends EventEmitter {
  public readonly processManager: PrologProcessManager;
  public readonly requestManager: PrologRequestManager;
  public readonly notificationManager: PrologNotificationManager;
  public readonly concurrencyManager: PrologConcurrencyManager;
  public readonly sessionManager: PrologSessionManager;
  public readonly historyManager: PrologHistoryManager;

  constructor(options: PrologBackendOptions) {
    super();
    this.processManager = new PrologProcessManager(options);
    const reqMgrOpts: any = {
      port: options.port ?? 3060,
      maxResultsPerChunk: options.maxResultsPerChunk ?? 50,
      streamingEnabled: options.streamingEnabled ?? true
    };
    if (options.logger) reqMgrOpts.logger = options.logger;
    this.requestManager = new PrologRequestManager(reqMgrOpts);
    this.notificationManager = new PrologNotificationManager(options);
    this.concurrencyManager = new PrologConcurrencyManager(options);
    this.sessionManager = new PrologSessionManager(options);
    this.historyManager = new PrologHistoryManager(options);
    // Optionally, wire up events between managers and this orchestrator
  }

  // Example API surface delegating to sub-managers
  start() {
    this.processManager.start();
  }
  stop() {
    this.processManager.stop();
  }
  restart() {
    this.processManager.restart();
  }
  isRunning() {
    return this.processManager.isRunning();
  }
  sendRequest(cmd: string, params: Record<string, any> = {}, timeoutMs = 10000) {
    return this.requestManager.sendRequest(cmd, params, timeoutMs);
  }

  // --- Added API methods for modular orchestration ---

  /**
   * Send a request with notification callbacks (progress, complete, error)
   */
  async sendRequestWithNotifications(cmd: string, params: Record<string, any> = {}, callbacks: any = {}) {
    // For now, just call sendRequest and invoke callbacks synchronously as a stub
    try {
      if (callbacks.onProgress) callbacks.onProgress({ progress: 0 });
      const result = await this.sendRequest(cmd, params);
      if (callbacks.onProgress) callbacks.onProgress({ progress: 100 });
      if (callbacks.onComplete) callbacks.onComplete({ result });
      return result;
    } catch (error) {
      if (callbacks.onError) callbacks.onError({ error });
      throw error;
    }
  }

  // --- Session Management ---
  listSessions(options?: any) {
    if (typeof this.sessionManager.listSessions === 'function') {
      return this.sessionManager.listSessions(options);
    }
    return [];
  }
  async createSession(name: string, options?: any) {
    if (typeof this.sessionManager.createSession === 'function') {
      return this.sessionManager.createSession(name, options);
    }
    return null;
  }
  getSession(sessionId: string) {
    if (typeof this.sessionManager.getSession === 'function') {
      return this.sessionManager.getSession(sessionId);
    }
    return null;
  }
  async getSessionStatistics(sessionId: string) {
    if (typeof this.sessionManager.getSessionStatistics === 'function') {
      return this.sessionManager.getSessionStatistics(sessionId);
    }
    return {};
  }
  async deleteSession(sessionId: string) {
    if (typeof this.sessionManager.deleteSession === 'function') {
      return this.sessionManager.deleteSession(sessionId);
    }
    return false;
  }
  async saveSessionState(sessionId: string, state: any) {
    if (typeof this.sessionManager.saveSessionState === 'function') {
      return this.sessionManager.saveSessionState(sessionId, state);
    }
    return false;
  }

  // --- Query History ---
  async getQueryHistory(filter?: any) {
    if (typeof this.historyManager.getQueryHistory === 'function') {
      return this.historyManager.getQueryHistory(filter);
    }
    return [];
  }

  // --- Concurrency/Resource Management ---
  getConcurrencyStatus() {
    if (typeof this.concurrencyManager.getConcurrencyStatus === 'function') {
      return this.concurrencyManager.getConcurrencyStatus();
    }
    return {};
  }
  getQueryStatistics() {
    if (typeof this.concurrencyManager.getQueryStatistics === 'function') {
      return this.concurrencyManager.getQueryStatistics();
    }
    return {};
  }
  getSchedulerStatistics() {
    if (typeof this.concurrencyManager.getSchedulerStatistics === 'function') {
      return this.concurrencyManager.getSchedulerStatistics();
    }
    return {};
  }
}
