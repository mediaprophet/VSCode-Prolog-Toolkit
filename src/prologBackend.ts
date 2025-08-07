// Interface for predicate help result
export interface PredicateHelpResult {
  summary?: string;
  name: string;
  arity: number;
  module?: string;
  args?: string[];
  examples?: string[];
  doc?: string;
  [key: string]: any;
}

/**
 * Retrieve documentation/help for a given predicate indicator (e.g., 'member/2').
 * Returns an object with summary, name, arity, module, args, examples, and doc if available.
 * Throws if the backend is not ready or the request fails.
 *
 * @param predicateIndicator - The predicate indicator string, e.g., 'member/2'.
 */

// Removed invalid import of 'node:globals'; Node.js types are available globally.

/// <reference types="node" />
import axios from 'axios';
import type { ChildProcessWithoutNullStreams } from 'child_process';
// Removed incorrect import; rely on global NodeJS types
import { spawn } from 'child_process';

import { NodeEventEmitter } from './shim/eventemitter-shim.js';
import type { BackendEventMap } from './types/backend.js';

import { v4 as uuidv4 } from 'uuid';
import type { QueryPriority, ResourceQuota } from './features/concurrencyManager.js';
import { ConcurrencyManager } from './features/concurrencyManager.js';
import type { QueryHistoryOptions } from './features/queryHistoryManager.js';
import { QueryHistoryManager } from './features/queryHistoryManager.js';
import type {
  QueryCallback,
  QueryNotificationOptions,
} from './features/queryNotificationManager.js';
import { QueryNotificationManager } from './features/queryNotificationManager.js';
import { QueryScheduler } from './features/queryScheduler.js';
import type { SessionManagerOptions } from './features/sessionManager.js';
import { SessionManager } from './features/sessionManager.js';
import type { UIHandler } from './features/uiHandler.js';
import { defaultUIHandler } from './features/uiHandler.js';
import { ExecutableFinder } from './utils/executableFinder.js';
import { PlatformUtils } from './utils/platformUtils.js';

export interface PrologBackendOptions {
  swiplPath?: string;
  args?: string[];
  cwd?: string;
  port?: number;
  maxResultsPerChunk?: number;
  streamingEnabled?: boolean;
  notificationOptions?: QueryNotificationOptions;
  concurrencyOptions?: {
    resourceQuota?: Partial<ResourceQuota>;
    enabled?: boolean;
  };
  historyOptions?: Partial<QueryHistoryOptions>;
  schedulerOptions?: {
    enabled?: boolean;
    maxScheduledQueries?: number;
    checkInterval?: number;
    enableRecurring?: boolean;
    enableConditional?: boolean;
    enableDependencies?: boolean;
  };
  sessionOptions?: Partial<SessionManagerOptions>;
  uiHandler?: UIHandler;
}

export class PrologBackend extends NodeEventEmitter<BackendEventMap> {
  /**
   * Retrieve documentation/help for a given predicate indicator (e.g., 'member/2').
   * Returns an object with summary, name, arity, module, args, examples, and doc if available.
   * Throws if the backend is not ready or the request fails.
   *
   * @param predicateIndicator - The predicate indicator string, e.g., 'member/2'.
   */
  public async getPredicateHelp(predicateIndicator: string): Promise<PredicateHelpResult> {
    if (!this.isReady) {
      throw new Error('Prolog backend not ready');
    }
    // Assume the backend supports a 'predicate_help' command
    const [name, arityStr] = predicateIndicator.split('/');
    const arity = parseInt(arityStr ?? '0', 10);
    const params = { name, arity };
    const result = await this.sendRequest('predicate_help', params);
    // The backend should return an object matching PredicateHelpResult
    return result;
  }
  /**
   * Retrieve documentation/help for a given predicate indicator (e.g., 'member/2').
   * Returns an object with summary, name, arity, module, args, examples, and doc if available.
   * Throws if the backend is not ready or the request fails.
   *
   * @param predicateIndicator - The predicate indicator string, e.g., 'member/2'.
   */
  private process: ChildProcessWithoutNullStreams | null = null;
  private options: PrologBackendOptions;
  private isReady: boolean = false;
  private pendingRequests: Map<
    string,
    { resolve: (v: any) => void; reject: (e: any) => void; timeout: ReturnType<typeof setTimeout> }
  > = new Map();
  private intentionalStop: boolean = false;
  private _suppressStoppedEvent: boolean = false;
  private port: number;
  private maxResultsPerChunk: number;
  private streamingEnabled: boolean;
  private notificationManager: QueryNotificationManager;
  private concurrencyManager: ConcurrencyManager;
  private historyManager: QueryHistoryManager;
  private queryScheduler: QueryScheduler;
  private sessionManager: SessionManager;
  private runningQueries: Map<string, { cancel: () => void }> = new Map();
  private uiHandler: UIHandler;

  // VS Code event emitters for backend events

  // Logging and diagnostics
  private log(msg: string) {
    // In production, use a proper logger or VS Code output channel
    console.log('[PrologBackend]', msg);
  }

  constructor(options: PrologBackendOptions = {}) {
    super();
    this.options = options;
    this.port = options.port || 3060;
    this.maxResultsPerChunk = options.maxResultsPerChunk || 50;
    this.streamingEnabled = options.streamingEnabled ?? true;
    this.uiHandler = options.uiHandler || defaultUIHandler;

    // Initialize notification manager
    this.notificationManager = new QueryNotificationManager({
      enableWebSocket: true,
      webSocketPort: (options.port || 3060) + 2, // Use port + 2 for WebSocket
      ...options.notificationOptions,
    });

    // Initialize concurrency manager
    this.concurrencyManager = new ConcurrencyManager(
      options.concurrencyOptions?.resourceQuota || {}
    );

    // Initialize history manager
    this.historyManager = new QueryHistoryManager({
      storageDir: PlatformUtils.joinPath(process.cwd(), '.prolog-history'),
      ...options.historyOptions,
    });

    // Initialize query scheduler
    this.queryScheduler = new QueryScheduler(
      this.concurrencyManager,
      this.historyManager,
      options.schedulerOptions
    );

    // Initialize session manager
    this.sessionManager = new SessionManager({
      storageDir: PlatformUtils.joinPath(process.cwd(), '.prolog-sessions'),
      ...options.sessionOptions,
    });

    // Set up integration between managers
    this.sessionManager.setIntegrationManagers(this.concurrencyManager, this.historyManager);

    // Set up event handlers
    this.setupEventHandlers();
  }

  /**
   * Set up event handlers for all managers
   */
  private setupEventHandlers(): void {
    // Notification manager events
    // Notification manager events
    this.notificationManager.onQueryCancelled(queryId => {
      this.handleQueryCancellation(queryId);
    });

    // Concurrency manager events
    this.concurrencyManager.onExecuteQuery(async event => {
      await this.handleConcurrencyManagerExecution(event);
    });

    this.concurrencyManager.onQueryCompleted(event => {
      // Forward as needed, or handle here
    });

    this.concurrencyManager.onResourceUsageUpdated(usage => {
      // Forward as needed, or handle here
    });

    // History manager events
    this.historyManager.onQueryAdded(entry => {
      // Forward as needed, or handle here
    });

    // Scheduler events
    this.queryScheduler.onQueryScheduleCompleted(query => {
      // Forward as needed, or handle here
    });

    this.queryScheduler.onQueryScheduleExecutionStarted(query => {
      // Forward as needed, or handle here
    });

    // Session manager events
    this.sessionManager.onSessionCreated(event => {
      // Forward as needed, or handle here
    });

    this.sessionManager.onSessionSwitched(event => {
      // Forward as needed, or handle here
    });

    this.sessionManager.onSessionDeleted(event => {
      // Forward as needed, or handle here
    });

    this.sessionManager.onSessionStateSaved(event => {
      // Forward as needed, or handle here
    });

    this.sessionManager.onSessionStateRestored(event => {
      // Forward as needed, or handle here
    });
  }

  /**
   * Handle query execution from concurrency manager
   */
  private async handleConcurrencyManagerExecution(event: any): Promise<void> {
    const { query, resolve, reject } = event;

    try {
      // Execute the actual query
      const result = await this.executeQueryDirect(query.cmd, query.params);
      resolve(result);
    } catch (error) {
      reject(error);
    }
  }

  isRunning(): boolean {
    return !!this.process;
  }

  stop(intentional = true) {
    this.log(
      '[DEBUG] stop() called. this.process=' +
        !!this.process +
        ', intentionalStop=' +
        this.intentionalStop +
        ', param=' +
        intentional
    );
    if (this.process) {
      this.intentionalStop = intentional;
      if (intentional) {
        this.log('[DEBUG] Setting intentionalStop=true and killing process');
      } else {
        this.log('[DEBUG] Killing process for restart (intentionalStop=false)');
      }
      this.process.kill();
      this.process = null;
      this.isReady = false;

      // Cancel all running queries
      this.runningQueries.forEach((query, queryId) => {
        this.notificationManager.cancelQuery(queryId);
      });
      this.runningQueries.clear();

      // Close all managers if intentional stop
      if (intentional) {
        this.notificationManager.close();
        this.concurrencyManager.dispose();
        this.historyManager.dispose();
        this.queryScheduler.dispose();
        this.sessionManager.dispose();
      }

      // Only emit 'stopped' if not in the middle of an automatic restart
      if (!this._suppressStoppedEvent) {
        this.log('[DEBUG] Emitting stopped event');
        this.emit('stopped');
      } else {
        this.log('[DEBUG] Suppressed stopped event');
      }
      this.log('Prolog backend stopped.');
    }
  }

  restart() {
    this.log('Restarting Prolog backend...');
    // Do not set intentionalStop=true for restarts
    this.stop(false);
    this.start();
  }

  /**
   * Handle query cancellation
   */
  private handleQueryCancellation(queryId: string): void {
    const query = this.runningQueries.get(queryId);
    if (query) {
      query.cancel();
      this.runningQueries.delete(queryId);
      this.log(`[DEBUG] Cancelled query ${queryId}`);
    }
  }

  async sendRequest(
    cmdOrBatch: string | Array<{ cmd: string; params?: Record<string, any>; timeoutMs?: number }>,
    params: Record<string, any> = {}
  ): Promise<any> {
    if (!this.isReady) {
      throw new Error('Prolog backend not ready');
    }
    // Single request
    if (typeof cmdOrBatch === 'string') {
      const cmd = cmdOrBatch;
      const id = uuidv4();
      const timeoutMsValue = typeof params.timeoutMs === 'number' ? params.timeoutMs : 10000;
      const paramsWithLimit = { ...params };
      if (typeof paramsWithLimit.time_limit === 'undefined') {
        paramsWithLimit.time_limit = Math.ceil(timeoutMsValue / 1000);
      }

      // Add streaming parameters if enabled
      if (this.streamingEnabled && !paramsWithLimit.disable_streaming) {
        paramsWithLimit.max_results_per_chunk =
          paramsWithLimit.max_results_per_chunk || this.maxResultsPerChunk;
        paramsWithLimit.streaming = true;
      }

      const { timeoutMs: _timeoutMs, ...paramsNoTimeout } = paramsWithLimit;
      const request = {
        id,
        cmd,
        ...paramsNoTimeout,
        protocol: 1,
      };
      const timeout = setTimeout(() => {
        throw new Error('Prolog request timeout');
      }, timeoutMsValue);
      try {
        const response = await axios.post(`http://localhost:${this.port}`, request);
        clearTimeout(timeout);
        return response.data;
      } catch (err) {
        clearTimeout(timeout);
        throw err;
      }
    }
    // Batch request
    const batch = cmdOrBatch.map(req => {
      const id = uuidv4();
      const paramsWithLimit = { ...(req.params || {}) };
      if (typeof req.timeoutMs === 'number' && typeof paramsWithLimit.time_limit === 'undefined') {
        paramsWithLimit.time_limit = Math.ceil(req.timeoutMs / 1000);
      }

      // Add streaming parameters if enabled
      if (this.streamingEnabled && !paramsWithLimit.disable_streaming) {
        paramsWithLimit.max_results_per_chunk =
          paramsWithLimit.max_results_per_chunk || this.maxResultsPerChunk;
        paramsWithLimit.streaming = true;
      }

      return {
        id,
        cmd: req.cmd,
        ...paramsWithLimit,
        protocol: 1,
        timeoutMs: req.timeoutMs,
      };
    });
    const responses = await Promise.all(
      batch.map(async req => {
        const timeout = setTimeout(() => {
          throw new Error('Prolog request timeout in batch');
        }, req.timeoutMs || 10000);
        try {
          const response = await axios.post(`http://localhost:${this.port}`, req);
          clearTimeout(timeout);
          return response.data;
        } catch (err) {
          clearTimeout(timeout);
          throw err;
        }
      })
    );
    return responses;
  }

  /**
   * Send request with notification support
   */
  async sendRequestWithNotifications(
    cmdOrBatch: string | Array<{ cmd: string; params?: Record<string, any>; timeoutMs?: number }>,
    params: Record<string, any> = {},
    callback?: QueryCallback
  ): Promise<any> {
    if (!this.isReady) {
      throw new Error('Prolog backend not ready');
    }

    // Single request with notifications
    if (typeof cmdOrBatch === 'string') {
      const cmd = cmdOrBatch;
      const queryId = uuidv4();
      const timeoutMs = typeof params.timeoutMs === 'number' ? params.timeoutMs : 10000;

      // Register query for tracking
      this.notificationManager.registerQuery(queryId, callback);
      this.notificationManager.updateQueryStatus(queryId, { status: 'running' });

      const paramsWithLimit = { ...params };
      if (typeof paramsWithLimit.time_limit === 'undefined') {
        paramsWithLimit.time_limit = Math.ceil(timeoutMs / 1000);
      }

      // Add streaming parameters if enabled
      if (this.streamingEnabled && !paramsWithLimit.disable_streaming) {
        paramsWithLimit.max_results_per_chunk =
          paramsWithLimit.max_results_per_chunk || this.maxResultsPerChunk;
        paramsWithLimit.streaming = true;
      }

      const { timeoutMs: _omit, ...paramsNoTimeout } = paramsWithLimit;
      const request = {
        id: queryId,
        cmd,
        ...paramsNoTimeout,
        protocol: 1,
      };

      // Set up cancellation support
      let cancelled = false;
      const cancelToken = {
        cancel: () => {
          cancelled = true;
        },
      };
      this.runningQueries.set(queryId, cancelToken);

      const timeout = setTimeout(() => {
        if (!cancelled) {
          this.notificationManager.updateQueryStatus(queryId, { status: 'timeout' });
          this.runningQueries.delete(queryId);
        }
      }, timeoutMs);

      try {
        // Simulate progress updates for long-running queries
        const progressInterval = setInterval(() => {
          if (!cancelled && this.runningQueries.has(queryId)) {
            const elapsed =
              Date.now() -
              (this.notificationManager.getQueryStatus(queryId)?.startTime || Date.now());
            const progress = Math.min(90, (elapsed / timeoutMs) * 100);
            this.notificationManager.updateQueryProgress(queryId, progress, 'Processing query...');
          } else {
            clearInterval(progressInterval);
          }
        }, 1000);

        const response = await axios.post(`http://localhost:${this.port}`, request);

        clearTimeout(timeout);
        clearInterval(progressInterval);
        this.runningQueries.delete(queryId);

        if (!cancelled) {
          this.notificationManager.completeQuery(queryId, response.data);
        }

        return response.data;
      } catch (err) {
        clearTimeout(timeout);
        this.runningQueries.delete(queryId);

        if (!cancelled) {
          this.notificationManager.failQuery(queryId, err);
        }
        throw err;
      }
    }

    // Batch requests with notifications
    const batch = cmdOrBatch.map((req, index) => {
      const queryId = uuidv4();

      // Register each batch item for tracking
      this.notificationManager.registerQuery(queryId, callback, true, index, cmdOrBatch.length);

      const paramsWithLimit = { ...(req.params || {}) };
      if (typeof req.timeoutMs === 'number' && typeof paramsWithLimit.time_limit === 'undefined') {
        paramsWithLimit.time_limit = Math.ceil(req.timeoutMs / 1000);
      }

      // Add streaming parameters if enabled
      if (this.streamingEnabled && !paramsWithLimit.disable_streaming) {
        paramsWithLimit.max_results_per_chunk =
          paramsWithLimit.max_results_per_chunk || this.maxResultsPerChunk;
        paramsWithLimit.streaming = true;
      }

      return {
        id: queryId,
        cmd: req.cmd,
        ...paramsWithLimit,
        protocol: 1,
        timeoutMs: req.timeoutMs,
      };
    });

    // Execute batch with individual tracking
    const responses = await Promise.all(
      batch.map(async (req, index) => {
        const queryId = req.id;
        this.notificationManager.updateQueryStatus(queryId, { status: 'running' });

        // Set up cancellation support
        let cancelled = false;
        const cancelToken = {
          cancel: () => {
            cancelled = true;
          },
        };
        this.runningQueries.set(queryId, cancelToken);

        const timeout = setTimeout(() => {
          if (!cancelled) {
            this.notificationManager.updateQueryStatus(queryId, { status: 'timeout' });
            this.runningQueries.delete(queryId);
          }
        }, req.timeoutMs || 10000);

        try {
          // Progress simulation for batch items
          const progressInterval = setInterval(() => {
            if (!cancelled && this.runningQueries.has(queryId)) {
              const elapsed =
                Date.now() -
                (this.notificationManager.getQueryStatus(queryId)?.startTime || Date.now());
              const progress = Math.min(90, (elapsed / (req.timeoutMs || 10000)) * 100);
              this.notificationManager.updateQueryProgress(
                queryId,
                progress,
                `Processing batch item ${index + 1}/${batch.length}...`
              );
            } else {
              clearInterval(progressInterval);
            }
          }, 1000);

          const { timeoutMs: _omit, ...reqToSend } = req;
          const response = await axios.post(`http://localhost:${this.port}`, reqToSend);

          clearTimeout(timeout);
          clearInterval(progressInterval);
          this.runningQueries.delete(queryId);

          if (!cancelled) {
            this.notificationManager.completeQuery(queryId, response.data);
          }

          return response.data;
        } catch (err) {
          clearTimeout(timeout);
          this.runningQueries.delete(queryId);

          if (!cancelled) {
            this.notificationManager.failQuery(queryId, err);
          }
          throw err;
        }
      })
    );

    return responses;
  }

  /**
   * Send a streaming request that can handle large result sets
   */
  async sendStreamingRequest(
    cmd: string,
    params: Record<string, any> = {},
    onChunk?: (chunk: any, isFirst: boolean, isLast: boolean) => void
  ): Promise<any> {
    if (!this.isReady) {
      throw new Error('Prolog backend not ready');
    }

    const streamingParams = {
      ...params,
      streaming: true,
      max_results_per_chunk: params.max_results_per_chunk || this.maxResultsPerChunk,
    };

    // For now, we'll simulate streaming by chunking the response
    // In a full implementation, this would use WebSockets or Server-Sent Events
    const response = await this.sendRequest(cmd, streamingParams);

    if (response.status === 'ok' && response.results && Array.isArray(response.results)) {
      const results = response.results;
      const chunkSize = this.maxResultsPerChunk;

      if (results.length <= chunkSize) {
        // Small result set, return as single chunk
        if (onChunk) {
          onChunk(response, true, true);
        }
        return response;
      }

      // Large result set, chunk it
      const chunks = [];
      for (let i = 0; i < results.length; i += chunkSize) {
        const chunk = results.slice(i, i + chunkSize);
        const isFirst = i === 0;
        const isLast = i + chunkSize >= results.length;

        const chunkResponse = {
          ...response,
          results: chunk,
          chunk_info: {
            chunk_index: Math.floor(i / chunkSize),
            chunk_size: chunk.length,
            total_results: results.length,
            is_first: isFirst,
            is_last: isLast,
          },
        };

        chunks.push(chunkResponse);

        if (onChunk) {
          onChunk(chunkResponse, isFirst, isLast);
        }
      }

      // Return summary response
      return {
        ...response,
        results: results.slice(0, chunkSize), // First chunk
        total_chunks: chunks.length,
        streaming: true,
        chunk_info: {
          chunk_index: 0,
          chunk_size: Math.min(chunkSize, results.length),
          total_results: results.length,
          is_first: true,
          is_last: chunks.length === 1,
        },
      };
    }

    return response;
  }

  /**
   * Get streaming configuration
   */
  getStreamingConfig() {
    return {
      enabled: this.streamingEnabled,
      maxResultsPerChunk: this.maxResultsPerChunk,
    };
  }

  /**
   * Update streaming configuration
   */
  updateStreamingConfig(config: { enabled?: boolean; maxResultsPerChunk?: number }) {
    if (config.enabled !== undefined) {
      this.streamingEnabled = config.enabled;
    }
    if (config.maxResultsPerChunk !== undefined) {
      this.maxResultsPerChunk = config.maxResultsPerChunk;
    }
  }

  /**
   * Get notification manager for direct access
   */
  getNotificationManager(): QueryNotificationManager {
    return this.notificationManager;
  }

  /**
   * Cancel a running query
   */
  cancelQuery(queryId: string): boolean {
    return this.notificationManager.cancelQuery(queryId);
  }

  /**
   * Get query status
   */
  getQueryStatus(queryId: string) {
    return this.notificationManager.getQueryStatus(queryId);
  }

  /**
   * Get all active queries
   */
  getActiveQueries() {
    return this.notificationManager.getActiveQueries();
  }

  /**
   * Get query statistics
   */
  getQueryStatistics() {
    return this.notificationManager.getStatistics();
  }

  /**
   * Execute a query directly (used internally by concurrency manager)
   */
  private async executeQueryDirect(cmd: string, params: Record<string, any> = {}): Promise<any> {
    const id = uuidv4();
    const timeoutMs = typeof params.timeoutMs === 'number' ? params.timeoutMs : 10000;
    const paramsWithLimit = { ...params };
    if (typeof paramsWithLimit.time_limit === 'undefined') {
      paramsWithLimit.time_limit = Math.ceil(timeoutMs / 1000);
    }

    // Add streaming parameters if enabled
    if (this.streamingEnabled && !paramsWithLimit.disable_streaming) {
      paramsWithLimit.max_results_per_chunk =
        paramsWithLimit.max_results_per_chunk || this.maxResultsPerChunk;
      paramsWithLimit.streaming = true;
    }

    const { timeoutMs: _omit, ...paramsNoTimeout } = paramsWithLimit;
    const request = {
      id,
      cmd,
      ...paramsNoTimeout,
      protocol: 1,
    };

    const timeout = setTimeout(() => {
      throw new Error('Prolog request timeout');
    }, timeoutMs);

    try {
      const response = await axios.post(`http://localhost:${this.port}`, request);
      clearTimeout(timeout);
      return response.data;
    } catch (err) {
      clearTimeout(timeout);
      throw err;
    }
  }

  /**
   * Send request with advanced concurrency control
   */
  async sendRequestWithConcurrency(
    cmd: string,
    params: Record<string, any> = {},
    priority: Partial<QueryPriority> = {},
    resourceRequirements?: { memoryMB?: number; cpuPercent?: number }
  ): Promise<any> {
    if (!this.isReady) {
      throw new Error('Prolog backend not ready');
    }

    const queryId = uuidv4();
    return await this.concurrencyManager.queueQuery(
      queryId,
      cmd,
      params,
      priority,
      resourceRequirements
    );
  }

  /**
   * Schedule a query for future execution
   */
  async scheduleQuery(
    cmd: string,
    params: Record<string, any> = {},
    scheduleType: 'immediate' | 'delayed' | 'recurring' | 'conditional' = 'immediate',
    scheduleConfig: any = {},
    priority: Partial<QueryPriority> = {},
    metadata?: any
  ): Promise<string> {
    if (!this.isReady) {
      throw new Error('Prolog backend not ready');
    }

    const queryId = uuidv4();

    await this.queryScheduler.scheduleQuery(
      queryId,
      cmd,
      params,
      scheduleType,
      scheduleConfig,
      priority,
      metadata
    );

    return queryId;
  }

  /**
   * Cancel a scheduled query
   */
  async cancelScheduledQuery(queryId: string): Promise<boolean> {
    return await this.queryScheduler.cancelScheduledQuery(queryId);
  }

  /**
   * Get scheduled queries
   */
  getScheduledQueries(filter?: any): any[] {
    return this.queryScheduler.getScheduledQueries(filter);
  }

  /**
   * Get query history
   */
  async getQueryHistory(filter: any = {}): Promise<any> {
    return await this.historyManager.getHistory(filter);
  }

  /**
   * Get query history statistics
   */
  async getQueryHistoryStatistics(): Promise<any> {
    return await this.historyManager.getStatistics();
  }

  /**
   * Get concurrency manager status
   */
  getConcurrencyStatus(): any {
    return this.concurrencyManager.getStatus();
  }

  /**
   * Get scheduler statistics
   */
  getSchedulerStatistics(): any {
    return this.queryScheduler.getStatistics();
  }

  /**
   * Update resource quota
   */
  updateResourceQuota(newQuota: Partial<ResourceQuota>): void {
    this.concurrencyManager.updateResourceQuota(newQuota);
  }

  /**
   * Pause a recurring query
   */
  async pauseRecurringQuery(queryId: string): Promise<boolean> {
    return await this.queryScheduler.pauseRecurringQuery(queryId);
  }

  /**
   * Resume a paused recurring query
   */
  async resumeRecurringQuery(queryId: string): Promise<boolean> {
    return await this.queryScheduler.resumeRecurringQuery(queryId);
  }

  /**
   * Register a custom condition evaluator for conditional queries
   */
  registerConditionEvaluator(queryId: string, evaluator: () => boolean): void {
    this.queryScheduler.registerConditionEvaluator(queryId, evaluator);
  }

  /**
   * Clear query history
   */
  async clearQueryHistory(): Promise<void> {
    await this.historyManager.clearHistory();
  }

  /**
   * Get a specific query from history
   */
  async getQueryFromHistory(queryId: string): Promise<any> {
    return await this.historyManager.getQuery(queryId);
  }

  /**
   * Delete a query from history
   */
  async deleteQueryFromHistory(queryId: string): Promise<boolean> {
    return await this.historyManager.deleteQuery(queryId);
  }

  // Session Management API

  /**
   * Create a new session
   */
  async createSession(
    name: string,
    options: {
      description?: string;
      userId?: string;
      agentId?: string;
      resourceQuota?: Partial<ResourceQuota>;
      persistenceEnabled?: boolean;
      autoSave?: boolean;
      metadata?: Record<string, any>;
    } = {}
  ): Promise<string> {
    const sessionId = await this.sessionManager.createSession(name, options);

    // Also create session in Prolog backend
    try {
      await this.sendRequest('session_create', { name });
    } catch (error) {
      console.warn('[PrologBackend] Failed to create session in Prolog backend:', error);
    }

    return sessionId;
  }

  /**
   * Switch to a different session
   */
  async switchToSession(sessionId: string): Promise<void> {
    await this.sessionManager.switchToSession(sessionId);

    // Also switch session in Prolog backend
    try {
      await this.sendRequest('session_switch', { session_id: sessionId });
    } catch (error) {
      console.warn('[PrologBackend] Failed to switch session in Prolog backend:', error);
    }
  }

  /**
   * Get current active session
   */
  getCurrentSession(): { sessionId: string; config: any; state: any } | null {
    return this.sessionManager.getCurrentSession();
  }

  /**
   * Get session by ID
   */
  getSession(sessionId: string): { config: any; state: any } | null {
    return this.sessionManager.getSession(sessionId);
  }

  /**
   * List all sessions
   */
  listSessions(filter?: {
    userId?: string;
    agentId?: string;
    isActive?: boolean;
    includeInactive?: boolean;
  }): Array<{ sessionId: string; config: any }> {
    return this.sessionManager.listSessions(filter);
  }

  /**
   * Delete a session
   */
  async deleteSession(sessionId: string): Promise<boolean> {
    const result = await this.sessionManager.deleteSession(sessionId);

    // Also delete session in Prolog backend
    if (result) {
      try {
        await this.sendRequest('session_delete', { session_id: sessionId });
      } catch (error) {
        console.warn('[PrologBackend] Failed to delete session in Prolog backend:', error);
      }
    }

    return result;
  }

  /**
   * Save current session state
   */
  async saveCurrentSessionState(): Promise<void> {
    await this.sessionManager.saveCurrentSessionState();

    // Also save state in Prolog backend
    try {
      await this.sendRequest('session_save_state', {});
    } catch (error) {
      console.warn('[PrologBackend] Failed to save session state in Prolog backend:', error);
    }
  }

  /**
   * Save session state
   */
  async saveSessionState(sessionId: string, state?: any): Promise<void> {
    await this.sessionManager.saveSessionState(sessionId, state);

    // Also save state in Prolog backend
    try {
      await this.sendRequest('session_save_state', { session_id: sessionId });
    } catch (error) {
      console.warn('[PrologBackend] Failed to save session state in Prolog backend:', error);
    }
  }

  /**
   * Restore session state
   */
  async restoreSessionState(sessionId: string, snapshotId?: string): Promise<void> {
    await this.sessionManager.restoreSessionState(sessionId, snapshotId);

    // Also restore state in Prolog backend
    try {
      await this.sendRequest('session_restore_state', { session_id: sessionId });
    } catch (error) {
      console.warn('[PrologBackend] Failed to restore session state in Prolog backend:', error);
    }
  }

  /**
   * Create a snapshot of session state
   */
  async createSessionSnapshot(
    sessionId: string,
    name: string,
    description?: string
  ): Promise<string> {
    return await this.sessionManager.createSnapshot(sessionId, name, description);
  }

  /**
   * Get session-specific concurrency manager
   */
  getSessionConcurrencyManager(sessionId: string): ConcurrencyManager | undefined {
    return this.sessionManager.getSessionConcurrencyManager(sessionId);
  }

  /**
   * Get session-specific history manager
   */
  getSessionHistoryManager(sessionId: string): QueryHistoryManager | undefined {
    return this.sessionManager.getSessionHistoryManager(sessionId);
  }

  /**
   * Update session resource quota
   */
  async updateSessionResourceQuota(
    sessionId: string,
    quota: Partial<ResourceQuota>
  ): Promise<void> {
    await this.sessionManager.updateSessionResourceQuota(sessionId, quota);
  }

  /**
   * Get session statistics
   */
  async getSessionStatistics(sessionId: string): Promise<any> {
    return this.sessionManager.getSessionStatistics(sessionId);
  }

  /**
   * Export session state to file
   */
  async exportSessionState(sessionId: string, filePath: string): Promise<void> {
    try {
      await this.sendRequest('session_export', {
        session_id: sessionId,
        file_path: filePath,
      });
    } catch (error) {
      console.error('[PrologBackend] Failed to export session state:', error);
      throw error;
    }
  }

  /**
   * Import session state from file
   */
  async importSessionState(sessionId: string, filePath: string): Promise<void> {
    try {
      await this.sendRequest('session_import', {
        session_id: sessionId,
        file_path: filePath,
      });
      // Refresh session state in TypeScript side
      await this.sessionManager.restoreSessionState(sessionId);
    } catch (error) {
      console.error('[PrologBackend] Failed to import session state:', error);
      throw error;
    }
  }

  /**
   * Get session manager for direct access
   */
  getSessionManager(): SessionManager {
    return this.sessionManager;
  }

  private handleExit(code: number | null, signal: string | null) {
    this.log(
      `[DEBUG] handleExit() called. code=${code}, signal=${signal}, intentionalStop=${this.intentionalStop}`
    );
    this.isReady = false;
    this.emit('exit', code, signal);
    this.log(`Prolog backend exited with code ${code}, signal ${signal}`);
    // Automatic restart logic
    if (!this.intentionalStop) {
      this.log('[DEBUG] Scheduling automatic restart in 1s');
      setTimeout(() => {
        this.log('[DEBUG] Automatic restart: suppressing stopped event and listening for started');
        this._suppressStoppedEvent = true;
        const startedListener = () => {
          this.log('[DEBUG] onStarted (auto-restart): emitting restarted');
          this.off('started', startedListener);
          this._suppressStoppedEvent = false;
          this.emit('restarted');
        };
        this.on('started', startedListener);
        this.start();
      }, 1000); // restart after 1s
    } else {
      this.log('[DEBUG] Not auto-restarting because intentionalStop=true');
    }
    this.intentionalStop = false;
  }

  async start() {
    this.log('[DEBUG] start() called. this.process=' + !!this.process);
    if (this.process) {
      this.log('Prolog backend already running.');
      return;
    }

    // Enhanced executable resolution with permission checking
    let swiplPath = this.options.swiplPath || PlatformUtils.getDefaultExecutablePath();
    if (swiplPath && swiplPath !== PlatformUtils.getDefaultExecutablePath()) {
      const normalizedPath = PlatformUtils.normalizePath(swiplPath);
      if (await PlatformUtils.pathExists(normalizedPath)) {
        if (await PlatformUtils.isExecutable(normalizedPath)) {
          swiplPath = normalizedPath;
        } else {
          const executableFinder = new ExecutableFinder();
          const detectionResult = await executableFinder.findSwiplExecutable();
          if (detectionResult.found && detectionResult.path) {
            swiplPath = detectionResult.path;
          }
        }
      } else {
        const executableFinder = new ExecutableFinder();
        const detectionResult = await executableFinder.findSwiplExecutable();
        if (detectionResult.found && detectionResult.path) {
          swiplPath = detectionResult.path;
        }
      }
    } else {
      const executableFinder = new ExecutableFinder();
      const detectionResult = await executableFinder.findSwiplExecutable();
      if (detectionResult.found && detectionResult.path) {
        swiplPath = detectionResult.path;
      } else {
        swiplPath = PlatformUtils.normalizePath(swiplPath);
      }
    }

    const serverPath = PlatformUtils.resolvePath(__dirname, 'prolog_json_server.pl');
    // Use proper escaping for cross-platform paths in Prolog
    const prologPath = serverPath.replace(/\\/g, '/').replace(/'/g, "''");
    const args = this.options.args || [
      '-q',
      '-f',
      'none',
      '-g',
      `consult('${prologPath}'), main(${this.port})`,
    ];
    const cwd = PlatformUtils.normalizePath(this.options.cwd || process.cwd());

    this.log('Spawning Prolog with:');
    this.log('  swiplPath: ' + swiplPath);
    this.log('  serverPath: ' + serverPath);
    this.log('  args: ' + JSON.stringify(args));
    this.process = spawn(swiplPath, args, {
      cwd,
      detached: false,
      stdio: ['pipe', 'pipe', 'pipe'],
    });
    this.isReady = false;
    this.intentionalStop = false;

    if (this.process) {
      this.process.on('exit', (code, signal) => this.handleExit(code, signal));

      // Capture stderr for debugging
      this.process.stderr?.on('data', data => {
        this.log(`[STDERR] ${data.toString()}`);
      });

      // Capture stdout for debugging
      this.process.stdout?.on('data', data => {
        this.log(`[STDOUT] ${data.toString()}`);
      });

      this.process.once('spawn', () => {
        // Wait for server to start - increased delay for HTTP server initialization
        setTimeout(() => {
          this.sendRequest('version')
            .then(output => {
              this.isReady = true;
              this.emit('ready');
              this.emit('started');
              this.log('Prolog backend started. Version: ' + output.version);
            })
            .catch(async err => {
              this.log('Prolog handshake/version check failed: ' + err.message);

              // Show enhanced error message for backend startup failures
              if (err.code === 'ENOENT' || err.message?.includes('not found')) {
                const action = await this.uiHandler.showErrorMessage(
                  'SWI-Prolog backend failed to start. The Prolog backend requires SWI-Prolog to provide language features.',
                  'Install SWI-Prolog',
                  'Setup Wizard',
                  'Configure Path',
                  'Dismiss'
                );

                // Removed unused variable 'installationGuide' to resolve lint warning
                switch (action) {
                  case 'Install SWI-Prolog': {
                    // In LSP context, we can't show the installation guide dialog
                    // This would need to be handled by the extension
                    console.log('SWI-Prolog installation required');
                    break;
                  }
                  case 'Setup Wizard': {
                    await this.uiHandler.executeCommand('prolog.setupWizard');
                    break;
                  }
                  case 'Configure Path': {
                    await this.uiHandler.executeCommand(
                      'workbench.action.openSettings',
                      'prolog.executablePath'
                    );
                    break;
                  }
                }
              }

              this.stop();
            });
        }, 2000); // Give more time for HTTP server to start
      });
    }
  }
}
