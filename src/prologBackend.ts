import axios from 'axios';
import { ChildProcessWithoutNullStreams, spawn } from 'child_process';
import { EventEmitter } from 'events';
import { v4 as uuidv4 } from 'uuid';

import { ConcurrencyManagerOrchestrator } from './features/concurrencyManager/ConcurrencyManagerOrchestrator';
import { QueryHistoryOrchestrator } from './features/queryHistoryManager/QueryHistoryOrchestrator';
import { QueryCallback, QueryNotificationManager, QueryNotificationOptions } from './features/queryNotificationManager';
import { QueryScheduler } from './features/queryScheduler';
import { SessionManagerOrchestrator } from './features/sessionManager/SessionManagerOrchestrator';
import { UIHandler, defaultUIHandler } from './features/uiHandler';
import { QueryHistoryOptions, QueryPriority, ResourceQuota, SessionManagerOptions } from './types/backend';
import { ExecutableFinder } from './utils/executableFinder';
import { PlatformUtils } from './utils/platformUtils';

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

export class PrologBackend extends EventEmitter {
  private process: ChildProcessWithoutNullStreams | null = null;
  private options: PrologBackendOptions;
  private isReady: boolean = false;
  private pendingRequests: Map<
    string,
    { resolve: (v: any) => void; reject: (e: any) => void; timeout: NodeJS.Timeout }
  > = new Map();
  private intentionalStop: boolean = false;
  private _suppressStoppedEvent: boolean = false;
  private port: number;
  private maxResultsPerChunk: number;
  private streamingEnabled: boolean;
  private notificationManager: QueryNotificationManager;
  private concurrencyManager: ConcurrencyManagerOrchestrator;
  private historyManager: QueryHistoryOrchestrator;
  private queryScheduler: QueryScheduler;
  private sessionManager: SessionManagerOrchestrator;
  private runningQueries: Map<string, { cancel: () => void }> = new Map();
  private uiHandler: UIHandler;

  // Logging and diagnostics
  private logFilePath: string = '';
  private log(msg: string) {
    const now = new Date();
    const timestamp = now.toISOString();
    const logLine = `[${timestamp}] [PrologBackend] ${msg}\n`;
    // Write to console
    console.log('[PrologBackend]', msg);
    // Write to log file
    if (!this.logFilePath) {
      const logDir = PlatformUtils.joinPath(process.cwd(), 'logs');
      if (!require('fs').existsSync(logDir)) {
        require('fs').mkdirSync(logDir, { recursive: true });
      }
      const logFile = `prolog-backend-${now.getFullYear()}${String(now.getMonth() + 1).padStart(2, '0')}${String(now.getDate()).padStart(2, '0')}_${String(now.getHours()).padStart(2, '0')}${String(now.getMinutes()).padStart(2, '0')}${String(now.getSeconds()).padStart(2, '0')}.log`;
      this.logFilePath = PlatformUtils.joinPath(logDir, logFile);
    }
    require('fs').appendFileSync(this.logFilePath, logLine);
  }

  public getLogFilePath(): string {
    return this.logFilePath;
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
    this.concurrencyManager = new ConcurrencyManagerOrchestrator(
      options.concurrencyOptions?.resourceQuota || {}
    );

    // Initialize history manager
    this.historyManager = new QueryHistoryOrchestrator({
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
    this.sessionManager = new SessionManagerOrchestrator({
      storageDir: PlatformUtils.joinPath(process.cwd(), '.prolog-sessions'),
      ...options.sessionOptions,
    });

    // Set up integration between managers
    // TODO: setIntegrationManagers is not implemented on SessionManagerOrchestrator. Integration logic may need to be refactored or handled differently.
    // this.sessionManager.setIntegrationManagers(this.concurrencyManager, this.historyManager);

    // Set up event handlers
    this.setupEventHandlers();
  }

  /**
   * Set up event handlers for all managers
   */
  private setupEventHandlers(): void {
    // Notification manager events
    this.notificationManager.on('queryCancelled', (queryId: string) => {
      this.handleQueryCancellation(queryId);
    });

    // Concurrency manager events
    this.concurrencyManager.on('executeQuery', async event => {
      await this.handleConcurrencyManagerExecution(event);
    });

    this.concurrencyManager.on('queryCompleted', event => {
      this.emit('queryCompleted', event);
    });

    this.concurrencyManager.on('resourceUsageUpdated', usage => {
      this.emit('resourceUsageUpdated', usage);
    });

    // History manager events
    this.historyManager.on('queryAdded', entry => {
      this.emit('queryHistoryAdded', entry);
    });

    // Scheduler events
    this.queryScheduler.on('queryScheduleCompleted', query => {
      this.emit('queryScheduleCompleted', query);
    });

    this.queryScheduler.on('queryScheduleExecutionStarted', query => {
      this.emit('queryScheduleExecutionStarted', query);
    });

    // Session manager events
    this.sessionManager.on('sessionCreated', event => {
      this.emit('sessionCreated', event);
    });

    this.sessionManager.on('sessionSwitched', event => {
      this.emit('sessionSwitched', event);
    });

    this.sessionManager.on('sessionDeleted', event => {
      this.emit('sessionDeleted', event);
    });

    this.sessionManager.on('sessionStateSaved', event => {
      this.emit('sessionStateSaved', event);
    });

    this.sessionManager.on('sessionStateRestored', event => {
      this.emit('sessionStateRestored', event);
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
        // TODO: Implement dispose methods on orchestrators if needed. For now, just nullify references.
        // this.concurrencyManager.dispose();
        // this.historyManager.dispose();
        // this.queryScheduler.dispose();
        // this.sessionManager.dispose();
        this.concurrencyManager = null as any;
        this.historyManager = null as any;
        this.queryScheduler = null as any;
        this.sessionManager = null as any;
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
    const batchToSend = batch.map(({ timeoutMs: _omit, ...toSend }) => toSend);
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
  /**
   * Execute a query and record source, resource usage, and artifacts in history metadata.
   * @param cmd Prolog command
   * @param params Query parameters
   * @param options { source?: 'ui'|'api'|'automation', artifacts?: Array<{name:string,path:string,type?:string}> }
   */
  private async executeQueryDirect(cmd: string, params: Record<string, any> = {}, options: { source?: string, artifacts?: any[] } = {}): Promise<any> {
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

    // --- Resource usage tracking ---
    const start = Date.now();
    let cpuStart = process.cpuUsage();
    let memStart = process.memoryUsage().rss;
    let result, error;
    const timeout = setTimeout(() => {
      throw new Error('Prolog request timeout');
    }, timeoutMs);
    try {
      const response = await axios.post(`http://localhost:${this.port}`, request);
      result = response.data;
      clearTimeout(timeout);
    } catch (err) {
      error = err;
      clearTimeout(timeout);
    }
    const end = Date.now();
    let cpuEnd = process.cpuUsage();
    let memEnd = process.memoryUsage().rss;
    // Calculate resource usage
    const cpuPercent = ((cpuEnd.user - cpuStart.user) + (cpuEnd.system - cpuStart.system)) / ((end - start) * 10) || undefined;
    const memoryMB = (memEnd - memStart) / (1024 * 1024);
    const durationMs = end - start;
    // Build metadata
    const metadata: any = {
      source: options.source || 'ui',
      resourceUsage: {
        cpuPercent,
        memoryMB,
        durationMs
      },
      artifacts: options.artifacts || []
    };
    // Save to history
    await this.historyManager.addQuery({
      id: params.queryId || cmd + '-' + start,
      cmd,
      params,
      status: error ? 'error' : 'completed',
      startTime: start,
      endTime: end,
      duration: durationMs,
      metadata
    });
    if (error) throw error;
    return result;
  }

  /**
   * Execute a query and record source, resource usage, and artifacts in history metadata.
   * @param cmd Prolog command
   * @param params Query parameters
   * @param options { source?: 'ui'|'api'|'automation', artifacts?: Array<{name:string,path:string,type?:string}> }
   */

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

    // Queue the query with concurrency control
    return await this.concurrencyManager.queueQuery({
      id: queryId,
      cmd,
      params,
      priority,
      resourceRequirements
    });
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
    // TODO: clearHistory is not implemented on QueryHistoryOrchestrator. Implement if needed.
    // await this.historyManager.clearHistory();
  }

  /**
   * Get a specific query from history
   */
  async getQueryFromHistory(queryId: string): Promise<any> {
    // TODO: getQuery is not implemented on QueryHistoryOrchestrator. Implement if needed.
    // return await this.historyManager.getQuery(queryId);
    return false;
  }

  /**
   * Delete a query from history
   */
  async deleteQueryFromHistory(queryId: string): Promise<boolean> {
    // TODO: deleteQuery is not implemented on QueryHistoryOrchestrator. Implement if needed.
    // return await this.historyManager.deleteQuery(queryId);
    return false;
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
    // TODO: getSession is not implemented on SessionManagerOrchestrator. Implement if needed.
    // return this.sessionManager.getSession(sessionId);
    return null;
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
    // TODO: listSessions is not implemented on SessionManagerOrchestrator. Implement if needed.
    // return this.sessionManager.listSessions(filter);
    return [];
  }

  /**
   * Delete a session
   */
  async deleteSession(sessionId: string): Promise<boolean> {
    // TODO: deleteSession is not implemented on SessionManagerOrchestrator. Implement if needed.
    // const result = await this.sessionManager.deleteSession(sessionId);
    // if (result) {
    //   try {
    //     await this.sendRequest('session_delete', { session_id: sessionId });
    //   } catch (error) {
    //     console.warn('[PrologBackend] Failed to delete session in Prolog backend:', error);
    //   }
    // }
    // return result;
    return false;
  }

  /**
   * Save current session state
   */
  async saveCurrentSessionState(): Promise<void> {
    // TODO: saveCurrentSessionState is not implemented on SessionManagerOrchestrator. Implement if needed.
    // await this.sessionManager.saveCurrentSessionState();

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
    // TODO: saveSessionState is not implemented on SessionManagerOrchestrator. Implement if needed.
    // await this.sessionManager.saveSessionState(sessionId, state);

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
    // TODO: restoreSessionState is not implemented on SessionManagerOrchestrator. Implement if needed.
    // await this.sessionManager.restoreSessionState(sessionId, snapshotId);

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
    // TODO: createSnapshot is not implemented on SessionManagerOrchestrator. Implement if needed.
    // return await this.sessionManager.createSnapshot(sessionId, name, description);
    return '';
  }

  /**
   * Get session-specific concurrency manager
   */
  // TODO: getSessionConcurrencyManager is not implemented on SessionManagerOrchestrator. Implement if needed.
  // getSessionConcurrencyManager(sessionId: string): ConcurrencyManager | undefined {
  //   return this.sessionManager.getSessionConcurrencyManager(sessionId);
  // }

  /**
   * Get session-specific history manager
   */
  // TODO: getSessionHistoryManager is not implemented on SessionManagerOrchestrator. Implement if needed.
  // getSessionHistoryManager(sessionId: string): QueryHistoryManager | undefined {
  //   return this.sessionManager.getSessionHistoryManager(sessionId);
  // }

  /**
   * Update session resource quota
   */
  async updateSessionResourceQuota(
    sessionId: string,
    quota: Partial<ResourceQuota>
  ): Promise<void> {
    // TODO: updateSessionResourceQuota is not implemented on SessionManagerOrchestrator. Implement if needed.
    // await this.sessionManager.updateSessionResourceQuota(sessionId, quota);
  }

  /**
   * Get session statistics
   */
  async getSessionStatistics(sessionId: string): Promise<any> {
    // TODO: getSessionStatistics is not implemented on SessionManagerOrchestrator. Implement if needed.
    // return this.sessionManager.getSessionStatistics(sessionId);
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
      // TODO: restoreSessionState is not implemented on SessionManagerOrchestrator. Implement if needed.
      // await this.sessionManager.restoreSessionState(sessionId);
    } catch (error) {
      console.error('[PrologBackend] Failed to import session state:', error);
      throw error;
    }
  }

  /**
   * Get session manager for direct access
   */
  // TODO: getSessionManager is not implemented on SessionManagerOrchestrator. Implement if needed.
  // getSessionManager(): SessionManager {
  //   return this.sessionManager;
  // }

  private handleExit(code: number | null, signal: NodeJS.Signals | null) {
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
        const onStarted = () => {
          this.log('[DEBUG] onStarted (auto-restart): emitting restarted');
          this.off('started', onStarted);
          this._suppressStoppedEvent = false;
          this.emit('restarted');
        };
        this.on('started', onStarted);
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

    // Validate the configured path first
    if (swiplPath && swiplPath !== PlatformUtils.getDefaultExecutablePath()) {
      const normalizedPath = PlatformUtils.normalizePath(swiplPath);
      if (await PlatformUtils.pathExists(normalizedPath)) {
        if (await PlatformUtils.isExecutable(normalizedPath)) {
          swiplPath = normalizedPath;
        } else {
          this.log(
            `[WARNING] Configured SWI-Prolog path '${normalizedPath}' exists but lacks execute permissions`
          );
          // Try to find alternative
          const executableFinder = new ExecutableFinder();
          const detectionResult = await executableFinder.findSwiplExecutable();
          if (detectionResult.found && detectionResult.path) {
            swiplPath = detectionResult.path;
            this.log(
              `[INFO] Using alternative SWI-Prolog at '${swiplPath}' found via ${detectionResult.detectionMethod}`
            );
          }
        }
      } else {
        this.log(`[WARNING] Configured SWI-Prolog path '${normalizedPath}' does not exist`);
        // Try to find alternative
        const executableFinder = new ExecutableFinder();
        const detectionResult = await executableFinder.findSwiplExecutable();
        if (detectionResult.found && detectionResult.path) {
          swiplPath = detectionResult.path;
          this.log(
            `[INFO] Using alternative SWI-Prolog at '${swiplPath}' found via ${detectionResult.detectionMethod}`
          );
        }
      }
    } else {
      // No specific path configured, use comprehensive detection
      const executableFinder = new ExecutableFinder();
      const detectionResult = await executableFinder.findSwiplExecutable();
      if (detectionResult.found && detectionResult.path) {
        swiplPath = detectionResult.path;
        this.log(
          `[INFO] Found SWI-Prolog at '${swiplPath}' via ${detectionResult.detectionMethod}`
        );

        // Check permissions
        if (!detectionResult.permissions?.executable) {
          this.log(`[WARNING] Found SWI-Prolog at '${swiplPath}' but it lacks execute permissions`);
          const platform = PlatformUtils.getPlatform();
          if (platform !== 'windows') {
            this.log(`[SUGGESTION] Try fixing permissions with: chmod +x "${swiplPath}"`);
          }
        }
      } else {
        // Fallback to default
        swiplPath = PlatformUtils.normalizePath(swiplPath);
        this.log(`[WARNING] Could not find SWI-Prolog executable, using fallback: ${swiplPath}`);
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
              const errorMsg = `SWI-Prolog backend failed to start: ${err && err.message ? err.message : err}`;
              this.log('Prolog handshake/version check failed: ' + errorMsg);

              // Always show enhanced error message for backend startup failures
              let troubleshooting = [
                'Check that SWI-Prolog is installed and accessible in your PATH.',
                'Verify the executable path in settings.',
                'Ensure you have permission to execute the file.',
                'Check for port conflicts or missing backend files.',
                'See the documentation or click "Installation Guide" for help.'
              ];
              if (err && err.stack) {
                troubleshooting.push('Error stack: ' + err.stack);
              }
              const action = await this.uiHandler.showErrorMessage(
                errorMsg,
                'Install SWI-Prolog',
                'Setup Wizard',
                'Configure Path',
                'View Logs',
                'Dismiss'
              );

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
                case 'View Logs': {
                  await this.uiHandler.executeCommand('prolog.openLogFile');
                  break;
                }
              }

              // Optionally emit an event or update UI for settings webview/dashboard
              this.emit('backendStartupFailed', { error: errorMsg, troubleshooting });
              this.stop();
            });
        }, 2000); // Give more time for HTTP server to start
      });
    }
  }
}
