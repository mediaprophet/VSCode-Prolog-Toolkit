import { EventEmitter } from 'events';
import { v4 as uuidv4 } from 'uuid';
import * as WebSocket from 'ws';

export interface QueryStatus {
  id: string;
  status: 'pending' | 'running' | 'completed' | 'error' | 'cancelled' | 'timeout';
  startTime: number;
  endTime?: number;
  progress?: number;
  message?: string;
  results?: any;
  error?: any;
  isBatch?: boolean;
  batchIndex?: number;
  totalBatchSize?: number;
}

export interface QueryNotificationOptions {
  enableProgress?: boolean;
  progressInterval?: number;
  enableWebSocket?: boolean;
  webSocketPort?: number;
}

export interface QueryCallback {
  onProgress?: (status: QueryStatus) => void;
  onComplete?: (status: QueryStatus) => void;
  onError?: (status: QueryStatus) => void;
  onCancel?: (status: QueryStatus) => void;
}

/**
 * Manages query status tracking and notifications for long-running Prolog queries
 */
export class QueryNotificationManager extends EventEmitter {
  private queries: Map<string, QueryStatus> = new Map();
  private callbacks: Map<string, QueryCallback> = new Map();
  private wsServer?: WebSocket.Server;
  private wsClients: Set<WebSocket> = new Set();
  private options: QueryNotificationOptions;

  constructor(options: QueryNotificationOptions = {}) {
    super();
    this.options = {
      enableProgress: true,
      progressInterval: 1000,
      enableWebSocket: false,
      webSocketPort: 3062,
      ...options
    };

    if (this.options.enableWebSocket) {
      this.initializeWebSocketServer();
    }
  }

  /**
   * Initialize WebSocket server for real-time notifications
   */
  private initializeWebSocketServer(): void {
    try {
      this.wsServer = new WebSocket.Server({ 
        port: this.options.webSocketPort!,
        perMessageDeflate: false
      });

      this.wsServer.on('connection', (ws: WebSocket) => {
        console.log('[QueryNotificationManager] WebSocket client connected');
        this.wsClients.add(ws);

        // Send current query statuses to new client
        const currentQueries = Array.from(this.queries.values());
        if (currentQueries.length > 0) {
          ws.send(JSON.stringify({
            type: 'query_status_batch',
            queries: currentQueries
          }));
        }

        ws.on('close', () => {
          console.log('[QueryNotificationManager] WebSocket client disconnected');
          this.wsClients.delete(ws);
        });

        ws.on('error', (error) => {
          console.error('[QueryNotificationManager] WebSocket error:', error);
          this.wsClients.delete(ws);
        });

        // Handle client messages (e.g., cancel requests)
        ws.on('message', (data) => {
          try {
            const message = JSON.parse(data.toString());
            this.handleWebSocketMessage(message, ws);
          } catch (error) {
            console.error('[QueryNotificationManager] Invalid WebSocket message:', error);
          }
        });
      });

      this.wsServer.on('error', (error) => {
        console.error('[QueryNotificationManager] WebSocket server error:', error);
      });

      console.log(`[QueryNotificationManager] WebSocket server started on port ${this.options.webSocketPort}`);
    } catch (error) {
      console.error('[QueryNotificationManager] Failed to start WebSocket server:', error);
    }
  }

  /**
   * Handle incoming WebSocket messages
   */
  private handleWebSocketMessage(message: any, ws: WebSocket): void {
    switch (message.type) {
      case 'cancel_query':
        if (message.queryId) {
          this.cancelQuery(message.queryId);
        }
        break;
      case 'get_query_status':
        if (message.queryId) {
          const status = this.getQueryStatus(message.queryId);
          if (status) {
            ws.send(JSON.stringify({
              type: 'query_status',
              query: status
            }));
          }
        }
        break;
      case 'get_all_queries':
        const allQueries = Array.from(this.queries.values());
        ws.send(JSON.stringify({
          type: 'query_status_batch',
          queries: allQueries
        }));
        break;
    }
  }

  /**
   * Broadcast notification to all WebSocket clients
   */
  private broadcastNotification(notification: any): void {
    if (this.wsClients.size === 0) return;

    const message = JSON.stringify(notification);
    this.wsClients.forEach(ws => {
      if (ws.readyState === WebSocket.OPEN) {
        try {
          ws.send(message);
        } catch (error) {
          console.error('[QueryNotificationManager] Failed to send WebSocket message:', error);
          this.wsClients.delete(ws);
        }
      }
    });
  }

  /**
   * Register a new query for tracking
   */
  registerQuery(
    queryId: string, 
    callback?: QueryCallback,
    isBatch: boolean = false,
    batchIndex?: number,
    totalBatchSize?: number
  ): void {
    const status: QueryStatus = {
      id: queryId,
      status: 'pending',
      startTime: Date.now(),
      isBatch,
      batchIndex,
      totalBatchSize
    };

    this.queries.set(queryId, status);
    
    if (callback) {
      this.callbacks.set(queryId, callback);
    }

    // Emit event
    this.emit('queryRegistered', status);

    // Broadcast to WebSocket clients
    this.broadcastNotification({
      type: 'query_registered',
      query: status
    });

    console.log(`[QueryNotificationManager] Registered query ${queryId}`);
  }

  /**
   * Update query status
   */
  updateQueryStatus(
    queryId: string, 
    updates: Partial<QueryStatus>
  ): void {
    const status = this.queries.get(queryId);
    if (!status) {
      console.warn(`[QueryNotificationManager] Query ${queryId} not found`);
      return;
    }

    // Update status
    Object.assign(status, updates);

    // Set end time for terminal states
    if (['completed', 'error', 'cancelled', 'timeout'].includes(status.status)) {
      status.endTime = Date.now();
    }

    this.queries.set(queryId, status);

    // Call registered callbacks
    const callback = this.callbacks.get(queryId);
    if (callback) {
      switch (status.status) {
        case 'running':
          if (callback.onProgress) {
            callback.onProgress(status);
          }
          break;
        case 'completed':
          if (callback.onComplete) {
            callback.onComplete(status);
          }
          break;
        case 'error':
          if (callback.onError) {
            callback.onError(status);
          }
          break;
        case 'cancelled':
          if (callback.onCancel) {
            callback.onCancel(status);
          }
          break;
      }
    }

    // Emit events
    this.emit('queryStatusUpdated', status);
    this.emit(`query_${status.status}`, status);

    // Broadcast to WebSocket clients
    this.broadcastNotification({
      type: 'query_status_updated',
      query: status
    });

    console.log(`[QueryNotificationManager] Updated query ${queryId} status to ${status.status}`);

    // Clean up completed queries after a delay
    if (['completed', 'error', 'cancelled', 'timeout'].includes(status.status)) {
      setTimeout(() => {
        this.cleanupQuery(queryId);
      }, 30000); // Keep for 30 seconds
    }
  }

  /**
   * Update query progress
   */
  updateQueryProgress(queryId: string, progress: number, message?: string): void {
    this.updateQueryStatus(queryId, {
      status: 'running',
      progress: Math.max(0, Math.min(100, progress)),
      message
    });
  }

  /**
   * Mark query as completed
   */
  completeQuery(queryId: string, results?: any): void {
    this.updateQueryStatus(queryId, {
      status: 'completed',
      results,
      progress: 100
    });
  }

  /**
   * Mark query as failed
   */
  failQuery(queryId: string, error: any): void {
    this.updateQueryStatus(queryId, {
      status: 'error',
      error
    });
  }

  /**
   * Cancel a running query
   */
  cancelQuery(queryId: string): boolean {
    const status = this.queries.get(queryId);
    if (!status) {
      return false;
    }

    if (['completed', 'error', 'cancelled'].includes(status.status)) {
      return false; // Already finished
    }

    this.updateQueryStatus(queryId, {
      status: 'cancelled'
    });

    // Emit cancellation event for external handlers
    this.emit('queryCancelled', queryId);

    return true;
  }

  /**
   * Get query status
   */
  getQueryStatus(queryId: string): QueryStatus | undefined {
    return this.queries.get(queryId);
  }

  /**
   * Get all active queries
   */
  getActiveQueries(): QueryStatus[] {
    return Array.from(this.queries.values())
      .filter(q => !['completed', 'error', 'cancelled', 'timeout'].includes(q.status));
  }

  /**
   * Get all queries
   */
  getAllQueries(): QueryStatus[] {
    return Array.from(this.queries.values());
  }

  /**
   * Clean up query from tracking
   */
  private cleanupQuery(queryId: string): void {
    this.queries.delete(queryId);
    this.callbacks.delete(queryId);
    
    this.emit('queryCleanedUp', queryId);
    
    this.broadcastNotification({
      type: 'query_cleaned_up',
      queryId
    });

    console.log(`[QueryNotificationManager] Cleaned up query ${queryId}`);
  }

  /**
   * Clean up all completed queries
   */
  cleanupCompletedQueries(): void {
    const completedQueries = Array.from(this.queries.entries())
      .filter(([_, status]) => ['completed', 'error', 'cancelled', 'timeout'].includes(status.status))
      .map(([id, _]) => id);

    completedQueries.forEach(id => this.cleanupQuery(id));
  }

  /**
   * Get query statistics
   */
  getStatistics(): {
    total: number;
    pending: number;
    running: number;
    completed: number;
    error: number;
    cancelled: number;
    timeout: number;
  } {
    const queries = Array.from(this.queries.values());
    return {
      total: queries.length,
      pending: queries.filter(q => q.status === 'pending').length,
      running: queries.filter(q => q.status === 'running').length,
      completed: queries.filter(q => q.status === 'completed').length,
      error: queries.filter(q => q.status === 'error').length,
      cancelled: queries.filter(q => q.status === 'cancelled').length,
      timeout: queries.filter(q => q.status === 'timeout').length
    };
  }

  /**
   * Close WebSocket server and cleanup
   */
  close(): void {
    if (this.wsServer) {
      this.wsServer.close();
      this.wsClients.clear();
    }
    this.queries.clear();
    this.callbacks.clear();
    this.removeAllListeners();
  }
}