import WebSocket from 'ws';
import { EventEmitter } from 'events';

export interface PrologWebSocketConfig {
  url: string;
  auth?: {
    type: 'api_key' | 'jwt_token' | 'none';
    apiKey?: string;
    jwtToken?: string;
  };
  reconnect?: {
    enabled: boolean;
    maxAttempts: number;
    delay: number;
    backoff: boolean;
  };
  heartbeat?: {
    enabled: boolean;
    interval: number;
  };
}

export interface QueryNotification {
  type: 'query_progress' | 'query_complete';
  query_id: string;
  status?: string;
  progress?: number;
  message?: string;
  results?: any[];
  execution_time?: number;
  timestamp: string;
}

export interface SessionEvent {
  type: 'session_event';
  session_id: string;
  event: 'created' | 'switched' | 'deleted' | 'state_saved';
  timestamp: string;
}

export interface SystemStatus {
  type: 'system_status';
  active_queries: number;
  active_sessions: number;
  memory_usage?: any;
  cpu_usage?: number;
}

/**
 * WebSocket client for real-time Prolog notifications
 * Provides real-time updates for query progress, session events, and system status
 */
export class PrologWebSocketClient extends EventEmitter {
  private ws: WebSocket | null = null;
  private config: PrologWebSocketConfig;
  private reconnectAttempts: number = 0;
  private reconnectTimer: NodeJS.Timeout | null = null;
  private heartbeatTimer: NodeJS.Timeout | null = null;
  private isConnected: boolean = false;
  private subscriptions: Set<string> = new Set();

  constructor(config: PrologWebSocketConfig) {
    super();
    this.config = {
      reconnect: {
        enabled: true,
        maxAttempts: 5,
        delay: 1000,
        backoff: true,
      },
      heartbeat: {
        enabled: true,
        interval: 30000,
      },
      ...config,
    };
  }

  /**
   * Connect to the WebSocket server
   */
  async connect(): Promise<void> {
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      return;
    }

    return new Promise((resolve, reject) => {
      try {
        const url = this.buildConnectionUrl();
        this.ws = new WebSocket(url);

        this.ws.on('open', () => {
          this.isConnected = true;
          this.reconnectAttempts = 0;
          this.startHeartbeat();
          this.emit('connected');
          console.log('[PrologWebSocketClient] Connected to WebSocket server');
          resolve();
        });

        this.ws.on('message', data => {
          this.handleMessage(data);
        });

        this.ws.on('close', (code, reason) => {
          this.isConnected = false;
          this.stopHeartbeat();
          this.emit('disconnected', code, reason);
          console.log(`[PrologWebSocketClient] Disconnected (${code}: ${reason})`);

          if (this.config.reconnect?.enabled && code !== 1000) {
            this.scheduleReconnect();
          }
        });

        this.ws.on('error', error => {
          this.emit('error', error);
          console.error('[PrologWebSocketClient] WebSocket error:', error);
          reject(error);
        });

        this.ws.on('pong', () => {
          this.emit('pong');
        });
      } catch (error) {
        reject(error);
      }
    });
  }

  /**
   * Disconnect from the WebSocket server
   */
  disconnect(): void {
    if (this.reconnectTimer) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }

    this.stopHeartbeat();

    if (this.ws) {
      this.ws.close(1000, 'Client disconnect');
      this.ws = null;
    }

    this.isConnected = false;
    this.subscriptions.clear();
  }

  /**
   * Subscribe to query notifications
   */
  async subscribeToQuery(
    queryId: string,
    eventTypes: string[] = ['progress', 'complete', 'error']
  ): Promise<void> {
    if (!this.isConnected) {
      throw new Error('WebSocket not connected');
    }

    const message = {
      type: 'subscribe',
      query_id: queryId,
      event_types: eventTypes,
    };

    this.send(message);
    this.subscriptions.add(`query:${queryId}`);
  }

  /**
   * Subscribe to session events
   */
  async subscribeToSession(
    sessionId: string,
    eventTypes: string[] = ['created', 'switched', 'deleted', 'state_saved']
  ): Promise<void> {
    if (!this.isConnected) {
      throw new Error('WebSocket not connected');
    }

    const message = {
      type: 'subscribe',
      session_id: sessionId,
      event_types: eventTypes,
    };

    this.send(message);
    this.subscriptions.add(`session:${sessionId}`);
  }

  /**
   * Subscribe to system status updates
   */
  async subscribeToSystemStatus(): Promise<void> {
    if (!this.isConnected) {
      throw new Error('WebSocket not connected');
    }

    const message = {
      type: 'subscribe',
      event_types: ['system_status'],
    };

    this.send(message);
    this.subscriptions.add('system:status');
  }

  /**
   * Unsubscribe from query notifications
   */
  async unsubscribeFromQuery(queryId: string): Promise<void> {
    if (!this.isConnected) {
      return;
    }

    const message = {
      type: 'unsubscribe',
      query_id: queryId,
    };

    this.send(message);
    this.subscriptions.delete(`query:${queryId}`);
  }

  /**
   * Unsubscribe from session events
   */
  async unsubscribeFromSession(sessionId: string): Promise<void> {
    if (!this.isConnected) {
      return;
    }

    const message = {
      type: 'unsubscribe',
      session_id: sessionId,
    };

    this.send(message);
    this.subscriptions.delete(`session:${sessionId}`);
  }

  /**
   * Cancel a running query
   */
  async cancelQuery(queryId: string): Promise<void> {
    if (!this.isConnected) {
      throw new Error('WebSocket not connected');
    }

    const message = {
      type: 'cancel_query',
      query_id: queryId,
    };

    this.send(message);
  }

  /**
   * Get query status
   */
  async getQueryStatus(queryId: string): Promise<void> {
    if (!this.isConnected) {
      throw new Error('WebSocket not connected');
    }

    const message = {
      type: 'get_query_status',
      query_id: queryId,
    };

    this.send(message);
  }

  /**
   * Get system status
   */
  async getSystemStatus(): Promise<void> {
    if (!this.isConnected) {
      throw new Error('WebSocket not connected');
    }

    const message = {
      type: 'get_system_status',
    };

    this.send(message);
  }

  /**
   * Send ping to server
   */
  ping(): void {
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      this.ws.ping();
    }
  }

  /**
   * Check if connected
   */
  isConnectedToServer(): boolean {
    return this.isConnected && this.ws?.readyState === WebSocket.OPEN;
  }

  /**
   * Get current subscriptions
   */
  getSubscriptions(): string[] {
    return Array.from(this.subscriptions);
  }

  /**
   * Build connection URL with authentication
   */
  private buildConnectionUrl(): string {
    const url = new URL(this.config.url);

    if (this.config.auth?.type === 'api_key' && this.config.auth.apiKey) {
      url.searchParams.set('api_key', this.config.auth.apiKey);
    } else if (this.config.auth?.type === 'jwt_token' && this.config.auth.jwtToken) {
      url.searchParams.set('token', this.config.auth.jwtToken);
    }

    return url.toString();
  }

  /**
   * Handle incoming WebSocket messages
   */
  private handleMessage(data: WebSocket.Data): void {
    try {
      const message = JSON.parse(data.toString());

      switch (message.type) {
        case 'welcome': {
          this.emit('welcome', message);
          break;
        }
        case 'query_progress': {
          this.emit('queryProgress', message as QueryNotification);
          this.emit('notification', message);
          break;
        }
        case 'query_complete': {
          this.emit('queryComplete', message as QueryNotification);
          this.emit('notification', message);
          break;
        }
        case 'session_event': {
          this.emit('sessionEvent', message as SessionEvent);
          this.emit('notification', message);
          break;
        }
        case 'system_status':
        case 'system_status_response': {
          this.emit('systemStatus', message as SystemStatus);
          this.emit('notification', message);
          break;
        }
        case 'query_status_response': {
          this.emit('queryStatusResponse', message);
          break;
        }
        case 'query_cancel_response': {
          this.emit('queryCancelResponse', message);
          break;
        }
        case 'subscribed': {
          this.emit('subscribed', message);
          break;
        }
        case 'unsubscribed': {
          this.emit('unsubscribed', message);
          break;
        }
        case 'error': {
          this.emit('serverError', message);
          break;
        }
        case 'pong': {
          this.emit('pong', message);
          break;
        }

        default:
          this.emit('unknownMessage', message);
          console.warn('[PrologWebSocketClient] Unknown message type:', message.type);
      }
    } catch (error) {
      console.error('[PrologWebSocketClient] Error parsing message:', error);
      this.emit('parseError', error, data);
    }
  }

  /**
   * Send message to server
   */
  private send(message: any): void {
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify(message));
    } else {
      throw new Error('WebSocket not connected');
    }
  }

  /**
   * Schedule reconnection attempt
   */
  private scheduleReconnect(): void {
    if (
      !this.config.reconnect?.enabled ||
      this.reconnectAttempts >= (this.config.reconnect.maxAttempts || 5)
    ) {
      this.emit('reconnectFailed');
      return;
    }

    this.reconnectAttempts++;

    let delay = this.config.reconnect.delay || 1000;
    if (this.config.reconnect.backoff) {
      delay *= Math.pow(2, this.reconnectAttempts - 1);
    }

    console.log(
      `[PrologWebSocketClient] Reconnecting in ${delay}ms (attempt ${this.reconnectAttempts})`
    );

    this.reconnectTimer = setTimeout(() => {
      this.emit('reconnecting', this.reconnectAttempts);
      this.connect().catch(error => {
        console.error('[PrologWebSocketClient] Reconnection failed:', error);
        this.scheduleReconnect();
      });
    }, delay);
  }

  /**
   * Start heartbeat mechanism
   */
  private startHeartbeat(): void {
    if (!this.config.heartbeat?.enabled) return;

    this.heartbeatTimer = setInterval(() => {
      if (this.isConnected) {
        this.send({ type: 'ping', timestamp: Date.now() });
      }
    }, this.config.heartbeat.interval || 30000);
  }

  /**
   * Stop heartbeat mechanism
   */
  private stopHeartbeat(): void {
    if (this.heartbeatTimer) {
      clearInterval(this.heartbeatTimer);
      this.heartbeatTimer = null;
    }
  }
}

/**
 * Factory function to create a PrologWebSocketClient instance
 */
export function createPrologWebSocketClient(config: PrologWebSocketConfig): PrologWebSocketClient {
  return new PrologWebSocketClient(config);
}

/**
 * Convenience function to create a WebSocket client with API key authentication
 */
export function createApiKeyWebSocketClient(url: string, apiKey: string): PrologWebSocketClient {
  return new PrologWebSocketClient({
    url,
    auth: {
      type: 'api_key',
      apiKey,
    },
  });
}

/**
 * Convenience function to create a WebSocket client with JWT authentication
 */
export function createJwtWebSocketClient(url: string, jwtToken: string): PrologWebSocketClient {
  return new PrologWebSocketClient({
    url,
    auth: {
      type: 'jwt_token',
      jwtToken,
    },
  });
}

/**
 * Convenience function to create a local WebSocket client (no authentication)
 */
export function createLocalWebSocketClient(
  url: string = 'ws://localhost:8081'
): PrologWebSocketClient {
  return new PrologWebSocketClient({
    url,
    auth: {
      type: 'none',
    },
  });
}
