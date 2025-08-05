import * as WebSocket from 'ws';
import { EventEmitter } from 'events';
import { QueryNotificationManager } from './queryNotificationManager';
import { AuthConfig, verifyJwtToken, hasPermission } from './apiMiddleware';
import { IncomingMessage } from 'http';
import { URL } from 'url';

export interface ExternalWebSocketConfig {
  enabled: boolean;
  port: number;
  maxConnections: number;
  heartbeatInterval: number;
  auth: AuthConfig;
}

export interface WebSocketClient {
  id: string;
  ws: WebSocket;
  user?: {
    id: string;
    role: string;
    permissions: string[];
    method: string;
  };
  subscriptions: Set<string>;
  lastHeartbeat: number;
  isAlive: boolean;
}

/**
 * External WebSocket manager for AI agent real-time notifications
 * Extends the existing QueryNotificationManager with external access capabilities
 */
export class ExternalWebSocketManager extends EventEmitter {
  private server: WebSocket.Server | null = null;
  private clients: Map<string, WebSocketClient> = new Map();
  private config: ExternalWebSocketConfig;
  private queryNotificationManager: QueryNotificationManager;
  private heartbeatInterval: NodeJS.Timeout | null = null;
  private isRunning: boolean = false;

  constructor(config: ExternalWebSocketConfig, queryNotificationManager: QueryNotificationManager) {
    super();
    this.config = config;
    this.queryNotificationManager = queryNotificationManager;
    this.setupQueryNotificationForwarding();
  }

  /**
   * Start the external WebSocket server
   */
  async start(): Promise<void> {
    if (this.isRunning) {
      throw new Error('External WebSocket server is already running');
    }

    if (!this.config.enabled) {
      console.log('[ExternalWebSocketManager] External WebSocket server is disabled');
      return;
    }

    return new Promise((resolve, reject) => {
      try {
        this.server = new WebSocket.Server({
          port: this.config.port,
          perMessageDeflate: false,
          maxPayload: 1024 * 1024, // 1MB max message size
          verifyClient: (info: { origin: string; secure: boolean; req: IncomingMessage }) => this.verifyClient(info)
        });

        this.server.on('connection', (ws: WebSocket, request: IncomingMessage) => {
          this.handleConnection(ws, request);
        });

        this.server.on('error', (error) => {
          console.error('[ExternalWebSocketManager] Server error:', error);
          this.emit('error', error);
        });

        this.server.on('listening', () => {
          this.isRunning = true;
          this.startHeartbeat();
          console.log(`[ExternalWebSocketManager] External WebSocket server started on port ${this.config.port}`);
          resolve();
        });

      } catch (error: unknown) {
        reject(error);
      }
    });
  }

  /**
   * Stop the external WebSocket server
   */
  async stop(): Promise<void> {
    if (!this.isRunning) return;

    return new Promise((resolve) => {
      // Stop heartbeat
      if (this.heartbeatInterval) {
        clearInterval(this.heartbeatInterval);
        this.heartbeatInterval = null;
      }

      // Close all client connections
      this.clients.forEach((client) => {
        client.ws.close(1001, 'Server shutting down');
      });
      this.clients.clear();

      // Close server
      if (this.server) {
        this.server.close(() => {
          this.isRunning = false;
          console.log('[ExternalWebSocketManager] External WebSocket server stopped');
          resolve();
        });
      } else {
        resolve();
      }
    });
  }

  /**
   * Verify client connection
   */
  private verifyClient(info: { origin: string; secure: boolean; req: IncomingMessage }): boolean {
    // Check connection limits
    if (this.clients.size >= this.config.maxConnections) {
      console.log('[ExternalWebSocketManager] Connection rejected: max connections reached');
      return false;
    }

    // In local-only mode, only allow localhost connections
    if (this.config.auth.localOnly) {
      const clientIP = info.req.socket.remoteAddress || '';
      const isLocalhost = ['127.0.0.1', '::1', '::ffff:127.0.0.1'].includes(clientIP) ||
                         clientIP.startsWith('127.') ||
                         clientIP === 'localhost';
      
      if (!isLocalhost) {
        console.log(`[ExternalWebSocketManager] Connection rejected: non-localhost IP ${clientIP}`);
        return false;
      }
    }

    return true;
  }

  /**
   * Handle new WebSocket connection
   */
  private async handleConnection(ws: WebSocket, request: IncomingMessage): Promise<void> {
    const clientId = this.generateClientId();
    const client: WebSocketClient = {
      id: clientId,
      ws,
      subscriptions: new Set(),
      lastHeartbeat: Date.now(),
      isAlive: true
    };

    // Authenticate the client
    try {
      const user = await this.authenticateClient(request);
      client.user = user;
    } catch (error: unknown) {
      console.log(`[ExternalWebSocketManager] Authentication failed for client ${clientId}:`, error);
      ws.close(1008, 'Authentication failed');
      return;
    }

    this.clients.set(clientId, client);
    console.log(`[ExternalWebSocketManager] Client ${clientId} connected (user: ${client.user?.id || 'anonymous'})`);

    // Set up client event handlers
    ws.on('message', (data) => {
      this.handleClientMessage(clientId, data);
    });

    ws.on('close', (code, reason) => {
      console.log(`[ExternalWebSocketManager] Client ${clientId} disconnected (${code}: ${reason})`);
      this.clients.delete(clientId);
      this.emit('clientDisconnected', clientId);
    });

    ws.on('error', (error) => {
      console.error(`[ExternalWebSocketManager] Client ${clientId} error:`, error);
      this.clients.delete(clientId);
    });

    ws.on('pong', () => {
      const client = this.clients.get(clientId);
      if (client) {
        client.isAlive = true;
        client.lastHeartbeat = Date.now();
      }
    });

    // Send welcome message
    this.sendToClient(clientId, {
      type: 'welcome',
      client_id: clientId,
      server_time: new Date().toISOString(),
      capabilities: [
        'query_notifications',
        'session_events',
        'system_status',
        'real_time_updates'
      ]
    });

    this.emit('clientConnected', clientId, client.user);
  }

  /**
   * Authenticate WebSocket client
   */
  private async authenticateClient(request: IncomingMessage): Promise<{ id: string; role: string; permissions: string[]; method: string }> {
    const url = new URL(request.url || '', `ws://localhost:${this.config.port}`);
    
    // Try different authentication methods
    
    // 1. JWT token from query parameter
    const token = url.searchParams.get('token');
    if (token && this.config.auth.method === 'jwt_token') {
      try {
        const decoded = verifyJwtToken(token, this.config.auth);
        return {
          id: (decoded.sub as string) || (decoded.id as string) || 'jwt_user',
          role: (decoded.role as string) || 'agent',
          permissions: (decoded.permissions as string[]) || this.getDefaultPermissions((decoded.role as string) || 'agent'),
          method: 'jwt_token'
        };
      } catch (error: unknown) {
        console.error('[ExternalWebSocketManager] JWT verification failed:', error);
      }
    }

    // 2. API key from query parameter
    const apiKey = url.searchParams.get('api_key');
    if (apiKey && this.config.auth.method === 'api_key') {
      const isValidKey = this.config.auth.apiKeys.includes(apiKey);
      if (isValidKey) {
        return {
          id: `api_key_${apiKey.substring(0, 8)}`,
          role: 'agent',
          permissions: this.getDefaultPermissions('agent'),
          method: 'api_key'
        };
      }
    }

    // 3. Local-only mode
    if (this.config.auth.localOnly || this.config.auth.method === 'local_only') {
      const clientIP = request.socket.remoteAddress || '';
      const isLocalhost = ['127.0.0.1', '::1', '::ffff:127.0.0.1'].includes(clientIP) ||
                         clientIP.startsWith('127.') ||
                         clientIP === 'localhost';
      
      if (isLocalhost) {
        return {
          id: 'localhost',
          role: 'admin',
          permissions: ['*'],
          method: 'local_only'
        };
      }
    }

    throw new Error('Authentication required');
  }

  /**
   * Handle client message
   */
  private handleClientMessage(clientId: string, data: WebSocket.Data): void {
    const client = this.clients.get(clientId);
    if (!client) return;

    try {
      const message = JSON.parse(data.toString());
      console.log(`[ExternalWebSocketManager] Message from ${clientId}:`, message.type);

      switch (message.type) {
        case 'subscribe':
          this.handleSubscribe(clientId, message);
          break;
        case 'unsubscribe':
          this.handleUnsubscribe(clientId, message);
          break;
        case 'cancel_query':
          this.handleCancelQuery(clientId, message);
          break;
        case 'get_query_status':
          this.handleGetQueryStatus(clientId, message);
          break;
        case 'get_system_status':
          this.handleGetSystemStatus(clientId, message);
          break;
        case 'ping':
          this.sendToClient(clientId, { type: 'pong', timestamp: Date.now() });
          break;
        default:
          this.sendToClient(clientId, {
            type: 'error',
            message: `Unknown message type: ${message.type}`
          });
      }
    } catch (error: unknown) {
      console.error(`[ExternalWebSocketManager] Error parsing message from ${clientId}:`, error);
      this.sendToClient(clientId, {
        type: 'error',
        message: 'Invalid message format'
      });
    }
  }

  /**
   * Handle subscription request
   */
  private handleSubscribe(clientId: string, message: { query_id?: string; session_id?: string; event_types?: string[] }): void {
    const client = this.clients.get(clientId);
    if (!client) return;

    const { query_id, session_id, event_types = ['progress', 'complete', 'error', 'cancel'] } = message;

    // Check permissions
    if (!hasPermission(client.user ?? undefined, 'notifications:subscribe')) {
      this.sendToClient(clientId, {
        type: 'error',
        message: 'Insufficient permissions to subscribe to notifications'
      });
      return;
    }

    // Add subscriptions
    if (query_id) {
      client.subscriptions.add(`query:${query_id}`);
    }
    if (session_id) {
      client.subscriptions.add(`session:${session_id}`);
    }
    
    event_types.forEach((eventType: string) => {
      client.subscriptions.add(`event:${eventType}`);
    });

    this.sendToClient(clientId, {
      type: 'subscribed',
      query_id,
      session_id,
      event_types,
      message: 'Successfully subscribed to notifications'
    });
  }

  /**
   * Handle unsubscribe request
   */
  private handleUnsubscribe(clientId: string, message: { query_id?: string; session_id?: string; event_types?: string[] }): void {
    const client = this.clients.get(clientId);
    if (!client) return;

    const { query_id, session_id, event_types } = message;

    if (query_id) {
      client.subscriptions.delete(`query:${query_id}`);
    }
    if (session_id) {
      client.subscriptions.delete(`session:${session_id}`);
    }
    if (event_types) {
      event_types.forEach((eventType: string) => {
        client.subscriptions.delete(`event:${eventType}`);
      });
    }

    this.sendToClient(clientId, {
      type: 'unsubscribed',
      query_id,
      session_id,
      event_types,
      message: 'Successfully unsubscribed from notifications'
    });
  }

  /**
   * Handle query cancellation request
   */
  private handleCancelQuery(clientId: string, message: { query_id?: string }): void {
    const client = this.clients.get(clientId);
    if (!client || !hasPermission(client.user ?? undefined, 'query:cancel')) {
      this.sendToClient(clientId, {
        type: 'error',
        message: 'Insufficient permissions to cancel queries'
      });
      return;
    }

    const { query_id } = message;
    if (!query_id) {
      this.sendToClient(clientId, {
        type: 'error',
        message: 'Query ID is required for cancellation'
      });
      return;
    }

    // Forward to query notification manager
    const cancelled = this.queryNotificationManager.cancelQuery(query_id);
    
    this.sendToClient(clientId, {
      type: 'query_cancel_response',
      query_id,
      cancelled,
      message: cancelled ? 'Query cancelled successfully' : 'Query not found or already completed'
    });
  }

  /**
   * Handle query status request
   */
  private handleGetQueryStatus(clientId: string, message: { query_id?: string }): void {
    const client = this.clients.get(clientId);
    if (!client || !hasPermission(client.user ?? undefined, 'query:status')) {
      this.sendToClient(clientId, {
        type: 'error',
        message: 'Insufficient permissions to get query status'
      });
      return;
    }

    const { query_id } = message;
    const status = this.queryNotificationManager.getQueryStatus(query_id || '');

    this.sendToClient(clientId, {
      type: 'query_status_response',
      query_id,
      status: status || null,
      found: !!status
    });
  }

  /**
   * Handle system status request
   */
  private handleGetSystemStatus(clientId: string, message: Record<string, unknown>): void {
    const client = this.clients.get(clientId);
    if (!client || !hasPermission(client.user ?? undefined, 'status:read')) {
      this.sendToClient(clientId, {
        type: 'error',
        message: 'Insufficient permissions to get system status'
      });
      return;
    }

    const stats = this.queryNotificationManager.getStatistics();
    
    this.sendToClient(clientId, {
      type: 'system_status_response',
      active_queries: stats.running,
      total_queries: stats.total,
      connected_clients: this.clients.size,
      server_uptime: process.uptime(),
      timestamp: new Date().toISOString()
    });
  }

  /**
   * Send message to specific client
   */
  private sendToClient(clientId: string, message: Record<string, unknown>): void {
    const client = this.clients.get(clientId);
    if (!client || client.ws.readyState !== WebSocket.OPEN) return;

    try {
      client.ws.send(JSON.stringify(message));
    } catch (error: unknown) {
      console.error(`[ExternalWebSocketManager] Error sending message to ${clientId}:`, error);
      this.clients.delete(clientId);
    }
  }

  /**
   * Broadcast message to all subscribed clients
   */
  private broadcast(message: Record<string, unknown>, subscription?: string): void {
    this.clients.forEach((client, clientId) => {
      if (subscription && !client.subscriptions.has(subscription)) return;
      this.sendToClient(clientId, message);
    });
  }

  /**
   * Set up forwarding from QueryNotificationManager
   */
  private setupQueryNotificationForwarding(): void {
    this.queryNotificationManager.on('queryStatusUpdated', (status) => {
      this.broadcast({
        type: 'query_progress',
        query_id: status.id,
        status: status.status,
        progress: status.progress,
        message: status.message,
        timestamp: new Date().toISOString()
      }, `query:${status.id}`);

      this.broadcast({
        type: 'query_progress',
        query_id: status.id,
        status: status.status,
        progress: status.progress,
        message: status.message,
        timestamp: new Date().toISOString()
      }, `event:${status.status}`);
    });

    this.queryNotificationManager.on('query_completed', (status) => {
      this.broadcast({
        type: 'query_complete',
        query_id: status.id,
        results: status.results,
        execution_time: status.endTime ? status.endTime - status.startTime : 0,
        timestamp: new Date().toISOString()
      }, `query:${status.id}`);
    });
  }

  /**
   * Start heartbeat mechanism
   */
  private startHeartbeat(): void {
    this.heartbeatInterval = setInterval(() => {
      this.clients.forEach((client, clientId) => {
        if (!client.isAlive) {
          console.log(`[ExternalWebSocketManager] Terminating inactive client ${clientId}`);
          client.ws.terminate();
          this.clients.delete(clientId);
          return;
        }

        client.isAlive = false;
        client.ws.ping();
      });
    }, this.config.heartbeatInterval * 1000);
  }

  /**
   * Generate unique client ID
   */
  private generateClientId(): string {
    return `ws_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  }

  /**
   * Get default permissions for role
   */
  private getDefaultPermissions(role: string): string[] {
    const rolePermissions: { [key: string]: string[] } = {
      admin: ['*'],
      agent: [
        'query:execute',
        'query:cancel',
        'query:status',
        'notifications:subscribe',
        'session:read',
        'status:read'
      ],
      readonly: [
        'query:status',
        'notifications:subscribe',
        'status:read'
      ],
      limited: [
        'query:status'
      ]
    };

    return rolePermissions[role] || rolePermissions['limited'] || [];
  }

  /**
   * Get server status
   */
  getStatus(): {
    running: boolean;
    port: number;
    connectedClients: number;
    maxConnections: number;
  } {
    return {
      running: this.isRunning,
      port: this.config.port,
      connectedClients: this.clients.size,
      maxConnections: this.config.maxConnections
    };
  }
}