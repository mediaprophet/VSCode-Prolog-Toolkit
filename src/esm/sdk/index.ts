/**
 * VSCode Prolog Toolkit API SDK
 *
 * A comprehensive TypeScript/JavaScript SDK for integrating AI agents and external applications
 * with the VSCode Prolog Toolkit's HTTP and WebSocket APIs.
 *
 * @version 1.0.0
 * @author VSCode Prolog Toolkit Team
 */

// Main API Client
export {
  createApiKeyClient,
  createJwtClient,
  createLocalClient,
  createPrologApiClient,
  PrologApiClient,
} from './prologApiClient.js';

// WebSocket Client
export {
  createApiKeyWebSocketClient,
  createJwtWebSocketClient,
  createLocalWebSocketClient,
  createPrologWebSocketClient,
  PrologWebSocketClient,
} from './prologWebSocketClient.js';

// Type Definitions
export * from './types.js';

// Re-export specific types for convenience
export type {
  BatchRequest,
  BatchResponse,
  CLPRequest,
  N3Request,
  ProbabilisticRequest,
  PrologApiClientConfig,
  QueryRequest,
  QueryResponse,
  Session,
  SessionConfig,
} from './prologApiClient.js';

export type {
  PrologWebSocketConfig,
  QueryNotification,
  SessionEvent,
  SystemStatus,
} from './prologWebSocketClient.js';

// Import the classes for use in the SDK
import { PrologApiClient } from './prologApiClient.js';
import { PrologWebSocketClient } from './prologWebSocketClient.js';

/**
 * Combined SDK class that provides both HTTP and WebSocket functionality
 */
export class PrologSDK {
  public readonly api: import('./prologApiClient.js').PrologApiClient;
  public readonly ws: import('./prologWebSocketClient.js').PrologWebSocketClient;

  constructor(config: {
    apiConfig: import('./prologApiClient.js').PrologApiClientConfig;
    wsConfig?: import('./prologWebSocketClient.js').PrologWebSocketConfig;
  }) {
    this.api = new PrologApiClient(config.apiConfig);

    if (config.wsConfig) {
      this.ws = new PrologWebSocketClient(config.wsConfig);
    } else {
      // Create WebSocket client with same auth as API client
      const wsUrl =
        config.apiConfig.baseUrl.replace(/^https?:/, 'ws:').replace(/^http:/, 'ws:') + ':8081';
      this.ws = new PrologWebSocketClient({
        url: wsUrl,
        auth: config.apiConfig.auth ?? { type: 'none' },
      });
    }
  }

  /**
   * Initialize both API and WebSocket connections
   */
  async connect(): Promise<void> {
    await this.ws.connect();
  }

  /**
   * Disconnect from both API and WebSocket
   */
  async disconnect(): Promise<void> {
    this.ws.disconnect();
  }

  /**
   * Execute a query with real-time progress notifications
   */
  async queryWithNotifications(
    request: import('./prologApiClient.js').QueryRequest,
    onProgress?: (notification: import('./prologWebSocketClient.js').QueryNotification) => void
  ): Promise<import('./prologApiClient.js').QueryResponse> {
    // Subscribe to query notifications if WebSocket is connected
    if (this.ws.isConnectedToServer() && onProgress) {
      const response = await this.api.query(request);

      // Subscribe to this specific query
      await this.ws.subscribeToQuery(response.query_id);

      // Set up progress listener
      const progressHandler = (
        notification: import('./prologWebSocketClient.js').QueryNotification
      ) => {
        if (notification.query_id === response.query_id) {
          onProgress(notification);

          // Unsubscribe when query completes
          if (notification.type === 'query_complete') {
            this.ws.unsubscribeFromQuery(response.query_id);
            this.ws.off('queryProgress', progressHandler);
            this.ws.off('queryComplete', progressHandler);
          }
        }
      };

      this.ws.on('queryProgress', progressHandler);
      this.ws.on('queryComplete', progressHandler);

      return response;
    } else {
      // Fallback to regular query without notifications
      return this.api.query(request);
    }
  }

  /**
   * Test connection to both API and WebSocket servers
   */
  async testConnection(): Promise<{
    api: { connected: boolean; latency: number; version?: string };
    websocket: { connected: boolean; latency: number };
  }> {
    const apiTest = await this.api.testConnection();

    const wsStart = Date.now();
    const wsConnected = this.ws.isConnectedToServer();
    const wsLatency = wsConnected ? Date.now() - wsStart : -1;

    return {
      api: apiTest,
      websocket: {
        connected: wsConnected,
        latency: wsLatency,
      },
    };
  }
}

/**
 * Factory function to create a complete SDK instance
 */
export function createPrologSDK(config: {
  baseUrl: string;
  auth?: {
    type: 'api_key' | 'jwt_token' | 'none';
    apiKey?: string;
    jwtToken?: string;
  };
  wsUrl?: string;
}): PrologSDK {
  const apiConfig = {
    baseUrl: config.baseUrl,
    auth: config.auth ?? { type: 'none' },
  };

  const wsConfig = {
    url: config.wsUrl || config.baseUrl.replace(/^https?:/, 'ws:') + ':8081',
    auth: config.auth ?? { type: 'none' },
  };

  return new PrologSDK({ apiConfig, wsConfig });
}

/**
 * Convenience function to create SDK for local development
 */
export function createLocalSDK(
  apiUrl: string = 'http://localhost:8080',
  wsUrl: string = 'ws://localhost:8081'
): PrologSDK {
  return createPrologSDK({
    baseUrl: apiUrl,
    wsUrl,
    auth: { type: 'none' },
  });
}

/**
 * Convenience function to create SDK with API key authentication
 */
export function createApiKeySDK(baseUrl: string, apiKey: string, wsUrl?: string): PrologSDK {
  return createPrologSDK({
    baseUrl,
    wsUrl: wsUrl || baseUrl.replace(/^https?:/, 'ws:') + ':8081',
    auth: {
      type: 'api_key',
      apiKey,
    },
  });
}

/**
 * Convenience function to create SDK with JWT authentication
 */
export function createJwtSDK(baseUrl: string, jwtToken: string, wsUrl?: string): PrologSDK {
  return createPrologSDK({
    baseUrl,
    wsUrl: wsUrl || baseUrl.replace(/^https?:/, 'ws:') + ':8081',
    auth: {
      type: 'jwt_token',
      jwtToken,
    },
  });
}

// Version information
export const SDK_VERSION = '1.0.0';
export const SUPPORTED_API_VERSION = 'v1';

// Default configurations
export const DEFAULT_CONFIG = {
  API_PORT: 8080,
  WEBSOCKET_PORT: 8081,
  TIMEOUT: 30000,
  MAX_RETRIES: 3,
  RETRY_DELAY: 1000,
  HEARTBEAT_INTERVAL: 30000,
  MAX_RECONNECT_ATTEMPTS: 5,
};

/**
 * Utility functions for common operations
 */
export const utils = {
  /**
   * Validate Prolog query syntax (basic validation)
   */
  validateQuery(query: string): { valid: boolean; error?: string } {
    if (!query || typeof query !== 'string') {
      return { valid: false, error: 'Query must be a non-empty string' };
    }

    const trimmed = query.trim();
    if (trimmed.length === 0) {
      return { valid: false, error: 'Query cannot be empty' };
    }

    // Basic syntax checks
    const openParens = (trimmed.match(/\(/g) || []).length;
    const closeParens = (trimmed.match(/\)/g) || []).length;

    if (openParens !== closeParens) {
      return { valid: false, error: 'Mismatched parentheses' };
    }

    return { valid: true };
  },

  /**
   * Format query results for display
   */
  formatResults(results: any[]): string {
    if (!results || results.length === 0) {
      return 'No results found.';
    }

    return results
      .map((result, index) => {
        if (typeof result === 'object' && result !== null) {
          const bindings = Object.entries(result)
            .map(([variable, value]) => `${variable} = ${value}`)
            .join(', ');
          return `Solution ${index + 1}: ${bindings}`;
        } else {
          return `Solution ${index + 1}: ${result}`;
        }
      })
      .join('\n');
  },

  /**
   * Create a session name with timestamp
   */
  createSessionName(prefix: string = 'session'): string {
    const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
    return `${prefix}-${timestamp}`;
  },

  /**
   * Parse error response
   */
  parseError(error: any): { message: string; code?: string; status?: number } {
    if (error.response?.data) {
      return {
        message: error.response.data.message || error.response.data.error || 'Unknown API error',
        code: error.response.data.code,
        status: error.response.status,
      };
    } else if (error.message) {
      return { message: error.message };
    } else {
      return { message: 'Unknown error occurred' };
    }
  },
};

// Export default SDK class
export default PrologSDK;
