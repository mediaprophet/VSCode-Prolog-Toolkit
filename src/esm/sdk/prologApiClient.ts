import type { AxiosInstance, AxiosResponse } from 'axios';
import axios from 'axios';
import { NodeEventEmitter } from '../shim/eventemitter-shim.js';

export interface PrologApiClientConfig {
  baseUrl: string;
  timeout?: number;
  auth?: {
    type: 'api_key' | 'jwt_token' | 'none';
    apiKey?: string;
    jwtToken?: string;
  };
  retries?: {
    enabled: boolean;
    maxRetries: number;
    retryDelay: number;
  };
}

export interface QueryRequest {
  query: string;
  session_id?: string;
  options?: {
    timeout?: number;
    max_results?: number;
    reasoning_mode?: 'default' | 'clp' | 'probabilistic' | 'n3' | 'custom';
    stream?: boolean;
  };
}

export interface QueryResponse {
  query_id: string;
  success: boolean;
  results: any[];
  execution_time: number;
  more_available: boolean;
  cursor?: string;
  streaming_info?: any;
}

export interface BatchRequest {
  queries: (QueryRequest | string)[];
  session_id?: string;
  batch_options?: {
    parallel?: boolean;
    fail_fast?: boolean;
    timeout?: number;
  };
}

export interface BatchResponse {
  batch_id: string;
  results: Array<{
    query_index: number;
    success: boolean;
    results: any[];
    error?: string;
  }>;
  total_queries: number;
  successful_queries: number;
}

export interface SessionConfig {
  name: string;
  description?: string;
  config?: any;
}

export interface Session {
  session_id: string;
  name: string;
  description?: string;
  created_at: string;
  is_active: boolean;
  user_id?: string;
}

export interface CLPRequest {
  constraints: string[];
  domain: 'fd' | 'r' | 'q';
  variables: string[];
}

export interface ProbabilisticRequest {
  facts: Array<{ fact: string; probability: number }>;
  query: string;
  samples?: number;
}

export interface N3Request {
  rules?: string;
  data?: string;
  query: string;
}

/**
 * TypeScript/JavaScript SDK for VSCode Prolog Toolkit API
 * Provides easy integration for AI agents and external applications
 */
export interface PrologApiClientEventMap {
  requestStart: [any];
  requestError: [any];
  responseSuccess: [any];
  responseError: [any];
  requestRetry: [number, any];
}

export class PrologApiClient extends NodeEventEmitter<PrologApiClientEventMap> {
  private client: AxiosInstance;
  private config: PrologApiClientConfig;

  constructor(config: PrologApiClientConfig) {
    super();
    this.config = {
      timeout: 30000,
      retries: {
        enabled: true,
        maxRetries: 3,
        retryDelay: 1000,
      },
      ...config,
    };

    this.client = axios.create({
      baseURL: this.config.baseUrl,
      timeout: this.config.timeout ?? 30000,
      headers: {
        'Content-Type': 'application/json',
        'User-Agent': 'PrologApiClient/1.0.0',
      },
    });

    this.setupAuthentication();
    this.setupInterceptors();
  }

  /**
   * Set up authentication headers
   */
  private setupAuthentication(): void {
    if (this.config.auth?.type === 'api_key' && this.config.auth.apiKey) {
      this.client.defaults.headers['X-API-Key'] = this.config.auth.apiKey;
    } else if (this.config.auth?.type === 'jwt_token' && this.config.auth.jwtToken) {
      this.client.defaults.headers['Authorization'] = `Bearer ${this.config.auth.jwtToken}`;
    }
  }

  /**
   * Set up request/response interceptors
   */
  private setupInterceptors(): void {
    // Request interceptor
    this.client.interceptors.request.use(
      config => {
        this.emit('requestStart', config);
        return config;
      },
      error => {
        this.emit('requestError', error);
        return Promise.reject(error);
      }
    );

    // Response interceptor with retry logic
    this.client.interceptors.response.use(
      response => {
        this.emit('responseSuccess', response);
        return response;
      },
      async error => {
        this.emit('responseError', error);

        if (this.config.retries?.enabled && this.shouldRetry(error)) {
          return this.retryRequest(error);
        }

        return Promise.reject(error);
      }
    );
  }

  /**
   * Check if request should be retried
   */
  private shouldRetry(error: any): boolean {
    if (!error.config || error.config.__retryCount >= (this.config.retries?.maxRetries || 3)) {
      return false;
    }

    // Retry on network errors or 5xx server errors
    return !error.response || (error.response.status >= 500 && error.response.status < 600);
  }

  /**
   * Retry failed request
   */
  private async retryRequest(error: any): Promise<AxiosResponse> {
    const config = error.config;
    config.__retryCount = (config.__retryCount || 0) + 1;

    const delay = this.config.retries?.retryDelay || 1000;
    await new Promise(resolve => setTimeout(resolve, delay * config.__retryCount));

    this.emit('requestRetry', config.__retryCount, config);
    return this.client.request(config);
  }

  /**
   * Execute a single Prolog query
   */
  async query(request: QueryRequest): Promise<QueryResponse> {
    try {
      const response = await this.client.post('/api/v1/query', request);
      return response.data;
    } catch (error) {
      throw this.handleError(error, 'Query execution failed');
    }
  }

  /**
   * Execute multiple queries in batch
   */
  async batch(request: BatchRequest): Promise<BatchResponse> {
    try {
      const response = await this.client.post('/api/v1/batch', request);
      return response.data;
    } catch (error) {
      throw this.handleError(error, 'Batch execution failed');
    }
  }

  /**
   * List all sessions
   */
  async listSessions(
    includeInactive: boolean = false
  ): Promise<{ sessions: Session[]; total: number }> {
    try {
      const response = await this.client.get('/api/v1/sessions', {
        params: { include_inactive: includeInactive },
      });
      return response.data;
    } catch (error) {
      throw this.handleError(error, 'Failed to list sessions');
    }
  }

  /**
   * Create a new session
   */
  async createSession(config: SessionConfig): Promise<Session> {
    try {
      const response = await this.client.post('/api/v1/sessions', config);
      return response.data;
    } catch (error) {
      throw this.handleError(error, 'Failed to create session');
    }
  }

  /**
   * Get session details
   */
  async getSession(sessionId: string): Promise<any> {
    try {
      const response = await this.client.get(`/api/v1/sessions/${sessionId}`);
      return response.data;
    } catch (error) {
      throw this.handleError(error, 'Failed to get session details');
    }
  }

  /**
   * Delete a session
   */
  async deleteSession(sessionId: string): Promise<{ message: string }> {
    try {
      const response = await this.client.delete(`/api/v1/sessions/${sessionId}`);
      return response.data;
    } catch (error) {
      throw this.handleError(error, 'Failed to delete session');
    }
  }

  /**
   * Export session state
   */
  async exportSessionState(sessionId: string): Promise<any> {
    try {
      const response = await this.client.get(`/api/v1/sessions/${sessionId}/state`);
      return response.data;
    } catch (error) {
      throw this.handleError(error, 'Failed to export session state');
    }
  }

  /**
   * Import session state
   */
  async importSessionState(sessionId: string, state: any): Promise<{ message: string }> {
    try {
      const response = await this.client.post(`/api/v1/sessions/${sessionId}/state`, { state });
      return response.data;
    } catch (error) {
      throw this.handleError(error, 'Failed to import session state');
    }
  }

  /**
   * Execute CLP (Constraint Logic Programming) reasoning
   */
  async clpReasoning(request: CLPRequest): Promise<any> {
    try {
      const response = await this.client.post('/api/v1/reasoning/clp', request);
      return response.data;
    } catch (error) {
      throw this.handleError(error, 'CLP reasoning failed');
    }
  }

  /**
   * Execute probabilistic inference
   */
  async probabilisticReasoning(request: ProbabilisticRequest): Promise<any> {
    try {
      const response = await this.client.post('/api/v1/reasoning/probabilistic', request);
      return response.data;
    } catch (error) {
      throw this.handleError(error, 'Probabilistic reasoning failed');
    }
  }

  /**
   * Execute N3/RDF reasoning
   */
  async n3Reasoning(request: N3Request): Promise<any> {
    try {
      const response = await this.client.post('/api/v1/reasoning/n3', request);
      return response.data;
    } catch (error) {
      throw this.handleError(error, 'N3 reasoning failed');
    }
  }

  /**
   * Get query history
   */
  async getHistory(
    options: {
      session_id?: string;
      limit?: number;
      offset?: number;
      status?: string;
    } = {}
  ): Promise<any> {
    try {
      const response = await this.client.get('/api/v1/history', { params: options });
      return response.data;
    } catch (error) {
      throw this.handleError(error, 'Failed to get query history');
    }
  }

  /**
   * Get system status
   */
  async getStatus(): Promise<any> {
    try {
      const response = await this.client.get('/api/v1/status');
      return response.data;
    } catch (error) {
      throw this.handleError(error, 'Failed to get system status');
    }
  }

  /**
   * Check API health
   */
  async health(): Promise<any> {
    try {
      const response = await this.client.get('/health');
      return response.data;
    } catch (error) {
      throw this.handleError(error, 'Health check failed');
    }
  }

  /**
   * Update authentication
   */
  updateAuth(auth: PrologApiClientConfig['auth']): void {
    this.config.auth = auth ?? { type: 'none' };

    // Clear existing auth headers
    delete this.client.defaults.headers['X-API-Key'];
    delete this.client.defaults.headers['Authorization'];

    // Set new auth headers
    this.setupAuthentication();
  }

  /**
   * Update base URL
   */
  updateBaseUrl(baseUrl: string): void {
    this.config.baseUrl = baseUrl;
    this.client.defaults.baseURL = baseUrl;
  }

  /**
   * Handle API errors
   */
  private handleError(error: any, defaultMessage: string): Error {
    if (error.response) {
      // Server responded with error status
      const { status, data } = error.response;
      const message = data?.message || data?.error || defaultMessage;
      const apiError = new Error(`API Error (${status}): ${message}`);
      (apiError as any).status = status;
      (apiError as any).response = data;
      return apiError;
    } else if (error.request) {
      // Request was made but no response received
      return new Error(`Network Error: ${defaultMessage}`);
    } else {
      // Something else happened
      return new Error(`Client Error: ${error.message || defaultMessage}`);
    }
  }

  /**
   * Get client configuration
   */
  getConfig(): PrologApiClientConfig {
    return { ...this.config };
  }

  /**
   * Test connection to the API
   */
  async testConnection(): Promise<{
    connected: boolean;
    latency: number;
    version?: string;
    error?: string;
  }> {
    const start = Date.now();

    try {
      const response = await this.health();
      const latency = Date.now() - start;

      return {
        connected: true,
        latency,
        version: response.version,
      };
    } catch (error) {
      const errorMsg = error instanceof Error ? error.message : String(error);
      console.warn('[PrologApiClient] Connection test failed:', errorMsg);
      return {
        connected: false,
        latency: Date.now() - start,
        error: errorMsg,
      };
    }
  }
}

/**
 * Factory function to create a PrologApiClient instance
 */
export function createPrologApiClient(config: PrologApiClientConfig): PrologApiClient {
  return new PrologApiClient(config);
}

/**
 * Convenience function to create a client with API key authentication
 */
export function createApiKeyClient(baseUrl: string, apiKey: string): PrologApiClient {
  return new PrologApiClient({
    baseUrl,
    auth: {
      type: 'api_key',
      apiKey,
    },
  });
}

/**
 * Convenience function to create a client with JWT authentication
 */
export function createJwtClient(baseUrl: string, jwtToken: string): PrologApiClient {
  return new PrologApiClient({
    baseUrl,
    auth: {
      type: 'jwt_token',
      jwtToken,
    },
  });
}

/**
 * Convenience function to create a local client (no authentication)
 */
export function createLocalClient(baseUrl: string = 'http://localhost:8080'): PrologApiClient {
  return new PrologApiClient({
    baseUrl,
    auth: {
      type: 'none',
    },
  });
}
