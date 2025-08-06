/**
 * API-related type definitions for HTTP API, WebSocket, and external integrations
 */

import { Request, Response } from 'express';

// API Server Configuration
export interface ApiServerConfig {
  enabled: boolean;
  port: number;
  host: string;
  corsOrigins: string[];
  maxConnections: number;
  requestTimeout: number;
  rateLimiting: {
    enabled: boolean;
    requestsPerMinute: number;
    burstLimit: number;
  };
  auth: AuthConfig;
}

// Authentication Configuration
export interface AuthConfig {
  method: 'api_key' | 'jwt_token' | 'local_only' | 'oauth2';
  apiKeys: string[];
  jwtSecret: string;
  localOnly: boolean;
  oauth2: {
    providers: string[];
    clientId?: string;
    clientSecret?: string;
    redirectUri?: string;
    scope?: string;
  };
  roles: {
    admin: string[];
    agent: string[];
    readonly: string[];
    limited: string[];
  };
  quotas: {
    admin: UserQuota;
    agent: UserQuota;
    readonly: UserQuota;
    limited: UserQuota;
  };
}

export interface UserQuota {
  requestsPerMinute: number;
  maxConcurrentSessions: number;
}

// User and Authentication Types
export interface AuthenticatedUser {
  id: string;
  role: 'admin' | 'agent' | 'readonly' | 'limited';
  permissions: string[];
  method: 'api_key' | 'jwt_token' | 'local_only' | 'oauth2';
}

export interface AuthenticatedRequest extends Request {
  user?: AuthenticatedUser;
  quota?: UserQuota;
}

// API Response Types
export interface ApiResponse<T = any> {
  success: boolean;
  data?: T;
  error?: string;
  message?: string;
  timestamp: string;
  requestId?: string;
}

export interface ApiErrorResponse extends ApiResponse {
  success: false;
  error: string;
  code?: string;
  details?: Record<string, any>;
  stack?: string; // Only in development
}

export interface PaginatedApiResponse<T = any> extends ApiResponse<T[]> {
  pagination: {
    total: number;
    limit: number;
    offset: number;
    hasMore: boolean;
    nextOffset?: number;
  };
}

// WebSocket Configuration and Types
export interface ExternalWebSocketConfig {
  enabled: boolean;
  port: number;
  maxConnections: number;
  heartbeatInterval: number;
  auth: AuthConfig;
}

export interface WebSocketMessage {
  type: string;
  id?: string;
  timestamp: string;
  data?: any;
  [key: string]: any;
}

export interface WebSocketClient {
  id: string;
  socket: any; // WebSocket instance
  user?: AuthenticatedUser;
  connectedAt: Date;
  lastActivity: Date;
  subscriptions: Set<string>;
}

// Query API Types
export interface QueryRequest {
  query: string;
  options?: {
    timeout?: number;
    maxResults?: number;
    streaming?: boolean;
    sessionId?: string;
    priority?: 'low' | 'medium' | 'high' | 'critical';
  };
}

export interface QueryResponse extends ApiResponse {
  queryId: string;
  results?: any[];
  executionTime: number;
  streaming?: {
    totalChunks: number;
    currentChunk: number;
    hasMore: boolean;
  };
}

export interface BatchQueryRequest {
  queries: Array<{
    id?: string;
    query: string;
    options?: QueryRequest['options'];
  }>;
  batchOptions?: {
    parallel?: boolean;
    failFast?: boolean;
    timeout?: number;
  };
}

export interface BatchQueryResponse extends ApiResponse {
  batchId: string;
  results: Array<{
    queryId: string;
    success: boolean;
    results?: any[];
    error?: string;
    executionTime: number;
  }>;
  totalQueries: number;
  successfulQueries: number;
  failedQueries: number;
  totalExecutionTime: number;
}

// Session API Types
export interface CreateSessionRequest {
  name: string;
  description?: string;
  userId?: string;
  agentId?: string;
  resourceQuota?: Partial<{
    maxConcurrentQueries: number;
    maxQueryTime: number;
    maxMemoryPerQuery: number;
    maxResultsPerQuery: number;
  }>;
  metadata?: Record<string, any>;
}

export interface SessionResponse extends ApiResponse {
  session: {
    sessionId: string;
    name: string;
    description?: string;
    createdAt: string;
    isActive: boolean;
    userId?: string;
    agentId?: string;
    statistics?: {
      totalQueries: number;
      successfulQueries: number;
      failedQueries: number;
      averageExecutionTime: number;
      lastActivity: string;
    };
  };
}

export interface ListSessionsResponse extends PaginatedApiResponse {
  data: SessionResponse['session'][];
}

// History API Types
export interface QueryHistoryFilter {
  sessionId?: string;
  userId?: string;
  status?: 'pending' | 'running' | 'completed' | 'error' | 'cancelled' | 'timeout';
  fromDate?: string;
  toDate?: string;
  limit?: number;
  offset?: number;
}

export interface QueryHistoryEntry {
  queryId: string;
  query: string;
  sessionId: string;
  userId?: string;
  status: 'pending' | 'running' | 'completed' | 'error' | 'cancelled' | 'timeout';
  results?: any[];
  error?: string;
  executionTime: number;
  createdAt: string;
  completedAt?: string;
  metadata?: Record<string, any>;
}

export interface QueryHistoryResponse extends PaginatedApiResponse {
  data: QueryHistoryEntry[];
}

// Status and Health API Types
export interface SystemStatusResponse extends ApiResponse {
  data: {
    backend: {
      running: boolean;
      version?: string;
      uptime: number;
      activeQueries: number;
      activeSessions: number;
      resourceUsage: {
        memoryUsage: number;
        cpuUsage: number;
        diskUsage?: number;
      };
    };
    apiServer: {
      running: boolean;
      port: number;
      connectedClients: number;
      requestCount: number;
      errorCount: number;
    };
    webSocketServer: {
      running: boolean;
      port: number;
      connectedClients: number;
      messageCount: number;
    };
    scheduler: {
      scheduledQueries: number;
      completedQueries: number;
      failedQueries: number;
    };
  };
}

export interface HealthCheckResponse extends ApiResponse {
  data: {
    status: 'healthy' | 'degraded' | 'unhealthy';
    checks: {
      backend: 'pass' | 'fail';
      database: 'pass' | 'fail';
      memory: 'pass' | 'warn' | 'fail';
      disk: 'pass' | 'warn' | 'fail';
    };
    uptime: number;
    version: string;
  };
}

// Advanced Reasoning API Types
export interface CLPRequest {
  domain: 'fd' | 'r' | 'q';
  variables: string[];
  constraints: string[];
  options?: {
    labelingStrategy?: 'leftmost' | 'ff' | 'ffc' | 'min' | 'max';
    optimization?: {
      objective: 'minimize' | 'maximize';
      variable: string;
    };
  };
}

export interface CLPResponse extends ApiResponse {
  data: {
    success: boolean;
    domain: 'fd' | 'r' | 'q';
    solution: Array<{
      variable: string;
      value: any;
      domain?: string;
    }>;
    optimizationResult?: {
      optimalValue: number;
      isOptimal: boolean;
    };
    statistics?: {
      choicePoints: number;
      inferences: number;
      executionTime: number;
    };
  };
}

export interface ProbabilisticRequest {
  facts: Array<{
    fact: string;
    probability: number;
  }>;
  query: string;
  samples?: number;
  method?: 'monte_carlo' | 'exact' | 'approximate';
}

export interface ProbabilisticResponse extends ApiResponse {
  data: {
    success: boolean;
    query: string;
    probability: number;
    confidenceInterval?: [number, number];
    evidence: {
      successCount: number;
      totalSamples: number;
      method: string;
    };
    statistics?: {
      executionTime: number;
      convergence: boolean;
    };
  };
}

export interface N3Request {
  rules?: string;
  data?: string;
  query: string;
  format?: 'turtle' | 'n3' | 'rdf_xml';
}

export interface N3Response extends ApiResponse {
  data: {
    success: boolean;
    query: string;
    results: any[];
    inferredTriples?: Array<{
      subject: string;
      predicate: string;
      object: string;
    }>;
    count: number;
    proofTrace?: {
      goal: string;
      type: 'proof' | 'fact' | 'inference' | 'builtin';
      subproofs?: any[];
      fact?: string;
      inference?: string;
      builtin?: string;
    };
  };
}

// Middleware Types
export type ApiMiddleware = (
  req: AuthenticatedRequest,
  res: Response,
  next: () => void
) => void | Promise<void>;

export interface RateLimitInfo {
  limit: number;
  remaining: number;
  resetTime: number;
  retryAfter?: number;
}

export interface RequestContext {
  requestId: string;
  startTime: number;
  user?: AuthenticatedUser;
  quota?: UserQuota;
  rateLimitInfo?: RateLimitInfo;
}

// Error Types
export interface ApiError extends Error {
  statusCode: number;
  code?: string;
  details?: Record<string, any>;
  isOperational?: boolean;
}

export class ValidationError extends Error implements ApiError {
  statusCode = 400;
  code = 'VALIDATION_ERROR';

  constructor(
    message: string,
    public field?: string,
    public value?: any,
    public constraint?: string
  ) {
    super(message);
    this.name = 'ValidationError';
  }
}

export class AuthenticationError extends Error implements ApiError {
  statusCode = 401;
  code = 'AUTHENTICATION_ERROR';

  constructor(message: string = 'Authentication required') {
    super(message);
    this.name = 'AuthenticationError';
  }
}

export class AuthorizationError extends Error implements ApiError {
  statusCode = 403;
  code = 'AUTHORIZATION_ERROR';

  constructor(message: string = 'Insufficient permissions') {
    super(message);
    this.name = 'AuthorizationError';
  }
}

export class RateLimitError extends Error implements ApiError {
  statusCode = 429;
  code = 'RATE_LIMIT_ERROR';

  constructor(
    message: string = 'Rate limit exceeded',
    public retryAfter?: number
  ) {
    super(message);
    this.name = 'RateLimitError';
  }
}

export class ResourceNotFoundError extends Error implements ApiError {
  statusCode = 404;
  code = 'RESOURCE_NOT_FOUND';

  constructor(resource: string, id?: string) {
    super(`${resource}${id ? ` with id '${id}'` : ''} not found`);
    this.name = 'ResourceNotFoundError';
  }
}

export class InternalServerError extends Error implements ApiError {
  statusCode = 500;
  code = 'INTERNAL_SERVER_ERROR';

  constructor(message: string = 'Internal server error') {
    super(message);
    this.name = 'InternalServerError';
  }
}
