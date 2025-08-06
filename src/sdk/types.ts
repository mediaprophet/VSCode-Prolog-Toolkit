/**
 * TypeScript type definitions for VSCode Prolog Toolkit API SDK
 */

// Core API Types
export interface ApiResponse<T = any> {
  success: boolean;
  data?: T;
  error?: string;
  message?: string;
  timestamp: string;
}

export interface PaginatedResponse<T = any> {
  items: T[];
  total: number;
  limit: number;
  offset: number;
  has_more: boolean;
}

// Query Types
export interface QueryOptions {
  timeout?: number;
  max_results?: number;
  reasoning_mode?: ReasoningMode;
  stream?: boolean;
  session_id?: string;
}

export type ReasoningMode = 'default' | 'clp' | 'probabilistic' | 'n3' | 'custom';

export interface QueryResult {
  query_id: string;
  success: boolean;
  results: VariableBinding[];
  execution_time: number;
  more_available: boolean;
  cursor?: string;
  streaming_info?: StreamingInfo;
  error?: string;
}

export interface VariableBinding {
  [variable: string]: any;
}

export interface StreamingInfo {
  total_count: number;
  chunk_size: number;
  is_large_result: boolean;
  has_more: boolean;
  next_offset?: number;
}

// Batch Processing Types
export interface BatchQueryRequest {
  query: string;
  options?: QueryOptions;
}

export interface BatchOptions {
  parallel?: boolean;
  fail_fast?: boolean;
  timeout?: number;
}

export interface BatchResult {
  batch_id: string;
  results: Array<{
    query_index: number;
    success: boolean;
    results: VariableBinding[];
    error?: string;
    execution_time?: number;
  }>;
  total_queries: number;
  successful_queries: number;
  failed_queries: number;
  total_execution_time: number;
}

// Session Management Types
export interface SessionConfig {
  name: string;
  description?: string;
  userId?: string;
  agentId?: string;
  resourceQuota?: ResourceQuota;
  persistenceEnabled?: boolean;
  autoSave?: boolean;
  metadata?: Record<string, any>;
}

export interface Session {
  session_id: string;
  name: string;
  description?: string;
  created_at: string;
  updated_at?: string;
  is_active: boolean;
  user_id?: string;
  agent_id?: string;
  config: SessionConfig;
  statistics?: SessionStatistics;
}

export interface SessionState {
  session_id: string;
  state: any;
  snapshots?: SessionSnapshot[];
  exported_at: string;
}

export interface SessionSnapshot {
  snapshot_id: string;
  name: string;
  description?: string;
  created_at: string;
  state: any;
}

export interface SessionStatistics {
  total_queries: number;
  successful_queries: number;
  failed_queries: number;
  average_execution_time: number;
  total_execution_time: number;
  memory_usage: number;
  created_at: string;
  last_activity: string;
}

// Resource Management Types
export interface ResourceQuota {
  max_concurrent_queries: number;
  max_sessions: number;
  max_query_time: number;
  max_memory_per_query: number;
  max_results_per_query: number;
  rate_limit_per_minute: number;
}

export interface ResourceUsage {
  current_queries: number;
  current_sessions: number;
  memory_usage: number;
  cpu_usage: number;
  disk_usage?: number;
}

// Advanced Reasoning Types
export interface CLPRequest {
  domain: CLPDomain;
  variables: string[];
  constraints: string[];
  options?: CLPOptions;
}

export type CLPDomain = 'fd' | 'r' | 'q';

export interface CLPOptions {
  labeling_strategy?: 'leftmost' | 'ff' | 'ffc' | 'min' | 'max';
  optimization?: {
    objective: 'minimize' | 'maximize';
    variable: string;
  };
}

export interface CLPResult {
  success: boolean;
  domain: CLPDomain;
  solution: Array<{
    variable: string;
    value: any;
    domain?: string;
  }>;
  optimization_result?: {
    optimal_value: number;
    is_optimal: boolean;
  };
  statistics?: {
    choice_points: number;
    inferences: number;
    execution_time: number;
  };
}

export interface ProbabilisticFact {
  fact: string;
  probability: number;
}

export interface ProbabilisticRequest {
  facts: ProbabilisticFact[];
  query: string;
  samples?: number;
  method?: 'monte_carlo' | 'exact' | 'approximate';
}

export interface ProbabilisticResult {
  success: boolean;
  query: string;
  probability: number;
  confidence_interval?: [number, number];
  evidence: {
    success_count: number;
    total_samples: number;
    method: string;
  };
  statistics?: {
    execution_time: number;
    convergence: boolean;
  };
}

export interface N3Request {
  rules?: string;
  data?: string;
  query: string;
  format?: 'turtle' | 'n3' | 'rdf_xml';
}

export interface N3Result {
  success: boolean;
  query: string;
  results: any[];
  inferred_triples?: N3Triple[];
  count: number;
  proof_trace?: ProofTree;
}

export interface N3Triple {
  subject: string;
  predicate: string;
  object: string;
}

export interface ProofTree {
  goal: string;
  type: 'proof' | 'fact' | 'inference' | 'builtin';
  subproofs?: ProofTree[];
  fact?: string;
  inference?: string;
  builtin?: string;
}

// Query History Types
export interface QueryHistoryEntry {
  query_id: string;
  query: string;
  session_id: string;
  user_id?: string;
  status: QueryStatus;
  results?: VariableBinding[];
  error?: string;
  execution_time: number;
  created_at: string;
  completed_at?: string;
  metadata?: Record<string, any>;
}

export type QueryStatus = 'pending' | 'running' | 'completed' | 'error' | 'cancelled' | 'timeout';

export interface QueryHistoryFilter {
  session_id?: string;
  user_id?: string;
  status?: QueryStatus;
  from_date?: string;
  to_date?: string;
  limit?: number;
  offset?: number;
}

// System Status Types
export interface SystemStatus {
  backend: {
    running: boolean;
    version?: string;
    uptime: number;
    active_queries: number;
    active_sessions: number;
    resource_usage: ResourceUsage;
  };
  api_server: {
    running: boolean;
    port: number;
    connected_clients: number;
    request_count: number;
    error_count: number;
  };
  websocket_server: {
    running: boolean;
    port: number;
    connected_clients: number;
    message_count: number;
  };
  scheduler: {
    scheduled_queries: number;
    completed_queries: number;
    failed_queries: number;
  };
  timestamp: string;
}

// Authentication Types
export interface AuthConfig {
  type: AuthMethod;
  apiKey?: string;
  jwtToken?: string;
  refreshToken?: string;
  expiresAt?: string;
}

export type AuthMethod = 'api_key' | 'jwt_token' | 'oauth2' | 'local_only' | 'none';

export interface User {
  id: string;
  role: UserRole;
  permissions: string[];
  metadata?: Record<string, any>;
}

export type UserRole = 'admin' | 'agent' | 'readonly' | 'limited';

// WebSocket Types
export interface WebSocketMessage {
  type: string;
  timestamp: string;
  [key: string]: any;
}

export interface QueryNotification extends WebSocketMessage {
  type: 'query_progress' | 'query_complete' | 'query_error' | 'query_cancelled';
  query_id: string;
  status: QueryStatus;
  progress?: number;
  message?: string;
  results?: VariableBinding[];
  execution_time?: number;
}

export interface SessionEvent extends WebSocketMessage {
  type: 'session_event';
  session_id: string;
  event: 'created' | 'switched' | 'deleted' | 'state_saved' | 'state_restored';
  user_id?: string;
}

export interface SystemStatusUpdate extends WebSocketMessage {
  type: 'system_status';
  active_queries: number;
  active_sessions: number;
  memory_usage: ResourceUsage;
  cpu_usage: number;
}

// Error Types
export interface ApiError extends Error {
  status?: number;
  code?: string;
  response?: any;
  details?: Record<string, any>;
}

export interface ValidationError extends ApiError {
  field?: string;
  value?: any;
  constraint?: string;
}

// Configuration Types
export interface ClientConfig {
  baseUrl: string;
  timeout?: number;
  retries?: {
    enabled: boolean;
    maxRetries: number;
    retryDelay: number;
    backoffFactor?: number;
  };
  auth?: AuthConfig;
  logging?: {
    enabled: boolean;
    level: 'debug' | 'info' | 'warn' | 'error';
  };
}

export interface WebSocketConfig {
  url: string;
  auth?: AuthConfig;
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

// Utility Types
export type Awaitable<T> = T | Promise<T>;

export type EventCallback<T = any> = (data: T) => void;

export interface EventMap {
  connected: () => void;
  disconnected: (code: number, reason: string) => void;
  error: (error: Error) => void;
  queryProgress: (notification: QueryNotification) => void;
  queryComplete: (notification: QueryNotification) => void;
  sessionEvent: (event: SessionEvent) => void;
  systemStatus: (status: SystemStatusUpdate) => void;
  notification: (message: WebSocketMessage) => void;
}

// Export all types with PrologAPI prefix for convenience
export type PrologAPIQuery = QueryResult;
export type PrologAPIBatch = BatchResult;
export type PrologAPISessionType = Session;
export type PrologAPICLP = CLPResult;
export type PrologAPIProbabilistic = ProbabilisticResult;
export type PrologAPIN3 = N3Result;
export type PrologAPIHistory = QueryHistoryEntry;
export type PrologAPIStatus = SystemStatus;
export type PrologAPIAuth = AuthConfig;
export type PrologAPIWebSocket = WebSocketMessage;
