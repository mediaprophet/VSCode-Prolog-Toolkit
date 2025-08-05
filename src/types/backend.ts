/**
 * Backend-related type definitions for PrologBackend and related functionality
 */

import { EventEmitter } from 'events';

// PrologBackend Options and Configuration
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
}

// Query and Request Types
export interface PrologRequest {
  id: string;
  cmd: string;
  params?: Record<string, any>;
  protocol: number;
  timeoutMs?: number;
}

export interface PrologResponse<T = any> {
  id: string;
  status: 'ok' | 'error' | 'timeout';
  data?: T;
  error?: string;
  message?: string;
  results?: any[];
  output?: string;
  version?: string;
  streaming_info?: StreamingInfo;
  total_chunks?: number;
  chunk_info?: ChunkInfo;
}

export interface StreamingInfo {
  total_count: number;
  chunk_size: number;
  is_large_result: boolean;
  has_more: boolean;
  next_offset?: number;
}

export interface ChunkInfo {
  chunk_index: number;
  chunk_size: number;
  total_results: number;
  is_first: boolean;
  is_last: boolean;
}

// Query Management Types
export interface QueryNotificationOptions {
  enableWebSocket?: boolean;
  webSocketPort?: number;
  enableProgressUpdates?: boolean;
  progressUpdateInterval?: number;
  enableBatchNotifications?: boolean;
  maxConcurrentQueries?: number;
}

export interface QueryHistoryOptions {
  storageDir: string;
  maxEntries?: number;
  enablePersistence?: boolean;
  compressionEnabled?: boolean;
  retentionDays?: number;
}

export interface SessionManagerOptions {
  storageDir: string;
  maxSessions?: number;
  defaultResourceQuota?: ResourceQuota;
  enablePersistence?: boolean;
  autoSaveInterval?: number;
  maxSnapshotsPerSession?: number;
}

// Resource Management Types
export interface ResourceQuota {
  maxConcurrentQueries: number;
  maxSessions: number;
  maxQueryTime: number;
  maxMemoryPerQuery: number;
  maxResultsPerQuery: number;
  rateLimitPerMinute: number;
}

export interface ResourceUsage {
  currentQueries: number;
  currentSessions: number;
  memoryUsage: number;
  cpuUsage: number;
  diskUsage?: number;
}

// Query Priority and Scheduling Types
export interface QueryPriority {
  level: 'low' | 'medium' | 'high' | 'critical';
  weight: number;
  deadline?: Date;
  userId?: string;
  sessionId?: string;
}

export interface QueuedQuery {
  id: string;
  cmd: string;
  params: Record<string, any>;
  priority: QueryPriority;
  queuedAt: number;
  resourceRequirements?: {
    memoryMB?: number;
    cpuPercent?: number;
  };
  resolve: (value: unknown) => void;
  reject: (reason?: any) => void;
}

export interface ScheduledQuery {
  id: string;
  cmd: string;
  params: Record<string, any>;
  scheduleType: 'immediate' | 'delayed' | 'recurring' | 'conditional';
  scheduleConfig: {
    executeAt?: number;
    interval?: number;
    maxExecutions?: number;
    condition?: string;
    dependencies?: string[];
  };
  priority: QueryPriority;
  status: 'pending' | 'running' | 'completed' | 'failed' | 'cancelled';
  createdAt: number;
  lastExecutedAt?: number;
  nextExecutionAt?: number;
  executionCount: number;
  metadata?: {
    tags?: string[];
    description?: string;
    createdBy?: string;
  };
}

// Query Status and Tracking Types
export interface QueryStatus {
  id: string;
  status: 'pending' | 'running' | 'completed' | 'error' | 'cancelled' | 'timeout';
  startTime: number;
  endTime?: number;
  progress?: number;
  message?: string;
  results?: any[];
  error?: string;
  executionTime?: number;
  isBatch: boolean;
  batchIndex?: number;
  totalBatchSize?: number;
}

export interface QueryStatistics {
  totalQueries: number;
  successfulQueries: number;
  failedQueries: number;
  averageExecutionTime: number;
  totalExecutionTime: number;
  activeQueries: number;
  queuedQueries: number;
  lastQueryTime?: number;
}

export interface SchedulerStats {
  totalScheduled: number;
  activeScheduled: number;
  completedScheduled: number;
  failedScheduled: number;
  recurringQueries: number;
  conditionalQueries: number;
  dependentQueries: number;
  nextExecutionTime?: number;
}

// Session Management Types
export interface SessionConfig {
  id: string;
  name: string;
  description?: string;
  userId?: string;
  agentId?: string;
  createdAt: number;
  lastAccessedAt: number;
  isActive: boolean;
  metadata?: Record<string, any>;
  resourceQuota: ResourceQuota;
  persistenceEnabled: boolean;
  autoSave: boolean;
  maxIdleTime: number;
}

export interface SessionState {
  timestamp: number;
  sessionId: string;
  prologFacts: string[];
  prologRules: string[];
  loadedFiles: string[];
  consultedModules: string[];
  rdfTriples: Array<{
    subject: string;
    predicate: string;
    object: string;
    graph?: string;
  }>;
  variables: Record<string, any>;
  queryHistory: string[];
  customData: Record<string, any>;
}

export interface SessionSnapshot {
  sessionId: string;
  snapshotId: string;
  name: string;
  description?: string;
  createdAt: number;
  state: SessionState;
  metadata?: Record<string, any>;
}

// Event Types for Backend
export interface BackendEventMap {
  ready: () => void;
  started: () => void;
  stopped: () => void;
  restarted: () => void;
  error: (error: Error) => void;
  exit: (code: number | null, signal: NodeJS.Signals | null) => void;
  queryCompleted: (event: { queryId: string; result: any; executionTime: number }) => void;
  queryHistoryAdded: (entry: any) => void;
  queryScheduleCompleted: (query: ScheduledQuery) => void;
  queryScheduleExecutionStarted: (query: ScheduledQuery) => void;
  resourceUsageUpdated: (usage: ResourceUsage) => void;
  sessionCreated: (event: { sessionId: string; config: SessionConfig }) => void;
  sessionSwitched: (event: { fromSessionId?: string; toSessionId: string }) => void;
  sessionDeleted: (event: { sessionId: string }) => void;
  sessionStateSaved: (event: { sessionId: string; snapshotId?: string }) => void;
  sessionStateRestored: (event: { sessionId: string; snapshotId?: string }) => void;
}

// Callback Types
export type QueryCallback = (status: QueryStatus) => void;
export type StreamingCallback = (chunk: any, isFirst: boolean, isLast: boolean) => void;
export type ProgressCallback = (progress: number, message?: string) => void;

// Backend Interface
export interface IPrologBackend extends EventEmitter {
  isRunning(): boolean;
  start(): void;
  stop(intentional?: boolean): void;
  restart(): void;
  
  // Basic request methods
  sendRequest(cmd: string, params?: Record<string, any>): Promise<PrologResponse>;
  sendRequest(batch: Array<{cmd: string, params?: Record<string, any>, timeoutMs?: number}>): Promise<PrologResponse[]>;
  
  // Advanced request methods
  sendRequestWithNotifications(
    cmdOrBatch: string | Array<{cmd: string, params?: Record<string, any>, timeoutMs?: number}>,
    params?: Record<string, any>,
    callback?: QueryCallback
  ): Promise<PrologResponse | PrologResponse[]>;
  
  sendStreamingRequest(
    cmd: string,
    params?: Record<string, any>,
    onChunk?: StreamingCallback
  ): Promise<PrologResponse>;
  
  sendRequestWithConcurrency(
    cmd: string,
    params?: Record<string, any>,
    priority?: Partial<QueryPriority>,
    resourceRequirements?: { memoryMB?: number; cpuPercent?: number }
  ): Promise<PrologResponse>;
  
  // Query management
  cancelQuery(queryId: string): boolean;
  getQueryStatus(queryId: string): QueryStatus | undefined;
  getActiveQueries(): QueryStatus[];
  getQueryStatistics(): QueryStatistics;
  
  // Scheduling
  scheduleQuery(
    cmd: string,
    params?: Record<string, any>,
    scheduleType?: 'immediate' | 'delayed' | 'recurring' | 'conditional',
    scheduleConfig?: any,
    priority?: Partial<QueryPriority>,
    metadata?: any
  ): Promise<string>;
  
  cancelScheduledQuery(queryId: string): Promise<boolean>;
  getScheduledQueries(filter?: any): ScheduledQuery[];
  getSchedulerStatistics(): SchedulerStats;
  
  // History
  getQueryHistory(filter?: any): Promise<any>;
  getQueryHistoryStatistics(): Promise<any>;
  clearQueryHistory(): Promise<void>;
  
  // Sessions
  createSession(name: string, options?: any): Promise<string>;
  switchToSession(sessionId: string): Promise<void>;
  getCurrentSession(): { sessionId: string; config: SessionConfig; state: SessionState } | null;
  listSessions(filter?: any): Array<{ sessionId: string; config: SessionConfig }>;
  deleteSession(sessionId: string): Promise<boolean>;
  
  // Configuration
  getStreamingConfig(): { enabled: boolean; maxResultsPerChunk: number };
  updateStreamingConfig(config: { enabled?: boolean; maxResultsPerChunk?: number }): void;
  updateResourceQuota(newQuota: Partial<ResourceQuota>): void;
  getConcurrencyStatus(): any;
}