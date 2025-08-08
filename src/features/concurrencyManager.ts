
export interface ResourceQuota {
  maxConcurrentQueries: number;
  maxMemoryUsageMB: number;
  maxCpuUsagePercent: number;
  maxQueryDurationMs: number;
  maxQueueSize: number;
}

export interface QueryPriority {
  level: 'low' | 'normal' | 'high' | 'critical';
  weight: number;
  timeout: number;
}

export interface QueuedQuery {
  id: string;
  cmd: string;
  params: Record<string, unknown>;
  priority: QueryPriority;
  queuedAt: number;
  estimatedDuration?: number;
  resourceRequirements?: {
    memoryMB?: number;
    cpuPercent?: number;
  };
  resolve: (value: unknown) => void;
  reject: (error: unknown) => void;
}

export interface ResourceUsage {
  activeConcurrentQueries: number;
  memoryUsageMB: number;
  cpuUsagePercent: number;
  queueSize: number;
  lastUpdated: number;
}

/**
 * Advanced concurrency manager with resource quotas and priority queues
 */

/**
 * DEPRECATED: All logic has been migrated to modular orchestrator and modules.
 * This file now only exports types and interfaces for backward compatibility.
 * Use ConcurrencyManagerOrchestrator and related modules instead.
 */
