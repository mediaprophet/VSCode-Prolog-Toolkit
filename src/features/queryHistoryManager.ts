
export interface QueryHistoryEntry {
  id: string;
  cmd: string;
  params: Record<string, unknown>;
  status: 'pending' | 'running' | 'completed' | 'error' | 'cancelled' | 'timeout';
  startTime: number;
  endTime?: number;
  duration?: number;
  results?: unknown;
  error?: unknown;
  priority?: string;
  deleted?: boolean;
  resourceUsage?: {
    memoryMB?: number;
    cpuPercent?: number;
  };
  metadata?: {
    userAgent?: string;
    sessionId?: string;
    workspaceId?: string;
    tags?: string[];
  };
}

export interface QueryHistoryFilter {
  status?: string[];
  startDate?: Date;
  endDate?: Date;
  cmd?: string;
  priority?: string[];
  tags?: string[];
  limit?: number;
  offset?: number;
  sortBy?: 'startTime' | 'endTime' | 'duration' | 'cmd';
  sortOrder?: 'asc' | 'desc';
}

export interface QueryHistoryStats {
  totalQueries: number;
  completedQueries: number;
  errorQueries: number;
  cancelledQueries: number;
  timeoutQueries: number;
  averageDuration: number;
  totalDuration: number;
  queriesByStatus: Record<string, number>;
  queriesByPriority: Record<string, number>;
  queriesByCmd: Record<string, number>;
  dailyStats: Array<{
    date: string;
    count: number;
    avgDuration: number;
  }>;
}

export interface QueryHistoryOptions {
  storageDir: string;
  maxHistorySize: number;
  compressionEnabled: boolean;
  retentionDays: number;
  autoCleanup: boolean;
  batchSize: number;
}

/**
 * Persistent query history storage and management system
 */

/**
 * DEPRECATED: All logic has been migrated to modular orchestrator and modules.
 * This file now only exports types and interfaces for backward compatibility.
 * Use QueryHistoryOrchestrator and related modules instead.
 */
