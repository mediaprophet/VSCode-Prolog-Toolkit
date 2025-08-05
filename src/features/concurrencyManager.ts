import { EventEmitter } from 'events';
import { QueryStatus } from './queryNotificationManager';

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
export class ConcurrencyManager extends EventEmitter {
  private resourceQuota: ResourceQuota;
  private queryQueue: QueuedQuery[] = [];
  private activeQueries: Map<string, QueuedQuery> = new Map();
  private resourceUsage: ResourceUsage;
  private processingInterval?: NodeJS.Timeout;
  private resourceMonitorInterval?: NodeJS.Timeout;

  constructor(quota: Partial<ResourceQuota> = {}) {
    super();
    
    this.resourceQuota = {
      maxConcurrentQueries: 10,
      maxMemoryUsageMB: 512,
      maxCpuUsagePercent: 80,
      maxQueryDurationMs: 30000,
      maxQueueSize: 100,
      ...quota
    };

    this.resourceUsage = {
      activeConcurrentQueries: 0,
      memoryUsageMB: 0,
      cpuUsagePercent: 0,
      queueSize: 0,
      lastUpdated: Date.now()
    };

    this.startProcessing();
    this.startResourceMonitoring();
  }

  /**
   * Queue a query with priority and resource requirements
   */
  async queueQuery(
    id: string,
    cmd: string,
    params: Record<string, unknown> = {},
    priority: Partial<QueryPriority> = {},
    resourceRequirements?: { memoryMB?: number; cpuPercent?: number }
  ): Promise<unknown> {
    return new Promise((resolve, reject) => {
      // Check queue size limit
      if (this.queryQueue.length >= this.resourceQuota.maxQueueSize) {
        reject(new Error('Query queue is full. Please try again later.'));
        return;
      }

      const queryPriority: QueryPriority = {
        level: 'normal',
        weight: this.getPriorityWeight(priority.level || 'normal'),
        timeout: priority.timeout || this.resourceQuota.maxQueryDurationMs,
        ...priority
      };

      const queuedQuery: QueuedQuery = {
        id,
        cmd,
        params,
        priority: queryPriority,
        queuedAt: Date.now(),
        ...(resourceRequirements && { resourceRequirements }),
        resolve,
        reject
      };

      // Insert query in priority order
      this.insertQueryByPriority(queuedQuery);
      this.updateResourceUsage();

      this.emit('queryQueued', {
        queryId: id,
        priority: queryPriority.level,
        queuePosition: this.queryQueue.findIndex(q => q.id === id) + 1,
        queueSize: this.queryQueue.length
      });

      console.log(`[ConcurrencyManager] Queued query ${id} with priority ${queryPriority.level} (queue size: ${this.queryQueue.length})`);
    });
  }

  /**
   * Cancel a queued or active query
   */
  cancelQuery(queryId: string): boolean {
    // Check if query is in queue
    const queueIndex = this.queryQueue.findIndex(q => q.id === queryId);
    if (queueIndex !== -1) {
      const query = this.queryQueue.splice(queueIndex, 1)[0];
      if (query) {
        query.reject(new Error('Query cancelled'));
        this.updateResourceUsage();
        this.emit('queryCancelled', { queryId, location: 'queue' });
        return true;
      }
    }

    // Check if query is active
    const activeQuery = this.activeQueries.get(queryId);
    if (activeQuery) {
      this.activeQueries.delete(queryId);
      activeQuery.reject(new Error('Query cancelled'));
      this.updateResourceUsage();
      this.emit('queryCancelled', { queryId, location: 'active' });
      return true;
    }

    return false;
  }

  /**
   * Get current resource usage and queue status
   */
  getStatus(): {
    resourceUsage: ResourceUsage;
    resourceQuota: ResourceQuota;
    queuedQueries: Array<{
      id: string;
      priority: string;
      queuedAt: number;
      waitTime: number;
    }>;
    activeQueries: Array<{
      id: string;
      startedAt: number;
      duration: number;
    }>;
  } {
    const now = Date.now();
    return {
      resourceUsage: this.resourceUsage,
      resourceQuota: this.resourceQuota,
      queuedQueries: this.queryQueue.map(q => ({
        id: q.id,
        priority: q.priority.level,
        queuedAt: q.queuedAt,
        waitTime: now - q.queuedAt
      })),
      activeQueries: Array.from(this.activeQueries.values()).map(q => ({
        id: q.id,
        startedAt: q.queuedAt,
        duration: now - q.queuedAt
      }))
    };
  }

  /**
   * Update resource quota configuration
   */
  updateResourceQuota(newQuota: Partial<ResourceQuota>): void {
    this.resourceQuota = { ...this.resourceQuota, ...newQuota };
    this.emit('resourceQuotaUpdated', this.resourceQuota);
    console.log('[ConcurrencyManager] Resource quota updated:', this.resourceQuota);
  }

  /**
   * Get priority weight for sorting
   */
  private getPriorityWeight(level: string): number {
    switch (level) {
      case 'critical': return 1000;
      case 'high': return 100;
      case 'normal': return 10;
      case 'low': return 1;
      default: return 10;
    }
  }

  /**
   * Insert query in priority order (higher priority first, then FIFO within same priority)
   */
  private insertQueryByPriority(query: QueuedQuery): void {
    let insertIndex = this.queryQueue.length;
    
    for (let i = 0; i < this.queryQueue.length; i++) {
      const existingQuery = this.queryQueue[i];
      
      // Higher priority goes first
      if (existingQuery && query.priority.weight > existingQuery.priority.weight) {
        insertIndex = i;
        break;
      }
      
      // Same priority, maintain FIFO order (already at end)
      if (existingQuery && query.priority.weight === existingQuery.priority.weight) {
        continue;
      }
    }
    
    this.queryQueue.splice(insertIndex, 0, query);
  }

  /**
   * Check if resources are available for a query
   */
  private canExecuteQuery(query: QueuedQuery): boolean {
    // Check concurrent query limit
    if (this.activeQueries.size >= this.resourceQuota.maxConcurrentQueries) {
      return false;
    }

    // Check memory requirements
    if (query.resourceRequirements?.memoryMB) {
      const projectedMemory = this.resourceUsage.memoryUsageMB + query.resourceRequirements.memoryMB;
      if (projectedMemory > this.resourceQuota.maxMemoryUsageMB) {
        return false;
      }
    }

    // Check CPU requirements
    if (query.resourceRequirements?.cpuPercent) {
      const projectedCpu = this.resourceUsage.cpuUsagePercent + query.resourceRequirements.cpuPercent;
      if (projectedCpu > this.resourceQuota.maxCpuUsagePercent) {
        return false;
      }
    }

    return true;
  }

  /**
   * Process the query queue
   */
  private startProcessing(): void {
    this.processingInterval = setInterval(() => {
      this.processQueue();
    }, 100); // Check every 100ms
  }

  /**
   * Process queued queries based on priority and resource availability
   */
  private processQueue(): void {
    if (this.queryQueue.length === 0) {
      return;
    }

    // Process queries in priority order
    for (let i = 0; i < this.queryQueue.length; i++) {
      const query = this.queryQueue[i];
      if (!query) {
        continue;
      }
      
      // Check if query has timed out in queue
      const waitTime = Date.now() - query.queuedAt;
      if (waitTime > query.priority.timeout) {
        this.queryQueue.splice(i, 1);
        query.reject(new Error('Query timed out in queue'));
        this.emit('queryTimeout', { queryId: query.id, location: 'queue', waitTime });
        i--; // Adjust index after removal
        continue;
      }

      // Check if resources are available
      if (this.canExecuteQuery(query)) {
        // Remove from queue and add to active queries
        this.queryQueue.splice(i, 1);
        this.activeQueries.set(query.id, query);
        
        // Execute the query
        this.executeQuery(query);
        this.updateResourceUsage();
        
        i--; // Adjust index after removal
      }
    }
  }

  /**
   * Execute a query
   */
  private async executeQuery(query: QueuedQuery): Promise<void> {
    const startTime = Date.now();
    
    this.emit('queryStarted', {
      queryId: query.id,
      priority: query.priority.level,
      waitTime: startTime - query.queuedAt
    });

    try {
      // Set up timeout for the query execution
      const timeoutPromise = new Promise((_, reject) => {
        setTimeout(() => {
          reject(new Error('Query execution timeout'));
        }, query.priority.timeout);
      });

      // This would be replaced with actual query execution
      // For now, we'll emit an event that the backend can listen to
      const executionPromise = new Promise((resolve, reject) => {
        this.emit('executeQuery', {
          query,
          resolve,
          reject
        });
      });

      const result = await Promise.race([executionPromise, timeoutPromise]);
      
      // Query completed successfully
      this.activeQueries.delete(query.id);
      query.resolve(result);
      
      const duration = Date.now() - startTime;
      this.emit('queryCompleted', {
        queryId: query.id,
        duration,
        success: true
      });

    } catch (error: unknown) {
      // Query failed or timed out
      this.activeQueries.delete(query.id);
      query.reject(error);
      
      const duration = Date.now() - startTime;
      const errorMessage = error instanceof Error ? error.message : 'Unknown error';
      this.emit('queryCompleted', {
        queryId: query.id,
        duration,
        success: false,
        error: errorMessage
      });
    }

    this.updateResourceUsage();
  }

  /**
   * Monitor system resources
   */
  private startResourceMonitoring(): void {
    this.resourceMonitorInterval = setInterval(() => {
      this.updateResourceUsage();
    }, 1000); // Update every second
  }

  /**
   * Update current resource usage
   */
  private updateResourceUsage(): void {
    this.resourceUsage = {
      activeConcurrentQueries: this.activeQueries.size,
      memoryUsageMB: this.estimateMemoryUsage(),
      cpuUsagePercent: this.estimateCpuUsage(),
      queueSize: this.queryQueue.length,
      lastUpdated: Date.now()
    };

    this.emit('resourceUsageUpdated', this.resourceUsage);
  }

  /**
   * Estimate memory usage (simplified)
   */
  private estimateMemoryUsage(): number {
    // Simple estimation based on active queries
    // In a real implementation, this would use actual system metrics
    const baseMemory = 50; // Base memory usage in MB
    const perQueryMemory = 10; // Estimated memory per query in MB
    return baseMemory + (this.activeQueries.size * perQueryMemory);
  }

  /**
   * Estimate CPU usage (simplified)
   */
  private estimateCpuUsage(): number {
    // Simple estimation based on active queries
    // In a real implementation, this would use actual system metrics
    const perQueryCpu = 15; // Estimated CPU per query in percent
    return Math.min(100, this.activeQueries.size * perQueryCpu);
  }

  /**
   * Clean up and stop processing
   */
  dispose(): void {
    if (this.processingInterval) {
      clearInterval(this.processingInterval);
    }
    if (this.resourceMonitorInterval) {
      clearInterval(this.resourceMonitorInterval);
    }

    // Cancel all queued queries
    this.queryQueue.forEach(query => {
      query.reject(new Error('Concurrency manager disposed'));
    });
    this.queryQueue = [];

    // Cancel all active queries
    this.activeQueries.forEach(query => {
      query.reject(new Error('Concurrency manager disposed'));
    });
    this.activeQueries.clear();

    this.removeAllListeners();
  }
}