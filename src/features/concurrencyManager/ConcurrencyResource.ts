import { EventEmitter } from 'events';

export interface ResourceUsage {
  activeConcurrentQueries: number;
  memoryUsageMB: number;
  cpuUsagePercent: number;
  queueSize: number;
  lastUpdated: number;
}

export interface ResourceQuota {
  maxConcurrentQueries: number;
  maxMemoryUsageMB: number;
  maxCpuUsagePercent: number;
  maxQueryDurationMs: number;
  maxQueueSize: number;
}

export class ConcurrencyResource extends EventEmitter {
  private usage: ResourceUsage = {
    activeConcurrentQueries: 0,
    memoryUsageMB: 0,
    cpuUsagePercent: 0,
    queueSize: 0,
    lastUpdated: Date.now(),
  };

  constructor(private quota: Partial<ResourceQuota> = {}) {
    super();
  }

  /**
   * Dynamically scale resource quotas based on observed usage or external signals.
   * Example: If usage is consistently high, increase quotas; if low, decrease.
   * Accepts a scaling policy callback or uses a default heuristic.
   */
  autoScaleQuota(policy?: (usage: ResourceUsage, quota: Partial<ResourceQuota>) => Partial<ResourceQuota>): void {
    let newQuota: Partial<ResourceQuota>;
    if (policy) {
      newQuota = policy(this.usage, this.quota);
    } else {
      // Default: If >80% usage, increase quotas by 20%; if <30%, decrease by 10%
      newQuota = { ...this.quota };
      const q = this.quota;
      if (q.maxConcurrentQueries && q.maxConcurrentQueries > 0) {
        const ratio = this.usage.activeConcurrentQueries / q.maxConcurrentQueries;
        if (ratio > 0.8) {
          newQuota.maxConcurrentQueries = Math.ceil(q.maxConcurrentQueries * 1.2);
        } else if (ratio < 0.3) {
          newQuota.maxConcurrentQueries = Math.max(1, Math.floor(q.maxConcurrentQueries * 0.9));
        }
      }
      if (q.maxMemoryUsageMB && q.maxMemoryUsageMB > 0) {
        const ratio = this.usage.memoryUsageMB / q.maxMemoryUsageMB;
        if (ratio > 0.8) {
          newQuota.maxMemoryUsageMB = Math.ceil(q.maxMemoryUsageMB * 1.2);
        } else if (ratio < 0.3) {
          newQuota.maxMemoryUsageMB = Math.max(32, Math.floor(q.maxMemoryUsageMB * 0.9));
        }
      }
      if (q.maxCpuUsagePercent && q.maxCpuUsagePercent > 0) {
        const ratio = this.usage.cpuUsagePercent / q.maxCpuUsagePercent;
        if (ratio > 0.8) {
          newQuota.maxCpuUsagePercent = Math.ceil(q.maxCpuUsagePercent * 1.2);
        } else if (ratio < 0.3) {
          newQuota.maxCpuUsagePercent = Math.max(10, Math.floor(q.maxCpuUsagePercent * 0.9));
        }
      }
    }
    this.updateResourceQuota(newQuota);
    this.emit('quotaScaled', { quota: this.quota });
  }

  getStatus() {
    return {
      ...this.usage,
      quota: this.quota,
    };
  }

  updateResourceQuota(newQuota: Partial<ResourceQuota>): void {
    this.quota = { ...this.quota, ...newQuota };
    this.emit('resourceUpdated', { quota: this.quota });
  }

  incrementUsage({ memoryMB = 0, cpuPercent = 0 }: { memoryMB?: number; cpuPercent?: number } = {}): void {
    this.usage.activeConcurrentQueries++;
    this.usage.memoryUsageMB += memoryMB;
    this.usage.cpuUsagePercent += cpuPercent;
    this.usage.lastUpdated = Date.now();
    this.emit('resourceUpdated', { usage: this.usage });
  }

  decrementUsage({ memoryMB = 0, cpuPercent = 0 }: { memoryMB?: number; cpuPercent?: number } = {}): void {
    this.usage.activeConcurrentQueries = Math.max(0, this.usage.activeConcurrentQueries - 1);
    this.usage.memoryUsageMB = Math.max(0, this.usage.memoryUsageMB - memoryMB);
    this.usage.cpuUsagePercent = Math.max(0, this.usage.cpuUsagePercent - cpuPercent);
    this.usage.lastUpdated = Date.now();
    this.emit('resourceUpdated', { usage: this.usage });
  }

  setQueueSize(size: number): void {
    this.usage.queueSize = size;
    this.usage.lastUpdated = Date.now();
    this.emit('resourceUpdated', { usage: this.usage });
  }

  estimateMemoryUsage(query: any): number {
    // TODO: Use query details to estimate memory usage
    return query?.resourceRequirements?.memoryMB || 10;
  }

  estimateCpuUsage(query: any): number {
    // TODO: Use query details to estimate CPU usage
    return query?.resourceRequirements?.cpuPercent || 5;
  }

  canRunQuery(query: any): boolean {
    const mem = this.estimateMemoryUsage(query);
    const cpu = this.estimateCpuUsage(query);
    return (
      (this.quota.maxConcurrentQueries === undefined || this.usage.activeConcurrentQueries < this.quota.maxConcurrentQueries) &&
      (this.quota.maxMemoryUsageMB === undefined || this.usage.memoryUsageMB + mem <= this.quota.maxMemoryUsageMB) &&
      (this.quota.maxCpuUsagePercent === undefined || this.usage.cpuUsagePercent + cpu <= this.quota.maxCpuUsagePercent)
    );
  }
}
