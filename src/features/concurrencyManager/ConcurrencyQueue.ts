// Handles query queueing, priority, and execution
import { ConcurrencyResource } from './ConcurrencyResource';

import { EventEmitter } from 'events';

interface QueuedQuery {
  id: string;
  cmd: string;
  params: Record<string, unknown>;
  priority: { level: 'low' | 'normal' | 'high' | 'critical'; weight: number; timeout: number };
  queuedAt: number;
  estimatedDuration?: number;
  resourceRequirements?: { memoryMB?: number; cpuPercent?: number };
  resolve: (value: unknown) => void;
  reject: (error: unknown) => void;
}

export class ConcurrencyQueue extends EventEmitter {
  private queue: QueuedQuery[] = [];
  private running: Set<string> = new Set();
  private processing = false;

  constructor(private resource: ConcurrencyResource) {
    super();
  }

  async queueQuery(query: Omit<QueuedQuery, 'resolve' | 'reject'>): Promise<unknown> {
    return new Promise((resolve, reject) => {
      const q: QueuedQuery = { ...query, resolve, reject };
      this.queue.push(q);
      this.queue.sort((a, b) => b.priority.weight - a.priority.weight);
      this.resource.setQueueSize(this.queue.length);
      this.emit('queryQueued', q);
      this.processQueue();
    });
  }

  cancelQuery(queryId: string): boolean {
    const idx = this.queue.findIndex(q => q.id === queryId);
    if (idx !== -1) {
      const [q] = this.queue.splice(idx, 1);
      this.resource.setQueueSize(this.queue.length);
      if (q) {
        q.reject(new Error('Query cancelled'));
        this.emit('queryCancelled', q);
      }
      return true;
    }
    return false;
  }

  private async processQueue() {
    if (this.processing) return;
    this.processing = true;
    try {
      while (this.queue.length > 0) {
        const q = this.queue[0];
        if (!q || !this.resource.canRunQuery(q)) break;
        this.queue.shift();
        this.resource.setQueueSize(this.queue.length);
        this.running.add(q.id);
        this.resource.incrementUsage(q.resourceRequirements);
        this.emit('queryStarted', q);
        try {
          const result = await this.executeQuery(q);
          q.resolve(result);
          this.emit('queryCompleted', { ...q, result });
        } catch (err) {
          q.reject(err);
          this.emit('error', err);
        } finally {
          this.running.delete(q.id);
          this.resource.decrementUsage(q.resourceRequirements);
        }
      }
    } finally {
      this.processing = false;
    }
  }

  private async executeQuery(q: QueuedQuery): Promise<unknown> {
    // TODO: Replace with actual query execution logic
    await new Promise(res => setTimeout(res, q.estimatedDuration || 100));
    return { success: true, id: q.id };
  }
}
