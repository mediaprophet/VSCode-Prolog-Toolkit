// Orchestrator for Concurrency Management
// Composes resource, queue, and event modules for robust, testable concurrency management

import { EventEmitter } from 'events';
import { ResourceQuota } from '../concurrencyManager';

import { ConcurrencyEvents } from './ConcurrencyEvents';
import { ConcurrencyLogic } from './ConcurrencyLogic';
import { ConcurrencyQueue } from './ConcurrencyQueue';
import { ConcurrencyResource } from './ConcurrencyResource';

export class ConcurrencyManagerOrchestrator extends EventEmitter {
  private resource: ConcurrencyResource;
  private queue: ConcurrencyQueue;
  private logic: ConcurrencyLogic;
  private events: ConcurrencyEvents;

  constructor(quota: Partial<ResourceQuota> = {}) {
    super();
    this.resource = new ConcurrencyResource(quota);
    this.queue = new ConcurrencyQueue(this.resource);
    this.logic = new ConcurrencyLogic(this.resource, this.queue);
    this.events = new ConcurrencyEvents(this.queue, this.resource);
    this.registerEventHandlers();
  }

  private registerEventHandlers() {
    this.events.on('queryQueued', (data: unknown) => this.emit('queryQueued', data));
    this.events.on('queryStarted', (data: unknown) => this.emit('queryStarted', data));
    this.events.on('queryCompleted', (data: unknown) => this.emit('queryCompleted', data));
    this.events.on('error', (err: unknown) => this.emit('error', err));
    // ...add more as needed
  }

  // Expose orchestrated API
  async queueQuery(query: any): Promise<unknown> {
    return this.logic.queueQuery(query);
  }

  cancelQuery(queryId: string): boolean {
    return this.logic.cancelQuery(queryId);
  }

  getStatus() {
    return this.logic.getStatus();
  }

  updateResourceQuota(newQuota: Partial<ResourceQuota>): void {
    this.logic.updateResourceQuota(newQuota);
  }

  // ...add more methods as needed
}
