// Orchestrator for Query History Management
// Composes storage, logic, and event modules for robust, testable query history management

import { EventEmitter } from 'events';
import { QueryHistoryOptions } from '../queryHistoryManager';
import { QueryHistoryEvents } from './QueryHistoryEvents';
import { QueryHistoryLogic } from './QueryHistoryLogic';
import { QueryHistoryStorage } from './QueryHistoryStorage';

export class QueryHistoryOrchestrator extends EventEmitter {
  private storage: QueryHistoryStorage;
  private logic: QueryHistoryLogic;
  private events: QueryHistoryEvents;

  constructor(options: Partial<QueryHistoryOptions> = {}) {
    super();
    this.storage = new QueryHistoryStorage(options);
    this.logic = new QueryHistoryLogic(this.storage, options);
    this.events = new QueryHistoryEvents(this.logic, this.storage);
    this.registerEventHandlers();
  }

  private registerEventHandlers() {
    this.events.on('queryAdded', (data: unknown) => this.emit('queryAdded', data));
    this.events.on('queryUpdated', (data: unknown) => this.emit('queryUpdated', data));
    this.events.on('error', (err: unknown) => this.emit('error', err));
    // ...add more as needed
  }


  /**
   * Add a query to history. Entry metadata may include:
   *   source: 'ui' | 'api' | 'automation'
   *   resourceUsage: { cpuPercent?: number; memoryMB?: number; durationMs?: number }
   *   artifacts: Array<{ name: string; path: string; type?: string }>
   */
  async addQuery(entry: any): Promise<void> {
    return this.logic.addQuery(entry);
  }

  async updateQuery(queryId: string, updates: any): Promise<void> {
    return this.logic.updateQuery(queryId, updates);
  }

  async getHistory(filter: any = {}): Promise<any> {
    return this.logic.getHistory(filter);
  }

  // Tag management
  async addTags(queryId: string, tags: string[]): Promise<void> {
    return this.logic.addTags(queryId, tags);
  }

  async removeTags(queryId: string, tags: string[]): Promise<void> {
    return this.logic.removeTags(queryId, tags);
  }

  async getAllTags(): Promise<string[]> {
    return this.logic.getAllTags();
  }

  // Analytics/statistics
  async getStatistics(): Promise<any> {
    return this.logic.getStatistics();
  }

  async emitAnalyticsUpdate(): Promise<void> {
    return this.logic.emitAnalyticsUpdate();
  }
}
