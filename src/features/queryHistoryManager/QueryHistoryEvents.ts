// Handles all event emission and subscription for query history management
import { EventEmitter } from 'events';
import { QueryHistoryLogic } from './QueryHistoryLogic';
import { QueryHistoryStorage } from './QueryHistoryStorage';

export class QueryHistoryEvents extends EventEmitter {
  constructor(private logic: QueryHistoryLogic, private storage: QueryHistoryStorage) {
    super();
    // Listen to logic events
    if (typeof (logic as any).on === 'function') {
      (logic as any).on('queryAdded', (data: unknown) => this.emit('queryAdded', data));
      (logic as any).on('queryUpdated', (data: unknown) => this.emit('queryUpdated', data));
      (logic as any).on('error', (err: unknown) => this.emit('error', err));
    }
    // Listen to storage events
    if (typeof (storage as any).on === 'function') {
      (storage as any).on('historyLoaded', (data: unknown) => this.emit('historyLoaded', data));
    }
  }

  /**
   * Subscribe to a query history event
   * @param event Event name
   * @param listener Listener function
   */
  onEvent(event: string, listener: (...args: any[]) => void): void {
    this.on(event, listener);
  }

  /**
   * Emit a custom query history event
   * @param event Event name
   * @param args Arguments
   */
  emitEvent(event: string, ...args: any[]): void {
    this.emit(event, ...args);
  }
}
