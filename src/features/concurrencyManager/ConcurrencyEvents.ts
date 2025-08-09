// Handles all event emission and subscription for concurrency management
import { EventEmitter } from 'events';
import { ConcurrencyQueue } from './ConcurrencyQueue';
import { ConcurrencyResource } from './ConcurrencyResource';

export class ConcurrencyEvents extends EventEmitter {
  constructor(private queue: ConcurrencyQueue, private resource: ConcurrencyResource) {
    super();
    // Listen to queue events
    if (typeof (queue as any).on === 'function') {
      (queue as any).on('queryQueued', (data: unknown) => this.emit('queryQueued', data));
      (queue as any).on('queryStarted', (data: unknown) => this.emit('queryStarted', data));
      (queue as any).on('queryCompleted', (data: unknown) => this.emit('queryCompleted', data));
      (queue as any).on('error', (err: unknown) => this.emit('error', err));
    }
    // Listen to resource events
    if (typeof (resource as any).on === 'function') {
      (resource as any).on('resourceUpdated', (data: unknown) => this.emit('resourceUpdated', data));
    }
  }

  /**
   * Subscribe to a concurrency event
   * @param event Event name
   * @param listener Listener function
   */
  onEvent(event: string, listener: (...args: any[]) => void): void {
    this.on(event, listener);
  }

  /**
   * Emit a custom concurrency event
   * @param event Event name
   * @param args Arguments
   */
  emitEvent(event: string, ...args: any[]): void {
    this.emit(event, ...args);
  }
}
