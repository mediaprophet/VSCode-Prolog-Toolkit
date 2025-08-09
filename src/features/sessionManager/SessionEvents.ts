// Handles all event emission and subscription for session management
import { EventEmitter } from 'events';
import { SessionLogic } from './SessionLogic';
import { SessionStorage } from './SessionStorage';

export class SessionEvents extends EventEmitter {
  constructor(private logic: SessionLogic, private storage: SessionStorage) {
    super();
    // Listen to logic events
    if (typeof (logic as any).on === 'function') {
      (logic as any).on('sessionCreated', (data: unknown) => this.emit('sessionCreated', data));
      (logic as any).on('sessionDeleted', (data: unknown) => this.emit('sessionDeleted', data));
      (logic as any).on('sessionSwitched', (data: unknown) => this.emit('sessionSwitched', data));
      (logic as any).on('snapshotCreated', (data: unknown) => this.emit('snapshotCreated', data));
      (logic as any).on('error', (err: unknown) => this.emit('error', err));
    }
    // Listen to storage events
    if (typeof (storage as any).on === 'function') {
      (storage as any).on('sessionsLoaded', (data: unknown) => this.emit('sessionsLoaded', data));
      (storage as any).on('stateLoaded', (data: unknown) => this.emit('stateLoaded', data));
      (storage as any).on('snapshotLoaded', (data: unknown) => this.emit('snapshotLoaded', data));
    }
  }

  /**
   * Subscribe to a session event
   * @param event Event name
   * @param listener Listener function
   */
  onEvent(event: string, listener: (...args: any[]) => void): void {
    this.on(event, listener);
  }

  /**
   * Emit a custom session event
   * @param event Event name
   * @param args Arguments
   */
  emitEvent(event: string, ...args: any[]): void {
    this.emit(event, ...args);
  }
}
