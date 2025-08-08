// Orchestrator for Session Management
// Composes storage, event, and logic modules for robust, testable session management

import { EventEmitter } from 'events';
import { SessionManagerOptions } from '../sessionManager';
import { SessionEvents } from './SessionEvents';
import { SessionLogic } from './SessionLogic';
import { SessionStorage } from './SessionStorage';

export class SessionManagerOrchestrator extends EventEmitter {
  private storage: SessionStorage;
  private logic: SessionLogic;
  private events: SessionEvents;

  constructor(options: Partial<SessionManagerOptions> = {}) {
    super();
    this.storage = new SessionStorage(options);
    this.logic = new SessionLogic(this.storage, options);
    this.events = new SessionEvents(this.logic, this.storage);
    this.registerEventHandlers();
  }

  private registerEventHandlers() {
    this.events.on('sessionCreated', (data) => this.emit('sessionCreated', data));
    this.events.on('sessionDeleted', (data) => this.emit('sessionDeleted', data));
    this.events.on('error', (err) => this.emit('error', err));
    // ...add more as needed
  }

  // Expose orchestrated API
  async createSession(name: string, options?: any): Promise<string> {
    return this.logic.createSession(name, options);
  }

  async switchToSession(sessionId: string): Promise<void> {
    return this.logic.switchToSession(sessionId);
  }

  getCurrentSession() {
    return this.logic.getCurrentSession();
  }

  // ...add more methods as needed
}
