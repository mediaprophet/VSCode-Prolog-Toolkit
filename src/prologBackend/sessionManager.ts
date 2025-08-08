import { EventEmitter } from 'events';
import { SessionManagerOptions } from '../features/sessionManager';

export interface PrologSessionManagerOptions extends Partial<SessionManagerOptions> {
  logger?: (msg: string) => void;
}

export class PrologSessionManager extends EventEmitter {
  constructor(options: PrologSessionManagerOptions) {
    super();
    // ...setup logic...
  }
  // ...session, state, persistence logic...
}
