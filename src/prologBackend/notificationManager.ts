import { EventEmitter } from 'events';

export interface PrologNotificationManagerOptions {
  enableWebSocket?: boolean;
  webSocketPort?: number;
  logger?: (msg: string) => void;
}

export class PrologNotificationManager extends EventEmitter {
  constructor(options: PrologNotificationManagerOptions) {
    super();
    // ...setup logic...
  }
  // ...notification/event logic...
}
