import { EventEmitter } from 'events';
import { QueryHistoryOptions } from '../features/queryHistoryManager';

export interface PrologHistoryManagerOptions extends Partial<QueryHistoryOptions> {
  logger?: (msg: string) => void;
}

export class PrologHistoryManager extends EventEmitter {
  constructor(options: PrologHistoryManagerOptions) {
    super();
    // ...setup logic...
  }
  // ...history, statistics logic...
}
