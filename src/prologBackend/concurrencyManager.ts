import { EventEmitter } from 'events';
import { ResourceQuota } from '../features/concurrencyManager';

export interface PrologConcurrencyManagerOptions {
  resourceQuota?: Partial<ResourceQuota>;
  logger?: (msg: string) => void;
}

export class PrologConcurrencyManager extends EventEmitter {
  constructor(options: PrologConcurrencyManagerOptions) {
    super();
    // ...setup logic...
  }
  // ...concurrency, queue, resource logic...
}
