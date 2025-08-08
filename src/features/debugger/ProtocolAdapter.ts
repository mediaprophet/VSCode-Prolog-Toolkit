import { EventEmitter } from 'events';

export type ProtocolMessage = Record<string, any>;

export interface ProtocolAdapterEvents {
  breakpoints: (data: any) => void;
  functionBreakpoints: (data: any) => void;
  frame: (data: any) => void;
  variables: (data: any) => void;
  error: (err: Error) => void;
  unknown: (data: any) => void;
}

export class ProtocolAdapter extends EventEmitter {
  constructor() {
    super();
  }

  /**
   * Parse and dispatch a protocol message string.
   * Emits events for recognized message types, or 'unknown' for unrecognized.
   */
  public handleProtocolData(data: string): void {
    let resObj: ProtocolMessage;
    try {
      resObj = JSON.parse(data);
      if (!resObj || typeof resObj !== 'object' || !('response' in resObj)) {
        throw new Error('Invalid protocol message: missing "response" property');
      }
    } catch (err: any) {
      const errObj = err instanceof Error ? err : new Error(String(err));
      this.emit('error', errObj);
      console.error('[ProtocolAdapter] Protocol parse error:', errObj, '\nRaw data:', data);
      return;
    }

    const responseType = Object.keys(resObj.response)[0];
    switch (responseType) {
      case 'breakpoints':
        this.emit('breakpoints', resObj.response.breakpoints);
        break;
      case 'functionbps':
        this.emit('functionBreakpoints', resObj.response.functionbps);
        break;
      case 'frame':
        this.emit('frame', resObj.response.frame);
        break;
      case 'variables':
        this.emit('variables', resObj.response.variables);
        break;
      default:
        this.emit('unknown', resObj.response);
        console.warn('[ProtocolAdapter] Unknown protocol response:', responseType, resObj.response);
        break;
    }
  }

  /**
   * Handle protocol data with timeout and retry logic.
   * @param data The protocol message string
   * @param timeoutMs Timeout in ms
   * @param maxRetries Number of retries
   */
  public handleProtocolDataWithRetry(data: string, timeoutMs = 5000, maxRetries = 2): Promise<void> {
    let attempt = 0;
    return new Promise((resolve, reject) => {
      const tryHandle = () => {
        let handled = false;
        const timer = setTimeout(() => {
          if (!handled) {
            if (attempt < maxRetries) {
              attempt++;
              console.warn(`[ProtocolAdapter] Protocol message handling timed out, retrying (${attempt}/${maxRetries})`);
              tryHandle();
            } else {
              const err = new Error(`[ProtocolAdapter] Protocol message handling timed out after ${timeoutMs}ms (retries: ${maxRetries})`);
              this.emit('error', err);
              reject(err);
            }
          }
        }, timeoutMs);
        try {
          this.handleProtocolData(data);
          handled = true;
          clearTimeout(timer);
          resolve();
        } catch (err) {
          clearTimeout(timer);
          if (attempt < maxRetries) {
            attempt++;
            console.warn(`[ProtocolAdapter] Protocol message handling error, retrying (${attempt}/${maxRetries})`, err);
            tryHandle();
          } else {
            this.emit('error', err);
            reject(err);
          }
        }
      };
      tryHandle();
    });
  }
}
