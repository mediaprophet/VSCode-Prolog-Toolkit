import { EventEmitter } from 'events';
import { CancellationToken, Progress, ProgressLocation, window } from 'vscode';

export interface StreamingOptions {
  chunkSize?: number;
  maxTotalResults?: number;
  progressTitle?: string;
  showProgress?: boolean;
  bufferTimeout?: number;
}

export interface StreamChunk<T> {
  data: T[];
  index: number;
  isFirst: boolean;
  isLast: boolean;
  totalCount?: number;
  hasMore: boolean;
}

export class StreamingHandler<T> extends EventEmitter {
  private options: Required<StreamingOptions>;
  private buffer: T[] = [];
  private totalProcessed: number = 0;
  private isStreaming: boolean = false;
  private bufferTimer?: NodeJS.Timeout;

  constructor(options: StreamingOptions = {}) {
    super();
    this.options = {
      chunkSize: options.chunkSize ?? 20,
      maxTotalResults: options.maxTotalResults ?? 1000,
      progressTitle: options.progressTitle ?? 'Processing results',
      showProgress: options.showProgress ?? true,
      bufferTimeout: options.bufferTimeout ?? 100
    };
  }

  /**
   * Start streaming data processing
   */
  async startStreaming(
    dataSource: AsyncIterable<T> | T[],
    processor: (chunk: StreamChunk<T>) => Promise<void> | void,
    cancellationToken?: CancellationToken
  ): Promise<void> {
    if (this.isStreaming) {
      throw new Error('Streaming is already in progress');
    }

    this.isStreaming = true;
    this.totalProcessed = 0;
    this.buffer = [];

    try {
      if (this.options.showProgress) {
        await this.streamWithProgress(dataSource, processor, cancellationToken);
      } else {
        await this.streamWithoutProgress(dataSource, processor, cancellationToken);
      }
    } finally {
      this.isStreaming = false;
      this.clearBuffer();
    }
  }

  /**
   * Stream with progress indication
   */
  private async streamWithProgress(
    dataSource: AsyncIterable<T> | T[],
    processor: (chunk: StreamChunk<T>) => Promise<void> | void,
    cancellationToken?: CancellationToken
  ): Promise<void> {
    return window.withProgress(
      {
        location: ProgressLocation.Notification,
        title: this.options.progressTitle,
        cancellable: true
      },
      async (progress: Progress<{ message?: string; increment?: number }>, token: CancellationToken) => {
        const effectiveToken = cancellationToken || token;
        await this.processStream(dataSource, processor, progress, effectiveToken);
      }
    );
  }

  /**
   * Stream without progress indication
   */
  private async streamWithoutProgress(
    dataSource: AsyncIterable<T> | T[],
    processor: (chunk: StreamChunk<T>) => Promise<void> | void,
    cancellationToken?: CancellationToken
  ): Promise<void> {
    await this.processStream(dataSource, processor, undefined, cancellationToken);
  }

  /**
   * Process the stream data
   */
  private async processStream(
    dataSource: AsyncIterable<T> | T[],
    processor: (chunk: StreamChunk<T>) => Promise<void> | void,
    progress?: Progress<{ message?: string; increment?: number }>,
    cancellationToken?: CancellationToken
  ): Promise<void> {
    let chunkIndex = 0;
    let isFirst = true;
    let totalCount: number | undefined;

    // Handle array data source
    if (Array.isArray(dataSource)) {
      totalCount = Math.min(dataSource.length, this.options.maxTotalResults);
      const chunks = this.chunkArray(dataSource.slice(0, this.options.maxTotalResults), this.options.chunkSize);
      
      for (const chunk of chunks) {
        if (cancellationToken?.isCancellationRequested) {
          break;
        }

        const isLast = chunkIndex === chunks.length - 1;
        const streamChunk: StreamChunk<T> = {
          data: chunk,
          index: chunkIndex,
          isFirst,
          isLast,
          totalCount,
          hasMore: !isLast
        };

        await processor(streamChunk);
        
        this.totalProcessed += chunk.length;
        chunkIndex++;
        isFirst = false;

        if (progress) {
          const percentage = (this.totalProcessed / totalCount) * 100;
          progress.report({
            message: `Processed ${this.totalProcessed} of ${totalCount} results`,
            increment: percentage / chunks.length
          });
        }

        this.emit('chunk', streamChunk);
      }
    } else {
      // Handle async iterable data source
      for await (const item of dataSource) {
        if (cancellationToken?.isCancellationRequested) {
          break;
        }

        if (this.totalProcessed >= this.options.maxTotalResults) {
          break;
        }

        this.buffer.push(item);
        this.totalProcessed++;

        // Process buffer when it reaches chunk size or timeout
        if (this.buffer.length >= this.options.chunkSize) {
          await this.flushBuffer(processor, chunkIndex, isFirst, false, progress);
          chunkIndex++;
          isFirst = false;
        } else {
          // Set timeout to flush buffer if no more data comes quickly
          this.resetBufferTimer(() => {
            if (this.buffer.length > 0) {
              this.flushBuffer(processor, chunkIndex, isFirst, false, progress);
              chunkIndex++;
              isFirst = false;
            }
          });
        }
      }

      // Flush remaining buffer
      if (this.buffer.length > 0) {
        await this.flushBuffer(processor, chunkIndex, isFirst, true, progress);
      }
    }

    this.emit('complete', { totalProcessed: this.totalProcessed });
  }

  /**
   * Flush the current buffer
   */
  private async flushBuffer(
    processor: (chunk: StreamChunk<T>) => Promise<void> | void,
    chunkIndex: number,
    isFirst: boolean,
    isLast: boolean,
    progress?: Progress<{ message?: string; increment?: number }>
  ): Promise<void> {
    if (this.buffer.length === 0) {
      return;
    }

    const chunk = [...this.buffer];
    this.buffer = [];

    const streamChunk: StreamChunk<T> = {
      data: chunk,
      index: chunkIndex,
      isFirst,
      isLast,
      totalCount: undefined,
      hasMore: !isLast
    };

    await processor(streamChunk);

    if (progress) {
      progress.report({
        message: `Processed ${this.totalProcessed} results`,
        increment: 10 // Arbitrary increment for unknown totals
      });
    }

    this.emit('chunk', streamChunk);
  }

  /**
   * Reset buffer timeout
   */
  private resetBufferTimer(callback: () => void): void {
    if (this.bufferTimer) {
      clearTimeout(this.bufferTimer);
    }
    
    this.bufferTimer = setTimeout(callback, this.options.bufferTimeout);
  }

  /**
   * Clear buffer and timer
   */
  private clearBuffer(): void {
    this.buffer = [];
    if (this.bufferTimer) {
      clearTimeout(this.bufferTimer);
      this.bufferTimer = undefined;
    }
  }

  /**
   * Split array into chunks
   */
  private chunkArray<U>(array: U[], chunkSize: number): U[][] {
    const chunks: U[][] = [];
    for (let i = 0; i < array.length; i += chunkSize) {
      chunks.push(array.slice(i, i + chunkSize));
    }
    return chunks;
  }

  /**
   * Stop streaming (if in progress)
   */
  stop(): void {
    this.isStreaming = false;
    this.clearBuffer();
    this.emit('stopped');
  }

  /**
   * Check if currently streaming
   */
  get streaming(): boolean {
    return this.isStreaming;
  }

  /**
   * Get total processed count
   */
  get processed(): number {
    return this.totalProcessed;
  }

  /**
   * Update streaming options
   */
  updateOptions(newOptions: Partial<StreamingOptions>): void {
    this.options = { ...this.options, ...newOptions };
  }
}

/**
 * Utility function to create a streaming handler for Prolog results
 */
export function createPrologResultStreamer(options?: StreamingOptions): StreamingHandler<any> {
  return new StreamingHandler({
    chunkSize: 50,
    maxTotalResults: 1000,
    progressTitle: 'Processing Prolog results',
    showProgress: true,
    bufferTimeout: 100,
    ...options
  });
}

/**
 * Utility function to create a streaming handler for large file processing
 */
export function createFileStreamer(options?: StreamingOptions): StreamingHandler<string> {
  return new StreamingHandler({
    chunkSize: 100,
    maxTotalResults: 10000,
    progressTitle: 'Processing file',
    showProgress: true,
    bufferTimeout: 50,
    ...options
  });
}

/**
 * Stream processor for paginated results
 */
export class PaginatedStreamer<T> {
  private currentPage: number = 0;
  private pageSize: number;
  private totalPages?: number;

  constructor(pageSize: number = 20) {
    this.pageSize = pageSize;
  }

  /**
   * Process paginated data
   */
  async processPaginated(
    fetcher: (page: number, pageSize: number) => Promise<{ data: T[]; totalCount?: number; hasMore: boolean }>,
    processor: (chunk: StreamChunk<T>) => Promise<void> | void,
    cancellationToken?: CancellationToken
  ): Promise<void> {
    this.currentPage = 0;
    let isFirst = true;
    let totalCount: number | undefined;

    while (true) {
      if (cancellationToken?.isCancellationRequested) {
        break;
      }

      const result = await fetcher(this.currentPage, this.pageSize);
      
      if (result.totalCount !== undefined && totalCount === undefined) {
        totalCount = result.totalCount;
        this.totalPages = Math.ceil(totalCount / this.pageSize);
      }

      const isLast = !result.hasMore || result.data.length < this.pageSize;
      
      const streamChunk: StreamChunk<T> = {
        data: result.data,
        index: this.currentPage,
        isFirst,
        isLast,
        totalCount,
        hasMore: result.hasMore
      };

      await processor(streamChunk);

      if (isLast || result.data.length === 0) {
        break;
      }

      this.currentPage++;
      isFirst = false;
    }
  }

  /**
   * Get current page number
   */
  getCurrentPage(): number {
    return this.currentPage;
  }

  /**
   * Get total pages (if known)
   */
  getTotalPages(): number | undefined {
    return this.totalPages;
  }

  /**
   * Reset pagination state
   */
  reset(): void {
    this.currentPage = 0;
    this.totalPages = undefined;
  }
}

/**
 * Utility for streaming large query results
 */
export async function streamQueryResults(
  results: any[],
  formatter: (chunk: any[], chunkInfo: StreamChunk<any>) => string,
  options?: StreamingOptions
): Promise<string[]> {
  const streamer = createPrologResultStreamer(options);
  const formattedChunks: string[] = [];

  await streamer.startStreaming(
    results,
    async (chunk) => {
      const formatted = formatter(chunk.data, chunk);
      formattedChunks.push(formatted);
    }
  );

  return formattedChunks;
}

/**
 * Create a streaming handler optimized for chat responses
 */
export function createChatStreamer(options?: StreamingOptions): StreamingHandler<any> {
  return new StreamingHandler({
    chunkSize: 30,
    maxTotalResults: 2000,
    progressTitle: 'Processing chat response',
    showProgress: false, // Chat has its own progress indicators
    bufferTimeout: 50,
    ...options
  });
}

/**
 * Stream large results to chat with progress updates
 */
export async function streamToChatResponse(
  results: any[],
  chatStream: any, // ChatResponseStream type
  formatter: (chunk: any[], isFirst: boolean, isLast: boolean, totalCount?: number) => void,
  options?: StreamingOptions
): Promise<void> {
  if (results.length <= (options?.chunkSize || 30)) {
    // Small result set, process immediately
    formatter(results, true, true, results.length);
    return;
  }

  const streamer = createChatStreamer(options);
  let totalCount = results.length;

  await streamer.startStreaming(
    results,
    async (chunk) => {
      formatter(chunk.data, chunk.isFirst, chunk.isLast, totalCount);
      
      // Add small delay between chunks for better UX
      if (!chunk.isLast) {
        await new Promise(resolve => setTimeout(resolve, 50));
      }
    }
  );
}