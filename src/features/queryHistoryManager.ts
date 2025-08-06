import { EventEmitter } from 'events';
import * as fs from 'fs';
import * as path from 'path';
import { QueryStatus } from './queryNotificationManager';

export interface QueryHistoryEntry {
  id: string;
  cmd: string;
  params: Record<string, unknown>;
  status: 'pending' | 'running' | 'completed' | 'error' | 'cancelled' | 'timeout';
  startTime: number;
  endTime?: number;
  duration?: number;
  results?: unknown;
  error?: unknown;
  priority?: string;
  deleted?: boolean;
  resourceUsage?: {
    memoryMB?: number;
    cpuPercent?: number;
  };
  metadata?: {
    userAgent?: string;
    sessionId?: string;
    workspaceId?: string;
    tags?: string[];
  };
}

export interface QueryHistoryFilter {
  status?: string[];
  startDate?: Date;
  endDate?: Date;
  cmd?: string;
  priority?: string[];
  tags?: string[];
  limit?: number;
  offset?: number;
  sortBy?: 'startTime' | 'endTime' | 'duration' | 'cmd';
  sortOrder?: 'asc' | 'desc';
}

export interface QueryHistoryStats {
  totalQueries: number;
  completedQueries: number;
  errorQueries: number;
  cancelledQueries: number;
  timeoutQueries: number;
  averageDuration: number;
  totalDuration: number;
  queriesByStatus: Record<string, number>;
  queriesByPriority: Record<string, number>;
  queriesByCmd: Record<string, number>;
  dailyStats: Array<{
    date: string;
    count: number;
    avgDuration: number;
  }>;
}

export interface QueryHistoryOptions {
  storageDir: string;
  maxHistorySize: number;
  compressionEnabled: boolean;
  retentionDays: number;
  autoCleanup: boolean;
  batchSize: number;
}

/**
 * Persistent query history storage and management system
 */
export class QueryHistoryManager extends EventEmitter {
  private static instance: QueryHistoryManager;
  private options: QueryHistoryOptions;
  private historyFile: string;
  private indexFile: string;
  private memoryCache: Map<string, QueryHistoryEntry> = new Map();
  private writeQueue: QueryHistoryEntry[] = [];
  private writeInterval?: ReturnType<typeof setInterval>;
  private cleanupInterval?: ReturnType<typeof setInterval>;
  private isInitialized: boolean = false;

  /**
   * Get singleton instance of QueryHistoryManager
   */
  public static getInstance(options?: Partial<QueryHistoryOptions>): QueryHistoryManager {
    if (!QueryHistoryManager.instance) {
      QueryHistoryManager.instance = new QueryHistoryManager(options);
    }
    return QueryHistoryManager.instance;
  }

  constructor(options: Partial<QueryHistoryOptions> = {}) {
    super();

    this.options = {
      storageDir: path.join(process.cwd(), '.prolog-history'),
      maxHistorySize: 10000,
      compressionEnabled: true,
      retentionDays: 30,
      autoCleanup: true,
      batchSize: 100,
      ...options,
    };

    this.historyFile = path.join(this.options.storageDir, 'query-history.jsonl');
    this.indexFile = path.join(this.options.storageDir, 'query-index.json');

    this.initialize();
  }

  /**
   * Initialize the history manager
   */
  private async initialize(): Promise<void> {
    try {
      // Ensure storage directory exists
      if (!fs.existsSync(this.options.storageDir)) {
        fs.mkdirSync(this.options.storageDir, { recursive: true });
      }

      // Load existing history into memory cache
      await this.loadHistoryFromDisk();

      // Start background processes
      this.startWriteProcessor();
      if (this.options.autoCleanup) {
        this.startCleanupProcessor();
      }

      this.isInitialized = true;
      this.emit('initialized');
      console.log(`[QueryHistoryManager] Initialized with ${this.memoryCache.size} entries`);
    } catch (error: unknown) {
      console.error('[QueryHistoryManager] Initialization failed:', error);
      this.emit('error', error);
    }
  }

  /**
   * Add a query to the history
   */
  async addQuery(entry: Omit<QueryHistoryEntry, 'duration'>): Promise<void> {
    if (!this.isInitialized) {
      throw new Error('QueryHistoryManager not initialized');
    }

    const historyEntry: QueryHistoryEntry = {
      ...entry,
    };

    // Only set duration if endTime is provided
    if (entry.endTime) {
      historyEntry.duration = entry.endTime - entry.startTime;
    }

    // Add to memory cache
    this.memoryCache.set(entry.id, historyEntry);

    // Add to write queue for persistent storage
    this.writeQueue.push(historyEntry);

    // Emit event
    this.emit('queryAdded', historyEntry);

    // Check if we need to enforce size limits
    if (this.memoryCache.size > this.options.maxHistorySize) {
      await this.enforceHistoryLimit();
    }

    console.log(`[QueryHistoryManager] Added query ${entry.id} to history`);
  }

  /**
   * Update an existing query in the history
   */
  async updateQuery(queryId: string, updates: Partial<QueryHistoryEntry>): Promise<void> {
    if (!this.isInitialized) {
      throw new Error('QueryHistoryManager not initialized');
    }

    const existingEntry = this.memoryCache.get(queryId);
    if (!existingEntry) {
      console.warn(`[QueryHistoryManager] Query ${queryId} not found for update`);
      return;
    }

    const updatedEntry: QueryHistoryEntry = {
      ...existingEntry,
      ...updates,
    };

    // Calculate duration if endTime is provided in updates
    if (updates.endTime) {
      updatedEntry.duration = updates.endTime - existingEntry.startTime;
    }

    // Update memory cache
    this.memoryCache.set(queryId, updatedEntry);

    // Add to write queue
    this.writeQueue.push(updatedEntry);

    this.emit('queryUpdated', updatedEntry);
    console.log(`[QueryHistoryManager] Updated query ${queryId} in history`);
  }

  /**
   * Get query history with filtering and pagination
   */
  async getHistory(filter: QueryHistoryFilter = {}): Promise<{
    entries: QueryHistoryEntry[];
    total: number;
    hasMore: boolean;
  }> {
    if (!this.isInitialized) {
      throw new Error('QueryHistoryManager not initialized');
    }

    let entries = Array.from(this.memoryCache.values());

    // Apply filters
    if (filter.status && filter.status.length > 0) {
      entries = entries.filter(entry => filter.status!.includes(entry.status));
    }

    if (filter.startDate) {
      entries = entries.filter(entry => entry.startTime >= filter.startDate!.getTime());
    }

    if (filter.endDate) {
      entries = entries.filter(entry => entry.startTime <= filter.endDate!.getTime());
    }

    if (filter.cmd) {
      entries = entries.filter(entry => entry.cmd.includes(filter.cmd!));
    }

    if (filter.priority && filter.priority.length > 0) {
      entries = entries.filter(
        entry => entry.priority && filter.priority!.includes(entry.priority)
      );
    }

    if (filter.tags && filter.tags.length > 0) {
      entries = entries.filter(
        entry =>
          entry.metadata?.tags && filter.tags!.some(tag => entry.metadata!.tags!.includes(tag))
      );
    }

    // Sort entries
    const sortBy = filter.sortBy || 'startTime';
    const sortOrder = filter.sortOrder || 'desc';

    entries.sort((a, b) => {
      let aValue: unknown, bValue: unknown;

      switch (sortBy) {
        case 'startTime': {
          aValue = a.startTime;
          bValue = b.startTime;
          break;
        }
        case 'endTime': {
          aValue = a.endTime || 0;
          bValue = b.endTime || 0;
          break;
        }
        case 'duration': {
          aValue = a.duration || 0;
          bValue = b.duration || 0;
          break;
        }
        case 'cmd': {
          aValue = a.cmd;
          bValue = b.cmd;
          break;
        }
        default: {
          aValue = a.startTime;
          bValue = b.startTime;
        }
      }

      if (sortOrder === 'asc') {
        return aValue < bValue ? -1 : aValue > bValue ? 1 : 0;
      } else {
        return aValue > bValue ? -1 : aValue < bValue ? 1 : 0;
      }
    });

    const total = entries.length;
    const offset = filter.offset || 0;
    const limit = filter.limit || 100;

    // Apply pagination
    const paginatedEntries = entries.slice(offset, offset + limit);
    const hasMore = offset + limit < total;

    return {
      entries: paginatedEntries,
      total,
      hasMore,
    };
  }

  /**
   * Get a specific query by ID
   */
  async getQuery(queryId: string): Promise<QueryHistoryEntry | undefined> {
    if (!this.isInitialized) {
      throw new Error('QueryHistoryManager not initialized');
    }

    return this.memoryCache.get(queryId);
  }

  /**
   * Delete a query from history
   */
  async deleteQuery(queryId: string): Promise<boolean> {
    if (!this.isInitialized) {
      throw new Error('QueryHistoryManager not initialized');
    }

    const deleted = this.memoryCache.delete(queryId);
    if (deleted) {
      // Mark for deletion in persistent storage
      this.writeQueue.push({ id: queryId, deleted: true } as any);
      this.emit('queryDeleted', queryId);
      console.log(`[QueryHistoryManager] Deleted query ${queryId} from history`);
    }

    return deleted;
  }

  /**
   * Get comprehensive statistics about query history
   */
  async getStatistics(): Promise<QueryHistoryStats> {
    if (!this.isInitialized) {
      throw new Error('QueryHistoryManager not initialized');
    }

    const entries = Array.from(this.memoryCache.values());
    const completedEntries = entries.filter(e => e.status === 'completed' && e.duration);

    const stats: QueryHistoryStats = {
      totalQueries: entries.length,
      completedQueries: entries.filter(e => e.status === 'completed').length,
      errorQueries: entries.filter(e => e.status === 'error').length,
      cancelledQueries: entries.filter(e => e.status === 'cancelled').length,
      timeoutQueries: entries.filter(e => e.status === 'timeout').length,
      averageDuration:
        completedEntries.length > 0
          ? completedEntries.reduce((sum, e) => sum + (e.duration || 0), 0) /
            completedEntries.length
          : 0,
      totalDuration: completedEntries.reduce((sum, e) => sum + (e.duration || 0), 0),
      queriesByStatus: {},
      queriesByPriority: {},
      queriesByCmd: {},
      dailyStats: [],
    };

    // Count by status
    entries.forEach(entry => {
      stats.queriesByStatus[entry.status] = (stats.queriesByStatus[entry.status] || 0) + 1;
    });

    // Count by priority
    entries.forEach(entry => {
      if (entry.priority) {
        stats.queriesByPriority[entry.priority] =
          (stats.queriesByPriority[entry.priority] || 0) + 1;
      }
    });

    // Count by command
    entries.forEach(entry => {
      stats.queriesByCmd[entry.cmd] = (stats.queriesByCmd[entry.cmd] || 0) + 1;
    });

    // Generate daily stats for the last 30 days
    const now = new Date();
    for (let i = 29; i >= 0; i--) {
      const date = new Date(now);
      date.setDate(date.getDate() - i);
      const dateStr = date.toISOString().split('T')[0] || date.toISOString().substring(0, 10);

      const dayStart = new Date(date);
      dayStart.setHours(0, 0, 0, 0);
      const dayEnd = new Date(date);
      dayEnd.setHours(23, 59, 59, 999);

      const dayEntries = entries.filter(
        entry => entry.startTime >= dayStart.getTime() && entry.startTime <= dayEnd.getTime()
      );

      const dayCompletedEntries = dayEntries.filter(e => e.status === 'completed' && e.duration);

      stats.dailyStats.push({
        date: dateStr,
        count: dayEntries.length,
        avgDuration:
          dayCompletedEntries.length > 0
            ? dayCompletedEntries.reduce((sum, e) => sum + (e.duration || 0), 0) /
              dayCompletedEntries.length
            : 0,
      });
    }

    return stats;
  }

  /**
   * Get recent queries with limit
   */
  async getRecentQueries(limit: number = 10): Promise<
    Array<{
      query: string;
      success: boolean;
      timestamp: number;
      result?: unknown;
    }>
  > {
    if (!this.isInitialized) {
      throw new Error('QueryHistoryManager not initialized');
    }

    const entries = Array.from(this.memoryCache.values())
      .filter(entry => entry.status === 'completed' || entry.status === 'error')
      .sort((a, b) => b.startTime - a.startTime)
      .slice(0, limit);

    return entries.map(entry => ({
      query: entry.cmd,
      success: entry.status === 'completed',
      timestamp: entry.startTime,
      result: entry.results,
    }));
  }

  /**
   * Clear all history
   */
  async clearHistory(): Promise<void> {
    if (!this.isInitialized) {
      throw new Error('QueryHistoryManager not initialized');
    }

    this.memoryCache.clear();
    this.writeQueue = [];

    // Clear persistent storage
    try {
      if (fs.existsSync(this.historyFile)) {
        fs.unlinkSync(this.historyFile);
      }
      if (fs.existsSync(this.indexFile)) {
        fs.unlinkSync(this.indexFile);
      }
    } catch (error: unknown) {
      console.error('[QueryHistoryManager] Error clearing persistent storage:', error);
    }

    this.emit('historyCleared');
    console.log('[QueryHistoryManager] History cleared');
  }

  /**
   * Load history from disk into memory cache
   */
  private async loadHistoryFromDisk(): Promise<void> {
    if (!fs.existsSync(this.historyFile)) {
      return;
    }

    try {
      const data = fs.readFileSync(this.historyFile, 'utf8');
      const lines = data
        .trim()
        .split('\n')
        .filter(line => line.trim());

      for (const line of lines) {
        try {
          const entry: QueryHistoryEntry = JSON.parse(line);
          if (!entry.deleted) {
            this.memoryCache.set(entry.id, entry);
          }
        } catch (error: unknown) {
          console.warn('[QueryHistoryManager] Invalid history entry:', line);
        }
      }

      console.log(`[QueryHistoryManager] Loaded ${this.memoryCache.size} entries from disk`);
    } catch (error: unknown) {
      console.error('[QueryHistoryManager] Error loading history from disk:', error);
    }
  }

  /**
   * Start the background write processor
   */
  private startWriteProcessor(): void {
    this.writeInterval = setInterval(() => {
      this.processWriteQueue();
    }, 5000); // Write every 5 seconds
  }

  /**
   * Process the write queue to persist entries to disk
   */
  private async processWriteQueue(): Promise<void> {
    if (this.writeQueue.length === 0) {
      return;
    }

    const batch = this.writeQueue.splice(0, this.options.batchSize);

    try {
      const lines = batch.map(entry => JSON.stringify(entry)).join('\n') + '\n';
      fs.appendFileSync(this.historyFile, lines, 'utf8');

      console.log(`[QueryHistoryManager] Persisted ${batch.length} entries to disk`);
    } catch (error: unknown) {
      console.error('[QueryHistoryManager] Error writing to disk:', error);
      // Put entries back in queue for retry
      this.writeQueue.unshift(...batch);
    }
  }

  /**
   * Start the cleanup processor for old entries
   */
  private startCleanupProcessor(): void {
    this.cleanupInterval = setInterval(
      () => {
        this.cleanupOldEntries();
      },
      24 * 60 * 60 * 1000
    ); // Run daily
  }

  /**
   * Clean up old entries based on retention policy
   */
  private async cleanupOldEntries(): Promise<void> {
    const cutoffTime = Date.now() - this.options.retentionDays * 24 * 60 * 60 * 1000;
    let removedCount = 0;

    for (const [id, entry] of this.memoryCache.entries()) {
      if (entry.startTime < cutoffTime) {
        this.memoryCache.delete(id);
        removedCount++;
      }
    }

    if (removedCount > 0) {
      // Rewrite the history file without old entries
      await this.rewriteHistoryFile();
      this.emit('historyCleanedUp', { removedCount });
      console.log(`[QueryHistoryManager] Cleaned up ${removedCount} old entries`);
    }
  }

  /**
   * Enforce history size limits
   */
  private async enforceHistoryLimit(): Promise<void> {
    if (this.memoryCache.size <= this.options.maxHistorySize) {
      return;
    }

    // Remove oldest entries
    const entries = Array.from(this.memoryCache.entries()).sort(
      ([, a], [, b]) => a.startTime - b.startTime
    );

    const toRemove = entries.slice(0, this.memoryCache.size - this.options.maxHistorySize);

    for (const [id] of toRemove) {
      this.memoryCache.delete(id);
    }

    await this.rewriteHistoryFile();
    console.log(`[QueryHistoryManager] Enforced history limit, removed ${toRemove.length} entries`);
  }

  /**
   * Rewrite the entire history file (used for cleanup)
   */
  private async rewriteHistoryFile(): Promise<void> {
    try {
      const entries = Array.from(this.memoryCache.values());
      const lines = entries.map(entry => JSON.stringify(entry)).join('\n');

      fs.writeFileSync(this.historyFile, lines + '\n', 'utf8');
      console.log(`[QueryHistoryManager] Rewrote history file with ${entries.length} entries`);
    } catch (error: unknown) {
      console.error('[QueryHistoryManager] Error rewriting history file:', error);
    }
  }

  /**
   * Dispose of the history manager
   */
  dispose(): void {
    if (this.writeInterval) {
      clearInterval(this.writeInterval);
    }
    if (this.cleanupInterval) {
      clearInterval(this.cleanupInterval);
    }

    // Process any remaining writes
    this.processWriteQueue();

    this.removeAllListeners();
    console.log('[QueryHistoryManager] Disposed');
  }
}
