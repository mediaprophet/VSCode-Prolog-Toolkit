// Core business logic for query history management
import { QueryHistoryOptions } from '../queryHistoryManager';
import { QueryHistoryStorage } from './QueryHistoryStorage';

import { EventEmitter } from 'events';


/**
 * Query entry metadata structure:
 * {
 *   tags?: string[];
 *   userAgent?: string;
 *   sessionId?: string;
 *   source?: 'ui' | 'api' | 'automation';
 *   resourceUsage?: { cpuPercent?: number; memoryMB?: number; durationMs?: number };
 *   artifacts?: Array<{ name: string; path: string; type?: string }>;
 *   ...
 * }
 */
export class QueryHistoryLogic extends EventEmitter {
  constructor(private storage: QueryHistoryStorage, private options: Partial<QueryHistoryOptions> = {}) {
    super();
  }

  async addQuery(entry: any): Promise<void> {
    // Ensure metadata fields exist and are normalized
    entry.metadata = entry.metadata || {};
    if (!entry.metadata.source) {
      entry.metadata.source = 'ui'; // Default to UI if not specified
    }
    if (!entry.metadata.resourceUsage) {
      entry.metadata.resourceUsage = {};
    }
    if (!entry.metadata.artifacts) {
      entry.metadata.artifacts = [];
    }
    await this.storage.saveEntry(entry);
    this.emit('queryAdded', entry);
  }

  async updateQuery(queryId: string, updates: any): Promise<void> {
    let entries = await this.storage.loadHistory();
    let idx = entries.findIndex((e: any) => e.id === queryId);
    if (idx !== -1) {
      // Merge metadata fields if present
      if (updates.metadata) {
        entries[idx].metadata = { ...entries[idx].metadata, ...updates.metadata };
        delete updates.metadata;
      }
      entries[idx] = { ...entries[idx], ...updates };
      await this.storage.rewriteHistoryFile(entries);
      this.emit('queryUpdated', entries[idx]);
    }
  }


  async getHistory(filter: any = {}): Promise<any[]> {
    let entries = await this.storage.loadHistory();
    // Apply filtering, sorting, pagination
    if (filter.status) entries = entries.filter((e: any) => filter.status.includes(e.status));
    if (filter.cmd) entries = entries.filter((e: any) => e.cmd === filter.cmd);
    if (filter.priority) entries = entries.filter((e: any) => filter.priority.includes(e.priority));
    if (filter.tags) entries = entries.filter((e: any) => e.metadata?.tags?.some((t: string) => filter.tags.includes(t)));
    if (filter.startDate) entries = entries.filter((e: any) => e.startTime >= filter.startDate.getTime());
    if (filter.endDate) entries = entries.filter((e: any) => e.endTime && e.endTime <= filter.endDate.getTime());
    if (filter.sortBy) {
      entries = entries.sort((a: any, b: any) => {
        if (filter.sortOrder === 'desc') return b[filter.sortBy] - a[filter.sortBy];
        return a[filter.sortBy] - b[filter.sortBy];
      });
    }
    if (filter.offset) entries = entries.slice(filter.offset);
    if (filter.limit) entries = entries.slice(0, filter.limit);
    return entries;
  }

  async getStatistics(): Promise<any> {
    let entries = await this.storage.loadHistory();
    const stats = {
      totalQueries: entries.length,
      completedQueries: entries.filter((e: any) => e.status === 'completed').length,
      errorQueries: entries.filter((e: any) => e.status === 'error').length,
      cancelledQueries: entries.filter((e: any) => e.status === 'cancelled').length,
      timeoutQueries: entries.filter((e: any) => e.status === 'timeout').length,
      averageDuration: 0,
      totalDuration: 0,
      queriesByStatus: {} as Record<string, number>,
      queriesByPriority: {} as Record<string, number>,
      queriesByCmd: {} as Record<string, number>,
      dailyStats: [] as Array<{ date: string; count: number; avgDuration: number }>,
    };
    let totalDuration = 0;
    let dailyMap: Record<string, { count: number; total: number }> = {};
    for (const e of entries) {
      if (e.duration) totalDuration += e.duration;
      stats.queriesByStatus[e.status] = (stats.queriesByStatus[e.status] || 0) + 1;
      if (e.priority) stats.queriesByPriority[e.priority] = (stats.queriesByPriority[e.priority] || 0) + 1;
      if (e.cmd) stats.queriesByCmd[e.cmd] = (stats.queriesByCmd[e.cmd] || 0) + 1;
      if (e.startTime) {
        const date = new Date(e.startTime).toISOString().slice(0, 10);
        if (!dailyMap[date]) dailyMap[date] = { count: 0, total: 0 };
        dailyMap[date].count++;
        if (e.duration) dailyMap[date].total += e.duration;
      }
    }
    stats.totalDuration = totalDuration;
    stats.averageDuration = entries.length ? totalDuration / entries.length : 0;
    stats.dailyStats = Object.entries(dailyMap).map(([date, { count, total }]) => ({ date, count, avgDuration: count ? total / count : 0 }));
    return stats;
  }
  // --- Tag Management ---
  async addTags(queryId: string, tags: string[]): Promise<void> {
    let entries = await this.storage.loadHistory();
    let idx = entries.findIndex((e: any) => e.id === queryId);
    if (idx !== -1) {
      const entry = entries[idx];
      entry.metadata = entry.metadata || {};
      entry.metadata.tags = Array.from(new Set([...(entry.metadata.tags || []), ...tags]));
      await this.storage.rewriteHistoryFile(entries);
      this.emit('queryUpdated', entry);
    }
  }

  async removeTags(queryId: string, tags: string[]): Promise<void> {
    let entries = await this.storage.loadHistory();
    let idx = entries.findIndex((e: any) => e.id === queryId);
    if (idx !== -1) {
      const entry = entries[idx];
      entry.metadata = entry.metadata || {};
      entry.metadata.tags = (entry.metadata.tags || []).filter((t: string) => !tags.includes(t));
      await this.storage.rewriteHistoryFile(entries);
      this.emit('queryUpdated', entry);
    }
  }

  async getAllTags(): Promise<string[]> {
    let entries = await this.storage.loadHistory();
    const tagSet = new Set<string>();
    for (const e of entries) {
      if (e.metadata?.tags) {
        for (const t of e.metadata.tags) tagSet.add(t);
      }
    }
    return Array.from(tagSet);
  }

  // --- Analytics Event Emission ---
  async emitAnalyticsUpdate(): Promise<void> {
    const stats = await this.getStatistics();
    this.emit('analyticsUpdated', stats);
  }
}
