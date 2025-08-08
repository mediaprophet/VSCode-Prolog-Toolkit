// Handles all persistent storage for query history
import { EventEmitter } from 'events';
import * as fs from 'fs/promises';
import * as path from 'path';
import * as zlib from 'zlib';
import { QueryHistoryOptions } from '../queryHistoryManager';

export class QueryHistoryStorage extends EventEmitter {
  private historyFile: string;
  private compressionEnabled: boolean;
  private retentionDays: number;
  private batchSize: number;

  constructor(private options: Partial<QueryHistoryOptions> = {}) {
    super();
    this.historyFile = path.resolve(options.storageDir || '.', 'queryHistory.json' + (options.compressionEnabled ? '.gz' : ''));
    this.compressionEnabled = !!options.compressionEnabled;
    this.retentionDays = options.retentionDays ?? 30;
    this.batchSize = options.batchSize ?? 100;
  }

  private async readFile(): Promise<any[]> {
    try {
      let data: Buffer;
      try {
        data = await fs.readFile(this.historyFile);
      } catch (e) {
        if ((e as any).code === 'ENOENT') return [];
        throw e;
      }
      let json: string;
      if (this.compressionEnabled) {
        json = zlib.gunzipSync(data).toString('utf8');
      } else {
        json = data.toString('utf8');
      }
      return JSON.parse(json);
    } catch (err) {
      this.emit('error', err);
      return [];
    }
  }

  private async writeFile(entries: any[]): Promise<void> {
    try {
      let json = JSON.stringify(entries, null, 2);
      let data: Buffer;
      if (this.compressionEnabled) {
        data = zlib.gzipSync(Buffer.from(json, 'utf8'));
      } else {
        data = Buffer.from(json, 'utf8');
      }
      await fs.writeFile(this.historyFile, data);
    } catch (err) {
      this.emit('error', err);
    }
  }

  async loadHistory(): Promise<any[]> {
    const entries = await this.readFile();
    this.emit('historyLoaded', entries);
    return entries;
  }

  async saveEntry(entry: any): Promise<void> {
    let entries = await this.readFile();
    entries.push(entry);
    await this.writeFile(entries);
    this.emit('entrySaved', entry);
  }

  async deleteEntry(queryId: string): Promise<void> {
    let entries = await this.readFile();
    entries = entries.filter((e: any) => e.id !== queryId);
    await this.writeFile(entries);
    this.emit('entryDeleted', queryId);
  }

  async rewriteHistoryFile(entries: any[]): Promise<void> {
    await this.writeFile(entries);
    this.emit('historyRewritten', entries);
  }

  async cleanupOldEntries(): Promise<void> {
    let entries = await this.readFile();
    const cutoff = Date.now() - this.retentionDays * 24 * 60 * 60 * 1000;
    entries = entries.filter((e: any) => !e.endTime || e.endTime >= cutoff);
    await this.writeFile(entries);
    this.emit('historyCleaned', entries);
  }
}
