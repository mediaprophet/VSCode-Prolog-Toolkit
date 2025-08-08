// Handles all session persistence and storage operations
import { SessionManagerOptions } from '../sessionManager';


import * as fs from 'fs/promises';
import * as path from 'path';
import { SessionConfig, SessionSnapshot, SessionState } from '../sessionManager';

export class SessionStorage {
  private storageDir: string;
  private sessionsFile: string;
  private statesDir: string;
  private snapshotsDir: string;

  constructor(private options: Partial<SessionManagerOptions> = {}) {
    this.storageDir = options.storageDir || path.join(process.cwd(), '.prolog-sessions');
    this.sessionsFile = path.join(this.storageDir, 'sessions.json');
    this.statesDir = path.join(this.storageDir, 'states');
    this.snapshotsDir = path.join(this.storageDir, 'snapshots');
  }

  async ensureDirs(): Promise<void> {
    await fs.mkdir(this.storageDir, { recursive: true });
    await fs.mkdir(this.statesDir, { recursive: true });
    await fs.mkdir(this.snapshotsDir, { recursive: true });
  }

  async loadSessions(): Promise<Record<string, SessionConfig>> {
    try {
      await this.ensureDirs();
      const data = await fs.readFile(this.sessionsFile, 'utf8');
      return JSON.parse(data);
    } catch (err: any) {
      if (err.code === 'ENOENT') return {};
      throw err;
    }
  }

  async saveSessions(sessions: Record<string, SessionConfig>): Promise<void> {
    await this.ensureDirs();
    await fs.writeFile(this.sessionsFile, JSON.stringify(sessions, null, 2), 'utf8');
  }

  async loadState(sessionId: string): Promise<SessionState | null> {
    const stateFile = path.join(this.statesDir, `${sessionId}.json`);
    try {
      const data = await fs.readFile(stateFile, 'utf8');
      return JSON.parse(data);
    } catch (err: any) {
      if (err.code === 'ENOENT') return null;
      throw err;
    }
  }

  async saveState(sessionId: string, state: SessionState): Promise<void> {
    await this.ensureDirs();
    const stateFile = path.join(this.statesDir, `${sessionId}.json`);
    await fs.writeFile(stateFile, JSON.stringify(state, null, 2), 'utf8');
  }

  async deleteSession(sessionId: string): Promise<void> {
    const stateFile = path.join(this.statesDir, `${sessionId}.json`);
    try {
      await fs.unlink(stateFile);
    } catch (err: any) {
      if (err.code !== 'ENOENT') throw err;
    }
    // Optionally remove session from sessionsFile and snapshots
  }

  async saveSnapshot(snapshotId: string, snapshot: SessionSnapshot): Promise<void> {
    await this.ensureDirs();
    const snapshotFile = path.join(this.snapshotsDir, `${snapshotId}.json`);
    await fs.writeFile(snapshotFile, JSON.stringify(snapshot, null, 2), 'utf8');
  }

  async loadSnapshot(snapshotId: string): Promise<SessionSnapshot | null> {
    const snapshotFile = path.join(this.snapshotsDir, `${snapshotId}.json`);
    try {
      const data = await fs.readFile(snapshotFile, 'utf8');
      return JSON.parse(data);
    } catch (err: any) {
      if (err.code === 'ENOENT') return null;
      throw err;
    }
  }
}
