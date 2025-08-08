// Core business logic for session management
import { SessionManagerOptions } from '../sessionManager';
import { SessionStorage } from './SessionStorage';


import type { ConcurrencyManagerOrchestrator } from '../concurrencyManager/ConcurrencyManagerOrchestrator';
import { ResourceQuota } from '../concurrencyManager/ConcurrencyResource';
import type { QueryHistoryOrchestrator } from '../queryHistoryManager/QueryHistoryOrchestrator';
import { SessionConfig, SessionSnapshot, SessionState } from '../sessionManager';

import { EventEmitter } from 'events';

export interface SessionLogicEvents {
  sessionCreated: { sessionId: string; config: SessionConfig };
  sessionDeleted: { sessionId: string; config: SessionConfig };
  sessionSwitched: { previousSessionId: string | null; currentSessionId: string; sessionConfig: SessionConfig };
  snapshotCreated: { snapshotId: string; snapshot: SessionSnapshot };
}

export class SessionLogic extends EventEmitter {
  private sessions: Map<string, SessionConfig> = new Map();
  private sessionStates: Map<string, SessionState> = new Map();
  private activeSession: string | null = null;
  private isInitialized: boolean = false;
  private sessionConcurrencyManagers: Map<string, ConcurrencyManagerOrchestrator> = new Map();
  private sessionHistoryManagers: Map<string, QueryHistoryOrchestrator> = new Map();

  // Optionally inject orchestrators for concurrency/history
  constructor(
    private storage: SessionStorage,
    private options: Partial<SessionManagerOptions> = {},
    private concurrencyOrchestrator?: ConcurrencyManagerOrchestrator,
    private historyOrchestrator?: QueryHistoryOrchestrator
  ) {
    super();
    // Optionally, load sessions from storage here
    // this.loadSessions();
  }

  async createSession(
    name: string,
    options: {
      description?: string;
      userId?: string;
      agentId?: string;
      resourceQuota?: Partial<ResourceQuota>;
      persistenceEnabled?: boolean;
      autoSave?: boolean;
      metadata?: Record<string, unknown>;
    } = {}
  ): Promise<string> {
    if (!this.isInitialized) {
      // Optionally, load sessions from storage
      const loaded = await this.storage.loadSessions();
      for (const [id, config] of Object.entries(loaded)) {
        this.sessions.set(id, config);
      }
      this.isInitialized = true;
    }

    // Check session limit
    const maxSessions = this.options.maxSessions || 50;
    if (this.sessions.size >= maxSessions) {
      throw new Error(`Maximum number of sessions (${maxSessions}) reached`);
    }

    const sessionId = crypto.randomUUID ? crypto.randomUUID() : (Math.random().toString(36).slice(2) + Date.now());
    const now = Date.now();

    const defaultQuota: ResourceQuota = {
      maxConcurrentQueries: 5,
      maxMemoryUsageMB: 256,
      maxCpuUsagePercent: 50,
      maxQueryDurationMs: 30000,
      maxQueueSize: 50,
      ...options.resourceQuota,
    };

    const sessionConfig: SessionConfig = {
      id: sessionId,
      name,
      description: options.description as string | undefined,
      userId: options.userId as string | undefined,
      agentId: options.agentId as string | undefined,
      createdAt: now,
      lastAccessedAt: now,
      isActive: false,
      metadata: options.metadata as Record<string, unknown> | undefined,
      resourceQuota: defaultQuota,
      persistenceEnabled: options.persistenceEnabled ?? true,
      autoSave: options.autoSave ?? true,
      maxIdleTime: this.options.maxIdleTime ?? 24 * 60 * 60 * 1000,
    };

    const sessionState: SessionState = {
      sessionId,
      prologFacts: [],
      prologRules: [],
      loadedFiles: [],
      consultedModules: [],
      rdfTriples: [],
      variables: {},
      customPredicates: [],
      timestamp: now,
    };

    this.sessions.set(sessionId, sessionConfig);
    this.sessionStates.set(sessionId, sessionState);

    // Persist
    if (sessionConfig.persistenceEnabled) {
      const allSessions: Record<string, SessionConfig> = {};
      for (const [id, config] of this.sessions.entries()) {
        allSessions[id] = config;
      }
      await this.storage.saveSessions(allSessions);
      await this.storage.saveState(sessionId, sessionState);
    }

    this.emit('sessionCreated', { sessionId, config: sessionConfig });
    return sessionId;
  }


  async switchToSession(sessionId: string): Promise<void> {
    if (!this.isInitialized) {
      const loaded = await this.storage.loadSessions();
      for (const [id, config] of Object.entries(loaded)) {
        this.sessions.set(id, config);
      }
      this.isInitialized = true;
    }
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error(`Session ${sessionId} not found`);
    }
    // Deactivate current session
    let previousSessionId = this.activeSession;
    if (this.activeSession) {
      const currentSession = this.sessions.get(this.activeSession);
      if (currentSession) {
        currentSession.isActive = false;
        currentSession.lastAccessedAt = Date.now();
        // Persist current session state if needed
        if (currentSession.autoSave) {
          const state = this.sessionStates.get(this.activeSession);
          if (state) await this.storage.saveState(this.activeSession, state);
        }
      }
    }
    // Activate new session
    session.isActive = true;
    session.lastAccessedAt = Date.now();
    this.activeSession = sessionId;
    // Load session state
    let state = this.sessionStates.get(sessionId);
    if (!state) {
      state = await this.storage.loadState(sessionId) ?? {
        sessionId,
        prologFacts: [],
        prologRules: [],
        loadedFiles: [],
        consultedModules: [],
        rdfTriples: [],
        variables: {},
        customPredicates: [],
        timestamp: Date.now(),
      };
      this.sessionStates.set(sessionId, state);
    }
    this.emit('sessionSwitched', {
      previousSessionId,
      currentSessionId: sessionId,
      sessionConfig: session,
    });
  }
  async deleteSession(sessionId: string): Promise<boolean> {
    if (!this.isInitialized) {
      const loaded = await this.storage.loadSessions();
      for (const [id, config] of Object.entries(loaded)) {
        this.sessions.set(id, config);
      }
      this.isInitialized = true;
    }
    const session = this.sessions.get(sessionId);
    if (!session) return false;
    if (sessionId === this.activeSession) {
      throw new Error('Cannot delete active session. Switch to another session first.');
    }
    this.sessions.delete(sessionId);
    this.sessionStates.delete(sessionId);
    await this.storage.deleteSession(sessionId);
    // Persist updated sessions
    const allSessions: Record<string, SessionConfig> = {};
    for (const [id, config] of this.sessions.entries()) {
      allSessions[id] = config;
    }
    await this.storage.saveSessions(allSessions);
    this.emit('sessionDeleted', { sessionId, config: session });
    return true;
  }

  listSessions(filter?: {
    userId?: string;
    agentId?: string;
    isActive?: boolean;
    includeInactive?: boolean;
  }): Array<{ sessionId: string; config: SessionConfig }> {
    let sessions = Array.from(this.sessions.entries());
    if (filter) {
      sessions = sessions.filter(([_, config]) => {
        if (filter.userId && config.userId !== filter.userId) return false;
        if (filter.agentId && config.agentId !== filter.agentId) return false;
        if (filter.isActive !== undefined && config.isActive !== filter.isActive) return false;
        if (!filter.includeInactive && !config.isActive) return false;
        return true;
      });
    }
    return sessions.map(([sessionId, config]) => ({ sessionId, config }));
  }

  async createSnapshot(sessionId: string, name: string, description?: string): Promise<string> {
    const session = this.sessions.get(sessionId);
    const state = this.sessionStates.get(sessionId);
    if (!session || !state) {
      throw new Error(`Session ${sessionId} not found`);
    }
    const snapshotId = crypto.randomUUID ? crypto.randomUUID() : (Math.random().toString(36).slice(2) + Date.now());
    const snapshot: SessionSnapshot = {
      sessionId,
      name,
      description: description as string | undefined,
      state: { ...state, timestamp: Date.now() },
      createdAt: Date.now(),
      metadata: { ...session.metadata },
    };
    await this.storage.saveSnapshot(snapshotId, snapshot);
    this.emit('snapshotCreated', { snapshotId, snapshot });
    return snapshotId;
  }

  async loadSnapshot(snapshotId: string): Promise<SessionSnapshot | null> {
    return this.storage.loadSnapshot(snapshotId);
  }

  getCurrentSession(): { sessionId: string; config: SessionConfig; state: SessionState } | null {
    if (!this.activeSession) return null;
    const config = this.sessions.get(this.activeSession);
    const state = this.sessionStates.get(this.activeSession);
    if (!config || !state) return null;
    return { sessionId: this.activeSession, config, state };
  }

  // Add more migrated methods as needed
}
