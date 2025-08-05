import { EventEmitter } from 'events';
import * as fs from 'fs';
import * as path from 'path';
import { v4 as uuidv4 } from 'uuid';
import { ConcurrencyManager, ResourceQuota } from './concurrencyManager';
import { QueryHistoryManager } from './queryHistoryManager';

export interface SessionConfig {
  id: string;
  name: string;
  description?: string;
  userId?: string;
  agentId?: string;
  createdAt: number;
  lastAccessedAt: number;
  isActive: boolean;
  metadata?: Record<string, any>;
  resourceQuota?: Partial<ResourceQuota>;
  persistenceEnabled: boolean;
  autoSave: boolean;
  maxIdleTime?: number; // in milliseconds
}

export interface SessionState {
  sessionId: string;
  prologFacts: string[];
  prologRules: string[];
  loadedFiles: string[];
  consultedModules: string[];
  rdfTriples: Array<{
    subject: string;
    predicate: string;
    object: string;
    graph?: string;
  }>;
  variables: Record<string, any>;
  customPredicates: string[];
  timestamp: number;
  checksum?: string;
}

export interface SessionSnapshot {
  sessionId: string;
  name: string;
  description?: string;
  state: SessionState;
  createdAt: number;
  metadata?: Record<string, any>;
}

export interface SessionManagerOptions {
  storageDir: string;
  maxSessions: number;
  defaultResourceQuota: Partial<ResourceQuota>;
  autoCleanupInterval: number;
  maxIdleTime: number;
  enablePersistence: boolean;
  enableAutoSave: boolean;
  autoSaveInterval: number;
  compressionEnabled: boolean;
}

/**
 * Session and State Management System
 * Supports multiple concurrent sessions for different agents or users
 * with state persistence and resource isolation
 */
export class SessionManager extends EventEmitter {
  private options: SessionManagerOptions;
  private sessions: Map<string, SessionConfig> = new Map();
  private sessionStates: Map<string, SessionState> = new Map();
  private activeSession: string | null = null;
  private storageDir: string;
  private sessionsFile: string;
  private statesDir: string;
  private snapshotsDir: string;
  private cleanupInterval?: NodeJS.Timeout;
  private autoSaveInterval?: NodeJS.Timeout;
  private isInitialized: boolean = false;

  // Integration with existing managers
  private concurrencyManager?: ConcurrencyManager;
  private historyManager?: QueryHistoryManager;
  private sessionConcurrencyManagers: Map<string, ConcurrencyManager> = new Map();
  private sessionHistoryManagers: Map<string, QueryHistoryManager> = new Map();

  constructor(options: Partial<SessionManagerOptions> = {}) {
    super();
    
    this.options = {
      storageDir: path.join(process.cwd(), '.prolog-sessions'),
      maxSessions: 50,
      defaultResourceQuota: {
        maxConcurrentQueries: 5,
        maxMemoryUsageMB: 256,
        maxCpuUsagePercent: 50,
        maxQueryDurationMs: 30000,
        maxQueueSize: 50
      },
      autoCleanupInterval: 60 * 60 * 1000, // 1 hour
      maxIdleTime: 24 * 60 * 60 * 1000, // 24 hours
      enablePersistence: true,
      enableAutoSave: true,
      autoSaveInterval: 5 * 60 * 1000, // 5 minutes
      compressionEnabled: true,
      ...options
    };

    this.storageDir = this.options.storageDir;
    this.sessionsFile = path.join(this.storageDir, 'sessions.json');
    this.statesDir = path.join(this.storageDir, 'states');
    this.snapshotsDir = path.join(this.storageDir, 'snapshots');

    this.initialize();
  }

  /**
   * Initialize the session manager
   */
  private async initialize(): Promise<void> {
    try {
      // Create storage directories
      if (this.options.enablePersistence) {
        [this.storageDir, this.statesDir, this.snapshotsDir].forEach(dir => {
          if (!fs.existsSync(dir)) {
            fs.mkdirSync(dir, { recursive: true });
          }
        });

        // Load existing sessions
        await this.loadSessionsFromDisk();
      }

      // Start background processes
      this.startCleanupProcess();
      if (this.options.enableAutoSave) {
        this.startAutoSaveProcess();
      }

      this.isInitialized = true;
      this.emit('initialized');
      console.log(`[SessionManager] Initialized with ${this.sessions.size} sessions`);
    } catch (error) {
      console.error('[SessionManager] Initialization failed:', error);
      this.emit('error', error);
    }
  }

  /**
   * Set integration managers
   */
  setIntegrationManagers(
    concurrencyManager?: ConcurrencyManager,
    historyManager?: QueryHistoryManager
  ): void {
    this.concurrencyManager = concurrencyManager;
    this.historyManager = historyManager;
  }

  /**
   * Create a new session
   */
  async createSession(
    name: string,
    options: {
      description?: string;
      userId?: string;
      agentId?: string;
      resourceQuota?: Partial<ResourceQuota>;
      persistenceEnabled?: boolean;
      autoSave?: boolean;
      metadata?: Record<string, any>;
    } = {}
  ): Promise<string> {
    if (!this.isInitialized) {
      throw new Error('SessionManager not initialized');
    }

    // Check session limit
    if (this.sessions.size >= this.options.maxSessions) {
      throw new Error(`Maximum number of sessions (${this.options.maxSessions}) reached`);
    }

    const sessionId = uuidv4();
    const now = Date.now();

    const sessionConfig: SessionConfig = {
      id: sessionId,
      name,
      description: options.description,
      userId: options.userId,
      agentId: options.agentId,
      createdAt: now,
      lastAccessedAt: now,
      isActive: false,
      metadata: options.metadata,
      resourceQuota: { ...this.options.defaultResourceQuota, ...options.resourceQuota },
      persistenceEnabled: options.persistenceEnabled ?? this.options.enablePersistence,
      autoSave: options.autoSave ?? this.options.enableAutoSave,
      maxIdleTime: this.options.maxIdleTime
    };

    // Initialize session state
    const sessionState: SessionState = {
      sessionId,
      prologFacts: [],
      prologRules: [],
      loadedFiles: [],
      consultedModules: [],
      rdfTriples: [],
      variables: {},
      customPredicates: [],
      timestamp: now
    };

    // Store session and state
    this.sessions.set(sessionId, sessionConfig);
    this.sessionStates.set(sessionId, sessionState);

    // Create per-session managers if integration is enabled
    if (this.concurrencyManager) {
      const sessionConcurrencyManager = new ConcurrencyManager(sessionConfig.resourceQuota);
      this.sessionConcurrencyManagers.set(sessionId, sessionConcurrencyManager);
    }

    if (this.historyManager) {
      const sessionHistoryManager = new QueryHistoryManager({
        storageDir: path.join(this.statesDir, sessionId, 'history'),
        maxHistorySize: 1000,
        retentionDays: 7
      });
      this.sessionHistoryManagers.set(sessionId, sessionHistoryManager);
    }

    // Persist if enabled
    if (sessionConfig.persistenceEnabled) {
      await this.saveSessionToDisk(sessionId);
      await this.saveSessionState(sessionId);
    }

    this.emit('sessionCreated', { sessionId, config: sessionConfig });
    console.log(`[SessionManager] Created session ${sessionId} (${name})`);

    return sessionId;
  }

  /**
   * Switch to a different session
   */
  async switchToSession(sessionId: string): Promise<void> {
    if (!this.isInitialized) {
      throw new Error('SessionManager not initialized');
    }

    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error(`Session ${sessionId} not found`);
    }

    // Deactivate current session
    if (this.activeSession) {
      const currentSession = this.sessions.get(this.activeSession);
      if (currentSession) {
        currentSession.isActive = false;
        currentSession.lastAccessedAt = Date.now();
        
        // Auto-save current session state if enabled
        if (currentSession.autoSave) {
          await this.saveSessionState(this.activeSession);
        }
      }
    }

    // Activate new session
    session.isActive = true;
    session.lastAccessedAt = Date.now();
    this.activeSession = sessionId;

    // Load session state
    await this.loadSessionState(sessionId);

    this.emit('sessionSwitched', { 
      previousSessionId: this.activeSession, 
      currentSessionId: sessionId,
      sessionConfig: session
    });
    
    console.log(`[SessionManager] Switched to session ${sessionId} (${session.name})`);
  }

  /**
   * Get current active session
   */
  getCurrentSession(): { sessionId: string; config: SessionConfig; state: SessionState } | null {
    if (!this.activeSession) {
      return null;
    }

    const config = this.sessions.get(this.activeSession);
    const state = this.sessionStates.get(this.activeSession);

    if (!config || !state) {
      return null;
    }

    return {
      sessionId: this.activeSession,
      config,
      state
    };
  }

  /**
   * Get session by ID
   */
  getSession(sessionId: string): { config: SessionConfig; state: SessionState } | null {
    const config = this.sessions.get(sessionId);
    const state = this.sessionStates.get(sessionId);

    if (!config || !state) {
      return null;
    }

    return { config, state };
  }

  /**
   * List all sessions
   */
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

  /**
   * Delete a session
   */
  async deleteSession(sessionId: string): Promise<boolean> {
    if (!this.isInitialized) {
      throw new Error('SessionManager not initialized');
    }

    const session = this.sessions.get(sessionId);
    if (!session) {
      return false;
    }

    // Cannot delete active session
    if (sessionId === this.activeSession) {
      throw new Error('Cannot delete active session. Switch to another session first.');
    }

    // Clean up managers
    const concurrencyManager = this.sessionConcurrencyManagers.get(sessionId);
    if (concurrencyManager) {
      concurrencyManager.dispose();
      this.sessionConcurrencyManagers.delete(sessionId);
    }

    const historyManager = this.sessionHistoryManagers.get(sessionId);
    if (historyManager) {
      historyManager.dispose();
      this.sessionHistoryManagers.delete(sessionId);
    }

    // Remove from memory
    this.sessions.delete(sessionId);
    this.sessionStates.delete(sessionId);

    // Remove from disk if persistence is enabled
    if (session.persistenceEnabled) {
      await this.deleteSessionFromDisk(sessionId);
    }

    this.emit('sessionDeleted', { sessionId, config: session });
    console.log(`[SessionManager] Deleted session ${sessionId} (${session.name})`);

    return true;
  }

  /**
   * Save current session state
   */
  async saveCurrentSessionState(): Promise<void> {
    if (!this.activeSession) {
      throw new Error('No active session to save');
    }

    await this.saveSessionState(this.activeSession);
  }

  /**
   * Save session state to memory and optionally to disk
   */
  async saveSessionState(sessionId: string, state?: Partial<SessionState>): Promise<void> {
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error(`Session ${sessionId} not found`);
    }

    let currentState = this.sessionStates.get(sessionId);
    if (!currentState) {
      throw new Error(`Session state ${sessionId} not found`);
    }

    // Update state if provided
    if (state) {
      currentState = { ...currentState, ...state, timestamp: Date.now() };
      this.sessionStates.set(sessionId, currentState);
    }

    // Persist to disk if enabled
    if (session.persistenceEnabled) {
      await this.saveSessionStateToDisk(sessionId, currentState);
    }

    this.emit('sessionStateSaved', { sessionId, state: currentState });
    console.log(`[SessionManager] Saved state for session ${sessionId}`);
  }

  /**
   * Restore session state from a snapshot
   */
  async restoreSessionState(sessionId: string, snapshotId?: string): Promise<void> {
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error(`Session ${sessionId} not found`);
    }

    let state: SessionState;

    if (snapshotId) {
      // Restore from specific snapshot
      const snapshot = await this.loadSnapshot(snapshotId);
      if (!snapshot || snapshot.sessionId !== sessionId) {
        throw new Error(`Snapshot ${snapshotId} not found or doesn't belong to session ${sessionId}`);
      }
      state = snapshot.state;
    } else {
      // Restore from latest saved state
      state = await this.loadSessionStateFromDisk(sessionId);
    }

    this.sessionStates.set(sessionId, state);

    this.emit('sessionStateRestored', { sessionId, state });
    console.log(`[SessionManager] Restored state for session ${sessionId}${snapshotId ? ` from snapshot ${snapshotId}` : ''}`);
  }

  /**
   * Create a snapshot of session state
   */
  async createSnapshot(
    sessionId: string, 
    name: string, 
    description?: string
  ): Promise<string> {
    const session = this.sessions.get(sessionId);
    const state = this.sessionStates.get(sessionId);

    if (!session || !state) {
      throw new Error(`Session ${sessionId} not found`);
    }

    const snapshotId = uuidv4();
    const snapshot: SessionSnapshot = {
      sessionId,
      name,
      description,
      state: { ...state, timestamp: Date.now() },
      createdAt: Date.now(),
      metadata: { ...session.metadata }
    };

    // Save snapshot to disk
    if (session.persistenceEnabled) {
      await this.saveSnapshot(snapshotId, snapshot);
    }

    this.emit('snapshotCreated', { snapshotId, snapshot });
    console.log(`[SessionManager] Created snapshot ${snapshotId} for session ${sessionId}`);

    return snapshotId;
  }

  /**
   * Get session-specific concurrency manager
   */
  getSessionConcurrencyManager(sessionId: string): ConcurrencyManager | undefined {
    return this.sessionConcurrencyManagers.get(sessionId);
  }

  /**
   * Get session-specific history manager
   */
  getSessionHistoryManager(sessionId: string): QueryHistoryManager | undefined {
    return this.sessionHistoryManagers.get(sessionId);
  }

  /**
   * Update session resource quota
   */
  async updateSessionResourceQuota(sessionId: string, quota: Partial<ResourceQuota>): Promise<void> {
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error(`Session ${sessionId} not found`);
    }

    session.resourceQuota = { ...session.resourceQuota, ...quota };
    session.lastAccessedAt = Date.now();

    // Update session-specific concurrency manager
    const concurrencyManager = this.sessionConcurrencyManagers.get(sessionId);
    if (concurrencyManager) {
      concurrencyManager.updateResourceQuota(quota);
    }

    // Persist changes
    if (session.persistenceEnabled) {
      await this.saveSessionToDisk(sessionId);
    }

    this.emit('sessionResourceQuotaUpdated', { sessionId, quota });
    console.log(`[SessionManager] Updated resource quota for session ${sessionId}`);
  }

  /**
   * Get session statistics
   */
  getSessionStatistics(sessionId: string): {
    config: SessionConfig;
    state: SessionState;
    concurrencyStats?: any;
    historyStats?: any;
    uptime: number;
    idleTime: number;
  } | null {
    const session = this.sessions.get(sessionId);
    const state = this.sessionStates.get(sessionId);

    if (!session || !state) {
      return null;
    }

    const now = Date.now();
    const concurrencyManager = this.sessionConcurrencyManagers.get(sessionId);
    const historyManager = this.sessionHistoryManagers.get(sessionId);

    return {
      config: session,
      state,
      concurrencyStats: concurrencyManager?.getStatus(),
      historyStats: historyManager ? await historyManager.getStatistics() : undefined,
      uptime: now - session.createdAt,
      idleTime: now - session.lastAccessedAt
    };
  }

  // Private methods for persistence

  private async loadSessionsFromDisk(): Promise<void> {
    if (!fs.existsSync(this.sessionsFile)) {
      return;
    }

    try {
      const data = fs.readFileSync(this.sessionsFile, 'utf8');
      const sessionsData = JSON.parse(data);

      for (const [sessionId, config] of Object.entries(sessionsData)) {
        this.sessions.set(sessionId, config as SessionConfig);
        
        // Load session state
        try {
          const state = await this.loadSessionStateFromDisk(sessionId);
          this.sessionStates.set(sessionId, state);
        } catch (error) {
          console.warn(`[SessionManager] Could not load state for session ${sessionId}:`, error);
        }
      }

      console.log(`[SessionManager] Loaded ${this.sessions.size} sessions from disk`);
    } catch (error) {
      console.error('[SessionManager] Error loading sessions from disk:', error);
    }
  }

  private async saveSessionToDisk(sessionId: string): Promise<void> {
    try {
      const sessionsData: Record<string, SessionConfig> = {};
      for (const [id, config] of this.sessions.entries()) {
        sessionsData[id] = config;
      }

      fs.writeFileSync(this.sessionsFile, JSON.stringify(sessionsData, null, 2), 'utf8');
    } catch (error) {
      console.error(`[SessionManager] Error saving session ${sessionId} to disk:`, error);
    }
  }

  private async loadSessionState(sessionId: string): Promise<void> {
    const session = this.sessions.get(sessionId);
    if (!session || !session.persistenceEnabled) {
      return;
    }

    try {
      const state = await this.loadSessionStateFromDisk(sessionId);
      this.sessionStates.set(sessionId, state);
    } catch (error) {
      console.warn(`[SessionManager] Could not load state for session ${sessionId}:`, error);
    }
  }

  private async loadSessionStateFromDisk(sessionId: string): Promise<SessionState> {
    const stateFile = path.join(this.statesDir, `${sessionId}.json`);
    
    if (!fs.existsSync(stateFile)) {
      throw new Error(`State file not found for session ${sessionId}`);
    }

    const data = fs.readFileSync(stateFile, 'utf8');
    return JSON.parse(data);
  }

  private async saveSessionStateToDisk(sessionId: string, state: SessionState): Promise<void> {
    const stateFile = path.join(this.statesDir, `${sessionId}.json`);
    
    try {
      fs.writeFileSync(stateFile, JSON.stringify(state, null, 2), 'utf8');
    } catch (error) {
      console.error(`[SessionManager] Error saving state for session ${sessionId}:`, error);
    }
  }

  private async deleteSessionFromDisk(sessionId: string): Promise<void> {
    try {
      // Remove state file
      const stateFile = path.join(this.statesDir, `${sessionId}.json`);
      if (fs.existsSync(stateFile)) {
        fs.unlinkSync(stateFile);
      }

      // Remove session history directory
      const historyDir = path.join(this.statesDir, sessionId);
      if (fs.existsSync(historyDir)) {
        fs.rmSync(historyDir, { recursive: true, force: true });
      }

      // Update sessions file
      await this.saveSessionToDisk(sessionId);
    } catch (error) {
      console.error(`[SessionManager] Error deleting session ${sessionId} from disk:`, error);
    }
  }

  private async saveSnapshot(snapshotId: string, snapshot: SessionSnapshot): Promise<void> {
    const snapshotFile = path.join(this.snapshotsDir, `${snapshotId}.json`);
    
    try {
      fs.writeFileSync(snapshotFile, JSON.stringify(snapshot, null, 2), 'utf8');
    } catch (error) {
      console.error(`[SessionManager] Error saving snapshot ${snapshotId}:`, error);
    }
  }

  private async loadSnapshot(snapshotId: string): Promise<SessionSnapshot | null> {
    const snapshotFile = path.join(this.snapshotsDir, `${snapshotId}.json`);
    
    if (!fs.existsSync(snapshotFile)) {
      return null;
    }

    try {
      const data = fs.readFileSync(snapshotFile, 'utf8');
      return JSON.parse(data);
    } catch (error) {
      console.error(`[SessionManager] Error loading snapshot ${snapshotId}:`, error);
      return null;
    }
  }

  private startCleanupProcess(): void {
    this.cleanupInterval = setInterval(() => {
      this.cleanupIdleSessions();
    }, this.options.autoCleanupInterval);
  }

  private startAutoSaveProcess(): void {
    this.autoSaveInterval = setInterval(() => {
      this.autoSaveActiveSessions();
    }, this.options.autoSaveInterval);
  }

  private async cleanupIdleSessions(): Promise<void> {
    const now = Date.now();
    const sessionsToCleanup: string[] = [];

    for (const [sessionId, session] of this.sessions.entries()) {
      const idleTime = now - session.lastAccessedAt;
      
      if (!session.isActive && session.maxIdleTime && idleTime > session.maxIdleTime) {
        sessionsToCleanup.push(sessionId);
      }
    }

    for (const sessionId of sessionsToCleanup) {
      try {
        await this.deleteSession(sessionId);
        console.log(`[SessionManager] Cleaned up idle session ${sessionId}`);
      } catch (error) {
        console.error(`[SessionManager] Error cleaning up session ${sessionId}:`, error);
      }
    }

    if (sessionsToCleanup.length > 0) {
      this.emit('sessionsCleanedUp', { count: sessionsToCleanup.length });
    }
  }

  private async autoSaveActiveSessions(): Promise<void> {
    for (const [sessionId, session] of this.sessions.entries()) {
      if (session.isActive && session.autoSave) {
        try {
          await this.saveSessionState(sessionId);
        } catch (error) {
          console.error(`[SessionManager] Error auto-saving session ${sessionId}:`, error);
        }
      }
    }
  }

  /**
   * Dispose of the session manager
   */
  dispose(): void {
    if (this.cleanupInterval) {
      clearInterval(this.cleanupInterval);
    }
    if (this.autoSaveInterval) {
      clearInterval(this.autoSaveInterval);
    }

    // Dispose all session managers
    for (const manager of this.sessionConcurrencyManagers.values()) {
      manager.dispose();
    }
    for (const manager of this.sessionHistoryManagers.values()) {
      manager.dispose();
    }

    this.sessionConcurrencyManagers.clear();
    this.sessionHistoryManagers.clear();
    this.sessions.clear();
    this.sessionStates.clear();

    this.removeAllListeners();
    console.log('[SessionManager] Disposed');
  }
}