import { expect } from 'chai';
import * as fs from 'fs';
import * as path from 'path';
import { ConcurrencyManager } from '../src/features/concurrencyManager.js';
import { QueryHistoryManager } from '../src/features/queryHistoryManager.js';
import { SessionManager } from '../src/features/sessionManager.js';
import { PrologBackend } from '../src/prologBackend.js';

describe('Session Management System', function () {
  this.timeout(30000);

  let sessionManager: SessionManager;
  let concurrencyManager: ConcurrencyManager;
  let historyManager: QueryHistoryManager;
  let prologBackend: PrologBackend;
  let testStorageDir: string;

  beforeEach(async function () {
    // Create temporary storage directory
    testStorageDir = path.join(__dirname, 'temp-sessions-' + Date.now());

    // Initialize managers
    concurrencyManager = new ConcurrencyManager();
    historyManager = new QueryHistoryManager({
      storageDir: path.join(testStorageDir, 'history')
    });

    sessionManager = new SessionManager({
      storageDir: testStorageDir,
      maxSessions: 10,
      enablePersistence: true,
      enableAutoSave: false, // Disable for testing
      autoCleanupInterval: 60000 // 1 minute for testing
    });

    sessionManager.setIntegrationManagers(concurrencyManager, historyManager);

    // Wait for initialization
    await new Promise(resolve => {
      if (sessionManager['isInitialized']) {
        resolve(undefined);
      } else {
        sessionManager.once('initialized', resolve);
      }
    });
  });

  afterEach(async function () {
    // Clean up
    if (sessionManager) {
      sessionManager.dispose();
    }
    if (concurrencyManager) {
      concurrencyManager.dispose();
    }
    if (historyManager) {
      historyManager.dispose();
    }
    if (prologBackend) {
      prologBackend.stop();
    }

    // Remove test storage directory
    if (fs.existsSync(testStorageDir)) {
      fs.rmSync(testStorageDir, { recursive: true, force: true });
    }
  });

  describe('Session Creation and Management', function () {
    it('should create a new session', async function () {
      const sessionId = await sessionManager.createSession('Test Session', {
        description: 'A test session',
        userId: 'user123',
        metadata: { test: true }
      });

      expect(sessionId).to.be.a('string');
      expect(sessionId).to.have.length.greaterThan(0);

      const session = sessionManager.getSession(sessionId);
      expect(session).to.not.be.null;
      expect(session!.config.name).to.equal('Test Session');
      expect(session!.config.description).to.equal('A test session');
      expect(session!.config.userId).to.equal('user123');
      expect(session!.config.metadata).to.deep.equal({ test: true });
    });

    it('should list sessions', async function () {
      const sessionId1 = await sessionManager.createSession('Session 1', { userId: 'user1' });
      const sessionId2 = await sessionManager.createSession('Session 2', { userId: 'user2' });

      const sessions = sessionManager.listSessions();
      expect(sessions).to.have.length(2);

      const sessionIds = sessions.map(s => s.sessionId);
      expect(sessionIds).to.include(sessionId1);
      expect(sessionIds).to.include(sessionId2);
    });

    it('should filter sessions by user', async function () {
      await sessionManager.createSession('Session 1', { userId: 'user1' });
      await sessionManager.createSession('Session 2', { userId: 'user2' });
      await sessionManager.createSession('Session 3', { userId: 'user1' });

      const user1Sessions = sessionManager.listSessions({ userId: 'user1' });
      expect(user1Sessions).to.have.length(2);

      const user2Sessions = sessionManager.listSessions({ userId: 'user2' });
      expect(user2Sessions).to.have.length(1);
    });

    it('should switch between sessions', async function () {
      const sessionId1 = await sessionManager.createSession('Session 1');
      const sessionId2 = await sessionManager.createSession('Session 2');

      // Switch to session 1
      await sessionManager.switchToSession(sessionId1);
      let currentSession = sessionManager.getCurrentSession();
      expect(currentSession).to.not.be.null;
      expect(currentSession!.sessionId).to.equal(sessionId1);
      expect(currentSession!.config.isActive).to.be.true;

      // Switch to session 2
      await sessionManager.switchToSession(sessionId2);
      currentSession = sessionManager.getCurrentSession();
      expect(currentSession).to.not.be.null;
      expect(currentSession!.sessionId).to.equal(sessionId2);
      expect(currentSession!.config.isActive).to.be.true;

      // Verify session 1 is no longer active
      const session1 = sessionManager.getSession(sessionId1);
      expect(session1!.config.isActive).to.be.false;
    });

    it('should delete a session', async function () {
      const sessionId = await sessionManager.createSession('Test Session');

      // Verify session exists
      let session = sessionManager.getSession(sessionId);
      expect(session).to.not.be.null;

      // Delete session
      const deleted = await sessionManager.deleteSession(sessionId);
      expect(deleted).to.be.true;

      // Verify session no longer exists
      session = sessionManager.getSession(sessionId);
      expect(session).to.be.null;
    });

    it('should not delete active session', async function () {
      const sessionId = await sessionManager.createSession('Active Session');
      await sessionManager.switchToSession(sessionId);

      try {
        await sessionManager.deleteSession(sessionId);
        expect.fail('Should have thrown an error');
      } catch (error) {
        expect(error.message).to.include('Cannot delete active session');
      }
    });

    it('should enforce session limit', async function () {
      // Create sessions up to the limit
      for (let i = 0; i < 10; i++) {
        await sessionManager.createSession(`Session ${i}`);
      }

      // Try to create one more session
      try {
        await sessionManager.createSession('Excess Session');
        expect.fail('Should have thrown an error');
      } catch (error) {
        expect(error.message).to.include('Maximum number of sessions');
      }
    });
  });

  describe('Session State Management', function () {
    let sessionId: string;

    beforeEach(async function () {
      sessionId = await sessionManager.createSession('State Test Session');
      await sessionManager.switchToSession(sessionId);
    });

    it('should save and restore session state', async function () {
      // Create some test state
      const testState = {
        prologFacts: ['fact(a)', 'fact(b)'],
        prologRules: ['rule(X) :- fact(X)'],
        variables: { testVar: 'testValue' },
        rdfTriples: [{
          subject: 'http://example.org/subject',
          predicate: 'http://example.org/predicate',
          object: 'http://example.org/object'
        }]
      };

      // Save state
      await sessionManager.saveSessionState(sessionId, testState);

      // Get saved state
      const session = sessionManager.getSession(sessionId);
      expect(session).to.not.be.null;
      expect(session!.state.prologFacts).to.deep.equal(testState.prologFacts);
      expect(session!.state.prologRules).to.deep.equal(testState.prologRules);
      expect(session!.state.variables).to.deep.equal(testState.variables);
      expect(session!.state.rdfTriples).to.deep.equal(testState.rdfTriples);
    });

    it('should create and restore from snapshots', async function () {
      // Create initial state
      const initialState = {
        prologFacts: ['initial_fact(1)'],
        variables: { version: 1 }
      };
      await sessionManager.saveSessionState(sessionId, initialState);

      // Create snapshot
      const snapshotId = await sessionManager.createSnapshot(
        sessionId,
        'Initial State',
        'Snapshot of initial state'
      );
      expect(snapshotId).to.be.a('string');

      // Modify state
      const modifiedState = {
        prologFacts: ['modified_fact(2)'],
        variables: { version: 2 }
      };
      await sessionManager.saveSessionState(sessionId, modifiedState);

      // Restore from snapshot
      await sessionManager.restoreSessionState(sessionId, snapshotId);

      // Verify restored state
      const session = sessionManager.getSession(sessionId);
      expect(session!.state.prologFacts).to.deep.equal(initialState.prologFacts);
      expect(session!.state.variables).to.deep.equal(initialState.variables);
    });

    it('should persist state to disk', async function () {
      const testState = {
        prologFacts: ['persistent_fact(test)'],
        timestamp: Date.now()
      };

      await sessionManager.saveSessionState(sessionId, testState);

      // Create new session manager to test persistence
      const newSessionManager = new SessionManager({
        storageDir: testStorageDir,
        enablePersistence: true
      });

      await new Promise(resolve => {
        if (newSessionManager['isInitialized']) {
          resolve(undefined);
        } else {
          newSessionManager.once('initialized', resolve);
        }
      });

      // Verify session was loaded from disk
      const loadedSession = newSessionManager.getSession(sessionId);
      expect(loadedSession).to.not.be.null;
      expect(loadedSession!.state.prologFacts).to.deep.equal(testState.prologFacts);

      newSessionManager.dispose();
    });
  });

  describe('Resource Management Integration', function () {
    let sessionId: string;

    beforeEach(async function () {
      sessionId = await sessionManager.createSession('Resource Test Session', {
        resourceQuota: {
          maxConcurrentQueries: 3,
          maxMemoryUsageMB: 128
        }
      });
    });

    it('should create session-specific concurrency manager', async function () {
      const sessionConcurrencyManager = sessionManager.getSessionConcurrencyManager(sessionId);
      expect(sessionConcurrencyManager).to.not.be.undefined;

      const status = sessionConcurrencyManager!.getStatus();
      expect(status.resourceQuota.maxConcurrentQueries).to.equal(3);
      expect(status.resourceQuota.maxMemoryUsageMB).to.equal(128);
    });

    it('should create session-specific history manager', async function () {
      const sessionHistoryManager = sessionManager.getSessionHistoryManager(sessionId);
      expect(sessionHistoryManager).to.not.be.undefined;

      // Test adding a query to session history
      await sessionHistoryManager!.addQuery({
        id: 'test-query-1',
        cmd: 'query',
        params: { goal: 'test(X)' },
        status: 'completed',
        startTime: Date.now(),
        endTime: Date.now() + 1000
      });

      const history = await sessionHistoryManager!.getHistory();
      expect(history.entries).to.have.length(1);
      expect(history.entries[0].id).to.equal('test-query-1');
    });

    it('should update session resource quota', async function () {
      await sessionManager.updateSessionResourceQuota(sessionId, {
        maxConcurrentQueries: 5,
        maxMemoryUsageMB: 256
      });

      const session = sessionManager.getSession(sessionId);
      expect(session!.config.resourceQuota!.maxConcurrentQueries).to.equal(5);
      expect(session!.config.resourceQuota!.maxMemoryUsageMB).to.equal(256);

      const sessionConcurrencyManager = sessionManager.getSessionConcurrencyManager(sessionId);
      const status = sessionConcurrencyManager!.getStatus();
      expect(status.resourceQuota.maxConcurrentQueries).to.equal(5);
      expect(status.resourceQuota.maxMemoryUsageMB).to.equal(256);
    });

    it('should get session statistics', async function () {
      await sessionManager.switchToSession(sessionId);

      // Add some test data
      const sessionHistoryManager = sessionManager.getSessionHistoryManager(sessionId);
      await sessionHistoryManager!.addQuery({
        id: 'stats-query-1',
        cmd: 'query',
        params: { goal: 'stats_test(X)' },
        status: 'completed',
        startTime: Date.now() - 5000,
        endTime: Date.now() - 4000
      });

      const stats = sessionManager.getSessionStatistics(sessionId);
      expect(stats).to.not.be.null;
      expect(stats!.config.id).to.equal(sessionId);
      expect(stats!.uptime).to.be.greaterThan(0);
      expect(stats!.idleTime).to.be.greaterThan(0);
    });
  });

  describe('PrologBackend Integration', function () {
    beforeEach(async function () {
      // Initialize PrologBackend with session support
      prologBackend = new PrologBackend({
        port: 3061, // Use different port for testing
        sessionOptions: {
          storageDir: path.join(testStorageDir, 'backend-sessions'),
          maxSessions: 5
        }
      });

      // Start the backend
      prologBackend.start();

      // Wait for backend to be ready
      await new Promise((resolve, reject) => {
        const timeout = setTimeout(() => {
          reject(new Error('Backend startup timeout'));
        }, 10000);

        prologBackend.once('ready', () => {
          clearTimeout(timeout);
          resolve(undefined);
        });
      });
    });

    it('should create session through PrologBackend', async function () {
      const sessionId = await prologBackend.createSession('Backend Test Session', {
        description: 'Session created through PrologBackend',
        userId: 'backend-user'
      });

      expect(sessionId).to.be.a('string');

      const session = prologBackend.getSession(sessionId);
      expect(session).to.not.be.null;
      expect(session!.config.name).to.equal('Backend Test Session');
    });

    it('should switch sessions through PrologBackend', async function () {
      const sessionId1 = await prologBackend.createSession('Backend Session 1');
      const sessionId2 = await prologBackend.createSession('Backend Session 2');

      await prologBackend.switchToSession(sessionId1);
      let currentSession = prologBackend.getCurrentSession();
      expect(currentSession!.sessionId).to.equal(sessionId1);

      await prologBackend.switchToSession(sessionId2);
      currentSession = prologBackend.getCurrentSession();
      expect(currentSession!.sessionId).to.equal(sessionId2);
    });

    it('should save and restore session state through PrologBackend', async function () {
      const sessionId = await prologBackend.createSession('State Backend Session');
      await prologBackend.switchToSession(sessionId);

      // Save session state
      await prologBackend.saveCurrentSessionState();

      // Create snapshot
      const snapshotId = await prologBackend.createSessionSnapshot(
        sessionId,
        'Backend Snapshot',
        'Snapshot created through backend'
      );

      expect(snapshotId).to.be.a('string');

      // Restore from snapshot
      await prologBackend.restoreSessionState(sessionId, snapshotId);
    });

    it('should handle session events', async function () {
      let sessionCreatedEvent: any = null;
      let sessionSwitchedEvent: any = null;

      prologBackend.on('sessionCreated', (event) => {
        sessionCreatedEvent = event;
      });

      prologBackend.on('sessionSwitched', (event) => {
        sessionSwitchedEvent = event;
      });

      const sessionId = await prologBackend.createSession('Event Test Session');
      await prologBackend.switchToSession(sessionId);

      // Wait a bit for events to be processed
      await new Promise(resolve => setTimeout(resolve, 100));

      expect(sessionCreatedEvent).to.not.be.null;
      expect(sessionCreatedEvent.sessionId).to.equal(sessionId);

      expect(sessionSwitchedEvent).to.not.be.null;
      expect(sessionSwitchedEvent.currentSessionId).to.equal(sessionId);
    });
  });

  describe('Multi-Session Scenarios', function () {
    it('should handle multiple concurrent sessions', async function () {
      const sessions: string[] = [];

      // Create multiple sessions
      for (let i = 0; i < 5; i++) {
        const sessionId = await sessionManager.createSession(`Concurrent Session ${i}`, {
          userId: `user${i}`,
          resourceQuota: {
            maxConcurrentQueries: 2 + i,
            maxMemoryUsageMB: 64 * (i + 1)
          }
        });
        sessions.push(sessionId);
      }

      // Verify all sessions exist
      const allSessions = sessionManager.listSessions();
      expect(allSessions).to.have.length(5);

      // Test switching between sessions
      for (const sessionId of sessions) {
        await sessionManager.switchToSession(sessionId);
        const currentSession = sessionManager.getCurrentSession();
        expect(currentSession!.sessionId).to.equal(sessionId);
      }

      // Test session isolation - each should have its own managers
      for (const sessionId of sessions) {
        const concurrencyManager = sessionManager.getSessionConcurrencyManager(sessionId);
        const historyManager = sessionManager.getSessionHistoryManager(sessionId);

        expect(concurrencyManager).to.not.be.undefined;
        expect(historyManager).to.not.be.undefined;
      }
    });

    it('should maintain session state isolation', async function () {
      const sessionId1 = await sessionManager.createSession('Isolation Session 1');
      const sessionId2 = await sessionManager.createSession('Isolation Session 2');

      // Set different states for each session
      await sessionManager.saveSessionState(sessionId1, {
        prologFacts: ['session1_fact(a)'],
        variables: { sessionId: 1 }
      });

      await sessionManager.saveSessionState(sessionId2, {
        prologFacts: ['session2_fact(b)'],
        variables: { sessionId: 2 }
      });

      // Verify state isolation
      const session1 = sessionManager.getSession(sessionId1);
      const session2 = sessionManager.getSession(sessionId2);

      expect(session1!.state.prologFacts).to.deep.equal(['session1_fact(a)']);
      expect(session1!.state.variables.sessionId).to.equal(1);

      expect(session2!.state.prologFacts).to.deep.equal(['session2_fact(b)']);
      expect(session2!.state.variables.sessionId).to.equal(2);
    });

    it('should handle session cleanup', async function () {
      // Create sessions with short idle time
      const sessionManager2 = new SessionManager({
        storageDir: path.join(testStorageDir, 'cleanup-test'),
        maxIdleTime: 100, // 100ms for testing
        autoCleanupInterval: 200, // 200ms for testing
        enablePersistence: false
      });

      await new Promise(resolve => {
        if (sessionManager2['isInitialized']) {
          resolve(undefined);
        } else {
          sessionManager2.once('initialized', resolve);
        }
      });

      const sessionId = await sessionManager2.createSession('Cleanup Test Session');

      // Verify session exists
      let session = sessionManager2.getSession(sessionId);
      expect(session).to.not.be.null;

      // Wait for cleanup to occur
      await new Promise(resolve => setTimeout(resolve, 500));

      // Session should be cleaned up
      session = sessionManager2.getSession(sessionId);
      expect(session).to.be.null;

      sessionManager2.dispose();
    });
  });

  describe('Error Handling', function () {
    it('should handle non-existent session operations', async function () {
      const nonExistentId = 'non-existent-session-id';

      try {
        await sessionManager.switchToSession(nonExistentId);
        expect.fail('Should have thrown an error');
      } catch (error) {
        expect(error.message).to.include('not found');
      }

      try {
        await sessionManager.saveSessionState(nonExistentId);
        expect.fail('Should have thrown an error');
      } catch (error) {
        expect(error.message).to.include('not found');
      }

      const deleted = await sessionManager.deleteSession(nonExistentId);
      expect(deleted).to.be.false;
    });

    it('should handle storage errors gracefully', async function () {
      // Create session manager with invalid storage directory
      const invalidSessionManager = new SessionManager({
        storageDir: '/invalid/path/that/does/not/exist',
        enablePersistence: true
      });

      // Should still initialize but with warnings
      await new Promise(resolve => {
        if (invalidSessionManager['isInitialized']) {
          resolve(undefined);
        } else {
          invalidSessionManager.once('initialized', resolve);
        }
      });

      // Should be able to create sessions in memory
      const sessionId = await invalidSessionManager.createSession('Memory Only Session');
      expect(sessionId).to.be.a('string');

      invalidSessionManager.dispose();
    });
  });
});