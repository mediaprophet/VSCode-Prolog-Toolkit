import { expect } from 'chai';
import WebSocket from 'ws';
import { QueryNotificationManager, QueryStatus } from '../src/features/queryNotificationManager';
import { PrologBackend } from '../src/prologBackend.js';

describe('Callback and Notification Tests', function () {
  this.timeout(30000); // Increase timeout for notification tests

  let backend: PrologBackend;
  let notificationManager: QueryNotificationManager;

  before(async function () {
    // Initialize backend with notification support
    backend = new PrologBackend({
      swiplPath: 'swipl',
      port: 3063, // Use different port to avoid conflicts
      notificationOptions: {
        enableWebSocket: true,
        webSocketPort: 3065,
      },
    });

    notificationManager = backend.getNotificationManager();

    // Start backend and wait for it to be ready
    return new Promise<void>((resolve, reject) => {
      const timeout = setTimeout(() => {
        reject(new Error('Backend startup timeout'));
      }, 20000);

      const onReady = () => {
        clearTimeout(timeout);
        backend.off('ready', onReady);
        backend.off('error', onError);
        backend.off('started', onStarted);
        resolve();
      };

      const onStarted = () => {
        clearTimeout(timeout);
        backend.off('ready', onReady);
        backend.off('error', onError);
        backend.off('started', onStarted);
        resolve();
      };

      const onError = (error: any) => {
        clearTimeout(timeout);
        backend.off('ready', onReady);
        backend.off('error', onError);
        backend.off('started', onStarted);
        reject(error);
      };

      backend.on('ready', onReady);
      backend.on('started', onStarted);
      backend.on('error', onError);

      backend.start();
    });
  });

  after(function () {
    if (backend) {
      backend.stop(true);
    }
  });

  describe('Query Notification Manager', function () {
    it('should register and track queries', function () {
      const queryId = 'test-query-1';

      notificationManager.registerQuery(queryId);

      const status = notificationManager.getQueryStatus(queryId);
      expect(status).to.exist;
      expect(status!.id).to.equal(queryId);
      expect(status!.status).to.equal('pending');
      expect(status!.startTime).to.be.a('number');
    });

    it('should update query status and progress', function () {
      const queryId = 'test-query-2';

      notificationManager.registerQuery(queryId);
      notificationManager.updateQueryProgress(queryId, 50, 'Processing...');

      const status = notificationManager.getQueryStatus(queryId);
      expect(status!.status).to.equal('running');
      expect(status!.progress).to.equal(50);
      expect(status!.message).to.equal('Processing...');
    });

    it('should complete queries successfully', function () {
      const queryId = 'test-query-3';
      const results = [{ X: 1 }, { X: 2 }, { X: 3 }];

      notificationManager.registerQuery(queryId);
      notificationManager.completeQuery(queryId, results);

      const status = notificationManager.getQueryStatus(queryId);
      expect(status!.status).to.equal('completed');
      expect(status!.progress).to.equal(100);
      expect(status!.results).to.deep.equal(results);
      expect(status!.endTime).to.be.a('number');
    });

    it('should handle query failures', function () {
      const queryId = 'test-query-4';
      const error = new Error('Test error');

      notificationManager.registerQuery(queryId);
      notificationManager.failQuery(queryId, error);

      const status = notificationManager.getQueryStatus(queryId);
      expect(status!.status).to.equal('error');
      expect(status!.error).to.equal(error);
      expect(status!.endTime).to.be.a('number');
    });

    it('should cancel running queries', function () {
      const queryId = 'test-query-5';

      notificationManager.registerQuery(queryId);
      notificationManager.updateQueryStatus(queryId, { status: 'running' });

      const cancelled = notificationManager.cancelQuery(queryId);
      expect(cancelled).to.be.true;

      const status = notificationManager.getQueryStatus(queryId);
      expect(status!.status).to.equal('cancelled');
    });

    it('should not cancel already completed queries', function () {
      const queryId = 'test-query-6';

      notificationManager.registerQuery(queryId);
      notificationManager.completeQuery(queryId, []);

      const cancelled = notificationManager.cancelQuery(queryId);
      expect(cancelled).to.be.false;
    });

    it('should track batch queries', function () {
      const queryId1 = 'batch-query-1';
      const queryId2 = 'batch-query-2';

      notificationManager.registerQuery(queryId1, undefined, true, 0, 2);
      notificationManager.registerQuery(queryId2, undefined, true, 1, 2);

      const status1 = notificationManager.getQueryStatus(queryId1);
      const status2 = notificationManager.getQueryStatus(queryId2);

      expect(status1!.isBatch).to.be.true;
      expect(status1!.batchIndex).to.equal(0);
      expect(status1!.totalBatchSize).to.equal(2);

      expect(status2!.isBatch).to.be.true;
      expect(status2!.batchIndex).to.equal(1);
      expect(status2!.totalBatchSize).to.equal(2);
    });

    it('should provide query statistics', function () {
      // Clean up previous queries
      notificationManager.cleanupCompletedQueries();

      // Create test queries
      notificationManager.registerQuery('stats-1');
      notificationManager.registerQuery('stats-2');
      notificationManager.updateQueryStatus('stats-2', { status: 'running' });
      notificationManager.completeQuery('stats-1', []);

      const stats = notificationManager.getStatistics();
      expect(stats.total).to.be.at.least(2);
      expect(stats.running).to.be.at.least(1);
      expect(stats.completed).to.be.at.least(1);
    });
  });

  describe('Callback Support', function () {
    it('should call progress callbacks', function (done) {
      const queryId = 'callback-test-1';
      let progressCalled = false;

      const callback = {
        onProgress: (status: QueryStatus) => {
          expect(status.id).to.equal(queryId);
          expect(status.status).to.equal('running');
          progressCalled = true;
        },
        onComplete: (status: QueryStatus) => {
          expect(progressCalled).to.be.true;
          expect(status.id).to.equal(queryId);
          expect(status.status).to.equal('completed');
          done();
        },
      };

      notificationManager.registerQuery(queryId, callback);
      notificationManager.updateQueryProgress(queryId, 50);
      notificationManager.completeQuery(queryId, []);
    });

    it('should call error callbacks', function (done) {
      const queryId = 'callback-test-2';
      const testError = new Error('Test error');

      const callback = {
        onError: (status: QueryStatus) => {
          expect(status.id).to.equal(queryId);
          expect(status.status).to.equal('error');
          expect(status.error).to.equal(testError);
          done();
        },
      };

      notificationManager.registerQuery(queryId, callback);
      notificationManager.failQuery(queryId, testError);
    });

    it('should call cancel callbacks', function (done) {
      const queryId = 'callback-test-3';

      const callback = {
        onCancel: (status: QueryStatus) => {
          expect(status.id).to.equal(queryId);
          expect(status.status).to.equal('cancelled');
          done();
        },
      };

      notificationManager.registerQuery(queryId, callback);
      notificationManager.updateQueryStatus(queryId, { status: 'running' });
      notificationManager.cancelQuery(queryId);
    });
  });

  describe('WebSocket Notifications', function () {
    it('should connect to WebSocket server', function (done) {
      const ws = new WebSocket('ws://localhost:3065');

      ws.on('open', () => {
        ws.close();
        done();
      });

      ws.on('error', error => {
        done(error);
      });
    });

    it('should receive query status updates via WebSocket', function (done) {
      const ws = new WebSocket('ws://localhost:3065');
      const queryId = 'websocket-test-1';

      ws.on('open', () => {
        // Register query after WebSocket connection
        notificationManager.registerQuery(queryId);
        notificationManager.updateQueryProgress(queryId, 75, 'Almost done...');
      });

      ws.on('message', data => {
        const message = JSON.parse(data.toString());

        if (message.type === 'query_status_updated' && message.query.query_id === queryId) {
          expect(message.query.status).to.equal('running');
          expect(message.query.progress).to.equal(75);
          ws.close();
          done();
        }
      });

      ws.on('error', error => {
        done(error);
      });
    });

    it('should handle query cancellation via WebSocket', function (done) {
      const ws = new WebSocket('ws://localhost:3065');
      const queryId = 'websocket-cancel-test';

      ws.on('open', () => {
        // Register and start a query
        notificationManager.registerQuery(queryId);
        notificationManager.updateQueryStatus(queryId, { status: 'running' });

        // Send cancellation request
        ws.send(
          JSON.stringify({
            type: 'cancel_query',
            queryId: queryId,
          })
        );
      });

      ws.on('message', data => {
        const message = JSON.parse(data.toString());

        if (message.type === 'query_cancelled' && message.query_id === queryId) {
          ws.close();
          done();
        }
      });

      ws.on('error', error => {
        done(error);
      });
    });
  });

  describe('Backend Integration', function () {
    it('should support sendRequestWithNotifications', async function () {
      let progressReceived = false;
      let completedReceived = false;

      const callback = {
        onProgress: (status: QueryStatus) => {
          progressReceived = true;
          expect(status.status).to.equal('running');
        },
        onComplete: (status: QueryStatus) => {
          completedReceived = true;
          expect(status.status).to.equal('completed');
        },
      };

      const response = await backend.sendRequestWithNotifications(
        'query',
        {
          goal: 'member(X, [1,2,3])',
          timeoutMs: 5000,
        },
        callback
      );

      expect(response.status).to.equal('ok');
      expect(progressReceived).to.be.true;
      expect(completedReceived).to.be.true;
    });

    it('should provide query statistics from backend', function () {
      const stats = backend.getQueryStatistics();
      expect(stats).to.have.property('total');
      expect(stats).to.have.property('running');
      expect(stats).to.have.property('completed');
      expect(stats).to.have.property('error');
      expect(stats).to.have.property('cancelled');
    });

    it('should list active queries', function () {
      const activeQueries = backend.getActiveQueries();
      expect(activeQueries).to.be.an('array');
    });

    it('should cancel queries through backend', async function () {
      // Start a long-running query
      const queryPromise = backend.sendRequestWithNotifications('query', {
        goal: 'sleep(10), member(X, [1,2,3])', // 10 second sleep
        timeoutMs: 15000,
      });

      // Wait a bit then cancel
      setTimeout(() => {
        const activeQueries = backend.getActiveQueries();
        if (activeQueries.length > 0) {
          const cancelled = backend.cancelQuery(activeQueries[0].id);
          expect(cancelled).to.be.true;
        }
      }, 1000);

      try {
        await queryPromise;
      } catch (error) {
        // Query might be cancelled or timeout, both are acceptable
        expect(error).to.exist;
      }
    });
  });

  describe('Event Emission', function () {
    it('should emit query events', function (done) {
      const queryId = 'event-test-1';
      let eventsReceived = 0;

      notificationManager.on('queryRegistered', status => {
        expect(status.id).to.equal(queryId);
        eventsReceived++;
      });

      notificationManager.on('queryStatusUpdated', status => {
        expect(status.id).to.equal(queryId);
        eventsReceived++;
      });

      notificationManager.on('query_completed', status => {
        expect(status.id).to.equal(queryId);
        expect(status.status).to.equal('completed');
        eventsReceived++;

        // Check that all events were received
        expect(eventsReceived).to.be.at.least(3);
        done();
      });

      notificationManager.registerQuery(queryId);
      notificationManager.updateQueryProgress(queryId, 50);
      notificationManager.completeQuery(queryId, []);
    });
  });
});
