import { expect } from 'chai';
import { ConcurrencyManager, ResourceQuota } from '../src/features/concurrencyManager.js';
import { QueryHistoryManager } from '../src/features/queryHistoryManager.js';
import { QueryScheduler } from '../src/features/queryScheduler.js';

describe('Advanced Concurrency Features', () => {
  let concurrencyManager: ConcurrencyManager;
  let historyManager: QueryHistoryManager;
  let queryScheduler: QueryScheduler;

  beforeEach(() => {
    const resourceQuota: Partial<ResourceQuota> = {
      maxConcurrentQueries: 3,
      maxMemoryUsageMB: 256,
      maxCpuUsagePercent: 70,
      maxQueryDurationMs: 5000,
      maxQueueSize: 10,
    };

    concurrencyManager = new ConcurrencyManager(resourceQuota);
    historyManager = new QueryHistoryManager({
      storageDir: './test-history',
      maxHistorySize: 100,
      retentionDays: 1,
    });
    queryScheduler = new QueryScheduler(concurrencyManager, historyManager);
  });

  afterEach(() => {
    concurrencyManager.dispose();
    historyManager.dispose();
    queryScheduler.dispose();
  });

  describe('ConcurrencyManager', () => {
    it('should queue queries with different priorities', async () => {
      const highPriorityQuery = concurrencyManager.queueQuery(
        'high-1',
        'test_query',
        {},
        { level: 'high', weight: 100, timeout: 5000 }
      );

      const normalPriorityQuery = concurrencyManager.queueQuery(
        'normal-1',
        'test_query',
        {},
        { level: 'normal', weight: 10, timeout: 5000 }
      );

      const lowPriorityQuery = concurrencyManager.queueQuery(
        'low-1',
        'test_query',
        {},
        { level: 'low', weight: 1, timeout: 5000 }
      );

      // Wait a bit for queries to be queued
      await new Promise(resolve => setTimeout(resolve, 100));

      const status = concurrencyManager.getStatus();
      expect(status.queuedQueries).to.have.length(3);

      // High priority should be first
      expect(status.queuedQueries[0].priority).to.equal('high');
      expect(status.queuedQueries[1].priority).to.equal('normal');
      expect(status.queuedQueries[2].priority).to.equal('low');
    });

    it('should respect resource quotas', async () => {
      // Fill up the concurrent query limit
      const queries = [];
      for (let i = 0; i < 5; i++) {
        queries.push(concurrencyManager.queueQuery(`query-${i}`, 'test_query'));
      }

      await new Promise(resolve => setTimeout(resolve, 100));

      const status = concurrencyManager.getStatus();
      expect(status.resourceUsage.activeConcurrentQueries).to.be.at.most(3);
      expect(status.queuedQueries.length).to.be.greaterThan(0);
    });

    it('should cancel queued queries', async () => {
      const queryPromise = concurrencyManager.queueQuery('cancel-test', 'test_query');

      await new Promise(resolve => setTimeout(resolve, 50));

      const cancelled = concurrencyManager.cancelQuery('cancel-test');
      expect(cancelled).to.be.true;

      try {
        await queryPromise;
        expect.fail('Query should have been cancelled');
      } catch (error) {
        expect(error.message).to.include('cancelled');
      }
    });

    it('should update resource quotas', () => {
      const newQuota: Partial<ResourceQuota> = {
        maxConcurrentQueries: 5,
        maxMemoryUsageMB: 512,
      };

      concurrencyManager.updateResourceQuota(newQuota);
      const status = concurrencyManager.getStatus();

      expect(status.resourceQuota.maxConcurrentQueries).to.equal(5);
      expect(status.resourceQuota.maxMemoryUsageMB).to.equal(512);
    });

    it('should provide resource usage statistics', () => {
      const status = concurrencyManager.getStatus();

      expect(status.resourceUsage).to.have.property('activeConcurrentQueries');
      expect(status.resourceUsage).to.have.property('memoryUsageMB');
      expect(status.resourceUsage).to.have.property('cpuUsagePercent');
      expect(status.resourceUsage).to.have.property('queueSize');
      expect(status.resourceUsage).to.have.property('lastUpdated');
    });
  });

  describe('QueryHistoryManager', () => {
    it('should add and retrieve query history', async () => {
      const queryEntry = {
        id: 'test-query-1',
        cmd: 'test_predicate',
        params: { arg1: 'value1' },
        status: 'completed' as const,
        startTime: Date.now() - 1000,
        endTime: Date.now(),
        results: { success: true },
      };

      await historyManager.addQuery(queryEntry);

      const history = await historyManager.getHistory();
      expect(history.entries).to.have.length(1);
      expect(history.entries[0].id).to.equal('test-query-1');
      expect(history.entries[0].duration).to.be.a('number');
    });

    it('should filter query history', async () => {
      // Add multiple queries with different statuses
      const queries = [
        {
          id: 'completed-1',
          cmd: 'test_predicate',
          params: {},
          status: 'completed' as const,
          startTime: Date.now() - 2000,
          endTime: Date.now() - 1000,
        },
        {
          id: 'error-1',
          cmd: 'test_predicate',
          params: {},
          status: 'error' as const,
          startTime: Date.now() - 1000,
          endTime: Date.now(),
          error: 'Test error',
        },
      ];

      for (const query of queries) {
        await historyManager.addQuery(query);
      }

      // Filter by status
      const completedHistory = await historyManager.getHistory({
        status: ['completed'],
      });
      expect(completedHistory.entries).to.have.length(1);
      expect(completedHistory.entries[0].status).to.equal('completed');

      const errorHistory = await historyManager.getHistory({
        status: ['error'],
      });
      expect(errorHistory.entries).to.have.length(1);
      expect(errorHistory.entries[0].status).to.equal('error');
    });

    it('should provide comprehensive statistics', async () => {
      // Add various queries
      const queries = [
        {
          id: 'stat-1',
          cmd: 'test_predicate',
          params: {},
          status: 'completed' as const,
          startTime: Date.now() - 2000,
          endTime: Date.now() - 1500,
          priority: 'high',
        },
        {
          id: 'stat-2',
          cmd: 'another_predicate',
          params: {},
          status: 'error' as const,
          startTime: Date.now() - 1000,
          endTime: Date.now() - 500,
          priority: 'normal',
        },
      ];

      for (const query of queries) {
        await historyManager.addQuery(query);
      }

      const stats = await historyManager.getStatistics();

      expect(stats.totalQueries).to.equal(2);
      expect(stats.completedQueries).to.equal(1);
      expect(stats.errorQueries).to.equal(1);
      expect(stats.queriesByStatus).to.have.property('completed', 1);
      expect(stats.queriesByStatus).to.have.property('error', 1);
      expect(stats.queriesByPriority).to.have.property('high', 1);
      expect(stats.queriesByCmd).to.have.property('test_predicate', 1);
      expect(stats.dailyStats).to.be.an('array');
    });

    it('should update existing queries', async () => {
      const queryEntry = {
        id: 'update-test',
        cmd: 'test_predicate',
        params: {},
        status: 'running' as const,
        startTime: Date.now(),
      };

      await historyManager.addQuery(queryEntry);

      // Update the query
      await historyManager.updateQuery('update-test', {
        status: 'completed',
        endTime: Date.now(),
        results: { success: true },
      });

      const query = await historyManager.getQuery('update-test');
      expect(query?.status).to.equal('completed');
      expect(query?.results).to.deep.equal({ success: true });
      expect(query?.duration).to.be.a('number');
    });

    it('should delete queries', async () => {
      const queryEntry = {
        id: 'delete-test',
        cmd: 'test_predicate',
        params: {},
        status: 'completed' as const,
        startTime: Date.now() - 1000,
        endTime: Date.now(),
      };

      await historyManager.addQuery(queryEntry);

      let query = await historyManager.getQuery('delete-test');
      expect(query).to.not.be.undefined;

      const deleted = await historyManager.deleteQuery('delete-test');
      expect(deleted).to.be.true;

      query = await historyManager.getQuery('delete-test');
      expect(query).to.be.undefined;
    });
  });

  describe('QueryScheduler', () => {
    it('should schedule immediate queries', async () => {
      let executionStarted = false;
      let executionCompleted = false;

      queryScheduler.on('queryScheduleExecutionStarted', () => {
        executionStarted = true;
      });

      queryScheduler.on('queryScheduleCompleted', () => {
        executionCompleted = true;
      });

      // Mock the concurrency manager execution
      concurrencyManager.on('executeQuery', event => {
        setTimeout(() => {
          event.resolve({ success: true });
        }, 100);
      });

      await queryScheduler.scheduleQuery('immediate-test', 'test_predicate', {}, 'immediate');

      // Wait for execution
      await new Promise(resolve => setTimeout(resolve, 200));

      expect(executionStarted).to.be.true;
      expect(executionCompleted).to.be.true;
    });

    it('should schedule delayed queries', async () => {
      const executeAt = Date.now() + 500; // 500ms in the future
      let executionStarted = false;

      queryScheduler.on('queryScheduleExecutionStarted', () => {
        executionStarted = true;
      });

      // Mock the concurrency manager execution
      concurrencyManager.on('executeQuery', event => {
        event.resolve({ success: true });
      });

      await queryScheduler.scheduleQuery('delayed-test', 'test_predicate', {}, 'delayed', {
        executeAt,
      });

      // Should not execute immediately
      await new Promise(resolve => setTimeout(resolve, 200));
      expect(executionStarted).to.be.false;

      // Should execute after delay
      await new Promise(resolve => setTimeout(resolve, 400));
      expect(executionStarted).to.be.true;
    });

    it('should handle recurring queries', async () => {
      let executionCount = 0;

      queryScheduler.on('queryScheduleExecutionStarted', () => {
        executionCount++;
      });

      // Mock the concurrency manager execution
      concurrencyManager.on('executeQuery', event => {
        setTimeout(() => {
          event.resolve({ success: true });
        }, 50);
      });

      await queryScheduler.scheduleQuery('recurring-test', 'test_predicate', {}, 'recurring', {
        interval: 200, // Every 200ms
        maxExecutions: 3,
      });

      // Wait for multiple executions
      await new Promise(resolve => setTimeout(resolve, 800));

      expect(executionCount).to.equal(3);
    });

    it('should handle conditional queries', async () => {
      let condition = false;
      let executionStarted = false;

      queryScheduler.on('queryScheduleExecutionStarted', () => {
        executionStarted = true;
      });

      // Mock the concurrency manager execution
      concurrencyManager.on('executeQuery', event => {
        event.resolve({ success: true });
      });

      await queryScheduler.scheduleQuery('conditional-test', 'test_predicate', {}, 'conditional', {
        condition: 'condition',
      });

      // Register condition evaluator
      queryScheduler.registerConditionEvaluator('conditional-test', () => condition);

      // Should not execute when condition is false
      await new Promise(resolve => setTimeout(resolve, 200));
      expect(executionStarted).to.be.false;

      // Should execute when condition becomes true
      condition = true;
      await new Promise(resolve => setTimeout(resolve, 200));
      expect(executionStarted).to.be.true;
    });

    it('should cancel scheduled queries', async () => {
      await queryScheduler.scheduleQuery('cancel-scheduled-test', 'test_predicate', {}, 'delayed', {
        executeAt: Date.now() + 1000,
      });

      const queries = queryScheduler.getScheduledQueries();
      expect(queries).to.have.length(1);

      const cancelled = await queryScheduler.cancelScheduledQuery('cancel-scheduled-test');
      expect(cancelled).to.be.true;

      const queriesAfterCancel = queryScheduler.getScheduledQueries();
      expect(queriesAfterCancel).to.have.length(0);
    });

    it('should provide scheduler statistics', async () => {
      await queryScheduler.scheduleQuery('stat-test-1', 'test_predicate', {}, 'immediate');
      await queryScheduler.scheduleQuery('stat-test-2', 'test_predicate', {}, 'recurring', {
        interval: 1000,
      });
      await queryScheduler.scheduleQuery('stat-test-3', 'test_predicate', {}, 'conditional', {
        condition: 'true',
      });

      const stats = queryScheduler.getStatistics();

      expect(stats.totalScheduled).to.be.greaterThan(0);
      expect(stats.recurringQueries).to.equal(1);
      expect(stats.conditionalQueries).to.equal(1);
    });

    it('should pause and resume recurring queries', async () => {
      let executionCount = 0;

      queryScheduler.on('queryScheduleExecutionStarted', () => {
        executionCount++;
      });

      // Mock the concurrency manager execution
      concurrencyManager.on('executeQuery', event => {
        setTimeout(() => {
          event.resolve({ success: true });
        }, 50);
      });

      await queryScheduler.scheduleQuery('pause-resume-test', 'test_predicate', {}, 'recurring', {
        interval: 200,
      });

      // Let it execute once
      await new Promise(resolve => setTimeout(resolve, 300));
      const countAfterFirst = executionCount;

      // Pause the query
      const paused = await queryScheduler.pauseRecurringQuery('pause-resume-test');
      expect(paused).to.be.true;

      // Wait and check it doesn't execute
      await new Promise(resolve => setTimeout(resolve, 300));
      expect(executionCount).to.equal(countAfterFirst);

      // Resume the query
      const resumed = await queryScheduler.resumeRecurringQuery('pause-resume-test');
      expect(resumed).to.be.true;

      // Wait and check it executes again
      await new Promise(resolve => setTimeout(resolve, 300));
      expect(executionCount).to.be.greaterThan(countAfterFirst);
    });
  });

  describe('Integration Tests', () => {
    it('should integrate all systems for a complete workflow', async () => {
      let queryCompleted = false;
      let historyAdded = false;

      // Set up event listeners
      queryScheduler.on('queryScheduleCompleted', () => {
        queryCompleted = true;
      });

      historyManager.on('queryAdded', () => {
        historyAdded = true;
      });

      // Mock the concurrency manager execution
      concurrencyManager.on('executeQuery', event => {
        setTimeout(() => {
          event.resolve({ success: true, result: 'test_result' });
        }, 100);
      });

      // Schedule a query
      await queryScheduler.scheduleQuery(
        'integration-test',
        'test_predicate',
        { arg1: 'value1' },
        'immediate',
        {},
        { level: 'high', weight: 100, timeout: 5000 },
        { tags: ['integration', 'test'] }
      );

      // Wait for completion
      await new Promise(resolve => setTimeout(resolve, 300));

      expect(queryCompleted).to.be.true;
      expect(historyAdded).to.be.true;

      // Check history
      const history = await historyManager.getHistory();
      expect(history.entries).to.have.length(1);
      expect(history.entries[0].cmd).to.equal('test_predicate');
      expect(history.entries[0].status).to.equal('completed');

      // Check statistics
      const concurrencyStats = concurrencyManager.getStatus();
      const historyStats = await historyManager.getStatistics();
      const schedulerStats = queryScheduler.getStatistics();

      expect(concurrencyStats.resourceUsage).to.be.an('object');
      expect(historyStats.totalQueries).to.equal(1);
      expect(schedulerStats.completedScheduled).to.equal(1);
    });
  });
});
