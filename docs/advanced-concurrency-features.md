# Advanced Concurrency Features

This document describes the advanced concurrency control, persistent query history, and query scheduling features implemented in the VSCode Prolog Toolkit.

## Overview

The toolkit now includes three major advanced features:

1. **Advanced Concurrency Controls** - Resource quotas and priority queues for query execution
2. **Persistent Query History Storage** - Complete query history with filtering and statistics
3. **Query Scheduling and Queuing System** - Support for immediate, delayed, recurring, and conditional queries

## Architecture

### ConcurrencyManager

The `ConcurrencyManager` class provides advanced concurrency control with resource quotas and priority-based query queuing.

#### Features

- **Resource Quotas**: Configurable limits for concurrent queries, memory usage, CPU usage, and queue size
- **Priority Queues**: Support for low, normal, high, and critical priority levels
- **Resource Monitoring**: Real-time tracking of system resource usage
- **Query Cancellation**: Ability to cancel queued or running queries

#### Configuration

```typescript
const resourceQuota: ResourceQuota = {
  maxConcurrentQueries: 10,      // Maximum concurrent queries
  maxMemoryUsageMB: 512,         // Memory limit in MB
  maxCpuUsagePercent: 80,        // CPU usage limit in percent
  maxQueryDurationMs: 30000,     // Maximum query duration
  maxQueueSize: 100              // Maximum queue size
};

const concurrencyManager = new ConcurrencyManager(resourceQuota);
```

#### Usage

```typescript
// Queue a high-priority query with resource requirements
await concurrencyManager.queueQuery(
  'query-id',
  'my_predicate(X)',
  { timeout: 5000 },
  { level: 'high', weight: 100, timeout: 5000 },
  { memoryMB: 50, cpuPercent: 20 }
);

// Get current status
const status = concurrencyManager.getStatus();
console.log('Active queries:', status.resourceUsage.activeConcurrentQueries);
console.log('Queue size:', status.resourceUsage.queueSize);

// Cancel a query
concurrencyManager.cancelQuery('query-id');
```

### QueryHistoryManager

The `QueryHistoryManager` class provides persistent storage and management of query history with comprehensive filtering and statistics.

#### Features

- **Persistent Storage**: Queries are stored in JSONL format on disk
- **Memory Caching**: Fast access with in-memory caching
- **Filtering**: Filter by status, date range, command, priority, and tags
- **Statistics**: Comprehensive statistics including daily trends
- **Automatic Cleanup**: Configurable retention policies and size limits

#### Configuration

```typescript
const historyOptions: QueryHistoryOptions = {
  storageDir: './.prolog-history',   // Storage directory
  maxHistorySize: 10000,             // Maximum entries in memory
  compressionEnabled: true,          // Enable compression
  retentionDays: 30,                 // Keep entries for 30 days
  autoCleanup: true,                 // Enable automatic cleanup
  batchSize: 100                     // Batch size for disk writes
};

const historyManager = new QueryHistoryManager(historyOptions);
```

#### Usage

```typescript
// Add a query to history
await historyManager.addQuery({
  id: 'query-1',
  cmd: 'my_predicate(X)',
  params: { timeout: 5000 },
  status: 'completed',
  startTime: Date.now() - 1000,
  endTime: Date.now(),
  results: { bindings: [{ X: 'value1' }] },
  priority: 'high',
  metadata: { tags: ['important'], sessionId: 'session-1' }
});

// Get filtered history
const history = await historyManager.getHistory({
  status: ['completed', 'error'],
  startDate: new Date(Date.now() - 24 * 60 * 60 * 1000), // Last 24 hours
  priority: ['high', 'critical'],
  limit: 50,
  sortBy: 'startTime',
  sortOrder: 'desc'
});

// Get comprehensive statistics
const stats = await historyManager.getStatistics();
console.log('Total queries:', stats.totalQueries);
console.log('Average duration:', stats.averageDuration);
console.log('Daily stats:', stats.dailyStats);
```

### QueryScheduler

The `QueryScheduler` class provides advanced query scheduling with support for immediate, delayed, recurring, and conditional execution.

#### Features

- **Multiple Schedule Types**: Immediate, delayed, recurring, and conditional queries
- **Dependencies**: Support for query dependencies
- **Priority Integration**: Works with the concurrency manager's priority system
- **Condition Evaluation**: Custom condition evaluators for conditional queries
- **Pause/Resume**: Control over recurring queries

#### Configuration

```typescript
const schedulerOptions = {
  maxScheduledQueries: 1000,    // Maximum scheduled queries
  checkInterval: 1000,          // Check interval in ms
  enableRecurring: true,        // Enable recurring queries
  enableConditional: true,      // Enable conditional queries
  enableDependencies: true      // Enable query dependencies
};

const queryScheduler = new QueryScheduler(
  concurrencyManager,
  historyManager,
  schedulerOptions
);
```

#### Usage

```typescript
// Schedule an immediate query
await queryScheduler.scheduleQuery(
  'immediate-query',
  'my_predicate(X)',
  { arg1: 'value' },
  'immediate'
);

// Schedule a delayed query
await queryScheduler.scheduleQuery(
  'delayed-query',
  'cleanup_data',
  {},
  'delayed',
  { executeAt: Date.now() + 60000 } // Execute in 1 minute
);

// Schedule a recurring query
await queryScheduler.scheduleQuery(
  'recurring-query',
  'health_check',
  {},
  'recurring',
  { 
    interval: 300000,      // Every 5 minutes
    maxExecutions: 100     // Maximum 100 executions
  }
);

// Schedule a conditional query
await queryScheduler.scheduleQuery(
  'conditional-query',
  'process_pending_items',
  {},
  'conditional',
  { condition: 'hasWork()' }
);

// Register condition evaluator
queryScheduler.registerConditionEvaluator('conditional-query', () => {
  return getPendingItemCount() > 0;
});

// Pause/resume recurring queries
await queryScheduler.pauseRecurringQuery('recurring-query');
await queryScheduler.resumeRecurringQuery('recurring-query');
```

## Integration with PrologBackend

The new features are fully integrated into the `PrologBackend` class:

### Configuration

```typescript
const backend = new PrologBackend({
  port: 3060,
  concurrencyOptions: {
    enabled: true,
    resourceQuota: {
      maxConcurrentQueries: 5,
      maxMemoryUsageMB: 256,
      maxCpuUsagePercent: 70
    }
  },
  historyOptions: {
    storageDir: './.prolog-history',
    maxHistorySize: 5000,
    retentionDays: 14
  },
  schedulerOptions: {
    enabled: true,
    maxScheduledQueries: 500,
    enableRecurring: true,
    enableConditional: true
  }
});
```

### New Methods

```typescript
// Send request with concurrency control
const result = await backend.sendRequestWithConcurrency(
  'my_predicate(X)',
  { timeout: 5000 },
  { level: 'high', weight: 100, timeout: 5000 },
  { memoryMB: 50, cpuPercent: 20 }
);

// Schedule queries
const queryId = await backend.scheduleQuery(
  'my_predicate(X)',
  {},
  'recurring',
  { interval: 60000 }
);

// Get history and statistics
const history = await backend.getQueryHistory({ limit: 100 });
const historyStats = await backend.getQueryHistoryStatistics();
const concurrencyStatus = backend.getConcurrencyStatus();
const schedulerStats = backend.getSchedulerStatistics();

// Manage scheduled queries
await backend.pauseRecurringQuery(queryId);
await backend.resumeRecurringQuery(queryId);
await backend.cancelScheduledQuery(queryId);
```

## Events

All managers emit comprehensive events for monitoring and integration:

### ConcurrencyManager Events

- `queryQueued`: When a query is added to the queue
- `queryStarted`: When query execution begins
- `queryCompleted`: When query execution completes
- `queryCancelled`: When a query is cancelled
- `resourceUsageUpdated`: When resource usage is updated
- `resourceQuotaUpdated`: When resource quota is changed

### QueryHistoryManager Events

- `initialized`: When the manager is initialized
- `queryAdded`: When a query is added to history
- `queryUpdated`: When a query is updated
- `queryDeleted`: When a query is deleted
- `historyCleared`: When history is cleared
- `historyCleanedUp`: When old entries are cleaned up

### QueryScheduler Events

- `queryScheduled`: When a query is scheduled
- `queryScheduleExecutionStarted`: When scheduled query execution begins
- `queryScheduleCompleted`: When scheduled query completes
- `queryScheduleFailed`: When scheduled query fails
- `queryScheduleCancelled`: When scheduled query is cancelled
- `querySchedulePaused`: When recurring query is paused
- `queryScheduleResumed`: When recurring query is resumed

## Performance Considerations

### Memory Usage

- The history manager uses in-memory caching for fast access
- Configurable memory limits prevent excessive memory usage
- Automatic cleanup removes old entries based on retention policies

### Disk I/O

- Batch writing reduces disk I/O overhead
- JSONL format provides efficient append-only storage
- Compression can be enabled to reduce storage requirements

### CPU Usage

- Background processing uses configurable intervals
- Resource monitoring provides CPU usage estimates
- Priority queues ensure important queries are processed first

## Best Practices

### Resource Quotas

- Set realistic limits based on your system capabilities
- Monitor resource usage and adjust quotas as needed
- Use priority levels to ensure critical queries are processed

### Query History

- Configure appropriate retention policies to balance storage and performance
- Use filtering to efficiently query large history datasets
- Regular cleanup prevents unbounded growth

### Query Scheduling

- Use appropriate schedule types for different use cases
- Set reasonable intervals for recurring queries
- Implement proper condition evaluators for conditional queries
- Use dependencies to ensure proper execution order

## Troubleshooting

### Common Issues

1. **Queue Full**: Increase `maxQueueSize` or implement better query management
2. **Memory Issues**: Reduce `maxHistorySize` or enable more aggressive cleanup
3. **Slow Performance**: Adjust check intervals and batch sizes
4. **Storage Issues**: Check disk space and permissions for history storage

### Monitoring

Use the provided statistics and events to monitor system health:

```typescript
// Monitor resource usage
backend.on('resourceUsageUpdated', (usage) => {
  if (usage.memoryUsageMB > 400) {
    console.warn('High memory usage:', usage.memoryUsageMB);
  }
});

// Monitor query completion rates
backend.on('queryCompleted', (event) => {
  if (!event.success) {
    console.error('Query failed:', event.queryId, event.error);
  }
});
```

## Migration Guide

### From Basic to Advanced Features

1. **Update Configuration**: Add new options to your `PrologBackendOptions`
2. **Update Code**: Replace `sendRequest` calls with `sendRequestWithConcurrency` where appropriate
3. **Add Monitoring**: Implement event listeners for the new events
4. **Test Thoroughly**: Ensure your application works with the new concurrency controls

### Backward Compatibility

All existing methods continue to work unchanged. The new features are opt-in and don't affect existing functionality.