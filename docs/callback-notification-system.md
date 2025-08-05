# Callback and Notification System for Long-Running Queries

This document describes the callback and notification mechanism implemented for the VSCode Prolog Toolkit to support long-running queries with real-time progress updates, cancellation, and WebSocket-based notifications.

## Overview

The callback/notification system provides:

- **Real-time progress tracking** for long-running Prolog queries
- **WebSocket-based notifications** for live updates
- **Query cancellation support** for running operations
- **Batch processing with individual query tracking**
- **Event-driven callbacks** for query lifecycle events
- **Query status management** with comprehensive statistics

## Architecture

### Core Components

1. **QueryNotificationManager** - Central manager for query tracking and notifications
2. **PrologBackend Extensions** - Enhanced backend with notification support
3. **WebSocket Server** - Real-time communication channel
4. **Prolog Server Integration** - Server-side query tracking

### Query Lifecycle

```
Pending → Running → [Progress Updates] → Completed/Error/Cancelled/Timeout
```

## Usage Examples

### Basic Query with Callbacks

```typescript
import { PrologBackend } from './src/prologBackend';

const backend = new PrologBackend({
  port: 3060,
  notificationOptions: {
    enableWebSocket: true,
    webSocketPort: 3062
  }
});

// Start backend
await new Promise(resolve => {
  backend.on('ready', resolve);
  backend.start();
});

// Execute query with callbacks
const response = await backend.sendRequestWithNotifications('query', {
  goal: 'findall(X, between(1, 1000000, X), Results)',
  timeoutMs: 30000
}, {
  onProgress: (status) => {
    console.log(`Progress: ${status.progress}% - ${status.message}`);
  },
  onComplete: (status) => {
    console.log('Query completed:', status.results);
  },
  onError: (status) => {
    console.error('Query failed:', status.error);
  },
  onCancel: (status) => {
    console.log('Query was cancelled');
  }
});
```

### Batch Processing with Notifications

```typescript
const batchQueries = [
  { cmd: 'query', params: { goal: 'member(X, [1,2,3])' } },
  { cmd: 'query', params: { goal: 'between(1, 10, Y)' } },
  { cmd: 'help', params: { predicate: 'member/2' } }
];

const responses = await backend.sendRequestWithNotifications(batchQueries, {}, {
  onProgress: (status) => {
    if (status.isBatch) {
      console.log(`Batch item ${status.batchIndex + 1}/${status.totalBatchSize}: ${status.progress}%`);
    }
  },
  onComplete: (status) => {
    console.log(`Batch item completed:`, status.results);
  }
});
```

### WebSocket Client Integration

```typescript
import * as WebSocket from 'ws';

const ws = new WebSocket('ws://localhost:3062');

ws.on('open', () => {
  console.log('Connected to notification server');
});

ws.on('message', (data) => {
  const message = JSON.parse(data.toString());
  
  switch (message.type) {
    case 'query_status_updated':
      console.log('Query update:', message.query);
      break;
    case 'query_registered':
      console.log('New query registered:', message.query.query_id);
      break;
    case 'query_cleaned_up':
      console.log('Query cleaned up:', message.queryId);
      break;
  }
});

// Cancel a query via WebSocket
ws.send(JSON.stringify({
  type: 'cancel_query',
  queryId: 'some-query-id'
}));

// Get query status
ws.send(JSON.stringify({
  type: 'get_query_status',
  queryId: 'some-query-id'
}));
```

### Query Management

```typescript
// Get notification manager
const notificationManager = backend.getNotificationManager();

// Get all active queries
const activeQueries = backend.getActiveQueries();
console.log('Active queries:', activeQueries.length);

// Get query statistics
const stats = backend.getQueryStatistics();
console.log('Query stats:', stats);
// Output: { total: 10, running: 2, completed: 7, error: 1, cancelled: 0, timeout: 0 }

// Cancel a specific query
const cancelled = backend.cancelQuery('query-id-123');
if (cancelled) {
  console.log('Query cancelled successfully');
}

// Get specific query status
const status = backend.getQueryStatus('query-id-123');
if (status) {
  console.log(`Query ${status.id}: ${status.status} (${status.progress}%)`);
}
```

## API Reference

### QueryNotificationManager

#### Methods

- `registerQuery(queryId, callback?, isBatch?, batchIndex?, totalBatchSize?)` - Register a new query
- `updateQueryStatus(queryId, updates)` - Update query status
- `updateQueryProgress(queryId, progress, message?)` - Update progress
- `completeQuery(queryId, results?)` - Mark query as completed
- `failQuery(queryId, error)` - Mark query as failed
- `cancelQuery(queryId)` - Cancel a running query
- `getQueryStatus(queryId)` - Get query status
- `getActiveQueries()` - Get all active queries
- `getAllQueries()` - Get all queries
- `getStatistics()` - Get query statistics
- `cleanupCompletedQueries()` - Clean up completed queries

#### Events

- `queryRegistered` - Emitted when a query is registered
- `queryStatusUpdated` - Emitted when query status changes
- `query_running` - Emitted when query starts running
- `query_completed` - Emitted when query completes
- `query_error` - Emitted when query fails
- `query_cancelled` - Emitted when query is cancelled
- `queryCancelled` - Emitted for cancellation handling
- `queryCleanedUp` - Emitted when query is cleaned up

### PrologBackend Extensions

#### New Methods

- `sendRequestWithNotifications(cmdOrBatch, params?, callback?)` - Send request with notification support
- `getNotificationManager()` - Get notification manager instance
- `cancelQuery(queryId)` - Cancel a running query
- `getQueryStatus(queryId)` - Get query status
- `getActiveQueries()` - Get active queries
- `getQueryStatistics()` - Get query statistics

#### Configuration Options

```typescript
interface PrologBackendOptions {
  // ... existing options
  notificationOptions?: {
    enableProgress?: boolean;        // Enable progress tracking (default: true)
    progressInterval?: number;       // Progress update interval in ms (default: 1000)
    enableWebSocket?: boolean;       // Enable WebSocket server (default: false)
    webSocketPort?: number;          // WebSocket server port (default: port + 2)
  };
}
```

### QueryStatus Interface

```typescript
interface QueryStatus {
  id: string;                        // Unique query identifier
  status: 'pending' | 'running' | 'completed' | 'error' | 'cancelled' | 'timeout';
  startTime: number;                 // Query start timestamp
  endTime?: number;                  // Query end timestamp
  progress?: number;                 // Progress percentage (0-100)
  message?: string;                  // Progress message
  results?: any;                     // Query results (when completed)
  error?: any;                       // Error information (when failed)
  isBatch?: boolean;                 // Is part of batch operation
  batchIndex?: number;               // Index in batch (0-based)
  totalBatchSize?: number;           // Total batch size
}
```

### QueryCallback Interface

```typescript
interface QueryCallback {
  onProgress?: (status: QueryStatus) => void;    // Called on progress updates
  onComplete?: (status: QueryStatus) => void;    // Called when query completes
  onError?: (status: QueryStatus) => void;       // Called when query fails
  onCancel?: (status: QueryStatus) => void;      // Called when query is cancelled
}
```

## WebSocket Protocol

### Client → Server Messages

#### Cancel Query
```json
{
  "type": "cancel_query",
  "queryId": "query-id-123"
}
```

#### Get Query Status
```json
{
  "type": "get_query_status",
  "queryId": "query-id-123"
}
```

#### Get All Queries
```json
{
  "type": "get_all_queries"
}
```

### Server → Client Messages

#### Query Status Update
```json
{
  "type": "query_status_updated",
  "query": {
    "query_id": "query-id-123",
    "status": "running",
    "progress": 75,
    "message": "Processing..."
  }
}
```

#### Query Registered
```json
{
  "type": "query_registered",
  "query": {
    "id": "query-id-123",
    "status": "pending",
    "startTime": 1640995200000
  }
}
```

#### Query Status Batch
```json
{
  "type": "query_status_batch",
  "queries": [
    {
      "id": "query-1",
      "status": "running",
      "progress": 50
    },
    {
      "id": "query-2",
      "status": "completed",
      "progress": 100
    }
  ]
}
```

#### Query Cancelled
```json
{
  "type": "query_cancelled",
  "query_id": "query-id-123"
}
```

#### Query Cleaned Up
```json
{
  "type": "query_cleaned_up",
  "queryId": "query-id-123"
}
```

## Testing

Run the callback/notification tests:

```bash
npm run test:callback-notification
```

Or run all tests:

```bash
npm run test:all
```

## Performance Considerations

- **Memory Management**: Completed queries are automatically cleaned up after 30 seconds
- **WebSocket Connections**: Inactive connections are automatically removed
- **Progress Updates**: Limited to 1-second intervals to avoid overwhelming clients
- **Batch Processing**: Each batch item is tracked individually for granular control

## Error Handling

- **Connection Failures**: WebSocket connections are automatically cleaned up on errors
- **Query Timeouts**: Queries that exceed their timeout are marked as 'timeout' status
- **Cancellation**: Cancelled queries are properly cleaned up and marked as 'cancelled'
- **Server Errors**: Server-side errors are propagated through the notification system

## Integration with Existing Features

The callback/notification system integrates seamlessly with:

- **Streaming Handler**: Progress updates work with streaming queries
- **Batch Processing**: Each batch item gets individual tracking
- **Chat Integration**: Chat commands can use notification callbacks
- **LSP Extension**: LSP operations can provide progress feedback
- **N3 Reasoning**: Long-running reasoning operations get progress tracking

## Future Enhancements

Potential future improvements:

- **Persistent Query History**: Store query history in database
- **Query Scheduling**: Queue and schedule queries for execution
- **Resource Monitoring**: Track memory and CPU usage per query
- **Advanced Cancellation**: Support for partial cancellation in batch operations
- **Query Prioritization**: Priority-based query execution
- **Distributed Processing**: Support for distributed query execution