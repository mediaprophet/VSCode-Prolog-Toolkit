# VSCode Prolog Toolkit API Reference

## Overview

The VSCode Prolog Toolkit API provides comprehensive access to Prolog reasoning capabilities for AI agents and external applications. This RESTful API enables:

- **Standard Prolog Query Execution**: Execute Prolog queries with full SWI-Prolog compatibility
- **Advanced Reasoning**: CLP constraint solving, probabilistic inference, and N3/RDF semantic reasoning
- **Session Management**: Create and manage isolated Prolog sessions with state persistence
- **Batch Processing**: Execute multiple queries efficiently with parallel processing options
- **Real-time Notifications**: WebSocket-based notifications for query progress and system events
- **Comprehensive Security**: Multi-method authentication, role-based access control, and resource quotas

## Base URL

```
http://localhost:8080  # Default local development
```

## Authentication

The API supports multiple authentication methods:

### API Key Authentication
Include the API key in the request header:
```http
X-API-Key: your-api-key-here
```

### JWT Token Authentication
Include the JWT token in the Authorization header:
```http
Authorization: Bearer your-jwt-token-here
```

### Local-Only Mode (Development)
For development, the API can be configured to accept requests only from localhost without authentication.

### OAuth2 (Enterprise)
OAuth2 integration with providers like GitHub, Microsoft, and Google (requires configuration).

## User Roles and Permissions

| Role | Permissions | Description |
|------|-------------|-------------|
| **admin** | All operations | Full access to all API endpoints |
| **agent** | Standard AI agent operations | Query execution, session management, advanced reasoning |
| **readonly** | Read-only operations | Query execution (read-only), status monitoring |
| **limited** | Basic operations only | Simple query execution, own session access |

## Rate Limits

Rate limits are enforced based on user role:

| Role | Requests/Minute | Concurrent Sessions |
|------|-----------------|-------------------|
| admin | Unlimited | Unlimited |
| agent | 60 | 10 |
| readonly | 30 | 5 |
| limited | 10 | 2 |

## Core Endpoints

### Health Check

Check API server health and backend status.

```http
GET /health
```

**Response:**
```json
{
  "status": "healthy",
  "timestamp": "2025-08-05T07:45:00.000Z",
  "version": "1.0.0",
  "backend": {
    "running": true,
    "port": 3060
  }
}
```

### API Information

Get API metadata and available endpoints.

```http
GET /api
```

**Response:**
```json
{
  "name": "VSCode Prolog Toolkit API",
  "version": "1.0.0",
  "description": "RESTful API for Prolog operations and AI agent integration",
  "documentation": "/api/docs",
  "endpoints": {
    "query": "POST /api/v1/query",
    "batch": "POST /api/v1/batch",
    "sessions": "GET|POST /api/v1/sessions",
    "reasoning": {
      "clp": "POST /api/v1/reasoning/clp",
      "probabilistic": "POST /api/v1/reasoning/probabilistic",
      "n3": "POST /api/v1/reasoning/n3"
    },
    "history": "GET /api/v1/history",
    "status": "GET /api/v1/status"
  }
}
```

## Query Execution

### Execute Single Query

Execute a single Prolog query with optional session context.

```http
POST /api/v1/query
```

**Request Body:**
```json
{
  "query": "member(X, [1,2,3])",
  "session_id": "optional-session-id",
  "options": {
    "timeout": 30000,
    "max_results": 100,
    "reasoning_mode": "default",
    "stream": false
  }
}
```

**Response:**
```json
{
  "query_id": "uuid-query-id",
  "success": true,
  "results": [
    {"X": 1},
    {"X": 2},
    {"X": 3}
  ],
  "execution_time": 15,
  "more_available": false,
  "cursor": null
}
```

**Query Options:**
- `timeout`: Query timeout in milliseconds (default: 30000)
- `max_results`: Maximum number of results (default: 100)
- `reasoning_mode`: Reasoning mode (`default`, `clp`, `probabilistic`, `n3`, `custom`)
- `stream`: Enable streaming for large result sets (default: false)

### Execute Batch Queries

Execute multiple Prolog queries in a single request.

```http
POST /api/v1/batch
```

**Request Body:**
```json
{
  "queries": [
    "member(X, [1,2,3])",
    {
      "query": "length([a,b,c], L)",
      "timeout": 5000
    }
  ],
  "session_id": "optional-session-id",
  "batch_options": {
    "parallel": true,
    "fail_fast": false,
    "timeout": 60000
  }
}
```

**Response:**
```json
{
  "batch_id": "uuid-batch-id",
  "results": [
    {
      "query_index": 0,
      "success": true,
      "results": [{"X": 1}, {"X": 2}, {"X": 3}],
      "error": null
    },
    {
      "query_index": 1,
      "success": true,
      "results": [{"L": 3}],
      "error": null
    }
  ],
  "total_queries": 2,
  "successful_queries": 2
}
```

## Session Management

### List Sessions

Get list of all sessions accessible to the user.

```http
GET /api/v1/sessions?include_inactive=false
```

**Response:**
```json
{
  "sessions": [
    {
      "session_id": "session-123",
      "name": "My AI Session",
      "description": "Session for AI agent reasoning",
      "created_at": "2025-08-05T07:00:00.000Z",
      "is_active": true,
      "user_id": "user-456"
    }
  ],
  "total": 1
}
```

### Create Session

Create a new Prolog session for maintaining context.

```http
POST /api/v1/sessions
```

**Request Body:**
```json
{
  "name": "My AI Session",
  "description": "Session for AI agent reasoning",
  "config": {
    "timeout": 300000,
    "max_memory": "100MB"
  }
}
```

**Response:**
```json
{
  "session_id": "session-123",
  "name": "My AI Session",
  "description": "Session for AI agent reasoning",
  "created_at": "2025-08-05T07:00:00.000Z",
  "message": "Session created successfully"
}
```

### Get Session Details

Get detailed information about a specific session.

```http
GET /api/v1/sessions/{session_id}
```

**Response:**
```json
{
  "session_id": "session-123",
  "config": {
    "name": "My AI Session",
    "description": "Session for AI agent reasoning",
    "timeout": 300000,
    "max_memory": "100MB"
  },
  "state": {
    "facts_count": 150,
    "rules_count": 25,
    "memory_usage": "15MB"
  },
  "statistics": {
    "queries_executed": 42,
    "total_execution_time": 1250,
    "last_activity": "2025-08-05T07:30:00.000Z"
  }
}
```

### Delete Session

Delete a Prolog session and all its data.

```http
DELETE /api/v1/sessions/{session_id}
```

**Response:**
```json
{
  "session_id": "session-123",
  "message": "Session deleted successfully"
}
```

### Export Session State

Export the current state of a session for backup or migration.

```http
GET /api/v1/sessions/{session_id}/state
```

**Response:**
```json
{
  "session_id": "session-123",
  "state": {
    "facts": ["fact(a)", "fact(b)"],
    "rules": ["rule(X) :- fact(X)"],
    "variables": {},
    "metadata": {}
  },
  "exported_at": "2025-08-05T07:45:00.000Z"
}
```

### Import Session State

Import state into a session from a previous export.

```http
POST /api/v1/sessions/{session_id}/state
```

**Request Body:**
```json
{
  "state": {
    "facts": ["fact(a)", "fact(b)"],
    "rules": ["rule(X) :- fact(X)"],
    "variables": {},
    "metadata": {}
  }
}
```

**Response:**
```json
{
  "session_id": "session-123",
  "message": "Session state imported successfully",
  "imported_at": "2025-08-05T07:45:00.000Z"
}
```

## Advanced Reasoning

### CLP Constraint Solving

Execute Constraint Logic Programming reasoning.

```http
POST /api/v1/reasoning/clp
```

**Request Body:**
```json
{
  "constraints": [
    "X + Y #= 10",
    "X #> Y",
    "X #> 0",
    "Y #> 0"
  ],
  "domain": "fd",
  "variables": ["X", "Y"]
}
```

**Response:**
```json
{
  "success": true,
  "domain": "fd",
  "solution": [
    {"X": 6, "Y": 4},
    {"X": 7, "Y": 3},
    {"X": 8, "Y": 2},
    {"X": 9, "Y": 1}
  ],
  "error": null
}
```

**Constraint Domains:**
- `fd`: Finite Domain constraints (integers)
- `r`: Real number constraints
- `q`: Rational number constraints

### Probabilistic Inference

Execute probabilistic reasoning with Monte Carlo sampling.

```http
POST /api/v1/reasoning/probabilistic
```

**Request Body:**
```json
{
  "facts": [
    {"fact": "rain", "probability": 0.3},
    {"fact": "sprinkler", "probability": 0.5},
    {"fact": "wet_grass :- rain", "probability": 0.9},
    {"fact": "wet_grass :- sprinkler", "probability": 0.8}
  ],
  "query": "wet_grass",
  "samples": 10000
}
```

**Response:**
```json
{
  "success": true,
  "query": "wet_grass",
  "probability": 0.69,
  "evidence": {
    "rain": 0.3,
    "sprinkler": 0.5
  },
  "samples": 10000,
  "error": null
}
```

### N3/RDF Reasoning

Execute N3/RDF semantic web reasoning.

```http
POST /api/v1/reasoning/n3
```

**Request Body:**
```json
{
  "rules": "@prefix : <http://example.org/> . { ?x :parent ?y . ?y :parent ?z } => { ?x :grandparent ?z } .",
  "data": "@prefix : <http://example.org/> . :john :parent :mary . :mary :parent :susan .",
  "query": "SELECT ?x ?z WHERE { ?x :grandparent ?z }"
}
```

**Response:**
```json
{
  "success": true,
  "query": "SELECT ?x ?z WHERE { ?x :grandparent ?z }",
  "results": [
    {"x": "john", "z": "susan"}
  ],
  "inferred_triples": [
    ":john :grandparent :susan"
  ],
  "count": 1,
  "error": null
}
```

## Query History

### Get Query History

Retrieve query execution history with filtering options.

```http
GET /api/v1/history?session_id=session-123&limit=50&offset=0&status=completed
```

**Query Parameters:**
- `session_id`: Filter by session ID
- `limit`: Maximum number of results (1-1000, default: 50)
- `offset`: Number of results to skip (default: 0)
- `status`: Filter by status (`completed`, `error`, `cancelled`)

**Response:**
```json
{
  "queries": [
    {
      "query_id": "query-456",
      "query": "member(X, [1,2,3])",
      "session_id": "session-123",
      "status": "completed",
      "execution_time": 15,
      "created_at": "2025-08-05T07:30:00.000Z",
      "results_count": 3
    }
  ],
  "total": 1,
  "limit": 50,
  "offset": 0,
  "has_more": false
}
```

## System Status

### Get System Status

Get current system status and health information.

```http
GET /api/v1/status
```

**Response:**
```json
{
  "backend": {
    "running": true,
    "active_queries": 2,
    "active_sessions": 5,
    "resource_usage": {
      "memory": "150MB",
      "cpu": "15%"
    }
  },
  "scheduler": {
    "queued_queries": 0,
    "completed_queries": 1250,
    "failed_queries": 15
  },
  "query_statistics": {
    "running": 2,
    "completed": 1250,
    "failed": 15,
    "average_execution_time": 125
  },
  "timestamp": "2025-08-05T07:45:00.000Z"
}
```

## WebSocket API

### Connection

Connect to the WebSocket server for real-time notifications:

```
ws://localhost:8081/ws
```

### Authentication

WebSocket connections require authentication via query parameters:

```
ws://localhost:8081/ws?api_key=your-api-key
ws://localhost:8081/ws?token=your-jwt-token
```

### Message Types

#### Subscribe to Query Notifications

```json
{
  "type": "subscribe",
  "query_id": "query-456",
  "event_types": ["progress", "complete", "error"]
}
```

#### Subscribe to Session Events

```json
{
  "type": "subscribe",
  "session_id": "session-123",
  "event_types": ["created", "switched", "deleted", "state_saved"]
}
```

#### Subscribe to System Status

```json
{
  "type": "subscribe",
  "event_types": ["system_status"]
}
```

### Notification Messages

#### Query Progress

```json
{
  "type": "query_progress",
  "query_id": "query-456",
  "status": "running",
  "progress": 45,
  "message": "Processing results...",
  "timestamp": "2025-08-05T07:45:00.000Z"
}
```

#### Query Complete

```json
{
  "type": "query_complete",
  "query_id": "query-456",
  "results": [{"X": 1}, {"X": 2}, {"X": 3}],
  "execution_time": 150,
  "timestamp": "2025-08-05T07:45:00.000Z"
}
```

#### Session Event

```json
{
  "type": "session_event",
  "session_id": "session-123",
  "event": "created",
  "timestamp": "2025-08-05T07:45:00.000Z"
}
```

#### System Status

```json
{
  "type": "system_status",
  "active_queries": 3,
  "active_sessions": 7,
  "memory_usage": {
    "total": "500MB",
    "used": "180MB"
  },
  "cpu_usage": 25
}
```

## Error Handling

### HTTP Status Codes

| Code | Description |
|------|-------------|
| 200 | Success |
| 201 | Created |
| 400 | Bad Request - Invalid parameters |
| 401 | Unauthorized - Authentication required |
| 403 | Forbidden - Insufficient permissions |
| 404 | Not Found - Resource not found |
| 429 | Too Many Requests - Rate limit exceeded |
| 500 | Internal Server Error |

### Error Response Format

```json
{
  "error": "Bad Request",
  "message": "Query parameter is required",
  "details": {
    "field": "query",
    "code": "MISSING_REQUIRED_FIELD"
  }
}
```

## SDK Usage Examples

### JavaScript/TypeScript

```typescript
import { createApiKeyClient } from './sdk/prologApiClient';

const client = createApiKeyClient('http://localhost:8080', 'your-api-key');

// Execute a query
const result = await client.query({
  query: 'member(X, [1,2,3])',
  options: { max_results: 10 }
});

console.log(result.results); // [{"X": 1}, {"X": 2}, {"X": 3}]

// Create a session
const session = await client.createSession({
  name: 'AI Reasoning Session',
  description: 'Session for AI agent operations'
});

// Execute CLP reasoning
const clpResult = await client.clpReasoning({
  constraints: ['X + Y #= 10', 'X #> Y'],
  domain: 'fd',
  variables: ['X', 'Y']
});
```

### WebSocket Client

```typescript
import { createApiKeyWebSocketClient } from './sdk/prologWebSocketClient';

const wsClient = createApiKeyWebSocketClient('ws://localhost:8081/ws', 'your-api-key');

await wsClient.connect();

// Subscribe to query notifications
wsClient.on('queryProgress', (notification) => {
  console.log(`Query ${notification.query_id}: ${notification.progress}%`);
});

wsClient.on('queryComplete', (notification) => {
  console.log(`Query completed:`, notification.results);
});

await wsClient.subscribeToQuery('query-id', ['progress', 'complete']);
```

## Configuration

### API Server Configuration

```json
{
  "prolog.apiServer.enabled": true,
  "prolog.apiServer.port": 8080,
  "prolog.apiServer.host": "localhost",
  "prolog.apiServer.corsOrigins": ["*"],
  "prolog.apiServer.maxConnections": 100,
  "prolog.apiServer.requestTimeout": 60000,
  "prolog.apiServer.rateLimiting.enabled": true,
  "prolog.apiServer.rateLimiting.requestsPerMinute": 60,
  "prolog.apiServer.rateLimiting.burstLimit": 10
}
```

### Authentication Configuration

```json
{
  "prolog.apiServer.auth.method": "api_key",
  "prolog.apiServer.auth.apiKeys": ["your-secure-api-key"],
  "prolog.apiServer.auth.jwtSecret": "your-jwt-secret",
  "prolog.apiServer.auth.localOnly": false
}
```

### WebSocket Configuration

```json
{
  "prolog.webSocketServer.enabled": true,
  "prolog.webSocketServer.port": 8081,
  "prolog.webSocketServer.maxConnections": 50,
  "prolog.webSocketServer.heartbeatInterval": 30
}
```

## Best Practices

### Query Optimization

1. **Use appropriate timeouts**: Set reasonable timeouts for queries to prevent resource exhaustion
2. **Limit result sets**: Use `max_results` to prevent memory issues with large result sets
3. **Use sessions**: Create sessions for related queries to maintain context and improve performance
4. **Enable streaming**: For large result sets, enable streaming to reduce memory usage

### Session Management

1. **Clean up sessions**: Delete sessions when no longer needed to free resources
2. **Export important state**: Regularly export session state for backup and recovery
3. **Use descriptive names**: Give sessions meaningful names for easier management
4. **Monitor session usage**: Track session statistics to optimize resource allocation

### Security

1. **Use strong API keys**: Generate cryptographically secure API keys
2. **Rotate credentials**: Regularly rotate API keys and JWT secrets
3. **Limit permissions**: Use the principle of least privilege for user roles
4. **Monitor usage**: Track API usage and watch for suspicious activity
5. **Use HTTPS**: Always use HTTPS in production environments

### Error Handling

1. **Handle timeouts**: Implement proper timeout handling in client applications
2. **Retry logic**: Implement exponential backoff for transient errors
3. **Validate inputs**: Validate query syntax and parameters before sending requests
4. **Monitor errors**: Track error rates and investigate patterns

## Support and Resources

- **GitHub Repository**: [VSCode Prolog Toolkit](https://github.com/arthwang/vscode-prolog)
- **OpenAPI Specification**: `/docs/openapi.yaml`
- **Issue Tracker**: GitHub Issues
- **Documentation**: `/docs/` directory

For additional support and examples, see the comprehensive documentation in the `docs/` directory.