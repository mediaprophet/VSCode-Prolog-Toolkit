# AI Agent Integration Guide

## Overview

This guide provides comprehensive instructions for integrating AI agents with the VSCode Prolog Toolkit API. Whether you're building custom AI agents, integrating with existing platforms like GitHub Copilot or Claude Desktop, or developing enterprise solutions, this guide covers all the essential steps and best practices.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Integration Scenarios](#integration-scenarios)
3. [Authentication Setup](#authentication-setup)
4. [SDK Usage](#sdk-usage)
5. [WebSocket Integration](#websocket-integration)
6. [MCP Server Integration](#mcp-server-integration)
7. [Advanced Use Cases](#advanced-use-cases)
8. [Troubleshooting](#troubleshooting)
9. [Best Practices](#best-practices)

## Quick Start

### Prerequisites

1. **Install VSCode Prolog Toolkit Extension**
   ```bash
   # Install from VSCode Marketplace
   code --install-extension arthwang.prolog
   ```

2. **Install SWI-Prolog**
   - Download from [https://www.swi-prolog.org/download/stable](https://www.swi-prolog.org/download/stable)
   - Ensure it's in your system PATH

3. **Enable API Server**
   Add to your VSCode settings.json:
   ```json
   {
     "prolog.apiServer.enabled": true,
     "prolog.apiServer.port": 8080,
     "prolog.apiServer.auth.method": "local_only"
   }
   ```

### Basic Integration Test

```typescript
import { createLocalClient } from './sdk/prologApiClient';

const client = createLocalClient('http://localhost:8080');

// Test connection
const health = await client.health();
console.log('API Status:', health.status);

// Execute a simple query
const result = await client.query({
  query: 'member(X, [hello, world])'
});
console.log('Results:', result.results);
```

## Integration Scenarios

### 1. Local Development (Recommended for Getting Started)

**Use Case**: AI agents running on the same machine as VSCode

**Setup**:
```json
{
  "prolog.apiServer.enabled": true,
  "prolog.apiServer.port": 8080,
  "prolog.apiServer.host": "localhost",
  "prolog.apiServer.auth.method": "local_only",
  "prolog.apiServer.corsOrigins": ["*"]
}
```

**Advantages**:
- No network latency
- Complete privacy (no data leaves machine)
- No authentication complexity
- Full control over resources

**Example Client**:
```typescript
const client = createLocalClient('http://localhost:8080');
```

### 2. Team Development

**Use Case**: Shared Prolog service for development teams

**Setup**:
```json
{
  "prolog.apiServer.enabled": true,
  "prolog.apiServer.port": 8080,
  "prolog.apiServer.host": "0.0.0.0",
  "prolog.apiServer.auth.method": "api_key",
  "prolog.apiServer.auth.apiKeys": ["team-api-key-1", "team-api-key-2"],
  "prolog.apiServer.corsOrigins": ["https://your-team-domain.com"]
}
```

**Example Client**:
```typescript
const client = createApiKeyClient('http://team-server:8080', 'team-api-key-1');
```

### 3. Enterprise Integration

**Use Case**: Production deployment with OAuth2 authentication

**Setup**:
```json
{
  "prolog.apiServer.enabled": true,
  "prolog.apiServer.auth.method": "oauth2",
  "prolog.apiServer.auth.oauth2": {
    "providers": ["microsoft"],
    "clientId": "your-client-id",
    "clientSecret": "your-client-secret"
  },
  "prolog.apiServer.rateLimiting.enabled": true
}
```

## Authentication Setup

### API Key Authentication

1. **Generate API Key**:
   ```typescript
   import { generateApiKey } from './sdk/prologApiClient';
   const apiKey = generateApiKey();
   console.log('Generated API Key:', apiKey);
   ```

2. **Configure Server**:
   ```json
   {
     "prolog.apiServer.auth.method": "api_key",
     "prolog.apiServer.auth.apiKeys": ["your-generated-api-key"]
   }
   ```

3. **Use in Client**:
   ```typescript
   const client = createApiKeyClient('http://localhost:8080', 'your-generated-api-key');
   ```

### JWT Token Authentication

1. **Generate JWT Secret**:
   ```bash
   node -e "console.log(require('crypto').randomBytes(64).toString('hex'))"
   ```

2. **Configure Server**:
   ```json
   {
     "prolog.apiServer.auth.method": "jwt_token",
     "prolog.apiServer.auth.jwtSecret": "your-jwt-secret"
   }
   ```

3. **Generate Token**:
   ```typescript
   import jwt from 'jsonwebtoken';
   
   const token = jwt.sign(
     { 
       sub: 'ai-agent-1',
       role: 'agent',
       permissions: ['query:execute', 'session:create']
     },
     'your-jwt-secret',
     { expiresIn: '1h' }
   );
   ```

4. **Use in Client**:
   ```typescript
   const client = createJwtClient('http://localhost:8080', token);
   ```

## SDK Usage

### Basic Query Execution

```typescript
import { createLocalClient } from './sdk/prologApiClient';

const client = createLocalClient();

// Simple query
const result = await client.query({
  query: 'append([1,2], [3,4], X)'
});
console.log(result.results); // [{"X": [1,2,3,4]}]

// Query with options
const complexResult = await client.query({
  query: 'between(1, 1000, X)',
  options: {
    max_results: 10,
    timeout: 5000
  }
});
```

### Session Management

```typescript
// Create a session for maintaining context
const session = await client.createSession({
  name: 'AI Reasoning Session',
  description: 'Session for maintaining AI agent context'
});

const sessionId = session.session_id;

// Add facts to the session
await client.query({
  query: 'assert(fact(important_data))',
  session_id: sessionId
});

// Query using session context
const result = await client.query({
  query: 'fact(X)',
  session_id: sessionId
});

// Export session state for backup
const state = await client.exportSessionState(sessionId);
console.log('Session state:', state);
```

### Batch Processing

```typescript
// Execute multiple queries efficiently
const batchResult = await client.batch({
  queries: [
    'length([a,b,c], L)',
    'reverse([1,2,3], R)',
    'sort([3,1,2], S)'
  ],
  batch_options: {
    parallel: true,
    fail_fast: false
  }
});

console.log('Batch results:', batchResult.results);
```

### Advanced Reasoning

#### CLP Constraint Solving

```typescript
// Solve constraint satisfaction problems
const clpResult = await client.clpReasoning({
  constraints: [
    'X + Y + Z #= 15',
    'X #> Y',
    'Y #> Z',
    'X #> 0',
    'Y #> 0',
    'Z #> 0'
  ],
  domain: 'fd',
  variables: ['X', 'Y', 'Z']
});

console.log('CLP Solutions:', clpResult.solution);
```

#### Probabilistic Inference

```typescript
// Perform probabilistic reasoning
const probResult = await client.probabilisticReasoning({
  facts: [
    { fact: 'cloudy', probability: 0.4 },
    { fact: 'rain :- cloudy', probability: 0.8 },
    { fact: 'wet_ground :- rain', probability: 0.95 }
  ],
  query: 'wet_ground',
  samples: 10000
});

console.log('Probability of wet ground:', probResult.probability);
```

#### N3/RDF Semantic Reasoning

```typescript
// Semantic web reasoning
const n3Result = await client.n3Reasoning({
  rules: `
    @prefix : <http://example.org/> .
    { ?person :hasParent ?parent . ?parent :hasParent ?grandparent } 
    => { ?person :hasGrandparent ?grandparent } .
  `,
  data: `
    @prefix : <http://example.org/> .
    :alice :hasParent :bob .
    :bob :hasParent :charlie .
  `,
  query: 'SELECT ?person ?grandparent WHERE { ?person :hasGrandparent ?grandparent }'
});

console.log('Inferred relationships:', n3Result.results);
```

## WebSocket Integration

### Real-time Query Notifications

```typescript
import { createLocalWebSocketClient } from './sdk/prologWebSocketClient';

const wsClient = createLocalWebSocketClient('ws://localhost:8081');

// Connect to WebSocket server
await wsClient.connect();

// Subscribe to query progress
wsClient.on('queryProgress', (notification) => {
  console.log(`Query ${notification.query_id}: ${notification.progress}% complete`);
});

wsClient.on('queryComplete', (notification) => {
  console.log('Query completed:', notification.results);
});

// Execute a long-running query and monitor progress
const queryResult = await client.query({
  query: 'findall(X, between(1, 100000, X), List), length(List, L)',
  options: { stream: true }
});

// Subscribe to this specific query
await wsClient.subscribeToQuery(queryResult.query_id, ['progress', 'complete']);
```

### System Monitoring

```typescript
// Monitor system status
wsClient.on('systemStatus', (status) => {
  console.log('Active queries:', status.active_queries);
  console.log('Memory usage:', status.memory_usage);
});

await wsClient.subscribeToSystemStatus();
```

## MCP Server Integration

### Claude Desktop Integration

1. **Install MCP Server**:
   ```bash
   cd mcp-server
   npm install
   npm run build
   ```

2. **Configure Claude Desktop**:
   Add to `claude_desktop_config.json`:
   ```json
   {
     "mcpServers": {
       "prolog-toolkit": {
         "command": "node",
         "args": ["path/to/mcp-server/dist/index.js"],
         "env": {
           "PROLOG_API_URL": "http://localhost:8080",
           "PROLOG_API_KEY": "your-api-key"
         }
       }
     }
   }
   ```

3. **Use in Claude Desktop**:
   ```
   Can you help me solve this constraint problem using Prolog?
   I need to find values for X, Y, Z where:
   - X + Y + Z = 20
   - X > Y > Z > 0
   - All are integers
   ```

### Custom MCP Tools

The MCP server provides these tools to AI agents:

- `execute_prolog_query`: Execute single Prolog queries
- `execute_batch_queries`: Execute multiple queries
- `clp_constraint_solving`: Solve constraint problems
- `probabilistic_inference`: Perform probabilistic reasoning
- `n3_semantic_reasoning`: Execute semantic web reasoning
- `create_prolog_session`: Create sessions for context
- `get_system_status`: Monitor system health

## Advanced Use Cases

### 1. Code Analysis with Prolog

```typescript
// Analyze code structure using Prolog
const codeAnalysis = await client.query({
  query: `
    analyze_function(Function, Complexity) :-
      function_calls(Function, Calls),
      length(Calls, CallCount),
      function_branches(Function, Branches),
      length(Branches, BranchCount),
      Complexity is CallCount + BranchCount.
  `,
  session_id: 'code-analysis-session'
});

// Query for high-complexity functions
const complexFunctions = await client.query({
  query: 'analyze_function(F, C), C > 10',
  session_id: 'code-analysis-session'
});
```

### 2. Knowledge Graph Reasoning

```typescript
// Build and query knowledge graphs
const session = await client.createSession({
  name: 'Knowledge Graph Session'
});

// Add knowledge
await client.query({
  query: `
    assert(isa(dog, animal)),
    assert(isa(cat, animal)),
    assert(isa(animal, living_thing)),
    assert(has_property(living_thing, breathes))
  `,
  session_id: session.session_id
});

// Infer properties
const inference = await client.query({
  query: `
    infer_property(X, Property) :-
      isa(X, Category),
      has_property(Category, Property).
    
    infer_property(X, Property) :-
      isa(X, Category),
      infer_property(Category, Property).
  `,
  session_id: session.session_id
});

// Query inferred knowledge
const result = await client.query({
  query: 'infer_property(dog, breathes)',
  session_id: session.session_id
});
```

### 3. Planning and Scheduling

```typescript
// Use CLP for scheduling problems
const scheduleResult = await client.clpReasoning({
  constraints: [
    // Task durations
    'Duration_A #= 3',
    'Duration_B #= 2',
    'Duration_C #= 4',
    
    // Task dependencies (B must start after A finishes)
    'Start_B #>= Start_A + Duration_A',
    'Start_C #>= Start_B + Duration_B',
    
    // Resource constraints
    'Start_A #>= 0',
    'Start_B #>= 0',
    'Start_C #>= 0',
    
    // Minimize total time
    'Total_Time #= Start_C + Duration_C',
    'Total_Time #=< 10'
  ],
  domain: 'fd',
  variables: ['Start_A', 'Start_B', 'Start_C', 'Total_Time']
});

console.log('Optimal schedule:', scheduleResult.solution);
```

## Troubleshooting

### Common Issues

#### 1. Connection Refused
```
Error: connect ECONNREFUSED 127.0.0.1:8080
```

**Solution**:
- Ensure VSCode Prolog Toolkit extension is installed and active
- Check that API server is enabled in settings
- Verify the port number matches your configuration

#### 2. Authentication Failed
```
Error: API Error (401): Valid authentication required
```

**Solution**:
- Verify your API key or JWT token is correct
- Check that the authentication method matches server configuration
- Ensure the token hasn't expired (for JWT)

#### 3. Permission Denied
```
Error: API Error (403): Insufficient permissions for query execution
```

**Solution**:
- Check your user role and permissions
- Verify the operation is allowed for your role
- Contact administrator to adjust permissions if needed

#### 4. Query Timeout
```
Error: API Error (408): Request exceeded 30000ms timeout
```

**Solution**:
- Increase timeout in query options
- Optimize your Prolog query for better performance
- Consider breaking complex queries into smaller parts

### Debug Mode

Enable debug logging:

```typescript
const client = createLocalClient('http://localhost:8080');

client.on('requestStart', (config) => {
  console.log('Request:', config.method, config.url);
});

client.on('responseError', (error) => {
  console.error('Response error:', error.response?.data);
});
```

### Health Checks

Regular health monitoring:

```typescript
async function monitorHealth() {
  try {
    const health = await client.health();
    console.log('API Health:', health.status);
    
    const status = await client.getStatus();
    console.log('Active queries:', status.backend.active_queries);
    console.log('Memory usage:', status.backend.resource_usage.memory);
  } catch (error) {
    console.error('Health check failed:', error.message);
  }
}

// Check health every 30 seconds
setInterval(monitorHealth, 30000);
```

## Best Practices

### 1. Resource Management

```typescript
// Always clean up sessions when done
try {
  const session = await client.createSession({ name: 'Temp Session' });
  
  // Use session for queries
  await client.query({
    query: 'your_query_here',
    session_id: session.session_id
  });
  
} finally {
  // Clean up
  await client.deleteSession(session.session_id);
}
```

### 2. Error Handling

```typescript
async function robustQuery(query: string, maxRetries = 3) {
  for (let attempt = 1; attempt <= maxRetries; attempt++) {
    try {
      return await client.query({ query });
    } catch (error) {
      if (attempt === maxRetries) throw error;
      
      // Exponential backoff
      const delay = Math.pow(2, attempt) * 1000;
      await new Promise(resolve => setTimeout(resolve, delay));
    }
  }
}
```

### 3. Performance Optimization

```typescript
// Use batch processing for multiple queries
const queries = ['query1', 'query2', 'query3'];
const batchResult = await client.batch({
  queries,
  batch_options: { parallel: true }
});

// Use streaming for large result sets
const largeResult = await client.query({
  query: 'large_dataset_query(X)',
  options: { 
    stream: true,
    max_results: 1000
  }
});
```

### 4. Security

```typescript
// Validate inputs before sending to API
function validateQuery(query: string): boolean {
  // Check for dangerous predicates
  const dangerous = ['shell', 'system', 'halt', 'abort'];
  return !dangerous.some(pred => query.includes(pred));
}

// Use appropriate timeouts
const result = await client.query({
  query: 'your_query',
  options: { timeout: 10000 } // 10 second timeout
});
```

### 5. Monitoring and Logging

```typescript
// Log all API interactions
client.on('requestStart', (config) => {
  console.log(`[${new Date().toISOString()}] API Request: ${config.method} ${config.url}`);
});

client.on('responseSuccess', (response) => {
  console.log(`[${new Date().toISOString()}] API Success: ${response.status}`);
});

client.on('responseError', (error) => {
  console.error(`[${new Date().toISOString()}] API Error:`, error.message);
});
```

## Example Integrations

### GitHub Copilot Extension

```typescript
// Example integration with GitHub Copilot
class PrologCopilotIntegration {
  private client: PrologApiClient;
  
  constructor() {
    this.client = createLocalClient('http://localhost:8080');
  }
  
  async analyzeCode(code: string): Promise<any> {
    // Convert code to Prolog facts
    const session = await this.client.createSession({
      name: 'Code Analysis Session'
    });
    
    // Add code structure as facts
    await this.client.query({
      query: `assert_code_structure('${code}')`,
      session_id: session.session_id
    });
    
    // Analyze for patterns
    const patterns = await this.client.query({
      query: 'find_code_patterns(Pattern, Confidence)',
      session_id: session.session_id
    });
    
    return patterns.results;
  }
}
```

### Custom AI Agent

```typescript
class CustomPrologAgent {
  private client: PrologApiClient;
  private wsClient: PrologWebSocketClient;
  private sessionId: string;
  
  constructor(apiUrl: string, apiKey: string) {
    this.client = createApiKeyClient(apiUrl, apiKey);
    this.wsClient = createApiKeyWebSocketClient(apiUrl.replace('http', 'ws'), apiKey);
  }
  
  async initialize(): Promise<void> {
    // Connect WebSocket for notifications
    await this.wsClient.connect();
    
    // Create persistent session
    const session = await this.client.createSession({
      name: 'AI Agent Session',
      description: 'Persistent session for AI agent reasoning'
    });
    
    this.sessionId = session.session_id;
    
    // Load knowledge base
    await this.loadKnowledgeBase();
  }
  
  private async loadKnowledgeBase(): Promise<void> {
    const knowledge = [
      'rule(X) :- condition(X), action(X)',
      'fact(important_data)',
      'relationship(a, b, related)'
    ];
    
    for (const fact of knowledge) {
      await this.client.query({
        query: `assert(${fact})`,
        session_id: this.sessionId
      });
    }
  }
  
  async reason(query: string): Promise<any> {
    return await this.client.query({
      query,
      session_id: this.sessionId,
      options: { max_results: 100 }
    });
  }
  
  async solveConstraints(constraints: string[], variables: string[]): Promise<any> {
    return await this.client.clpReasoning({
      constraints,
      domain: 'fd',
      variables
    });
  }
}
```

## Conclusion

The VSCode Prolog Toolkit API provides a powerful foundation for AI agent integration, offering:

- **Comprehensive Prolog Access**: Full SWI-Prolog functionality via REST API
- **Advanced Reasoning**: CLP, probabilistic, and semantic web reasoning
- **Real-time Notifications**: WebSocket-based progress monitoring
- **Flexible Authentication**: Multiple authentication methods for different scenarios
- **Production Ready**: Security, rate limiting, and monitoring features
- **Easy Integration**: TypeScript SDK and MCP server support

Whether you're building simple query interfaces or complex AI reasoning systems, this API provides the tools and flexibility needed for successful integration.

For additional support and examples, refer to the complete documentation in the `docs/` directory and the GitHub repository.