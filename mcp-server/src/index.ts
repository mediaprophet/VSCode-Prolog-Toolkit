#!/usr/bin/env node
import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';

import axios from 'axios';
import { WebSocket } from 'ws';
import {
  BatchQueryExecutionRequestSchema,
  ClpConstraintSolvingRequestSchema,
  ListResourcesRequestSchema,
  ListToolsRequestSchema,
  N3SemanticReasoningRequestSchema,
  ProbabilisticInferenceRequestSchema,
  QueryHistoryRequestSchema,
  ReadResourceRequestSchema,
  SystemStatusRequestSchema,
  toolSchemas
} from './features/toolSchemas';

interface PrologToolkitConfig {
  apiUrl: string;
  wsUrl?: string;
  apiKey?: string;
  timeout?: number;
}

interface QueryResult {
  success: boolean;
  results?: any[];
  error?: string;
  executionTime?: number;
  metadata?: {
    queryId: string;
    timestamp: string;
    [key: string]: any;
  };
}

interface SessionInfo {
  id: string;
  name: string;
  isActive: boolean;
  createdAt: string;
  lastAccessedAt: string;
  metadata?: Record<string, any>;
}

class PrologToolkitMCPServer {
  // Register resource handlers for new resources using Zod schemas
  public registerResourceHandlers() {
    // ListResources handler
    this.server.setRequestHandler(
      ListResourcesRequestSchema,
      async () => ({
        resources: [
          {
            uri: 'prolog://sessions',
            name: 'Active Prolog Sessions',
            description: 'List of currently active Prolog sessions',
            mimeType: 'application/json'
          },
          {
            uri: 'prolog://predicates',
            name: 'Available Predicates',
            description: 'List of available Prolog predicates and their documentation',
            mimeType: 'application/json'
          },
          {
            uri: 'prolog://examples',
            name: 'Prolog Examples',
            description: 'Collection of Prolog code examples and tutorials',
            mimeType: 'text/plain'
          },
          {
            uri: 'prolog://history',
            name: 'Prolog Query History',
            description: 'History of executed Prolog queries',
            mimeType: 'application/json'
          },
          {
            uri: 'prolog://system-status',
            name: 'Prolog System Status',
            description: 'System/installation/runtime status of the Prolog backend',
            mimeType: 'application/json'
          }
        ]
      })
    );
    // ReadResource handler
    this.server.setRequestHandler(
      ReadResourceRequestSchema,
      async (req: any) => {
        const { uri } = req.params;
        switch (uri) {
          case 'prolog://history':
            // Use the queryHistory tool implementation
            return await this.queryHistory({});
          case 'prolog://system-status':
            return await this.systemStatus({});
          default:
            return { error: 'Unknown resource URI' };
        }
      }
    );
  }
  // Register all modularized tool handlers using setRequestHandler and Zod schemas
  public registerAllToolHandlers() {
    this.server.setRequestHandler(BatchQueryExecutionRequestSchema, async (req) => {
      return this.batchQueryExecution(req.params);
    });
    this.server.setRequestHandler(ClpConstraintSolvingRequestSchema, async (req) => {
      return this.clpConstraintSolving(req.params);
    });
    this.server.setRequestHandler(ProbabilisticInferenceRequestSchema, async (req) => {
      return this.probabilisticInference(req.params);
    });
    this.server.setRequestHandler(N3SemanticReasoningRequestSchema, async (req) => {
      return this.n3SemanticReasoning(req.params);
    });
    this.server.setRequestHandler(QueryHistoryRequestSchema, async (req) => {
      return this.queryHistory(req.params);
    });
    this.server.setRequestHandler(SystemStatusRequestSchema, async (req) => {
      return this.systemStatus(req.params);
    });
    // Add additional tool handlers here as needed
  }
  // Register tool schema handler for MCP protocol
  public registerToolHandlers() {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => {
      return {
        tools: toolSchemas
      };
    });
  }

  private async makeApiRequest(endpoint: string, options: any): Promise<any> {
    const config = {
      ...options,
      url: `${this.config.apiUrl}${endpoint}`,
      timeout: this.config.timeout,
      headers: {
        'Content-Type': 'application/json',
        ...(this.config.apiKey && { 'Authorization': `Bearer ${this.config.apiKey}` }),
        ...options.headers
      }
    };
    try {
      return await axios(config);
    } catch (error) {
      if (axios.isAxiosError(error)) {
        const message = error.response?.data?.message || error.message;
        throw new Error(`API request failed: ${message}`);
      }
      throw error;
    }
  }
  private server: Server;
  private config: PrologToolkitConfig;
  private wsConnection?: WebSocket;

  constructor(config: PrologToolkitConfig) {
    this.config = {
      timeout: 30000,
      ...config
    };

    this.server = new Server({
      name: 'vscode-prolog-toolkit',
      version: '1.0.0',
    });

  }


  private formatQueryResult(result: QueryResult): string {
    if (!result.success) {
      return `Query failed: ${result.error}`;
    }
    if (!result.results || result.results.length === 0) {
      return 'Query succeeded but returned no results.';
    }
    let output = `Query succeeded with ${result.results.length} result(s):\n\n`;
    result.results.forEach((res, index) => {
      output += `Result ${index + 1}:\n`;
      if (typeof res === 'object') {
        output += JSON.stringify(res, null, 2);
      } else {
        output += String(res);
      }
      output += '\n\n';
    });
    if (result.executionTime) {
      output += `Execution time: ${result.executionTime}ms`;
    }
    return output;
  }

  private formatSessionsList(sessions: SessionInfo[]): string {
    if (sessions.length === 0) {
      return 'No Prolog sessions found.';
    }
    let output = `Found ${sessions.length} Prolog session(s):\n\n`;
    sessions.forEach(session => {
      output += `• ${session.name} (${session.id})\n`;
      output += `  Status: ${session.isActive ? 'Active' : 'Inactive'}\n`;
      output += `  Created: ${session.createdAt}\n`;
      output += `  Last accessed: ${session.lastAccessedAt}\n\n`;
    });
    return output;
  }

  private formatHelpContent(help: any): string {
    if (!help || !help.topic) {
      return 'No help information found for the specified topic.';
    }
    let output = `Help for: ${help.topic}\n\n`;
    if (help.description) {
      output += `Description: ${help.description}\n\n`;
    }
    if (help.syntax) {
      output += `Syntax: ${help.syntax}\n\n`;
    }
    if (help.examples && help.examples.length > 0) {
      output += 'Examples:\n';
      help.examples.forEach((example: string, index: number) => {
        output += `${index + 1}. ${example}\n`;
      });
      output += '\n';
    }
    if (help.seeAlso && help.seeAlso.length > 0) {
      output += `See also: ${help.seeAlso.join(', ')}\n`;
    }
    return output;
  }

  public async start(): Promise<void> {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    this.registerToolHandlers();
    this.registerAllToolHandlers();
    this.registerResourceHandlers();
    console.error('VSCode Prolog Toolkit MCP Server started');
  }

  // New tool implementations
  private async batchQueryExecution(args: any): Promise<any> {
    const { queries, sessionId, timeout = 60000 } = args;
    const response = await this.makeApiRequest('/api/batch', {
      method: 'POST',
      data: { queries, sessionId, timeout }
    });
    const result = response.data;
    return {
      content: [
        {
          type: 'text',
          text: typeof result === 'string' ? result : JSON.stringify(result, null, 2)
        }
      ]
    };
  }

  private async clpConstraintSolving(args: any): Promise<any> {
    const { constraints, sessionId, timeout = 30000 } = args;
    const response = await this.makeApiRequest('/api/reasoning/clp', {
      method: 'POST',
      data: { constraints, sessionId, timeout }
    });
    const result = response.data;
    return {
      content: [
        {
          type: 'text',
          text: typeof result === 'string' ? result : JSON.stringify(result, null, 2)
        }
      ]
    };
  }

  private async probabilisticInference(args: any): Promise<any> {
    const { query, sessionId, timeout = 30000 } = args;
    const response = await this.makeApiRequest('/api/reasoning/probabilistic', {
      method: 'POST',
      data: { query, sessionId, timeout }
    });
    const result = response.data;
    return {
      content: [
        {
          type: 'text',
          text: typeof result === 'string' ? result : JSON.stringify(result, null, 2)
        }
      ]
    };
  }

  private async n3SemanticReasoning(args: any): Promise<any> {
    const { n3Input, query, timeout = 30000 } = args;
    const response = await this.makeApiRequest('/api/n3/reason', {
      method: 'POST',
      data: { n3Input, query, timeout }
    });
    const result = response.data;
    return {
      content: [
        {
          type: 'text',
          text: typeof result === 'string' ? result : JSON.stringify(result, null, 2)
        }
      ]
    };
  }

  private async queryHistory(args: any): Promise<any> {
    const { sessionId, limit = 50 } = args;
    const response = await this.makeApiRequest('/api/history', {
      method: 'GET',
      params: { sessionId, limit }
    });
    const result = response.data;
    return {
      content: [
        {
          type: 'text',
          text: typeof result === 'string' ? result : JSON.stringify(result, null, 2)
        }
      ]
    };
  }

  private async systemStatus(_args: any): Promise<any> {
    // TODO: Implement system status logic
    return { status: 'ok' };
  }
}

// Main execution
async function main(): Promise<void> {
  const config: PrologToolkitConfig = {
    apiUrl: process.env.PROLOG_TOOLKIT_API_URL || 'http://localhost:3000',
    wsUrl: process.env.PROLOG_TOOLKIT_WS_URL,
    apiKey: process.env.PROLOG_TOOLKIT_API_KEY,
    timeout: parseInt(process.env.PROLOG_TOOLKIT_TIMEOUT || '30000')
  };

  const server = new PrologToolkitMCPServer(config);
  await server.start();
}

// Handle process termination
process.on('SIGINT', () => {
  console.error('Received SIGINT, shutting down gracefully...');
  process.exit(0);
});

process.on('SIGTERM', () => {
  console.error('Received SIGTERM, shutting down gracefully...');
  process.exit(0);
});

// Start the server
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch((error) => {
    console.error('Failed to start MCP server:', error);
    process.exit(1);
  });
}

export { PrologToolkitMCPServer };

