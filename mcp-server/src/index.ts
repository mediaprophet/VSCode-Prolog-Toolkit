#!/usr/bin/env node

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import {
  CallToolRequestSchema,
  ErrorCode,
  ListResourcesRequestSchema,
  ListToolsRequestSchema,
  McpError,
  ReadResourceRequestSchema,
} from '@modelcontextprotocol/sdk/types.js';
import axios from 'axios';
import { WebSocket } from 'ws';

/**
 * VSCode Prolog Toolkit MCP Server
 * 
 * This server provides Model Context Protocol integration for the VSCode Prolog Toolkit,
 * enabling AI agents like Claude Desktop to interact with Prolog environments through
 * a standardized interface.
 */

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
      capabilities: {
        resources: {},
        tools: {},
      },
    });

    this.setupHandlers();
  }

  private setupHandlers(): void {
    // List available tools
    this.server.setRequestHandler(ListToolsRequestSchema, async () => {
      return {
        tools: [
          {
            name: 'execute_prolog_query',
            description: 'Execute a Prolog query and return results',
            inputSchema: {
              type: 'object',
              properties: {
                query: {
                  type: 'string',
                  description: 'The Prolog query to execute'
                },
                sessionId: {
                  type: 'string',
                  description: 'Optional session ID to execute query in specific session'
                },
                timeout: {
                  type: 'number',
                  description: 'Query timeout in milliseconds (default: 30000)'
                }
              },
              required: ['query']
            }
          },
          {
            name: 'consult_prolog_file',
            description: 'Load/consult a Prolog file into the knowledge base',
            inputSchema: {
              type: 'object',
              properties: {
                filePath: {
                  type: 'string',
                  description: 'Path to the Prolog file to consult'
                },
                sessionId: {
                  type: 'string',
                  description: 'Optional session ID to load file into specific session'
                }
              },
              required: ['filePath']
            }
          },
          {
            name: 'create_prolog_session',
            description: 'Create a new Prolog session for isolated query execution',
            inputSchema: {
              type: 'object',
              properties: {
                name: {
                  type: 'string',
                  description: 'Name for the new session'
                },
                description: {
                  type: 'string',
                  description: 'Optional description for the session'
                }
              },
              required: ['name']
            }
          },
          {
            name: 'list_prolog_sessions',
            description: 'List all available Prolog sessions',
            inputSchema: {
              type: 'object',
              properties: {
                includeInactive: {
                  type: 'boolean',
                  description: 'Include inactive sessions in the list (default: false)'
                }
              }
            }
          },
          {
            name: 'get_session_state',
            description: 'Get the current state of a Prolog session',
            inputSchema: {
              type: 'object',
              properties: {
                sessionId: {
                  type: 'string',
                  description: 'ID of the session to inspect'
                }
              },
              required: ['sessionId']
            }
          },
          {
            name: 'validate_prolog_syntax',
            description: 'Validate Prolog code syntax without executing it',
            inputSchema: {
              type: 'object',
              properties: {
                code: {
                  type: 'string',
                  description: 'Prolog code to validate'
                }
              },
              required: ['code']
            }
          },
          {
            name: 'get_prolog_help',
            description: 'Get help information about Prolog predicates or concepts',
            inputSchema: {
              type: 'object',
              properties: {
                topic: {
                  type: 'string',
                  description: 'Predicate name or concept to get help for'
                }
              },
              required: ['topic']
            }
          }
        ]
      };
    });

    // Handle tool calls
    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
      const { name, arguments: args } = request.params;

      try {
        switch (name) {
          case 'execute_prolog_query':
            return await this.executePrologQuery(args);

          case 'consult_prolog_file':
            return await this.consultPrologFile(args);

          case 'create_prolog_session':
            return await this.createPrologSession(args);

          case 'list_prolog_sessions':
            return await this.listPrologSessions(args);

          case 'get_session_state':
            return await this.getSessionState(args);

          case 'validate_prolog_syntax':
            return await this.validatePrologSyntax(args);

          case 'get_prolog_help':
            return await this.getPrologHelp(args);

          default:
            throw new McpError(
              ErrorCode.MethodNotFound,
              `Unknown tool: ${name}`
            );
        }
      } catch (error) {
        if (error instanceof McpError) {
          throw error;
        }

        throw new McpError(
          ErrorCode.InternalError,
          `Tool execution failed: ${error instanceof Error ? error.message : String(error)}`
        );
      }
    });

    // List available resources
    this.server.setRequestHandler(ListResourcesRequestSchema, async () => {
      return {
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
          }
        ]
      };
    });

    // Handle resource reads
    this.server.setRequestHandler(ReadResourceRequestSchema, async (request) => {
      const { uri } = request.params;

      try {
        switch (uri) {
          case 'prolog://sessions':
            return await this.getSessionsResource();

          case 'prolog://predicates':
            return await this.getPredicatesResource();

          case 'prolog://examples':
            return await this.getExamplesResource();

          default:
            throw new McpError(
              ErrorCode.InvalidRequest,
              `Unknown resource: ${uri}`
            );
        }
      } catch (error) {
        throw new McpError(
          ErrorCode.InternalError,
          `Failed to read resource: ${error instanceof Error ? error.message : String(error)}`
        );
      }
    });
  }

  // Tool implementations
  private async executePrologQuery(args: any): Promise<any> {
    const { query, sessionId, timeout = this.config.timeout } = args;

    const response = await this.makeApiRequest('/api/query', {
      method: 'POST',
      data: {
        query,
        sessionId,
        timeout
      }
    });

    const result: QueryResult = response.data;

    return {
      content: [
        {
          type: 'text',
          text: this.formatQueryResult(result)
        }
      ]
    };
  }

  private async consultPrologFile(args: any): Promise<any> {
    const { filePath, sessionId } = args;

    const response = await this.makeApiRequest('/api/consult', {
      method: 'POST',
      data: {
        filePath,
        sessionId
      }
    });

    return {
      content: [
        {
          type: 'text',
          text: `Successfully consulted file: ${filePath}\n${JSON.stringify(response.data, null, 2)}`
        }
      ]
    };
  }

  private async createPrologSession(args: any): Promise<any> {
    const { name, description } = args;

    const response = await this.makeApiRequest('/api/sessions', {
      method: 'POST',
      data: {
        name,
        description
      }
    });

    const session: SessionInfo = response.data;

    return {
      content: [
        {
          type: 'text',
          text: `Created new Prolog session:\nID: ${session.id}\nName: ${session.name}\nCreated: ${session.createdAt}`
        }
      ]
    };
  }

  private async listPrologSessions(args: any): Promise<any> {
    const { includeInactive = false } = args;

    const response = await this.makeApiRequest('/api/sessions', {
      method: 'GET',
      params: {
        includeInactive
      }
    });

    const sessions: SessionInfo[] = response.data;

    return {
      content: [
        {
          type: 'text',
          text: this.formatSessionsList(sessions)
        }
      ]
    };
  }

  private async getSessionState(args: any): Promise<any> {
    const { sessionId } = args;

    const response = await this.makeApiRequest(`/api/sessions/${sessionId}/state`, {
      method: 'GET'
    });

    return {
      content: [
        {
          type: 'text',
          text: `Session State for ${sessionId}:\n${JSON.stringify(response.data, null, 2)}`
        }
      ]
    };
  }

  private async validatePrologSyntax(args: any): Promise<any> {
    const { code } = args;

    const response = await this.makeApiRequest('/api/validate', {
      method: 'POST',
      data: {
        code
      }
    });

    const validation = response.data;

    return {
      content: [
        {
          type: 'text',
          text: validation.valid
            ? 'Prolog syntax is valid ✓'
            : `Syntax errors found:\n${validation.errors.map((e: any) => `- Line ${e.line}: ${e.message}`).join('\n')}`
        }
      ]
    };
  }

  private async getPrologHelp(args: any): Promise<any> {
    const { topic } = args;

    const response = await this.makeApiRequest('/api/help', {
      method: 'GET',
      params: {
        topic
      }
    });

    const help = response.data;

    return {
      content: [
        {
          type: 'text',
          text: this.formatHelpContent(help)
        }
      ]
    };
  }

  // Resource implementations
  private async getSessionsResource(): Promise<any> {
    const response = await this.makeApiRequest('/api/sessions', {
      method: 'GET'
    });

    return {
      contents: [
        {
          uri: 'prolog://sessions',
          mimeType: 'application/json',
          text: JSON.stringify(response.data, null, 2)
        }
      ]
    };
  }

  private async getPredicatesResource(): Promise<any> {
    const response = await this.makeApiRequest('/api/predicates', {
      method: 'GET'
    });

    return {
      contents: [
        {
          uri: 'prolog://predicates',
          mimeType: 'application/json',
          text: JSON.stringify(response.data, null, 2)
        }
      ]
    };
  }

  private async getExamplesResource(): Promise<any> {
    const examples = `
# Prolog Examples

## Basic Facts and Rules

\`\`\`prolog
% Facts
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

% Rules
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
ancestor(X, Z) :- parent(X, Z).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
\`\`\`

## List Processing

\`\`\`prolog
% List membership
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% List length
length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.

% List append
append([], L, L).
append([H|T1], L2, [H|T3]) :- append(T1, L2, T3).
\`\`\`

## Arithmetic

\`\`\`prolog
% Factorial
factorial(0, 1).
factorial(N, F) :- 
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% Fibonacci
fib(0, 0).
fib(1, 1).
fib(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.
\`\`\`
`;

    return {
      contents: [
        {
          uri: 'prolog://examples',
          mimeType: 'text/plain',
          text: examples
        }
      ]
    };
  }

  // Helper methods
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
    console.error('VSCode Prolog Toolkit MCP Server started');
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
