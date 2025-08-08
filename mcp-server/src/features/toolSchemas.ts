export const ListResourcesRequestSchema = z.object({ method: z.literal('listResources') });
export const ReadResourceRequestSchema = z.object({ method: z.literal('readResource'), params: z.object({ uri: z.string() }) });
// Zod schemas for each tool request
export const BatchQueryExecutionRequestSchema = z.object({ method: z.literal('batch_query_execution'), params: z.object({ queries: z.array(z.string()), sessionId: z.string().optional(), timeout: z.number().optional() }) });
export const ClpConstraintSolvingRequestSchema = z.object({ method: z.literal('clp_constraint_solving'), params: z.object({ constraints: z.string(), sessionId: z.string().optional(), timeout: z.number().optional() }) });
export const ProbabilisticInferenceRequestSchema = z.object({ method: z.literal('probabilistic_inference'), params: z.object({ query: z.string(), sessionId: z.string().optional(), timeout: z.number().optional() }) });
export const N3SemanticReasoningRequestSchema = z.object({ method: z.literal('n3_semantic_reasoning'), params: z.object({ n3Input: z.string(), query: z.string(), timeout: z.number().optional() }) });
export const QueryHistoryRequestSchema = z.object({ method: z.literal('query_history'), params: z.object({ sessionId: z.string().optional(), limit: z.number().optional() }) });
export const SystemStatusRequestSchema = z.object({ method: z.literal('system_status'), params: z.object({}) });
// Zod schema for ListToolsRequest
import { z } from 'zod';
export const ListToolsRequestSchema = z.object({
  method: z.literal('listTools')
});
// Modularized tool schemas for PrologToolkitMCPServer

export const toolSchemas = [
  {
    name: 'execute_prolog_query',
    description: 'Execute a Prolog query and return results',
    inputSchema: {
      type: 'object',
      properties: {
        query: { type: 'string', description: 'The Prolog query to execute' },
        sessionId: { type: 'string', description: 'Optional session ID to execute query in specific session' },
        timeout: { type: 'number', description: 'Query timeout in milliseconds (default: 30000)' }
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
        filePath: { type: 'string', description: 'Path to the Prolog file to consult' },
        sessionId: { type: 'string', description: 'Optional session ID to load file into specific session' }
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
        name: { type: 'string', description: 'Name for the new session' },
        description: { type: 'string', description: 'Optional description for the session' }
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
        includeInactive: { type: 'boolean', description: 'Include inactive sessions in the list (default: false)' }
      }
    }
  },
  {
    name: 'get_session_state',
    description: 'Get the current state of a Prolog session',
    inputSchema: {
      type: 'object',
      properties: {
        sessionId: { type: 'string', description: 'ID of the session to inspect' }
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
        code: { type: 'string', description: 'Prolog code to validate' }
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
        topic: { type: 'string', description: 'Predicate name or concept to get help for' }
      },
      required: ['topic']
    }
  },
  {
    name: 'batch_query_execution',
    description: 'Execute multiple Prolog queries in a single batch and return all results',
    inputSchema: {
      type: 'object',
      properties: {
        queries: {
          type: 'array',
          items: { type: 'string' },
          description: 'Array of Prolog queries to execute as a batch'
        },
        sessionId: {
          type: 'string',
          description: 'Optional session ID to execute batch in specific session'
        },
        timeout: {
          type: 'number',
          description: 'Batch query timeout in milliseconds (default: 60000)'
        }
      },
      required: ['queries']
    }
  },
  {
    name: 'clp_constraint_solving',
    description: 'Solve constraint logic programming (CLP) problems using the Prolog backend',
    inputSchema: {
      type: 'object',
      properties: {
        constraints: {
          type: 'string',
          description: 'CLP constraints to solve (Prolog syntax)'
        },
        sessionId: {
          type: 'string',
          description: 'Optional session ID to execute in specific session'
        },
        timeout: {
          type: 'number',
          description: 'CLP query timeout in milliseconds (default: 30000)'
        }
      },
      required: ['constraints']
    }
  },
  {
    name: 'probabilistic_inference',
    description: 'Perform probabilistic logic inference using the Prolog backend',
    inputSchema: {
      type: 'object',
      properties: {
        query: {
          type: 'string',
          description: 'Probabilistic logic query (Prolog syntax)'
        },
        sessionId: {
          type: 'string',
          description: 'Optional session ID to execute in specific session'
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
    name: 'n3_semantic_reasoning',
    description: 'Run N3/semantic web reasoning tasks using the Prolog backend',
    inputSchema: {
      type: 'object',
      properties: {
        n3Input: {
          type: 'string',
          description: 'N3 data or rules to reason over'
        },
        query: {
          type: 'string',
          description: 'N3 query to execute'
        },
        timeout: {
          type: 'number',
          description: 'Reasoning timeout in milliseconds (default: 30000)'
        }
      },
      required: ['n3Input', 'query']
    }
  },
  {
    name: 'query_history',
    description: 'Retrieve the history of executed Prolog queries',
    inputSchema: {
      type: 'object',
      properties: {
        sessionId: {
          type: 'string',
          description: 'Optional session ID to filter query history'
        },
        limit: {
          type: 'number',
          description: 'Maximum number of history entries to return (default: 50)'
        }
      }
    }
  },
  {
    name: 'system_status',
    description: 'Get system/installation/runtime status of the Prolog backend',
    inputSchema: {
      type: 'object',
      properties: {}
    }
  }
];
