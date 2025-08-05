import { Router, Request, Response } from 'express';
import { PrologBackend } from '../prologBackend';
import { v4 as uuidv4 } from 'uuid';

export interface AuthenticatedRequest extends Request {
  user?: {
    id: string;
    role: string;
    permissions: string[];
  };
}

/**
 * Create API routes for Prolog operations
 */
export function apiRoutes(prologBackend: PrologBackend): Router {
  const router = Router();

  // Query execution endpoint
  router.post('/query', async (req: AuthenticatedRequest, res: Response) => {
    try {
      const { query, session_id, options = {} } = req.body;

      if (!query) {
        return res.status(400).json({
          error: 'Bad Request',
          message: 'Query parameter is required'
        });
      }

      // Check permissions
      if (!hasPermission(req.user, 'query:execute')) {
        return res.status(403).json({
          error: 'Forbidden',
          message: 'Insufficient permissions for query execution'
        });
      }

      const queryId = uuidv4();
      const params = {
        goal: query,
        timeoutMs: options.timeout || 30000,
        max_results: options.max_results || 100,
        streaming: options.stream || false,
        session_id: session_id || 'default',
        ...options
      };

      // Execute query with notifications if callback provided
      const result = await prologBackend.sendRequestWithNotifications(
        'query',
        params,
        {
          onProgress: (status) => {
            // In a full implementation, this would use WebSocket to notify client
            console.log(`[ApiRoutes] Query ${queryId} progress: ${status.progress}%`);
          },
          onComplete: (status) => {
            console.log(`[ApiRoutes] Query ${queryId} completed`);
          },
          onError: (status) => {
            console.error(`[ApiRoutes] Query ${queryId} error:`, status.error);
          }
        }
      );

      res.json({
        query_id: queryId,
        success: result.status === 'ok',
        results: result.results || [],
        execution_time: result.execution_time || 0,
        more_available: result.streaming_info?.has_more || false,
        cursor: result.streaming_info?.next_offset || null,
        streaming_info: result.streaming_info
      });

    } catch (error) {
      console.error('[ApiRoutes] Query execution error:', error);
      res.status(500).json({
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : 'Query execution failed'
      });
    }
  });

  // Batch query execution endpoint
  router.post('/batch', async (req: AuthenticatedRequest, res: Response) => {
    try {
      const { queries, session_id, batch_options = {} } = req.body;

      if (!Array.isArray(queries) || queries.length === 0) {
        return res.status(400).json({
          error: 'Bad Request',
          message: 'Queries array is required and must not be empty'
        });
      }

      // Check permissions
      if (!hasPermission(req.user, 'batch:execute')) {
        return res.status(403).json({
          error: 'Forbidden',
          message: 'Insufficient permissions for batch execution'
        });
      }

      const batchId = uuidv4();
      const batchRequests = queries.map((query: any) => ({
        cmd: 'query',
        params: {
          goal: query.query || query,
          timeoutMs: query.timeout || batch_options.timeout || 30000,
          session_id: session_id || 'default'
        }
      }));

      const results = await prologBackend.sendRequest(batchRequests);

      res.json({
        batch_id: batchId,
        results: results.map((result: any, index: number) => ({
          query_index: index,
          success: result.status === 'ok',
          results: result.results || [],
          error: result.error || null
        })),
        total_queries: queries.length,
        successful_queries: results.filter((r: any) => r.status === 'ok').length
      });

    } catch (error) {
      console.error('[ApiRoutes] Batch execution error:', error);
      res.status(500).json({
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : 'Batch execution failed'
      });
    }
  });

  // Session management endpoints
  router.get('/sessions', async (req: AuthenticatedRequest, res: Response) => {
    try {
      if (!hasPermission(req.user, 'session:read')) {
        return res.status(403).json({
          error: 'Forbidden',
          message: 'Insufficient permissions to list sessions'
        });
      }

      const sessions = prologBackend.listSessions({
        userId: req.user?.id,
        includeInactive: req.query.include_inactive === 'true'
      });

      res.json({
        sessions: sessions.map(session => ({
          session_id: session.sessionId,
          name: session.config.name,
          description: session.config.description,
          created_at: session.config.createdAt,
          is_active: session.config.isActive,
          user_id: session.config.userId
        })),
        total: sessions.length
      });

    } catch (error) {
      console.error('[ApiRoutes] Session list error:', error);
      res.status(500).json({
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : 'Failed to list sessions'
      });
    }
  });

  router.post('/sessions', async (req: AuthenticatedRequest, res: Response) => {
    try {
      const { name, description, config = {} } = req.body;

      if (!name) {
        return res.status(400).json({
          error: 'Bad Request',
          message: 'Session name is required'
        });
      }

      if (!hasPermission(req.user, 'session:create')) {
        return res.status(403).json({
          error: 'Forbidden',
          message: 'Insufficient permissions to create sessions'
        });
      }

      const sessionId = await prologBackend.createSession(name, {
        description,
        userId: req.user?.id,
        agentId: req.headers['user-agent'],
        ...config
      });

      res.status(201).json({
        session_id: sessionId,
        name,
        description,
        created_at: new Date().toISOString(),
        message: 'Session created successfully'
      });

    } catch (error) {
      console.error('[ApiRoutes] Session creation error:', error);
      res.status(500).json({
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : 'Failed to create session'
      });
    }
  });

  router.get('/sessions/:session_id', async (req: AuthenticatedRequest, res: Response) => {
    try {
      const { session_id } = req.params;

      if (!hasPermission(req.user, 'session:read')) {
        return res.status(403).json({
          error: 'Forbidden',
          message: 'Insufficient permissions to read session details'
        });
      }

      const session = prologBackend.getSession(session_id);
      if (!session) {
        return res.status(404).json({
          error: 'Not Found',
          message: 'Session not found'
        });
      }

      res.json({
        session_id,
        config: session.config,
        state: session.state,
        statistics: await prologBackend.getSessionStatistics(session_id)
      });

    } catch (error) {
      console.error('[ApiRoutes] Session details error:', error);
      res.status(500).json({
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : 'Failed to get session details'
      });
    }
  });

  router.delete('/sessions/:session_id', async (req: AuthenticatedRequest, res: Response) => {
    try {
      const { session_id } = req.params;

      if (!hasPermission(req.user, 'session:delete')) {
        return res.status(403).json({
          error: 'Forbidden',
          message: 'Insufficient permissions to delete sessions'
        });
      }

      const deleted = await prologBackend.deleteSession(session_id);
      if (!deleted) {
        return res.status(404).json({
          error: 'Not Found',
          message: 'Session not found'
        });
      }

      res.json({
        session_id,
        message: 'Session deleted successfully'
      });

    } catch (error) {
      console.error('[ApiRoutes] Session deletion error:', error);
      res.status(500).json({
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : 'Failed to delete session'
      });
    }
  });

  // Session state management
  router.get('/sessions/:session_id/state', async (req: AuthenticatedRequest, res: Response) => {
    try {
      const { session_id } = req.params;

      if (!hasPermission(req.user, 'session:read')) {
        return res.status(403).json({
          error: 'Forbidden',
          message: 'Insufficient permissions to read session state'
        });
      }

      const session = prologBackend.getSession(session_id);
      if (!session) {
        return res.status(404).json({
          error: 'Not Found',
          message: 'Session not found'
        });
      }

      res.json({
        session_id,
        state: session.state,
        exported_at: new Date().toISOString()
      });

    } catch (error) {
      console.error('[ApiRoutes] Session state export error:', error);
      res.status(500).json({
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : 'Failed to export session state'
      });
    }
  });

  router.post('/sessions/:session_id/state', async (req: AuthenticatedRequest, res: Response) => {
    try {
      const { session_id } = req.params;
      const { state } = req.body;

      if (!hasPermission(req.user, 'session:write')) {
        return res.status(403).json({
          error: 'Forbidden',
          message: 'Insufficient permissions to modify session state'
        });
      }

      await prologBackend.saveSessionState(session_id, state);

      res.json({
        session_id,
        message: 'Session state imported successfully',
        imported_at: new Date().toISOString()
      });

    } catch (error) {
      console.error('[ApiRoutes] Session state import error:', error);
      res.status(500).json({
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : 'Failed to import session state'
      });
    }
  });

  // Advanced reasoning endpoints
  router.post('/reasoning/clp', async (req: AuthenticatedRequest, res: Response) => {
    try {
      const { constraints, domain = 'fd', variables } = req.body;

      if (!constraints || !variables) {
        return res.status(400).json({
          error: 'Bad Request',
          message: 'Constraints and variables are required'
        });
      }

      if (!hasPermission(req.user, 'reasoning:clp')) {
        return res.status(403).json({
          error: 'Forbidden',
          message: 'Insufficient permissions for CLP reasoning'
        });
      }

      const result = await prologBackend.sendRequest('clp_solve', {
        domain,
        variables,
        constraints
      });

      res.json({
        success: result.status === 'ok',
        domain,
        solution: result.solution || [],
        error: result.error || null
      });

    } catch (error) {
      console.error('[ApiRoutes] CLP reasoning error:', error);
      res.status(500).json({
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : 'CLP reasoning failed'
      });
    }
  });

  router.post('/reasoning/probabilistic', async (req: AuthenticatedRequest, res: Response) => {
    try {
      const { facts, query, samples = 1000 } = req.body;

      if (!query) {
        return res.status(400).json({
          error: 'Bad Request',
          message: 'Query is required for probabilistic inference'
        });
      }

      if (!hasPermission(req.user, 'reasoning:probabilistic')) {
        return res.status(403).json({
          error: 'Forbidden',
          message: 'Insufficient permissions for probabilistic reasoning'
        });
      }

      // Add probabilistic facts if provided
      if (facts && Array.isArray(facts)) {
        for (const fact of facts) {
          await prologBackend.sendRequest('probabilistic_fact', {
            fact: fact.fact,
            probability: fact.probability
          });
        }
      }

      const result = await prologBackend.sendRequest('probabilistic_query', {
        goal: query,
        samples,
        method: 'monte_carlo'
      });

      res.json({
        success: result.status === 'ok',
        query,
        probability: result.probability || 0,
        evidence: result.evidence || {},
        samples,
        error: result.error || null
      });

    } catch (error) {
      console.error('[ApiRoutes] Probabilistic reasoning error:', error);
      res.status(500).json({
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : 'Probabilistic reasoning failed'
      });
    }
  });

  router.post('/reasoning/n3', async (req: AuthenticatedRequest, res: Response) => {
    try {
      const { rules, data, query } = req.body;

      if (!query) {
        return res.status(400).json({
          error: 'Bad Request',
          message: 'Query is required for N3 reasoning'
        });
      }

      if (!hasPermission(req.user, 'reasoning:n3')) {
        return res.status(403).json({
          error: 'Forbidden',
          message: 'Insufficient permissions for N3 reasoning'
        });
      }

      // Load N3 data if provided
      if (data) {
        await prologBackend.sendRequest('n3_load', {
          content: data
        });
      }

      const result = await prologBackend.sendRequest('n3_reason', {
        goal: query
      });

      res.json({
        success: result.status === 'ok',
        query,
        results: result.results || [],
        inferred_triples: result.inferred_triples || [],
        count: result.count || 0,
        error: result.error || null
      });

    } catch (error) {
      console.error('[ApiRoutes] N3 reasoning error:', error);
      res.status(500).json({
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : 'N3 reasoning failed'
      });
    }
  });

  // Query history endpoint
  router.get('/history', async (req: AuthenticatedRequest, res: Response) => {
    try {
      if (!hasPermission(req.user, 'history:read')) {
        return res.status(403).json({
          error: 'Forbidden',
          message: 'Insufficient permissions to read query history'
        });
      }

      const {
        session_id,
        limit = 50,
        offset = 0,
        status
      } = req.query;

      const filter: any = {
        limit: parseInt(limit as string),
        offset: parseInt(offset as string)
      };

      if (session_id) filter.session_id = session_id;
      if (status) filter.status = status;
      if (req.user?.id) filter.user_id = req.user.id;

      const history = await prologBackend.getQueryHistory(filter);

      res.json({
        queries: history.queries || [],
        total: history.total || 0,
        limit: filter.limit,
        offset: filter.offset,
        has_more: (history.total || 0) > (filter.offset + filter.limit)
      });

    } catch (error) {
      console.error('[ApiRoutes] Query history error:', error);
      res.status(500).json({
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : 'Failed to retrieve query history'
      });
    }
  });

  // System status endpoint
  router.get('/status', async (req: AuthenticatedRequest, res: Response) => {
    try {
      if (!hasPermission(req.user, 'status:read')) {
        return res.status(403).json({
          error: 'Forbidden',
          message: 'Insufficient permissions to read system status'
        });
      }

      const backendStatus = prologBackend.getConcurrencyStatus();
      const queryStats = prologBackend.getQueryStatistics();
      const schedulerStats = prologBackend.getSchedulerStatistics();

      res.json({
        backend: {
          running: prologBackend.isRunning(),
          active_queries: queryStats.running || 0,
          active_sessions: prologBackend.listSessions().length,
          resource_usage: backendStatus.resourceUsage || {}
        },
        scheduler: schedulerStats,
        query_statistics: queryStats,
        timestamp: new Date().toISOString()
      });

    } catch (error) {
      console.error('[ApiRoutes] Status endpoint error:', error);
      res.status(500).json({
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : 'Failed to retrieve system status'
      });
    }
  });

  return router;
}

/**
 * Check if user has required permission
 */
function hasPermission(user: any, permission: string): boolean {
  if (!user) return false;
  
  // Admin role has all permissions
  if (user.role === 'admin') return true;
  
  // Check specific permissions
  return user.permissions && user.permissions.includes(permission);
}