// API server route setup
import { Express, Request, Response } from 'express';
import { apiRoutes } from '../apiRoutes';

export function setupApiRoutes(app: Express, prologBackend: any) {
  app.get('/health', (req: Request, res: Response) => {
    res.json({
      status: 'healthy',
      timestamp: new Date().toISOString(),
      version: '1.0.0',
      backend: {
        running: prologBackend.isRunning(),
        port: prologBackend['port'] || 3060,
      },
    });
  });
  app.get('/api', (req: Request, res: Response) => {
    res.json({
      name: 'VSCode Prolog Toolkit API',
      version: '1.0.0',
      description: 'RESTful API for Prolog operations and AI agent integration',
      documentation: '/api/docs',
      endpoints: {
        query: 'POST /api/v1/query',
        batch: 'POST /api/v1/batch',
        sessions: 'GET|POST /api/v1/sessions',
        reasoning: {
          clp: 'POST /api/v1/reasoning/clp',
          probabilistic: 'POST /api/v1/reasoning/probabilistic',
          n3: 'POST /api/v1/reasoning/n3',
        },
        history: 'GET /api/v1/history',
        status: 'GET /api/v1/status',
      },
    });
  });
  app.use('/api/v1', apiRoutes(prologBackend));
}
