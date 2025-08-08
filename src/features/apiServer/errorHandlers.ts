// API server error handling
import { Express, NextFunction, Request, Response } from 'express';

export function setupApiErrorHandlers(app: Express) {
  app.use('*', (req: Request, res: Response) => {
    res.status(404).json({
      error: 'Not Found',
      message: `Endpoint ${req.method} ${req.originalUrl} not found`,
      availableEndpoints: [
        'GET /health',
        'GET /api',
        'POST /api/v1/query',
        'POST /api/v1/batch',
        'GET /api/v1/sessions',
        'POST /api/v1/sessions',
        'GET /api/v1/status',
      ],
    });
  });
  app.use((error: Error, req: Request, res: Response, next: NextFunction) => {
    console.error(`[ApiServer] Error in ${req.method} ${req.path}:`, error);
    const isDevelopment = process.env.NODE_ENV !== 'production';
    res.status(500).json({
      error: 'Internal Server Error',
      message: isDevelopment ? error.message : 'An unexpected error occurred',
      ...(isDevelopment && { stack: error.stack }),
    });
  });
}
