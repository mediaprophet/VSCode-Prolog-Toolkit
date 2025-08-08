// Express middleware for API server
import cors from 'cors';
import express, { Express, NextFunction, Request, Response } from 'express';
import rateLimit from 'express-rate-limit';
import helmet from 'helmet';
import { authMiddleware } from '../apiMiddleware';

export function setupApiMiddleware(app: Express, config: any) {
  app.use(
    helmet({
      contentSecurityPolicy: {
        directives: {
          defaultSrc: ["'self'"],
          scriptSrc: ["'self'"],
          styleSrc: ["'self'", "'unsafe-inline'"],
          imgSrc: ["'self'", 'data:', 'https:'],
        },
      },
      crossOriginEmbedderPolicy: false,
    })
  );
  app.use(
    cors({
      origin: (origin, callback) => {
        if (!origin) return callback(null, true);
        if (config.corsOrigins.includes('*') || config.corsOrigins.includes(origin)) {
          return callback(null, true);
        }
        callback(new Error('Not allowed by CORS'));
      },
      credentials: true,
      methods: ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS'],
      allowedHeaders: ['Content-Type', 'Authorization', 'X-API-Key'],
    })
  );
  if (config.rateLimiting.enabled) {
    const limiter = rateLimit({
      windowMs: 60 * 1000,
      max: config.rateLimiting.requestsPerMinute,
      message: {
        error: 'Too many requests',
        message: `Rate limit exceeded. Maximum ${config.rateLimiting.requestsPerMinute} requests per minute.`,
        retryAfter: 60,
      },
      standardHeaders: true,
      legacyHeaders: false,
      skip: req => false,
    });
    app.use('/api/', limiter);
  }
  app.use(express.json({ limit: '10mb' }));
  app.use(express.urlencoded({ extended: true, limit: '10mb' }));
  app.use((req: Request, res: Response, next: NextFunction) => {
    req.setTimeout(config.requestTimeout, () => {
      res.status(408).json({
        error: 'Request timeout',
        message: `Request exceeded ${config.requestTimeout}ms timeout`,
      });
    });
    next();
  });
  app.use('/api/', authMiddleware(config.auth));
  app.use((req: Request, res: Response, next: NextFunction) => {
    const start = Date.now();
    console.log(`[ApiServer] ${req.method} ${req.path} - ${req.ip}`);
    res.on('finish', () => {
      const duration = Date.now() - start;
      console.log(`[ApiServer] ${req.method} ${req.path} - ${res.statusCode} (${duration}ms)`);
    });
    next();
  });
}
