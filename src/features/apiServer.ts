import express, { Express, Request, Response, NextFunction } from 'express';
import cors from 'cors';
import helmet from 'helmet';
import rateLimit from 'express-rate-limit';
import { Server } from 'http';
import { PrologBackend } from '../prologBackend';
import { apiRoutes } from './apiRoutes';
import { authMiddleware, AuthConfig } from './apiMiddleware';

export interface ApiServerConfig {
  enabled: boolean;
  port: number;
  host: string;
  corsOrigins: string[];
  maxConnections: number;
  requestTimeout: number;
  rateLimiting: {
    enabled: boolean;
    requestsPerMinute: number;
    burstLimit: number;
  };
  auth: AuthConfig;
}

export interface ApiServerOptions {
  config: ApiServerConfig;
  prologBackend: PrologBackend;
}

/**
 * Express.js-based HTTP API server for external AI agent access
 * Provides RESTful endpoints for all Prolog operations with security middleware
 */
export class ApiServer {
  private app: Express;
  private server: Server | null = null;
  private config: ApiServerConfig;
  private prologBackend: PrologBackend;
  private isRunning: boolean = false;

  constructor(options: ApiServerOptions) {
    this.config = options.config;
    this.prologBackend = options.prologBackend;
    this.app = express();
    this.setupMiddleware();
    this.setupRoutes();
    this.setupErrorHandling();
  }

  /**
   * Set up Express middleware stack
   */
  private setupMiddleware(): void {
    // Security middleware
    this.app.use(helmet({
      contentSecurityPolicy: {
        directives: {
          defaultSrc: ["'self'"],
          scriptSrc: ["'self'"],
          styleSrc: ["'self'", "'unsafe-inline'"],
          imgSrc: ["'self'", "data:", "https:"],
        },
      },
      crossOriginEmbedderPolicy: false, // Allow WebSocket connections
    }));

    // CORS configuration
    this.app.use(cors({
      origin: (origin, callback) => {
        // Allow requests with no origin (like mobile apps or curl requests)
        if (!origin) return callback(null, true);
        
        if (this.config.corsOrigins.includes('*') || 
            this.config.corsOrigins.includes(origin)) {
          return callback(null, true);
        }
        
        callback(new Error('Not allowed by CORS'));
      },
      credentials: true,
      methods: ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS'],
      allowedHeaders: ['Content-Type', 'Authorization', 'X-API-Key'],
    }));

    // Rate limiting
    if (this.config.rateLimiting.enabled) {
      const limiter = rateLimit({
        windowMs: 60 * 1000, // 1 minute
        max: this.config.rateLimiting.requestsPerMinute,
        message: {
          error: 'Too many requests',
          message: `Rate limit exceeded. Maximum ${this.config.rateLimiting.requestsPerMinute} requests per minute.`,
          retryAfter: 60
        },
        standardHeaders: true,
        legacyHeaders: false,
        // Allow burst requests up to the burst limit
        skip: (req) => {
          const burstKey = `burst_${req.ip}`;
          // Simple burst tracking (in production, use Redis or similar)
          return false; // For now, apply rate limiting to all requests
        }
      });
      this.app.use('/api/', limiter);
    }

    // Request parsing
    this.app.use(express.json({ limit: '10mb' }));
    this.app.use(express.urlencoded({ extended: true, limit: '10mb' }));

    // Request timeout
    this.app.use((req: Request, res: Response, next: NextFunction) => {
      req.setTimeout(this.config.requestTimeout, () => {
        res.status(408).json({
          error: 'Request timeout',
          message: `Request exceeded ${this.config.requestTimeout}ms timeout`
        });
      });
      next();
    });

    // Authentication middleware
    this.app.use('/api/', authMiddleware(this.config.auth));

    // Request logging
    this.app.use((req: Request, res: Response, next: NextFunction) => {
      const start = Date.now();
      console.log(`[ApiServer] ${req.method} ${req.path} - ${req.ip}`);
      
      res.on('finish', () => {
        const duration = Date.now() - start;
        console.log(`[ApiServer] ${req.method} ${req.path} - ${res.statusCode} (${duration}ms)`);
      });
      
      next();
    });
  }

  /**
   * Set up API routes
   */
  private setupRoutes(): void {
    // Health check endpoint (no auth required)
    this.app.get('/health', (req: Request, res: Response) => {
      res.json({
        status: 'healthy',
        timestamp: new Date().toISOString(),
        version: '1.0.0',
        backend: {
          running: this.prologBackend.isRunning(),
          port: this.prologBackend['port'] || 3060
        }
      });
    });

    // API documentation endpoint
    this.app.get('/api', (req: Request, res: Response) => {
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
            n3: 'POST /api/v1/reasoning/n3'
          },
          history: 'GET /api/v1/history',
          status: 'GET /api/v1/status'
        }
      });
    });

    // Mount API routes
    this.app.use('/api/v1', apiRoutes(this.prologBackend));
  }

  /**
   * Set up error handling middleware
   */
  private setupErrorHandling(): void {
    // 404 handler
    this.app.use('*', (req: Request, res: Response) => {
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
          'GET /api/v1/status'
        ]
      });
    });

    // Global error handler
    this.app.use((error: Error, req: Request, res: Response, next: NextFunction) => {
      console.error(`[ApiServer] Error in ${req.method} ${req.path}:`, error);
      
      // Don't send error details in production
      const isDevelopment = process.env.NODE_ENV !== 'production';
      
      res.status(500).json({
        error: 'Internal Server Error',
        message: isDevelopment ? error.message : 'An unexpected error occurred',
        ...(isDevelopment && { stack: error.stack })
      });
    });
  }

  /**
   * Start the API server
   */
  async start(): Promise<void> {
    if (this.isRunning) {
      throw new Error('API server is already running');
    }

    if (!this.config.enabled) {
      console.log('[ApiServer] API server is disabled in configuration');
      return;
    }

    return new Promise((resolve, reject) => {
      try {
        this.server = this.app.listen(this.config.port, this.config.host, () => {
          this.isRunning = true;
          console.log(`[ApiServer] HTTP API server started on ${this.config.host}:${this.config.port}`);
          console.log(`[ApiServer] Health check: http://${this.config.host}:${this.config.port}/health`);
          console.log(`[ApiServer] API documentation: http://${this.config.host}:${this.config.port}/api`);
          resolve();
        });

        this.server.on('error', (error: Error) => {
          console.error('[ApiServer] Server error:', error);
          reject(error);
        });

        // Handle connection limits
        this.server.maxConnections = this.config.maxConnections;

      } catch (error) {
        reject(error);
      }
    });
  }

  /**
   * Stop the API server
   */
  async stop(): Promise<void> {
    if (!this.isRunning || !this.server) {
      return;
    }

    return new Promise((resolve, reject) => {
      this.server!.close((error) => {
        if (error) {
          console.error('[ApiServer] Error stopping server:', error);
          reject(error);
        } else {
          this.isRunning = false;
          this.server = null;
          console.log('[ApiServer] HTTP API server stopped');
          resolve();
        }
      });
    });
  }

  /**
   * Get server status
   */
  getStatus(): {
    running: boolean;
    port: number;
    host: string;
    connections: number;
  } {
    return {
      running: this.isRunning,
      port: this.config.port,
      host: this.config.host,
      connections: this.server?.connections || 0
    };
  }

  /**
   * Get Express app instance (for testing)
   */
  getApp(): Express {
    return this.app;
  }
}