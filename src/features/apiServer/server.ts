// Main ApiServer class orchestrating modular components
import express, { Express } from 'express';
import { Server } from 'http';
import { ApiServerConfig, ApiServerOptions } from './config';
import { setupApiErrorHandlers } from './errorHandlers';
import { setupApiMiddleware } from './middleware';
import { setupApiRoutes } from './routes';

export class ApiServer {
  private app: Express;
  private server: Server | null = null;
  private config: ApiServerConfig;
  private prologBackend: any;
  private isRunning: boolean = false;

  constructor(options: ApiServerOptions) {
    this.config = options.config;
    this.prologBackend = options.prologBackend;
    this.app = express();
    setupApiMiddleware(this.app, this.config);
    setupApiRoutes(this.app, this.prologBackend);
    setupApiErrorHandlers(this.app);
  }

  async start(): Promise<void> {
    if (this.isRunning) throw new Error('API server is already running');
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
        this.server.on('error', (error: unknown) => {
          console.error('[ApiServer] Server error:', error);
          reject(error);
        });
        this.server.maxConnections = this.config.maxConnections;
      } catch (error: unknown) {
        reject(error);
      }
    });
  }

  async stop(): Promise<void> {
    if (!this.isRunning || !this.server) return;
    return new Promise((resolve, reject) => {
      this.server!.close(error => {
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

  getStatus() {
    return {
      running: this.isRunning,
      port: this.config.port,
      host: this.config.host,
      connections: (this.server as any)?.connections || 0,
    };
  }

  getApp(): Express {
    return this.app;
  }
}
