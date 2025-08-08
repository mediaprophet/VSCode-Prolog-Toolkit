// API server config types and validation
import { AuthConfig } from '../apiMiddleware';

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
  prologBackend: any;
}
