import crypto from 'crypto';
import type { NextFunction, Request, Response } from 'express';
import jwt from 'jsonwebtoken';

export interface AuthConfig {
  method: 'api_key' | 'jwt_token' | 'local_only';
  apiKeys: string[];
  jwtSecret: string;
  localOnly: boolean;
  roles: {
    admin: string[];
    agent: string[];
    readonly: string[];
    limited: string[];
  };
  quotas: {
    admin: {
      requestsPerMinute: number;
      maxConcurrentSessions: number;
    };
    agent: {
      requestsPerMinute: number;
      maxConcurrentSessions: number;
    };
    readonly: {
      requestsPerMinute: number;
      maxConcurrentSessions: number;
    };
    limited: {
      requestsPerMinute: number;
      maxConcurrentSessions: number;
    };
  };
}

export interface AuthenticatedUser {
  id: string;
  role: string;
  permissions: string[];
  method: string;
}

export interface AuthenticatedRequest extends Request {
  user?: AuthenticatedUser;
}

/**
 * Authentication middleware factory
 */
export function authMiddleware(config: AuthConfig) {
  return async (req: AuthenticatedRequest, res: Response, next: NextFunction) => {
    try {
      // Skip authentication for health check and documentation endpoints
      if (req.path === '/health' || req.path === '/api' || req.path === '/api/docs') {
        return next();
      }

      // Local-only mode: restrict to localhost
      if (config.localOnly || config.method === 'local_only') {
        const clientIP = req.ip || req.connection?.remoteAddress || '';
        const isLocalhost =
          ['127.0.0.1', '::1', '::ffff:127.0.0.1'].includes(clientIP) ||
          clientIP.startsWith('127.') ||
          clientIP === 'localhost';

        if (isLocalhost) {
          // Grant admin permissions for localhost in local-only mode
          req.user = {
            id: 'localhost',
            role: 'admin',
            permissions: ['*'],
            method: 'local_only',
          };
          return next();
        } else if (config.localOnly) {
          return res.status(403).json({
            error: 'Forbidden',
            message: 'API access restricted to localhost only',
          });
        }
      }

      // Try different authentication methods
      let user: AuthenticatedUser | null = null;

      // 1. Try API Key authentication
      if (config.method === 'api_key') {
        user = await tryApiKeyAuth(req, config);
        if (user) {
          req.user = user;
          return next();
        }
      }

      // 2. Try JWT authentication
      if (config.method === 'jwt_token') {
        user = await tryJwtAuth(req, config);
        if (user) {
          req.user = user;
          return next();
        }
      }

      // OAuth2 authentication removed

      // No valid authentication found
      return res.status(401).json({
        error: 'Unauthorized',
        message: 'Valid authentication required',
        supportedMethods: [config.method],
        hints: {
          api_key: 'Include X-API-Key header',
          jwt_token: 'Include Authorization: Bearer <token> header',
          local_only: 'Access from localhost only',
        },
      });
    } catch (error: unknown) {
      console.error('[AuthMiddleware] Authentication error:', error);
      return res.status(500).json({
        error: 'Internal Server Error',
        message: 'Authentication system error',
      });
    }
  };
}

/**
 * Try API Key authentication
 */
async function tryApiKeyAuth(req: Request, config: AuthConfig): Promise<AuthenticatedUser | null> {
  const apiKey = req.headers['x-api-key'] as string;
  if (!apiKey) return null;

  // Check against configured API keys
  if (config.apiKeys.includes(apiKey)) {
    return {
      id: `api_key_${apiKey.substring(0, 8)}`,
      role: 'agent', // Default role for API key users
      permissions: getDefaultPermissions('agent'),
      method: 'api_key',
    };
  }

  return null;
}

/**
 * Try JWT authentication
 */
async function tryJwtAuth(req: Request, config: AuthConfig): Promise<AuthenticatedUser | null> {
  const authHeader = req.headers.authorization;
  if (!authHeader || !authHeader.startsWith('Bearer ')) return null;

  const token = authHeader.substring(7);

  try {
    const decoded = jwt.verify(token, config.jwtSecret) as Record<string, unknown>;

    return {
      id: (decoded.sub as string) || (decoded.id as string) || 'jwt_user',
      role: (decoded.role as string) || 'agent',
      permissions:
        (decoded.permissions as string[]) ||
        getDefaultPermissions((decoded.role as string) || 'agent'),
      method: 'jwt_token',
    };
  } catch (error: unknown) {
    console.error('[AuthMiddleware] JWT verification failed:', error);
    return null;
  }
}

/**
 * Get default permissions for a role
 */
function getDefaultPermissions(role: string): string[] {
  const rolePermissions: { [key: string]: string[] } = {
    admin: ['*'],
    agent: [
      'query:execute',
      'batch:execute',
      'session:create',
      'session:read',
      'session:delete_own',
      'reasoning:*',
      'history:read_own',
    ],
    readonly: ['query:execute_readonly', 'session:read', 'history:read_own', 'status:read'],
    limited: ['query:execute_simple', 'session:read_own'],
  };

  return rolePermissions[role] || rolePermissions['limited'] || [];
}

/**
 * Generate a new API key
 */
export function generateApiKey(): string {
  return crypto.randomBytes(32).toString('hex');
}

/**
 * Hash an API key for secure storage
 */
export function hashApiKey(apiKey: string): string {
  return crypto.createHash('sha256').update(apiKey).digest('hex');
}

/**
 * Generate a JWT token
 */
export function generateJwtToken(payload: any, config: AuthConfig): string {
  return jwt.sign(payload, config.jwtSecret, {
    expiresIn: '1h',
    issuer: 'vscode-prolog-toolkit',
    audience: 'ai-agents',
  });
}

/**
 * Verify JWT token
 */
export function verifyJwtToken(token: string, config: AuthConfig): Record<string, unknown> {
  const decoded = jwt.verify(token, config.jwtSecret);
  if (typeof decoded === 'string') {
    throw new Error('Invalid JWT token format');
  }
  return decoded as Record<string, unknown>;
}

/**
 * Permission checking utility
 */
export function hasPermission(user: AuthenticatedUser | undefined, permission: string): boolean {
  if (!user) return false;

  // Admin role has all permissions
  if (user.role === 'admin' || user.permissions.includes('*')) return true;

  // Check exact permission
  if (user.permissions.includes(permission)) return true;

  // Check wildcard permissions (e.g., 'reasoning:*' matches 'reasoning:clp')
  const wildcardPermissions = user.permissions.filter(p => p.endsWith(':*'));
  for (const wildcardPerm of wildcardPermissions) {
    const prefix = wildcardPerm.slice(0, -1); // Remove '*'
    if (permission.startsWith(prefix)) return true;
  }

  return false;
}

/**
 * Resource quota middleware
 */
export function resourceQuotaMiddleware(quotas: {
  [role: string]: { requests_per_minute?: number; maxConcurrentSessions?: number };
}) {
  return (req: AuthenticatedRequest, res: Response, next: NextFunction) => {
    const user = req.user;
    if (!user) return next();

    const userQuota = quotas[user.role] || quotas['default'];
    if (!userQuota) return next();

    // Add quota information to request for later use
    (req as AuthenticatedRequest & { quota?: typeof userQuota }).quota = userQuota;

    // Add quota headers to response
    res.set({
      'X-Rate-Limit-Limit': userQuota.requests_per_minute?.toString() || '60',
      'X-Rate-Limit-Remaining': '60', // This would be calculated based on actual usage
      'X-Rate-Limit-Reset': (Date.now() + 60000).toString(),
    });

    next();
  };
}

/**
 * Audit logging middleware
 */
export function auditMiddleware() {
  return (req: AuthenticatedRequest, res: Response, next: NextFunction) => {
    const start = Date.now();

    // Log request
    console.log(
      `[Audit] ${req.method} ${req.path} - User: ${req.user?.id || 'anonymous'} (${req.user?.role || 'none'})`
    );

    // Log response when finished
    res.on('finish', () => {
      const duration = Date.now() - start;
      const logLevel = res.statusCode >= 400 ? 'WARN' : 'INFO';

      console.log(
        `[Audit] ${logLevel} ${req.method} ${req.path} - ${res.statusCode} (${duration}ms) - User: ${req.user?.id || 'anonymous'}`
      );

      // Log security events
      if (res.statusCode === 401) {
        console.log(`[Security] Authentication failed - ${req.ip} - ${req.method} ${req.path}`);
      } else if (res.statusCode === 403) {
        console.log(
          `[Security] Authorization denied - User: ${req.user?.id} - ${req.method} ${req.path}`
        );
      } else if (res.statusCode === 429) {
        console.log(`[Security] Rate limit exceeded - User: ${req.user?.id} - ${req.ip}`);
      }
    });

    next();
  };
}
