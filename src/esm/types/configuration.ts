/**
 * Configuration type definitions for the extension
 */

// Base configuration interfaces
export interface BaseConfiguration {
  enabled: boolean;
  [key: string]: any;
}

// Main extension configuration
export interface PrologConfiguration {
  executablePath: string;
  dialect: 'swi' | 'ecl';
  linter: LinterConfiguration;
  format: FormatConfiguration;
  terminal: TerminalConfiguration;
  telemetry: TelemetryConfiguration;
  apiServer: ApiServerConfiguration;
  webSocketServer: WebSocketServerConfiguration;
}

// Linter configuration
export interface LinterConfiguration {
  run: 'onSave' | 'onType' | 'never';
  delay: number;
  enableMsgInOutput: boolean;
}

// Format configuration
export interface FormatConfiguration {
  enabled: boolean;
  addSpace: boolean;
  tabSize?: number;
  insertSpaces?: boolean;
  maxLineLength?: number;
  indentClauses?: boolean;
  alignParameters?: boolean;
}

// Terminal configuration
export interface TerminalConfiguration {
  runtimeArgs: string[];
  defaultShell?: string;
  env?: Record<string, string>;
  cwd?: string;
}

// Telemetry configuration
export interface TelemetryConfiguration {
  enabled: boolean;
  level?: 'basic' | 'detailed';
  endpoint?: string;
  anonymize?: boolean;
}

// API Server configuration
export interface ApiServerConfiguration extends BaseConfiguration {
  port: number;
  host: string;
  corsOrigins: string[];
  maxConnections: number;
  requestTimeout: number;
  rateLimiting: RateLimitingConfiguration;
  auth: AuthenticationConfiguration;
}

// Rate limiting configuration
export interface RateLimitingConfiguration extends BaseConfiguration {
  requestsPerMinute: number;
  burstLimit: number;
  windowMs?: number;
  skipSuccessfulRequests?: boolean;
  skipFailedRequests?: boolean;
}

// Authentication configuration
export interface AuthenticationConfiguration {
  method: 'local_only' | 'api_key' | 'jwt_token' | 'oauth2';
  apiKeys: Record<string, ApiKeyConfiguration>;
  jwtSecret: string;
  jwtExpiration: string;
  oauth2?: OAuth2Configuration;
  roles?: RoleConfiguration;
  quotas?: QuotaConfiguration;
}

// API Key configuration
export interface ApiKeyConfiguration {
  role: 'admin' | 'agent' | 'readonly' | 'limited';
  permissions: string[];
  description?: string;
  expiresAt?: string;
  createdAt?: string;
  lastUsed?: string;
}

// OAuth2 configuration
export interface OAuth2Configuration {
  providers: string[];
  clientId?: string;
  clientSecret?: string;
  redirectUri?: string;
  scope?: string;
  authorizationUrl?: string;
  tokenUrl?: string;
  userInfoUrl?: string;
}

// Role configuration
export interface RoleConfiguration {
  admin: RoleDefinition;
  agent: RoleDefinition;
  readonly: RoleDefinition;
  limited: RoleDefinition;
}

export interface RoleDefinition {
  permissions: string[];
  description?: string;
  inherits?: string[];
  quotas?: UserQuotaConfiguration;
}

// Quota configuration
export interface QuotaConfiguration {
  admin: UserQuotaConfiguration;
  agent: UserQuotaConfiguration;
  readonly: UserQuotaConfiguration;
  limited: UserQuotaConfiguration;
}

export interface UserQuotaConfiguration {
  requestsPerMinute: number;
  maxConcurrentSessions: number;
  maxQueryTime?: number;
  maxMemoryPerQuery?: number;
  maxResultsPerQuery?: number;
  maxStorageSize?: number;
}

// WebSocket Server configuration
export interface WebSocketServerConfiguration extends BaseConfiguration {
  port: number;
  maxConnections: number;
  heartbeatInterval: number;
  compression?: boolean;
  perMessageDeflate?: boolean;
  maxPayload?: number;
  idleTimeout?: number;
}

// Backend configuration
export interface BackendConfiguration {
  swiplPath: string;
  args: string[];
  cwd?: string;
  port: number;
  maxResultsPerChunk: number;
  streamingEnabled: boolean;
  timeout: number;
  retries: number;
  retryDelay: number;
}

// Notification configuration
export interface NotificationConfiguration extends BaseConfiguration {
  webSocketPort: number;
  enableProgressUpdates: boolean;
  progressUpdateInterval: number;
  enableBatchNotifications: boolean;
  maxConcurrentQueries: number;
  queueSize?: number;
  persistNotifications?: boolean;
}

// Concurrency configuration
export interface ConcurrencyConfiguration extends BaseConfiguration {
  maxConcurrentQueries: number;
  queueSize: number;
  priorityLevels: string[];
  resourceQuota: ResourceQuotaConfiguration;
  loadBalancing?: LoadBalancingConfiguration;
}

export interface ResourceQuotaConfiguration {
  maxConcurrentQueries: number;
  maxSessions: number;
  maxQueryTime: number;
  maxMemoryPerQuery: number;
  maxResultsPerQuery: number;
  rateLimitPerMinute: number;
  maxDiskUsage?: number;
  maxCpuUsage?: number;
}

export interface LoadBalancingConfiguration {
  strategy: 'round_robin' | 'least_connections' | 'weighted' | 'priority';
  weights?: Record<string, number>;
  healthCheck?: HealthCheckConfiguration;
}

export interface HealthCheckConfiguration {
  enabled: boolean;
  interval: number;
  timeout: number;
  retries: number;
  endpoint?: string;
}

// History configuration
export interface HistoryConfiguration extends BaseConfiguration {
  storageDir: string;
  maxEntries: number;
  enablePersistence: boolean;
  compressionEnabled: boolean;
  retentionDays: number;
  indexing?: IndexingConfiguration;
  backup?: BackupConfiguration;
}

export interface IndexingConfiguration {
  enabled: boolean;
  fields: string[];
  fullTextSearch: boolean;
  caseSensitive: boolean;
}

export interface BackupConfiguration {
  enabled: boolean;
  interval: number;
  maxBackups: number;
  compression: boolean;
  location: string;
}

// Scheduler configuration
export interface SchedulerConfiguration extends BaseConfiguration {
  maxScheduledQueries: number;
  checkInterval: number;
  enableRecurring: boolean;
  enableConditional: boolean;
  enableDependencies: boolean;
  persistence?: PersistenceConfiguration;
  monitoring?: MonitoringConfiguration;
}

export interface PersistenceConfiguration {
  enabled: boolean;
  storageDir: string;
  format: 'json' | 'sqlite' | 'mongodb';
  connectionString?: string;
  options?: Record<string, any>;
}

export interface MonitoringConfiguration {
  enabled: boolean;
  metricsInterval: number;
  alerting?: AlertingConfiguration;
}

export interface AlertingConfiguration {
  enabled: boolean;
  thresholds: {
    failureRate: number;
    responseTime: number;
    queueSize: number;
    memoryUsage: number;
  };
  channels: AlertChannelConfiguration[];
}

export interface AlertChannelConfiguration {
  type: 'email' | 'webhook' | 'slack' | 'teams';
  config: Record<string, any>;
  enabled: boolean;
}

// Session configuration
export interface SessionConfiguration extends BaseConfiguration {
  storageDir: string;
  maxSessions: number;
  defaultResourceQuota: ResourceQuotaConfiguration;
  enablePersistence: boolean;
  autoSaveInterval: number;
  maxSnapshotsPerSession: number;
  cleanup?: CleanupConfiguration;
}

export interface CleanupConfiguration {
  enabled: boolean;
  interval: number;
  maxIdleTime: number;
  maxAge: number;
  preserveActive: boolean;
}

// Logging configuration
export interface LoggingConfiguration extends BaseConfiguration {
  level: 'debug' | 'info' | 'warn' | 'error';
  format: 'simple' | 'json' | 'structured';
  output: 'console' | 'file' | 'both';
  file?: FileLoggingConfiguration;
  console?: ConsoleLoggingConfiguration;
  remote?: RemoteLoggingConfiguration;
}

export interface FileLoggingConfiguration {
  path: string;
  maxSize: number;
  maxFiles: number;
  compress: boolean;
  datePattern?: string;
}

export interface ConsoleLoggingConfiguration {
  colorize: boolean;
  timestamp: boolean;
  level: boolean;
}

export interface RemoteLoggingConfiguration {
  endpoint: string;
  apiKey?: string;
  batchSize: number;
  flushInterval: number;
  retries: number;
}

// Security configuration
export interface SecurityConfiguration extends BaseConfiguration {
  encryption: EncryptionConfiguration;
  audit: AuditConfiguration;
  firewall?: FirewallConfiguration;
  rateLimit?: SecurityRateLimitConfiguration;
}

export interface EncryptionConfiguration {
  enabled: boolean;
  algorithm: 'aes-256-gcm' | 'aes-256-cbc' | 'chacha20-poly1305';
  keyDerivation: 'pbkdf2' | 'scrypt' | 'argon2';
  keySize: number;
  iterations?: number;
}

export interface AuditConfiguration {
  enabled: boolean;
  logLevel: 'all' | 'security' | 'errors';
  storage: 'file' | 'database' | 'remote';
  retention: number;
  encryption: boolean;
}

export interface FirewallConfiguration {
  enabled: boolean;
  allowedIPs: string[];
  blockedIPs: string[];
  allowedPorts: number[];
  blockedPorts: number[];
  rules: FirewallRule[];
}

export interface FirewallRule {
  name: string;
  action: 'allow' | 'deny';
  source?: string;
  destination?: string;
  port?: number;
  protocol?: 'tcp' | 'udp' | 'http' | 'https';
}

export interface SecurityRateLimitConfiguration {
  enabled: boolean;
  maxRequests: number;
  windowMs: number;
  blockDuration: number;
  whitelist: string[];
  blacklist: string[];
}

// Performance configuration
export interface PerformanceConfiguration extends BaseConfiguration {
  caching: CachingConfiguration;
  optimization: OptimizationConfiguration;
  monitoring: PerformanceMonitoringConfiguration;
}

export interface CachingConfiguration {
  enabled: boolean;
  provider: 'memory' | 'redis' | 'memcached';
  ttl: number;
  maxSize: number;
  compression: boolean;
  connectionString?: string;
}

export interface OptimizationConfiguration {
  enableCompression: boolean;
  enableMinification: boolean;
  enableBundling: boolean;
  lazyLoading: boolean;
  preloading: string[];
  codesplitting: boolean;
}

export interface PerformanceMonitoringConfiguration {
  enabled: boolean;
  sampleRate: number;
  thresholds: {
    responseTime: number;
    memoryUsage: number;
    cpuUsage: number;
    errorRate: number;
  };
  reporting: {
    interval: number;
    endpoint?: string;
    format: 'json' | 'prometheus' | 'statsd';
  };
}

// Development configuration
export interface DevelopmentConfiguration extends BaseConfiguration {
  debug: boolean;
  verbose: boolean;
  hotReload: boolean;
  sourceMap: boolean;
  profiling: boolean;
  testing: TestingConfiguration;
}

export interface TestingConfiguration {
  enabled: boolean;
  framework: 'mocha' | 'jest' | 'vitest';
  coverage: boolean;
  coverageThreshold: number;
  mockData: boolean;
  fixtures: string[];
}

// Complete configuration interface
export interface CompleteConfiguration {
  prolog: PrologConfiguration;
  backend: BackendConfiguration;
  notifications: NotificationConfiguration;
  concurrency: ConcurrencyConfiguration;
  history: HistoryConfiguration;
  scheduler: SchedulerConfiguration;
  sessions: SessionConfiguration;
  logging: LoggingConfiguration;
  security: SecurityConfiguration;
  performance: PerformanceConfiguration;
  development: DevelopmentConfiguration;
}

// Configuration validation types
export interface ConfigurationValidator {
  validate: (config: any) => ConfigurationValidationResult;
  validateSection: (section: string, config: any) => ConfigurationValidationResult;
  getDefaults: () => CompleteConfiguration;
  merge: (base: any, override: any) => any;
}

export interface ConfigurationValidationResult {
  valid: boolean;
  errors: ConfigurationError[];
  warnings: ConfigurationWarning[];
}

export interface ConfigurationError {
  path: string;
  message: string;
  value?: any;
  expected?: any;
}

export interface ConfigurationWarning {
  path: string;
  message: string;
  suggestion?: string;
}

// Configuration manager types
export interface ConfigurationManager {
  get: <T>(path: string, defaultValue?: T) => T;
  set: (path: string, value: any) => void;
  has: (path: string) => boolean;
  delete: (path: string) => void;
  reset: (path?: string) => void;
  load: (source: string | object) => void;
  save: (target?: string) => void;
  watch: (path: string, callback: (value: any, oldValue: any) => void) => () => void;
  validate: () => ConfigurationValidationResult;
  getSchema: () => any;
  export: (format: 'json' | 'yaml' | 'toml') => string;
  import: (content: string, format: 'json' | 'yaml' | 'toml') => void;
}
