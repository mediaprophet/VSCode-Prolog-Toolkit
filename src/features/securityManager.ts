import { EventEmitter } from 'events';
import { spawn, ChildProcess } from 'child_process';
import * as path from 'path';
import * as fs from 'fs';

export interface SecurityConfig {
  sandboxing: {
    enabled: boolean;
    isolatedProcesses: boolean;
    fileSystemRestrictions: string[];
    networkAccess: boolean;
    dangerousPredicates: string[];
  };
  resourceLimits: {
    maxInferenceSteps: number;
    maxCallDepth: number;
    maxChoicePoints: number;
    maxMemoryPerQuery: number; // in MB
    maxExecutionTime: number; // in milliseconds
  };
  queryValidation: {
    syntaxChecking: boolean;
    predicateBlacklist: string[];
    maxQueryLength: number;
  };
}

export interface QuerySecurityContext {
  userId: string;
  role: string;
  permissions: string[];
  trusted: boolean;
  resourceQuota: ResourceQuota;
}

export interface ResourceQuota {
  maxConcurrentQueries: number;
  maxSessionsPerUser: number;
  maxQueryTime: number;
  maxMemoryPerQuery: number;
  maxResultsPerQuery: number;
  rateLimitPerMinute: number;
}

export interface SecurityViolation {
  type: 'dangerous_predicate' | 'resource_limit' | 'syntax_error' | 'permission_denied';
  message: string;
  query?: string;
  userId?: string;
  timestamp: Date;
  severity: 'low' | 'medium' | 'high' | 'critical';
}

/**
 * Security Manager for Prolog query execution
 * Handles sandboxing, resource quotas, and query validation
 */
export class SecurityManager extends EventEmitter {
  private config: SecurityConfig;
  private activeProcesses: Map<string, ChildProcess> = new Map();
  private userResourceUsage: Map<string, {
    activeQueries: number;
    activeSessions: number;
    lastActivity: Date;
    requestCount: number;
    requestWindow: Date;
  }> = new Map();

  constructor(config: SecurityConfig) {
    super();
    this.config = config;
    this.setupCleanupInterval();
  }

  /**
   * Validate a query before execution
   */
  async validateQuery(query: string, context: QuerySecurityContext): Promise<{
    valid: boolean;
    violations: SecurityViolation[];
  }> {
    const violations: SecurityViolation[] = [];

    // Check query length
    if (query.length > this.config.queryValidation.maxQueryLength) {
      violations.push({
        type: 'syntax_error',
        message: `Query exceeds maximum length of ${this.config.queryValidation.maxQueryLength} characters`,
        query,
        userId: context.userId,
        timestamp: new Date(),
        severity: 'medium'
      });
    }

    // Check for dangerous predicates
    if (!context.trusted) {
      const dangerousPredicates = this.config.sandboxing.dangerousPredicates;
      for (const predicate of dangerousPredicates) {
        if (query.includes(predicate)) {
          violations.push({
            type: 'dangerous_predicate',
            message: `Query contains dangerous predicate: ${predicate}`,
            query,
            userId: context.userId,
            timestamp: new Date(),
            severity: 'high'
          });
        }
      }
    }

    // Check resource quotas
    const resourceViolations = await this.checkResourceQuotas(context);
    violations.push(...resourceViolations);

    // Syntax validation (if enabled)
    if (this.config.queryValidation.syntaxChecking) {
      const syntaxViolations = await this.validateSyntax(query);
      violations.push(...syntaxViolations);
    }

    // Emit security events
    for (const violation of violations) {
      this.emit('securityViolation', violation);
    }

    return {
      valid: violations.length === 0,
      violations
    };
  }

  /**
   * Execute query in sandbox if required
   */
  async executeSecureQuery(
    query: string, 
    context: QuerySecurityContext,
    originalExecutor: (query: string) => Promise<any>
  ): Promise<any> {
    // Validate query first
    const validation = await this.validateQuery(query, context);
    if (!validation.valid) {
      throw new Error(`Security validation failed: ${validation.violations.map(v => v.message).join(', ')}`);
    }

    // Update resource usage tracking
    this.updateResourceUsage(context.userId);

    // Execute in sandbox if required for untrusted users
    if (this.config.sandboxing.enabled && !context.trusted) {
      return await this.executeSandboxedQuery(query, context);
    } else {
      // Execute normally with resource limits
      return await this.executeWithResourceLimits(query, context, originalExecutor);
    }
  }

  /**
   * Execute query in isolated sandbox process
   */
  private async executeSandboxedQuery(
    query: string, 
    context: QuerySecurityContext
  ): Promise<any> {
    const queryId = `sandbox_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
    
    return new Promise((resolve, reject) => {
      // Create sandbox configuration
      const sandboxConfig = {
        query,
        resourceLimits: this.config.resourceLimits,
        allowedPredicates: this.getAllowedPredicates(context),
        timeout: context.resourceQuota.maxQueryTime
      };

      // Spawn isolated Prolog process
      const sandboxProcess = spawn('swipl', [
        '-g', 'halt',
        '--quiet',
        '--no-tty',
        '-t', 'halt(1)'
      ], {
        stdio: ['pipe', 'pipe', 'pipe'],
        timeout: context.resourceQuota.maxQueryTime,
        env: {
          ...process.env,
          PROLOG_STACK_LIMIT: `${context.resourceQuota.maxMemoryPerQuery}M`,
          PROLOG_TABLE_SPACE: `${Math.floor(context.resourceQuota.maxMemoryPerQuery / 4)}M`
        }
      });

      this.activeProcesses.set(queryId, sandboxProcess);

      let output = '';
      let errorOutput = '';

      sandboxProcess.stdout?.on('data', (data) => {
        output += data.toString();
      });

      sandboxProcess.stderr?.on('data', (data) => {
        errorOutput += data.toString();
      });

      sandboxProcess.on('close', (code) => {
        this.activeProcesses.delete(queryId);
        
        if (code === 0) {
          try {
            const result = this.parseSandboxOutput(output);
            resolve(result);
          } catch (error) {
            reject(new Error(`Failed to parse sandbox output: ${error}`));
          }
        } else {
          reject(new Error(`Sandbox execution failed: ${errorOutput}`));
        }
      });

      sandboxProcess.on('error', (error) => {
        this.activeProcesses.delete(queryId);
        reject(new Error(`Sandbox process error: ${error.message}`));
      });

      // Send query to sandbox process
      const sandboxQuery = this.buildSandboxQuery(query, sandboxConfig);
      sandboxProcess.stdin?.write(sandboxQuery);
      sandboxProcess.stdin?.end();

      // Set timeout
      setTimeout(() => {
        if (this.activeProcesses.has(queryId)) {
          sandboxProcess.kill('SIGKILL');
          this.activeProcesses.delete(queryId);
          reject(new Error('Query execution timeout in sandbox'));
        }
      }, context.resourceQuota.maxQueryTime);
    });
  }

  /**
   * Execute query with resource limits but not in sandbox
   */
  private async executeWithResourceLimits(
    query: string,
    context: QuerySecurityContext,
    originalExecutor: (query: string) => Promise<any>
  ): Promise<any> {
    const startTime = Date.now();
    const queryId = `limited_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;

    // Set up timeout
    const timeoutPromise = new Promise((_, reject) => {
      setTimeout(() => {
        reject(new Error(`Query execution timeout: ${context.resourceQuota.maxQueryTime}ms`));
      }, context.resourceQuota.maxQueryTime);
    });

    // Execute with resource monitoring
    const executionPromise = originalExecutor(query).then(result => {
      const executionTime = Date.now() - startTime;
      
      // Log resource usage
      this.emit('resourceUsage', {
        queryId,
        userId: context.userId,
        executionTime,
        memoryUsage: process.memoryUsage(),
        resultCount: Array.isArray(result.results) ? result.results.length : 0
      });

      // Check result limits
      if (Array.isArray(result.results) && 
          result.results.length > context.resourceQuota.maxResultsPerQuery) {
        result.results = result.results.slice(0, context.resourceQuota.maxResultsPerQuery);
        result.truncated = true;
        result.truncatedAt = context.resourceQuota.maxResultsPerQuery;
      }

      return result;
    });

    return Promise.race([executionPromise, timeoutPromise]);
  }

  /**
   * Check resource quotas for user
   */
  private async checkResourceQuotas(context: QuerySecurityContext): Promise<SecurityViolation[]> {
    const violations: SecurityViolation[] = [];
    const usage = this.getUserResourceUsage(context.userId || 'anonymous');
    const quota = context.resourceQuota;

    // Check concurrent queries
    if (usage.activeQueries >= quota.maxConcurrentQueries) {
      violations.push({
        type: 'resource_limit',
        message: `Maximum concurrent queries exceeded: ${usage.activeQueries}/${quota.maxConcurrentQueries}`,
        userId: context.userId,
        timestamp: new Date(),
        severity: 'medium'
      });
    }

    // Check session limit
    if (usage.activeSessions >= quota.maxSessionsPerUser) {
      violations.push({
        type: 'resource_limit',
        message: `Maximum sessions exceeded: ${usage.activeSessions}/${quota.maxSessionsPerUser}`,
        userId: context.userId,
        timestamp: new Date(),
        severity: 'medium'
      });
    }

    // Check rate limiting
    const now = new Date();
    const windowStart = new Date(now.getTime() - 60000); // 1 minute window
    
    if (usage.requestWindow < windowStart) {
      // Reset window
      usage.requestCount = 0;
      usage.requestWindow = now;
    }

    if (usage.requestCount >= quota.rateLimitPerMinute) {
      violations.push({
        type: 'resource_limit',
        message: `Rate limit exceeded: ${usage.requestCount}/${quota.rateLimitPerMinute} requests per minute`,
        userId: context.userId,
        timestamp: new Date(),
        severity: 'high'
      });
    }

    return violations;
  }

  /**
   * Validate Prolog syntax
   */
  private async validateSyntax(query: string): Promise<SecurityViolation[]> {
    const violations: SecurityViolation[] = [];

    try {
      // Basic syntax checks
      const balanced = this.checkBalancedParentheses(query);
      if (!balanced) {
        violations.push({
          type: 'syntax_error',
          message: 'Unbalanced parentheses in query',
          query,
          timestamp: new Date(),
          severity: 'medium'
        });
      }

      // Check for valid Prolog structure
      if (!this.isValidPrologStructure(query)) {
        violations.push({
          type: 'syntax_error',
          message: 'Invalid Prolog syntax structure',
          query,
          timestamp: new Date(),
          severity: 'medium'
        });
      }

    } catch (error) {
      violations.push({
        type: 'syntax_error',
        message: `Syntax validation error: ${error}`,
        query,
        timestamp: new Date(),
        severity: 'low'
      });
    }

    return violations;
  }

  /**
   * Get user resource usage, creating if not exists
   */
  private getUserResourceUsage(userId: string) {
    if (!this.userResourceUsage.has(userId)) {
      this.userResourceUsage.set(userId, {
        activeQueries: 0,
        activeSessions: 0,
        lastActivity: new Date(),
        requestCount: 0,
        requestWindow: new Date()
      });
    }
    return this.userResourceUsage.get(userId)!;
  }

  /**
   * Update resource usage for user
   */
  private updateResourceUsage(userId: string): void {
    const usage = this.getUserResourceUsage(userId);
    usage.activeQueries++;
    usage.requestCount++;
    usage.lastActivity = new Date();
  }

  /**
   * Get allowed predicates for security context
   */
  private getAllowedPredicates(context: QuerySecurityContext): string[] {
    const basePredicate = [
      'member/2', 'append/3', 'length/2', 'reverse/2', 'sort/2',
      'findall/3', 'bagof/3', 'setof/3', 'forall/2',
      'between/3', 'succ/2', 'plus/3', 'is/2',
      '=/2', '\\=/2', '==/2', '\\==/2', '@</2', '@>/2',
      'var/1', 'nonvar/1', 'atom/1', 'number/1', 'compound/1',
      'functor/3', 'arg/3', '=../2', 'copy_term/2'
    ];

    if (context.role === 'admin') {
      return ['*']; // Admin can use any predicate
    }

    if (context.trusted) {
      return [...basePredicate, 'assert/1', 'retract/1', 'retractall/1'];
    }

    return basePredicate;
  }

  /**
   * Build sandbox query with security constraints
   */
  private buildSandboxQuery(query: string, config: any): string {
    return `
      % Set resource limits
      set_prolog_flag(stack_limit, ${config.resourceLimits.maxMemoryPerQuery * 1024 * 1024}).
      set_prolog_flag(table_space, ${Math.floor(config.resourceLimits.maxMemoryPerQuery / 4) * 1024 * 1024}).
      
      % Disable dangerous predicates
      :- abolish(shell/1).
      :- abolish(system/1).
      :- abolish(halt/0).
      :- abolish(halt/1).
      :- abolish(abort/0).
      
      % Execute query with error handling
      sandbox_query :-
        catch(
          (${query}),
          Error,
          (write('ERROR: '), write(Error), nl)
        ).
      
      % Run query and output results
      :- sandbox_query.
    `;
  }

  /**
   * Parse output from sandbox execution
   */
  private parseSandboxOutput(output: string): any {
    try {
      // Simple parsing - in production, this would be more sophisticated
      const lines = output.trim().split('\n');
      const results = [];
      
      for (const line of lines) {
        if (line.startsWith('ERROR:')) {
          throw new Error(line.substring(6).trim());
        }
        if (line.trim() && !line.startsWith('%')) {
          results.push({ solution: line.trim() });
        }
      }
      
      return {
        status: 'ok',
        results,
        sandboxed: true
      };
    } catch (error) {
      throw new Error(`Failed to parse sandbox output: ${error}`);
    }
  }

  /**
   * Check if parentheses are balanced
   */
  private checkBalancedParentheses(query: string): boolean {
    let count = 0;
    for (const char of query) {
      if (char === '(') count++;
      if (char === ')') count--;
      if (count < 0) return false;
    }
    return count === 0;
  }

  /**
   * Basic Prolog structure validation
   */
  private isValidPrologStructure(query: string): boolean {
    // Very basic validation - in production, use proper Prolog parser
    const trimmed = query.trim();
    if (!trimmed) return false;
    
    // Check for basic Prolog patterns
    const prologPattern = /^[a-zA-Z_][a-zA-Z0-9_]*(\([^)]*\))?(,\s*[a-zA-Z_][a-zA-Z0-9_]*(\([^)]*\))?)*\.?$/;
    return prologPattern.test(trimmed.replace(/\s+/g, ' '));
  }

  /**
   * Clean up inactive processes and old usage data
   */
  private setupCleanupInterval(): void {
    setInterval(() => {
      const now = new Date();
      const maxAge = 5 * 60 * 1000; // 5 minutes

      // Clean up old resource usage data
      for (const [userId, usage] of this.userResourceUsage.entries()) {
        if (now.getTime() - usage.lastActivity.getTime() > maxAge) {
          this.userResourceUsage.delete(userId);
        }
      }

      // Clean up zombie processes
      for (const [queryId, process] of this.activeProcesses.entries()) {
        if (!process.killed && process.exitCode === null) {
          const age = now.getTime() - parseInt(queryId.split('_')[1]);
          if (age > maxAge) {
            process.kill('SIGTERM');
            this.activeProcesses.delete(queryId);
          }
        }
      }
    }, 60000); // Run every minute
  }

  /**
   * Get security statistics
   */
  getSecurityStatistics(): {
    activeProcesses: number;
    trackedUsers: number;
    totalViolations: number;
    resourceUsage: Map<string, any>;
  } {
    return {
      activeProcesses: this.activeProcesses.size,
      trackedUsers: this.userResourceUsage.size,
      totalViolations: this.listenerCount('securityViolation'),
      resourceUsage: this.userResourceUsage
    };
  }

  /**
   * Kill all active sandbox processes
   */
  async shutdown(): Promise<void> {
    for (const [queryId, process] of this.activeProcesses.entries()) {
      if (!process.killed) {
        process.kill('SIGTERM');
      }
    }
    this.activeProcesses.clear();
    this.userResourceUsage.clear();
  }
}

/**
 * Default security configuration
 */
export const defaultSecurityConfig: SecurityConfig = {
  sandboxing: {
    enabled: true,
    isolatedProcesses: true,
    fileSystemRestrictions: ['/tmp', '/var/tmp'],
    networkAccess: false,
    dangerousPredicates: [
      'shell/1', 'system/1', 'exec/1',
      'open/3', 'close/1', 'read/2', 'write/2',
      'see/1', 'tell/1', 'seen/0', 'told/0',
      'halt/0', 'halt/1', 'abort/0',
      'load_files/1', 'consult/1', 'include/1',
      'use_module/1', 'use_module/2'
    ]
  },
  resourceLimits: {
    maxInferenceSteps: 100000,
    maxCallDepth: 1000,
    maxChoicePoints: 10000,
    maxMemoryPerQuery: 100, // MB
    maxExecutionTime: 30000 // 30 seconds
  },
  queryValidation: {
    syntaxChecking: true,
    predicateBlacklist: [
      'shell', 'system', 'exec', 'halt', 'abort',
      'open', 'close', 'read', 'write', 'see', 'tell'
    ],
    maxQueryLength: 10000
  }
};

/**
 * Get default resource quota for role
 */
export function getDefaultResourceQuota(role: string): ResourceQuota {
  const quotas: { [role: string]: ResourceQuota } = {
    admin: {
      maxConcurrentQueries: 100,
      maxSessionsPerUser: 50,
      maxQueryTime: 300000, // 5 minutes
      maxMemoryPerQuery: 500, // MB
      maxResultsPerQuery: 10000,
      rateLimitPerMinute: 1000
    },
    agent: {
      maxConcurrentQueries: 10,
      maxSessionsPerUser: 10,
      maxQueryTime: 60000, // 1 minute
      maxMemoryPerQuery: 100, // MB
      maxResultsPerQuery: 1000,
      rateLimitPerMinute: 60
    },
    readonly: {
      maxConcurrentQueries: 5,
      maxSessionsPerUser: 5,
      maxQueryTime: 30000, // 30 seconds
      maxMemoryPerQuery: 50, // MB
      maxResultsPerQuery: 500,
      rateLimitPerMinute: 30
    },
    limited: {
      maxConcurrentQueries: 2,
      maxSessionsPerUser: 2,
      maxQueryTime: 10000, // 10 seconds
      maxMemoryPerQuery: 25, // MB
      maxResultsPerQuery: 100,
      rateLimitPerMinute: 10
    }
  };

  return quotas[role] || quotas['limited'];
}