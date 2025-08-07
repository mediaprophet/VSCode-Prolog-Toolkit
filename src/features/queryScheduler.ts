import * as vscode from 'vscode';
import type { QueryPriority } from './concurrencyManager.js';
import { ConcurrencyManager } from './concurrencyManager.js';
import { QueryHistoryManager } from './queryHistoryManager.js';

export interface ScheduledQuery {
  id: string;
  cmd: string;
  params: Record<string, unknown>;
  scheduleType: 'immediate' | 'delayed' | 'recurring' | 'conditional';
  scheduleConfig: {
    executeAt?: number; // timestamp for delayed execution
    interval?: number; // milliseconds for recurring queries
    maxExecutions?: number; // limit for recurring queries
    condition?: string; // condition expression for conditional execution
    dependencies?: string[]; // query IDs that must complete first
  };
  priority: QueryPriority;
  createdAt: number;
  lastExecutedAt?: number;
  executionCount: number;
  status: 'scheduled' | 'running' | 'completed' | 'failed' | 'cancelled' | 'paused';
  metadata?: {
    tags?: string[];
    description?: string;
    createdBy?: string;
  };
}

export interface SchedulerOptions {
  maxScheduledQueries: number;
  checkInterval: number;
  enableRecurring: boolean;
  enableConditional: boolean;
  enableDependencies: boolean;
  defaultPriority: QueryPriority;
}

export interface SchedulerStats {
  totalScheduled: number;
  activeScheduled: number;
  completedScheduled: number;
  failedScheduled: number;
  recurringQueries: number;
  conditionalQueries: number;
  dependentQueries: number;
  nextExecutionTime?: number;
}

/**
 * Advanced query scheduling and queuing system
 */
export class QueryScheduler {
  /**
   * Node.js-style event API compatibility: .on(eventName, handler)
   */
  public on(eventName: string, handler: (...args: any[]) => void): void {
    switch (eventName) {
      case 'queryScheduled':
        this.onQueryScheduled(handler);
        break;
      case 'queryScheduleCancelled':
        this.onQueryScheduleCancelled(handler);
        break;
      case 'querySchedulePaused':
        this.onQuerySchedulePaused(handler);
        break;
      case 'queryScheduleResumed':
        this.onQueryScheduleResumed(handler);
        break;
      case 'queryScheduleExecutionStarted':
        this.onQueryScheduleExecutionStarted(handler);
        break;
      case 'queryScheduleExecutionFailed':
        this.onQueryScheduleExecutionFailed(handler);
        break;
      case 'queryScheduleCompleted':
        this.onQueryScheduleCompleted(handler);
        break;
      case 'queryScheduleRecurring':
        this.onQueryScheduleRecurring(handler);
        break;
      case 'queryScheduleError':
        this.onQueryScheduleError(handler);
        break;
      default:
        throw new Error(`Unknown event: ${eventName}`);
    }
  }
  private _onQueryScheduled = new vscode.EventEmitter<ScheduledQuery>();
  public readonly onQueryScheduled = this._onQueryScheduled.event;
  private _onQueryScheduleCancelled = new vscode.EventEmitter<string>();
  public readonly onQueryScheduleCancelled = this._onQueryScheduleCancelled.event;
  private _onQuerySchedulePaused = new vscode.EventEmitter<string>();
  public readonly onQuerySchedulePaused = this._onQuerySchedulePaused.event;
  private _onQueryScheduleResumed = new vscode.EventEmitter<string>();
  public readonly onQueryScheduleResumed = this._onQueryScheduleResumed.event;
  private _onQueryScheduleExecutionStarted = new vscode.EventEmitter<ScheduledQuery>();
  public readonly onQueryScheduleExecutionStarted = this._onQueryScheduleExecutionStarted.event;
  private _onQueryScheduleExecutionFailed = new vscode.EventEmitter<{
    query: ScheduledQuery;
    error: unknown;
  }>();
  public readonly onQueryScheduleExecutionFailed = this._onQueryScheduleExecutionFailed.event;
  private _onQueryScheduleCompleted = new vscode.EventEmitter<ScheduledQuery>();
  public readonly onQueryScheduleCompleted = this._onQueryScheduleCompleted.event;
  private _onQueryScheduleRecurring = new vscode.EventEmitter<ScheduledQuery>();
  public readonly onQueryScheduleRecurring = this._onQueryScheduleRecurring.event;
  private _onQueryScheduleError = new vscode.EventEmitter<{ queryId: string; error: unknown }>();
  public readonly onQueryScheduleError = this._onQueryScheduleError.event;
  private options: SchedulerOptions;
  private concurrencyManager: ConcurrencyManager;
  private historyManager: QueryHistoryManager | undefined;
  private scheduledQueries: Map<string, ScheduledQuery> = new Map();
  private schedulerInterval?: ReturnType<typeof setInterval>;
  private dependencyGraph: Map<string, Set<string>> = new Map(); // queryId -> dependents
  private conditionEvaluator: Map<string, () => boolean> = new Map();

  constructor(
    concurrencyManager: ConcurrencyManager,
    historyManager?: QueryHistoryManager,
    options: Partial<SchedulerOptions> = {}
  ) {
    // no super();

    this.concurrencyManager = concurrencyManager;
    this.historyManager = historyManager;

    this.options = {
      maxScheduledQueries: 1000,
      checkInterval: 1000, // Check every second
      enableRecurring: true,
      enableConditional: true,
      enableDependencies: true,
      defaultPriority: {
        level: 'normal',
        weight: 10,
        timeout: 30000,
      },
      ...options,
    };

    this.startScheduler();
    this.setupConcurrencyManagerListeners();
  }

  /**
   * Schedule a query for execution
   */
  async scheduleQuery(
    id: string,
    cmd: string,
    params: Record<string, unknown> = {},
    scheduleType: ScheduledQuery['scheduleType'] = 'immediate',
    scheduleConfig: ScheduledQuery['scheduleConfig'] = {},
    priority: Partial<QueryPriority> = {},
    metadata?: ScheduledQuery['metadata']
  ): Promise<void> {
    if (this.scheduledQueries.size >= this.options.maxScheduledQueries) {
      throw new Error('Maximum number of scheduled queries reached');
    }

    if (this.scheduledQueries.has(id)) {
      throw new Error(`Query with ID ${id} is already scheduled`);
    }

    const scheduledQuery: ScheduledQuery = {
      id,
      cmd,
      params,
      scheduleType,
      scheduleConfig,
      priority: { ...this.options.defaultPriority, ...priority },
      createdAt: Date.now(),
      executionCount: 0,
      status: 'scheduled',
      metadata: metadata || {},
    };

    // Validate schedule configuration
    this.validateScheduleConfig(scheduledQuery);

    // Set up dependencies if specified
    if (scheduleConfig.dependencies && this.options.enableDependencies) {
      this.setupDependencies(id, scheduleConfig.dependencies);
    }

    // Set up condition evaluator if specified
    if (scheduleConfig.condition && this.options.enableConditional) {
      this.setupConditionEvaluator(id, scheduleConfig.condition);
    }

    this.scheduledQueries.set(id, scheduledQuery);

    this._onQueryScheduled.fire(scheduledQuery);
    console.log(`[QueryScheduler] Scheduled query ${id} (type: ${scheduleType})`);

    // If it's an immediate query, try to execute it right away
    if (scheduleType === 'immediate') {
      await this.tryExecuteQuery(scheduledQuery);
    }
  }

  /**
   * Cancel a scheduled query
   */
  async cancelScheduledQuery(queryId: string): Promise<boolean> {
    const scheduledQuery = this.scheduledQueries.get(queryId);
    if (!scheduledQuery) {
      return false;
    }

    // If the query is currently running, cancel it in the concurrency manager
    if (scheduledQuery.status === 'running') {
      this.concurrencyManager.cancelQuery(queryId);
    }

    scheduledQuery.status = 'cancelled';
    this.scheduledQueries.delete(queryId);

    // Clean up dependencies
    this.cleanupDependencies(queryId);

    this._onQueryScheduleCancelled.fire(queryId);
    console.log(`[QueryScheduler] Cancelled scheduled query ${queryId}`);
    return true;
  }

  /**
   * Pause a recurring query
   */
  async pauseRecurringQuery(queryId: string): Promise<boolean> {
    const scheduledQuery = this.scheduledQueries.get(queryId);
    if (!scheduledQuery || scheduledQuery.scheduleType !== 'recurring') {
      return false;
    }

    scheduledQuery.status = 'paused';
    this._onQuerySchedulePaused.fire(queryId);
    console.log(`[QueryScheduler] Paused recurring query ${queryId}`);
    return true;
  }

  /**
   * Resume a paused recurring query
   */
  async resumeRecurringQuery(queryId: string): Promise<boolean> {
    const scheduledQuery = this.scheduledQueries.get(queryId);
    if (!scheduledQuery || scheduledQuery.status !== 'paused') {
      return false;
    }

    scheduledQuery.status = 'scheduled';
    this._onQueryScheduleResumed.fire(queryId);
    console.log(`[QueryScheduler] Resumed recurring query ${queryId}`);
    return true;
  }

  /**
   * Get all scheduled queries with optional filtering
   */
  getScheduledQueries(filter?: {
    status?: ScheduledQuery['status'][];
    scheduleType?: ScheduledQuery['scheduleType'][];
    tags?: string[];
  }): ScheduledQuery[] {
    let queries = Array.from(this.scheduledQueries.values());

    if (filter?.status) {
      queries = queries.filter(q => filter.status!.includes(q.status));
    }

    if (filter?.scheduleType) {
      queries = queries.filter(q => filter.scheduleType!.includes(q.scheduleType));
    }

    if (filter?.tags) {
      queries = queries.filter(
        q => q.metadata?.tags && filter.tags!.some(tag => q.metadata!.tags!.includes(tag))
      );
    }

    return queries.sort((a, b) => a.createdAt - b.createdAt);
  }

  /**
   * Get scheduler statistics
   */
  getStatistics(): SchedulerStats {
    const queries = Array.from(this.scheduledQueries.values());

    const nextExecution = queries
      .filter(q => q.scheduleType === 'delayed' && q.scheduleConfig.executeAt)
      .map(q => q.scheduleConfig.executeAt!)
      .sort((a, b) => a - b)[0];

    return {
      totalScheduled: queries.length,
      activeScheduled: queries.filter(q => ['scheduled', 'running'].includes(q.status)).length,
      completedScheduled: queries.filter(q => q.status === 'completed').length,
      failedScheduled: queries.filter(q => q.status === 'failed').length,
      recurringQueries: queries.filter(q => q.scheduleType === 'recurring').length,
      conditionalQueries: queries.filter(q => q.scheduleType === 'conditional').length,
      dependentQueries: queries.filter(q => q.scheduleConfig.dependencies?.length).length,
      nextExecutionTime: nextExecution ?? 0,
    };
  }

  /**
   * Register a custom condition evaluator
   */
  registerConditionEvaluator(queryId: string, evaluator: () => boolean): void {
    this.conditionEvaluator.set(queryId, evaluator);
  }

  /**
   * Validate schedule configuration
   */
  private validateScheduleConfig(query: ScheduledQuery): void {
    const { scheduleType, scheduleConfig } = query;

    switch (scheduleType) {
      case 'delayed': {
        if (!scheduleConfig.executeAt || scheduleConfig.executeAt <= Date.now()) {
          throw new Error('Delayed queries must have a future executeAt timestamp');
        }
        break;
      }
      case 'recurring': {
        if (!this.options.enableRecurring) {
          throw new Error('Recurring queries are disabled');
        }
        if (!scheduleConfig.interval || scheduleConfig.interval < 1000) {
          throw new Error('Recurring queries must have an interval of at least 1000ms');
        }
        break;
      }
      case 'conditional': {
        if (!this.options.enableConditional) {
          throw new Error('Conditional queries are disabled');
        }
        if (!scheduleConfig.condition) {
          throw new Error('Conditional queries must have a condition');
        }
        break;
      }
    }

    if (scheduleConfig.dependencies && !this.options.enableDependencies) {
      throw new Error('Query dependencies are disabled');
    }
  }

  /**
   * Set up query dependencies
   */
  private setupDependencies(queryId: string, dependencies: string[]): void {
    for (const depId of dependencies) {
      if (!this.dependencyGraph.has(depId)) {
        this.dependencyGraph.set(depId, new Set());
      }
      this.dependencyGraph.get(depId)!.add(queryId);
    }
  }

  /**
   * Clean up dependencies for a query
   */
  private cleanupDependencies(queryId: string): void {
    // Remove this query as a dependent of others
    for (const [depId, dependents] of this.dependencyGraph.entries()) {
      dependents.delete(queryId);
      if (dependents.size === 0) {
        this.dependencyGraph.delete(depId);
      }
    }

    // Remove this query's own dependencies
    this.dependencyGraph.delete(queryId);
  }

  /**
   * Set up condition evaluator
   */
  private setupConditionEvaluator(queryId: string, condition: string): void {
    // Simple condition evaluator - in a real implementation, this would be more sophisticated
    try {
      const evaluator = new Function('return ' + condition) as () => boolean;
      this.conditionEvaluator.set(queryId, evaluator);
    } catch (_error: unknown) {
      throw new Error(`Invalid condition expression: ${condition}`);
    }
  }

  /**
   * Start the scheduler loop
   */
  private startScheduler(): void {
    this.schedulerInterval = setInterval(() => {
      this.processScheduledQueries();
    }, this.options.checkInterval);
  }

  /**
   * Process all scheduled queries
   */
  private async processScheduledQueries(): Promise<void> {
    const now = Date.now();
    const queries = Array.from(this.scheduledQueries.values());

    for (const query of queries) {
      if (query.status !== 'scheduled') {
        continue;
      }

      try {
        if (await this.shouldExecuteQuery(query, now)) {
          await this.tryExecuteQuery(query);
        }
      } catch (error: unknown) {
        console.error(`[QueryScheduler] Error processing query ${query.id}:`, error);
        query.status = 'failed';
        this._onQueryScheduleError.fire({ queryId: query.id, error });
      }
    }

    // Clean up completed non-recurring queries
    this.cleanupCompletedQueries();
  }

  /**
   * Check if a query should be executed
   */
  private async shouldExecuteQuery(query: ScheduledQuery, now: number): Promise<boolean> {
    switch (query.scheduleType) {
      case 'immediate': {
        return true;
      }
      case 'delayed': {
        return query.scheduleConfig.executeAt! <= now;
      }
      case 'recurring': {
        if (query.status === 'paused') {
          return false;
        }

        if (!query.lastExecutedAt) {
          return true; // First execution
        }

        const timeSinceLastExecution = now - query.lastExecutedAt;
        const shouldExecute = timeSinceLastExecution >= query.scheduleConfig.interval!;

        // Check max executions limit
        if (shouldExecute && query.scheduleConfig.maxExecutions) {
          return query.executionCount < query.scheduleConfig.maxExecutions;
        }

        return shouldExecute;
      }
      case 'conditional': {
        const evaluator = this.conditionEvaluator.get(query.id);
        if (!evaluator) {
          return false;
        }

        try {
          return evaluator();
        } catch (error: unknown) {
          console.error(`[QueryScheduler] Condition evaluation error for ${query.id}:`, error);
          return false;
        }
      }
      default: {
        return false;
      }
    }
  }

  /**
   * Try to execute a scheduled query
   */
  private async tryExecuteQuery(query: ScheduledQuery): Promise<void> {
    // Check dependencies
    if (
      query.scheduleConfig.dependencies &&
      !this.areDependenciesSatisfied(query.scheduleConfig.dependencies)
    ) {
      return; // Dependencies not satisfied yet
    }

    try {
      query.status = 'running';
      query.lastExecutedAt = Date.now();
      query.executionCount++;

      this._onQueryScheduleExecutionStarted.fire(query);

      // Queue the query in the concurrency manager
      await this.concurrencyManager.queueQuery(query.id, query.cmd, query.params, query.priority);
    } catch (error: unknown) {
      query.status = 'failed';
      this._onQueryScheduleExecutionFailed.fire({ query, error });
      console.error(`[QueryScheduler] Failed to execute query ${query.id}:`, error);
    }
  }

  /**
   * Check if all dependencies are satisfied
   */
  private areDependenciesSatisfied(dependencies: string[]): boolean {
    return dependencies.every(depId => {
      const depQuery = this.scheduledQueries.get(depId);
      return depQuery && depQuery.status === 'completed';
    });
  }

  /**
   * Set up listeners for concurrency manager events
   */
  private setupConcurrencyManagerListeners(): void {
    if (typeof (this.concurrencyManager as any).onQueryCompleted === 'function') {
      (this.concurrencyManager as any).onQueryCompleted(
        (event: { queryId: string; success: boolean; error?: string }) => {
          this.handleQueryCompletion(event.queryId, event.success, event.error);
        }
      );
    }
    if (typeof (this.concurrencyManager as any).onQueryCancelled === 'function') {
      (this.concurrencyManager as any).onQueryCancelled((event: { queryId: string }) => {
        this.handleQueryCancellation(event.queryId);
      });
    }
  }

  /**
   * Handle query completion from concurrency manager
   */
  private handleQueryCompletion(queryId: string, success: boolean, error?: string): void {
    const query = this.scheduledQueries.get(queryId);
    if (!query) {
      return;
    }

    if (success) {
      // Handle recurring queries
      if (query.scheduleType === 'recurring') {
        // Check if we've reached max executions
        if (
          query.scheduleConfig.maxExecutions &&
          query.executionCount >= query.scheduleConfig.maxExecutions
        ) {
          query.status = 'completed';
          this._onQueryScheduleCompleted.fire(query);
        } else {
          // Reset to scheduled for next execution
          query.status = 'scheduled';
          this._onQueryScheduleRecurring.fire(query);
        }
      } else {
        query.status = 'completed';
        this._onQueryScheduleCompleted.fire(query);
      }

      // Trigger dependent queries
      this.triggerDependentQueries(queryId);
    } else {
      query.status = 'failed';
      this._onQueryScheduleExecutionFailed.fire({ query, error });
    }

    // Add to history if history manager is available
    if (this.historyManager) {
      this.historyManager.addQuery({
        id: queryId,
        cmd: query.cmd,
        params: query.params,
        status: success ? 'completed' : 'error',
        startTime: query.lastExecutedAt!,
        endTime: Date.now(),
        error: error,
        priority: query.priority.level,
        metadata: {
          tags: query.metadata?.tags ?? [],
          sessionId: 'scheduler',
        },
      });
    }
  }

  /**
   * Handle query cancellation from concurrency manager
   */
  private handleQueryCancellation(queryId: string): void {
    const query = this.scheduledQueries.get(queryId);
    if (query) {
      query.status = 'cancelled';
      this._onQueryScheduleCancelled.fire(queryId);
    }
  }

  /**
   * Trigger queries that depend on the completed query
   */
  private triggerDependentQueries(completedQueryId: string): void {
    const dependents = this.dependencyGraph.get(completedQueryId);
    if (!dependents) {
      return;
    }

    for (const dependentId of dependents) {
      const dependentQuery = this.scheduledQueries.get(dependentId);
      if (dependentQuery && dependentQuery.status === 'scheduled') {
        // Check if all dependencies are now satisfied
        if (this.areDependenciesSatisfied(dependentQuery.scheduleConfig.dependencies || [])) {
          this.tryExecuteQuery(dependentQuery);
        }
      }
    }
  }

  /**
   * Clean up completed non-recurring queries
   */
  private cleanupCompletedQueries(): void {
    const toRemove: string[] = [];

    for (const [id, query] of this.scheduledQueries.entries()) {
      if (
        ['completed', 'failed', 'cancelled'].includes(query.status) &&
        query.scheduleType !== 'recurring'
      ) {
        // Keep completed queries for a while before cleanup
        const timeSinceCompletion = Date.now() - (query.lastExecutedAt || query.createdAt);
        if (timeSinceCompletion > 5 * 60 * 1000) {
          // 5 minutes
          toRemove.push(id);
        }
      }
    }

    for (const id of toRemove) {
      this.scheduledQueries.delete(id);
      this.cleanupDependencies(id);
      this.conditionEvaluator.delete(id);
    }

    if (toRemove.length > 0) {
      console.log(`[QueryScheduler] Cleaned up ${toRemove.length} completed queries`);
    }
  }

  /**
   * Dispose of the scheduler
   */
  dispose(): void {
    if (this.schedulerInterval) {
      clearInterval(this.schedulerInterval);
    }

    // Cancel all scheduled queries
    for (const [id] of this.scheduledQueries.entries()) {
      this.cancelScheduledQuery(id);
    }

    this.scheduledQueries.clear();
    this.dependencyGraph.clear();
    this.conditionEvaluator.clear();
    // Dispose all event emitters
    this._onQueryScheduled.dispose();
    this._onQueryScheduleCancelled.dispose();
    this._onQuerySchedulePaused.dispose();
    this._onQueryScheduleResumed.dispose();
    this._onQueryScheduleExecutionStarted.dispose();
    this._onQueryScheduleExecutionFailed.dispose();
    this._onQueryScheduleCompleted.dispose();
    this._onQueryScheduleRecurring.dispose();
    this._onQueryScheduleError.dispose();

    console.log('[QueryScheduler] Disposed');
  }
}
