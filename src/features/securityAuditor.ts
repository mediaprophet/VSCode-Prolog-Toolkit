import * as fs from 'fs';
import * as path from 'path';
import { NodeEventEmitter } from '../shim/eventemitter-shim.js';

export interface SecurityEvent {
  id: string;
  type:
    | 'authentication_success'
    | 'authentication_failure'
    | 'authorization_denied'
    | 'quota_exceeded'
    | 'suspicious_query_blocked'
    | 'rate_limit_exceeded'
    | 'resource_limit_exceeded'
    | 'dangerous_predicate_blocked'
    | 'sandbox_violation';
  userId?: string | undefined;
  userRole?: string | undefined;
  ipAddress?: string | undefined;
  userAgent?: string | undefined;
  timestamp: Date;
  severity: 'low' | 'medium' | 'high' | 'critical';
  message: string;
  details: {
    query?: string | undefined;
    endpoint?: string | undefined;
    resource?: string | undefined;
    limit?: number | undefined;
    actual?: number | undefined;
    predicate?: string | undefined;
    violation?: string | undefined;
    [key: string]: any;
  };
  metadata?: {
    sessionId?: string | undefined;
    requestId?: string | undefined;
    duration?: number | undefined;
    [key: string]: any;
  };
}

export interface SecurityAlert {
  id: string;
  type: 'threshold_exceeded' | 'pattern_detected' | 'anomaly_detected';
  severity: 'medium' | 'high' | 'critical';
  message: string;
  events: SecurityEvent[];
  timestamp: Date;
  acknowledged: boolean;
  acknowledgedBy?: string;
  acknowledgedAt?: Date;
}

export interface AuditConfig {
  enabled: boolean;
  logLevel: 'none' | 'basic' | 'detailed';
  includeResults: boolean;
  retention: number; // days
  format: 'json' | 'text';
  outputPath: string;
  alerting: {
    enabled: boolean;
    thresholds: {
      failed_auth_attempts: number;
      quota_violations: number;
      blocked_queries: number;
      time_window: number; // minutes
    };
    notifications: ('log' | 'webhook' | 'email')[];
    webhookUrl?: string;
    emailConfig?: {
      smtp: string;
      from: string;
      to: string[];
    };
  };
}

/**
 * Security Auditor for comprehensive security event logging and monitoring
 */
export interface SecurityAuditorEventMap {
  securityEvent: [SecurityEvent];
  alertAcknowledged: [SecurityAlert];
  securityAlert: [SecurityAlert];
}

export class SecurityAuditor extends NodeEventEmitter<SecurityAuditorEventMap> {
  private config: AuditConfig;
  private events: SecurityEvent[] = [];
  private alerts: SecurityAlert[] = [];
  private eventCounts: Map<string, { count: number; windowStart: Date }> = new Map();
  private logStream?: fs.WriteStream;

  constructor(config: AuditConfig) {
    super();
    this.config = config;
    this.setupLogStream();
    this.setupCleanupInterval();
  }

  /**
   * Log a security event
   */
  logSecurityEvent(event: Omit<SecurityEvent, 'id' | 'timestamp'>): void {
    if (!this.config.enabled) return;

    const securityEvent: SecurityEvent = {
      ...event,
      id: this.generateEventId(),
      timestamp: new Date(),
    };

    // Store event
    this.events.push(securityEvent);

    // Log to file/console
    this.writeEventLog(securityEvent);

    // Check for alert conditions
    this.checkAlertThresholds(securityEvent);

    // Emit event for real-time monitoring
    this.emit('securityEvent', securityEvent);

    // Cleanup old events if needed
    if (this.events.length > 10000) {
      this.cleanupOldEvents();
    }
  }

  /**
   * Log authentication success
   */
  logAuthenticationSuccess(
    userId: string,
    userRole: string,
    ipAddress?: string,
    userAgent?: string
  ): void {
    this.logSecurityEvent({
      type: 'authentication_success',
      userId,
      userRole,
      ipAddress,
      userAgent,
      severity: 'low',
      message: `User ${userId} authenticated successfully`,
      details: {
        method: 'api_key', // This could be dynamic based on auth method
      },
    });
  }

  /**
   * Log authentication failure
   */
  logAuthenticationFailure(
    reason: string,
    ipAddress?: string,
    userAgent?: string,
    details?: any
  ): void {
    this.logSecurityEvent({
      type: 'authentication_failure',
      ipAddress,
      userAgent,
      severity: 'medium',
      message: `Authentication failed: ${reason}`,
      details: {
        reason,
        ...details,
      },
    });
  }

  /**
   * Log authorization denied
   */
  logAuthorizationDenied(
    userId: string,
    resource: string,
    requiredPermission: string,
    ipAddress?: string
  ): void {
    this.logSecurityEvent({
      type: 'authorization_denied',
      userId,
      ipAddress,
      severity: 'medium',
      message: `Access denied to ${resource} for user ${userId}`,
      details: {
        resource,
        requiredPermission,
      },
    });
  }

  /**
   * Log quota exceeded
   */
  logQuotaExceeded(userId: string, quotaType: string, limit: number, actual: number): void {
    this.logSecurityEvent({
      type: 'quota_exceeded',
      userId,
      severity: 'medium',
      message: `Quota exceeded for user ${userId}: ${quotaType}`,
      details: {
        quotaType,
        limit,
        actual,
      },
    });
  }

  /**
   * Log suspicious query blocked
   */
  logSuspiciousQueryBlocked(
    userId: string,
    query: string,
    reason: string,
    predicate?: string
  ): void {
    this.logSecurityEvent({
      type: 'suspicious_query_blocked',
      userId,
      severity: 'high',
      message: `Suspicious query blocked for user ${userId}: ${reason}`,
      details: {
        query: this.config.includeResults ? query : '[REDACTED]',
        reason,
        predicate,
      },
    });
  }

  /**
   * Log rate limit exceeded
   */
  logRateLimitExceeded(userId: string, endpoint: string, limit: number, ipAddress?: string): void {
    this.logSecurityEvent({
      type: 'rate_limit_exceeded',
      userId,
      ipAddress,
      severity: 'medium',
      message: `Rate limit exceeded for user ${userId} on ${endpoint}`,
      details: {
        endpoint,
        limit,
      },
    });
  }

  /**
   * Log resource limit exceeded
   */
  logResourceLimitExceeded(userId: string, resource: string, limit: number, actual: number): void {
    this.logSecurityEvent({
      type: 'resource_limit_exceeded',
      userId,
      severity: 'medium',
      message: `Resource limit exceeded for user ${userId}: ${resource}`,
      details: {
        resource,
        limit,
        actual,
      },
    });
  }

  /**
   * Log dangerous predicate blocked
   */
  logDangerousPredicateBlocked(userId: string, predicate: string, query: string): void {
    this.logSecurityEvent({
      type: 'dangerous_predicate_blocked',
      userId,
      severity: 'high',
      message: `Dangerous predicate blocked for user ${userId}: ${predicate}`,
      details: {
        predicate,
        query: this.config.includeResults ? query : '[REDACTED]',
      },
    });
  }

  /**
   * Log sandbox violation
   */
  logSandboxViolation(userId: string, violation: string, query: string): void {
    this.logSecurityEvent({
      type: 'sandbox_violation',
      userId,
      severity: 'critical',
      message: `Sandbox violation detected for user ${userId}: ${violation}`,
      details: {
        violation,
        query: this.config.includeResults ? query : '[REDACTED]',
      },
    });
  }

  /**
   * Get security events with filtering
   */
  getSecurityEvents(
    filter: {
      type?: SecurityEvent['type'];
      userId?: string;
      severity?: SecurityEvent['severity'];
      startDate?: Date;
      endDate?: Date;
      limit?: number;
      offset?: number;
    } = {}
  ): {
    events: SecurityEvent[];
    total: number;
  } {
    let filteredEvents = [...this.events];

    // Apply filters
    if (filter.type) {
      filteredEvents = filteredEvents.filter(e => e.type === filter.type);
    }
    if (filter.userId) {
      filteredEvents = filteredEvents.filter(e => e.userId === filter.userId);
    }
    if (filter.severity) {
      filteredEvents = filteredEvents.filter(e => e.severity === filter.severity);
    }
    if (filter.startDate) {
      filteredEvents = filteredEvents.filter(e => e.timestamp >= filter.startDate!);
    }
    if (filter.endDate) {
      filteredEvents = filteredEvents.filter(e => e.timestamp <= filter.endDate!);
    }

    // Sort by timestamp (newest first)
    filteredEvents.sort((a, b) => b.timestamp.getTime() - a.timestamp.getTime());

    const total = filteredEvents.length;
    const offset = filter.offset || 0;
    const limit = filter.limit || 100;

    return {
      events: filteredEvents.slice(offset, offset + limit),
      total,
    };
  }

  /**
   * Get security alerts
   */
  getSecurityAlerts(
    filter: {
      acknowledged?: boolean;
      severity?: SecurityAlert['severity'];
      limit?: number;
    } = {}
  ): SecurityAlert[] {
    let alerts = [...this.alerts];

    if (filter.acknowledged !== undefined) {
      alerts = alerts.filter(a => a.acknowledged === filter.acknowledged);
    }
    if (filter.severity) {
      alerts = alerts.filter(a => a.severity === filter.severity);
    }

    // Sort by timestamp (newest first)
    alerts.sort((a, b) => b.timestamp.getTime() - a.timestamp.getTime());

    return alerts.slice(0, filter.limit || 50);
  }

  /**
   * Acknowledge a security alert
   */
  acknowledgeAlert(alertId: string, acknowledgedBy: string): boolean {
    const alert = this.alerts.find(a => a.id === alertId);
    if (alert && !alert.acknowledged) {
      alert.acknowledged = true;
      alert.acknowledgedBy = acknowledgedBy;
      alert.acknowledgedAt = new Date();
      this.emit('alertAcknowledged', alert);
      return true;
    }
    return false;
  }

  /**
   * Get security statistics
   */
  getSecurityStatistics(timeWindow: number = 24): {
    totalEvents: number;
    eventsByType: { [type: string]: number };
    eventsBySeverity: { [severity: string]: number };
    topUsers: { userId: string; eventCount: number }[];
    alertsGenerated: number;
    unacknowledgedAlerts: number;
  } {
    const cutoff = new Date(Date.now() - timeWindow * 60 * 60 * 1000);
    const recentEvents = this.events.filter(e => e.timestamp >= cutoff);

    const eventsByType: { [type: string]: number } = {};
    const eventsBySeverity: { [severity: string]: number } = {};
    const userEventCounts: { [userId: string]: number } = {};

    for (const event of recentEvents) {
      eventsByType[event.type] = (eventsByType[event.type] || 0) + 1;
      eventsBySeverity[event.severity] = (eventsBySeverity[event.severity] || 0) + 1;

      if (event.userId) {
        userEventCounts[event.userId] = (userEventCounts[event.userId] || 0) + 1;
      }
    }

    const topUsers = Object.entries(userEventCounts)
      .map(([userId, eventCount]) => ({ userId, eventCount }))
      .sort((a, b) => b.eventCount - a.eventCount)
      .slice(0, 10);

    return {
      totalEvents: recentEvents.length,
      eventsByType,
      eventsBySeverity,
      topUsers,
      alertsGenerated: this.alerts.length,
      unacknowledgedAlerts: this.alerts.filter(a => !a.acknowledged).length,
    };
  }

  /**
   * Export security events to file
   */
  async exportEvents(
    filePath: string,
    filter: Parameters<typeof this.getSecurityEvents>[0] = {}
  ): Promise<void> {
    const { events } = this.getSecurityEvents(filter);
    const data = JSON.stringify(events, null, 2);
    await fs.promises.writeFile(filePath, data, 'utf8');
  }

  /**
   * Setup log stream for file output
   */
  private setupLogStream(): void {
    if (this.config.enabled && this.config.outputPath) {
      try {
        // Ensure directory exists
        const dir = path.dirname(this.config.outputPath);
        if (!fs.existsSync(dir)) {
          fs.mkdirSync(dir, { recursive: true });
        }

        this.logStream = fs.createWriteStream(this.config.outputPath, { flags: 'a' });
      } catch (error) {
        console.error('[SecurityAuditor] Failed to setup log stream:', error);
      }
    }
  }

  /**
   * Write event to log
   */
  private writeEventLog(event: SecurityEvent): void {
    if (this.config.logLevel === 'none') return;

    const logEntry = this.formatLogEntry(event);

    // Write to file
    if (this.logStream) {
      this.logStream.write(logEntry + '\n');
    }

    // Write to console for high severity events
    if (event.severity === 'high' || event.severity === 'critical') {
      console.warn(`[SecurityAuditor] ${event.severity.toUpperCase()}: ${event.message}`);
    }
  }

  /**
   * Format log entry
   */
  private formatLogEntry(event: SecurityEvent): string {
    if (this.config.format === 'json') {
      return JSON.stringify(event);
    } else {
      const timestamp = event.timestamp.toISOString();
      const severity = event.severity.toUpperCase().padEnd(8);
      const type = event.type.padEnd(25);
      const user = event.userId ? `[${event.userId}]` : '[anonymous]';
      return `${timestamp} ${severity} ${type} ${user} ${event.message}`;
    }
  }

  /**
   * Check alert thresholds
   */
  private checkAlertThresholds(event: SecurityEvent): void {
    if (!this.config.alerting.enabled) return;

    const thresholds = this.config.alerting.thresholds;
    const windowMs = thresholds.time_window * 60 * 1000;
    const now = new Date();

    // Check failed authentication attempts
    if (event.type === 'authentication_failure') {
      this.checkThreshold('failed_auth', thresholds.failed_auth_attempts, windowMs, now, event);
    }

    // Check quota violations
    if (event.type === 'quota_exceeded') {
      this.checkThreshold('quota_violations', thresholds.quota_violations, windowMs, now, event);
    }

    // Check blocked queries
    if (event.type === 'suspicious_query_blocked' || event.type === 'dangerous_predicate_blocked') {
      this.checkThreshold('blocked_queries', thresholds.blocked_queries, windowMs, now, event);
    }
  }

  /**
   * Check specific threshold
   */
  private checkThreshold(
    key: string,
    threshold: number,
    windowMs: number,
    now: Date,
    event: SecurityEvent
  ): void {
    const countData = this.eventCounts.get(key);

    if (!countData || now.getTime() - countData.windowStart.getTime() > windowMs) {
      // Reset window
      this.eventCounts.set(key, { count: 1, windowStart: now });
      return;
    }

    countData.count++;

    if (countData.count >= threshold) {
      this.generateAlert(key, threshold, countData.count, event);
      // Reset counter after generating alert
      this.eventCounts.set(key, { count: 0, windowStart: now });
    }
  }

  /**
   * Generate security alert
   */
  private generateAlert(
    type: string,
    threshold: number,
    actual: number,
    triggerEvent: SecurityEvent
  ): void {
    const alert: SecurityAlert = {
      id: this.generateEventId(),
      type: 'threshold_exceeded',
      severity: triggerEvent.severity === 'critical' ? 'critical' : 'high',
      message: `Security threshold exceeded: ${type} (${actual}/${threshold})`,
      events: [triggerEvent],
      timestamp: new Date(),
      acknowledged: false,
    };

    this.alerts.push(alert);
    this.emit('securityAlert', alert);

    // Send notifications
    this.sendAlertNotifications(alert);
  }

  /**
   * Send alert notifications
   */
  private sendAlertNotifications(alert: SecurityAlert): void {
    const notifications = this.config.alerting.notifications;

    if (notifications.includes('log')) {
      console.error(`[SecurityAuditor] ALERT: ${alert.message}`);
    }

    if (notifications.includes('webhook') && this.config.alerting.webhookUrl) {
      this.sendWebhookNotification(alert);
    }

    if (notifications.includes('email') && this.config.alerting.emailConfig) {
      this.sendEmailNotification(alert);
    }
  }

  /**
   * Send webhook notification
   */
  private async sendWebhookNotification(alert: SecurityAlert): Promise<void> {
    try {
      const response = await fetch(this.config.alerting.webhookUrl!, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          alert,
          timestamp: new Date().toISOString(),
          source: 'VSCode Prolog Toolkit Security Auditor',
        }),
      });

      if (!response.ok) {
        console.error('[SecurityAuditor] Webhook notification failed:', response.statusText);
      }
    } catch (error) {
      console.error('[SecurityAuditor] Webhook notification error:', error);
    }
  }

  /**
   * Send email notification (placeholder)
   */
  private async sendEmailNotification(alert: SecurityAlert): Promise<void> {
    // This would require an email library like nodemailer
    console.log('[SecurityAuditor] Email notification would be sent:', alert.message);
  }

  /**
   * Generate unique event ID
   */
  private generateEventId(): string {
    return `sec_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  }

  /**
   * Cleanup old events based on retention policy
   */
  private cleanupOldEvents(): void {
    const cutoff = new Date(Date.now() - this.config.retention * 24 * 60 * 60 * 1000);
    this.events = this.events.filter(e => e.timestamp >= cutoff);
    this.alerts = this.alerts.filter(a => a.timestamp >= cutoff);
  }

  /**
   * Setup cleanup interval
   */
  private setupCleanupInterval(): void {
    // Run cleanup every hour
    setInterval(
      () => {
        this.cleanupOldEvents();
      },
      60 * 60 * 1000
    );
  }

  /**
   * Shutdown auditor
   */
  async shutdown(): Promise<void> {
    if (this.logStream) {
      this.logStream.end();
    }
  }
}

/**
 * Default audit configuration
 */
export const defaultAuditConfig: AuditConfig = {
  enabled: true,
  logLevel: 'detailed',
  includeResults: false,
  retention: 30, // 30 days
  format: 'json',
  outputPath: './logs/security-audit.log',
  alerting: {
    enabled: true,
    thresholds: {
      failed_auth_attempts: 5,
      quota_violations: 3,
      blocked_queries: 10,
      time_window: 60, // 1 hour
    },
    notifications: ['log'],
  },
};
