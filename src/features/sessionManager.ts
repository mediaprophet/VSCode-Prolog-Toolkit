
import type { ResourceQuota } from './concurrencyManager';
export type { ResourceQuota } from './concurrencyManager';

export interface SessionConfig {
  id: string;
  name: string;
  description?: string | undefined;
  userId?: string | undefined;
  agentId?: string | undefined;
  createdAt: number;
  lastAccessedAt: number;
  isActive: boolean;
  metadata?: Record<string, unknown> | undefined;
  resourceQuota?: Partial<ResourceQuota>;
  persistenceEnabled: boolean;
  autoSave: boolean;
  maxIdleTime?: number; // in milliseconds
}

export interface SessionState {
  sessionId: string;
  prologFacts: string[];
  prologRules: string[];
  loadedFiles: string[];
  consultedModules: string[];
  rdfTriples: Array<{
    subject: string;
    predicate: string;
    object: string;
    graph?: string;
  }>;
  variables: Record<string, unknown>;
  customPredicates: string[];
  timestamp: number;
  checksum?: string;
}

export interface SessionSnapshot {
  sessionId: string;
  name: string;
  description?: string | undefined;
  state: SessionState;
  createdAt: number;
  metadata?: Record<string, unknown> | undefined;
}

export interface SessionManagerOptions {
  storageDir: string;
  maxSessions: number;
  defaultResourceQuota: Partial<ResourceQuota>;
  autoCleanupInterval: number;
  maxIdleTime: number;
  enablePersistence: boolean;
  enableAutoSave: boolean;
  autoSaveInterval: number;
  compressionEnabled: boolean;
}

/**
 * Session and State Management System
 * Supports multiple concurrent sessions for different agents or users
 * with state persistence and resource isolation
 */

/**
 * DEPRECATED: All logic has been migrated to modular orchestrator and modules.
 * This file now only exports types and interfaces for backward compatibility.
 * Use SessionManagerOrchestrator and related modules instead.
 */
