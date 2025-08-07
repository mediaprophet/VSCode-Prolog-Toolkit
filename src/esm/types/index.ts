/**
 * Core type definitions for VSCode Prolog Toolkit
 * This file exports all type definitions used throughout the extension
 */

// Note: SDK types are imported directly where needed to avoid conflicts

// Core extension types
export * from './api.js';
export * from './backend.js';
export * from './configuration.js';
export * from './utils.js';
export * from './vscode.js';

// Common utility types
export type Awaitable<T> = T | Promise<T>;
export type Optional<T, K extends keyof T> = Omit<T, K> & Partial<Pick<T, K>>;
export type RequiredKeys<T, K extends keyof T> = T & Required<Pick<T, K>>;
export type DeepPartial<T> = {
  [P in keyof T]?: T[P] extends object ? DeepPartial<T[P]> : T[P];
};

// Error handling types
export interface ErrorWithCode extends Error {
  code?: string;
  statusCode?: number;
  details?: Record<string, any>;
}

export interface ValidationError extends ErrorWithCode {
  field?: string;
  value?: any;
  constraint?: string;
}

// Event types
export type EventCallback<T = any> = (data: T) => void;
export type EventMap = Record<string, (...args: any[]) => void>;

// Generic result types
export interface Result<T, E = Error> {
  success: boolean;
  data?: T;
  error?: E;
  message?: string;
}

export interface PaginatedResult<T> {
  items: T[];
  total: number;
  limit: number;
  offset: number;
  hasMore: boolean;
}

// Time-related types
export interface TimeRange {
  start: Date;
  end: Date;
}

export interface Timestamp {
  createdAt: Date;
  updatedAt?: Date;
}

// Generic ID types
export type UUID = string;
export type EntityId = string;

// Status types
export type Status = 'active' | 'inactive' | 'pending' | 'error' | 'completed';
export type Priority = 'low' | 'medium' | 'high' | 'critical';

// Configuration types
export interface BaseConfig {
  enabled: boolean;
  [key: string]: any;
}

// Logging types
export type LogLevel = 'debug' | 'info' | 'warn' | 'error';
export interface LogEntry {
  level: LogLevel;
  message: string;
  timestamp: Date;
  context?: Record<string, any>;
}
