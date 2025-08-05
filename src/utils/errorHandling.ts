/**
 * Error handling utilities for strict TypeScript mode
 */

/**
 * Type guard to check if an error is an instance of Error
 */
export function isError(error: unknown): error is Error {
  return error instanceof Error;
}

/**
 * Type guard to check if an error has a specific property
 */
export function hasErrorProperty<K extends string>(
  error: unknown,
  property: K
): error is Error & Record<K, unknown> {
  return isError(error) && property in error;
}

/**
 * Safely extract error message from unknown error
 */
export function getErrorMessage(error: unknown): string {
  if (isError(error)) {
    return error.message;
  }
  if (typeof error === 'string') {
    return error;
  }
  if (error && typeof error === 'object' && 'message' in error) {
    return String(error.message);
  }
  return 'Unknown error occurred';
}

/**
 * Safely extract error code from unknown error
 */
export function getErrorCode(error: unknown): string | undefined {
  if (hasErrorProperty(error, 'code')) {
    return typeof error.code === 'string' ? error.code : String(error.code);
  }
  return undefined;
}

/**
 * Safely extract error stack from unknown error
 */
export function getErrorStack(error: unknown): string | undefined {
  if (isError(error)) {
    return error.stack;
  }
  return undefined;
}

/**
 * Create a standardized error object from unknown error
 */
export interface StandardError {
  message: string;
  code?: string | undefined;
  stack?: string | undefined;
  originalError: unknown;
}

export function standardizeError(error: unknown): StandardError {
  return {
    message: getErrorMessage(error),
    code: getErrorCode(error),
    stack: getErrorStack(error),
    originalError: error
  };
}

/**
 * Safely execute a function and handle errors
 */
export async function safeExecute<T>(
  fn: () => Promise<T>,
  onError?: (error: StandardError) => void
): Promise<T | undefined> {
  try {
    return await fn();
  } catch (error) {
    const standardError = standardizeError(error);
    onError?.(standardError);
    return undefined;
  }
}

/**
 * Safely execute a synchronous function and handle errors
 */
export function safeExecuteSync<T>(
  fn: () => T,
  onError?: (error: StandardError) => void
): T | undefined {
  try {
    return fn();
  } catch (error) {
    const standardError = standardizeError(error);
    onError?.(standardError);
    return undefined;
  }
}

/**
 * Create a result type that can contain either success or error
 */
export interface SafeResult<T, E = StandardError> {
  success: boolean;
  data?: T;
  error?: E;
}

/**
 * Wrap a function to return a SafeResult
 */
export async function wrapWithResult<T>(
  fn: () => Promise<T>
): Promise<SafeResult<T>> {
  try {
    const data = await fn();
    return { success: true, data };
  } catch (error) {
    return { success: false, error: standardizeError(error) };
  }
}

/**
 * Wrap a synchronous function to return a SafeResult
 */
export function wrapWithResultSync<T>(
  fn: () => T
): SafeResult<T> {
  try {
    const data = fn();
    return { success: true, data };
  } catch (error) {
    return { success: false, error: standardizeError(error) };
  }
}

/**
 * Assert that a value is not null or undefined
 */
export function assertNotNull<T>(
  value: T | null | undefined,
  message = 'Value is null or undefined'
): asserts value is T {
  if (value == null) {
    throw new Error(message);
  }
}

/**
 * Assert that a condition is true
 */
export function assert(
  condition: unknown,
  message = 'Assertion failed'
): asserts condition {
  if (!condition) {
    throw new Error(message);
  }
}

/**
 * Create a typed error class
 */
export class TypedError<T extends string = string> extends Error {
  constructor(
    public readonly type: T,
    message: string,
    public readonly details?: Record<string, unknown>
  ) {
    super(message);
    this.name = `TypedError<${type}>`;
  }
}

/**
 * Create specific error types
 */
export class ValidationError extends TypedError<'validation'> {
  constructor(message: string, public readonly field?: string, details?: Record<string, unknown>) {
    super('validation', message, details);
    this.name = 'ValidationError';
  }
}

export class NetworkError extends TypedError<'network'> {
  constructor(message: string, public readonly statusCode?: number, details?: Record<string, unknown>) {
    super('network', message, details);
    this.name = 'NetworkError';
  }
}

export class TimeoutError extends TypedError<'timeout'> {
  constructor(message: string, public readonly timeoutMs?: number, details?: Record<string, unknown>) {
    super('timeout', message, details);
    this.name = 'TimeoutError';
  }
}

/**
 * Retry a function with exponential backoff
 */
export async function retryWithBackoff<T>(
  fn: () => Promise<T>,
  maxRetries = 3,
  baseDelayMs = 1000,
  maxDelayMs = 10000
): Promise<T> {
  let lastError: unknown;
  
  for (let attempt = 0; attempt <= maxRetries; attempt++) {
    try {
      return await fn();
    } catch (error) {
      lastError = error;
      
      if (attempt === maxRetries) {
        throw error;
      }
      
      const delay = Math.min(baseDelayMs * Math.pow(2, attempt), maxDelayMs);
      await new Promise(resolve => setTimeout(resolve, delay));
    }
  }
  
  throw lastError;
}