/**
 * Utility type definitions and helper types
 */

// Safe access utility types
export type SafeArrayAccess<T> = (arr: T[], index: number) => T | undefined;
export type SafeObjectAccess<T> = (obj: Record<string, T>, key: string) => T | undefined;
export type SafePropertyAccess<T, K extends keyof T> = (obj: T, key: K) => T[K] | undefined;

// Configuration utility types
export type ConfigValue<T> = (config: any, key: string, defaultValue: T) => T;
export type DeepConfigValue<T> = (config: any, path: string[], defaultValue: T) => T;

// Error handling utility types
export interface ErrorInfo {
  message: string;
  code?: string;
  stack?: string;
  context?: Record<string, any>;
}

export type ErrorHandler = (error: unknown, context?: string) => ErrorInfo;
export type ErrorLogger = (error: unknown, context: string) => void;
export type ErrorFormatter = (error: unknown) => string;

// Type guards
export type TypeGuard<T> = (value: unknown) => value is T;
export type ErrorTypeGuard = (error: unknown) => error is Error;
export type StringTypeGuard = (value: unknown) => value is string;
export type NumberTypeGuard = (value: unknown) => value is number;
export type ObjectTypeGuard = (value: unknown) => value is Record<string, any>;
export type ArrayTypeGuard<T> = (value: unknown) => value is T[];

// Validation types
export interface ValidationRule<T> {
  name: string;
  validate: (value: T) => boolean;
  message: string;
}

export interface ValidationResult {
  valid: boolean;
  errors: string[];
  warnings?: string[];
}

export type Validator<T> = (value: T, rules: ValidationRule<T>[]) => ValidationResult;

// File system utility types
export interface FileInfo {
  path: string;
  name: string;
  extension: string;
  size: number;
  exists: boolean;
  isDirectory: boolean;
  isFile: boolean;
  lastModified: Date;
}

export interface DirectoryInfo {
  path: string;
  name: string;
  files: FileInfo[];
  directories: DirectoryInfo[];
  totalFiles: number;
  totalSize: number;
}

export type FileReader = (path: string) => Promise<string>;
export type FileWriter = (path: string, content: string) => Promise<void>;
export type FileExists = (path: string) => Promise<boolean>;
export type DirectoryLister = (path: string, recursive?: boolean) => Promise<DirectoryInfo>;

// String utility types
export interface StringUtils {
  isEmpty: (str: string | null | undefined) => boolean;
  isBlank: (str: string | null | undefined) => boolean;
  trim: (str: string | null | undefined) => string;
  capitalize: (str: string) => string;
  camelCase: (str: string) => string;
  kebabCase: (str: string) => string;
  snakeCase: (str: string) => string;
  truncate: (str: string, length: number, suffix?: string) => string;
  escapeHtml: (str: string) => string;
  escapeRegex: (str: string) => string;
}

// Array utility types
export interface ArrayUtils {
  isEmpty: <T>(arr: T[] | null | undefined) => boolean;
  isNotEmpty: <T>(arr: T[] | null | undefined) => arr is T[];
  first: <T>(arr: T[]) => T | undefined;
  last: <T>(arr: T[]) => T | undefined;
  unique: <T>(arr: T[]) => T[];
  chunk: <T>(arr: T[], size: number) => T[][];
  flatten: <T>(arr: T[][]) => T[];
  compact: <T>(arr: (T | null | undefined)[]) => T[];
  partition: <T>(arr: T[], predicate: (item: T) => boolean) => [T[], T[]];
}

// Object utility types
export interface ObjectUtils {
  isEmpty: (obj: Record<string, any> | null | undefined) => boolean;
  isNotEmpty: (obj: Record<string, any> | null | undefined) => obj is Record<string, any>;
  keys: <T extends Record<string, any>>(obj: T) => (keyof T)[];
  values: <T extends Record<string, any>>(obj: T) => T[keyof T][];
  entries: <T extends Record<string, any>>(obj: T) => [keyof T, T[keyof T]][];
  pick: <T, K extends keyof T>(obj: T, keys: K[]) => Pick<T, K>;
  omit: <T, K extends keyof T>(obj: T, keys: K[]) => Omit<T, K>;
  merge: <T extends Record<string, any>>(target: T, ...sources: Partial<T>[]) => T;
  clone: <T>(obj: T) => T;
  deepClone: <T>(obj: T) => T;
  get: <T>(obj: Record<string, any>, path: string, defaultValue?: T) => T;
  set: (obj: Record<string, any>, path: string, value: any) => void;
  has: (obj: Record<string, any>, path: string) => boolean;
}

// Date utility types
export interface DateUtils {
  now: () => Date;
  timestamp: () => number;
  format: (date: Date, format: string) => string;
  parse: (dateString: string, format?: string) => Date | null;
  isValid: (date: Date) => boolean;
  addDays: (date: Date, days: number) => Date;
  addHours: (date: Date, hours: number) => Date;
  addMinutes: (date: Date, minutes: number) => Date;
  diffInDays: (date1: Date, date2: Date) => number;
  diffInHours: (date1: Date, date2: Date) => number;
  diffInMinutes: (date1: Date, date2: Date) => number;
  startOfDay: (date: Date) => Date;
  endOfDay: (date: Date) => Date;
  isToday: (date: Date) => boolean;
  isYesterday: (date: Date) => boolean;
  isTomorrow: (date: Date) => Date;
}

// Promise utility types
export interface PromiseUtils {
  delay: (ms: number) => Promise<void>;
  timeout: <T>(promise: Promise<T>, ms: number) => Promise<T>;
  retry: <T>(fn: () => Promise<T>, attempts: number, delay?: number) => Promise<T>;
  all: <T>(promises: Promise<T>[]) => Promise<T[]>;
  allSettled: <T>(promises: Promise<T>[]) => Promise<
    Array<{
      status: 'fulfilled' | 'rejected';
      value?: T;
      reason?: any;
    }>
  >;
  race: <T>(promises: Promise<T>[]) => Promise<T>;
  series: <T>(tasks: (() => Promise<T>)[]) => Promise<T[]>;
  parallel: <T>(tasks: (() => Promise<T>)[], concurrency: number) => Promise<T[]>;
}

// Logging utility types
export interface Logger {
  debug: (message: string, ...args: any[]) => void;
  info: (message: string, ...args: any[]) => void;
  warn: (message: string, ...args: any[]) => void;
  error: (message: string, error?: Error, ...args: any[]) => void;
  trace: (message: string, ...args: any[]) => void;
}

export interface LoggerConfig {
  level: 'debug' | 'info' | 'warn' | 'error';
  format: 'simple' | 'json' | 'structured';
  output: 'console' | 'file' | 'both';
  filePath?: string;
  maxFileSize?: number;
  maxFiles?: number;
}

// Cache utility types
export interface CacheEntry<T> {
  value: T;
  timestamp: number;
  ttl: number;
  hits: number;
}

export interface Cache<T> {
  get: (key: string) => T | undefined;
  set: (key: string, value: T, ttl?: number) => void;
  has: (key: string) => boolean;
  delete: (key: string) => boolean;
  clear: () => void;
  size: () => number;
  keys: () => string[];
  values: () => T[];
  entries: () => [string, T][];
  stats: () => {
    hits: number;
    misses: number;
    hitRate: number;
    size: number;
    memoryUsage: number;
  };
}

// Event emitter utility types
export interface EventEmitterUtils {
  once: <T>(emitter: any, event: string) => Promise<T>;
  timeout: <T>(emitter: any, event: string, ms: number) => Promise<T>;
  race: <T>(emitter: any, events: string[]) => Promise<{ event: string; data: T }>;
  filter: <T>(emitter: any, event: string, predicate: (data: T) => boolean) => Promise<T>;
  map: <T, U>(emitter: any, event: string, mapper: (data: T) => U) => Promise<U>;
}

// Path utility types
export interface PathUtils {
  join: (...paths: string[]) => string;
  resolve: (...paths: string[]) => string;
  relative: (from: string, to: string) => string;
  dirname: (path: string) => string;
  basename: (path: string, ext?: string) => string;
  extname: (path: string) => string;
  normalize: (path: string) => string;
  isAbsolute: (path: string) => boolean;
  parse: (path: string) => {
    root: string;
    dir: string;
    base: string;
    ext: string;
    name: string;
  };
  format: (pathObject: {
    root?: string;
    dir?: string;
    base?: string;
    ext?: string;
    name?: string;
  }) => string;
}

// URL utility types
export interface UrlUtils {
  parse: (url: string) => {
    protocol: string;
    host: string;
    hostname: string;
    port: string;
    pathname: string;
    search: string;
    hash: string;
    query: Record<string, string>;
  } | null;
  format: (urlObject: {
    protocol?: string;
    host?: string;
    hostname?: string;
    port?: string | number;
    pathname?: string;
    search?: string;
    hash?: string;
    query?: Record<string, string>;
  }) => string;
  resolve: (base: string, relative: string) => string;
  isValid: (url: string) => boolean;
  addQuery: (url: string, params: Record<string, string>) => string;
  removeQuery: (url: string, keys: string[]) => string;
}

// Crypto utility types
export interface CryptoUtils {
  hash: (data: string, algorithm?: 'md5' | 'sha1' | 'sha256' | 'sha512') => string;
  hmac: (data: string, key: string, algorithm?: 'sha256' | 'sha512') => string;
  randomBytes: (size: number) => string;
  randomString: (length: number, charset?: string) => string;
  uuid: () => string;
  encrypt: (data: string, key: string) => string;
  decrypt: (encryptedData: string, key: string) => string;
}

// Performance utility types
export interface PerformanceTimer {
  start: () => void;
  end: () => number;
  elapsed: () => number;
  reset: () => void;
}

export interface PerformanceUtils {
  timer: () => PerformanceTimer;
  measure: <T>(name: string, fn: () => T) => T;
  measureAsync: <T>(name: string, fn: () => Promise<T>) => Promise<T>;
  benchmark: <T>(
    fn: () => T,
    iterations: number
  ) => {
    average: number;
    min: number;
    max: number;
    total: number;
    iterations: number;
  };
  memory: () => {
    used: number;
    total: number;
    free: number;
    usage: number;
  };
}

// Debounce and throttle utility types
export interface DebounceOptions {
  leading?: boolean;
  trailing?: boolean;
  maxWait?: number;
}

export interface ThrottleOptions {
  leading?: boolean;
  trailing?: boolean;
}

export interface TimingUtils {
  debounce: <T extends (...args: any[]) => any>(
    func: T,
    wait: number,
    options?: DebounceOptions
  ) => T & { cancel: () => void; flush: () => void };

  throttle: <T extends (...args: any[]) => any>(
    func: T,
    wait: number,
    options?: ThrottleOptions
  ) => T & { cancel: () => void; flush: () => void };

  once: <T extends (...args: any[]) => any>(func: T) => T;

  memoize: <T extends (...args: any[]) => any>(
    func: T,
    resolver?: (...args: Parameters<T>) => string
  ) => T & { cache: Map<string, ReturnType<T>> };
}

// Collection of all utility interfaces
export interface Utils {
  string: StringUtils;
  array: ArrayUtils;
  object: ObjectUtils;
  date: DateUtils;
  promise: PromiseUtils;
  path: PathUtils;
  url: UrlUtils;
  crypto: CryptoUtils;
  performance: PerformanceUtils;
  timing: TimingUtils;
  eventEmitter: EventEmitterUtils;

  // Safe access utilities
  safeArrayAccess: SafeArrayAccess<any>;
  safeObjectAccess: SafeObjectAccess<any>;
  getConfigValue: ConfigValue<any>;

  // Error handling utilities
  isError: ErrorTypeGuard;
  getErrorMessage: ErrorFormatter;
  formatError: ErrorFormatter;
  logError: ErrorLogger;

  // Type guards
  isString: StringTypeGuard;
  isNumber: NumberTypeGuard;
  isObject: ObjectTypeGuard;
  isArray: ArrayTypeGuard<any>;

  // File system utilities
  readFile: FileReader;
  writeFile: FileWriter;
  fileExists: FileExists;
  listDirectory: DirectoryLister;

  // Validation utilities
  validate: Validator<any>;

  // Cache utilities
  createCache: <T>(maxSize?: number, defaultTtl?: number) => Cache<T>;

  // Logger utilities
  createLogger: (config: LoggerConfig) => Logger;
}

// Export utility functions
export * from '../utils/errorHandling.js';
export * from '../utils/safeAccess.js';
