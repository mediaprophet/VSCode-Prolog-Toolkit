/**
 * Safe access utilities for handling null/undefined values in strict mode
 */

/**
 * Safely access a property that might be undefined
 */
export function safeGet<T, K extends keyof T>(obj: T | null | undefined, key: K): T[K] | undefined {
  return obj?.[key];
}

/**
 * Safely access a nested property
 */
export function safeGetNested<T>(obj: any, path: string): T | undefined {
  if (!obj || typeof obj !== 'object') return undefined;

  const keys = path.split('.');
  let current = obj;

  for (const key of keys) {
    if (current == null || typeof current !== 'object') return undefined;
    current = current[key];
  }

  return current as T;
}

/**
 * Provide a default value if the input is null or undefined
 */
export function withDefault<T>(value: T | null | undefined, defaultValue: T): T {
  return value ?? defaultValue;
}

/**
 * Check if a value is not null or undefined
 */
export function isNotNullish<T>(value: T | null | undefined): value is T {
  return value != null;
}

/**
 * Filter out null and undefined values from an array
 */
export function filterNullish<T>(array: (T | null | undefined)[]): T[] {
  return array.filter(isNotNullish);
}

/**
 * Safely access array element
 */
export function safeArrayAccess<T>(array: T[] | null | undefined, index: number): T | undefined {
  if (!array || index < 0 || index >= array.length) return undefined;
  return array[index];
}

/**
 * Safely call a function if it exists
 */
export function safeCall<T extends (...args: any[]) => any>(
  fn: T | null | undefined,
  ...args: Parameters<T>
): ReturnType<T> | undefined {
  return fn?.(...args);
}

/**
 * Create a safe property accessor
 */
export function createSafeAccessor<T extends Record<string, any>>(obj: T | null | undefined) {
  return {
    get<K extends keyof T>(key: K): T[K] | undefined {
      return obj?.[key];
    },
    has<K extends keyof T>(key: K): boolean {
      return obj != null && key in obj;
    },
    keys(): (keyof T)[] {
      return obj ? (Object.keys(obj) as (keyof T)[]) : [];
    },
  };
}

/**
 * Safely parse JSON with error handling
 */
export function safeJsonParse<T = any>(json: string | null | undefined): T | undefined {
  if (!json) return undefined;
  try {
    return JSON.parse(json) as T;
  } catch {
    return undefined;
  }
}

/**
 * Safely convert to string
 */
export function safeToString(value: any): string {
  if (value == null) return '';
  if (typeof value === 'string') return value;
  if (typeof value === 'object') {
    try {
      return JSON.stringify(value);
    } catch {
      return String(value);
    }
  }
  return String(value);
}

/**
 * Safely convert to number
 */
export function safeToNumber(value: any): number | undefined {
  if (value == null) return undefined;
  const num = Number(value);
  return isNaN(num) ? undefined : num;
}

/**
 * Safely convert to boolean
 */
export function safeToBoolean(value: any): boolean {
  if (value == null) return false;
  if (typeof value === 'boolean') return value;
  if (typeof value === 'string') {
    const lower = value.toLowerCase();
    return lower === 'true' || lower === '1' || lower === 'yes';
  }
  if (typeof value === 'number') return value !== 0;
  return Boolean(value);
}
