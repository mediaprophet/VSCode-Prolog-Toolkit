export interface PrologResult {
  type: 'success' | 'failure' | 'error' | 'multiple';
  bindings?: Record<string, any>[];
  message?: string;
  error?: string;
  count?: number;
  hasMore?: boolean;
}

export interface FormattingOptions {
  maxResults?: number;
  maxLineLength?: number;
  useCodeBlocks?: boolean;
  showLineNumbers?: boolean;
  highlightVariables?: boolean;
  compactMode?: boolean;
  locale?: string;
}
