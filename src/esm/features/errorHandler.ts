import { window } from 'vscode';

export interface PrologError {
  code: string;
  type: 'syntax' | 'runtime' | 'system' | 'network' | 'permission' | 'resource' | 'user';
  message: string;
  details?: string;
  suggestion?: string;
  documentation?: string;
  line?: number;
  column?: number;
  file?: string;
  context?: string;
  severity: 'error' | 'warning' | 'info';
}

export interface ErrorContext {
  command?: string;
  query?: string;
  file?: string;
  operation?: string;
  userInput?: string;
}

export class ErrorHandler {
  private errorCodes: Map<string, Partial<PrologError>> = new Map();
  private locale: string = 'en';

  constructor(locale: string = 'en') {
    this.locale = locale;
    this.initializeErrorCodes();
  }

  /**
   * Initialize common error codes and their descriptions
   */
  private initializeErrorCodes(): void {
    // Syntax Errors
    this.errorCodes.set('SYNTAX_ERROR', {
      type: 'syntax',
      message: 'Syntax error in Prolog code',
      suggestion:
        'Check your Prolog syntax. Common issues include missing periods, unmatched parentheses, or incorrect operators.',
      documentation: 'https://www.swi-prolog.org/pldoc/man?section=syntax',
    });

    this.errorCodes.set('UNMATCHED_PARENTHESES', {
      type: 'syntax',
      message: 'Unmatched parentheses in expression',
      suggestion: 'Ensure all opening parentheses have corresponding closing parentheses.',
      documentation: 'https://www.swi-prolog.org/pldoc/man?section=syntax-terms',
    });

    this.errorCodes.set('MISSING_PERIOD', {
      type: 'syntax',
      message: 'Missing period at end of clause',
      suggestion: 'Add a period (.) at the end of your Prolog clause or fact.',
      documentation: 'https://www.swi-prolog.org/pldoc/man?section=syntax-clauses',
    });

    this.errorCodes.set('INVALID_OPERATOR', {
      type: 'syntax',
      message: 'Invalid or undefined operator',
      suggestion:
        'Check if the operator is correctly spelled and defined. Use :- op/3 to define custom operators.',
      documentation: 'https://www.swi-prolog.org/pldoc/man?section=operators',
    });

    // Runtime Errors
    this.errorCodes.set('EXISTENCE_ERROR', {
      type: 'runtime',
      message: 'Predicate or resource does not exist',
      suggestion:
        'Check if the predicate is defined or the file exists. Use listing/1 to see available predicates.',
      documentation: 'https://www.swi-prolog.org/pldoc/man?section=exception',
    });

    this.errorCodes.set('TYPE_ERROR', {
      type: 'runtime',
      message: 'Argument has wrong type',
      suggestion:
        'Check the types of your arguments. Use var/1, atom/1, number/1, etc. to test types.',
      documentation: 'https://www.swi-prolog.org/pldoc/man?section=typetest',
    });

    this.errorCodes.set('INSTANTIATION_ERROR', {
      type: 'runtime',
      message: 'Variable is not sufficiently instantiated',
      suggestion:
        'Ensure variables have values before using them in operations that require instantiated arguments.',
      documentation: 'https://www.swi-prolog.org/pldoc/man?section=exception',
    });

    this.errorCodes.set('DOMAIN_ERROR', {
      type: 'runtime',
      message: 'Argument is outside expected domain',
      suggestion:
        'Check if the argument value is within the expected range or domain for this predicate.',
      documentation: 'https://www.swi-prolog.org/pldoc/man?section=exception',
    });

    this.errorCodes.set('PERMISSION_ERROR', {
      type: 'permission',
      message: 'Operation not permitted',
      suggestion:
        'You may not have permission to perform this operation. Check file permissions or predicate access rights.',
      documentation: 'https://www.swi-prolog.org/pldoc/man?section=exception',
    });

    // System Errors
    this.errorCodes.set('BACKEND_NOT_AVAILABLE', {
      type: 'system',
      message: 'Prolog backend is not available',
      suggestion:
        'The Prolog backend may be starting up or has crashed. Try restarting the extension or check your SWI-Prolog installation.',
      documentation: 'Check VS Code output panel for detailed error messages.',
    });

    this.errorCodes.set('BACKEND_TIMEOUT', {
      type: 'system',
      message: 'Backend request timed out',
      suggestion:
        'The query may be taking too long or the backend is unresponsive. Try simplifying your query or restarting the backend.',
      documentation: 'Consider using call_with_time_limit/2 for long-running queries.',
    });

    this.errorCodes.set('BACKEND_CRASH', {
      type: 'system',
      message: 'Prolog backend has crashed',
      suggestion:
        'The backend will be automatically restarted. If this persists, check your SWI-Prolog installation.',
      documentation: 'Check VS Code output panel for crash details.',
    });

    // Network Errors
    this.errorCodes.set('CONNECTION_FAILED', {
      type: 'network',
      message: 'Failed to connect to Prolog server',
      suggestion:
        'Check if SWI-Prolog is running and the HTTP server is accessible on the configured port.',
      documentation: 'Verify your network settings and firewall configuration.',
    });

    this.errorCodes.set('HTTP_ERROR', {
      type: 'network',
      message: 'HTTP request failed',
      suggestion:
        'There was a problem communicating with the Prolog server. Check network connectivity.',
      documentation: 'Check VS Code output panel for detailed HTTP error information.',
    });

    // Resource Errors
    this.errorCodes.set('MEMORY_LIMIT', {
      type: 'resource',
      message: 'Memory limit exceeded',
      suggestion:
        'Your query or program is using too much memory. Try optimizing your code or increasing memory limits.',
      documentation: 'Use set_prolog_flag/2 to adjust memory limits if needed.',
    });

    this.errorCodes.set('STACK_OVERFLOW', {
      type: 'resource',
      message: 'Stack overflow detected',
      suggestion:
        'Your program may have infinite recursion. Check your recursive predicates for proper base cases.',
      documentation: 'https://www.swi-prolog.org/pldoc/man?section=debugger',
    });

    // User Input Errors
    this.errorCodes.set('INVALID_COMMAND', {
      type: 'user',
      message: 'Invalid or unrecognized command',
      suggestion: 'Check the command syntax. Use /help to see available commands.',
      documentation: 'Type /help for a list of available chat commands.',
    });

    this.errorCodes.set('MISSING_ARGUMENT', {
      type: 'user',
      message: 'Required argument is missing',
      suggestion: 'This command requires additional arguments. Check the command syntax.',
      documentation: 'Use /help <command> for specific command usage.',
    });

    this.errorCodes.set('INVALID_FILE_PATH', {
      type: 'user',
      message: 'Invalid file path specified',
      suggestion:
        'Check if the file path exists and is accessible. Use relative paths from the workspace root.',
      documentation: 'File paths should be relative to your VS Code workspace.',
    });

    // Package Management Errors
    this.errorCodes.set('PACK_NOT_FOUND', {
      type: 'user',
      message: 'Prolog pack not found',
      suggestion:
        'Check the pack name spelling. Use /prolog pack search <keyword> to find available packs.',
      documentation: 'Visit https://www.swi-prolog.org/pack/list for available packs.',
    });

    this.errorCodes.set('PACK_INSTALL_FAILED', {
      type: 'system',
      message: 'Failed to install Prolog pack',
      suggestion:
        'Check your internet connection and SWI-Prolog installation. You may need administrator privileges.',
      documentation: 'Check VS Code output panel for detailed installation errors.',
    });

    this.errorCodes.set('PACK_DEPENDENCY_ERROR', {
      type: 'system',
      message: 'Pack dependency conflict',
      suggestion:
        'This pack conflicts with installed packs or has unmet dependencies. Try updating existing packs first.',
      documentation: 'Use /prolog pack info <name> to see pack dependencies.',
    });
  }

  /**
   * Process and format an error for display
   */
  handleError(error: unknown, context?: ErrorContext): PrologError {
    const prologError = this.parseError(error, context);
    this.logError(prologError, context);
    return prologError;
  }

  /**
   * Parse various error formats into standardized PrologError
   */
  private parseError(error: unknown, context?: ErrorContext): PrologError {
    // Handle string errors
    if (typeof error === 'string') {
      return this.parseStringError(error, context);
    }

    // Handle Error objects
    if (error instanceof Error) {
      return this.parseErrorObject(error, context);
    }

    // Handle Prolog error terms
    if (typeof error === 'object' && error !== null && 'functor' in error) {
      const errorTerm = error as { functor: string; args?: unknown[] };
      if (typeof errorTerm.functor === 'string') {
        return this.parsePrologErrorTerm(
          Object.assign(
            { functor: errorTerm.functor },
            errorTerm.args !== undefined ? { args: errorTerm.args } : {}
          ),
          context
        );
      }
    }

    // Handle HTTP errors
    if (
      typeof error === 'object' &&
      error !== null &&
      ('response' in error || 'request' in error)
    ) {
      const httpError = error as {
        code?: string;
        response?: { status: number; statusText?: string };
        message?: string;
      };
      return this.parseHttpError(httpError, context);
    }

    // Handle structured errors
    if (typeof error === 'object' && error !== null && 'code' in error) {
      const structuredError = error as {
        code?: string;
        message?: string;
        details?: string;
        line?: number;
        column?: number;
        file?: string;
      };
      if (typeof structuredError.code === 'string') {
        return this.parseStructuredError(structuredError, context);
      }
    }

    // Fallback for unknown error types
    return this.createGenericError(error, context);
  }

  /**
   * Parse string error messages
   */
  private parseStringError(errorStr: string, context?: ErrorContext): PrologError {
    const lowerError = errorStr.toLowerCase();

    // Syntax errors
    if (lowerError.includes('syntax error')) {
      return this.createError('SYNTAX_ERROR', errorStr, context);
    }

    // Existence errors
    if (lowerError.includes('existence_error')) {
      return this.createError('EXISTENCE_ERROR', errorStr, context);
    }

    // Type errors
    if (lowerError.includes('type_error')) {
      return this.createError('TYPE_ERROR', errorStr, context);
    }

    // Permission errors
    if (lowerError.includes('permission_error')) {
      return this.createError('PERMISSION_ERROR', errorStr, context);
    }

    // Timeout errors
    if (lowerError.includes('timeout') || lowerError.includes('time limit')) {
      return this.createError('BACKEND_TIMEOUT', errorStr, context);
    }

    // Connection errors
    if (lowerError.includes('connection') || lowerError.includes('econnrefused')) {
      return this.createError('CONNECTION_FAILED', errorStr, context);
    }

    // Generic error
    return this.createGenericError(errorStr, context);
  }

  /**
   * Parse Error objects
   */
  private parseErrorObject(error: Error, context?: ErrorContext): PrologError {
    if (error.name === 'SyntaxError') {
      return this.createError('SYNTAX_ERROR', error.message, context);
    }

    if (error.name === 'TypeError') {
      return this.createError('TYPE_ERROR', error.message, context);
    }

    if (error.message.includes('ECONNREFUSED')) {
      return this.createError('CONNECTION_FAILED', error.message, context);
    }

    if (error.message.includes('timeout')) {
      return this.createError('BACKEND_TIMEOUT', error.message, context);
    }

    return this.createGenericError(error.message, context);
  }

  /**
   * Parse Prolog error terms
   */
  private parsePrologErrorTerm(
    errorTerm: { functor: string; args?: unknown[] },
    context?: ErrorContext
  ): PrologError {
    const functor = errorTerm.functor;
    const args = errorTerm.args || [];

    switch (functor) {
      case 'error':
        if (args.length >= 1) {
          const formalError = args[0];
          if (typeof formalError === 'object' && formalError !== null && 'functor' in formalError) {
            return this.parseFormalError(
              formalError as { functor: string; args?: unknown[] },
              context
            );
          }
        }
        break;

      case 'syntax_error':
        return this.createError('SYNTAX_ERROR', args.join(' '), context);

      case 'existence_error':
        return this.createError(
          'EXISTENCE_ERROR',
          `${args[0]} '${args[1]}' does not exist`,
          context
        );

      case 'type_error':
        return this.createError('TYPE_ERROR', `Expected ${args[0]}, got ${args[1]}`, context);

      case 'permission_error':
        return this.createError('PERMISSION_ERROR', `Cannot ${args[0]} ${args[1]}`, context);
    }

    return this.createGenericError(JSON.stringify(errorTerm), context);
  }

  /**
   * Parse formal Prolog errors
   */
  private parseFormalError(
    formalError: { functor: string; args?: unknown[] },
    context?: ErrorContext
  ): PrologError {
    const functor = formalError.functor;
    const args = formalError.args || [];

    const errorMap: Record<string, string> = {
      syntax_error: 'SYNTAX_ERROR',
      existence_error: 'EXISTENCE_ERROR',
      type_error: 'TYPE_ERROR',
      domain_error: 'DOMAIN_ERROR',
      instantiation_error: 'INSTANTIATION_ERROR',
      permission_error: 'PERMISSION_ERROR',
      resource_error: 'MEMORY_LIMIT',
    };

    const errorCode = errorMap[functor] || 'RUNTIME_ERROR';
    const message = args.length > 0 ? args.join(' ') : formalError.functor;

    return this.createError(errorCode, message, context);
  }

  /**
   * Parse HTTP errors
   */
  private parseHttpError(
    error: { code?: string; response?: { status: number; statusText?: string }; message?: string },
    context?: ErrorContext
  ): PrologError {
    if (error.code === 'ECONNREFUSED') {
      return this.createError('CONNECTION_FAILED', 'Connection refused', context);
    }

    if (error.code === 'ETIMEDOUT') {
      return this.createError('BACKEND_TIMEOUT', 'Request timed out', context);
    }

    if (error.response) {
      const status = error.response.status;
      const statusText = error.response.statusText || 'Unknown error';
      return this.createError('HTTP_ERROR', `HTTP ${status}: ${statusText}`, context);
    }

    return this.createError('HTTP_ERROR', error.message || 'HTTP request failed', context);
  }

  /**
   * Parse structured error objects
   */
  private parseStructuredError(
    error: {
      code?: string;
      message?: string;
      details?: string;
      line?: number;
      column?: number;
      file?: string;
    },
    context?: ErrorContext
  ): PrologError {
    const code = error.code || 'UNKNOWN_ERROR';
    const message = error.message || 'Unknown error occurred';

    const additionalInfo: Partial<PrologError> = {};
    if (error.details !== undefined) {
      additionalInfo.details = error.details;
    }
    if (error.line !== undefined) {
      additionalInfo.line = error.line;
    }
    if (error.column !== undefined) {
      additionalInfo.column = error.column;
    }
    if (error.file !== undefined) {
      additionalInfo.file = error.file;
    }

    return this.createError(code, message, context, additionalInfo);
  }

  /**
   * Create a standardized error object
   */
  private createError(
    code: string,
    message: string,
    context?: ErrorContext,
    additional?: Partial<PrologError>
  ): PrologError {
    const template = this.errorCodes.get(code) || {};

    const result: PrologError = {
      code,
      type: template.type || 'runtime',
      message: message || template.message || 'Unknown error',
      severity: template.severity || 'error',
    };

    // Only add optional properties if they have values
    const detailsValue = additional?.details || template.details;
    if (detailsValue) {
      result.details = detailsValue;
    }
    if (template.suggestion) {
      result.suggestion = template.suggestion;
    }
    if (template.documentation) {
      result.documentation = template.documentation;
    }
    if (additional?.line) {
      result.line = additional.line;
    }
    if (additional?.column) {
      result.column = additional.column;
    }
    const fileValue = additional?.file || context?.file;
    if (fileValue) {
      result.file = fileValue;
    }
    const contextValue = context?.command || context?.operation;
    if (contextValue) {
      result.context = contextValue;
    }

    return result;
  }

  /**
   * Create a generic error for unknown error types
   */
  private createGenericError(error: unknown, context?: ErrorContext): PrologError {
    const result: PrologError = {
      code: 'UNKNOWN_ERROR',
      type: 'runtime',
      message: typeof error === 'string' ? error : JSON.stringify(error),
      suggestion:
        'This is an unexpected error. Please check the VS Code output panel for more details.',
      severity: 'error',
    };

    // Only add context if it has a value
    const contextValue = context?.command || context?.operation;
    if (contextValue) {
      result.context = contextValue;
    }

    return result;
  }

  /**
   * Format error for display in chat
   */
  formatError(error: PrologError): string {
    let output = '';

    // Error header with icon
    const icon = this.getErrorIcon(error.type);
    output += `${icon} **${error.type.charAt(0).toUpperCase() + error.type.slice(1)} Error**\n\n`;

    // Main error message
    output += `**Message:** ${error.message}\n\n`;

    // Location information
    if (error.file || error.line) {
      output += '**Location:**\n';
      if (error.file) {
        output += `- File: \`${error.file}\`\n`;
      }
      if (error.line) {
        output += `- Line: ${error.line}`;
        if (error.column) {
          output += `, Column: ${error.column}`;
        }
        output += '\n';
      }
      output += '\n';
    }

    // Context information
    if (error.context) {
      output += `**Context:** ${error.context}\n\n`;
    }

    // Additional details
    if (error.details) {
      output += '**Details:**\n```\n';
      output += error.details;
      output += '\n```\n\n';
    }

    // Suggestion
    if (error.suggestion) {
      output += `💡 **Suggestion:** ${error.suggestion}\n\n`;
    }

    // Documentation link
    if (error.documentation) {
      output += `📚 **Documentation:** ${error.documentation}\n\n`;
    }

    return output;
  }

  /**
   * Get appropriate icon for error type
   */
  private getErrorIcon(type: string): string {
    const icons: Record<string, string> = {
      syntax: '📝',
      runtime: '⚡',
      system: '🔧',
      network: '🌐',
      permission: '🔒',
      resource: '💾',
      user: '👤',
    };

    return icons[type] || '❌';
  }

  /**
   * Log error for debugging
   */
  private logError(error: PrologError, context?: ErrorContext): void {
    const logMessage = `[${error.code}] ${error.message}`;

    if (error.severity === 'error') {
      console.error(logMessage, { error, context });
    } else if (error.severity === 'warning') {
      console.warn(logMessage, { error, context });
    } else {
      console.info(logMessage, { error, context });
    }
  }

  /**
   * Show error notification to user
   */
  showErrorNotification(error: PrologError): void {
    const message = `${error.type.charAt(0).toUpperCase() + error.type.slice(1)} Error: ${error.message}`;

    if (error.severity === 'error') {
      window.showErrorMessage(message);
    } else if (error.severity === 'warning') {
      window.showWarningMessage(message);
    } else {
      window.showInformationMessage(message);
    }
  }

  /**
   * Add or update error code definition
   */
  addErrorCode(code: string, definition: Partial<PrologError>): void {
    this.errorCodes.set(code, definition);
  }

  /**
   * Get all registered error codes
   */
  getErrorCodes(): Map<string, Partial<PrologError>> {
    return new Map(this.errorCodes);
  }

  /**
   * Set locale for error messages
   */
  setLocale(locale: string): void {
    this.locale = locale;
    // TODO: Load localized error messages
  }

  /**
   * Create user-friendly error message for common mistakes
   */
  createUserFriendlyError(userInput: string, originalError: unknown): PrologError {
    const input = userInput.toLowerCase().trim();

    // Common beginner mistakes
    if (input.includes('?-') && !input.endsWith('.')) {
      return this.createError('MISSING_PERIOD', 'Query should end with a period', { userInput });
    }

    if (input.includes(':-') && !input.endsWith('.')) {
      return this.createError('MISSING_PERIOD', 'Rule should end with a period', { userInput });
    }

    if (input.match(/\([^)]*$/)) {
      return this.createError('UNMATCHED_PARENTHESES', 'Missing closing parenthesis', {
        userInput,
      });
    }

    if (input.match(/^[^(]*\)$/)) {
      return this.createError('UNMATCHED_PARENTHESES', 'Missing opening parenthesis', {
        userInput,
      });
    }

    // Fallback to original error processing
    return this.handleError(originalError, { userInput });
  }
}

/**
 * Default error handler instance
 */
export const defaultErrorHandler = new ErrorHandler();

/**
 * Create error handler with specific locale
 */
export function createErrorHandler(locale: string): ErrorHandler {
  return new ErrorHandler(locale);
}
