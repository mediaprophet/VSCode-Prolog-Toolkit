import { expect } from 'chai';
import { describe, it, beforeEach } from 'mocha';
import { ErrorHandler, PrologError, ErrorContext } from '../src/features/errorHandler';

describe('ErrorHandler', () => {
  let errorHandler: ErrorHandler;

  beforeEach(() => {
    errorHandler = new ErrorHandler();
  });

  describe('handleError', () => {
    it('should handle string errors', () => {
      const error = 'syntax error: unexpected token';
      const result = errorHandler.handleError(error);
      
      expect(result.code).to.equal('SYNTAX_ERROR');
      expect(result.type).to.equal('syntax');
      expect(result.message).to.include('syntax error');
      expect(result.severity).to.equal('error');
    });

    it('should handle Error objects', () => {
      const error = new SyntaxError('Invalid syntax');
      const result = errorHandler.handleError(error);
      
      expect(result.code).to.equal('SYNTAX_ERROR');
      expect(result.type).to.equal('syntax');
      expect(result.message).to.include('Invalid syntax');
    });

    it('should handle Prolog error terms', () => {
      const error = {
        functor: 'error',
        args: [{
          functor: 'existence_error',
          args: ['procedure', 'unknown_predicate/1']
        }]
      };
      
      const result = errorHandler.handleError(error);
      expect(result.code).to.equal('EXISTENCE_ERROR');
      expect(result.type).to.equal('runtime');
    });

    it('should handle HTTP errors', () => {
      const error = {
        code: 'ECONNREFUSED',
        message: 'Connection refused'
      };
      
      const result = errorHandler.handleError(error);
      expect(result.code).to.equal('CONNECTION_FAILED');
      expect(result.type).to.equal('network');
    });

    it('should include context information', () => {
      const error = 'test error';
      const context: ErrorContext = {
        command: 'query',
        query: 'test_predicate(X)',
        file: 'test.pl'
      };
      
      const result = errorHandler.handleError(error, context);
      expect(result.context).to.equal('query');
      expect(result.file).to.equal('test.pl');
    });
  });

  describe('parseStringError', () => {
    it('should identify syntax errors', () => {
      const error = 'Syntax error: operator expected';
      const result = errorHandler.handleError(error);
      
      expect(result.code).to.equal('SYNTAX_ERROR');
      expect(result.suggestion).to.include('Check your Prolog syntax');
    });

    it('should identify existence errors', () => {
      const error = 'existence_error(procedure, test/1)';
      const result = errorHandler.handleError(error);
      
      expect(result.code).to.equal('EXISTENCE_ERROR');
      expect(result.suggestion).to.include('Check if the predicate is defined');
    });

    it('should identify type errors', () => {
      const error = 'type_error(integer, atom)';
      const result = errorHandler.handleError(error);
      
      expect(result.code).to.equal('TYPE_ERROR');
      expect(result.suggestion).to.include('Check the types of your arguments');
    });

    it('should identify permission errors', () => {
      const error = 'permission_error(modify, static_procedure, test/1)';
      const result = errorHandler.handleError(error);
      
      expect(result.code).to.equal('PERMISSION_ERROR');
      expect(result.suggestion).to.include('permission');
    });

    it('should identify timeout errors', () => {
      const error = 'Query timeout after 30 seconds';
      const result = errorHandler.handleError(error);
      
      expect(result.code).to.equal('BACKEND_TIMEOUT');
      expect(result.suggestion).to.include('time limit');
    });

    it('should identify connection errors', () => {
      const error = 'Connection refused to localhost:3060';
      const result = errorHandler.handleError(error);
      
      expect(result.code).to.equal('CONNECTION_FAILED');
      expect(result.suggestion).to.include('SWI-Prolog is running');
    });
  });

  describe('parsePrologErrorTerm', () => {
    it('should parse formal error terms', () => {
      const errorTerm = {
        functor: 'error',
        args: [{
          functor: 'type_error',
          args: ['integer', 'hello']
        }]
      };
      
      const result = errorHandler.handleError(errorTerm);
      expect(result.code).to.equal('TYPE_ERROR');
      expect(result.message).to.include('integer');
      expect(result.message).to.include('hello');
    });

    it('should parse syntax error terms', () => {
      const errorTerm = {
        functor: 'syntax_error',
        args: ['operator_expected']
      };
      
      const result = errorHandler.handleError(errorTerm);
      expect(result.code).to.equal('SYNTAX_ERROR');
      expect(result.message).to.include('operator_expected');
    });

    it('should parse existence error terms', () => {
      const errorTerm = {
        functor: 'existence_error',
        args: ['procedure', 'unknown/1']
      };
      
      const result = errorHandler.handleError(errorTerm);
      expect(result.code).to.equal('EXISTENCE_ERROR');
      expect(result.message).to.include("procedure 'unknown/1' does not exist");
    });
  });

  describe('formatError', () => {
    it('should format error with all information', () => {
      const error: PrologError = {
        code: 'SYNTAX_ERROR',
        type: 'syntax',
        message: 'Missing period at end of clause',
        details: 'Line 5: parent(tom, bob)',
        suggestion: 'Add a period (.) at the end of your clause',
        documentation: 'https://example.com/syntax',
        line: 5,
        column: 18,
        file: 'test.pl',
        context: 'consult',
        severity: 'error'
      };
      
      const formatted = errorHandler.formatError(error);
      
      expect(formatted).to.include('📝 **Syntax Error**');
      expect(formatted).to.include('**Message:** Missing period at end of clause');
      expect(formatted).to.include('**Location:**');
      expect(formatted).to.include('- File: `test.pl`');
      expect(formatted).to.include('- Line: 5, Column: 18');
      expect(formatted).to.include('**Context:** consult');
      expect(formatted).to.include('**Details:**');
      expect(formatted).to.include('Line 5: parent(tom, bob)');
      expect(formatted).to.include('💡 **Suggestion:**');
      expect(formatted).to.include('📚 **Documentation:**');
    });

    it('should format minimal error', () => {
      const error: PrologError = {
        code: 'UNKNOWN_ERROR',
        type: 'runtime',
        message: 'Something went wrong',
        severity: 'error'
      };
      
      const formatted = errorHandler.formatError(error);
      
      expect(formatted).to.include('⚡ **Runtime Error**');
      expect(formatted).to.include('**Message:** Something went wrong');
      expect(formatted).to.not.include('**Location:**');
      expect(formatted).to.not.include('**Details:**');
    });

    it('should use appropriate icons for error types', () => {
      const syntaxError: PrologError = {
        code: 'SYNTAX_ERROR',
        type: 'syntax',
        message: 'Syntax error',
        severity: 'error'
      };
      
      const networkError: PrologError = {
        code: 'CONNECTION_FAILED',
        type: 'network',
        message: 'Network error',
        severity: 'error'
      };
      
      expect(errorHandler.formatError(syntaxError)).to.include('📝');
      expect(errorHandler.formatError(networkError)).to.include('🌐');
    });
  });

  describe('createUserFriendlyError', () => {
    it('should detect missing period in query', () => {
      const userInput = '?- parent(X, Y)';
      const originalError = 'syntax error';
      
      const result = errorHandler.createUserFriendlyError(userInput, originalError);
      
      expect(result.code).to.equal('MISSING_PERIOD');
      expect(result.message).to.include('Query should end with a period');
    });

    it('should detect missing period in rule', () => {
      const userInput = 'grandparent(X, Z) :- parent(X, Y), parent(Y, Z)';
      const originalError = 'syntax error';
      
      const result = errorHandler.createUserFriendlyError(userInput, originalError);
      
      expect(result.code).to.equal('MISSING_PERIOD');
      expect(result.message).to.include('Rule should end with a period');
    });

    it('should detect unmatched opening parenthesis', () => {
      const userInput = 'parent(tom, bob';
      const originalError = 'syntax error';
      
      const result = errorHandler.createUserFriendlyError(userInput, originalError);
      
      expect(result.code).to.equal('UNMATCHED_PARENTHESES');
      expect(result.message).to.include('Missing closing parenthesis');
    });

    it('should detect unmatched closing parenthesis', () => {
      const userInput = 'parent tom, bob)';
      const originalError = 'syntax error';
      
      const result = errorHandler.createUserFriendlyError(userInput, originalError);
      
      expect(result.code).to.equal('UNMATCHED_PARENTHESES');
      expect(result.message).to.include('Missing opening parenthesis');
    });

    it('should fallback to original error processing', () => {
      const userInput = 'valid_syntax(X).';
      const originalError = 'existence_error(procedure, unknown/1)';
      
      const result = errorHandler.createUserFriendlyError(userInput, originalError);
      
      expect(result.code).to.equal('EXISTENCE_ERROR');
    });
  });

  describe('error code management', () => {
    it('should add custom error codes', () => {
      errorHandler.addErrorCode('CUSTOM_ERROR', {
        type: 'user',
        message: 'Custom error occurred',
        suggestion: 'Try something else',
        severity: 'warning'
      });
      
      const error = errorHandler.handleError('custom error');
      // Should create generic error since string doesn't match pattern
      expect(error.code).to.not.equal('CUSTOM_ERROR');
      
      // But the error code should be available
      const errorCodes = errorHandler.getErrorCodes();
      expect(errorCodes.has('CUSTOM_ERROR')).to.be.true;
    });

    it('should get all error codes', () => {
      const errorCodes = errorHandler.getErrorCodes();
      
      expect(errorCodes.has('SYNTAX_ERROR')).to.be.true;
      expect(errorCodes.has('EXISTENCE_ERROR')).to.be.true;
      expect(errorCodes.has('TYPE_ERROR')).to.be.true;
      expect(errorCodes.has('BACKEND_NOT_AVAILABLE')).to.be.true;
    });
  });

  describe('HTTP error handling', () => {
    it('should handle connection refused', () => {
      const error = { code: 'ECONNREFUSED' };
      const result = errorHandler.handleError(error);
      
      expect(result.code).to.equal('CONNECTION_FAILED');
      expect(result.message).to.include('Connection refused');
    });

    it('should handle timeout', () => {
      const error = { code: 'ETIMEDOUT' };
      const result = errorHandler.handleError(error);
      
      expect(result.code).to.equal('BACKEND_TIMEOUT');
      expect(result.message).to.include('timed out');
    });

    it('should handle HTTP response errors', () => {
      const error = {
        response: {
          status: 404,
          statusText: 'Not Found'
        }
      };
      
      const result = errorHandler.handleError(error);
      
      expect(result.code).to.equal('HTTP_ERROR');
      expect(result.message).to.include('HTTP 404: Not Found');
    });

    it('should handle HTTP request errors', () => {
      const error = {
        request: {},
        message: 'Network error'
      };
      
      const result = errorHandler.handleError(error);
      
      expect(result.code).to.equal('HTTP_ERROR');
      expect(result.message).to.include('Network error');
    });
  });

  describe('structured error handling', () => {
    it('should handle structured errors with all fields', () => {
      const error = {
        code: 'CUSTOM_STRUCTURED',
        message: 'Structured error message',
        details: 'Additional details',
        line: 10,
        column: 5,
        file: 'structured.pl'
      };
      
      const result = errorHandler.handleError(error);
      
      expect(result.code).to.equal('CUSTOM_STRUCTURED');
      expect(result.message).to.equal('Structured error message');
      expect(result.details).to.equal('Additional details');
      expect(result.line).to.equal(10);
      expect(result.column).to.equal(5);
      expect(result.file).to.equal('structured.pl');
    });
  });

  describe('edge cases', () => {
    it('should handle null error', () => {
      const result = errorHandler.handleError(null);
      
      expect(result.code).to.equal('UNKNOWN_ERROR');
      expect(result.type).to.equal('runtime');
      expect(result.severity).to.equal('error');
    });

    it('should handle undefined error', () => {
      const result = errorHandler.handleError(undefined);
      
      expect(result.code).to.equal('UNKNOWN_ERROR');
      expect(result.message).to.include('undefined');
    });

    it('should handle empty string error', () => {
      const result = errorHandler.handleError('');
      
      expect(result.code).to.equal('UNKNOWN_ERROR');
      expect(result.message).to.equal('');
    });

    it('should handle complex object error', () => {
      const complexError = {
        nested: {
          deep: {
            value: 'error'
          }
        },
        array: [1, 2, 3]
      };
      
      const result = errorHandler.handleError(complexError);
      
      expect(result.code).to.equal('UNKNOWN_ERROR');
      expect(result.message).to.include('nested');
    });
  });

  describe('localization support', () => {
    it('should set locale', () => {
      errorHandler.setLocale('es');
      // Note: This test would need actual Spanish translations to be meaningful
      // For now, just verify the method doesn't throw
      expect(() => errorHandler.setLocale('es')).to.not.throw();
    });

    it('should handle invalid locale gracefully', () => {
      errorHandler.setLocale('invalid');
      // Should not throw and should continue working
      const error = errorHandler.handleError('test error');
      expect(error).to.be.an('object');
    });
  });
});