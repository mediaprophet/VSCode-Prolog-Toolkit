"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var chai_1 = require("chai");
var mocha_1 = require("mocha");
var errorHandler_js_1 = require("../src/features/errorHandler.js");
(0, mocha_1.describe)('ErrorHandler', function () {
    var errorHandler;
    (0, mocha_1.beforeEach)(function () {
        errorHandler = new errorHandler_js_1.ErrorHandler();
    });
    (0, mocha_1.describe)('handleError', function () {
        (0, mocha_1.it)('should handle string errors', function () {
            var error = 'syntax error: unexpected token';
            var result = errorHandler.handleError(error);
            (0, chai_1.expect)(result.code).to.equal('SYNTAX_ERROR');
            (0, chai_1.expect)(result.type).to.equal('syntax');
            (0, chai_1.expect)(result.message).to.include('syntax error');
            (0, chai_1.expect)(result.severity).to.equal('error');
        });
        (0, mocha_1.it)('should handle Error objects', function () {
            var error = new SyntaxError('Invalid syntax');
            var result = errorHandler.handleError(error);
            (0, chai_1.expect)(result.code).to.equal('SYNTAX_ERROR');
            (0, chai_1.expect)(result.type).to.equal('syntax');
            (0, chai_1.expect)(result.message).to.include('Invalid syntax');
        });
        (0, mocha_1.it)('should handle Prolog error terms', function () {
            var error = {
                functor: 'error',
                args: [
                    {
                        functor: 'existence_error',
                        args: ['procedure', 'unknown_predicate/1'],
                    },
                ],
            };
            var result = errorHandler.handleError(error);
            (0, chai_1.expect)(result.code).to.equal('EXISTENCE_ERROR');
            (0, chai_1.expect)(result.type).to.equal('runtime');
        });
        (0, mocha_1.it)('should handle HTTP errors', function () {
            var error = {
                code: 'ECONNREFUSED',
                message: 'Connection refused',
            };
            var result = errorHandler.handleError(error);
            (0, chai_1.expect)(result.code).to.equal('CONNECTION_FAILED');
            (0, chai_1.expect)(result.type).to.equal('network');
        });
        (0, mocha_1.it)('should include context information', function () {
            var error = 'test error';
            var context = {
                command: 'query',
                query: 'test_predicate(X)',
                file: 'test.pl',
            };
            var result = errorHandler.handleError(error, context);
            (0, chai_1.expect)(result.context).to.equal('query');
            (0, chai_1.expect)(result.file).to.equal('test.pl');
        });
    });
    (0, mocha_1.describe)('parseStringError', function () {
        (0, mocha_1.it)('should identify syntax errors', function () {
            var error = 'Syntax error: operator expected';
            var result = errorHandler.handleError(error);
            (0, chai_1.expect)(result.code).to.equal('SYNTAX_ERROR');
            (0, chai_1.expect)(result.suggestion).to.include('Check your Prolog syntax');
        });
        (0, mocha_1.it)('should identify existence errors', function () {
            var error = 'existence_error(procedure, test/1)';
            var result = errorHandler.handleError(error);
            (0, chai_1.expect)(result.code).to.equal('EXISTENCE_ERROR');
            (0, chai_1.expect)(result.suggestion).to.include('Check if the predicate is defined');
        });
        (0, mocha_1.it)('should identify type errors', function () {
            var error = 'type_error(integer, atom)';
            var result = errorHandler.handleError(error);
            (0, chai_1.expect)(result.code).to.equal('TYPE_ERROR');
            (0, chai_1.expect)(result.suggestion).to.include('Check the types of your arguments');
        });
        (0, mocha_1.it)('should identify permission errors', function () {
            var error = 'permission_error(modify, static_procedure, test/1)';
            var result = errorHandler.handleError(error);
            (0, chai_1.expect)(result.code).to.equal('PERMISSION_ERROR');
            (0, chai_1.expect)(result.suggestion).to.include('permission');
        });
        (0, mocha_1.it)('should identify timeout errors', function () {
            var error = 'Query timeout after 30 seconds';
            var result = errorHandler.handleError(error);
            (0, chai_1.expect)(result.code).to.equal('BACKEND_TIMEOUT');
            (0, chai_1.expect)(result.suggestion).to.include('time limit');
        });
        (0, mocha_1.it)('should identify connection errors', function () {
            var error = 'Connection refused to localhost:3060';
            var result = errorHandler.handleError(error);
            (0, chai_1.expect)(result.code).to.equal('CONNECTION_FAILED');
            (0, chai_1.expect)(result.suggestion).to.include('SWI-Prolog is running');
        });
    });
    (0, mocha_1.describe)('parsePrologErrorTerm', function () {
        (0, mocha_1.it)('should parse formal error terms', function () {
            var errorTerm = {
                functor: 'error',
                args: [
                    {
                        functor: 'type_error',
                        args: ['integer', 'hello'],
                    },
                ],
            };
            var result = errorHandler.handleError(errorTerm);
            (0, chai_1.expect)(result.code).to.equal('TYPE_ERROR');
            (0, chai_1.expect)(result.message).to.include('integer');
            (0, chai_1.expect)(result.message).to.include('hello');
        });
        (0, mocha_1.it)('should parse syntax error terms', function () {
            var errorTerm = {
                functor: 'syntax_error',
                args: ['operator_expected'],
            };
            var result = errorHandler.handleError(errorTerm);
            (0, chai_1.expect)(result.code).to.equal('SYNTAX_ERROR');
            (0, chai_1.expect)(result.message).to.include('operator_expected');
        });
        (0, mocha_1.it)('should parse existence error terms', function () {
            var errorTerm = {
                functor: 'existence_error',
                args: ['procedure', 'unknown/1'],
            };
            var result = errorHandler.handleError(errorTerm);
            (0, chai_1.expect)(result.code).to.equal('EXISTENCE_ERROR');
            (0, chai_1.expect)(result.message).to.include("procedure 'unknown/1' does not exist");
        });
    });
    (0, mocha_1.describe)('formatError', function () {
        (0, mocha_1.it)('should format error with all information', function () {
            var error = {
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
                severity: 'error',
            };
            var formatted = errorHandler.formatError(error);
            (0, chai_1.expect)(formatted).to.include('📝 **Syntax Error**');
            (0, chai_1.expect)(formatted).to.include('**Message:** Missing period at end of clause');
            (0, chai_1.expect)(formatted).to.include('**Location:**');
            (0, chai_1.expect)(formatted).to.include('- File: `test.pl`');
            (0, chai_1.expect)(formatted).to.include('- Line: 5, Column: 18');
            (0, chai_1.expect)(formatted).to.include('**Context:** consult');
            (0, chai_1.expect)(formatted).to.include('**Details:**');
            (0, chai_1.expect)(formatted).to.include('Line 5: parent(tom, bob)');
            (0, chai_1.expect)(formatted).to.include('💡 **Suggestion:**');
            (0, chai_1.expect)(formatted).to.include('📚 **Documentation:**');
        });
        (0, mocha_1.it)('should format minimal error', function () {
            var error = {
                code: 'UNKNOWN_ERROR',
                type: 'runtime',
                message: 'Something went wrong',
                severity: 'error',
            };
            var formatted = errorHandler.formatError(error);
            (0, chai_1.expect)(formatted).to.include('⚡ **Runtime Error**');
            (0, chai_1.expect)(formatted).to.include('**Message:** Something went wrong');
            (0, chai_1.expect)(formatted).to.not.include('**Location:**');
            (0, chai_1.expect)(formatted).to.not.include('**Details:**');
        });
        (0, mocha_1.it)('should use appropriate icons for error types', function () {
            var syntaxError = {
                code: 'SYNTAX_ERROR',
                type: 'syntax',
                message: 'Syntax error',
                severity: 'error',
            };
            var networkError = {
                code: 'CONNECTION_FAILED',
                type: 'network',
                message: 'Network error',
                severity: 'error',
            };
            (0, chai_1.expect)(errorHandler.formatError(syntaxError)).to.include('📝');
            (0, chai_1.expect)(errorHandler.formatError(networkError)).to.include('🌐');
        });
    });
    (0, mocha_1.describe)('createUserFriendlyError', function () {
        (0, mocha_1.it)('should detect missing period in query', function () {
            var userInput = '?- parent(X, Y)';
            var originalError = 'syntax error';
            var result = errorHandler.createUserFriendlyError(userInput, originalError);
            (0, chai_1.expect)(result.code).to.equal('MISSING_PERIOD');
            (0, chai_1.expect)(result.message).to.include('Query should end with a period');
        });
        (0, mocha_1.it)('should detect missing period in rule', function () {
            var userInput = 'grandparent(X, Z) :- parent(X, Y), parent(Y, Z)';
            var originalError = 'syntax error';
            var result = errorHandler.createUserFriendlyError(userInput, originalError);
            (0, chai_1.expect)(result.code).to.equal('MISSING_PERIOD');
            (0, chai_1.expect)(result.message).to.include('Rule should end with a period');
        });
        (0, mocha_1.it)('should detect unmatched opening parenthesis', function () {
            var userInput = 'parent(tom, bob';
            var originalError = 'syntax error';
            var result = errorHandler.createUserFriendlyError(userInput, originalError);
            (0, chai_1.expect)(result.code).to.equal('UNMATCHED_PARENTHESES');
            (0, chai_1.expect)(result.message).to.include('Missing closing parenthesis');
        });
        (0, mocha_1.it)('should detect unmatched closing parenthesis', function () {
            var userInput = 'parent tom, bob)';
            var originalError = 'syntax error';
            var result = errorHandler.createUserFriendlyError(userInput, originalError);
            (0, chai_1.expect)(result.code).to.equal('UNMATCHED_PARENTHESES');
            (0, chai_1.expect)(result.message).to.include('Missing opening parenthesis');
        });
        (0, mocha_1.it)('should fallback to original error processing', function () {
            var userInput = 'valid_syntax(X).';
            var originalError = 'existence_error(procedure, unknown/1)';
            var result = errorHandler.createUserFriendlyError(userInput, originalError);
            (0, chai_1.expect)(result.code).to.equal('EXISTENCE_ERROR');
        });
    });
    (0, mocha_1.describe)('error code management', function () {
        (0, mocha_1.it)('should add custom error codes', function () {
            errorHandler.addErrorCode('CUSTOM_ERROR', {
                type: 'user',
                message: 'Custom error occurred',
                suggestion: 'Try something else',
                severity: 'warning',
            });
            var error = errorHandler.handleError('custom error');
            // Should create generic error since string doesn't match pattern
            (0, chai_1.expect)(error.code).to.not.equal('CUSTOM_ERROR');
            // But the error code should be available
            var errorCodes = errorHandler.getErrorCodes();
            (0, chai_1.expect)(errorCodes.has('CUSTOM_ERROR')).to.be.true;
        });
        (0, mocha_1.it)('should get all error codes', function () {
            var errorCodes = errorHandler.getErrorCodes();
            (0, chai_1.expect)(errorCodes.has('SYNTAX_ERROR')).to.be.true;
            (0, chai_1.expect)(errorCodes.has('EXISTENCE_ERROR')).to.be.true;
            (0, chai_1.expect)(errorCodes.has('TYPE_ERROR')).to.be.true;
            (0, chai_1.expect)(errorCodes.has('BACKEND_NOT_AVAILABLE')).to.be.true;
        });
    });
    (0, mocha_1.describe)('HTTP error handling', function () {
        (0, mocha_1.it)('should handle connection refused', function () {
            var error = { code: 'ECONNREFUSED' };
            var result = errorHandler.handleError(error);
            (0, chai_1.expect)(result.code).to.equal('CONNECTION_FAILED');
            (0, chai_1.expect)(result.message).to.include('Connection refused');
        });
        (0, mocha_1.it)('should handle timeout', function () {
            var error = { code: 'ETIMEDOUT' };
            var result = errorHandler.handleError(error);
            (0, chai_1.expect)(result.code).to.equal('BACKEND_TIMEOUT');
            (0, chai_1.expect)(result.message).to.include('timed out');
        });
        (0, mocha_1.it)('should handle HTTP response errors', function () {
            var error = {
                response: {
                    status: 404,
                    statusText: 'Not Found',
                },
            };
            var result = errorHandler.handleError(error);
            (0, chai_1.expect)(result.code).to.equal('HTTP_ERROR');
            (0, chai_1.expect)(result.message).to.include('HTTP 404: Not Found');
        });
        (0, mocha_1.it)('should handle HTTP request errors', function () {
            var error = {
                request: {},
                message: 'Network error',
            };
            var result = errorHandler.handleError(error);
            (0, chai_1.expect)(result.code).to.equal('HTTP_ERROR');
            (0, chai_1.expect)(result.message).to.include('Network error');
        });
    });
    (0, mocha_1.describe)('structured error handling', function () {
        (0, mocha_1.it)('should handle structured errors with all fields', function () {
            var error = {
                code: 'CUSTOM_STRUCTURED',
                message: 'Structured error message',
                details: 'Additional details',
                line: 10,
                column: 5,
                file: 'structured.pl',
            };
            var result = errorHandler.handleError(error);
            (0, chai_1.expect)(result.code).to.equal('CUSTOM_STRUCTURED');
            (0, chai_1.expect)(result.message).to.equal('Structured error message');
            (0, chai_1.expect)(result.details).to.equal('Additional details');
            (0, chai_1.expect)(result.line).to.equal(10);
            (0, chai_1.expect)(result.column).to.equal(5);
            (0, chai_1.expect)(result.file).to.equal('structured.pl');
        });
    });
    (0, mocha_1.describe)('edge cases', function () {
        (0, mocha_1.it)('should handle null error', function () {
            var result = errorHandler.handleError(null);
            (0, chai_1.expect)(result.code).to.equal('UNKNOWN_ERROR');
            (0, chai_1.expect)(result.type).to.equal('runtime');
            (0, chai_1.expect)(result.severity).to.equal('error');
        });
        (0, mocha_1.it)('should handle undefined error', function () {
            var result = errorHandler.handleError(undefined);
            (0, chai_1.expect)(result.code).to.equal('UNKNOWN_ERROR');
            (0, chai_1.expect)(result.message).to.include('undefined');
        });
        (0, mocha_1.it)('should handle empty string error', function () {
            var result = errorHandler.handleError('');
            (0, chai_1.expect)(result.code).to.equal('UNKNOWN_ERROR');
            (0, chai_1.expect)(result.message).to.equal('');
        });
        (0, mocha_1.it)('should handle complex object error', function () {
            var complexError = {
                nested: {
                    deep: {
                        value: 'error',
                    },
                },
                array: [1, 2, 3],
            };
            var result = errorHandler.handleError(complexError);
            (0, chai_1.expect)(result.code).to.equal('UNKNOWN_ERROR');
            (0, chai_1.expect)(result.message).to.include('nested');
        });
    });
    (0, mocha_1.describe)('localization support', function () {
        (0, mocha_1.it)('should set locale', function () {
            errorHandler.setLocale('es');
            // Note: This test would need actual Spanish translations to be meaningful
            // For now, just verify the method doesn't throw
            (0, chai_1.expect)(function () { return errorHandler.setLocale('es'); }).to.not.throw();
        });
        (0, mocha_1.it)('should handle invalid locale gracefully', function () {
            errorHandler.setLocale('invalid');
            // Should not throw and should continue working
            var error = errorHandler.handleError('test error');
            (0, chai_1.expect)(error).to.be.an('object');
        });
    });
});
