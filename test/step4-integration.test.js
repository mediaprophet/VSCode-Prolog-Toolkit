"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g = Object.create((typeof Iterator === "function" ? Iterator : Object).prototype);
    return g.next = verb(0), g["throw"] = verb(1), g["return"] = verb(2), typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (g && (g = 0, op[0] && (_ = 0)), _) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
Object.defineProperty(exports, "__esModule", { value: true });
var chai_1 = require("chai");
var mocha_1 = require("mocha");
// Simple integration test for Step 4 components
(0, mocha_1.describe)('Step 4: Output Formatting and Error Handling Integration', function () {
    (0, mocha_1.it)('should be able to import OutputFormatter', function () { return __awaiter(void 0, void 0, void 0, function () {
        var OutputFormatter, formatter;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0: return [4 /*yield*/, Promise.resolve().then(function () { return require('../src/features/outputFormatter'); })];
                case 1:
                    OutputFormatter = (_a.sent()).OutputFormatter;
                    (0, chai_1.expect)(OutputFormatter).to.be.a('function');
                    formatter = new OutputFormatter();
                    (0, chai_1.expect)(formatter).to.be.an('object');
                    (0, chai_1.expect)(formatter.formatQueryResult).to.be.a('function');
                    return [2 /*return*/];
            }
        });
    }); });
    (0, mocha_1.it)('should be able to import ErrorHandler', function () { return __awaiter(void 0, void 0, void 0, function () {
        var ErrorHandler, errorHandler;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0: return [4 /*yield*/, Promise.resolve().then(function () { return require('../src/features/errorHandler'); })];
                case 1:
                    ErrorHandler = (_a.sent()).ErrorHandler;
                    (0, chai_1.expect)(ErrorHandler).to.be.a('function');
                    errorHandler = new ErrorHandler();
                    (0, chai_1.expect)(errorHandler).to.be.an('object');
                    (0, chai_1.expect)(errorHandler.handleError).to.be.a('function');
                    return [2 /*return*/];
            }
        });
    }); });
    (0, mocha_1.it)('should format a simple success result', function () { return __awaiter(void 0, void 0, void 0, function () {
        var OutputFormatter, formatter, result, formatted;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0: return [4 /*yield*/, Promise.resolve().then(function () { return require('../src/features/outputFormatter'); })];
                case 1:
                    OutputFormatter = (_a.sent()).OutputFormatter;
                    formatter = new OutputFormatter();
                    result = {
                        type: 'success',
                        bindings: [{ X: 'hello', Y: 42 }],
                    };
                    formatted = formatter.formatQueryResult(result);
                    (0, chai_1.expect)(formatted).to.include('✅');
                    (0, chai_1.expect)(formatted).to.include('Query succeeded');
                    (0, chai_1.expect)(formatted).to.include('X = hello');
                    (0, chai_1.expect)(formatted).to.include('Y = 42');
                    return [2 /*return*/];
            }
        });
    }); });
    (0, mocha_1.it)('should handle a simple error', function () { return __awaiter(void 0, void 0, void 0, function () {
        var ErrorHandler, errorHandler, error, result;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0: return [4 /*yield*/, Promise.resolve().then(function () { return require('../src/features/errorHandler'); })];
                case 1:
                    ErrorHandler = (_a.sent()).ErrorHandler;
                    errorHandler = new ErrorHandler();
                    error = 'syntax error: missing period';
                    result = errorHandler.handleError(error);
                    (0, chai_1.expect)(result.code).to.equal('SYNTAX_ERROR');
                    (0, chai_1.expect)(result.type).to.equal('syntax');
                    (0, chai_1.expect)(result.message).to.include('syntax error');
                    (0, chai_1.expect)(result.suggestion).to.include('Check your Prolog syntax');
                    return [2 /*return*/];
            }
        });
    }); });
    (0, mocha_1.it)('should format error for display', function () { return __awaiter(void 0, void 0, void 0, function () {
        var ErrorHandler, errorHandler, error, formatted;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0: return [4 /*yield*/, Promise.resolve().then(function () { return require('../src/features/errorHandler'); })];
                case 1:
                    ErrorHandler = (_a.sent()).ErrorHandler;
                    errorHandler = new ErrorHandler();
                    error = errorHandler.handleError('type_error(integer, atom)');
                    formatted = errorHandler.formatError(error);
                    (0, chai_1.expect)(formatted).to.include('⚡ **Runtime Error**');
                    (0, chai_1.expect)(formatted).to.include('**Message:**');
                    (0, chai_1.expect)(formatted).to.include('💡 **Suggestion:**');
                    return [2 /*return*/];
            }
        });
    }); });
    (0, mocha_1.it)('should handle streaming output', function () { return __awaiter(void 0, void 0, void 0, function () {
        var StreamingHandler, streamer;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0: return [4 /*yield*/, Promise.resolve().then(function () { return require('../src/features/streamingHandler'); })];
                case 1:
                    StreamingHandler = (_a.sent()).StreamingHandler;
                    streamer = new StreamingHandler({ chunkSize: 2, maxTotalResults: 10 });
                    (0, chai_1.expect)(streamer).to.be.an('object');
                    (0, chai_1.expect)(streamer.startStreaming).to.be.a('function');
                    (0, chai_1.expect)(streamer.streaming).to.be.false;
                    return [2 /*return*/];
            }
        });
    }); });
    (0, mocha_1.it)('should support localization', function () { return __awaiter(void 0, void 0, void 0, function () {
        var LocalizationManager, locManager, errorString, queryString;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0: return [4 /*yield*/, Promise.resolve().then(function () { return require('../src/features/localization'); })];
                case 1:
                    LocalizationManager = (_a.sent()).LocalizationManager;
                    locManager = new LocalizationManager('/fake/path');
                    (0, chai_1.expect)(locManager).to.be.an('object');
                    (0, chai_1.expect)(locManager.getString).to.be.a('function');
                    errorString = locManager.getString('errors.syntaxError');
                    (0, chai_1.expect)(errorString).to.include('Syntax error');
                    queryString = locManager.getString('query.succeeded');
                    (0, chai_1.expect)(queryString).to.include('Query succeeded');
                    return [2 /*return*/];
            }
        });
    }); });
    (0, mocha_1.it)('should integrate formatter with error handler', function () { return __awaiter(void 0, void 0, void 0, function () {
        var OutputFormatter, ErrorHandler, formatter, errorHandler, errorResult, formattedError, handledError;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0: return [4 /*yield*/, Promise.resolve().then(function () { return require('../src/features/outputFormatter'); })];
                case 1:
                    OutputFormatter = (_a.sent()).OutputFormatter;
                    return [4 /*yield*/, Promise.resolve().then(function () { return require('../src/features/errorHandler'); })];
                case 2:
                    ErrorHandler = (_a.sent()).ErrorHandler;
                    formatter = new OutputFormatter();
                    errorHandler = new ErrorHandler();
                    errorResult = {
                        type: 'error',
                        error: 'existence_error(procedure, unknown/1)',
                        message: 'Predicate not found',
                    };
                    formattedError = formatter.formatQueryResult(errorResult);
                    (0, chai_1.expect)(formattedError).to.include('🚫 **Query error**');
                    (0, chai_1.expect)(formattedError).to.include('existence_error');
                    handledError = errorHandler.handleError('existence_error(procedure, unknown/1)');
                    (0, chai_1.expect)(handledError.code).to.equal('EXISTENCE_ERROR');
                    (0, chai_1.expect)(handledError.suggestion).to.include('Check if the predicate is defined');
                    return [2 /*return*/];
            }
        });
    }); });
    (0, mocha_1.it)('should handle complex variable bindings', function () { return __awaiter(void 0, void 0, void 0, function () {
        var OutputFormatter, formatter, result, formatted;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0: return [4 /*yield*/, Promise.resolve().then(function () { return require('../src/features/outputFormatter'); })];
                case 1:
                    OutputFormatter = (_a.sent()).OutputFormatter;
                    formatter = new OutputFormatter();
                    result = {
                        type: 'multiple',
                        bindings: [
                            { Name: 'john', Age: 25, Hobbies: ['reading', 'coding'] },
                            { Name: 'jane', Age: 30, Hobbies: ['music', 'art'] },
                            { Name: 'bob', Age: 35, Hobbies: ['sports'] },
                        ],
                    };
                    formatted = formatter.formatQueryResult(result);
                    (0, chai_1.expect)(formatted).to.include('3 solutions');
                    (0, chai_1.expect)(formatted).to.include('**Solution 1:**');
                    (0, chai_1.expect)(formatted).to.include('Name = john');
                    (0, chai_1.expect)(formatted).to.include('Hobbies = [reading, coding]');
                    return [2 /*return*/];
            }
        });
    }); });
    (0, mocha_1.it)('should truncate very long output', function () { return __awaiter(void 0, void 0, void 0, function () {
        var OutputFormatter, formatter, longOutput, truncated;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0: return [4 /*yield*/, Promise.resolve().then(function () { return require('../src/features/outputFormatter'); })];
                case 1:
                    OutputFormatter = (_a.sent()).OutputFormatter;
                    formatter = new OutputFormatter();
                    longOutput = 'A'.repeat(3000);
                    truncated = formatter.truncateOutput(longOutput, 1000);
                    (0, chai_1.expect)(truncated.length).to.be.lessThan(longOutput.length);
                    (0, chai_1.expect)(truncated).to.include('... (output truncated)');
                    return [2 /*return*/];
            }
        });
    }); });
    (0, mocha_1.it)('should provide user-friendly error messages', function () { return __awaiter(void 0, void 0, void 0, function () {
        var ErrorHandler, errorHandler, missingPeriod, unmatchedParen;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0: return [4 /*yield*/, Promise.resolve().then(function () { return require('../src/features/errorHandler'); })];
                case 1:
                    ErrorHandler = (_a.sent()).ErrorHandler;
                    errorHandler = new ErrorHandler();
                    missingPeriod = errorHandler.createUserFriendlyError('?- parent(X, Y)', 'syntax error');
                    (0, chai_1.expect)(missingPeriod.code).to.equal('MISSING_PERIOD');
                    (0, chai_1.expect)(missingPeriod.message).to.include('Query should end with a period');
                    unmatchedParen = errorHandler.createUserFriendlyError('parent(tom, bob', 'syntax error');
                    (0, chai_1.expect)(unmatchedParen.code).to.equal('UNMATCHED_PARENTHESES');
                    (0, chai_1.expect)(unmatchedParen.message).to.include('Missing closing parenthesis');
                    return [2 /*return*/];
            }
        });
    }); });
});
