#!/usr/bin/env node
"use strict";
var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
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
exports.PrologToolkitMCPServer = void 0;
var index_js_1 = require("@modelcontextprotocol/sdk/server/index.js");
var stdio_js_1 = require("@modelcontextprotocol/sdk/server/stdio.js");
var types_js_1 = require("@modelcontextprotocol/sdk/types.js");
var axios_1 = require("axios");
var PrologToolkitMCPServer = /** @class */ (function () {
    function PrologToolkitMCPServer(config) {
        this.config = __assign({ timeout: 30000 }, config);
        this.server = new index_js_1.Server({
            name: 'vscode-prolog-toolkit',
            version: '1.0.0',
            capabilities: {
                resources: {},
                tools: {},
            },
        });
        this.setupHandlers();
    }
    PrologToolkitMCPServer.prototype.setupHandlers = function () {
        var _this = this;
        // List available tools
        this.server.setRequestHandler(types_js_1.ListToolsRequestSchema, function () { return __awaiter(_this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                return [2 /*return*/, {
                        tools: [
                            {
                                name: 'execute_prolog_query',
                                description: 'Execute a Prolog query and return results',
                                inputSchema: {
                                    type: 'object',
                                    properties: {
                                        query: {
                                            type: 'string',
                                            description: 'The Prolog query to execute'
                                        },
                                        sessionId: {
                                            type: 'string',
                                            description: 'Optional session ID to execute query in specific session'
                                        },
                                        timeout: {
                                            type: 'number',
                                            description: 'Query timeout in milliseconds (default: 30000)'
                                        }
                                    },
                                    required: ['query']
                                }
                            },
                            {
                                name: 'consult_prolog_file',
                                description: 'Load/consult a Prolog file into the knowledge base',
                                inputSchema: {
                                    type: 'object',
                                    properties: {
                                        filePath: {
                                            type: 'string',
                                            description: 'Path to the Prolog file to consult'
                                        },
                                        sessionId: {
                                            type: 'string',
                                            description: 'Optional session ID to load file into specific session'
                                        }
                                    },
                                    required: ['filePath']
                                }
                            },
                            {
                                name: 'create_prolog_session',
                                description: 'Create a new Prolog session for isolated query execution',
                                inputSchema: {
                                    type: 'object',
                                    properties: {
                                        name: {
                                            type: 'string',
                                            description: 'Name for the new session'
                                        },
                                        description: {
                                            type: 'string',
                                            description: 'Optional description for the session'
                                        }
                                    },
                                    required: ['name']
                                }
                            },
                            {
                                name: 'list_prolog_sessions',
                                description: 'List all available Prolog sessions',
                                inputSchema: {
                                    type: 'object',
                                    properties: {
                                        includeInactive: {
                                            type: 'boolean',
                                            description: 'Include inactive sessions in the list (default: false)'
                                        }
                                    }
                                }
                            },
                            {
                                name: 'get_session_state',
                                description: 'Get the current state of a Prolog session',
                                inputSchema: {
                                    type: 'object',
                                    properties: {
                                        sessionId: {
                                            type: 'string',
                                            description: 'ID of the session to inspect'
                                        }
                                    },
                                    required: ['sessionId']
                                }
                            },
                            {
                                name: 'validate_prolog_syntax',
                                description: 'Validate Prolog code syntax without executing it',
                                inputSchema: {
                                    type: 'object',
                                    properties: {
                                        code: {
                                            type: 'string',
                                            description: 'Prolog code to validate'
                                        }
                                    },
                                    required: ['code']
                                }
                            },
                            {
                                name: 'get_prolog_help',
                                description: 'Get help information about Prolog predicates or concepts',
                                inputSchema: {
                                    type: 'object',
                                    properties: {
                                        topic: {
                                            type: 'string',
                                            description: 'Predicate name or concept to get help for'
                                        }
                                    },
                                    required: ['topic']
                                }
                            }
                        ]
                    }];
            });
        }); });
        // Handle tool calls
        this.server.setRequestHandler(types_js_1.CallToolRequestSchema, function (request) { return __awaiter(_this, void 0, void 0, function () {
            var _a, name, args, _b, error_1;
            return __generator(this, function (_c) {
                switch (_c.label) {
                    case 0:
                        _a = request.params, name = _a.name, args = _a.arguments;
                        _c.label = 1;
                    case 1:
                        _c.trys.push([1, 18, , 19]);
                        _b = name;
                        switch (_b) {
                            case 'execute_prolog_query': return [3 /*break*/, 2];
                            case 'consult_prolog_file': return [3 /*break*/, 4];
                            case 'create_prolog_session': return [3 /*break*/, 6];
                            case 'list_prolog_sessions': return [3 /*break*/, 8];
                            case 'get_session_state': return [3 /*break*/, 10];
                            case 'validate_prolog_syntax': return [3 /*break*/, 12];
                            case 'get_prolog_help': return [3 /*break*/, 14];
                        }
                        return [3 /*break*/, 16];
                    case 2: return [4 /*yield*/, this.executePrologQuery(args)];
                    case 3: return [2 /*return*/, _c.sent()];
                    case 4: return [4 /*yield*/, this.consultPrologFile(args)];
                    case 5: return [2 /*return*/, _c.sent()];
                    case 6: return [4 /*yield*/, this.createPrologSession(args)];
                    case 7: return [2 /*return*/, _c.sent()];
                    case 8: return [4 /*yield*/, this.listPrologSessions(args)];
                    case 9: return [2 /*return*/, _c.sent()];
                    case 10: return [4 /*yield*/, this.getSessionState(args)];
                    case 11: return [2 /*return*/, _c.sent()];
                    case 12: return [4 /*yield*/, this.validatePrologSyntax(args)];
                    case 13: return [2 /*return*/, _c.sent()];
                    case 14: return [4 /*yield*/, this.getPrologHelp(args)];
                    case 15: return [2 /*return*/, _c.sent()];
                    case 16: throw new types_js_1.McpError(types_js_1.ErrorCode.MethodNotFound, "Unknown tool: ".concat(name));
                    case 17: return [3 /*break*/, 19];
                    case 18:
                        error_1 = _c.sent();
                        if (error_1 instanceof types_js_1.McpError) {
                            throw error_1;
                        }
                        throw new types_js_1.McpError(types_js_1.ErrorCode.InternalError, "Tool execution failed: ".concat(error_1 instanceof Error ? error_1.message : String(error_1)));
                    case 19: return [2 /*return*/];
                }
            });
        }); });
        // List available resources
        this.server.setRequestHandler(types_js_1.ListResourcesRequestSchema, function () { return __awaiter(_this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                return [2 /*return*/, {
                        resources: [
                            {
                                uri: 'prolog://sessions',
                                name: 'Active Prolog Sessions',
                                description: 'List of currently active Prolog sessions',
                                mimeType: 'application/json'
                            },
                            {
                                uri: 'prolog://predicates',
                                name: 'Available Predicates',
                                description: 'List of available Prolog predicates and their documentation',
                                mimeType: 'application/json'
                            },
                            {
                                uri: 'prolog://examples',
                                name: 'Prolog Examples',
                                description: 'Collection of Prolog code examples and tutorials',
                                mimeType: 'text/plain'
                            }
                        ]
                    }];
            });
        }); });
        // Handle resource reads
        this.server.setRequestHandler(types_js_1.ReadResourceRequestSchema, function (request) { return __awaiter(_this, void 0, void 0, function () {
            var uri, _a, error_2;
            return __generator(this, function (_b) {
                switch (_b.label) {
                    case 0:
                        uri = request.params.uri;
                        _b.label = 1;
                    case 1:
                        _b.trys.push([1, 10, , 11]);
                        _a = uri;
                        switch (_a) {
                            case 'prolog://sessions': return [3 /*break*/, 2];
                            case 'prolog://predicates': return [3 /*break*/, 4];
                            case 'prolog://examples': return [3 /*break*/, 6];
                        }
                        return [3 /*break*/, 8];
                    case 2: return [4 /*yield*/, this.getSessionsResource()];
                    case 3: return [2 /*return*/, _b.sent()];
                    case 4: return [4 /*yield*/, this.getPredicatesResource()];
                    case 5: return [2 /*return*/, _b.sent()];
                    case 6: return [4 /*yield*/, this.getExamplesResource()];
                    case 7: return [2 /*return*/, _b.sent()];
                    case 8: throw new types_js_1.McpError(types_js_1.ErrorCode.InvalidRequest, "Unknown resource: ".concat(uri));
                    case 9: return [3 /*break*/, 11];
                    case 10:
                        error_2 = _b.sent();
                        throw new types_js_1.McpError(types_js_1.ErrorCode.InternalError, "Failed to read resource: ".concat(error_2 instanceof Error ? error_2.message : String(error_2)));
                    case 11: return [2 /*return*/];
                }
            });
        }); });
    };
    // Tool implementations
    PrologToolkitMCPServer.prototype.executePrologQuery = function (args) {
        return __awaiter(this, void 0, void 0, function () {
            var query, sessionId, _a, timeout, response, result;
            return __generator(this, function (_b) {
                switch (_b.label) {
                    case 0:
                        query = args.query, sessionId = args.sessionId, _a = args.timeout, timeout = _a === void 0 ? this.config.timeout : _a;
                        return [4 /*yield*/, this.makeApiRequest('/api/query', {
                                method: 'POST',
                                data: {
                                    query: query,
                                    sessionId: sessionId,
                                    timeout: timeout
                                }
                            })];
                    case 1:
                        response = _b.sent();
                        result = response.data;
                        return [2 /*return*/, {
                                content: [
                                    {
                                        type: 'text',
                                        text: this.formatQueryResult(result)
                                    }
                                ]
                            }];
                }
            });
        });
    };
    PrologToolkitMCPServer.prototype.consultPrologFile = function (args) {
        return __awaiter(this, void 0, void 0, function () {
            var filePath, sessionId, response;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        filePath = args.filePath, sessionId = args.sessionId;
                        return [4 /*yield*/, this.makeApiRequest('/api/consult', {
                                method: 'POST',
                                data: {
                                    filePath: filePath,
                                    sessionId: sessionId
                                }
                            })];
                    case 1:
                        response = _a.sent();
                        return [2 /*return*/, {
                                content: [
                                    {
                                        type: 'text',
                                        text: "Successfully consulted file: ".concat(filePath, "\n").concat(JSON.stringify(response.data, null, 2))
                                    }
                                ]
                            }];
                }
            });
        });
    };
    PrologToolkitMCPServer.prototype.createPrologSession = function (args) {
        return __awaiter(this, void 0, void 0, function () {
            var name, description, response, session;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        name = args.name, description = args.description;
                        return [4 /*yield*/, this.makeApiRequest('/api/sessions', {
                                method: 'POST',
                                data: {
                                    name: name,
                                    description: description
                                }
                            })];
                    case 1:
                        response = _a.sent();
                        session = response.data;
                        return [2 /*return*/, {
                                content: [
                                    {
                                        type: 'text',
                                        text: "Created new Prolog session:\nID: ".concat(session.id, "\nName: ").concat(session.name, "\nCreated: ").concat(session.createdAt)
                                    }
                                ]
                            }];
                }
            });
        });
    };
    PrologToolkitMCPServer.prototype.listPrologSessions = function (args) {
        return __awaiter(this, void 0, void 0, function () {
            var _a, includeInactive, response, sessions;
            return __generator(this, function (_b) {
                switch (_b.label) {
                    case 0:
                        _a = args.includeInactive, includeInactive = _a === void 0 ? false : _a;
                        return [4 /*yield*/, this.makeApiRequest('/api/sessions', {
                                method: 'GET',
                                params: {
                                    includeInactive: includeInactive
                                }
                            })];
                    case 1:
                        response = _b.sent();
                        sessions = response.data;
                        return [2 /*return*/, {
                                content: [
                                    {
                                        type: 'text',
                                        text: this.formatSessionsList(sessions)
                                    }
                                ]
                            }];
                }
            });
        });
    };
    PrologToolkitMCPServer.prototype.getSessionState = function (args) {
        return __awaiter(this, void 0, void 0, function () {
            var sessionId, response;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        sessionId = args.sessionId;
                        return [4 /*yield*/, this.makeApiRequest("/api/sessions/".concat(sessionId, "/state"), {
                                method: 'GET'
                            })];
                    case 1:
                        response = _a.sent();
                        return [2 /*return*/, {
                                content: [
                                    {
                                        type: 'text',
                                        text: "Session State for ".concat(sessionId, ":\n").concat(JSON.stringify(response.data, null, 2))
                                    }
                                ]
                            }];
                }
            });
        });
    };
    PrologToolkitMCPServer.prototype.validatePrologSyntax = function (args) {
        return __awaiter(this, void 0, void 0, function () {
            var code, response, validation;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        code = args.code;
                        return [4 /*yield*/, this.makeApiRequest('/api/validate', {
                                method: 'POST',
                                data: {
                                    code: code
                                }
                            })];
                    case 1:
                        response = _a.sent();
                        validation = response.data;
                        return [2 /*return*/, {
                                content: [
                                    {
                                        type: 'text',
                                        text: validation.valid
                                            ? 'Prolog syntax is valid ✓'
                                            : "Syntax errors found:\n".concat(validation.errors.map(function (e) { return "- Line ".concat(e.line, ": ").concat(e.message); }).join('\n'))
                                    }
                                ]
                            }];
                }
            });
        });
    };
    PrologToolkitMCPServer.prototype.getPrologHelp = function (args) {
        return __awaiter(this, void 0, void 0, function () {
            var topic, response, help;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        topic = args.topic;
                        return [4 /*yield*/, this.makeApiRequest('/api/help', {
                                method: 'GET',
                                params: {
                                    topic: topic
                                }
                            })];
                    case 1:
                        response = _a.sent();
                        help = response.data;
                        return [2 /*return*/, {
                                content: [
                                    {
                                        type: 'text',
                                        text: this.formatHelpContent(help)
                                    }
                                ]
                            }];
                }
            });
        });
    };
    // Resource implementations
    PrologToolkitMCPServer.prototype.getSessionsResource = function () {
        return __awaiter(this, void 0, void 0, function () {
            var response;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.makeApiRequest('/api/sessions', {
                            method: 'GET'
                        })];
                    case 1:
                        response = _a.sent();
                        return [2 /*return*/, {
                                contents: [
                                    {
                                        uri: 'prolog://sessions',
                                        mimeType: 'application/json',
                                        text: JSON.stringify(response.data, null, 2)
                                    }
                                ]
                            }];
                }
            });
        });
    };
    PrologToolkitMCPServer.prototype.getPredicatesResource = function () {
        return __awaiter(this, void 0, void 0, function () {
            var response;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.makeApiRequest('/api/predicates', {
                            method: 'GET'
                        })];
                    case 1:
                        response = _a.sent();
                        return [2 /*return*/, {
                                contents: [
                                    {
                                        uri: 'prolog://predicates',
                                        mimeType: 'application/json',
                                        text: JSON.stringify(response.data, null, 2)
                                    }
                                ]
                            }];
                }
            });
        });
    };
    PrologToolkitMCPServer.prototype.getExamplesResource = function () {
        return __awaiter(this, void 0, void 0, function () {
            var examples;
            return __generator(this, function (_a) {
                examples = "\n# Prolog Examples\n\n## Basic Facts and Rules\n\n```prolog\n% Facts\nparent(tom, bob).\nparent(tom, liz).\nparent(bob, ann).\nparent(bob, pat).\nparent(pat, jim).\n\n% Rules\ngrandparent(X, Z) :- parent(X, Y), parent(Y, Z).\nancestor(X, Z) :- parent(X, Z).\nancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).\n```\n\n## List Processing\n\n```prolog\n% List membership\nmember(X, [X|_]).\nmember(X, [_|T]) :- member(X, T).\n\n% List length\nlength([], 0).\nlength([_|T], N) :- length(T, N1), N is N1 + 1.\n\n% List append\nappend([], L, L).\nappend([H|T1], L2, [H|T3]) :- append(T1, L2, T3).\n```\n\n## Arithmetic\n\n```prolog\n% Factorial\nfactorial(0, 1).\nfactorial(N, F) :- \n    N > 0,\n    N1 is N - 1,\n    factorial(N1, F1),\n    F is N * F1.\n\n% Fibonacci\nfib(0, 0).\nfib(1, 1).\nfib(N, F) :-\n    N > 1,\n    N1 is N - 1,\n    N2 is N - 2,\n    fib(N1, F1),\n    fib(N2, F2),\n    F is F1 + F2.\n```\n";
                return [2 /*return*/, {
                        contents: [
                            {
                                uri: 'prolog://examples',
                                mimeType: 'text/plain',
                                text: examples
                            }
                        ]
                    }];
            });
        });
    };
    // Helper methods
    PrologToolkitMCPServer.prototype.makeApiRequest = function (endpoint, options) {
        return __awaiter(this, void 0, void 0, function () {
            var config, error_3, message;
            var _a, _b;
            return __generator(this, function (_c) {
                switch (_c.label) {
                    case 0:
                        config = __assign(__assign({}, options), { url: "".concat(this.config.apiUrl).concat(endpoint), timeout: this.config.timeout, headers: __assign(__assign({ 'Content-Type': 'application/json' }, (this.config.apiKey && { 'Authorization': "Bearer ".concat(this.config.apiKey) })), options.headers) });
                        _c.label = 1;
                    case 1:
                        _c.trys.push([1, 3, , 4]);
                        return [4 /*yield*/, (0, axios_1.default)(config)];
                    case 2: return [2 /*return*/, _c.sent()];
                    case 3:
                        error_3 = _c.sent();
                        if (axios_1.default.isAxiosError(error_3)) {
                            message = ((_b = (_a = error_3.response) === null || _a === void 0 ? void 0 : _a.data) === null || _b === void 0 ? void 0 : _b.message) || error_3.message;
                            throw new Error("API request failed: ".concat(message));
                        }
                        throw error_3;
                    case 4: return [2 /*return*/];
                }
            });
        });
    };
    PrologToolkitMCPServer.prototype.formatQueryResult = function (result) {
        if (!result.success) {
            return "Query failed: ".concat(result.error);
        }
        if (!result.results || result.results.length === 0) {
            return 'Query succeeded but returned no results.';
        }
        var output = "Query succeeded with ".concat(result.results.length, " result(s):\n\n");
        result.results.forEach(function (res, index) {
            output += "Result ".concat(index + 1, ":\n");
            if (typeof res === 'object') {
                output += JSON.stringify(res, null, 2);
            }
            else {
                output += String(res);
            }
            output += '\n\n';
        });
        if (result.executionTime) {
            output += "Execution time: ".concat(result.executionTime, "ms");
        }
        return output;
    };
    PrologToolkitMCPServer.prototype.formatSessionsList = function (sessions) {
        if (sessions.length === 0) {
            return 'No Prolog sessions found.';
        }
        var output = "Found ".concat(sessions.length, " Prolog session(s):\n\n");
        sessions.forEach(function (session) {
            output += "\u2022 ".concat(session.name, " (").concat(session.id, ")\n");
            output += "  Status: ".concat(session.isActive ? 'Active' : 'Inactive', "\n");
            output += "  Created: ".concat(session.createdAt, "\n");
            output += "  Last accessed: ".concat(session.lastAccessedAt, "\n\n");
        });
        return output;
    };
    PrologToolkitMCPServer.prototype.formatHelpContent = function (help) {
        if (!help || !help.topic) {
            return 'No help information found for the specified topic.';
        }
        var output = "Help for: ".concat(help.topic, "\n\n");
        if (help.description) {
            output += "Description: ".concat(help.description, "\n\n");
        }
        if (help.syntax) {
            output += "Syntax: ".concat(help.syntax, "\n\n");
        }
        if (help.examples && help.examples.length > 0) {
            output += 'Examples:\n';
            help.examples.forEach(function (example, index) {
                output += "".concat(index + 1, ". ").concat(example, "\n");
            });
            output += '\n';
        }
        if (help.seeAlso && help.seeAlso.length > 0) {
            output += "See also: ".concat(help.seeAlso.join(', '), "\n");
        }
        return output;
    };
    PrologToolkitMCPServer.prototype.start = function () {
        return __awaiter(this, void 0, void 0, function () {
            var transport;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        transport = new stdio_js_1.StdioServerTransport();
                        return [4 /*yield*/, this.server.connect(transport)];
                    case 1:
                        _a.sent();
                        console.error('VSCode Prolog Toolkit MCP Server started');
                        return [2 /*return*/];
                }
            });
        });
    };
    return PrologToolkitMCPServer;
}());
exports.PrologToolkitMCPServer = PrologToolkitMCPServer;
// Main execution
function main() {
    return __awaiter(this, void 0, void 0, function () {
        var config, server;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    config = {
                        apiUrl: process.env.PROLOG_TOOLKIT_API_URL || 'http://localhost:3000',
                        wsUrl: process.env.PROLOG_TOOLKIT_WS_URL,
                        apiKey: process.env.PROLOG_TOOLKIT_API_KEY,
                        timeout: parseInt(process.env.PROLOG_TOOLKIT_TIMEOUT || '30000')
                    };
                    server = new PrologToolkitMCPServer(config);
                    return [4 /*yield*/, server.start()];
                case 1:
                    _a.sent();
                    return [2 /*return*/];
            }
        });
    });
}
// Handle process termination
process.on('SIGINT', function () {
    console.error('Received SIGINT, shutting down gracefully...');
    process.exit(0);
});
process.on('SIGTERM', function () {
    console.error('Received SIGTERM, shutting down gracefully...');
    process.exit(0);
});
// Start the server
if (import.meta.url === "file://".concat(process.argv[1])) {
    main().catch(function (error) {
        console.error('Failed to start MCP server:', error);
        process.exit(1);
    });
}
