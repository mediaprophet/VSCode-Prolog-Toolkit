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
var assert_1 = require("assert");
var path = require("path");
var prologBackend_1 = require("../src/prologBackend");
describe('N3 Integration Tests', function () {
    this.timeout(30000);
    var backend;
    var testPort = 3061; // Use different port for tests
    before(function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                // Initialize backend for testing
                backend = new prologBackend_1.PrologBackend({
                    swiplPath: 'swipl',
                    port: testPort,
                });
                // Start backend and wait for it to be ready
                return [2 /*return*/, new Promise(function (resolve, reject) {
                        var timeout = setTimeout(function () {
                            reject(new Error('Backend startup timeout'));
                        }, 15000);
                        backend.on('ready', function () {
                            clearTimeout(timeout);
                            resolve(undefined);
                        });
                        backend.on('error', function (error) {
                            clearTimeout(timeout);
                            reject(error);
                        });
                        backend.start();
                    })];
            });
        });
    });
    after(function () {
        if (backend) {
            backend.stop(true);
        }
    });
    describe('N3 Load Command', function () {
        it('should load N3 content successfully', function () {
            return __awaiter(this, void 0, void 0, function () {
                var n3Content, response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            n3Content = "\n                @prefix : <http://example.org/> .\n                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n                @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n                \n                :socrates a :Person .\n                :Person rdfs:subClassOf :Mortal .\n                \n                { ?x a :Person } => { ?x a :Mortal } .\n            ";
                            return [4 /*yield*/, backend.sendRequest('n3_load', {
                                    content: n3Content,
                                    timeoutMs: 10000,
                                })];
                        case 1:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'ok');
                            (0, assert_1.default)(response.triples_count > 0, 'Should load some triples');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should load N3 file successfully', function () {
            return __awaiter(this, void 0, void 0, function () {
                var filePath, response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            filePath = path.join(__dirname, 'resources', 'sample.n3');
                            return [4 /*yield*/, backend.sendRequest('n3_load', {
                                    file: filePath,
                                    timeoutMs: 10000,
                                })];
                        case 1:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'ok');
                            (0, assert_1.default)(response.triples_count > 0, 'Should load some triples');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle invalid N3 content', function () {
            return __awaiter(this, void 0, void 0, function () {
                var invalidContent, response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            invalidContent = 'invalid n3 content @#$%';
                            return [4 /*yield*/, backend.sendRequest('n3_load', {
                                    content: invalidContent,
                                    timeoutMs: 5000,
                                })];
                        case 1:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'error');
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('N3 List Command', function () {
        beforeEach(function () {
            return __awaiter(this, void 0, void 0, function () {
                var n3Content;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            n3Content = "\n                @prefix : <http://example.org/> .\n                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n                \n                :socrates a :Person .\n                :plato a :Person .\n                :aristotle a :Person .\n            ";
                            return [4 /*yield*/, backend.sendRequest('n3_load', {
                                    content: n3Content,
                                    timeoutMs: 5000,
                                })];
                        case 1:
                            _a.sent();
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should list loaded triples', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('n3_list', {
                                limit: 10,
                                format: 'readable',
                                timeoutMs: 5000,
                            })];
                        case 1:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'ok');
                            (0, assert_1.default)(Array.isArray(response.triples), 'Should return array of triples');
                            (0, assert_1.default)(response.total_count > 0, 'Should have some triples');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should respect limit parameter', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('n3_list', {
                                limit: 2,
                                format: 'readable',
                                timeoutMs: 5000,
                            })];
                        case 1:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'ok');
                            (0, assert_1.default)(response.triples.length <= 2, 'Should respect limit');
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('N3 Reason Command', function () {
        beforeEach(function () {
            return __awaiter(this, void 0, void 0, function () {
                var n3Content;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            n3Content = "\n                @prefix : <http://example.org/> .\n                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n                @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n                \n                :socrates a :Person .\n                :Person rdfs:subClassOf :Mortal .\n                \n                { ?x a :Person } => { ?x a :Mortal } .\n            ";
                            return [4 /*yield*/, backend.sendRequest('n3_load', {
                                    content: n3Content,
                                    timeoutMs: 5000,
                                })];
                        case 1:
                            _a.sent();
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should perform general reasoning', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('n3_reason', {
                                goal: '',
                                timeoutMs: 10000,
                            })];
                        case 1:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'ok');
                            (0, assert_1.default)(response.inferred_triples || response.results, 'Should return reasoning results');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should reason with specific goal', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('n3_reason', {
                                goal: 'rdf(socrates, type, Mortal)',
                                timeoutMs: 10000,
                            })];
                        case 1:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'ok');
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('N3 Explain Command', function () {
        beforeEach(function () {
            return __awaiter(this, void 0, void 0, function () {
                var n3Content;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            n3Content = "\n                @prefix : <http://example.org/> .\n                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n                @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n                \n                :socrates a :Person .\n                :Person rdfs:subClassOf :Mortal .\n            ";
                            return [4 /*yield*/, backend.sendRequest('n3_load', {
                                    content: n3Content,
                                    timeoutMs: 5000,
                                })];
                        case 1:
                            _a.sent();
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should generate proof explanation', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('n3_explain', {
                                goal: 'rdf(socrates, type, Person)',
                                timeoutMs: 10000,
                            })];
                        case 1:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'ok');
                            (0, assert_1.default)(response.proof, 'Should return proof tree');
                            assert_1.default.strictEqual(response.goal, 'rdf(socrates, type, Person)');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle goals that cannot be proven', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('n3_explain', {
                                goal: 'rdf(nonexistent, type, Something)',
                                timeoutMs: 5000,
                            })];
                        case 1:
                            response = _a.sent();
                            // Should either succeed with empty proof or fail gracefully
                            (0, assert_1.default)(response.status === 'ok' || response.status === 'error');
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('N3 Input Validation', function () {
        it('should validate file extensions', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('n3_load', {
                                file: 'invalid.txt',
                                timeoutMs: 5000,
                            })];
                        case 1:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'error');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle missing files', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('n3_load', {
                                file: 'nonexistent.n3',
                                timeoutMs: 5000,
                            })];
                        case 1:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'error');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should validate content size', function () {
            return __awaiter(this, void 0, void 0, function () {
                var largeContent, response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            largeContent = 'a'.repeat(2000000);
                            return [4 /*yield*/, backend.sendRequest('n3_load', {
                                    content: largeContent,
                                    timeoutMs: 5000,
                                })];
                        case 1:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'error');
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('N3 Prefix Formatting', function () {
        beforeEach(function () {
            return __awaiter(this, void 0, void 0, function () {
                var n3Content;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            n3Content = "\n                @prefix : <http://example.org/> .\n                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n                @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n                \n                :socrates a :Person .\n            ";
                            return [4 /*yield*/, backend.sendRequest('n3_load', {
                                    content: n3Content,
                                    timeoutMs: 5000,
                                })];
                        case 1:
                            _a.sent();
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should format URIs with readable prefixes', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response, hasReadablePrefix;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('n3_list', {
                                limit: 5,
                                format: 'readable',
                                timeoutMs: 5000,
                            })];
                        case 1:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'ok');
                            hasReadablePrefix = response.triples.some(function (triple) {
                                return triple.subject.startsWith(':') ||
                                    triple.predicate.startsWith('rdf:') ||
                                    triple.object.startsWith(':');
                            });
                            (0, assert_1.default)(hasReadablePrefix, 'Should use readable prefixes');
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
});
