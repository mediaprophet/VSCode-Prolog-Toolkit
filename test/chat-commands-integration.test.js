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
var fs = require("fs");
var path = require("path");
var prologBackend_js_1 = require("../src/prologBackend.js");
describe('Chat Commands Integration Tests', function () {
    this.timeout(30000); // Extended timeout for comprehensive testing
    var backend;
    var testPort = 3062; // Use unique port for this test suite
    before(function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                // Initialize backend for testing
                backend = new prologBackend_js_1.PrologBackend({
                    swiplPath: 'swipl',
                    port: testPort,
                });
                // Start backend and wait for it to be ready
                return [2 /*return*/, new Promise(function (resolve, reject) {
                        var timeout = setTimeout(function () {
                            reject(new Error('Backend startup timeout'));
                        }, 20000);
                        var onReady = function () {
                            clearTimeout(timeout);
                            backend.off('ready', onReady);
                            backend.off('error', onError);
                            backend.off('started', onStarted);
                            resolve();
                        };
                        var onStarted = function () {
                            clearTimeout(timeout);
                            backend.off('ready', onReady);
                            backend.off('error', onError);
                            backend.off('started', onStarted);
                            resolve();
                        };
                        var onError = function (error) {
                            clearTimeout(timeout);
                            backend.off('ready', onReady);
                            backend.off('error', onError);
                            backend.off('started', onStarted);
                            reject(error);
                        };
                        backend.on('ready', onReady);
                        backend.on('started', onStarted);
                        backend.on('error', onError);
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
    describe('/query command', function () {
        it('should execute simple arithmetic queries', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('query', {
                                goal: 'X is 2 + 3',
                                timeoutMs: 5000,
                            })];
                        case 1:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('ok');
                            (0, chai_1.expect)(response.results).to.be.an('array');
                            (0, chai_1.expect)(response.results.length).to.be.greaterThan(0);
                            (0, chai_1.expect)(response.results[0]).to.have.property('X', 5);
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should execute list membership queries', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('query', {
                                goal: 'member(X, [a, b, c])',
                                timeoutMs: 5000,
                            })];
                        case 1:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('ok');
                            (0, chai_1.expect)(response.results).to.be.an('array');
                            (0, chai_1.expect)(response.results.length).to.equal(3);
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle queries with no solutions', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('query', {
                                goal: 'member(z, [a, b, c])',
                                timeoutMs: 5000,
                            })];
                        case 1:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('ok');
                            (0, chai_1.expect)(response.results).to.be.an('array');
                            (0, chai_1.expect)(response.results.length).to.equal(0);
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle syntax errors gracefully', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('query', {
                                goal: 'invalid_syntax(',
                                timeoutMs: 5000,
                            })];
                        case 1:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('error');
                            (0, chai_1.expect)(response.error).to.be.a('string');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle timeout for long-running queries', function () {
            return __awaiter(this, void 0, void 0, function () {
                var error_1;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            _a.trys.push([0, 2, , 3]);
                            return [4 /*yield*/, backend.sendRequest('query', {
                                    goal: 'sleep(10)',
                                    timeoutMs: 2000,
                                })];
                        case 1:
                            _a.sent();
                            chai_1.expect.fail('Should have timed out');
                            return [3 /*break*/, 3];
                        case 2:
                            error_1 = _a.sent();
                            (0, chai_1.expect)(error_1).to.exist;
                            return [3 /*break*/, 3];
                        case 3: return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('/consult command', function () {
        it('should consult existing Prolog file', function () {
            return __awaiter(this, void 0, void 0, function () {
                var testFile, response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            testFile = path.join(__dirname, 'resources', 'foo_with_pldoc.pl');
                            return [4 /*yield*/, backend.sendRequest('consult', {
                                    file: testFile,
                                    timeoutMs: 10000,
                                })];
                        case 1:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('ok');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle non-existent files gracefully', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('consult', {
                                file: 'nonexistent.pl',
                                timeoutMs: 5000,
                            })];
                        case 1:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('error');
                            (0, chai_1.expect)(response.error).to.be.a('string');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle files with syntax errors', function () {
            return __awaiter(this, void 0, void 0, function () {
                var tempFile, tempDir, response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            tempFile = path.join(__dirname, 'temp', 'syntax_error.pl');
                            tempDir = path.dirname(tempFile);
                            if (!fs.existsSync(tempDir)) {
                                fs.mkdirSync(tempDir, { recursive: true });
                            }
                            fs.writeFileSync(tempFile, 'invalid_syntax(.\n');
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, , 3, 4]);
                            return [4 /*yield*/, backend.sendRequest('consult', {
                                    file: tempFile,
                                    timeoutMs: 5000,
                                })];
                        case 2:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('error');
                            (0, chai_1.expect)(response.error).to.be.a('string');
                            return [3 /*break*/, 4];
                        case 3:
                            // Clean up
                            if (fs.existsSync(tempFile)) {
                                fs.unlinkSync(tempFile);
                            }
                            return [7 /*endfinally*/];
                        case 4: return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('/help command', function () {
        it('should provide help for built-in predicates', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('help', {
                                predicate: 'member/2',
                                timeoutMs: 10000,
                            })];
                        case 1:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('ok');
                            (0, chai_1.expect)(response.doc).to.be.an('object');
                            (0, chai_1.expect)(response.doc.name).to.equal('member');
                            (0, chai_1.expect)(response.doc.arity).to.equal(2);
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should provide help for user-defined predicates', function () {
            return __awaiter(this, void 0, void 0, function () {
                var testFile, response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            testFile = path.join(__dirname, 'resources', 'foo_with_pldoc.pl');
                            return [4 /*yield*/, backend.sendRequest('consult', { file: testFile, timeoutMs: 5000 })];
                        case 1:
                            _a.sent();
                            return [4 /*yield*/, backend.sendRequest('help', {
                                    predicate: 'foo/2',
                                    timeoutMs: 10000,
                                })];
                        case 2:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('ok');
                            (0, chai_1.expect)(response.doc).to.be.an('object');
                            (0, chai_1.expect)(response.doc.name).to.equal('foo');
                            (0, chai_1.expect)(response.doc.arity).to.equal(2);
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle non-existent predicates', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('help', {
                                predicate: 'nonexistent_predicate/99',
                                timeoutMs: 5000,
                            })];
                        case 1:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('error');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle malformed predicate indicators', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('help', {
                                predicate: 'invalid_format',
                                timeoutMs: 5000,
                            })];
                        case 1:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('error');
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('/status command', function () {
        it('should return backend status information', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('status', {
                                timeoutMs: 5000,
                            })];
                        case 1:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('ok');
                            (0, chai_1.expect)(response).to.have.property('backend_status');
                            (0, chai_1.expect)(response).to.have.property('version');
                            (0, chai_1.expect)(response).to.have.property('uptime');
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('N3 Commands', function () {
        describe('/n3_load command', function () {
            it('should load N3 content from string', function () {
                return __awaiter(this, void 0, void 0, function () {
                    var n3Content, response;
                    return __generator(this, function (_a) {
                        switch (_a.label) {
                            case 0:
                                n3Content = "\n                    @prefix : <http://example.org/> .\n                    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n                    \n                    :socrates a :Person .\n                    :plato a :Person .\n                ";
                                return [4 /*yield*/, backend.sendRequest('n3_load', {
                                        content: n3Content,
                                        timeoutMs: 10000,
                                    })];
                            case 1:
                                response = _a.sent();
                                (0, chai_1.expect)(response.status).to.equal('ok');
                                (0, chai_1.expect)(response.triples_count).to.be.greaterThan(0);
                                return [2 /*return*/];
                        }
                    });
                });
            });
            it('should load N3 content from file', function () {
                return __awaiter(this, void 0, void 0, function () {
                    var n3File, response;
                    return __generator(this, function (_a) {
                        switch (_a.label) {
                            case 0:
                                n3File = path.join(__dirname, 'resources', 'sample.n3');
                                return [4 /*yield*/, backend.sendRequest('n3_load', {
                                        file: n3File,
                                        timeoutMs: 10000,
                                    })];
                            case 1:
                                response = _a.sent();
                                (0, chai_1.expect)(response.status).to.equal('ok');
                                (0, chai_1.expect)(response.triples_count).to.be.greaterThan(0);
                                return [2 /*return*/];
                        }
                    });
                });
            });
            it('should handle invalid N3 syntax', function () {
                return __awaiter(this, void 0, void 0, function () {
                    var invalidN3, response;
                    return __generator(this, function (_a) {
                        switch (_a.label) {
                            case 0:
                                invalidN3 = 'invalid n3 syntax @#$%';
                                return [4 /*yield*/, backend.sendRequest('n3_load', {
                                        content: invalidN3,
                                        timeoutMs: 5000,
                                    })];
                            case 1:
                                response = _a.sent();
                                (0, chai_1.expect)(response.status).to.equal('error');
                                return [2 /*return*/];
                        }
                    });
                });
            });
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
                                (0, chai_1.expect)(response.status).to.equal('error');
                                return [2 /*return*/];
                        }
                    });
                });
            });
        });
        describe('/n3_list command', function () {
            beforeEach(function () {
                return __awaiter(this, void 0, void 0, function () {
                    var n3Content;
                    return __generator(this, function (_a) {
                        switch (_a.label) {
                            case 0:
                                n3Content = "\n                    @prefix : <http://example.org/> .\n                    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n                    \n                    :socrates a :Person .\n                    :plato a :Person .\n                    :aristotle a :Person .\n                ";
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
                                (0, chai_1.expect)(response.status).to.equal('ok');
                                (0, chai_1.expect)(response.triples).to.be.an('array');
                                (0, chai_1.expect)(response.total_count).to.be.greaterThan(0);
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
                                (0, chai_1.expect)(response.status).to.equal('ok');
                                (0, chai_1.expect)(response.triples.length).to.be.at.most(2);
                                return [2 /*return*/];
                        }
                    });
                });
            });
            it('should support different output formats', function () {
                return __awaiter(this, void 0, void 0, function () {
                    var response;
                    return __generator(this, function (_a) {
                        switch (_a.label) {
                            case 0: return [4 /*yield*/, backend.sendRequest('n3_list', {
                                    limit: 5,
                                    format: 'turtle',
                                    timeoutMs: 5000,
                                })];
                            case 1:
                                response = _a.sent();
                                (0, chai_1.expect)(response.status).to.equal('ok');
                                (0, chai_1.expect)(response.triples).to.be.an('array');
                                return [2 /*return*/];
                        }
                    });
                });
            });
        });
        describe('/n3_reason command', function () {
            beforeEach(function () {
                return __awaiter(this, void 0, void 0, function () {
                    var n3Content;
                    return __generator(this, function (_a) {
                        switch (_a.label) {
                            case 0:
                                n3Content = "\n                    @prefix : <http://example.org/> .\n                    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n                    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n                    \n                    :socrates a :Person .\n                    :Person rdfs:subClassOf :Mortal .\n                    \n                    { ?x a :Person } => { ?x a :Mortal } .\n                ";
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
                                    timeoutMs: 15000,
                                })];
                            case 1:
                                response = _a.sent();
                                (0, chai_1.expect)(response.status).to.equal('ok');
                                (0, chai_1.expect)(response.inferred_triples || response.results).to.exist;
                                return [2 /*return*/];
                        }
                    });
                });
            });
            it('should reason with specific goals', function () {
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
                                (0, chai_1.expect)(response.status).to.equal('ok');
                                return [2 /*return*/];
                        }
                    });
                });
            });
        });
        describe('/n3_explain command', function () {
            beforeEach(function () {
                return __awaiter(this, void 0, void 0, function () {
                    var n3Content;
                    return __generator(this, function (_a) {
                        switch (_a.label) {
                            case 0:
                                n3Content = "\n                    @prefix : <http://example.org/> .\n                    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n                    \n                    :socrates a :Person .\n                ";
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
            it('should generate proof explanations', function () {
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
                                (0, chai_1.expect)(response.status).to.equal('ok');
                                (0, chai_1.expect)(response.proof).to.exist;
                                (0, chai_1.expect)(response.goal).to.equal('rdf(socrates, type, Person)');
                                return [2 /*return*/];
                        }
                    });
                });
            });
            it('should handle unprovable goals', function () {
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
                                (0, chai_1.expect)(['ok', 'error']).to.include(response.status);
                                return [2 /*return*/];
                        }
                    });
                });
            });
        });
    });
    describe('Batch Operations', function () {
        it('should handle batch requests with mixed commands', function () {
            return __awaiter(this, void 0, void 0, function () {
                var testFile, batch, responses;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            testFile = path.join(__dirname, 'resources', 'foo_with_pldoc.pl');
                            batch = [
                                { cmd: 'consult', params: { file: testFile } },
                                { cmd: 'query', params: { goal: 'foo(1, X)' } },
                                { cmd: 'help', params: { predicate: 'foo/2' } },
                            ];
                            return [4 /*yield*/, backend.sendRequest(batch)];
                        case 1:
                            responses = _a.sent();
                            (0, chai_1.expect)(responses).to.be.an('array').with.lengthOf(3);
                            (0, chai_1.expect)(responses[0].status).to.equal('ok'); // consult
                            (0, chai_1.expect)(responses[1].status).to.equal('ok'); // query
                            (0, chai_1.expect)(responses[2].status).to.equal('ok'); // help
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle batch requests with errors', function () {
            return __awaiter(this, void 0, void 0, function () {
                var batch, responses;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            batch = [
                                { cmd: 'query', params: { goal: 'X is 1 + 1' } },
                                { cmd: 'query', params: { goal: 'invalid_syntax(' } },
                                { cmd: 'query', params: { goal: 'Y is 2 + 2' } },
                            ];
                            return [4 /*yield*/, backend.sendRequest(batch)];
                        case 1:
                            responses = _a.sent();
                            (0, chai_1.expect)(responses).to.be.an('array').with.lengthOf(3);
                            (0, chai_1.expect)(responses[0].status).to.equal('ok');
                            (0, chai_1.expect)(responses[1].status).to.equal('error');
                            (0, chai_1.expect)(responses[2].status).to.equal('ok');
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('Error Handling and Edge Cases', function () {
        it('should handle empty requests', function () {
            return __awaiter(this, void 0, void 0, function () {
                var error_2;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            _a.trys.push([0, 2, , 3]);
                            return [4 /*yield*/, backend.sendRequest('', {})];
                        case 1:
                            _a.sent();
                            chai_1.expect.fail('Should have thrown an error');
                            return [3 /*break*/, 3];
                        case 2:
                            error_2 = _a.sent();
                            (0, chai_1.expect)(error_2).to.exist;
                            return [3 /*break*/, 3];
                        case 3: return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle invalid command types', function () {
            return __awaiter(this, void 0, void 0, function () {
                var error_3;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            _a.trys.push([0, 2, , 3]);
                            return [4 /*yield*/, backend.sendRequest('invalid_command', {})];
                        case 1:
                            _a.sent();
                            chai_1.expect.fail('Should have thrown an error');
                            return [3 /*break*/, 3];
                        case 2:
                            error_3 = _a.sent();
                            (0, chai_1.expect)(error_3).to.exist;
                            return [3 /*break*/, 3];
                        case 3: return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle requests with missing parameters', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('query', {})];
                        case 1:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('error');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle very large query results', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('query', {
                                goal: 'between(1, 1000, X)',
                                timeoutMs: 10000,
                            })];
                        case 1:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('ok');
                            (0, chai_1.expect)(response.results).to.be.an('array');
                            (0, chai_1.expect)(response.results.length).to.equal(1000);
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
});
