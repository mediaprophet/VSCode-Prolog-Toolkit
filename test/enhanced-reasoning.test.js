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
var prologBackend_js_1 = require("../src/prologBackend.js");
describe('Enhanced Reasoning Features Tests', function () {
    this.timeout(30000);
    var backend;
    var testPort = 3062; // Use different port for tests
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
    describe('Constraint Logic Programming (CLP) Tests', function () {
        describe('CLP(FD) - Finite Domain Constraints', function () {
            it('should solve basic arithmetic constraints', function () {
                return __awaiter(this, void 0, void 0, function () {
                    var response;
                    return __generator(this, function (_a) {
                        switch (_a.label) {
                            case 0: return [4 /*yield*/, backend.sendRequest('clp_solve', {
                                    domain: 'fd',
                                    variables: ['X', 'Y'],
                                    constraints: ['X #= Y + 1', 'X #< 10', 'Y #> 0'],
                                    timeoutMs: 10000,
                                })];
                            case 1:
                                response = _a.sent();
                                assert_1.default.strictEqual(response.status, 'ok');
                                assert_1.default.strictEqual(response.domain, 'fd');
                                (0, assert_1.default)(Array.isArray(response.solution), 'Should return solution array');
                                (0, assert_1.default)(response.solution.length === 2, 'Should have solutions for X and Y');
                                return [2 /*return*/];
                        }
                    });
                });
            });
            it('should solve all_different constraint', function () {
                return __awaiter(this, void 0, void 0, function () {
                    var response;
                    return __generator(this, function (_a) {
                        switch (_a.label) {
                            case 0: return [4 /*yield*/, backend.sendRequest('clp_solve', {
                                    domain: 'fd',
                                    variables: ['A', 'B', 'C'],
                                    constraints: ['all_different([A, B, C])', 'A in 1..3', 'B in 1..3', 'C in 1..3'],
                                    timeoutMs: 10000,
                                })];
                            case 1:
                                response = _a.sent();
                                assert_1.default.strictEqual(response.status, 'ok');
                                (0, assert_1.default)(response.solution.length === 3, 'Should have solutions for A, B, C');
                                return [2 /*return*/];
                        }
                    });
                });
            });
            it('should handle unsatisfiable constraints', function () {
                return __awaiter(this, void 0, void 0, function () {
                    var response;
                    return __generator(this, function (_a) {
                        switch (_a.label) {
                            case 0: return [4 /*yield*/, backend.sendRequest('clp_solve', {
                                    domain: 'fd',
                                    variables: ['X'],
                                    constraints: ['X #> 10', 'X #< 5'],
                                    timeoutMs: 5000,
                                })];
                            case 1:
                                response = _a.sent();
                                // Should either fail gracefully or return error
                                (0, assert_1.default)(response.status === 'error' ||
                                    (response.status === 'ok' && response.solution.length === 0));
                                return [2 /*return*/];
                        }
                    });
                });
            });
        });
        describe('CLP(R) - Real Number Constraints', function () {
            it('should solve real number constraints', function () {
                return __awaiter(this, void 0, void 0, function () {
                    var response;
                    return __generator(this, function (_a) {
                        switch (_a.label) {
                            case 0: return [4 /*yield*/, backend.sendRequest('clp_solve', {
                                    domain: 'r',
                                    variables: ['X', 'Y'],
                                    constraints: ['X =:= Y * 2.5', 'Y >= 1.0', 'X =< 10.0'],
                                    timeoutMs: 10000,
                                })];
                            case 1:
                                response = _a.sent();
                                assert_1.default.strictEqual(response.status, 'ok');
                                assert_1.default.strictEqual(response.domain, 'r');
                                (0, assert_1.default)(response.solution.length === 2, 'Should have solutions for X and Y');
                                return [2 /*return*/];
                        }
                    });
                });
            });
        });
        describe('CLP Constraint Management', function () {
            it('should add and store constraints', function () {
                return __awaiter(this, void 0, void 0, function () {
                    var response;
                    return __generator(this, function (_a) {
                        switch (_a.label) {
                            case 0: return [4 /*yield*/, backend.sendRequest('clp_constraint', {
                                    domain: 'fd',
                                    constraint: 'X #> 0',
                                    timeoutMs: 5000,
                                })];
                            case 1:
                                response = _a.sent();
                                assert_1.default.strictEqual(response.status, 'ok');
                                assert_1.default.strictEqual(response.domain, 'fd');
                                (0, assert_1.default)(response.message.includes('successfully'), 'Should confirm constraint addition');
                                return [2 /*return*/];
                        }
                    });
                });
            });
            it('should validate constraint domains', function () {
                return __awaiter(this, void 0, void 0, function () {
                    var response;
                    return __generator(this, function (_a) {
                        switch (_a.label) {
                            case 0: return [4 /*yield*/, backend.sendRequest('clp_constraint', {
                                    domain: 'invalid_domain',
                                    constraint: 'X #> 0',
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
    });
    describe('Probabilistic Logic Tests', function () {
        beforeEach(function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: 
                        // Add some probabilistic facts for testing
                        return [4 /*yield*/, backend.sendRequest('probabilistic_fact', {
                                fact: 'weather(sunny)',
                                probability: 0.7,
                                timeoutMs: 5000,
                            })];
                        case 1:
                            // Add some probabilistic facts for testing
                            _a.sent();
                            return [4 /*yield*/, backend.sendRequest('probabilistic_fact', {
                                    fact: 'weather(rainy)',
                                    probability: 0.3,
                                    timeoutMs: 5000,
                                })];
                        case 2:
                            _a.sent();
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should add probabilistic facts', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('probabilistic_fact', {
                                fact: 'likes(john, pizza)',
                                probability: 0.8,
                                timeoutMs: 5000,
                            })];
                        case 1:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'ok');
                            assert_1.default.strictEqual(response.probability, 0.8);
                            (0, assert_1.default)(response.message.includes('successfully'), 'Should confirm fact addition');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should validate probability values', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('probabilistic_fact', {
                                fact: 'invalid_prob_fact',
                                probability: 1.5, // Invalid probability > 1
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
        it('should perform probabilistic inference', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('probabilistic_query', {
                                goal: 'weather(sunny)',
                                method: 'monte_carlo',
                                samples: 100,
                                timeoutMs: 10000,
                            })];
                        case 1:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'ok');
                            (0, assert_1.default)(typeof response.probability === 'number', 'Should return probability');
                            (0, assert_1.default)(response.probability >= 0 && response.probability <= 1, 'Probability should be between 0 and 1');
                            assert_1.default.strictEqual(response.method, 'monte_carlo');
                            assert_1.default.strictEqual(response.samples, 100);
                            (0, assert_1.default)(response.evidence, 'Should provide evidence');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle complex probabilistic queries', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: 
                        // Add more complex probabilistic facts
                        return [4 /*yield*/, backend.sendRequest('probabilistic_fact', {
                                fact: 'traffic(heavy)',
                                probability: 0.4,
                                timeoutMs: 5000,
                            })];
                        case 1:
                            // Add more complex probabilistic facts
                            _a.sent();
                            return [4 /*yield*/, backend.sendRequest('probabilistic_query', {
                                    goal: 'traffic(heavy)',
                                    method: 'monte_carlo',
                                    samples: 500,
                                    timeoutMs: 10000,
                                })];
                        case 2:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'ok');
                            (0, assert_1.default)(response.evidence.success_count >= 0, 'Should have success count');
                            (0, assert_1.default)(response.evidence.total_samples === 500, 'Should match requested samples');
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('User-defined Logic Modules Tests', function () {
        it('should register a logic module', function () {
            return __awaiter(this, void 0, void 0, function () {
                var rules, response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            rules = ['mortal(X) :- human(X)', 'human(socrates)', 'human(plato)'];
                            return [4 /*yield*/, backend.sendRequest('logic_module_register', {
                                    name: 'philosophy',
                                    rules: rules,
                                    meta_interpreter: 'default',
                                    timeoutMs: 10000,
                                })];
                        case 1:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'ok');
                            assert_1.default.strictEqual(response.name, 'philosophy');
                            assert_1.default.strictEqual(response.meta_interpreter, 'default');
                            (0, assert_1.default)(response.message.includes('successfully'), 'Should confirm registration');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should query a registered logic module', function () {
            return __awaiter(this, void 0, void 0, function () {
                var rules, response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            rules = [
                                'animal(X) :- mammal(X)',
                                'mammal(dog)',
                                'mammal(cat)',
                                'bird(eagle)',
                                'animal(X) :- bird(X)',
                            ];
                            return [4 /*yield*/, backend.sendRequest('logic_module_register', {
                                    name: 'animals',
                                    rules: rules,
                                    meta_interpreter: 'default',
                                    timeoutMs: 5000,
                                })];
                        case 1:
                            _a.sent();
                            return [4 /*yield*/, backend.sendRequest('logic_module_query', {
                                    module: 'animals',
                                    goal: 'animal(X)',
                                    timeoutMs: 10000,
                                })];
                        case 2:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'ok');
                            assert_1.default.strictEqual(response.module, 'animals');
                            (0, assert_1.default)(Array.isArray(response.results), 'Should return results array');
                            (0, assert_1.default)(response.count >= 0, 'Should have result count');
                            (0, assert_1.default)(Array.isArray(response.proof_trace), 'Should return proof trace');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should list registered logic modules', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response, module;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: 
                        // Register a couple of modules first
                        return [4 /*yield*/, backend.sendRequest('logic_module_register', {
                                name: 'test_module_1',
                                rules: ['fact1(a)', 'fact2(b)'],
                                meta_interpreter: 'default',
                                timeoutMs: 5000,
                            })];
                        case 1:
                            // Register a couple of modules first
                            _a.sent();
                            return [4 /*yield*/, backend.sendRequest('logic_module_register', {
                                    name: 'test_module_2',
                                    rules: ['rule1(X) :- fact1(X)'],
                                    meta_interpreter: 'custom',
                                    timeoutMs: 5000,
                                })];
                        case 2:
                            _a.sent();
                            return [4 /*yield*/, backend.sendRequest('logic_module_list', {
                                    timeoutMs: 5000,
                                })];
                        case 3:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'ok');
                            (0, assert_1.default)(Array.isArray(response.modules), 'Should return modules array');
                            (0, assert_1.default)(response.count >= 2, 'Should have at least 2 modules');
                            module = response.modules.find(function (m) { return m.name === 'test_module_1'; });
                            (0, assert_1.default)(module, 'Should find test_module_1');
                            (0, assert_1.default)(typeof module.rules_count === 'number', 'Should have rules count');
                            (0, assert_1.default)(module.meta_interpreter, 'Should have meta interpreter');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle custom meta-interpreter', function () {
            return __awaiter(this, void 0, void 0, function () {
                var rules, response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            rules = ['custom_rule(X) :- base_fact(X)', 'base_fact(test_value)'];
                            return [4 /*yield*/, backend.sendRequest('logic_module_register', {
                                    name: 'custom_meta_test',
                                    rules: rules,
                                    meta_interpreter: 'custom',
                                    timeoutMs: 10000,
                                })];
                        case 1:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'ok');
                            assert_1.default.strictEqual(response.meta_interpreter, 'custom');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should validate module names', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('logic_module_register', {
                                name: 123, // Invalid name (not string/atom)
                                rules: ['test_rule'],
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
        it('should handle queries to non-existent modules', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('logic_module_query', {
                                module: 'non_existent_module',
                                goal: 'some_goal',
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
    describe('Integration Tests', function () {
        it('should combine CLP with probabilistic reasoning', function () {
            return __awaiter(this, void 0, void 0, function () {
                var clpResponse, probResponse;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: 
                        // Add probabilistic constraint
                        return [4 /*yield*/, backend.sendRequest('probabilistic_fact', {
                                fact: 'constraint_holds(X)',
                                probability: 0.6,
                                timeoutMs: 5000,
                            })];
                        case 1:
                            // Add probabilistic constraint
                            _a.sent();
                            return [4 /*yield*/, backend.sendRequest('clp_solve', {
                                    domain: 'fd',
                                    variables: ['X'],
                                    constraints: ['X in 1..10'],
                                    timeoutMs: 5000,
                                })];
                        case 2:
                            clpResponse = _a.sent();
                            return [4 /*yield*/, backend.sendRequest('probabilistic_query', {
                                    goal: 'constraint_holds(5)',
                                    method: 'monte_carlo',
                                    samples: 100,
                                    timeoutMs: 5000,
                                })];
                        case 3:
                            probResponse = _a.sent();
                            assert_1.default.strictEqual(clpResponse.status, 'ok');
                            assert_1.default.strictEqual(probResponse.status, 'ok');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should use logic modules with N3 reasoning', function () {
            return __awaiter(this, void 0, void 0, function () {
                var rules, response, queryResponse;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            rules = [
                                'semantic_relation(X, Y) :- rdf(X, related_to, Y)',
                                'inferred_fact(X) :- semantic_relation(X, _)',
                            ];
                            return [4 /*yield*/, backend.sendRequest('logic_module_register', {
                                    name: 'semantic_reasoning',
                                    rules: rules,
                                    meta_interpreter: 'default',
                                    timeoutMs: 10000,
                                })];
                        case 1:
                            response = _a.sent();
                            assert_1.default.strictEqual(response.status, 'ok');
                            return [4 /*yield*/, backend.sendRequest('logic_module_query', {
                                    module: 'semantic_reasoning',
                                    goal: 'inferred_fact(X)',
                                    timeoutMs: 5000,
                                })];
                        case 2:
                            queryResponse = _a.sent();
                            assert_1.default.strictEqual(queryResponse.status, 'ok');
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('Error Handling and Edge Cases', function () {
        it('should handle malformed CLP constraints', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('clp_solve', {
                                domain: 'fd',
                                variables: ['X'],
                                constraints: ['invalid constraint syntax'],
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
        it('should handle invalid probabilistic goals', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('probabilistic_query', {
                                goal: 'invalid goal syntax @#$',
                                method: 'monte_carlo',
                                samples: 100,
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
        it('should handle malformed logic module rules', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, backend.sendRequest('logic_module_register', {
                                name: 'malformed_test',
                                rules: ['invalid rule syntax @#$'],
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
        it('should handle missing required parameters', function () {
            return __awaiter(this, void 0, void 0, function () {
                var responses;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, Promise.all([
                                backend.sendRequest('clp_solve', { domain: 'fd' }), // Missing variables and constraints
                                backend.sendRequest('probabilistic_fact', { fact: 'test' }), // Missing probability
                                backend.sendRequest('logic_module_register', { name: 'test' }), // Missing rules
                            ])];
                        case 1:
                            responses = _a.sent();
                            responses.forEach(function (response) {
                                assert_1.default.strictEqual(response.status, 'error');
                            });
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
});
