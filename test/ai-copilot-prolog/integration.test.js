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
var axios_1 = require("axios");
var chai_1 = require("chai");
var prologBackend_js_1 = require("../../out/src/prologBackend.js");
describe('AI Copilot Prolog Support - Integration', function () {
    var backend;
    var baseUrl = 'http://localhost:9080';
    before(function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        backend = new prologBackend_js_1.PrologBackend();
                        return [4 /*yield*/, backend.start()];
                    case 1:
                        _a.sent();
                        return [2 /*return*/];
                }
            });
        });
    });
    after(function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, backend.stop()];
                    case 1:
                        _a.sent();
                        return [2 /*return*/];
                }
            });
        });
    });
    it('should execute a simple Prolog query and return variable bindings', function () {
        return __awaiter(this, void 0, void 0, function () {
            var response;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, axios_1.default.post("".concat(baseUrl, "/query"), {
                            goal: 'member(X, [a,b,c])',
                            options: { maxSolutions: 3 },
                        })];
                    case 1:
                        response = _a.sent();
                        (0, chai_1.expect)(response.data.success).to.be.true;
                        (0, chai_1.expect)(response.data.results).to.be.an('array').with.length(3);
                        (0, chai_1.expect)(response.data.results[0]).to.have.property('X');
                        return [2 /*return*/];
                }
            });
        });
    });
    it('should consult a Prolog file and query a predicate', function () {
        return __awaiter(this, void 0, void 0, function () {
            var consultResp, queryResp;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, axios_1.default.post("".concat(baseUrl, "/consult"), {
                            file: 'test/resources/simple.pl',
                        })];
                    case 1:
                        consultResp = _a.sent();
                        (0, chai_1.expect)(consultResp.data.success).to.be.true;
                        return [4 /*yield*/, axios_1.default.post("".concat(baseUrl, "/query"), {
                                goal: 'parent(john, X)',
                                options: { maxSolutions: 2 },
                            })];
                    case 2:
                        queryResp = _a.sent();
                        (0, chai_1.expect)(queryResp.data.success).to.be.true;
                        (0, chai_1.expect)(queryResp.data.results[0]).to.have.property('X');
                        return [2 /*return*/];
                }
            });
        });
    });
    it('should return documentation for a built-in predicate', function () {
        return __awaiter(this, void 0, void 0, function () {
            var response;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, axios_1.default.post("".concat(baseUrl, "/help"), {
                            predicate: 'member/2',
                        })];
                    case 1:
                        response = _a.sent();
                        (0, chai_1.expect)(response.data.success).to.be.true;
                        (0, chai_1.expect)(response.data.doc).to.include('member');
                        return [2 /*return*/];
                }
            });
        });
    });
    it('should load and reason over N3 data', function () {
        return __awaiter(this, void 0, void 0, function () {
            var n3Content, loadResp, reasonResp;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        n3Content = '@prefix : <http://example.org/> . :socrates a :Person . { ?x a :Person } => { ?x a :Mortal } .';
                        return [4 /*yield*/, axios_1.default.post("".concat(baseUrl, "/n3_load"), {
                                content: n3Content,
                            })];
                    case 1:
                        loadResp = _a.sent();
                        (0, chai_1.expect)(loadResp.data.success).to.be.true;
                        return [4 /*yield*/, axios_1.default.post("".concat(baseUrl, "/n3_reason"), {})];
                    case 2:
                        reasonResp = _a.sent();
                        (0, chai_1.expect)(reasonResp.data.success).to.be.true;
                        (0, chai_1.expect)(reasonResp.data.results).to.satisfy(function (arr) { return arr.some(function (t) { return t.includes('Mortal'); }); });
                        return [2 /*return*/];
                }
            });
        });
    });
    it('should handle errors and return structured error messages', function () {
        return __awaiter(this, void 0, void 0, function () {
            var response;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, axios_1.default.post("".concat(baseUrl, "/query"), {
                            goal: 'this_is_not_a_predicate(,)',
                            options: {},
                        })];
                    case 1:
                        response = _a.sent();
                        (0, chai_1.expect)(response.data.success).to.be.false;
                        (0, chai_1.expect)(response.data.error).to.have.property('message');
                        (0, chai_1.expect)(response.data.error).to.have.property('code');
                        return [2 /*return*/];
                }
            });
        });
    });
});
