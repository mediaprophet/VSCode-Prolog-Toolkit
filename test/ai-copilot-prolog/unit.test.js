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
var prologBackend_js_1 = require("../../out/src/prologBackend.js");
describe('AI Copilot Prolog Support - Unit', function () {
    var backend;
    beforeEach(function () {
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
    afterEach(function () {
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
    it('should sanitize and reject malformed input', function () {
        return __awaiter(this, void 0, void 0, function () {
            var err_1;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        _a.trys.push([0, 2, , 3]);
                        return [4 /*yield*/, backend.sendRequest('query', { goal: 'member(X, [a,b,c' })];
                    case 1:
                        _a.sent(); // missing closing ]
                        throw new Error('Should have thrown');
                    case 2:
                        err_1 = _a.sent();
                        (0, chai_1.expect)(err_1.message).to.match(/syntax|parse/i);
                        return [3 /*break*/, 3];
                    case 3: return [2 /*return*/];
                }
            });
        });
    });
    it('should enforce timeouts for long-running queries', function () {
        return __awaiter(this, void 0, void 0, function () {
            var err_2;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        this.timeout(3000);
                        _a.label = 1;
                    case 1:
                        _a.trys.push([1, 3, , 4]);
                        return [4 /*yield*/, backend.sendRequest('query', {
                                goal: 'repeat,repeat,repeat,repeat,repeat,repeat,repeat,repeat,repeat,repeat',
                                timeoutMs: 100,
                            })];
                    case 2:
                        _a.sent();
                        throw new Error('Should have timed out');
                    case 3:
                        err_2 = _a.sent();
                        (0, chai_1.expect)(err_2.message).to.match(/timeout/i);
                        return [3 /*break*/, 4];
                    case 4: return [2 /*return*/];
                }
            });
        });
    });
    it('should recover automatically if the Prolog process crashes', function () {
        return __awaiter(this, void 0, void 0, function () {
            var result;
            var _a;
            return __generator(this, function (_b) {
                switch (_b.label) {
                    case 0: 
                    // Simulate crash
                    return [4 /*yield*/, ((_a = backend['prologProcess']) === null || _a === void 0 ? void 0 : _a.kill())];
                    case 1:
                        // Simulate crash
                        _b.sent();
                        return [4 /*yield*/, backend.sendRequest('query', { goal: 'member(X, [1,2])' })];
                    case 2:
                        result = _b.sent();
                        (0, chai_1.expect)(result.success).to.be.true;
                        (0, chai_1.expect)(result.results[0]).to.have.property('X');
                        return [2 /*return*/];
                }
            });
        });
    });
    it('should queue and process multiple requests safely', function () {
        return __awaiter(this, void 0, void 0, function () {
            var promises, results;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        promises = [
                            backend.sendRequest('query', { goal: 'member(X, [a,b])' }),
                            backend.sendRequest('query', { goal: 'member(Y, [1,2])' }),
                        ];
                        return [4 /*yield*/, Promise.all(promises)];
                    case 1:
                        results = _a.sent();
                        (0, chai_1.expect)(results[0].success).to.be.true;
                        (0, chai_1.expect)(results[1].success).to.be.true;
                        return [2 /*return*/];
                }
            });
        });
    });
});
