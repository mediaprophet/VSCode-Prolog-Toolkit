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
var prologBackend_js_1 = require("../src/prologBackend.js");
describe('Chat Integration Tests', function () {
    this.timeout(20000); // Increase timeout for backend startup
    var backend;
    before(function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                // Initialize backend for testing
                backend = new prologBackend_js_1.PrologBackend({
                    swiplPath: 'swipl',
                    port: 3061, // Use different port to avoid conflicts
                });
                // Start backend and wait for it to be ready
                return [2 /*return*/, new Promise(function (resolve, reject) {
                        var timeout = setTimeout(function () {
                            reject(new Error('Backend startup timeout'));
                        }, 15000);
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
    it('should handle version command', function () {
        return __awaiter(this, void 0, void 0, function () {
            var response;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, backend.sendRequest('version')];
                    case 1:
                        response = _a.sent();
                        (0, chai_1.expect)(response.status).to.equal('ok');
                        (0, chai_1.expect)(response.version).to.be.a('number');
                        return [2 /*return*/];
                }
            });
        });
    });
    it('should handle simple query command', function () {
        return __awaiter(this, void 0, void 0, function () {
            var response;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, backend.sendRequest('query', {
                            goal: 'member(X, [1,2,3])',
                            timeoutMs: 5000,
                        })];
                    case 1:
                        response = _a.sent();
                        (0, chai_1.expect)(response.status).to.equal('ok');
                        (0, chai_1.expect)(response.results).to.be.an('array');
                        return [2 /*return*/];
                }
            });
        });
    });
    it('should handle help command', function () {
        return __awaiter(this, void 0, void 0, function () {
            var response;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, backend.sendRequest('help', {
                            predicate: 'member/2',
                            timeoutMs: 5000,
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
    it('should handle consult command with error gracefully', function () {
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
                        // Should return error status for non-existent file
                        (0, chai_1.expect)(response.status).to.equal('error');
                        return [2 /*return*/];
                }
            });
        });
    });
    it('should handle invalid query gracefully', function () {
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
                        return [2 /*return*/];
                }
            });
        });
    });
});
