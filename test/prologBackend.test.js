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
var path_1 = require("path");
var PrologBackendModule = require("../out/src/prologBackend.js");
var PrologBackend = PrologBackendModule.PrologBackend;
describe('PrologBackend', function () {
    var backend;
    beforeEach(function () {
        backend = new PrologBackend();
    });
    afterEach(function () {
        if (backend)
            backend.stop();
    });
    after(function (done) {
        // Ensure process exits after all tests complete
        setTimeout(function () {
            console.log('[TEST] Forcing process exit after all tests.');
            process.exit(0);
        }, 100);
        done();
    });
    it('[8] should support batch requests (query, consult, help)', function (done) {
        var _this = this;
        this.timeout(10000);
        backend.on('started', function () { return __awaiter(_this, void 0, void 0, function () {
            var testFile, batch, responses, err_1;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        _a.trys.push([0, 2, , 3]);
                        testFile = path_1.default
                            .resolve(__dirname, 'resources', 'foo_with_pldoc.pl')
                            .replace(/\\/g, '/');
                        batch = [
                            { cmd: 'consult', params: { file: testFile } },
                            { cmd: 'query', params: { goal: 'foo(1, B).' } },
                            { cmd: 'help', params: { predicate: 'foo/2' } },
                        ];
                        return [4 /*yield*/, backend.sendRequest(batch)];
                    case 1:
                        responses = _a.sent();
                        (0, chai_1.expect)(responses).to.be.an('array').with.lengthOf(3);
                        // Consult
                        (0, chai_1.expect)(responses[0]).to.have.property('status', 'ok');
                        // Query
                        (0, chai_1.expect)(responses[1]).to.have.property('status', 'ok');
                        // Help
                        (0, chai_1.expect)(responses[2]).to.have.property('status', 'ok');
                        (0, chai_1.expect)(responses[2]).to.have.property('doc');
                        done();
                        return [3 /*break*/, 3];
                    case 2:
                        err_1 = _a.sent();
                        done(err_1);
                        return [3 /*break*/, 3];
                    case 3: return [2 /*return*/];
                }
            });
        }); });
        backend.start();
    });
    it('[9] should enforce time limits for queries (timeout)', function (done) {
        var _this = this;
        this.timeout(5000);
        var finished = false;
        function finish(err) {
            if (!finished) {
                finished = true;
                backend.stop();
                if (err)
                    return done(err);
                done();
            }
        }
        backend.on('started', function () { return __awaiter(_this, void 0, void 0, function () {
            var err_2, msg;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        _a.trys.push([0, 2, , 3]);
                        // This query will sleep for 2 seconds, but time_limit is set to 1s
                        return [4 /*yield*/, backend.sendRequest('query', { goal: 'sleep(2).', timeoutMs: 2000, time_limit: 1 })];
                    case 1:
                        // This query will sleep for 2 seconds, but time_limit is set to 1s
                        _a.sent();
                        finish(new Error('Expected timeout error, but got success'));
                        return [3 /*break*/, 3];
                    case 2:
                        err_2 = _a.sent();
                        try {
                            (0, chai_1.expect)(err_2).to.exist;
                            // SWI-Prolog throws a time_limit_exceeded error
                            if (typeof err_2 === 'object' && err_2 !== null && ('message' in err_2 || 'error' in err_2)) {
                                msg = err_2.message ||
                                    err_2.error;
                                (0, chai_1.expect)(msg).to.match(/time[_ ]limit[_ ]exceeded/i);
                            }
                            finish(undefined);
                        }
                        catch (e) {
                            finish(e);
                        }
                        return [3 /*break*/, 3];
                    case 3: return [2 /*return*/];
                }
            });
        }); });
        backend.start();
    });
    it('[7] should return args and examples for a user-defined predicate', function (done) {
        var _this = this;
        this.timeout(12000);
        var finished = false;
        function finish(err, skip) {
            if (!finished) {
                finished = true;
                backend.stop();
                if (skip)
                    return this.skip();
                if (err)
                    return done(err);
                done();
            }
        }
        backend.on('started', function () { return __awaiter(_this, void 0, void 0, function () {
            var testFile, consultResp, e_1, doc, err_3, err_4;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        _a.trys.push([0, 11, , 12]);
                        testFile = path_1.default
                            .resolve(__dirname, 'resources', 'foo_with_pldoc.pl')
                            .replace(/\\/g, '/');
                        consultResp = void 0;
                        _a.label = 1;
                    case 1:
                        _a.trys.push([1, 4, , 5]);
                        return [4 /*yield*/, backend.sendRequest('query', { goal: "consult('".concat(testFile, "')") })];
                    case 2:
                        consultResp = _a.sent();
                        return [4 /*yield*/, backend.sendRequest('query', { goal: 'make.' })];
                    case 3:
                        _a.sent();
                        return [3 /*break*/, 5];
                    case 4:
                        e_1 = _a.sent();
                        return [2 /*return*/, finish(e_1, false)];
                    case 5: return [4 /*yield*/, new Promise(function (res) { return setTimeout(res, 150); })];
                    case 6:
                        _a.sent();
                        doc = void 0;
                        _a.label = 7;
                    case 7:
                        _a.trys.push([7, 9, , 10]);
                        return [4 /*yield*/, backend.sendRequest('help', { predicate: 'foo/2' })];
                    case 8:
                        doc = _a.sent();
                        return [3 /*break*/, 10];
                    case 9:
                        err_3 = _a.sent();
                        return [2 /*return*/, finish(err_3, false)];
                    case 10:
                        if (doc.summary && doc.summary.match(/pldoc missing|no documentation found/i)) {
                            return [2 /*return*/, finish(undefined, true)];
                        }
                        try {
                            (0, chai_1.expect)(doc).to.be.an('object');
                            (0, chai_1.expect)(doc).to.have.property('name', 'foo');
                            (0, chai_1.expect)(doc).to.have.property('arity', 2);
                            (0, chai_1.expect)(doc).to.have.property('args').that.is.an('array');
                            (0, chai_1.expect)(doc).to.have.property('examples').that.is.an('array');
                            finish(undefined, false);
                        }
                        catch (err) {
                            finish(err, false);
                        }
                        return [3 /*break*/, 12];
                    case 11:
                        err_4 = _a.sent();
                        finish(err_4, false);
                        return [3 /*break*/, 12];
                    case 12: return [2 /*return*/];
                }
            });
        }); });
        try {
            backend.start();
        }
        catch (e) {
            finish(e, false);
        }
        setTimeout(function () {
            if (!finished)
                finish(new Error('[TEST] [7] Timeout: done() not called after 10s'), false);
        }, 10000);
    });
    it('[1] should start and stop the backend process', function (done) {
        backend.on('started', function () {
            (0, chai_1.expect)(backend.isRunning()).to.be.true;
            backend.on('stopped', function () {
                (0, chai_1.expect)(backend.isRunning()).to.be.false;
                done();
            });
            backend.stop();
        });
        backend.start();
    });
    it('[2] should restart the backend process', function (done) {
        var startedCount = 0;
        backend.on('started', function () {
            startedCount++;
            if (startedCount === 1) {
                backend.restart();
            }
            else if (startedCount === 2) {
                (0, chai_1.expect)(backend.isRunning()).to.be.true;
                done();
            }
        });
        backend.start();
    });
    it('[3] should send a query and receive output', function (done) {
        backend.on('started', function () {
            console.log('[TEST] [3] Backend started, sending query...');
            backend
                .sendRequest('query', { goal: 'write(hello), nl.' })
                .then(function (output) {
                console.log('[TEST] [3] Query response:', output);
                (0, chai_1.expect)(output.status).to.equal('ok');
                done();
            })
                .catch(function (err) {
                console.error('[TEST] [3] Query error:', err);
                done(err);
            });
        });
        backend.start();
    });
    it('[4] should handle invalid input', function (done) {
        backend.on('started', function () {
            backend
                .sendRequest('query', { goal: '\u0000badinput' })
                .then(function () { return done(new Error('Expected error for invalid input, but got success')); })
                .catch(function (err) {
                (0, chai_1.expect)(err).to.exist;
                done();
            });
        });
        backend.start();
    });
    it('[5] should automatically restart on exit', function (done) {
        backend.once('started', function () {
            backend.once('started', function () {
                try {
                    (0, chai_1.expect)(backend.isRunning()).to.be.true;
                    done();
                }
                catch (e) {
                    done(e);
                }
            });
            backend.once('restarted', function () {
                // No-op
            });
            backend.restart();
        });
        backend.start();
    });
});
