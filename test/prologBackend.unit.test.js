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
var sinon_1 = require("sinon");
var sinon_chai_1 = require("sinon-chai");
var prologBackend_js_1 = require("../src/prologBackend.js");
chai_1.default.use(sinon_chai_1.default);
describe('PrologBackend Unit Tests (Mocked)', function () {
    var backend;
    var mockChildProcess;
    var axiosStub;
    beforeEach(function () {
        // Create a mock child process
        mockChildProcess = {
            pid: 12345,
            kill: sinon_1.default.stub(),
            on: sinon_1.default.stub(),
            stdout: {
                on: sinon_1.default.stub(),
                pipe: sinon_1.default.stub(),
            },
            stderr: {
                on: sinon_1.default.stub(),
                pipe: sinon_1.default.stub(),
            },
            stdin: {
                write: sinon_1.default.stub(),
                end: sinon_1.default.stub(),
            },
        };
        // Stub axios for HTTP requests
        axiosStub = sinon_1.default.stub(axios_1.default, 'request');
        // Create backend instance
        backend = new prologBackend_js_1.PrologBackend({
            swiplPath: 'swipl',
            port: 3063,
        });
        // Replace the spawn method to return our mock
        sinon_1.default.stub(require('child_process'), 'spawn').returns(mockChildProcess);
    });
    afterEach(function () {
        sinon_1.default.restore();
        if (backend) {
            backend.stop(true);
        }
    });
    describe('Backend Lifecycle', function () {
        it('should initialize with default configuration', function () {
            var defaultBackend = new prologBackend_js_1.PrologBackend();
            (0, chai_1.expect)(defaultBackend).to.be.instanceOf(prologBackend_js_1.PrologBackend);
        });
        it('should initialize with custom configuration', function () {
            var config = {
                swiplPath: '/custom/path/swipl',
                port: 9999,
                args: ['--quiet'],
            };
            var customBackend = new prologBackend_js_1.PrologBackend(config);
            (0, chai_1.expect)(customBackend).to.be.instanceOf(prologBackend_js_1.PrologBackend);
        });
        it('should start the backend process', function (done) {
            backend.on('started', function () {
                (0, chai_1.expect)(mockChildProcess.on).to.have.been.called;
                done();
            });
            backend.start();
            // Simulate process started
            var startedCallback = mockChildProcess.on.getCall(0).args[1];
            startedCallback();
        });
        it('should handle process exit and restart', function (done) {
            var restartCount = 0;
            backend.on('started', function () {
                restartCount++;
                if (restartCount === 2) {
                    done(); // Second start means restart worked
                }
            });
            backend.start();
            // Simulate first start
            var startedCallback = mockChildProcess.on.getCall(0).args[1];
            startedCallback();
            // Simulate process exit
            var exitCallback = mockChildProcess.on.getCall(1).args[1];
            exitCallback(1); // Exit with error code
        });
        it('should stop the backend process', function (done) {
            backend.on('stopped', function () {
                (0, chai_1.expect)(mockChildProcess.kill).to.have.been.called;
                done();
            });
            backend.start();
            // Simulate started
            var startedCallback = mockChildProcess.on.getCall(0).args[1];
            startedCallback();
            backend.stop();
        });
    });
    describe('Request Handling', function () {
        beforeEach(function () {
            // Setup successful HTTP response mock
            axiosStub.resolves({
                data: {
                    status: 'ok',
                    results: [{ X: 5 }],
                },
            });
        });
        it('should send HTTP requests to Prolog server', function () {
            return __awaiter(this, void 0, void 0, function () {
                var startedCallback, response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            backend.start();
                            startedCallback = mockChildProcess.on.getCall(0).args[1];
                            startedCallback();
                            return [4 /*yield*/, backend.sendRequest('query', {
                                    goal: 'X is 2 + 3',
                                })];
                        case 1:
                            response = _a.sent();
                            (0, chai_1.expect)(axiosStub).to.have.been.calledOnce;
                            (0, chai_1.expect)(response.status).to.equal('ok');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle HTTP request failures', function () {
            return __awaiter(this, void 0, void 0, function () {
                var startedCallback, error_1;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            axiosStub.rejects(new Error('Connection refused'));
                            backend.start();
                            startedCallback = mockChildProcess.on.getCall(0).args[1];
                            startedCallback();
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 3, , 4]);
                            return [4 /*yield*/, backend.sendRequest('query', { goal: 'test' })];
                        case 2:
                            _a.sent();
                            chai_1.expect.fail('Should have thrown an error');
                            return [3 /*break*/, 4];
                        case 3:
                            error_1 = _a.sent();
                            (0, chai_1.expect)(error_1.message).to.include('Connection refused');
                            return [3 /*break*/, 4];
                        case 4: return [2 /*return*/];
                    }
                });
            });
        });
        it('should validate request parameters', function () {
            return __awaiter(this, void 0, void 0, function () {
                var startedCallback, error_2;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            backend.start();
                            startedCallback = mockChildProcess.on.getCall(0).args[1];
                            startedCallback();
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 3, , 4]);
                            return [4 /*yield*/, backend.sendRequest('query', {})];
                        case 2:
                            _a.sent();
                            chai_1.expect.fail('Should have thrown validation error');
                            return [3 /*break*/, 4];
                        case 3:
                            error_2 = _a.sent();
                            (0, chai_1.expect)(error_2).to.exist;
                            return [3 /*break*/, 4];
                        case 4: return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle batch requests', function () {
            return __awaiter(this, void 0, void 0, function () {
                var startedCallback, batch, responses;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            axiosStub.resolves({
                                data: [
                                    { status: 'ok', results: [] },
                                    { status: 'ok', results: [{ X: 5 }] },
                                ],
                            });
                            backend.start();
                            startedCallback = mockChildProcess.on.getCall(0).args[1];
                            startedCallback();
                            batch = [
                                { cmd: 'consult', params: { file: 'test.pl' } },
                                { cmd: 'query', params: { goal: 'X is 2 + 3' } },
                            ];
                            return [4 /*yield*/, backend.sendRequest(batch)];
                        case 1:
                            responses = _a.sent();
                            (0, chai_1.expect)(responses).to.be.an('array').with.lengthOf(2);
                            (0, chai_1.expect)(axiosStub).to.have.been.calledOnce;
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('Input Validation and Security', function () {
        beforeEach(function () {
            backend.start();
            // Simulate backend ready
            var startedCallback = mockChildProcess.on.getCall(0).args[1];
            startedCallback();
        });
        it('should sanitize dangerous input characters', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            axiosStub.resolves({ data: { status: 'error', error: 'Invalid input' } });
                            return [4 /*yield*/, backend.sendRequest('query', {
                                    goal: 'test\u0000dangerous',
                                })];
                        case 1:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('error');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should validate command types', function () {
            return __awaiter(this, void 0, void 0, function () {
                var error_3;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            _a.trys.push([0, 2, , 3]);
                            return [4 /*yield*/, backend.sendRequest('invalid_command', {})];
                        case 1:
                            _a.sent();
                            chai_1.expect.fail('Should have thrown validation error');
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
        it('should enforce timeout limits', function () {
            return __awaiter(this, void 0, void 0, function () {
                var error_4;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            // Mock a delayed response
                            axiosStub.returns(new Promise(function (resolve) {
                                setTimeout(function () { return resolve({ data: { status: 'ok' } }); }, 10000);
                            }));
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 3, , 4]);
                            return [4 /*yield*/, backend.sendRequest('query', {
                                    goal: 'test',
                                    timeoutMs: 1000,
                                })];
                        case 2:
                            _a.sent();
                            chai_1.expect.fail('Should have timed out');
                            return [3 /*break*/, 4];
                        case 3:
                            error_4 = _a.sent();
                            (0, chai_1.expect)(error_4.message).to.include('timeout');
                            return [3 /*break*/, 4];
                        case 4: return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('Error Recovery', function () {
        it('should recover from process crashes', function (done) {
            var crashCount = 0;
            backend.on('error', function () {
                crashCount++;
            });
            backend.on('restarted', function () {
                (0, chai_1.expect)(crashCount).to.be.greaterThan(0);
                done();
            });
            backend.start();
            // Simulate process crash
            var errorCallback = mockChildProcess.on.getCall(2).args[1];
            errorCallback(new Error('Process crashed'));
        });
        it('should handle multiple rapid restart attempts', function (done) {
            var restartCount = 0;
            backend.on('started', function () {
                restartCount++;
                if (restartCount >= 3) {
                    done();
                }
            });
            backend.start();
            var _loop_1 = function (i) {
                setTimeout(function () {
                    var startedCallback = mockChildProcess.on.getCall(0).args[1];
                    startedCallback();
                    if (i < 2) {
                        var exitCallback = mockChildProcess.on.getCall(1).args[1];
                        exitCallback(1);
                    }
                }, i * 100);
            };
            // Simulate multiple rapid crashes
            for (var i = 0; i < 3; i++) {
                _loop_1(i);
            }
        });
    });
    describe('Health Checks', function () {
        beforeEach(function () {
            backend.start();
            // Simulate backend ready
            var startedCallback = mockChildProcess.on.getCall(0).args[1];
            startedCallback();
        });
        it('should perform health checks', function () {
            return __awaiter(this, void 0, void 0, function () {
                var health;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            axiosStub.resolves({
                                data: {
                                    status: 'ok',
                                    version: 1,
                                    uptime: 1000,
                                },
                            });
                            return [4 /*yield*/, backend.sendRequest('status', {})];
                        case 1:
                            health = _a.sent();
                            (0, chai_1.expect)(health.status).to.equal('ok');
                            (0, chai_1.expect)(health.version).to.be.a('number');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should detect unhealthy backend', function () {
            return __awaiter(this, void 0, void 0, function () {
                var error_5;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            axiosStub.rejects(new Error('Health check failed'));
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 3, , 4]);
                            return [4 /*yield*/, backend.sendRequest('status', {})];
                        case 2:
                            _a.sent();
                            chai_1.expect.fail('Should have detected unhealthy backend');
                            return [3 /*break*/, 4];
                        case 3:
                            error_5 = _a.sent();
                            (0, chai_1.expect)(error_5.message).to.include('Health check failed');
                            return [3 /*break*/, 4];
                        case 4: return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('Configuration Management', function () {
        it('should use default configuration values', function () {
            var defaultBackend = new prologBackend_js_1.PrologBackend();
            (0, chai_1.expect)(defaultBackend).to.be.instanceOf(prologBackend_js_1.PrologBackend);
        });
        it('should override default configuration', function () {
            var config = {
                swiplPath: '/custom/swipl',
                port: 8888,
                args: ['--custom-arg'],
            };
            var customBackend = new prologBackend_js_1.PrologBackend(config);
            (0, chai_1.expect)(customBackend).to.be.instanceOf(prologBackend_js_1.PrologBackend);
        });
        it('should validate configuration parameters', function () {
            (0, chai_1.expect)(function () {
                new prologBackend_js_1.PrologBackend({ port: -1 });
            }).to.throw();
            (0, chai_1.expect)(function () {
                new prologBackend_js_1.PrologBackend({ port: 70000 });
            }).to.throw();
        });
    });
    describe('Event Handling', function () {
        it('should emit lifecycle events', function (done) {
            var events = [];
            backend.on('starting', function () { return events.push('starting'); });
            backend.on('started', function () { return events.push('started'); });
            backend.on('ready', function () {
                events.push('ready');
                (0, chai_1.expect)(events).to.include.members(['starting', 'started', 'ready']);
                done();
            });
            backend.start();
            // Simulate event sequence
            setTimeout(function () {
                var startedCallback = mockChildProcess.on.getCall(0).args[1];
                startedCallback();
            }, 10);
        });
        it('should emit error events', function (done) {
            backend.on('error', function (error) {
                (0, chai_1.expect)(error).to.be.instanceOf(Error);
                done();
            });
            backend.start();
            // Simulate error
            var errorCallback = mockChildProcess.on.getCall(2).args[1];
            errorCallback(new Error('Test error'));
        });
    });
    describe('Resource Management', function () {
        it('should clean up resources on stop', function () {
            backend.start();
            backend.stop(true);
            (0, chai_1.expect)(mockChildProcess.kill).to.have.been.called;
        });
        it('should handle graceful shutdown', function (done) {
            backend.on('stopped', function () {
                done();
            });
            backend.start();
            // Simulate started
            var startedCallback = mockChildProcess.on.getCall(0).args[1];
            startedCallback();
            backend.stop();
        });
        it('should handle forced shutdown', function () {
            backend.start();
            backend.stop(true);
            (0, chai_1.expect)(mockChildProcess.kill).to.have.been.calledWith('SIGKILL');
        });
    });
});
