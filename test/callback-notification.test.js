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
var ws_1 = require("ws");
var prologBackend_js_1 = require("../src/prologBackend.js");
describe('Callback and Notification Tests', function () {
    this.timeout(30000); // Increase timeout for notification tests
    var backend;
    var notificationManager;
    before(function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                // Initialize backend with notification support
                backend = new prologBackend_js_1.PrologBackend({
                    swiplPath: 'swipl',
                    port: 3063, // Use different port to avoid conflicts
                    notificationOptions: {
                        enableWebSocket: true,
                        webSocketPort: 3065,
                    },
                });
                notificationManager = backend.getNotificationManager();
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
    describe('Query Notification Manager', function () {
        it('should register and track queries', function () {
            var queryId = 'test-query-1';
            notificationManager.registerQuery(queryId);
            var status = notificationManager.getQueryStatus(queryId);
            (0, chai_1.expect)(status).to.exist;
            (0, chai_1.expect)(status.id).to.equal(queryId);
            (0, chai_1.expect)(status.status).to.equal('pending');
            (0, chai_1.expect)(status.startTime).to.be.a('number');
        });
        it('should update query status and progress', function () {
            var queryId = 'test-query-2';
            notificationManager.registerQuery(queryId);
            notificationManager.updateQueryProgress(queryId, 50, 'Processing...');
            var status = notificationManager.getQueryStatus(queryId);
            (0, chai_1.expect)(status.status).to.equal('running');
            (0, chai_1.expect)(status.progress).to.equal(50);
            (0, chai_1.expect)(status.message).to.equal('Processing...');
        });
        it('should complete queries successfully', function () {
            var queryId = 'test-query-3';
            var results = [{ X: 1 }, { X: 2 }, { X: 3 }];
            notificationManager.registerQuery(queryId);
            notificationManager.completeQuery(queryId, results);
            var status = notificationManager.getQueryStatus(queryId);
            (0, chai_1.expect)(status.status).to.equal('completed');
            (0, chai_1.expect)(status.progress).to.equal(100);
            (0, chai_1.expect)(status.results).to.deep.equal(results);
            (0, chai_1.expect)(status.endTime).to.be.a('number');
        });
        it('should handle query failures', function () {
            var queryId = 'test-query-4';
            var error = new Error('Test error');
            notificationManager.registerQuery(queryId);
            notificationManager.failQuery(queryId, error);
            var status = notificationManager.getQueryStatus(queryId);
            (0, chai_1.expect)(status.status).to.equal('error');
            (0, chai_1.expect)(status.error).to.equal(error);
            (0, chai_1.expect)(status.endTime).to.be.a('number');
        });
        it('should cancel running queries', function () {
            var queryId = 'test-query-5';
            notificationManager.registerQuery(queryId);
            notificationManager.updateQueryStatus(queryId, { status: 'running' });
            var cancelled = notificationManager.cancelQuery(queryId);
            (0, chai_1.expect)(cancelled).to.be.true;
            var status = notificationManager.getQueryStatus(queryId);
            (0, chai_1.expect)(status.status).to.equal('cancelled');
        });
        it('should not cancel already completed queries', function () {
            var queryId = 'test-query-6';
            notificationManager.registerQuery(queryId);
            notificationManager.completeQuery(queryId, []);
            var cancelled = notificationManager.cancelQuery(queryId);
            (0, chai_1.expect)(cancelled).to.be.false;
        });
        it('should track batch queries', function () {
            var queryId1 = 'batch-query-1';
            var queryId2 = 'batch-query-2';
            notificationManager.registerQuery(queryId1, undefined, true, 0, 2);
            notificationManager.registerQuery(queryId2, undefined, true, 1, 2);
            var status1 = notificationManager.getQueryStatus(queryId1);
            var status2 = notificationManager.getQueryStatus(queryId2);
            (0, chai_1.expect)(status1.isBatch).to.be.true;
            (0, chai_1.expect)(status1.batchIndex).to.equal(0);
            (0, chai_1.expect)(status1.totalBatchSize).to.equal(2);
            (0, chai_1.expect)(status2.isBatch).to.be.true;
            (0, chai_1.expect)(status2.batchIndex).to.equal(1);
            (0, chai_1.expect)(status2.totalBatchSize).to.equal(2);
        });
        it('should provide query statistics', function () {
            // Clean up previous queries
            notificationManager.cleanupCompletedQueries();
            // Create test queries
            notificationManager.registerQuery('stats-1');
            notificationManager.registerQuery('stats-2');
            notificationManager.updateQueryStatus('stats-2', { status: 'running' });
            notificationManager.completeQuery('stats-1', []);
            var stats = notificationManager.getStatistics();
            (0, chai_1.expect)(stats.total).to.be.at.least(2);
            (0, chai_1.expect)(stats.running).to.be.at.least(1);
            (0, chai_1.expect)(stats.completed).to.be.at.least(1);
        });
    });
    describe('Callback Support', function () {
        it('should call progress callbacks', function (done) {
            var queryId = 'callback-test-1';
            var progressCalled = false;
            var callback = {
                onProgress: function (status) {
                    (0, chai_1.expect)(status.id).to.equal(queryId);
                    (0, chai_1.expect)(status.status).to.equal('running');
                    progressCalled = true;
                },
                onComplete: function (status) {
                    (0, chai_1.expect)(progressCalled).to.be.true;
                    (0, chai_1.expect)(status.id).to.equal(queryId);
                    (0, chai_1.expect)(status.status).to.equal('completed');
                    done();
                },
            };
            notificationManager.registerQuery(queryId, callback);
            notificationManager.updateQueryProgress(queryId, 50);
            notificationManager.completeQuery(queryId, []);
        });
        it('should call error callbacks', function (done) {
            var queryId = 'callback-test-2';
            var testError = new Error('Test error');
            var callback = {
                onError: function (status) {
                    (0, chai_1.expect)(status.id).to.equal(queryId);
                    (0, chai_1.expect)(status.status).to.equal('error');
                    (0, chai_1.expect)(status.error).to.equal(testError);
                    done();
                },
            };
            notificationManager.registerQuery(queryId, callback);
            notificationManager.failQuery(queryId, testError);
        });
        it('should call cancel callbacks', function (done) {
            var queryId = 'callback-test-3';
            var callback = {
                onCancel: function (status) {
                    (0, chai_1.expect)(status.id).to.equal(queryId);
                    (0, chai_1.expect)(status.status).to.equal('cancelled');
                    done();
                },
            };
            notificationManager.registerQuery(queryId, callback);
            notificationManager.updateQueryStatus(queryId, { status: 'running' });
            notificationManager.cancelQuery(queryId);
        });
    });
    describe('WebSocket Notifications', function () {
        it('should connect to WebSocket server', function (done) {
            var ws = new ws_1.default('ws://localhost:3065');
            ws.on('open', function () {
                ws.close();
                done();
            });
            ws.on('error', function (error) {
                done(error);
            });
        });
        it('should receive query status updates via WebSocket', function (done) {
            var ws = new ws_1.default('ws://localhost:3065');
            var queryId = 'websocket-test-1';
            ws.on('open', function () {
                // Register query after WebSocket connection
                notificationManager.registerQuery(queryId);
                notificationManager.updateQueryProgress(queryId, 75, 'Almost done...');
            });
            ws.on('message', function (data) {
                var message = JSON.parse(data.toString());
                if (message.type === 'query_status_updated' && message.query.query_id === queryId) {
                    (0, chai_1.expect)(message.query.status).to.equal('running');
                    (0, chai_1.expect)(message.query.progress).to.equal(75);
                    ws.close();
                    done();
                }
            });
            ws.on('error', function (error) {
                done(error);
            });
        });
        it('should handle query cancellation via WebSocket', function (done) {
            var ws = new ws_1.default('ws://localhost:3065');
            var queryId = 'websocket-cancel-test';
            ws.on('open', function () {
                // Register and start a query
                notificationManager.registerQuery(queryId);
                notificationManager.updateQueryStatus(queryId, { status: 'running' });
                // Send cancellation request
                ws.send(JSON.stringify({
                    type: 'cancel_query',
                    queryId: queryId,
                }));
            });
            ws.on('message', function (data) {
                var message = JSON.parse(data.toString());
                if (message.type === 'query_cancelled' && message.query_id === queryId) {
                    ws.close();
                    done();
                }
            });
            ws.on('error', function (error) {
                done(error);
            });
        });
    });
    describe('Backend Integration', function () {
        it('should support sendRequestWithNotifications', function () {
            return __awaiter(this, void 0, void 0, function () {
                var progressReceived, completedReceived, callback, response;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            progressReceived = false;
                            completedReceived = false;
                            callback = {
                                onProgress: function (status) {
                                    progressReceived = true;
                                    (0, chai_1.expect)(status.status).to.equal('running');
                                },
                                onComplete: function (status) {
                                    completedReceived = true;
                                    (0, chai_1.expect)(status.status).to.equal('completed');
                                },
                            };
                            return [4 /*yield*/, backend.sendRequestWithNotifications('query', {
                                    goal: 'member(X, [1,2,3])',
                                    timeoutMs: 5000,
                                }, callback)];
                        case 1:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('ok');
                            (0, chai_1.expect)(progressReceived).to.be.true;
                            (0, chai_1.expect)(completedReceived).to.be.true;
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should provide query statistics from backend', function () {
            var stats = backend.getQueryStatistics();
            (0, chai_1.expect)(stats).to.have.property('total');
            (0, chai_1.expect)(stats).to.have.property('running');
            (0, chai_1.expect)(stats).to.have.property('completed');
            (0, chai_1.expect)(stats).to.have.property('error');
            (0, chai_1.expect)(stats).to.have.property('cancelled');
        });
        it('should list active queries', function () {
            var activeQueries = backend.getActiveQueries();
            (0, chai_1.expect)(activeQueries).to.be.an('array');
        });
        it('should cancel queries through backend', function () {
            return __awaiter(this, void 0, void 0, function () {
                var queryPromise, error_1;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            queryPromise = backend.sendRequestWithNotifications('query', {
                                goal: 'sleep(10), member(X, [1,2,3])', // 10 second sleep
                                timeoutMs: 15000,
                            });
                            // Wait a bit then cancel
                            setTimeout(function () {
                                var activeQueries = backend.getActiveQueries();
                                if (activeQueries.length > 0) {
                                    var cancelled = backend.cancelQuery(activeQueries[0].id);
                                    (0, chai_1.expect)(cancelled).to.be.true;
                                }
                            }, 1000);
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 3, , 4]);
                            return [4 /*yield*/, queryPromise];
                        case 2:
                            _a.sent();
                            return [3 /*break*/, 4];
                        case 3:
                            error_1 = _a.sent();
                            // Query might be cancelled or timeout, both are acceptable
                            (0, chai_1.expect)(error_1).to.exist;
                            return [3 /*break*/, 4];
                        case 4: return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('Event Emission', function () {
        it('should emit query events', function (done) {
            var queryId = 'event-test-1';
            var eventsReceived = 0;
            notificationManager.on('queryRegistered', function (status) {
                (0, chai_1.expect)(status.id).to.equal(queryId);
                eventsReceived++;
            });
            notificationManager.on('queryStatusUpdated', function (status) {
                (0, chai_1.expect)(status.id).to.equal(queryId);
                eventsReceived++;
            });
            notificationManager.on('query_completed', function (status) {
                (0, chai_1.expect)(status.id).to.equal(queryId);
                (0, chai_1.expect)(status.status).to.equal('completed');
                eventsReceived++;
                // Check that all events were received
                (0, chai_1.expect)(eventsReceived).to.be.at.least(3);
                done();
            });
            notificationManager.registerQuery(queryId);
            notificationManager.updateQueryProgress(queryId, 50);
            notificationManager.completeQuery(queryId, []);
        });
    });
});
