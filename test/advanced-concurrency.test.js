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
var concurrencyManager_js_1 = require("../src/features/concurrencyManager.js");
var queryHistoryManager_js_1 = require("../src/features/queryHistoryManager.js");
var queryScheduler_js_1 = require("../src/features/queryScheduler.js");
describe('Advanced Concurrency Features', function () {
    var concurrencyManager;
    var historyManager;
    var queryScheduler;
    beforeEach(function () {
        var resourceQuota = {
            maxConcurrentQueries: 3,
            maxMemoryUsageMB: 256,
            maxCpuUsagePercent: 70,
            maxQueryDurationMs: 5000,
            maxQueueSize: 10,
        };
        concurrencyManager = new concurrencyManager_js_1.ConcurrencyManager(resourceQuota);
        historyManager = new queryHistoryManager_js_1.QueryHistoryManager({
            storageDir: './test-history',
            maxHistorySize: 100,
            retentionDays: 1,
        });
        queryScheduler = new queryScheduler_js_1.QueryScheduler(concurrencyManager, historyManager);
    });
    afterEach(function () {
        concurrencyManager.dispose();
        historyManager.dispose();
        queryScheduler.dispose();
    });
    describe('ConcurrencyManager', function () {
        it('should queue queries with different priorities', function () { return __awaiter(void 0, void 0, void 0, function () {
            var highPriorityQuery, normalPriorityQuery, lowPriorityQuery, status;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        highPriorityQuery = concurrencyManager.queueQuery('high-1', 'test_query', {}, { level: 'high', weight: 100, timeout: 5000 });
                        normalPriorityQuery = concurrencyManager.queueQuery('normal-1', 'test_query', {}, { level: 'normal', weight: 10, timeout: 5000 });
                        lowPriorityQuery = concurrencyManager.queueQuery('low-1', 'test_query', {}, { level: 'low', weight: 1, timeout: 5000 });
                        // Wait a bit for queries to be queued
                        return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 100); })];
                    case 1:
                        // Wait a bit for queries to be queued
                        _a.sent();
                        status = concurrencyManager.getStatus();
                        (0, chai_1.expect)(status.queuedQueries).to.have.length(3);
                        // High priority should be first
                        (0, chai_1.expect)(status.queuedQueries[0].priority).to.equal('high');
                        (0, chai_1.expect)(status.queuedQueries[1].priority).to.equal('normal');
                        (0, chai_1.expect)(status.queuedQueries[2].priority).to.equal('low');
                        return [2 /*return*/];
                }
            });
        }); });
        it('should respect resource quotas', function () { return __awaiter(void 0, void 0, void 0, function () {
            var queries, i, status;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        queries = [];
                        for (i = 0; i < 5; i++) {
                            queries.push(concurrencyManager.queueQuery("query-".concat(i), 'test_query'));
                        }
                        return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 100); })];
                    case 1:
                        _a.sent();
                        status = concurrencyManager.getStatus();
                        (0, chai_1.expect)(status.resourceUsage.activeConcurrentQueries).to.be.at.most(3);
                        (0, chai_1.expect)(status.queuedQueries.length).to.be.greaterThan(0);
                        return [2 /*return*/];
                }
            });
        }); });
        it('should cancel queued queries', function () { return __awaiter(void 0, void 0, void 0, function () {
            var queryPromise, cancelled, error_1;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        queryPromise = concurrencyManager.queueQuery('cancel-test', 'test_query');
                        return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 50); })];
                    case 1:
                        _a.sent();
                        cancelled = concurrencyManager.cancelQuery('cancel-test');
                        (0, chai_1.expect)(cancelled).to.be.true;
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 4, , 5]);
                        return [4 /*yield*/, queryPromise];
                    case 3:
                        _a.sent();
                        chai_1.expect.fail('Query should have been cancelled');
                        return [3 /*break*/, 5];
                    case 4:
                        error_1 = _a.sent();
                        (0, chai_1.expect)(error_1.message).to.include('cancelled');
                        return [3 /*break*/, 5];
                    case 5: return [2 /*return*/];
                }
            });
        }); });
        it('should update resource quotas', function () {
            var newQuota = {
                maxConcurrentQueries: 5,
                maxMemoryUsageMB: 512,
            };
            concurrencyManager.updateResourceQuota(newQuota);
            var status = concurrencyManager.getStatus();
            (0, chai_1.expect)(status.resourceQuota.maxConcurrentQueries).to.equal(5);
            (0, chai_1.expect)(status.resourceQuota.maxMemoryUsageMB).to.equal(512);
        });
        it('should provide resource usage statistics', function () {
            var status = concurrencyManager.getStatus();
            (0, chai_1.expect)(status.resourceUsage).to.have.property('activeConcurrentQueries');
            (0, chai_1.expect)(status.resourceUsage).to.have.property('memoryUsageMB');
            (0, chai_1.expect)(status.resourceUsage).to.have.property('cpuUsagePercent');
            (0, chai_1.expect)(status.resourceUsage).to.have.property('queueSize');
            (0, chai_1.expect)(status.resourceUsage).to.have.property('lastUpdated');
        });
    });
    describe('QueryHistoryManager', function () {
        it('should add and retrieve query history', function () { return __awaiter(void 0, void 0, void 0, function () {
            var queryEntry, history;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        queryEntry = {
                            id: 'test-query-1',
                            cmd: 'test_predicate',
                            params: { arg1: 'value1' },
                            status: 'completed',
                            startTime: Date.now() - 1000,
                            endTime: Date.now(),
                            results: { success: true },
                        };
                        return [4 /*yield*/, historyManager.addQuery(queryEntry)];
                    case 1:
                        _a.sent();
                        return [4 /*yield*/, historyManager.getHistory()];
                    case 2:
                        history = _a.sent();
                        (0, chai_1.expect)(history.entries).to.have.length(1);
                        (0, chai_1.expect)(history.entries[0].id).to.equal('test-query-1');
                        (0, chai_1.expect)(history.entries[0].duration).to.be.a('number');
                        return [2 /*return*/];
                }
            });
        }); });
        it('should filter query history', function () { return __awaiter(void 0, void 0, void 0, function () {
            var queries, _i, queries_1, query, completedHistory, errorHistory;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        queries = [
                            {
                                id: 'completed-1',
                                cmd: 'test_predicate',
                                params: {},
                                status: 'completed',
                                startTime: Date.now() - 2000,
                                endTime: Date.now() - 1000,
                            },
                            {
                                id: 'error-1',
                                cmd: 'test_predicate',
                                params: {},
                                status: 'error',
                                startTime: Date.now() - 1000,
                                endTime: Date.now(),
                                error: 'Test error',
                            },
                        ];
                        _i = 0, queries_1 = queries;
                        _a.label = 1;
                    case 1:
                        if (!(_i < queries_1.length)) return [3 /*break*/, 4];
                        query = queries_1[_i];
                        return [4 /*yield*/, historyManager.addQuery(query)];
                    case 2:
                        _a.sent();
                        _a.label = 3;
                    case 3:
                        _i++;
                        return [3 /*break*/, 1];
                    case 4: return [4 /*yield*/, historyManager.getHistory({
                            status: ['completed'],
                        })];
                    case 5:
                        completedHistory = _a.sent();
                        (0, chai_1.expect)(completedHistory.entries).to.have.length(1);
                        (0, chai_1.expect)(completedHistory.entries[0].status).to.equal('completed');
                        return [4 /*yield*/, historyManager.getHistory({
                                status: ['error'],
                            })];
                    case 6:
                        errorHistory = _a.sent();
                        (0, chai_1.expect)(errorHistory.entries).to.have.length(1);
                        (0, chai_1.expect)(errorHistory.entries[0].status).to.equal('error');
                        return [2 /*return*/];
                }
            });
        }); });
        it('should provide comprehensive statistics', function () { return __awaiter(void 0, void 0, void 0, function () {
            var queries, _i, queries_2, query, stats;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        queries = [
                            {
                                id: 'stat-1',
                                cmd: 'test_predicate',
                                params: {},
                                status: 'completed',
                                startTime: Date.now() - 2000,
                                endTime: Date.now() - 1500,
                                priority: 'high',
                            },
                            {
                                id: 'stat-2',
                                cmd: 'another_predicate',
                                params: {},
                                status: 'error',
                                startTime: Date.now() - 1000,
                                endTime: Date.now() - 500,
                                priority: 'normal',
                            },
                        ];
                        _i = 0, queries_2 = queries;
                        _a.label = 1;
                    case 1:
                        if (!(_i < queries_2.length)) return [3 /*break*/, 4];
                        query = queries_2[_i];
                        return [4 /*yield*/, historyManager.addQuery(query)];
                    case 2:
                        _a.sent();
                        _a.label = 3;
                    case 3:
                        _i++;
                        return [3 /*break*/, 1];
                    case 4: return [4 /*yield*/, historyManager.getStatistics()];
                    case 5:
                        stats = _a.sent();
                        (0, chai_1.expect)(stats.totalQueries).to.equal(2);
                        (0, chai_1.expect)(stats.completedQueries).to.equal(1);
                        (0, chai_1.expect)(stats.errorQueries).to.equal(1);
                        (0, chai_1.expect)(stats.queriesByStatus).to.have.property('completed', 1);
                        (0, chai_1.expect)(stats.queriesByStatus).to.have.property('error', 1);
                        (0, chai_1.expect)(stats.queriesByPriority).to.have.property('high', 1);
                        (0, chai_1.expect)(stats.queriesByCmd).to.have.property('test_predicate', 1);
                        (0, chai_1.expect)(stats.dailyStats).to.be.an('array');
                        return [2 /*return*/];
                }
            });
        }); });
        it('should update existing queries', function () { return __awaiter(void 0, void 0, void 0, function () {
            var queryEntry, query;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        queryEntry = {
                            id: 'update-test',
                            cmd: 'test_predicate',
                            params: {},
                            status: 'running',
                            startTime: Date.now(),
                        };
                        return [4 /*yield*/, historyManager.addQuery(queryEntry)];
                    case 1:
                        _a.sent();
                        // Update the query
                        return [4 /*yield*/, historyManager.updateQuery('update-test', {
                                status: 'completed',
                                endTime: Date.now(),
                                results: { success: true },
                            })];
                    case 2:
                        // Update the query
                        _a.sent();
                        return [4 /*yield*/, historyManager.getQuery('update-test')];
                    case 3:
                        query = _a.sent();
                        (0, chai_1.expect)(query === null || query === void 0 ? void 0 : query.status).to.equal('completed');
                        (0, chai_1.expect)(query === null || query === void 0 ? void 0 : query.results).to.deep.equal({ success: true });
                        (0, chai_1.expect)(query === null || query === void 0 ? void 0 : query.duration).to.be.a('number');
                        return [2 /*return*/];
                }
            });
        }); });
        it('should delete queries', function () { return __awaiter(void 0, void 0, void 0, function () {
            var queryEntry, query, deleted;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        queryEntry = {
                            id: 'delete-test',
                            cmd: 'test_predicate',
                            params: {},
                            status: 'completed',
                            startTime: Date.now() - 1000,
                            endTime: Date.now(),
                        };
                        return [4 /*yield*/, historyManager.addQuery(queryEntry)];
                    case 1:
                        _a.sent();
                        return [4 /*yield*/, historyManager.getQuery('delete-test')];
                    case 2:
                        query = _a.sent();
                        (0, chai_1.expect)(query).to.not.be.undefined;
                        return [4 /*yield*/, historyManager.deleteQuery('delete-test')];
                    case 3:
                        deleted = _a.sent();
                        (0, chai_1.expect)(deleted).to.be.true;
                        return [4 /*yield*/, historyManager.getQuery('delete-test')];
                    case 4:
                        query = _a.sent();
                        (0, chai_1.expect)(query).to.be.undefined;
                        return [2 /*return*/];
                }
            });
        }); });
    });
    describe('QueryScheduler', function () {
        it('should schedule immediate queries', function () { return __awaiter(void 0, void 0, void 0, function () {
            var executionStarted, executionCompleted;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        executionStarted = false;
                        executionCompleted = false;
                        queryScheduler.on('queryScheduleExecutionStarted', function () {
                            executionStarted = true;
                        });
                        queryScheduler.on('queryScheduleCompleted', function () {
                            executionCompleted = true;
                        });
                        // Mock the concurrency manager execution
                        concurrencyManager.on('executeQuery', function (event) {
                            setTimeout(function () {
                                event.resolve({ success: true });
                            }, 100);
                        });
                        return [4 /*yield*/, queryScheduler.scheduleQuery('immediate-test', 'test_predicate', {}, 'immediate')];
                    case 1:
                        _a.sent();
                        // Wait for execution
                        return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 200); })];
                    case 2:
                        // Wait for execution
                        _a.sent();
                        (0, chai_1.expect)(executionStarted).to.be.true;
                        (0, chai_1.expect)(executionCompleted).to.be.true;
                        return [2 /*return*/];
                }
            });
        }); });
        it('should schedule delayed queries', function () { return __awaiter(void 0, void 0, void 0, function () {
            var executeAt, executionStarted;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        executeAt = Date.now() + 500;
                        executionStarted = false;
                        queryScheduler.on('queryScheduleExecutionStarted', function () {
                            executionStarted = true;
                        });
                        // Mock the concurrency manager execution
                        concurrencyManager.on('executeQuery', function (event) {
                            event.resolve({ success: true });
                        });
                        return [4 /*yield*/, queryScheduler.scheduleQuery('delayed-test', 'test_predicate', {}, 'delayed', {
                                executeAt: executeAt,
                            })];
                    case 1:
                        _a.sent();
                        // Should not execute immediately
                        return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 200); })];
                    case 2:
                        // Should not execute immediately
                        _a.sent();
                        (0, chai_1.expect)(executionStarted).to.be.false;
                        // Should execute after delay
                        return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 400); })];
                    case 3:
                        // Should execute after delay
                        _a.sent();
                        (0, chai_1.expect)(executionStarted).to.be.true;
                        return [2 /*return*/];
                }
            });
        }); });
        it('should handle recurring queries', function () { return __awaiter(void 0, void 0, void 0, function () {
            var executionCount;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        executionCount = 0;
                        queryScheduler.on('queryScheduleExecutionStarted', function () {
                            executionCount++;
                        });
                        // Mock the concurrency manager execution
                        concurrencyManager.on('executeQuery', function (event) {
                            setTimeout(function () {
                                event.resolve({ success: true });
                            }, 50);
                        });
                        return [4 /*yield*/, queryScheduler.scheduleQuery('recurring-test', 'test_predicate', {}, 'recurring', {
                                interval: 200, // Every 200ms
                                maxExecutions: 3,
                            })];
                    case 1:
                        _a.sent();
                        // Wait for multiple executions
                        return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 800); })];
                    case 2:
                        // Wait for multiple executions
                        _a.sent();
                        (0, chai_1.expect)(executionCount).to.equal(3);
                        return [2 /*return*/];
                }
            });
        }); });
        it('should handle conditional queries', function () { return __awaiter(void 0, void 0, void 0, function () {
            var condition, executionStarted;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        condition = false;
                        executionStarted = false;
                        queryScheduler.on('queryScheduleExecutionStarted', function () {
                            executionStarted = true;
                        });
                        // Mock the concurrency manager execution
                        concurrencyManager.on('executeQuery', function (event) {
                            event.resolve({ success: true });
                        });
                        return [4 /*yield*/, queryScheduler.scheduleQuery('conditional-test', 'test_predicate', {}, 'conditional', {
                                condition: 'condition',
                            })];
                    case 1:
                        _a.sent();
                        // Register condition evaluator
                        queryScheduler.registerConditionEvaluator('conditional-test', function () { return condition; });
                        // Should not execute when condition is false
                        return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 200); })];
                    case 2:
                        // Should not execute when condition is false
                        _a.sent();
                        (0, chai_1.expect)(executionStarted).to.be.false;
                        // Should execute when condition becomes true
                        condition = true;
                        return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 200); })];
                    case 3:
                        _a.sent();
                        (0, chai_1.expect)(executionStarted).to.be.true;
                        return [2 /*return*/];
                }
            });
        }); });
        it('should cancel scheduled queries', function () { return __awaiter(void 0, void 0, void 0, function () {
            var queries, cancelled, queriesAfterCancel;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, queryScheduler.scheduleQuery('cancel-scheduled-test', 'test_predicate', {}, 'delayed', {
                            executeAt: Date.now() + 1000,
                        })];
                    case 1:
                        _a.sent();
                        queries = queryScheduler.getScheduledQueries();
                        (0, chai_1.expect)(queries).to.have.length(1);
                        return [4 /*yield*/, queryScheduler.cancelScheduledQuery('cancel-scheduled-test')];
                    case 2:
                        cancelled = _a.sent();
                        (0, chai_1.expect)(cancelled).to.be.true;
                        queriesAfterCancel = queryScheduler.getScheduledQueries();
                        (0, chai_1.expect)(queriesAfterCancel).to.have.length(0);
                        return [2 /*return*/];
                }
            });
        }); });
        it('should provide scheduler statistics', function () { return __awaiter(void 0, void 0, void 0, function () {
            var stats;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, queryScheduler.scheduleQuery('stat-test-1', 'test_predicate', {}, 'immediate')];
                    case 1:
                        _a.sent();
                        return [4 /*yield*/, queryScheduler.scheduleQuery('stat-test-2', 'test_predicate', {}, 'recurring', {
                                interval: 1000,
                            })];
                    case 2:
                        _a.sent();
                        return [4 /*yield*/, queryScheduler.scheduleQuery('stat-test-3', 'test_predicate', {}, 'conditional', {
                                condition: 'true',
                            })];
                    case 3:
                        _a.sent();
                        stats = queryScheduler.getStatistics();
                        (0, chai_1.expect)(stats.totalScheduled).to.be.greaterThan(0);
                        (0, chai_1.expect)(stats.recurringQueries).to.equal(1);
                        (0, chai_1.expect)(stats.conditionalQueries).to.equal(1);
                        return [2 /*return*/];
                }
            });
        }); });
        it('should pause and resume recurring queries', function () { return __awaiter(void 0, void 0, void 0, function () {
            var executionCount, countAfterFirst, paused, resumed;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        executionCount = 0;
                        queryScheduler.on('queryScheduleExecutionStarted', function () {
                            executionCount++;
                        });
                        // Mock the concurrency manager execution
                        concurrencyManager.on('executeQuery', function (event) {
                            setTimeout(function () {
                                event.resolve({ success: true });
                            }, 50);
                        });
                        return [4 /*yield*/, queryScheduler.scheduleQuery('pause-resume-test', 'test_predicate', {}, 'recurring', {
                                interval: 200,
                            })];
                    case 1:
                        _a.sent();
                        // Let it execute once
                        return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 300); })];
                    case 2:
                        // Let it execute once
                        _a.sent();
                        countAfterFirst = executionCount;
                        return [4 /*yield*/, queryScheduler.pauseRecurringQuery('pause-resume-test')];
                    case 3:
                        paused = _a.sent();
                        (0, chai_1.expect)(paused).to.be.true;
                        // Wait and check it doesn't execute
                        return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 300); })];
                    case 4:
                        // Wait and check it doesn't execute
                        _a.sent();
                        (0, chai_1.expect)(executionCount).to.equal(countAfterFirst);
                        return [4 /*yield*/, queryScheduler.resumeRecurringQuery('pause-resume-test')];
                    case 5:
                        resumed = _a.sent();
                        (0, chai_1.expect)(resumed).to.be.true;
                        // Wait and check it executes again
                        return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 300); })];
                    case 6:
                        // Wait and check it executes again
                        _a.sent();
                        (0, chai_1.expect)(executionCount).to.be.greaterThan(countAfterFirst);
                        return [2 /*return*/];
                }
            });
        }); });
    });
    describe('Integration Tests', function () {
        it('should integrate all systems for a complete workflow', function () { return __awaiter(void 0, void 0, void 0, function () {
            var queryCompleted, historyAdded, history, concurrencyStats, historyStats, schedulerStats;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        queryCompleted = false;
                        historyAdded = false;
                        // Set up event listeners
                        queryScheduler.on('queryScheduleCompleted', function () {
                            queryCompleted = true;
                        });
                        historyManager.on('queryAdded', function () {
                            historyAdded = true;
                        });
                        // Mock the concurrency manager execution
                        concurrencyManager.on('executeQuery', function (event) {
                            setTimeout(function () {
                                event.resolve({ success: true, result: 'test_result' });
                            }, 100);
                        });
                        // Schedule a query
                        return [4 /*yield*/, queryScheduler.scheduleQuery('integration-test', 'test_predicate', { arg1: 'value1' }, 'immediate', {}, { level: 'high', weight: 100, timeout: 5000 }, { tags: ['integration', 'test'] })];
                    case 1:
                        // Schedule a query
                        _a.sent();
                        // Wait for completion
                        return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 300); })];
                    case 2:
                        // Wait for completion
                        _a.sent();
                        (0, chai_1.expect)(queryCompleted).to.be.true;
                        (0, chai_1.expect)(historyAdded).to.be.true;
                        return [4 /*yield*/, historyManager.getHistory()];
                    case 3:
                        history = _a.sent();
                        (0, chai_1.expect)(history.entries).to.have.length(1);
                        (0, chai_1.expect)(history.entries[0].cmd).to.equal('test_predicate');
                        (0, chai_1.expect)(history.entries[0].status).to.equal('completed');
                        concurrencyStats = concurrencyManager.getStatus();
                        return [4 /*yield*/, historyManager.getStatistics()];
                    case 4:
                        historyStats = _a.sent();
                        schedulerStats = queryScheduler.getStatistics();
                        (0, chai_1.expect)(concurrencyStats.resourceUsage).to.be.an('object');
                        (0, chai_1.expect)(historyStats.totalQueries).to.equal(1);
                        (0, chai_1.expect)(schedulerStats.completedScheduled).to.equal(1);
                        return [2 /*return*/];
                }
            });
        }); });
    });
});
