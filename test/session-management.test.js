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
var concurrencyManager_js_1 = require("../src/features/concurrencyManager.js");
var queryHistoryManager_js_1 = require("../src/features/queryHistoryManager.js");
var sessionManager_js_1 = require("../src/features/sessionManager.js");
var prologBackend_js_1 = require("../src/prologBackend.js");
describe('Session Management System', function () {
    this.timeout(30000);
    var sessionManager;
    var concurrencyManager;
    var historyManager;
    var prologBackend;
    var testStorageDir;
    beforeEach(function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        // Create temporary storage directory
                        testStorageDir = path.join(__dirname, 'temp-sessions-' + Date.now());
                        // Initialize managers
                        concurrencyManager = new concurrencyManager_js_1.ConcurrencyManager();
                        historyManager = new queryHistoryManager_js_1.QueryHistoryManager({
                            storageDir: path.join(testStorageDir, 'history'),
                        });
                        sessionManager = new sessionManager_js_1.SessionManager({
                            storageDir: testStorageDir,
                            maxSessions: 10,
                            enablePersistence: true,
                            enableAutoSave: false, // Disable for testing
                            autoCleanupInterval: 60000, // 1 minute for testing
                        });
                        sessionManager.setIntegrationManagers(concurrencyManager, historyManager);
                        // Wait for initialization
                        // Wait for initialization (polling, since SessionManager does not have .once)
                        return [4 /*yield*/, new Promise(function (resolve) {
                                var check = function () {
                                    if (sessionManager['isInitialized']) {
                                        resolve(undefined);
                                    }
                                    else {
                                        setTimeout(check, 10);
                                    }
                                };
                                check();
                            })];
                    case 1:
                        // Wait for initialization
                        // Wait for initialization (polling, since SessionManager does not have .once)
                        _a.sent();
                        return [2 /*return*/];
                }
            });
        });
    });
    afterEach(function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                // Clean up
                if (sessionManager) {
                    sessionManager.dispose();
                }
                if (concurrencyManager) {
                    concurrencyManager.dispose();
                }
                if (historyManager) {
                    historyManager.dispose();
                }
                if (prologBackend) {
                    prologBackend.stop();
                }
                // Remove test storage directory
                if (fs.existsSync(testStorageDir)) {
                    fs.rmSync(testStorageDir, { recursive: true, force: true });
                }
                return [2 /*return*/];
            });
        });
    });
    describe('Session Creation and Management', function () {
        it('should create a new session', function () {
            return __awaiter(this, void 0, void 0, function () {
                var sessionId, session;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, sessionManager.createSession('Test Session', {
                                description: 'A test session',
                                userId: 'user123',
                                metadata: { test: true },
                            })];
                        case 1:
                            sessionId = _a.sent();
                            (0, chai_1.expect)(sessionId).to.be.a('string');
                            (0, chai_1.expect)(sessionId).to.have.length.greaterThan(0);
                            session = sessionManager.getSession(sessionId);
                            (0, chai_1.expect)(session).to.not.be.null;
                            (0, chai_1.expect)(session.config.name).to.equal('Test Session');
                            (0, chai_1.expect)(session.config.description).to.equal('A test session');
                            (0, chai_1.expect)(session.config.userId).to.equal('user123');
                            (0, chai_1.expect)(session.config.metadata).to.deep.equal({ test: true });
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should list sessions', function () {
            return __awaiter(this, void 0, void 0, function () {
                var sessionId1, sessionId2, sessions, sessionIds;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, sessionManager.createSession('Session 1', { userId: 'user1' })];
                        case 1:
                            sessionId1 = _a.sent();
                            return [4 /*yield*/, sessionManager.createSession('Session 2', { userId: 'user2' })];
                        case 2:
                            sessionId2 = _a.sent();
                            sessions = sessionManager.listSessions();
                            (0, chai_1.expect)(sessions).to.have.length(2);
                            sessionIds = sessions.map(function (s) { return s.sessionId; });
                            (0, chai_1.expect)(sessionIds).to.include(sessionId1);
                            (0, chai_1.expect)(sessionIds).to.include(sessionId2);
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should filter sessions by user', function () {
            return __awaiter(this, void 0, void 0, function () {
                var user1Sessions, user2Sessions;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, sessionManager.createSession('Session 1', { userId: 'user1' })];
                        case 1:
                            _a.sent();
                            return [4 /*yield*/, sessionManager.createSession('Session 2', { userId: 'user2' })];
                        case 2:
                            _a.sent();
                            return [4 /*yield*/, sessionManager.createSession('Session 3', { userId: 'user1' })];
                        case 3:
                            _a.sent();
                            user1Sessions = sessionManager.listSessions({ userId: 'user1' });
                            (0, chai_1.expect)(user1Sessions).to.have.length(2);
                            user2Sessions = sessionManager.listSessions({ userId: 'user2' });
                            (0, chai_1.expect)(user2Sessions).to.have.length(1);
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should switch between sessions', function () {
            return __awaiter(this, void 0, void 0, function () {
                var sessionId1, sessionId2, currentSession, session1;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, sessionManager.createSession('Session 1')];
                        case 1:
                            sessionId1 = _a.sent();
                            return [4 /*yield*/, sessionManager.createSession('Session 2')];
                        case 2:
                            sessionId2 = _a.sent();
                            // Switch to session 1
                            return [4 /*yield*/, sessionManager.switchToSession(sessionId1)];
                        case 3:
                            // Switch to session 1
                            _a.sent();
                            currentSession = sessionManager.getCurrentSession();
                            (0, chai_1.expect)(currentSession).to.not.be.null;
                            (0, chai_1.expect)(currentSession.sessionId).to.equal(sessionId1);
                            (0, chai_1.expect)(currentSession.config.isActive).to.be.true;
                            // Switch to session 2
                            return [4 /*yield*/, sessionManager.switchToSession(sessionId2)];
                        case 4:
                            // Switch to session 2
                            _a.sent();
                            currentSession = sessionManager.getCurrentSession();
                            (0, chai_1.expect)(currentSession).to.not.be.null;
                            (0, chai_1.expect)(currentSession.sessionId).to.equal(sessionId2);
                            (0, chai_1.expect)(currentSession.config.isActive).to.be.true;
                            session1 = sessionManager.getSession(sessionId1);
                            (0, chai_1.expect)(session1.config.isActive).to.be.false;
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should delete a session', function () {
            return __awaiter(this, void 0, void 0, function () {
                var sessionId, session, deleted;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, sessionManager.createSession('Test Session')];
                        case 1:
                            sessionId = _a.sent();
                            session = sessionManager.getSession(sessionId);
                            (0, chai_1.expect)(session).to.not.be.null;
                            return [4 /*yield*/, sessionManager.deleteSession(sessionId)];
                        case 2:
                            deleted = _a.sent();
                            (0, chai_1.expect)(deleted).to.be.true;
                            // Verify session no longer exists
                            session = sessionManager.getSession(sessionId);
                            (0, chai_1.expect)(session).to.be.null;
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should not delete active session', function () {
            return __awaiter(this, void 0, void 0, function () {
                var sessionId, error_1;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, sessionManager.createSession('Active Session')];
                        case 1:
                            sessionId = _a.sent();
                            return [4 /*yield*/, sessionManager.switchToSession(sessionId)];
                        case 2:
                            _a.sent();
                            _a.label = 3;
                        case 3:
                            _a.trys.push([3, 5, , 6]);
                            return [4 /*yield*/, sessionManager.deleteSession(sessionId)];
                        case 4:
                            _a.sent();
                            chai_1.expect.fail('Should have thrown an error');
                            return [3 /*break*/, 6];
                        case 5:
                            error_1 = _a.sent();
                            (0, chai_1.expect)(error_1.message).to.include('Cannot delete active session');
                            return [3 /*break*/, 6];
                        case 6: return [2 /*return*/];
                    }
                });
            });
        });
        it('should enforce session limit', function () {
            return __awaiter(this, void 0, void 0, function () {
                var i, error_2;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            i = 0;
                            _a.label = 1;
                        case 1:
                            if (!(i < 10)) return [3 /*break*/, 4];
                            return [4 /*yield*/, sessionManager.createSession("Session ".concat(i))];
                        case 2:
                            _a.sent();
                            _a.label = 3;
                        case 3:
                            i++;
                            return [3 /*break*/, 1];
                        case 4:
                            _a.trys.push([4, 6, , 7]);
                            return [4 /*yield*/, sessionManager.createSession('Excess Session')];
                        case 5:
                            _a.sent();
                            chai_1.expect.fail('Should have thrown an error');
                            return [3 /*break*/, 7];
                        case 6:
                            error_2 = _a.sent();
                            (0, chai_1.expect)(error_2.message).to.include('Maximum number of sessions');
                            return [3 /*break*/, 7];
                        case 7: return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('Session State Management', function () {
        var sessionId;
        beforeEach(function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, sessionManager.createSession('State Test Session')];
                        case 1:
                            sessionId = _a.sent();
                            return [4 /*yield*/, sessionManager.switchToSession(sessionId)];
                        case 2:
                            _a.sent();
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should save and restore session state', function () {
            return __awaiter(this, void 0, void 0, function () {
                var testState, session;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            testState = {
                                prologFacts: ['fact(a)', 'fact(b)'],
                                prologRules: ['rule(X) :- fact(X)'],
                                variables: { testVar: 'testValue' },
                                rdfTriples: [
                                    {
                                        subject: 'http://example.org/subject',
                                        predicate: 'http://example.org/predicate',
                                        object: 'http://example.org/object',
                                    },
                                ],
                            };
                            // Save state
                            return [4 /*yield*/, sessionManager.saveSessionState(sessionId, testState)];
                        case 1:
                            // Save state
                            _a.sent();
                            session = sessionManager.getSession(sessionId);
                            (0, chai_1.expect)(session).to.not.be.null;
                            (0, chai_1.expect)(session.state.prologFacts).to.deep.equal(testState.prologFacts);
                            (0, chai_1.expect)(session.state.prologRules).to.deep.equal(testState.prologRules);
                            (0, chai_1.expect)(session.state.variables).to.deep.equal(testState.variables);
                            (0, chai_1.expect)(session.state.rdfTriples).to.deep.equal(testState.rdfTriples);
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should create and restore from snapshots', function () {
            return __awaiter(this, void 0, void 0, function () {
                var initialState, snapshotId, modifiedState, session;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            initialState = {
                                prologFacts: ['initial_fact(1)'],
                                variables: { version: 1 },
                            };
                            return [4 /*yield*/, sessionManager.saveSessionState(sessionId, initialState)];
                        case 1:
                            _a.sent();
                            return [4 /*yield*/, sessionManager.createSnapshot(sessionId, 'Initial State', 'Snapshot of initial state')];
                        case 2:
                            snapshotId = _a.sent();
                            (0, chai_1.expect)(snapshotId).to.be.a('string');
                            modifiedState = {
                                prologFacts: ['modified_fact(2)'],
                                variables: { version: 2 },
                            };
                            return [4 /*yield*/, sessionManager.saveSessionState(sessionId, modifiedState)];
                        case 3:
                            _a.sent();
                            // Restore from snapshot
                            return [4 /*yield*/, sessionManager.restoreSessionState(sessionId, snapshotId)];
                        case 4:
                            // Restore from snapshot
                            _a.sent();
                            session = sessionManager.getSession(sessionId);
                            (0, chai_1.expect)(session.state.prologFacts).to.deep.equal(initialState.prologFacts);
                            (0, chai_1.expect)(session.state.variables).to.deep.equal(initialState.variables);
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should persist state to disk', function () {
            return __awaiter(this, void 0, void 0, function () {
                var testState, newSessionManager, loadedSession;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            testState = {
                                prologFacts: ['persistent_fact(test)'],
                                timestamp: Date.now(),
                            };
                            return [4 /*yield*/, sessionManager.saveSessionState(sessionId, testState)];
                        case 1:
                            _a.sent();
                            newSessionManager = new sessionManager_js_1.SessionManager({
                                storageDir: testStorageDir,
                                enablePersistence: true,
                            });
                            // Wait for initialization (polling)
                            return [4 /*yield*/, new Promise(function (resolve) {
                                    var check = function () {
                                        if (newSessionManager['isInitialized']) {
                                            resolve(undefined);
                                        }
                                        else {
                                            setTimeout(check, 10);
                                        }
                                    };
                                    check();
                                })];
                        case 2:
                            // Wait for initialization (polling)
                            _a.sent();
                            loadedSession = newSessionManager.getSession(sessionId);
                            (0, chai_1.expect)(loadedSession).to.not.be.null;
                            (0, chai_1.expect)(loadedSession.state.prologFacts).to.deep.equal(testState.prologFacts);
                            newSessionManager.dispose();
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('Resource Management Integration', function () {
        var sessionId;
        beforeEach(function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, sessionManager.createSession('Resource Test Session', {
                                resourceQuota: {
                                    maxConcurrentQueries: 3,
                                    maxMemoryUsageMB: 128,
                                },
                            })];
                        case 1:
                            sessionId = _a.sent();
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should create session-specific concurrency manager', function () {
            return __awaiter(this, void 0, void 0, function () {
                var sessionConcurrencyManager, status;
                return __generator(this, function (_a) {
                    sessionConcurrencyManager = sessionManager.getSessionConcurrencyManager(sessionId);
                    (0, chai_1.expect)(sessionConcurrencyManager).to.not.be.undefined;
                    status = sessionConcurrencyManager.getStatus();
                    (0, chai_1.expect)(status.resourceQuota.maxConcurrentQueries).to.equal(3);
                    (0, chai_1.expect)(status.resourceQuota.maxMemoryUsageMB).to.equal(128);
                    return [2 /*return*/];
                });
            });
        });
        it('should create session-specific history manager', function () {
            return __awaiter(this, void 0, void 0, function () {
                var sessionHistoryManager, history;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            sessionHistoryManager = sessionManager.getSessionHistoryManager(sessionId);
                            (0, chai_1.expect)(sessionHistoryManager).to.not.be.undefined;
                            // Test adding a query to session history
                            return [4 /*yield*/, sessionHistoryManager.addQuery({
                                    id: 'test-query-1',
                                    cmd: 'query',
                                    params: { goal: 'test(X)' },
                                    status: 'completed',
                                    startTime: Date.now(),
                                    endTime: Date.now() + 1000,
                                })];
                        case 1:
                            // Test adding a query to session history
                            _a.sent();
                            return [4 /*yield*/, sessionHistoryManager.getHistory()];
                        case 2:
                            history = _a.sent();
                            (0, chai_1.expect)(history.entries).to.have.length(1);
                            (0, chai_1.expect)(history.entries[0].id).to.equal('test-query-1');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should update session resource quota', function () {
            return __awaiter(this, void 0, void 0, function () {
                var session, sessionConcurrencyManager, status;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, sessionManager.updateSessionResourceQuota(sessionId, {
                                maxConcurrentQueries: 5,
                                maxMemoryUsageMB: 256,
                            })];
                        case 1:
                            _a.sent();
                            session = sessionManager.getSession(sessionId);
                            (0, chai_1.expect)(session.config.resourceQuota.maxConcurrentQueries).to.equal(5);
                            (0, chai_1.expect)(session.config.resourceQuota.maxMemoryUsageMB).to.equal(256);
                            sessionConcurrencyManager = sessionManager.getSessionConcurrencyManager(sessionId);
                            status = sessionConcurrencyManager.getStatus();
                            (0, chai_1.expect)(status.resourceQuota.maxConcurrentQueries).to.equal(5);
                            (0, chai_1.expect)(status.resourceQuota.maxMemoryUsageMB).to.equal(256);
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should get session statistics', function () {
            return __awaiter(this, void 0, void 0, function () {
                var sessionHistoryManager, stats;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, sessionManager.switchToSession(sessionId)];
                        case 1:
                            _a.sent();
                            sessionHistoryManager = sessionManager.getSessionHistoryManager(sessionId);
                            return [4 /*yield*/, sessionHistoryManager.addQuery({
                                    id: 'stats-query-1',
                                    cmd: 'query',
                                    params: { goal: 'stats_test(X)' },
                                    status: 'completed',
                                    startTime: Date.now() - 5000,
                                    endTime: Date.now() - 4000,
                                })];
                        case 2:
                            _a.sent();
                            return [4 /*yield*/, sessionManager.getSessionStatistics(sessionId)];
                        case 3:
                            stats = _a.sent();
                            (0, chai_1.expect)(stats).to.not.be.null;
                            (0, chai_1.expect)(stats.config.id).to.equal(sessionId);
                            (0, chai_1.expect)(stats.uptime).to.be.greaterThan(0);
                            (0, chai_1.expect)(stats.idleTime).to.be.greaterThan(0);
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('PrologBackend Integration', function () {
        beforeEach(function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            // Initialize PrologBackend with session support
                            prologBackend = new prologBackend_js_1.PrologBackend({
                                port: 3061, // Use different port for testing
                                sessionOptions: {
                                    storageDir: path.join(testStorageDir, 'backend-sessions'),
                                    maxSessions: 5,
                                },
                            });
                            // Start the backend
                            prologBackend.start();
                            // Wait for backend to be ready
                            return [4 /*yield*/, new Promise(function (resolve, reject) {
                                    var timeout = setTimeout(function () {
                                        reject(new Error('Backend startup timeout'));
                                    }, 10000);
                                    prologBackend.once('ready', function () {
                                        clearTimeout(timeout);
                                        resolve(undefined);
                                    });
                                })];
                        case 1:
                            // Wait for backend to be ready
                            _a.sent();
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should create session through PrologBackend', function () {
            return __awaiter(this, void 0, void 0, function () {
                var sessionId, session;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, prologBackend.createSession('Backend Test Session', {
                                description: 'Session created through PrologBackend',
                                userId: 'backend-user',
                            })];
                        case 1:
                            sessionId = _a.sent();
                            (0, chai_1.expect)(sessionId).to.be.a('string');
                            session = prologBackend.getSession(sessionId);
                            (0, chai_1.expect)(session).to.not.be.null;
                            (0, chai_1.expect)(session.config.name).to.equal('Backend Test Session');
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should switch sessions through PrologBackend', function () {
            return __awaiter(this, void 0, void 0, function () {
                var sessionId1, sessionId2, currentSession;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, prologBackend.createSession('Backend Session 1')];
                        case 1:
                            sessionId1 = _a.sent();
                            return [4 /*yield*/, prologBackend.createSession('Backend Session 2')];
                        case 2:
                            sessionId2 = _a.sent();
                            return [4 /*yield*/, prologBackend.switchToSession(sessionId1)];
                        case 3:
                            _a.sent();
                            currentSession = prologBackend.getCurrentSession();
                            (0, chai_1.expect)(currentSession.sessionId).to.equal(sessionId1);
                            return [4 /*yield*/, prologBackend.switchToSession(sessionId2)];
                        case 4:
                            _a.sent();
                            currentSession = prologBackend.getCurrentSession();
                            (0, chai_1.expect)(currentSession.sessionId).to.equal(sessionId2);
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should save and restore session state through PrologBackend', function () {
            return __awaiter(this, void 0, void 0, function () {
                var sessionId, snapshotId;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, prologBackend.createSession('State Backend Session')];
                        case 1:
                            sessionId = _a.sent();
                            return [4 /*yield*/, prologBackend.switchToSession(sessionId)];
                        case 2:
                            _a.sent();
                            // Save session state
                            return [4 /*yield*/, prologBackend.saveCurrentSessionState()];
                        case 3:
                            // Save session state
                            _a.sent();
                            return [4 /*yield*/, prologBackend.createSessionSnapshot(sessionId, 'Backend Snapshot', 'Snapshot created through backend')];
                        case 4:
                            snapshotId = _a.sent();
                            (0, chai_1.expect)(snapshotId).to.be.a('string');
                            // Restore from snapshot
                            return [4 /*yield*/, prologBackend.restoreSessionState(sessionId, snapshotId)];
                        case 5:
                            // Restore from snapshot
                            _a.sent();
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle session events', function () {
            return __awaiter(this, void 0, void 0, function () {
                var sessionCreatedEvent, sessionSwitchedEvent, sessionId;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            sessionCreatedEvent = null;
                            sessionSwitchedEvent = null;
                            prologBackend.on('sessionCreated', function (event) {
                                sessionCreatedEvent = event;
                            });
                            prologBackend.on('sessionSwitched', function (event) {
                                sessionSwitchedEvent = event;
                            });
                            return [4 /*yield*/, prologBackend.createSession('Event Test Session')];
                        case 1:
                            sessionId = _a.sent();
                            return [4 /*yield*/, prologBackend.switchToSession(sessionId)];
                        case 2:
                            _a.sent();
                            // Wait a bit for events to be processed
                            return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 100); })];
                        case 3:
                            // Wait a bit for events to be processed
                            _a.sent();
                            (0, chai_1.expect)(sessionCreatedEvent).to.not.be.null;
                            (0, chai_1.expect)(sessionCreatedEvent.sessionId).to.equal(sessionId);
                            (0, chai_1.expect)(sessionSwitchedEvent).to.not.be.null;
                            (0, chai_1.expect)(sessionSwitchedEvent.currentSessionId).to.equal(sessionId);
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('Multi-Session Scenarios', function () {
        it('should handle multiple concurrent sessions', function () {
            return __awaiter(this, void 0, void 0, function () {
                var sessions, i, sessionId, allSessions, _i, sessions_1, sessionId, currentSession, _a, sessions_2, sessionId, concurrencyManager_1, historyManager_1;
                return __generator(this, function (_b) {
                    switch (_b.label) {
                        case 0:
                            sessions = [];
                            i = 0;
                            _b.label = 1;
                        case 1:
                            if (!(i < 5)) return [3 /*break*/, 4];
                            return [4 /*yield*/, sessionManager.createSession("Concurrent Session ".concat(i), {
                                    userId: "user".concat(i),
                                    resourceQuota: {
                                        maxConcurrentQueries: 2 + i,
                                        maxMemoryUsageMB: 64 * (i + 1),
                                    },
                                })];
                        case 2:
                            sessionId = _b.sent();
                            sessions.push(sessionId);
                            _b.label = 3;
                        case 3:
                            i++;
                            return [3 /*break*/, 1];
                        case 4:
                            allSessions = sessionManager.listSessions();
                            (0, chai_1.expect)(allSessions).to.have.length(5);
                            _i = 0, sessions_1 = sessions;
                            _b.label = 5;
                        case 5:
                            if (!(_i < sessions_1.length)) return [3 /*break*/, 8];
                            sessionId = sessions_1[_i];
                            return [4 /*yield*/, sessionManager.switchToSession(sessionId)];
                        case 6:
                            _b.sent();
                            currentSession = sessionManager.getCurrentSession();
                            (0, chai_1.expect)(currentSession.sessionId).to.equal(sessionId);
                            _b.label = 7;
                        case 7:
                            _i++;
                            return [3 /*break*/, 5];
                        case 8:
                            // Test session isolation - each should have its own managers
                            for (_a = 0, sessions_2 = sessions; _a < sessions_2.length; _a++) {
                                sessionId = sessions_2[_a];
                                concurrencyManager_1 = sessionManager.getSessionConcurrencyManager(sessionId);
                                historyManager_1 = sessionManager.getSessionHistoryManager(sessionId);
                                (0, chai_1.expect)(concurrencyManager_1).to.not.be.undefined;
                                (0, chai_1.expect)(historyManager_1).to.not.be.undefined;
                            }
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should maintain session state isolation', function () {
            return __awaiter(this, void 0, void 0, function () {
                var sessionId1, sessionId2, session1, session2;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, sessionManager.createSession('Isolation Session 1')];
                        case 1:
                            sessionId1 = _a.sent();
                            return [4 /*yield*/, sessionManager.createSession('Isolation Session 2')];
                        case 2:
                            sessionId2 = _a.sent();
                            // Set different states for each session
                            return [4 /*yield*/, sessionManager.saveSessionState(sessionId1, {
                                    prologFacts: ['session1_fact(a)'],
                                    variables: { sessionId: 1 },
                                })];
                        case 3:
                            // Set different states for each session
                            _a.sent();
                            return [4 /*yield*/, sessionManager.saveSessionState(sessionId2, {
                                    prologFacts: ['session2_fact(b)'],
                                    variables: { sessionId: 2 },
                                })];
                        case 4:
                            _a.sent();
                            session1 = sessionManager.getSession(sessionId1);
                            session2 = sessionManager.getSession(sessionId2);
                            (0, chai_1.expect)(session1.state.prologFacts).to.deep.equal(['session1_fact(a)']);
                            (0, chai_1.expect)(session1.state.variables.sessionId).to.equal(1);
                            (0, chai_1.expect)(session2.state.prologFacts).to.deep.equal(['session2_fact(b)']);
                            (0, chai_1.expect)(session2.state.variables.sessionId).to.equal(2);
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle session cleanup', function () {
            return __awaiter(this, void 0, void 0, function () {
                var sessionManager2, sessionId, session;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            sessionManager2 = new sessionManager_js_1.SessionManager({
                                storageDir: path.join(testStorageDir, 'cleanup-test'),
                                maxIdleTime: 100, // 100ms for testing
                                autoCleanupInterval: 200, // 200ms for testing
                                enablePersistence: false,
                            });
                            // Wait for initialization (polling)
                            return [4 /*yield*/, new Promise(function (resolve) {
                                    var check = function () {
                                        if (sessionManager2['isInitialized']) {
                                            resolve(undefined);
                                        }
                                        else {
                                            setTimeout(check, 10);
                                        }
                                    };
                                    check();
                                })];
                        case 1:
                            // Wait for initialization (polling)
                            _a.sent();
                            return [4 /*yield*/, sessionManager2.createSession('Cleanup Test Session')];
                        case 2:
                            sessionId = _a.sent();
                            session = sessionManager2.getSession(sessionId);
                            (0, chai_1.expect)(session).to.not.be.null;
                            // Wait for cleanup to occur
                            return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 500); })];
                        case 3:
                            // Wait for cleanup to occur
                            _a.sent();
                            // Session should be cleaned up
                            session = sessionManager2.getSession(sessionId);
                            (0, chai_1.expect)(session).to.be.null;
                            sessionManager2.dispose();
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('Error Handling', function () {
        it('should handle non-existent session operations', function () {
            return __awaiter(this, void 0, void 0, function () {
                var nonExistentId, error_3, error_4, deleted;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            nonExistentId = 'non-existent-session-id';
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 3, , 4]);
                            return [4 /*yield*/, sessionManager.switchToSession(nonExistentId)];
                        case 2:
                            _a.sent();
                            chai_1.expect.fail('Should have thrown an error');
                            return [3 /*break*/, 4];
                        case 3:
                            error_3 = _a.sent();
                            (0, chai_1.expect)(error_3.message).to.include('not found');
                            return [3 /*break*/, 4];
                        case 4:
                            _a.trys.push([4, 6, , 7]);
                            return [4 /*yield*/, sessionManager.saveSessionState(nonExistentId)];
                        case 5:
                            _a.sent();
                            chai_1.expect.fail('Should have thrown an error');
                            return [3 /*break*/, 7];
                        case 6:
                            error_4 = _a.sent();
                            (0, chai_1.expect)(error_4.message).to.include('not found');
                            return [3 /*break*/, 7];
                        case 7: return [4 /*yield*/, sessionManager.deleteSession(nonExistentId)];
                        case 8:
                            deleted = _a.sent();
                            (0, chai_1.expect)(deleted).to.be.false;
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle storage errors gracefully', function () {
            return __awaiter(this, void 0, void 0, function () {
                var invalidSessionManager, sessionId;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            invalidSessionManager = new sessionManager_js_1.SessionManager({
                                storageDir: '/invalid/path/that/does/not/exist',
                                enablePersistence: true,
                            });
                            // Should still initialize but with warnings
                            // Wait for initialization (polling)
                            return [4 /*yield*/, new Promise(function (resolve) {
                                    var check = function () {
                                        if (invalidSessionManager['isInitialized']) {
                                            resolve(undefined);
                                        }
                                        else {
                                            setTimeout(check, 10);
                                        }
                                    };
                                    check();
                                })];
                        case 1:
                            // Should still initialize but with warnings
                            // Wait for initialization (polling)
                            _a.sent();
                            return [4 /*yield*/, invalidSessionManager.createSession('Memory Only Session')];
                        case 2:
                            sessionId = _a.sent();
                            (0, chai_1.expect)(sessionId).to.be.a('string');
                            invalidSessionManager.dispose();
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
});
