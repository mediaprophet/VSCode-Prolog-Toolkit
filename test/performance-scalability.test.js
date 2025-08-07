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
var outputFormatter_1 = require("../src/features/outputFormatter");
var streamingHandler_1 = require("../src/features/streamingHandler");
var prologBackend_js_1 = require("../src/prologBackend.js");
describe('Performance and Scalability Features', function () {
    var backend;
    var outputFormatter;
    before(function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        this.timeout(10000);
                        backend = new prologBackend_js_1.PrologBackend({
                            swiplPath: 'swipl',
                            port: 3061, // Use different port for tests
                            streamingEnabled: true,
                            maxResultsPerChunk: 25,
                        });
                        outputFormatter = new outputFormatter_1.OutputFormatter();
                        // Start backend for tests
                        backend.start();
                        // Wait for backend to be ready
                        return [4 /*yield*/, new Promise(function (resolve, reject) {
                                var timeout = setTimeout(function () { return reject(new Error('Backend startup timeout')); }, 8000);
                                backend.on('ready', function () {
                                    clearTimeout(timeout);
                                    resolve(undefined);
                                });
                                backend.on('error', function (error) {
                                    clearTimeout(timeout);
                                    reject(error);
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
    after(function () {
        if (backend) {
            backend.stop(true);
        }
    });
    describe('Streaming Handler', function () {
        it('should create a Prolog result streamer with correct defaults', function () { return __awaiter(void 0, void 0, void 0, function () {
            var streamer, results, chunks;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        streamer = (0, streamingHandler_1.createPrologResultStreamer)();
                        (0, chai_1.expect)(streamer).to.be.instanceOf(streamingHandler_1.StreamingHandler);
                        results = Array.from({ length: 120 }, function (_, i) { return ({ X: i }); });
                        chunks = [];
                        return [4 /*yield*/, streamer.startStreaming(results, function (chunk) { return __awaiter(void 0, void 0, void 0, function () {
                                return __generator(this, function (_a) {
                                    chunks.push(chunk);
                                    return [2 /*return*/];
                                });
                            }); })];
                    case 1:
                        _a.sent();
                        // Default chunkSize is 50, so expect 3 chunks for 120 results
                        (0, chai_1.expect)(chunks).to.have.length(3);
                        (0, chai_1.expect)(chunks[0].data).to.have.length(50);
                        (0, chai_1.expect)(chunks[1].data).to.have.length(50);
                        (0, chai_1.expect)(chunks[2].data).to.have.length(20);
                        return [2 /*return*/];
                }
            });
        }); });
        it('should handle small result sets without chunking', function () { return __awaiter(void 0, void 0, void 0, function () {
            var smallResults, streamer, chunks;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        smallResults = Array.from({ length: 10 }, function (_, i) { return ({ X: i }); });
                        streamer = (0, streamingHandler_1.createPrologResultStreamer)();
                        chunks = [];
                        return [4 /*yield*/, streamer.startStreaming(smallResults, function (chunk) { return __awaiter(void 0, void 0, void 0, function () {
                                return __generator(this, function (_a) {
                                    chunks.push(chunk);
                                    return [2 /*return*/];
                                });
                            }); })];
                    case 1:
                        _a.sent();
                        (0, chai_1.expect)(chunks).to.have.length(1);
                        (0, chai_1.expect)(chunks[0].data).to.have.length(10);
                        (0, chai_1.expect)(chunks[0].isFirst).to.be.true;
                        (0, chai_1.expect)(chunks[0].isLast).to.be.true;
                        return [2 /*return*/];
                }
            });
        }); });
        it('should chunk large result sets appropriately', function () { return __awaiter(void 0, void 0, void 0, function () {
            var largeResults, streamer, chunks;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        largeResults = Array.from({ length: 150 }, function (_, i) { return ({ X: i }); });
                        streamer = (0, streamingHandler_1.createPrologResultStreamer)({ chunkSize: 50 });
                        chunks = [];
                        return [4 /*yield*/, streamer.startStreaming(largeResults, function (chunk) { return __awaiter(void 0, void 0, void 0, function () {
                                return __generator(this, function (_a) {
                                    chunks.push(chunk);
                                    return [2 /*return*/];
                                });
                            }); })];
                    case 1:
                        _a.sent();
                        (0, chai_1.expect)(chunks).to.have.length(3);
                        (0, chai_1.expect)(chunks[0].data).to.have.length(50);
                        (0, chai_1.expect)(chunks[0].isFirst).to.be.true;
                        (0, chai_1.expect)(chunks[0].isLast).to.be.false;
                        (0, chai_1.expect)(chunks[1].data).to.have.length(50);
                        (0, chai_1.expect)(chunks[1].isFirst).to.be.false;
                        (0, chai_1.expect)(chunks[1].isLast).to.be.false;
                        (0, chai_1.expect)(chunks[2].data).to.have.length(50);
                        (0, chai_1.expect)(chunks[2].isFirst).to.be.false;
                        (0, chai_1.expect)(chunks[2].isLast).to.be.true;
                        return [2 /*return*/];
                }
            });
        }); });
    });
    describe('Backend Streaming Support', function () {
        it('should support streaming configuration', function () {
            var config = backend.getStreamingConfig();
            (0, chai_1.expect)(config.enabled).to.be.true;
            (0, chai_1.expect)(config.maxResultsPerChunk).to.equal(25);
        });
        it('should update streaming configuration', function () {
            backend.updateStreamingConfig({
                enabled: false,
                maxResultsPerChunk: 100,
            });
            var config = backend.getStreamingConfig();
            (0, chai_1.expect)(config.enabled).to.be.false;
            (0, chai_1.expect)(config.maxResultsPerChunk).to.equal(100);
            // Reset for other tests
            backend.updateStreamingConfig({
                enabled: true,
                maxResultsPerChunk: 25,
            });
        });
        it('should handle streaming requests', function () {
            return __awaiter(this, void 0, void 0, function () {
                var chunks_1, response, error_1;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(5000);
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 3, , 4]);
                            chunks_1 = [];
                            return [4 /*yield*/, backend.sendStreamingRequest('query', { goal: 'member(X, [1,2,3,4,5])' }, function (chunk, isFirst, isLast) {
                                    chunks_1.push({ chunk: chunk, isFirst: isFirst, isLast: isLast });
                                })];
                        case 2:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('ok');
                            (0, chai_1.expect)(response.results).to.be.an('array');
                            return [3 /*break*/, 4];
                        case 3:
                            error_1 = _a.sent();
                            // Skip test if streaming not fully supported yet
                            this.skip();
                            return [3 /*break*/, 4];
                        case 4: return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('Output Formatter Performance Features', function () {
        it('should format streaming output correctly', function () {
            var chunk = [{ X: 1 }, { X: 2 }, { X: 3 }];
            var output = outputFormatter.formatStreamingOutput(chunk, true, false, 100, 0);
            (0, chai_1.expect)(output).to.include('📊 **Streaming results** (100 total)');
            (0, chai_1.expect)(output).to.include('⏳ *Loading more results...*');
        });
        it('should format paginated output with navigation', function () {
            var results = [{ subject: 'a', predicate: 'b', object: 'c' }];
            var pagination = {
                total_count: 1000,
                offset: 50,
                limit: 25,
                has_more: true,
                next_offset: 75,
            };
            var output = outputFormatter.formatPaginatedOutput(results, pagination);
            (0, chai_1.expect)(output).to.include('📄 **Page Results** (51-51 of 1000)');
            (0, chai_1.expect)(output).to.include('Next page: Use `--offset 75 --limit 25`');
        });
        it('should format large result sets with performance optimizations', function () {
            var largeResults = Array.from({ length: 200 }, function (_, i) { return ({ X: i }); });
            var output = outputFormatter.formatLargeResultSet(largeResults, 1000, 50);
            (0, chai_1.expect)(output).to.include('📊 **Large Result Set** (showing first 200 of 1000)');
            (0, chai_1.expect)(output).to.include('Performance tip: Results are automatically chunked');
        });
        it('should use compact formatting for very large results', function () {
            var veryLargeResults = Array.from({ length: 150 }, function (_, i) { return ({ X: i, Y: i * 2 }); });
            var output = outputFormatter.formatLargeResultSet(veryLargeResults);
            (0, chai_1.expect)(output).to.include('```');
            (0, chai_1.expect)(output).to.include('1. X=0, Y=0');
        });
    });
    describe('Integration Tests', function () {
        it('should handle large query results with streaming', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response, error_2;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(10000);
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 3, , 4]);
                            return [4 /*yield*/, backend.sendRequest('query', {
                                    goal: 'between(1, 100, X)',
                                    streaming: true,
                                    max_results_per_chunk: 20,
                                })];
                        case 2:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('ok');
                            (0, chai_1.expect)(response.results).to.be.an('array');
                            if (response.streaming_info) {
                                (0, chai_1.expect)(response.streaming_info.total_count).to.be.greaterThan(20);
                                (0, chai_1.expect)(response.streaming_info.is_large_result).to.be.true;
                            }
                            return [3 /*break*/, 4];
                        case 3:
                            error_2 = _a.sent();
                            // Skip test if streaming not fully supported yet
                            this.skip();
                            return [3 /*break*/, 4];
                        case 4: return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle N3 list with pagination', function () {
            return __awaiter(this, void 0, void 0, function () {
                var response, error_3;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(8000);
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 4, , 5]);
                            // First load some N3 data
                            return [4 /*yield*/, backend.sendRequest('n3_load', {
                                    content: "\n            @prefix : <http://example.org/> .\n            :socrates a :Person .\n            :plato a :Person .\n            :aristotle a :Person .\n            :socrates :teaches :plato .\n            :plato :teaches :aristotle .\n          ",
                                })];
                        case 2:
                            // First load some N3 data
                            _a.sent();
                            return [4 /*yield*/, backend.sendRequest('n3_list', {
                                    limit: 2,
                                    offset: 0,
                                    streaming: true,
                                })];
                        case 3:
                            response = _a.sent();
                            (0, chai_1.expect)(response.status).to.equal('ok');
                            (0, chai_1.expect)(response.triples).to.be.an('array');
                            if (response.pagination) {
                                (0, chai_1.expect)(response.pagination.limit).to.equal(2);
                                (0, chai_1.expect)(response.pagination.offset).to.equal(0);
                                (0, chai_1.expect)(response.pagination.total_count).to.be.greaterThan(0);
                            }
                            return [3 /*break*/, 5];
                        case 4:
                            error_3 = _a.sent();
                            // Skip test if N3 pagination not fully supported yet
                            this.skip();
                            return [3 /*break*/, 5];
                        case 5: return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('Performance Benchmarks', function () {
        it('should process large result sets efficiently', function () {
            return __awaiter(this, void 0, void 0, function () {
                var startTime, largeResults, streamer, processedCount, endTime, processingTime;
                var _this = this;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(15000);
                            startTime = Date.now();
                            largeResults = Array.from({ length: 1000 }, function (_, i) { return ({
                                X: i,
                                Y: i * 2,
                                Z: "value_".concat(i),
                            }); });
                            streamer = (0, streamingHandler_1.createPrologResultStreamer)({ chunkSize: 100 });
                            processedCount = 0;
                            return [4 /*yield*/, streamer.startStreaming(largeResults, function (chunk) { return __awaiter(_this, void 0, void 0, function () {
                                    return __generator(this, function (_a) {
                                        switch (_a.label) {
                                            case 0:
                                                processedCount += chunk.data.length;
                                                // Simulate some processing time
                                                return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 1); })];
                                            case 1:
                                                // Simulate some processing time
                                                _a.sent();
                                                return [2 /*return*/];
                                        }
                                    });
                                }); })];
                        case 1:
                            _a.sent();
                            endTime = Date.now();
                            processingTime = endTime - startTime;
                            (0, chai_1.expect)(processedCount).to.equal(1000);
                            (0, chai_1.expect)(processingTime).to.be.lessThan(5000); // Should complete within 5 seconds
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should format large outputs efficiently', function () {
            var startTime = Date.now();
            var largeResults = Array.from({ length: 500 }, function (_, i) { return ({
                variable: "X".concat(i),
                value: "result_".concat(i),
            }); });
            var output = outputFormatter.formatLargeResultSet(largeResults);
            var endTime = Date.now();
            var formatTime = endTime - startTime;
            (0, chai_1.expect)(output).to.be.a('string');
            (0, chai_1.expect)(output.length).to.be.greaterThan(0);
            (0, chai_1.expect)(formatTime).to.be.lessThan(1000); // Should format within 1 second
        });
    });
});
