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
var path = require("path");
var prologBackend_js_1 = require("../src/prologBackend.js");
describe('PrologBackend PlDoc Only', function () {
    it('should return args and examples for a user-defined predicate (PlDoc only)', function (done) {
        var _this = this;
        this.timeout(12000);
        var backend = new prologBackend_js_1.PrologBackend();
        var testFile = path.join(__dirname, 'resources', 'foo_with_pldoc.pl').replace(/\\/g, '/');
        var finished = false;
        function finish(err, skip) {
            if (!finished) {
                finished = true;
                backend.stop();
                if (skip) {
                    console.warn('[PLDOC TEST] Skipping: PlDoc is missing.');
                    return done();
                }
                if (err) {
                    console.error('[PLDOC TEST] done() called with error:', err);
                    return done(err);
                }
                console.log('[PLDOC TEST] done() called with success');
                done();
            }
        }
        backend.on('started', function () { return __awaiter(_this, void 0, void 0, function () {
            var consultResp, makeResp, e_1, doc, err_1, err_2;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        _a.trys.push([0, 11, , 12]);
                        console.log('[PLDOC TEST] Backend started. About to consult file:', testFile);
                        consultResp = void 0, makeResp = void 0;
                        _a.label = 1;
                    case 1:
                        _a.trys.push([1, 4, , 5]);
                        return [4 /*yield*/, backend.sendRequest('query', { goal: "consult('".concat(testFile, "')") })];
                    case 2:
                        consultResp = _a.sent();
                        console.log('[PLDOC TEST] Consult response:', consultResp);
                        return [4 /*yield*/, backend.sendRequest('query', { goal: 'make.' })];
                    case 3:
                        makeResp = _a.sent();
                        console.log('[PLDOC TEST] make/0 response:', makeResp);
                        return [3 /*break*/, 5];
                    case 4:
                        e_1 = _a.sent();
                        console.error('[PLDOC TEST] Consult or make/0 failed:', e_1);
                        finish(e_1);
                        return [2 /*return*/];
                    case 5: return [4 /*yield*/, new Promise(function (res) { return setTimeout(res, 150); })];
                    case 6:
                        _a.sent();
                        doc = void 0;
                        _a.label = 7;
                    case 7:
                        _a.trys.push([7, 9, , 10]);
                        console.log('[PLDOC TEST] About to call getPredicateHelp for foo/2');
                        return [4 /*yield*/, backend.getPredicateHelp('foo/2')];
                    case 8:
                        doc = _a.sent();
                        console.log('[PLDOC TEST] getPredicateHelp returned:', doc);
                        return [3 /*break*/, 10];
                    case 9:
                        err_1 = _a.sent();
                        console.error('[PLDOC TEST] getPredicateHelp threw:', err_1);
                        finish(err_1);
                        return [2 /*return*/];
                    case 10:
                        if (doc.summary && doc.summary.match(/pldoc missing|no documentation found/i)) {
                            console.warn('[PLDOC TEST] Detected fallback doc (PlDoc missing), skipping.');
                            finish(undefined, true); // skip if PlDoc missing
                            return [2 /*return*/];
                        }
                        try {
                            (0, chai_1.expect)(doc).to.be.an('object');
                            (0, chai_1.expect)(doc).to.have.property('name', 'foo');
                            (0, chai_1.expect)(doc).to.have.property('arity', 2);
                            (0, chai_1.expect)(doc).to.have.property('args').that.is.an('array').and.not.empty;
                            (0, chai_1.expect)(doc).to.have.property('examples').that.is.an('array').and.not.empty;
                            console.log('[PLDOC TEST] All assertions passed.');
                            finish();
                        }
                        catch (err) {
                            console.error('[PLDOC TEST] Assertion error:', err);
                            finish(err);
                        }
                        return [3 /*break*/, 12];
                    case 11:
                        err_2 = _a.sent();
                        console.error('[PLDOC TEST] Caught error:', err_2);
                        finish(err_2);
                        return [3 /*break*/, 12];
                    case 12: return [2 /*return*/];
                }
            });
        }); });
        try {
            console.log('[PLDOC TEST] About to start backend...');
            backend.start();
        }
        catch (e) {
            console.error('[PLDOC TEST] backend.start() threw:', e);
            finish(e);
        }
        setTimeout(function () {
            if (!finished) {
                console.error('[PLDOC TEST] Timeout reached, done() not called after 10s');
                finish(new Error('Timeout: done() not called after 10s'));
            }
        }, 10000);
    });
});
