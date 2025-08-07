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
var mocha_1 = require("mocha");
var sinon = require("sinon");
var sinon_chai_1 = require("sinon-chai");
var prologPackageManager_js_1 = require("../src/features/prologPackageManager.js");
var prologBackend_js_1 = require("../src/prologBackend.js");
chai_1.default.use(sinon_chai_1.default);
(0, mocha_1.describe)('PrologPackageManager', function () {
    var packageManager;
    var mockBackend;
    var windowStub;
    (0, mocha_1.beforeEach)(function () {
        // Create mock backend
        mockBackend = sinon.createStubInstance(prologBackend_js_1.PrologBackend);
        packageManager = new prologPackageManager_js_1.PrologPackageManager(mockBackend);
        // Mock VS Code window API
        windowStub = {
            showErrorMessage: sinon.stub(),
            showWarningMessage: sinon.stub(),
            showInformationMessage: sinon.stub(),
            withProgress: sinon.stub(),
        };
        // Mock the VS Code module
        var vscode;
        before(function () { return __awaiter(void 0, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, Promise.resolve().then(function () { return require('vscode'); })];
                    case 1:
                        vscode = _a.sent();
                        return [2 /*return*/];
                }
            });
        }); });
        if (vscode && vscode.window) {
            Object.assign(vscode.window, windowStub);
        }
    });
    (0, mocha_1.afterEach)(function () {
        sinon.restore();
    });
    (0, mocha_1.describe)('listAvailablePacks', function () {
        (0, mocha_1.it)('should return empty array when no packs are available', function () { return __awaiter(void 0, void 0, void 0, function () {
            var packs;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockBackend.sendRequest.resolves({
                            status: 'ok',
                            results: [],
                        });
                        return [4 /*yield*/, packageManager.listAvailablePacks()];
                    case 1:
                        packs = _a.sent();
                        (0, chai_1.expect)(packs).to.be.an('array').that.is.empty;
                        return [2 /*return*/];
                }
            });
        }); });
        (0, mocha_1.it)('should parse and return available packs', function () { return __awaiter(void 0, void 0, void 0, function () {
            var mockResponse, packs;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockResponse = {
                            status: 'ok',
                            results: [
                                [
                                    {
                                        functor: 'pack',
                                        args: [
                                            'http',
                                            'HTTP client and server library',
                                            '1.0.0',
                                            'Jan Wielemaker',
                                            'https://swi-prolog.org',
                                            'https://github.com/SWI-Prolog/packages-http',
                                        ],
                                    },
                                    {
                                        functor: 'pack',
                                        args: [
                                            'clpfd',
                                            'Constraint Logic Programming over Finite Domains',
                                            '2.1.0',
                                            'Markus Triska',
                                            'https://swi-prolog.org',
                                            'https://github.com/triska/clpfd',
                                        ],
                                    },
                                ],
                            ],
                        };
                        mockBackend.sendRequest.resolves(mockResponse);
                        return [4 /*yield*/, packageManager.listAvailablePacks()];
                    case 1:
                        packs = _a.sent();
                        (0, chai_1.expect)(packs).to.have.length(2);
                        (0, chai_1.expect)(packs[0]).to.deep.include({
                            name: 'http',
                            title: 'HTTP client and server library',
                            version: '1.0.0',
                            author: 'Jan Wielemaker',
                        });
                        (0, chai_1.expect)(packs[1]).to.deep.include({
                            name: 'clpfd',
                            title: 'Constraint Logic Programming over Finite Domains',
                            version: '2.1.0',
                            author: 'Markus Triska',
                        });
                        return [2 /*return*/];
                }
            });
        }); });
        (0, mocha_1.it)('should handle backend errors gracefully', function () { return __awaiter(void 0, void 0, void 0, function () {
            var packs;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockBackend.sendRequest.rejects(new Error('Backend connection failed'));
                        return [4 /*yield*/, packageManager.listAvailablePacks()];
                    case 1:
                        packs = _a.sent();
                        (0, chai_1.expect)(packs).to.be.an('array').that.is.empty;
                        (0, chai_1.expect)(windowStub.showErrorMessage).to.have.been.calledOnce;
                        return [2 /*return*/];
                }
            });
        }); });
    });
    (0, mocha_1.describe)('listInstalledPacks', function () {
        (0, mocha_1.it)('should return installed packs with installed flag set to true', function () { return __awaiter(void 0, void 0, void 0, function () {
            var mockResponse, packs;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockResponse = {
                            status: 'ok',
                            results: [
                                [
                                    {
                                        functor: 'pack',
                                        args: [
                                            'http',
                                            'HTTP client and server library',
                                            '1.0.0',
                                            'Jan Wielemaker',
                                            'https://swi-prolog.org',
                                        ],
                                    },
                                ],
                            ],
                        };
                        mockBackend.sendRequest.resolves(mockResponse);
                        return [4 /*yield*/, packageManager.listInstalledPacks()];
                    case 1:
                        packs = _a.sent();
                        (0, chai_1.expect)(packs).to.have.length(1);
                        (0, chai_1.expect)(packs[0]).to.deep.include({
                            name: 'http',
                            installed: true,
                        });
                        return [2 /*return*/];
                }
            });
        }); });
    });
    (0, mocha_1.describe)('installPack', function () {
        (0, mocha_1.beforeEach)(function () {
            // Mock withProgress to immediately call the callback
            windowStub.withProgress.callsFake(function (options, callback) {
                var mockProgress = { report: sinon.stub() };
                var mockToken = { isCancellationRequested: false };
                return callback(mockProgress, mockToken);
            });
        });
        (0, mocha_1.it)('should reject invalid pack names', function () { return __awaiter(void 0, void 0, void 0, function () {
            var result;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, packageManager.installPack('invalid@pack!name')];
                    case 1:
                        result = _a.sent();
                        (0, chai_1.expect)(result.success).to.be.false;
                        (0, chai_1.expect)(result.message).to.include('Invalid pack name');
                        return [2 /*return*/];
                }
            });
        }); });
        (0, mocha_1.it)('should successfully install a valid pack', function () { return __awaiter(void 0, void 0, void 0, function () {
            var result;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        // Mock available packs check
                        mockBackend.sendRequest.onFirstCall().resolves({
                            status: 'ok',
                            results: [
                                [
                                    {
                                        functor: 'pack',
                                        args: [
                                            'http',
                                            'HTTP client and server library',
                                            '1.0.0',
                                            'Jan Wielemaker',
                                            'https://swi-prolog.org',
                                            'https://github.com/SWI-Prolog/packages-http',
                                        ],
                                    },
                                ],
                            ],
                        });
                        // Mock installation
                        mockBackend.sendRequest.onSecondCall().resolves({
                            status: 'ok',
                        });
                        // Mock verification
                        mockBackend.sendRequest.onThirdCall().resolves({
                            status: 'ok',
                            results: [
                                [
                                    {
                                        functor: 'pack',
                                        args: [
                                            'http',
                                            'HTTP client and server library',
                                            '1.0.0',
                                            'Jan Wielemaker',
                                            'https://swi-prolog.org',
                                        ],
                                    },
                                ],
                            ],
                        });
                        return [4 /*yield*/, packageManager.installPack('http')];
                    case 1:
                        result = _a.sent();
                        (0, chai_1.expect)(result.success).to.be.true;
                        (0, chai_1.expect)(result.message).to.include('installed successfully');
                        return [2 /*return*/];
                }
            });
        }); });
        (0, mocha_1.it)('should handle pack not found error', function () { return __awaiter(void 0, void 0, void 0, function () {
            var result;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        // Mock empty available packs
                        mockBackend.sendRequest.resolves({
                            status: 'ok',
                            results: [],
                        });
                        return [4 /*yield*/, packageManager.installPack('nonexistent')];
                    case 1:
                        result = _a.sent();
                        (0, chai_1.expect)(result.success).to.be.false;
                        (0, chai_1.expect)(result.message).to.include('not found in available packs');
                        return [2 /*return*/];
                }
            });
        }); });
        (0, mocha_1.it)('should handle installation failure', function () { return __awaiter(void 0, void 0, void 0, function () {
            var result;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        // Mock available packs check
                        mockBackend.sendRequest.onFirstCall().resolves({
                            status: 'ok',
                            results: [
                                [
                                    {
                                        functor: 'pack',
                                        args: [
                                            'http',
                                            'HTTP client and server library',
                                            '1.0.0',
                                            'Jan Wielemaker',
                                            'https://swi-prolog.org',
                                            'https://github.com/SWI-Prolog/packages-http',
                                        ],
                                    },
                                ],
                            ],
                        });
                        // Mock installation failure
                        mockBackend.sendRequest.onSecondCall().resolves({
                            status: 'error',
                            error: 'Installation failed',
                            message: 'Network error',
                        });
                        return [4 /*yield*/, packageManager.installPack('http')];
                    case 1:
                        result = _a.sent();
                        (0, chai_1.expect)(result.success).to.be.false;
                        (0, chai_1.expect)(result.message).to.include('Failed to install');
                        return [2 /*return*/];
                }
            });
        }); });
    });
    (0, mocha_1.describe)('uninstallPack', function () {
        (0, mocha_1.beforeEach)(function () {
            windowStub.withProgress.callsFake(function (options, callback) {
                var mockProgress = { report: sinon.stub() };
                return callback(mockProgress);
            });
        });
        (0, mocha_1.it)('should successfully uninstall an installed pack', function () { return __awaiter(void 0, void 0, void 0, function () {
            var result;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        // Mock installed packs check
                        mockBackend.sendRequest.onFirstCall().resolves({
                            status: 'ok',
                            results: [
                                [
                                    {
                                        functor: 'pack',
                                        args: [
                                            'http',
                                            'HTTP client and server library',
                                            '1.0.0',
                                            'Jan Wielemaker',
                                            'https://swi-prolog.org',
                                        ],
                                    },
                                ],
                            ],
                        });
                        // Mock uninstallation
                        mockBackend.sendRequest.onSecondCall().resolves({
                            status: 'ok',
                        });
                        // Mock verification (empty results = pack removed)
                        mockBackend.sendRequest.onThirdCall().resolves({
                            status: 'ok',
                            results: [],
                        });
                        return [4 /*yield*/, packageManager.uninstallPack('http')];
                    case 1:
                        result = _a.sent();
                        (0, chai_1.expect)(result.success).to.be.true;
                        (0, chai_1.expect)(result.message).to.include('uninstalled successfully');
                        return [2 /*return*/];
                }
            });
        }); });
        (0, mocha_1.it)('should handle pack not installed error', function () { return __awaiter(void 0, void 0, void 0, function () {
            var result;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        // Mock empty installed packs
                        mockBackend.sendRequest.resolves({
                            status: 'ok',
                            results: [],
                        });
                        return [4 /*yield*/, packageManager.uninstallPack('nonexistent')];
                    case 1:
                        result = _a.sent();
                        (0, chai_1.expect)(result.success).to.be.false;
                        (0, chai_1.expect)(result.message).to.include('is not installed');
                        return [2 /*return*/];
                }
            });
        }); });
    });
    (0, mocha_1.describe)('updatePack', function () {
        (0, mocha_1.beforeEach)(function () {
            windowStub.withProgress.callsFake(function (options, callback) {
                var mockProgress = { report: sinon.stub() };
                var mockToken = { isCancellationRequested: false };
                return callback(mockProgress, mockToken);
            });
        });
        (0, mocha_1.it)('should successfully update a pack', function () { return __awaiter(void 0, void 0, void 0, function () {
            var result;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockBackend.sendRequest.resolves({
                            status: 'ok',
                        });
                        return [4 /*yield*/, packageManager.updatePack('http')];
                    case 1:
                        result = _a.sent();
                        (0, chai_1.expect)(result.success).to.be.true;
                        (0, chai_1.expect)(result.message).to.include('updated successfully');
                        return [2 /*return*/];
                }
            });
        }); });
        (0, mocha_1.it)('should handle update failure', function () { return __awaiter(void 0, void 0, void 0, function () {
            var result;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockBackend.sendRequest.resolves({
                            status: 'error',
                            error: 'Update failed',
                        });
                        return [4 /*yield*/, packageManager.updatePack('http')];
                    case 1:
                        result = _a.sent();
                        (0, chai_1.expect)(result.success).to.be.false;
                        (0, chai_1.expect)(result.message).to.include('Failed to update');
                        return [2 /*return*/];
                }
            });
        }); });
    });
    (0, mocha_1.describe)('getPackInfo', function () {
        (0, mocha_1.it)('should return detailed pack information', function () { return __awaiter(void 0, void 0, void 0, function () {
            var mockResponse, packInfo;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockResponse = {
                            status: 'ok',
                            results: [
                                [
                                    { functor: '-', args: ['title', 'HTTP client and server library'] },
                                    { functor: '-', args: ['version', '1.0.0'] },
                                    { functor: '-', args: ['author', 'Jan Wielemaker'] },
                                    { functor: '-', args: ['home', 'https://swi-prolog.org'] },
                                    { functor: '-', args: ['requires', ['ssl', 'uri']] },
                                ],
                            ],
                        };
                        mockBackend.sendRequest.resolves(mockResponse);
                        return [4 /*yield*/, packageManager.getPackInfo('http')];
                    case 1:
                        packInfo = _a.sent();
                        (0, chai_1.expect)(packInfo).to.not.be.null;
                        (0, chai_1.expect)(packInfo.name).to.equal('http');
                        (0, chai_1.expect)(packInfo.title).to.equal('HTTP client and server library');
                        (0, chai_1.expect)(packInfo.version).to.equal('1.0.0');
                        (0, chai_1.expect)(packInfo.author).to.equal('Jan Wielemaker');
                        (0, chai_1.expect)(packInfo.requires).to.deep.equal(['ssl', 'uri']);
                        return [2 /*return*/];
                }
            });
        }); });
        (0, mocha_1.it)('should return null for non-existent pack', function () { return __awaiter(void 0, void 0, void 0, function () {
            var packInfo;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockBackend.sendRequest.resolves({
                            status: 'ok',
                            results: [],
                        });
                        return [4 /*yield*/, packageManager.getPackInfo('nonexistent')];
                    case 1:
                        packInfo = _a.sent();
                        (0, chai_1.expect)(packInfo).to.be.null;
                        return [2 /*return*/];
                }
            });
        }); });
    });
    (0, mocha_1.describe)('searchPacks', function () {
        (0, mocha_1.it)('should return search results', function () { return __awaiter(void 0, void 0, void 0, function () {
            var mockResponse, packs;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockResponse = {
                            status: 'ok',
                            results: [
                                [
                                    {
                                        functor: 'pack',
                                        args: [
                                            'http',
                                            'HTTP client and server library',
                                            '1.0.0',
                                            'Jan Wielemaker',
                                            'https://swi-prolog.org',
                                            'https://github.com/SWI-Prolog/packages-http',
                                        ],
                                    },
                                ],
                            ],
                        };
                        mockBackend.sendRequest.resolves(mockResponse);
                        return [4 /*yield*/, packageManager.searchPacks('http')];
                    case 1:
                        packs = _a.sent();
                        (0, chai_1.expect)(packs).to.have.length(1);
                        (0, chai_1.expect)(packs[0].name).to.equal('http');
                        return [2 /*return*/];
                }
            });
        }); });
        (0, mocha_1.it)('should return empty array for empty search term', function () { return __awaiter(void 0, void 0, void 0, function () {
            var packs;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, packageManager.searchPacks('')];
                    case 1:
                        packs = _a.sent();
                        (0, chai_1.expect)(packs).to.be.an('array').that.is.empty;
                        return [2 /*return*/];
                }
            });
        }); });
    });
    (0, mocha_1.describe)('checkOutdatedPacks', function () {
        (0, mocha_1.it)('should return outdated packs', function () { return __awaiter(void 0, void 0, void 0, function () {
            var mockResponse, outdatedPacks;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockResponse = {
                            status: 'ok',
                            results: [
                                [
                                    {
                                        functor: 'pack',
                                        args: ['http', '1.0.0', '1.1.0'],
                                    },
                                ],
                            ],
                        };
                        mockBackend.sendRequest.resolves(mockResponse);
                        return [4 /*yield*/, packageManager.checkOutdatedPacks()];
                    case 1:
                        outdatedPacks = _a.sent();
                        (0, chai_1.expect)(outdatedPacks).to.have.length(1);
                        (0, chai_1.expect)(outdatedPacks[0].outdated).to.be.true;
                        return [2 /*return*/];
                }
            });
        }); });
    });
    (0, mocha_1.describe)('validatePackSecurity', function () {
        (0, mocha_1.it)('should validate pack security and return warnings', function () { return __awaiter(void 0, void 0, void 0, function () {
            var mockPackInfo, result;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockPackInfo = {
                            name: 'test_pack',
                            home: 'http://example.com', // Non-HTTPS, non-official
                            download: 'http://example.com/download', // Non-HTTPS
                        };
                        // Stub getPackInfo method
                        sinon.stub(packageManager, 'getPackInfo').resolves(mockPackInfo);
                        return [4 /*yield*/, packageManager.validatePackSecurity('test_pack')];
                    case 1:
                        result = _a.sent();
                        (0, chai_1.expect)(result.safe).to.be.false;
                        (0, chai_1.expect)(result.warnings).to.have.length.greaterThan(0);
                        (0, chai_1.expect)(result.warnings.some(function (w) { return w.includes('not from the official SWI-Prolog repository'); }))
                            .to.be.true;
                        (0, chai_1.expect)(result.warnings.some(function (w) { return w.includes('not using HTTPS'); })).to.be.true;
                        return [2 /*return*/];
                }
            });
        }); });
        (0, mocha_1.it)('should mark official packs as safe', function () { return __awaiter(void 0, void 0, void 0, function () {
            var mockPackInfo, result;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockPackInfo = {
                            name: 'http',
                            home: 'https://swi-prolog.org/pack/http',
                            download: 'https://github.com/SWI-Prolog/packages-http',
                        };
                        sinon.stub(packageManager, 'getPackInfo').resolves(mockPackInfo);
                        return [4 /*yield*/, packageManager.validatePackSecurity('http')];
                    case 1:
                        result = _a.sent();
                        (0, chai_1.expect)(result.safe).to.be.true;
                        (0, chai_1.expect)(result.warnings).to.have.length(0);
                        return [2 /*return*/];
                }
            });
        }); });
    });
    (0, mocha_1.describe)('pack server management', function () {
        (0, mocha_1.it)('should add custom pack servers', function () {
            var initialServers = packageManager.getPackServers();
            var newServer = 'https://custom-pack-server.com';
            packageManager.addPackServer(newServer);
            var updatedServers = packageManager.getPackServers();
            (0, chai_1.expect)(updatedServers).to.have.length(initialServers.length + 1);
            (0, chai_1.expect)(updatedServers).to.include(newServer);
        });
        (0, mocha_1.it)('should not add duplicate servers', function () {
            var initialServers = packageManager.getPackServers();
            var existingServer = initialServers[0];
            packageManager.addPackServer(existingServer);
            var updatedServers = packageManager.getPackServers();
            (0, chai_1.expect)(updatedServers).to.have.length(initialServers.length);
        });
        (0, mocha_1.it)('should remove custom pack servers but not default', function () {
            var customServer = 'https://custom-pack-server.com';
            packageManager.addPackServer(customServer);
            var beforeRemoval = packageManager.getPackServers();
            packageManager.removePackServer(customServer);
            var afterRemoval = packageManager.getPackServers();
            (0, chai_1.expect)(afterRemoval).to.have.length(beforeRemoval.length - 1);
            (0, chai_1.expect)(afterRemoval).to.not.include(customServer);
            // Try to remove default server (should not work)
            var defaultServer = afterRemoval[0];
            packageManager.removePackServer(defaultServer);
            var afterDefaultRemoval = packageManager.getPackServers();
            (0, chai_1.expect)(afterDefaultRemoval).to.include(defaultServer);
        });
    });
    (0, mocha_1.describe)('pack name validation', function () {
        (0, mocha_1.it)('should accept valid pack names', function () {
            var validNames = ['http', 'clpfd', 'pack_name', 'pack-name', 'pack123'];
            for (var _i = 0, validNames_1 = validNames; _i < validNames_1.length; _i++) {
                var name_1 = validNames_1[_i];
                var result = packageManager.validatePackName(name_1);
                (0, chai_1.expect)(result, "".concat(name_1, " should be valid")).to.be.true;
            }
        });
        (0, mocha_1.it)('should reject invalid pack names', function () {
            var invalidNames = ['pack@name', 'pack name', 'pack.name', 'pack/name', ''];
            for (var _i = 0, invalidNames_1 = invalidNames; _i < invalidNames_1.length; _i++) {
                var name_2 = invalidNames_1[_i];
                var result = packageManager.validatePackName(name_2);
                (0, chai_1.expect)(result, "".concat(name_2, " should be invalid")).to.be.false;
            }
        });
    });
});
