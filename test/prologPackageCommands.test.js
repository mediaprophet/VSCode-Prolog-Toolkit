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
var prologPackageCommands_js_1 = require("../src/features/prologPackageCommands.js");
var prologPackageManager_js_1 = require("../src/features/prologPackageManager.js");
(0, mocha_1.describe)('PrologPackageCommands', function () {
    var packageCommands;
    var mockPackageManager;
    var windowStub;
    var mockPacks = [
        {
            name: 'http',
            title: 'HTTP client and server library',
            version: '1.0.0',
            author: 'Jan Wielemaker',
            home: 'https://swi-prolog.org',
            installed: true,
        },
        {
            name: 'clpfd',
            title: 'Constraint Logic Programming over Finite Domains',
            version: '2.1.0',
            author: 'Markus Triska',
            home: 'https://swi-prolog.org',
            installed: false,
        },
    ];
    (0, mocha_1.beforeEach)(function () {
        // Create mock package manager
        mockPackageManager = sinon.createStubInstance(prologPackageManager_js_1.PrologPackageManager);
        packageCommands = new prologPackageCommands_js_1.PrologPackageCommands(mockPackageManager);
        // Mock VS Code window API
        windowStub = {
            showErrorMessage: sinon.stub(),
            showWarningMessage: sinon.stub(),
            showInformationMessage: sinon.stub(),
            showQuickPick: sinon.stub(),
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
    (0, mocha_1.describe)('handlePackageCommand', function () {
        (0, mocha_1.describe)('list command', function () {
            (0, mocha_1.it)('should list installed packs by default', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            mockPackageManager.listInstalledPacks.resolves([mockPacks[0]]);
                            return [4 /*yield*/, packageCommands.handlePackageCommand('list', [])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('Installed Packs');
                            (0, chai_1.expect)(result).to.include('http');
                            (0, chai_1.expect)(result).to.include('v1.0.0');
                            (0, chai_1.expect)(mockPackageManager.listInstalledPacks).to.have.been.calledOnce;
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should list available packs when requested', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            mockPackageManager.listAvailablePacks.resolves(mockPacks);
                            return [4 /*yield*/, packageCommands.handlePackageCommand('list', ['available'])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('Available Packs');
                            (0, chai_1.expect)(result).to.include('http');
                            (0, chai_1.expect)(result).to.include('clpfd');
                            (0, chai_1.expect)(mockPackageManager.listAvailablePacks).to.have.been.calledOnce;
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should handle empty pack lists', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            mockPackageManager.listInstalledPacks.resolves([]);
                            return [4 /*yield*/, packageCommands.handlePackageCommand('list', [])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('No packs are currently installed');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should limit available packs display to 20', function () { return __awaiter(void 0, void 0, void 0, function () {
                var manyPacks, result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            manyPacks = Array.from({ length: 25 }, function (_, i) { return ({
                                name: "pack".concat(i),
                                version: '1.0.0',
                                title: "Pack ".concat(i),
                                author: 'Test Author',
                                installed: false,
                            }); });
                            mockPackageManager.listAvailablePacks.resolves(manyPacks);
                            return [4 /*yield*/, packageCommands.handlePackageCommand('list', ['available'])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('... and 5 more packs');
                            return [2 /*return*/];
                    }
                });
            }); });
        });
        (0, mocha_1.describe)('install command', function () {
            (0, mocha_1.it)('should require a pack name', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, packageCommands.handlePackageCommand('install', [])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('Please specify a pack name');
                            (0, chai_1.expect)(result).to.include('Usage:');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should install a pack successfully', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            mockPackageManager.validatePackSecurity.resolves({ safe: true, warnings: [] });
                            mockPackageManager.installPack.resolves({
                                success: true,
                                message: 'Pack installed successfully',
                            });
                            return [4 /*yield*/, packageCommands.handlePackageCommand('install', ['http'])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('✅');
                            (0, chai_1.expect)(result).to.include('Pack installed successfully');
                            (0, chai_1.expect)(mockPackageManager.validatePackSecurity).to.have.been.calledWith('http');
                            (0, chai_1.expect)(mockPackageManager.installPack).to.have.been.calledWith('http');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should handle security warnings', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            mockPackageManager.validatePackSecurity.resolves({
                                safe: false,
                                warnings: ['Pack is not from official repository'],
                            });
                            windowStub.showWarningMessage.resolves('Cancel');
                            return [4 /*yield*/, packageCommands.handlePackageCommand('install', ['unsafe_pack'])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('cancelled due to security concerns');
                            (0, chai_1.expect)(mockPackageManager.installPack).to.not.have.been.called;
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should proceed with installation despite warnings if user confirms', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            mockPackageManager.validatePackSecurity.resolves({
                                safe: false,
                                warnings: ['Pack is not from official repository'],
                            });
                            windowStub.showWarningMessage.resolves('Yes, Install Anyway');
                            mockPackageManager.installPack.resolves({ success: true, message: 'Pack installed' });
                            return [4 /*yield*/, packageCommands.handlePackageCommand('install', ['unsafe_pack'])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('✅');
                            (0, chai_1.expect)(mockPackageManager.installPack).to.have.been.calledWith('unsafe_pack');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should handle installation failures', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            mockPackageManager.validatePackSecurity.resolves({ safe: true, warnings: [] });
                            mockPackageManager.installPack.resolves({
                                success: false,
                                message: 'Installation failed',
                                details: 'Network error',
                            });
                            return [4 /*yield*/, packageCommands.handlePackageCommand('install', ['http'])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('❌');
                            (0, chai_1.expect)(result).to.include('Installation failed');
                            (0, chai_1.expect)(result).to.include('Network error');
                            return [2 /*return*/];
                    }
                });
            }); });
        });
        (0, mocha_1.describe)('uninstall command', function () {
            (0, mocha_1.it)('should require a pack name', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, packageCommands.handlePackageCommand('uninstall', [])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('Please specify a pack name');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should uninstall a pack with confirmation', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            windowStub.showWarningMessage.resolves('Yes, Uninstall');
                            mockPackageManager.uninstallPack.resolves({ success: true, message: 'Pack uninstalled' });
                            return [4 /*yield*/, packageCommands.handlePackageCommand('uninstall', ['http'])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('✅');
                            (0, chai_1.expect)(result).to.include('Pack uninstalled');
                            (0, chai_1.expect)(mockPackageManager.uninstallPack).to.have.been.calledWith('http');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should cancel uninstallation if user declines', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            windowStub.showWarningMessage.resolves('Cancel');
                            return [4 /*yield*/, packageCommands.handlePackageCommand('uninstall', ['http'])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('cancelled');
                            (0, chai_1.expect)(mockPackageManager.uninstallPack).to.not.have.been.called;
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should handle both uninstall and remove commands', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            windowStub.showWarningMessage.resolves('Yes, Uninstall');
                            mockPackageManager.uninstallPack.resolves({ success: true, message: 'Pack removed' });
                            return [4 /*yield*/, packageCommands.handlePackageCommand('remove', ['http'])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('✅');
                            (0, chai_1.expect)(mockPackageManager.uninstallPack).to.have.been.calledWith('http');
                            return [2 /*return*/];
                    }
                });
            }); });
        });
        (0, mocha_1.describe)('update command', function () {
            (0, mocha_1.it)('should update all outdated packs when no pack specified', function () { return __awaiter(void 0, void 0, void 0, function () {
                var outdatedPacks, result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            outdatedPacks = [
                                { name: 'http', version: '1.0.0', outdated: true },
                                { name: 'clpfd', version: '2.0.0', outdated: true },
                            ];
                            mockPackageManager.checkOutdatedPacks.resolves(outdatedPacks);
                            windowStub.showInformationMessage.resolves('Yes, Update All');
                            mockPackageManager.updatePack.onFirstCall().resolves({ success: true, message: 'Updated' });
                            mockPackageManager.updatePack
                                .onSecondCall()
                                .resolves({ success: true, message: 'Updated' });
                            return [4 /*yield*/, packageCommands.handlePackageCommand('update', [])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('Updating 2 pack(s)');
                            (0, chai_1.expect)(result).to.include('http: ✅');
                            (0, chai_1.expect)(result).to.include('clpfd: ✅');
                            (0, chai_1.expect)(mockPackageManager.updatePack).to.have.been.calledTwice;
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should report when all packs are up to date', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            mockPackageManager.checkOutdatedPacks.resolves([]);
                            return [4 /*yield*/, packageCommands.handlePackageCommand('update', [])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('All installed packs are up to date');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should update a specific pack', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            mockPackageManager.updatePack.resolves({ success: true, message: 'Pack updated' });
                            return [4 /*yield*/, packageCommands.handlePackageCommand('update', ['http'])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('✅');
                            (0, chai_1.expect)(result).to.include('Pack updated');
                            (0, chai_1.expect)(mockPackageManager.updatePack).to.have.been.calledWith('http');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should handle both update and upgrade commands', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            mockPackageManager.updatePack.resolves({ success: true, message: 'Pack upgraded' });
                            return [4 /*yield*/, packageCommands.handlePackageCommand('upgrade', ['http'])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('✅');
                            (0, chai_1.expect)(mockPackageManager.updatePack).to.have.been.calledWith('http');
                            return [2 /*return*/];
                    }
                });
            }); });
        });
        (0, mocha_1.describe)('info command', function () {
            (0, mocha_1.it)('should require a pack name', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, packageCommands.handlePackageCommand('info', [])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('Please specify a pack name');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should display pack information', function () { return __awaiter(void 0, void 0, void 0, function () {
                var packInfo, result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            packInfo = {
                                name: 'http',
                                title: 'HTTP client and server library',
                                version: '1.0.0',
                                author: 'Jan Wielemaker',
                                description: 'A comprehensive HTTP library',
                                home: 'https://swi-prolog.org',
                                requires: ['ssl', 'uri'],
                                installed: true,
                            };
                            mockPackageManager.getPackInfo.resolves(packInfo);
                            return [4 /*yield*/, packageCommands.handlePackageCommand('info', ['http'])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('Pack Information: http');
                            (0, chai_1.expect)(result).to.include('HTTP client and server library');
                            (0, chai_1.expect)(result).to.include('v1.0.0');
                            (0, chai_1.expect)(result).to.include('Jan Wielemaker');
                            (0, chai_1.expect)(result).to.include('ssl, uri');
                            (0, chai_1.expect)(result).to.include('✅ Yes');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should handle pack not found', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            mockPackageManager.getPackInfo.resolves(null);
                            return [4 /*yield*/, packageCommands.handlePackageCommand('info', ['nonexistent'])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('not found or information unavailable');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should handle both info and show commands', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            mockPackageManager.getPackInfo.resolves(mockPacks[0]);
                            return [4 /*yield*/, packageCommands.handlePackageCommand('show', ['http'])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('Pack Information');
                            (0, chai_1.expect)(mockPackageManager.getPackInfo).to.have.been.calledWith('http');
                            return [2 /*return*/];
                    }
                });
            }); });
        });
        (0, mocha_1.describe)('search command', function () {
            (0, mocha_1.it)('should require a search keyword', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, packageCommands.handlePackageCommand('search', [])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('Please specify a search keyword');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should return search results', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            mockPackageManager.searchPacks.resolves([mockPacks[0]]);
                            return [4 /*yield*/, packageCommands.handlePackageCommand('search', ['http'])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include("Search Results for 'http'");
                            (0, chai_1.expect)(result).to.include('http');
                            (0, chai_1.expect)(result).to.include('v1.0.0');
                            (0, chai_1.expect)(mockPackageManager.searchPacks).to.have.been.calledWith('http');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should handle no search results', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            mockPackageManager.searchPacks.resolves([]);
                            return [4 /*yield*/, packageCommands.handlePackageCommand('search', ['nonexistent'])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('No packs found matching');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should join multiple search terms', function () { return __awaiter(void 0, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            mockPackageManager.searchPacks.resolves([]);
                            return [4 /*yield*/, packageCommands.handlePackageCommand('search', ['http', 'client'])];
                        case 1:
                            _a.sent();
                            (0, chai_1.expect)(mockPackageManager.searchPacks).to.have.been.calledWith('http client');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should limit search results to 10', function () { return __awaiter(void 0, void 0, void 0, function () {
                var manyPacks, result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            manyPacks = Array.from({ length: 15 }, function (_, i) { return ({
                                name: "pack".concat(i),
                                version: '1.0.0',
                                title: "Pack ".concat(i),
                                author: 'Test Author',
                                installed: false,
                            }); });
                            mockPackageManager.searchPacks.resolves(manyPacks);
                            return [4 /*yield*/, packageCommands.handlePackageCommand('search', ['test'])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('... and 5 more results');
                            return [2 /*return*/];
                    }
                });
            }); });
        });
        (0, mocha_1.describe)('outdated command', function () {
            (0, mocha_1.it)('should list outdated packs', function () { return __awaiter(void 0, void 0, void 0, function () {
                var outdatedPacks, result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            outdatedPacks = [
                                { name: 'http', version: '1.0.0', outdated: true, title: 'HTTP library' },
                            ];
                            mockPackageManager.checkOutdatedPacks.resolves(outdatedPacks);
                            return [4 /*yield*/, packageCommands.handlePackageCommand('outdated', [])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('Outdated Packs');
                            (0, chai_1.expect)(result).to.include('http');
                            (0, chai_1.expect)(result).to.include('Use `/prolog pack update`');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should report when no packs are outdated', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            mockPackageManager.checkOutdatedPacks.resolves([]);
                            return [4 /*yield*/, packageCommands.handlePackageCommand('outdated', [])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('All installed packs are up to date');
                            return [2 /*return*/];
                    }
                });
            }); });
        });
        (0, mocha_1.describe)('servers command', function () {
            (0, mocha_1.it)('should list configured servers', function () { return __awaiter(void 0, void 0, void 0, function () {
                var servers, result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            servers = ['https://swi-prolog.org/pack/list', 'https://custom-server.com'];
                            mockPackageManager.getPackServers.returns(servers);
                            return [4 /*yield*/, packageCommands.handlePackageCommand('servers', [])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('Configured Pack Servers');
                            (0, chai_1.expect)(result).to.include('swi-prolog.org');
                            (0, chai_1.expect)(result).to.include('custom-server.com');
                            (0, chai_1.expect)(result).to.include('(default)');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should add a new server', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, packageCommands.handlePackageCommand('servers', [
                                'add',
                                'https://new-server.com',
                            ])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('Added pack server');
                            (0, chai_1.expect)(result).to.include('https://new-server.com');
                            (0, chai_1.expect)(mockPackageManager.addPackServer).to.have.been.calledWith('https://new-server.com');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should reject invalid URLs when adding', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, packageCommands.handlePackageCommand('servers', [
                                'add',
                                'invalid-url',
                            ])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('Invalid URL');
                            return [2 /*return*/];
                    }
                });
            }); });
            (0, mocha_1.it)('should remove a server', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, packageCommands.handlePackageCommand('servers', [
                                'remove',
                                'https://old-server.com',
                            ])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('Removed pack server');
                            (0, chai_1.expect)(mockPackageManager.removePackServer).to.have.been.calledWith('https://old-server.com');
                            return [2 /*return*/];
                    }
                });
            }); });
        });
        (0, mocha_1.describe)('help command', function () {
            (0, mocha_1.it)('should return help message for unknown commands', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, packageCommands.handlePackageCommand('unknown', [])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('Prolog Package Manager Commands');
                            (0, chai_1.expect)(result).to.include('Basic Commands');
                            (0, chai_1.expect)(result).to.include('Examples');
                            return [2 /*return*/];
                    }
                });
            }); });
        });
        (0, mocha_1.describe)('error handling', function () {
            (0, mocha_1.it)('should handle exceptions gracefully', function () { return __awaiter(void 0, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            mockPackageManager.listInstalledPacks.rejects(new Error('Backend error'));
                            return [4 /*yield*/, packageCommands.handlePackageCommand('list', [])];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.include('❌ Command failed');
                            (0, chai_1.expect)(result).to.include('Backend error');
                            return [2 /*return*/];
                    }
                });
            }); });
        });
    });
    (0, mocha_1.describe)('showPackPicker', function () {
        (0, mocha_1.it)('should show available packs for installation', function () { return __awaiter(void 0, void 0, void 0, function () {
            var availablePacks, installedPacks;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        availablePacks = [mockPacks[1]];
                        installedPacks = [mockPacks[0]];
                        mockPackageManager.listAvailablePacks.resolves(mockPacks);
                        mockPackageManager.listInstalledPacks.resolves(installedPacks);
                        mockPackageManager.installPack.resolves({ success: true, message: 'Installed' });
                        windowStub.showQuickPick.resolves({
                            label: 'clpfd',
                            pack: mockPacks[1],
                        });
                        return [4 /*yield*/, packageCommands.showPackPicker()];
                    case 1:
                        _a.sent();
                        (0, chai_1.expect)(windowStub.showQuickPick).to.have.been.calledOnce;
                        (0, chai_1.expect)(mockPackageManager.installPack).to.have.been.calledWith('clpfd');
                        (0, chai_1.expect)(windowStub.showInformationMessage).to.have.been.calledWith('Installed');
                        return [2 /*return*/];
                }
            });
        }); });
        (0, mocha_1.it)('should handle installation failure in picker', function () { return __awaiter(void 0, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockPackageManager.listAvailablePacks.resolves([mockPacks[1]]);
                        mockPackageManager.listInstalledPacks.resolves([]);
                        mockPackageManager.installPack.resolves({ success: false, message: 'Failed' });
                        windowStub.showQuickPick.resolves({
                            label: 'clpfd',
                            pack: mockPacks[1],
                        });
                        return [4 /*yield*/, packageCommands.showPackPicker()];
                    case 1:
                        _a.sent();
                        (0, chai_1.expect)(windowStub.showErrorMessage).to.have.been.calledWith('Failed');
                        return [2 /*return*/];
                }
            });
        }); });
        (0, mocha_1.it)('should handle user cancellation', function () { return __awaiter(void 0, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockPackageManager.listAvailablePacks.resolves([mockPacks[1]]);
                        mockPackageManager.listInstalledPacks.resolves([]);
                        windowStub.showQuickPick.resolves(undefined); // User cancelled
                        return [4 /*yield*/, packageCommands.showPackPicker()];
                    case 1:
                        _a.sent();
                        (0, chai_1.expect)(mockPackageManager.installPack).to.not.have.been.called;
                        return [2 /*return*/];
                }
            });
        }); });
    });
    (0, mocha_1.describe)('showUninstallPicker', function () {
        (0, mocha_1.it)('should show installed packs for uninstallation', function () { return __awaiter(void 0, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockPackageManager.listInstalledPacks.resolves([mockPacks[0]]);
                        mockPackageManager.uninstallPack.resolves({ success: true, message: 'Uninstalled' });
                        windowStub.showQuickPick.resolves({
                            label: 'http',
                            pack: mockPacks[0],
                        });
                        windowStub.showWarningMessage.resolves('Yes, Uninstall');
                        return [4 /*yield*/, packageCommands.showUninstallPicker()];
                    case 1:
                        _a.sent();
                        (0, chai_1.expect)(windowStub.showQuickPick).to.have.been.calledOnce;
                        (0, chai_1.expect)(mockPackageManager.uninstallPack).to.have.been.calledWith('http');
                        (0, chai_1.expect)(windowStub.showInformationMessage).to.have.been.calledWith('Uninstalled');
                        return [2 /*return*/];
                }
            });
        }); });
        (0, mocha_1.it)('should handle no installed packs', function () { return __awaiter(void 0, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockPackageManager.listInstalledPacks.resolves([]);
                        return [4 /*yield*/, packageCommands.showUninstallPicker()];
                    case 1:
                        _a.sent();
                        (0, chai_1.expect)(windowStub.showInformationMessage).to.have.been.calledWith('No packs are currently installed.');
                        (0, chai_1.expect)(windowStub.showQuickPick).to.not.have.been.called;
                        return [2 /*return*/];
                }
            });
        }); });
        (0, mocha_1.it)('should handle user declining uninstallation', function () { return __awaiter(void 0, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockPackageManager.listInstalledPacks.resolves([mockPacks[0]]);
                        windowStub.showQuickPick.resolves({
                            label: 'http',
                            pack: mockPacks[0],
                        });
                        windowStub.showWarningMessage.resolves('Cancel');
                        return [4 /*yield*/, packageCommands.showUninstallPicker()];
                    case 1:
                        _a.sent();
                        (0, chai_1.expect)(mockPackageManager.uninstallPack).to.not.have.been.called;
                        return [2 /*return*/];
                }
            });
        }); });
    });
});
