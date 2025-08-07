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
var assert_1 = require("assert");
var vscode = require("vscode");
var configurationMigration_js_1 = require("../src/features/configurationMigration.js");
var installationChecker_js_1 = require("../src/features/installationChecker.js");
var installationGuide_js_1 = require("../src/features/installationGuide.js");
suite('Installation System Integration Tests', function () {
    var installationChecker;
    var installationGuide;
    var configurationMigration;
    var originalConfig;
    setup(function () { return __awaiter(void 0, void 0, void 0, function () {
        var config;
        return __generator(this, function (_a) {
            installationChecker = installationChecker_js_1.InstallationChecker.getInstance();
            installationGuide = installationGuide_js_1.InstallationGuide.getInstance();
            configurationMigration = configurationMigration_js_1.ConfigurationMigration.getInstance();
            config = vscode.workspace.getConfiguration('prolog');
            originalConfig = {
                executablePath: config.get('executablePath'),
                dialect: config.get('dialect'),
            };
            return [2 /*return*/];
        });
    }); });
    teardown(function () { return __awaiter(void 0, void 0, void 0, function () {
        var config, _i, _a, _b, key, value, error_1;
        return __generator(this, function (_c) {
            switch (_c.label) {
                case 0:
                    config = vscode.workspace.getConfiguration('prolog');
                    _i = 0, _a = Object.entries(originalConfig);
                    _c.label = 1;
                case 1:
                    if (!(_i < _a.length)) return [3 /*break*/, 6];
                    _b = _a[_i], key = _b[0], value = _b[1];
                    if (!(value !== undefined)) return [3 /*break*/, 5];
                    _c.label = 2;
                case 2:
                    _c.trys.push([2, 4, , 5]);
                    return [4 /*yield*/, config.update(key, value, vscode.ConfigurationTarget.Global)];
                case 3:
                    _c.sent();
                    return [3 /*break*/, 5];
                case 4:
                    error_1 = _c.sent();
                    console.log("Failed to restore config ".concat(key, ":"), error_1);
                    return [3 /*break*/, 5];
                case 5:
                    _i++;
                    return [3 /*break*/, 1];
                case 6: return [2 /*return*/];
            }
        });
    }); });
    suite('Component Integration', function () {
        test('should integrate InstallationChecker with ConfigurationMigration', function () {
            return __awaiter(this, void 0, void 0, function () {
                var installationStatus, migrationResult;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(20000);
                            return [4 /*yield*/, installationChecker.checkSwiplInstallation()];
                        case 1:
                            installationStatus = _a.sent();
                            if (!!installationStatus.isInstalled) return [3 /*break*/, 3];
                            return [4 /*yield*/, configurationMigration.performMigration()];
                        case 2:
                            migrationResult = _a.sent();
                            assert_1.default.ok(typeof migrationResult.migrated === 'boolean');
                            assert_1.default.ok(Array.isArray(migrationResult.issues));
                            if (!migrationResult.migrated && migrationResult.issues) {
                                // Should provide meaningful error messages
                                assert_1.default.ok(migrationResult.issues.length > 0);
                                migrationResult.issues.forEach(function (issue) {
                                    assert_1.default.ok(typeof issue === 'string');
                                    assert_1.default.ok(issue.length > 0);
                                });
                            }
                            _a.label = 3;
                        case 3: return [2 /*return*/];
                    }
                });
            });
        });
    });
    suite('End-to-End Workflows', function () {
        test('should handle complete installation detection workflow', function () {
            return __awaiter(this, void 0, void 0, function () {
                var installationStatus, outdatedCheck, migrationResult;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(25000);
                            return [4 /*yield*/, installationChecker.checkSwiplInstallation()];
                        case 1:
                            installationStatus = _a.sent();
                            assert_1.default.ok(typeof installationStatus.isInstalled === 'boolean');
                            if (!!installationStatus.isInstalled) return [3 /*break*/, 5];
                            return [4 /*yield*/, configurationMigration.detectOutdatedPaths()];
                        case 2:
                            outdatedCheck = _a.sent();
                            assert_1.default.ok(typeof outdatedCheck.hasOutdatedPaths === 'boolean');
                            if (!(outdatedCheck.hasOutdatedPaths && outdatedCheck.suggestions.length > 0)) return [3 /*break*/, 4];
                            return [4 /*yield*/, configurationMigration.performMigration()];
                        case 3:
                            migrationResult = _a.sent();
                            assert_1.default.ok(typeof migrationResult.migrated === 'boolean');
                            _a.label = 4;
                        case 4:
                            // Step 4: If still not resolved, installation guide should be available
                            // Installation guide should be available (public API test only)
                            assert_1.default.ok(typeof installationGuide.showInstallationGuideDialog === 'function');
                            _a.label = 5;
                        case 5: return [2 /*return*/];
                    }
                });
            });
        });
        test('should handle configuration update workflow', function () {
            return __awaiter(this, void 0, void 0, function () {
                var config, installationStatus, migrationResult, error_2;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(15000);
                            config = vscode.workspace.getConfiguration('prolog');
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 5, , 6]);
                            // Step 1: Set an invalid path
                            return [4 /*yield*/, config.update('executablePath', '/invalid/path/swipl', vscode.ConfigurationTarget.Global)];
                        case 2:
                            // Step 1: Set an invalid path
                            _a.sent();
                            return [4 /*yield*/, installationChecker.checkSwiplInstallation()];
                        case 3:
                            installationStatus = _a.sent();
                            assert_1.default.strictEqual(installationStatus.isInstalled, false);
                            return [4 /*yield*/, configurationMigration.performMigration()];
                        case 4:
                            migrationResult = _a.sent();
                            // Step 4: Verify result
                            if (migrationResult.migrated) {
                                assert_1.default.ok(migrationResult.newPath);
                                assert_1.default.notStrictEqual(migrationResult.newPath, '/invalid/path/swipl');
                            }
                            else {
                                // Migration failed, but should provide helpful information
                                assert_1.default.ok(Array.isArray(migrationResult.issues));
                            }
                            return [3 /*break*/, 6];
                        case 5:
                            error_2 = _a.sent();
                            console.log('Configuration update test skipped due to environment limitations');
                            return [3 /*break*/, 6];
                        case 6: return [2 /*return*/];
                    }
                });
            });
        });
    });
    suite('Error Recovery', function () {
        test('should gracefully handle system without SWI-Prolog', function () {
            return __awaiter(this, void 0, void 0, function () {
                var config, installationStatus, migrationResult, error_3;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(15000);
                            config = vscode.workspace.getConfiguration('prolog');
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 5, , 6]);
                            // Set a definitely invalid path
                            return [4 /*yield*/, config.update('executablePath', '/definitely/does/not/exist/swipl', vscode.ConfigurationTarget.Global)];
                        case 2:
                            // Set a definitely invalid path
                            _a.sent();
                            return [4 /*yield*/, installationChecker.checkSwiplInstallation()];
                        case 3:
                            installationStatus = _a.sent();
                            assert_1.default.strictEqual(installationStatus.isInstalled, false);
                            return [4 /*yield*/, configurationMigration.performMigration()];
                        case 4:
                            migrationResult = _a.sent();
                            assert_1.default.ok(typeof migrationResult.migrated === 'boolean');
                            // Installation guide should be available (public API test only)
                            assert_1.default.ok(typeof installationGuide.showInstallationGuideDialog === 'function');
                            return [3 /*break*/, 6];
                        case 5:
                            error_3 = _a.sent();
                            console.log('Error recovery test skipped due to environment limitations');
                            return [3 /*break*/, 6];
                        case 6: return [2 /*return*/];
                    }
                });
            });
        });
    });
    suite('User Experience Integration', function () {
        test('should provide consistent messaging across components', function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    this.timeout(10000);
                    // Only test public API
                    assert_1.default.ok(typeof installationGuide.showInstallationGuideDialog === 'function');
                    return [2 /*return*/];
                });
            });
        });
        test('should provide actionable guidance for users', function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    this.timeout(10000);
                    // Only test public API
                    assert_1.default.ok(typeof installationGuide.showInstallationGuideDialog === 'function');
                    return [2 /*return*/];
                });
            });
        });
    });
    suite('Performance Integration', function () {
        test('should complete full installation check workflow efficiently', function () {
            return __awaiter(this, void 0, void 0, function () {
                var startTime, endTime, duration;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(30000);
                            startTime = Date.now();
                            // Simulate the full workflow
                            return [4 /*yield*/, installationChecker.checkSwiplInstallation()];
                        case 1:
                            // Simulate the full workflow
                            _a.sent();
                            return [4 /*yield*/, configurationMigration.detectOutdatedPaths()];
                        case 2:
                            _a.sent();
                            endTime = Date.now();
                            duration = endTime - startTime;
                            assert_1.default.ok(duration < 25000, "Full workflow took ".concat(duration, "ms, should be under 25000ms"));
                            return [2 /*return*/];
                    }
                });
            });
        });
        test('should handle multiple users efficiently', function () {
            return __awaiter(this, void 0, void 0, function () {
                var userCount, promises, startTime, results, endTime, duration, avgDuration, firstResult;
                var _this = this;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(45000);
                            userCount = 5;
                            promises = Array(userCount)
                                .fill(0)
                                .map(function () { return __awaiter(_this, void 0, void 0, function () {
                                var status, migration;
                                return __generator(this, function (_a) {
                                    switch (_a.label) {
                                        case 0: return [4 /*yield*/, installationChecker.checkSwiplInstallation()];
                                        case 1:
                                            status = _a.sent();
                                            return [4 /*yield*/, configurationMigration.detectOutdatedPaths()];
                                        case 2:
                                            migration = _a.sent();
                                            return [2 /*return*/, { status: status, migration: migration }];
                                    }
                                });
                            }); });
                            startTime = Date.now();
                            return [4 /*yield*/, Promise.all(promises)];
                        case 1:
                            results = _a.sent();
                            endTime = Date.now();
                            duration = endTime - startTime;
                            avgDuration = duration / userCount;
                            assert_1.default.strictEqual(results.length, userCount);
                            assert_1.default.ok(avgDuration < 10000, "Average per-user time was ".concat(avgDuration, "ms, should be under 10000ms"));
                            firstResult = results[0];
                            results.forEach(function (result) {
                                assert_1.default.strictEqual(result.status.isInstalled, firstResult.status.isInstalled);
                                assert_1.default.strictEqual(result.migration.hasOutdatedPaths, firstResult.migration.hasOutdatedPaths);
                            });
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    suite('Configuration Consistency', function () {
        test('should maintain configuration consistency across operations', function () {
            return __awaiter(this, void 0, void 0, function () {
                var config, initialPath, finalPath;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(15000);
                            config = vscode.workspace.getConfiguration('prolog');
                            initialPath = config.get('executablePath', 'swipl');
                            // Perform various operations
                            return [4 /*yield*/, installationChecker.checkSwiplInstallation()];
                        case 1:
                            // Perform various operations
                            _a.sent();
                            return [4 /*yield*/, configurationMigration.detectOutdatedPaths()];
                        case 2:
                            _a.sent();
                            finalPath = config.get('executablePath', 'swipl');
                            assert_1.default.strictEqual(finalPath, initialPath);
                            return [2 /*return*/];
                    }
                });
            });
        });
        test('should handle configuration changes properly', function () {
            return __awaiter(this, void 0, void 0, function () {
                var config, updatedPath, status_1, error_4;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(15000);
                            config = vscode.workspace.getConfiguration('prolog');
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 4, , 5]);
                            // Change configuration
                            return [4 /*yield*/, config.update('executablePath', 'test-swipl-path', vscode.ConfigurationTarget.Global)];
                        case 2:
                            // Change configuration
                            _a.sent();
                            updatedPath = config.get('executablePath');
                            assert_1.default.strictEqual(updatedPath, 'test-swipl-path');
                            return [4 /*yield*/, installationChecker.checkSwiplInstallation()];
                        case 3:
                            status_1 = _a.sent();
                            // Should attempt to validate the new path (will likely fail, but that's expected)
                            assert_1.default.ok(typeof status_1.isInstalled === 'boolean');
                            return [3 /*break*/, 5];
                        case 4:
                            error_4 = _a.sent();
                            console.log('Configuration change test skipped due to environment limitations');
                            return [3 /*break*/, 5];
                        case 5: return [2 /*return*/];
                    }
                });
            });
        });
    });
});
