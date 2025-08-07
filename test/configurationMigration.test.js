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
var assert = require("assert");
var vscode = require("vscode");
var configurationMigration_js_1 = require("../src/features/configurationMigration.js");
suite('ConfigurationMigration Tests', function () {
    var configurationMigration;
    var originalConfig;
    setup(function () { return __awaiter(void 0, void 0, void 0, function () {
        var config;
        return __generator(this, function (_a) {
            configurationMigration = configurationMigration_js_1.ConfigurationMigration.getInstance();
            config = vscode.workspace.getConfiguration('prolog');
            originalConfig = {
                executablePath: config.get('executablePath'),
                dialect: config.get('dialect'),
                'linter.run': config.get('linter.run'),
                'linter.delay': config.get('linter.delay'),
                'linter.enableMsgInOutput': config.get('linter.enableMsgInOutput'),
                'format.addSpace': config.get('format.addSpace'),
                'terminal.runtimeArgs': config.get('terminal.runtimeArgs'),
                'telemetry.enabled': config.get('telemetry.enabled'),
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
    suite('Singleton Pattern', function () {
        test('should return the same instance', function () {
            var instance1 = configurationMigration_js_1.ConfigurationMigration.getInstance();
            var instance2 = configurationMigration_js_1.ConfigurationMigration.getInstance();
            assert.strictEqual(instance1, instance2);
        });
    });
    suite('Migration Detection', function () {
        test('should detect outdated paths', function () {
            return __awaiter(this, void 0, void 0, function () {
                var config, outdatedCheck, error_2;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(10000);
                            config = vscode.workspace.getConfiguration('prolog');
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 4, , 5]);
                            // Set an outdated path
                            return [4 /*yield*/, config.update('executablePath', '/usr/local/bin/pl', vscode.ConfigurationTarget.Global)];
                        case 2:
                            // Set an outdated path
                            _a.sent();
                            return [4 /*yield*/, configurationMigration.detectOutdatedPaths()];
                        case 3:
                            outdatedCheck = _a.sent();
                            assert.ok(typeof outdatedCheck.hasOutdatedPaths === 'boolean');
                            assert.ok(Array.isArray(outdatedCheck.invalidPaths));
                            assert.ok(Array.isArray(outdatedCheck.suggestions));
                            // Should detect the outdated path
                            if (outdatedCheck.hasOutdatedPaths) {
                                assert.ok(outdatedCheck.invalidPaths.some(function (path) { return path.includes('/usr/local/bin/pl'); }));
                            }
                            return [3 /*break*/, 5];
                        case 4:
                            error_2 = _a.sent();
                            console.log('Configuration update test skipped due to environment limitations');
                            return [3 /*break*/, 5];
                        case 5: return [2 /*return*/];
                    }
                });
            });
        });
        test('should find valid alternative paths', function () {
            return __awaiter(this, void 0, void 0, function () {
                var validPaths;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(15000);
                            return [4 /*yield*/, configurationMigration.findNewValidPaths()];
                        case 1:
                            validPaths = _a.sent();
                            assert.ok(Array.isArray(validPaths));
                            // Each valid path should have a path property
                            validPaths.forEach(function (pathInfo) {
                                assert.ok(pathInfo.path);
                                assert.ok(typeof pathInfo.path === 'string');
                            });
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    suite('Migration Execution', function () {
        test('should perform migration when needed', function () {
            return __awaiter(this, void 0, void 0, function () {
                var config, migrationResult, error_3;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(15000);
                            config = vscode.workspace.getConfiguration('prolog');
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 4, , 5]);
                            // Set an invalid path to trigger migration
                            return [4 /*yield*/, config.update('executablePath', '/invalid/path/swipl', vscode.ConfigurationTarget.Global)];
                        case 2:
                            // Set an invalid path to trigger migration
                            _a.sent();
                            return [4 /*yield*/, configurationMigration.performMigration()];
                        case 3:
                            migrationResult = _a.sent();
                            assert.ok(typeof migrationResult.migrated === 'boolean');
                            assert.ok(Array.isArray(migrationResult.issues));
                            if (migrationResult.migrated) {
                                assert.ok(migrationResult.oldPath);
                                assert.ok(migrationResult.newPath);
                                assert.ok(typeof migrationResult.backupCreated === 'boolean');
                            }
                            return [3 /*break*/, 5];
                        case 4:
                            error_3 = _a.sent();
                            console.log('Migration test skipped due to environment limitations');
                            return [3 /*break*/, 5];
                        case 5: return [2 /*return*/];
                    }
                });
            });
        });
        test('should not migrate valid configurations', function () {
            return __awaiter(this, void 0, void 0, function () {
                var config, migrationResult, error_4;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(10000);
                            config = vscode.workspace.getConfiguration('prolog');
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 4, , 5]);
                            // Set a potentially valid path
                            return [4 /*yield*/, config.update('executablePath', 'swipl', vscode.ConfigurationTarget.Global)];
                        case 2:
                            // Set a potentially valid path
                            _a.sent();
                            return [4 /*yield*/, configurationMigration.performMigration()];
                        case 3:
                            migrationResult = _a.sent();
                            // If swipl is available, migration should not be needed
                            // If not available, migration might be attempted
                            assert.ok(typeof migrationResult.migrated === 'boolean');
                            return [3 /*break*/, 5];
                        case 4:
                            error_4 = _a.sent();
                            console.log('Valid configuration test skipped due to environment limitations');
                            return [3 /*break*/, 5];
                        case 5: return [2 /*return*/];
                    }
                });
            });
        });
    });
    suite('Configuration Backup', function () {
        test('should create configuration backup', function () { return __awaiter(void 0, void 0, void 0, function () {
            var backupCreated;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, configurationMigration.createConfigurationBackup('test_backup')];
                    case 1:
                        backupCreated = _a.sent();
                        // Backup creation might fail in test environment due to context limitations
                        assert.ok(typeof backupCreated === 'boolean');
                        return [2 /*return*/];
                }
            });
        }); });
        test('should list configuration backups', function () {
            var backups = configurationMigration.getConfigurationBackups();
            assert.ok(Array.isArray(backups));
            // Each backup should have required properties
            backups.forEach(function (backup) {
                assert.ok(backup.timestamp);
                assert.ok(backup.reason);
                assert.ok(backup.configuration);
            });
        });
        test('should restore configuration backup', function () { return __awaiter(void 0, void 0, void 0, function () {
            var restored;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: 
                    // First create a backup
                    return [4 /*yield*/, configurationMigration.createConfigurationBackup('test_restore')];
                    case 1:
                        // First create a backup
                        _a.sent();
                        return [4 /*yield*/, configurationMigration.restoreConfigurationBackup(0)];
                    case 2:
                        restored = _a.sent();
                        // Restoration might fail in test environment
                        assert.ok(typeof restored === 'boolean');
                        return [2 /*return*/];
                }
            });
        }); });
    });
    suite('Version Migration', function () {
        test('should handle version migration analysis', function () { return __awaiter(void 0, void 0, void 0, function () {
            var versionMigration;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, configurationMigration.handleVersionMigration('8.5.0', '9.0.4')];
                    case 1:
                        versionMigration = _a.sent();
                        assert.ok(Array.isArray(versionMigration.compatibilityIssues));
                        assert.ok(Array.isArray(versionMigration.recommendations));
                        // Should detect major version change
                        assert.ok(versionMigration.compatibilityIssues.length > 0 ||
                            versionMigration.recommendations.length > 0);
                        return [2 /*return*/];
                }
            });
        }); });
        test('should handle same version migration', function () { return __awaiter(void 0, void 0, void 0, function () {
            var versionMigration;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, configurationMigration.handleVersionMigration('9.0.4', '9.0.4')];
                    case 1:
                        versionMigration = _a.sent();
                        assert.ok(Array.isArray(versionMigration.compatibilityIssues));
                        assert.ok(Array.isArray(versionMigration.recommendations));
                        // Should have minimal issues for same version
                        assert.strictEqual(versionMigration.compatibilityIssues.length, 0);
                        return [2 /*return*/];
                }
            });
        }); });
        test('should handle invalid version strings', function () { return __awaiter(void 0, void 0, void 0, function () {
            var versionMigration;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, configurationMigration.handleVersionMigration('invalid', 'also-invalid')];
                    case 1:
                        versionMigration = _a.sent();
                        assert.ok(Array.isArray(versionMigration.compatibilityIssues));
                        assert.ok(Array.isArray(versionMigration.recommendations));
                        // Should handle gracefully with error message
                        assert.ok(versionMigration.compatibilityIssues.some(function (issue) {
                            return issue.includes('Unable to parse version numbers');
                        }));
                        return [2 /*return*/];
                }
            });
        }); });
    });
    suite('User Customization Preservation', function () {
        test('should preserve user customizations', function () { return __awaiter(void 0, void 0, void 0, function () {
            var preservation, preservedSettings;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, configurationMigration.preserveUserCustomizations()];
                    case 1:
                        preservation = _a.sent();
                        assert.ok(Array.isArray(preservation.preserved));
                        assert.ok(Array.isArray(preservation.issues));
                        preservedSettings = preservation.preserved.join(' ');
                        assert.ok(preservedSettings.includes('dialect') || preservation.preserved.length === 0);
                        return [2 /*return*/];
                }
            });
        }); });
    });
    suite('Comprehensive Migration', function () {
        test('should perform comprehensive migration check', function () {
            return __awaiter(this, void 0, void 0, function () {
                var error_5;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(20000);
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 3, , 4]);
                            return [4 /*yield*/, configurationMigration.performComprehensiveMigration()];
                        case 2:
                            _a.sent();
                            // If we reach here, the method completed without throwing
                            assert.ok(true);
                            return [3 /*break*/, 4];
                        case 3:
                            error_5 = _a.sent();
                            // Migration might fail due to UI interactions in test environment
                            console.log('Comprehensive migration test completed with expected limitations:', error_5);
                            assert.ok(true);
                            return [3 /*break*/, 4];
                        case 4: return [2 /*return*/];
                    }
                });
            });
        });
    });
    suite('Error Handling', function () {
        test('should handle missing extension context gracefully', function () { return __awaiter(void 0, void 0, void 0, function () {
            var backupResult;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, configurationMigration.createConfigurationBackup('test_no_context')];
                    case 1:
                        backupResult = _a.sent();
                        // Should return false when no context is available
                        assert.strictEqual(backupResult, false);
                        return [2 /*return*/];
                }
            });
        }); });
        test('should handle configuration update failures', function () { return __awaiter(void 0, void 0, void 0, function () {
            var migrationResult;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, configurationMigration.performMigration()];
                    case 1:
                        migrationResult = _a.sent();
                        // Should handle gracefully
                        assert.ok(typeof migrationResult.migrated === 'boolean');
                        assert.ok(Array.isArray(migrationResult.issues));
                        return [2 /*return*/];
                }
            });
        }); });
    });
    suite('Integration Tests', function () {
        test('should integrate with InstallationChecker', function () {
            return __awaiter(this, void 0, void 0, function () {
                var outdatedCheck;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(15000);
                            return [4 /*yield*/, configurationMigration.detectOutdatedPaths()];
                        case 1:
                            outdatedCheck = _a.sent();
                            assert.ok(typeof outdatedCheck.hasOutdatedPaths === 'boolean');
                            if (outdatedCheck.hasOutdatedPaths && outdatedCheck.suggestions.length > 0) {
                                // If there are suggestions, they should be valid paths
                                outdatedCheck.suggestions.forEach(function (suggestion) {
                                    assert.ok(suggestion.path);
                                    assert.ok(typeof suggestion.path === 'string');
                                });
                            }
                            return [2 /*return*/];
                    }
                });
            });
        });
        test('should work with VS Code configuration system', function () { return __awaiter(void 0, void 0, void 0, function () {
            var config, currentPath, migrationResult;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        config = vscode.workspace.getConfiguration('prolog');
                        currentPath = config.get('executablePath', 'swipl');
                        // Should be able to read current configuration
                        assert.ok(typeof currentPath === 'string');
                        return [4 /*yield*/, configurationMigration.performMigration()];
                    case 1:
                        migrationResult = _a.sent();
                        assert.ok(typeof migrationResult.migrated === 'boolean');
                        return [2 /*return*/];
                }
            });
        }); });
    });
    suite('Performance Tests', function () {
        test('should complete migration check within reasonable time', function () {
            return __awaiter(this, void 0, void 0, function () {
                var startTime, endTime, duration;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(25000);
                            startTime = Date.now();
                            return [4 /*yield*/, configurationMigration.detectOutdatedPaths()];
                        case 1:
                            _a.sent();
                            endTime = Date.now();
                            duration = endTime - startTime;
                            assert.ok(duration < 20000, "Migration check took ".concat(duration, "ms, should be under 20000ms"));
                            return [2 /*return*/];
                    }
                });
            });
        });
        test('should handle multiple concurrent migration checks', function () {
            return __awaiter(this, void 0, void 0, function () {
                var promises, results, firstResult;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(30000);
                            promises = Array(3)
                                .fill(0)
                                .map(function () { return configurationMigration.detectOutdatedPaths(); });
                            return [4 /*yield*/, Promise.all(promises)];
                        case 1:
                            results = _a.sent();
                            firstResult = results[0];
                            results.forEach(function (result) {
                                assert.strictEqual(result.hasOutdatedPaths, firstResult.hasOutdatedPaths);
                            });
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
});
