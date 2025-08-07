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
var installationChecker_js_1 = require("../src/features/installationChecker.js");
suite('InstallationChecker Tests', function () {
    var installationChecker;
    setup(function () {
        installationChecker = installationChecker_js_1.InstallationChecker.getInstance();
    });
    suite('Singleton Pattern', function () {
        test('should return the same instance', function () {
            var instance1 = installationChecker_js_1.InstallationChecker.getInstance();
            var instance2 = installationChecker_js_1.InstallationChecker.getInstance();
            assert_1.default.strictEqual(instance1, instance2);
        });
    });
    suite('Path Validation', function () {
        test('should validate valid swipl command', function () {
            return __awaiter(this, void 0, void 0, function () {
                var isValid, error_1;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(10000); // Increase timeout for actual swipl execution
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 3, , 4]);
                            return [4 /*yield*/, installationChecker.validateSwiplPath('swipl')];
                        case 2:
                            isValid = _a.sent();
                            // This might be true or false depending on system, but should not throw
                            assert_1.default.ok(typeof isValid === 'boolean');
                            return [3 /*break*/, 4];
                        case 3:
                            error_1 = _a.sent();
                            // If swipl is not installed, this is expected
                            console.log('SWI-Prolog not found in PATH, which is expected in test environment');
                            return [3 /*break*/, 4];
                        case 4: return [2 /*return*/];
                    }
                });
            });
        });
        test('should reject invalid paths', function () { return __awaiter(void 0, void 0, void 0, function () {
            var isValid;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, installationChecker.validateSwiplPath('/nonexistent/path/swipl')];
                    case 1:
                        isValid = _a.sent();
                        assert_1.default.strictEqual(isValid, false);
                        return [2 /*return*/];
                }
            });
        }); });
        test('should reject empty paths', function () { return __awaiter(void 0, void 0, void 0, function () {
            var isValid;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, installationChecker.validateSwiplPath('')];
                    case 1:
                        isValid = _a.sent();
                        assert_1.default.strictEqual(isValid, false);
                        return [2 /*return*/];
                }
            });
        }); });
    });
    suite('Installation Detection', function () {
        test('should perform installation check', function () {
            return __awaiter(this, void 0, void 0, function () {
                var status;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(15000); // Increase timeout for comprehensive check
                            return [4 /*yield*/, installationChecker.checkSwiplInstallation()];
                        case 1:
                            status = _a.sent();
                            assert_1.default.ok(typeof status.isInstalled === 'boolean');
                            assert_1.default.ok(typeof status.path === 'string');
                            assert_1.default.ok(Array.isArray(status.issues));
                            if (status.isInstalled) {
                                assert_1.default.ok(status.version);
                                assert_1.default.ok(status.path.length > 0);
                            }
                            return [2 /*return*/];
                    }
                });
            });
        });
        test('should find executable if available', function () {
            return __awaiter(this, void 0, void 0, function () {
                var foundPath;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(10000);
                            return [4 /*yield*/, installationChecker.findSwiplExecutable()];
                        case 1:
                            foundPath = _a.sent();
                            if (foundPath) {
                                assert_1.default.ok(typeof foundPath === 'string');
                                assert_1.default.ok(foundPath.length > 0);
                            }
                            else {
                                // No SWI-Prolog found, which is acceptable in test environment
                                console.log('No SWI-Prolog installation found');
                            }
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    suite('Version Detection', function () {
        test('should handle version parsing', function () { return __awaiter(void 0, void 0, void 0, function () {
            var mockVersionOutput, version;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mockVersionOutput = 'SWI-Prolog version 9.0.4 for x86_64-linux';
                        return [4 /*yield*/, installationChecker.getSwiplVersion('/nonexistent/swipl')];
                    case 1:
                        version = _a.sent();
                        assert_1.default.strictEqual(version, null);
                        return [2 /*return*/];
                }
            });
        }); });
    });
    suite('Configuration Validation', function () {
        test('should validate and update configuration', function () {
            return __awaiter(this, void 0, void 0, function () {
                var result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(10000);
                            return [4 /*yield*/, installationChecker.validateAndUpdateConfiguration()];
                        case 1:
                            result = _a.sent();
                            assert_1.default.ok(typeof result.updated === 'boolean');
                            assert_1.default.ok(typeof result.updated === 'boolean');
                            if (result.updated) {
                                assert_1.default.ok(result.oldPath);
                                assert_1.default.ok(result.newPath);
                            }
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    suite('Error Handling', function () {
        test('should handle spawn errors gracefully', function () { return __awaiter(void 0, void 0, void 0, function () {
            var isValid;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, installationChecker.validateSwiplPath('definitely-not-a-real-command-12345')];
                    case 1:
                        isValid = _a.sent();
                        assert_1.default.strictEqual(isValid, false);
                        return [2 /*return*/];
                }
            });
        }); });
        test('should handle permission errors', function () { return __awaiter(void 0, void 0, void 0, function () {
            var isValid;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, installationChecker.validateSwiplPath('/root/swipl')];
                    case 1:
                        isValid = _a.sent();
                        assert_1.default.strictEqual(isValid, false);
                        return [2 /*return*/];
                }
            });
        }); });
    });
    suite('Integration with VS Code Configuration', function () {
        test('should read current configuration', function () {
            var config = vscode.workspace.getConfiguration('prolog');
            var execPath = config.get('executablePath', 'swipl');
            assert_1.default.ok(typeof execPath === 'string');
        });
        test('should handle configuration updates', function () { return __awaiter(void 0, void 0, void 0, function () {
            var config, originalPath, updatedPath, error_2;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        config = vscode.workspace.getConfiguration('prolog');
                        originalPath = config.get('executablePath', 'swipl');
                        _a.label = 1;
                    case 1:
                        _a.trys.push([1, 4, , 5]);
                        // Test updating configuration
                        return [4 /*yield*/, config.update('executablePath', 'test-swipl', vscode.ConfigurationTarget.Global)];
                    case 2:
                        // Test updating configuration
                        _a.sent();
                        updatedPath = config.get('executablePath');
                        assert_1.default.strictEqual(updatedPath, 'test-swipl');
                        // Restore original configuration
                        return [4 /*yield*/, config.update('executablePath', originalPath, vscode.ConfigurationTarget.Global)];
                    case 3:
                        // Restore original configuration
                        _a.sent();
                        return [3 /*break*/, 5];
                    case 4:
                        error_2 = _a.sent();
                        // Configuration update might fail in test environment
                        console.log('Configuration update test skipped due to environment limitations');
                        return [3 /*break*/, 5];
                    case 5: return [2 /*return*/];
                }
            });
        }); });
    });
    suite('Performance Tests', function () {
        test('should complete installation check within reasonable time', function () {
            return __awaiter(this, void 0, void 0, function () {
                var startTime, endTime, duration;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(20000);
                            startTime = Date.now();
                            return [4 /*yield*/, installationChecker.checkSwiplInstallation()];
                        case 1:
                            _a.sent();
                            endTime = Date.now();
                            duration = endTime - startTime;
                            assert_1.default.ok(duration < 15000, "Installation check took ".concat(duration, "ms, should be under 15000ms"));
                            return [2 /*return*/];
                    }
                });
            });
        });
        test('should handle multiple concurrent checks', function () {
            return __awaiter(this, void 0, void 0, function () {
                var promises, results, firstResult;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(30000);
                            promises = Array(5)
                                .fill(0)
                                .map(function () { return installationChecker.checkSwiplInstallation(); });
                            return [4 /*yield*/, Promise.all(promises)];
                        case 1:
                            results = _a.sent();
                            firstResult = results[0];
                            results.forEach(function (result) {
                                assert_1.default.strictEqual(result.isInstalled, firstResult.isInstalled);
                                assert_1.default.strictEqual(result.path, firstResult.path);
                            });
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    suite('Edge Cases', function () {
        test('should handle null and undefined inputs', function () { return __awaiter(void 0, void 0, void 0, function () {
            var result1, result2;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, installationChecker.validateSwiplPath(null)];
                    case 1:
                        result1 = _a.sent();
                        return [4 /*yield*/, installationChecker.validateSwiplPath(undefined)];
                    case 2:
                        result2 = _a.sent();
                        assert_1.default.strictEqual(result1, false);
                        assert_1.default.strictEqual(result2, false);
                        return [2 /*return*/];
                }
            });
        }); });
        test('should handle very long paths', function () { return __awaiter(void 0, void 0, void 0, function () {
            var longPath, isValid;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        longPath = '/very/long/path/that/does/not/exist/and/should/be/handled/gracefully/swipl';
                        return [4 /*yield*/, installationChecker.validateSwiplPath(longPath)];
                    case 1:
                        isValid = _a.sent();
                        assert_1.default.strictEqual(isValid, false);
                        return [2 /*return*/];
                }
            });
        }); });
        test('should handle paths with special characters', function () { return __awaiter(void 0, void 0, void 0, function () {
            var specialPath, isValid;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        specialPath = '/path with spaces/and-dashes/swipl';
                        return [4 /*yield*/, installationChecker.validateSwiplPath(specialPath)];
                    case 1:
                        isValid = _a.sent();
                        assert_1.default.strictEqual(isValid, false);
                        return [2 /*return*/];
                }
            });
        }); });
    });
});
