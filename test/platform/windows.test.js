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
var os = require("os");
var packageManagerIntegration_js_1 = require("../../src/features/packageManagerIntegration.js");
var executableFinder_js_1 = require("../../src/utils/executableFinder.js");
var platformUtils_js_1 = require("../../src/utils/platformUtils.js");
/**
 * Windows-specific platform tests
 */
suite('Windows Platform Tests', function () {
    // Skip tests if not running on Windows
    var isWindows = os.platform() === 'win32';
    suiteSetup(function () {
        if (!isWindows) {
            this.skip();
        }
    });
    suite('Platform Detection', function () {
        test('should detect Windows platform correctly', function () {
            var platform = platformUtils_js_1.PlatformUtils.getPlatform();
            assert.strictEqual(platform, 'windows');
        });
        test('should detect architecture correctly', function () {
            var arch = platformUtils_js_1.PlatformUtils.getArchitecture();
            assert.ok(['x64', 'arm64', 'x86'].includes(arch));
        });
        test('should provide Windows-specific defaults', function () {
            var defaults = platformUtils_js_1.PlatformUtils.getPlatformDefaults();
            assert.strictEqual(defaults.pathSeparator, '\\');
            assert.strictEqual(defaults.executableExtension, '.exe');
            assert.ok(defaults.executablePaths.some(function (path) { return path.includes('.exe'); }));
        });
    });
    suite('Path Handling', function () {
        test('should normalize Windows paths correctly', function () {
            var testPaths = [
                'C:\\Program Files\\swipl\\bin\\swipl.exe',
                'C:/Program Files/swipl/bin/swipl.exe',
                '..\\relative\\path\\file.exe',
                '../relative/path/file.exe',
            ];
            testPaths.forEach(function (testPath) {
                var normalized = platformUtils_js_1.PlatformUtils.normalizePath(testPath);
                assert.ok(normalized.length > 0);
                // Should handle both forward and backward slashes
                assert.ok(!normalized.includes('//') && !normalized.includes('\\\\'));
            });
        });
        test('should handle UNC paths correctly', function () {
            var uncPath = '\\\\server\\share\\file.exe';
            var normalized = platformUtils_js_1.PlatformUtils.normalizePath(uncPath);
            assert.ok(normalized.startsWith('\\\\'));
        });
        test('should expand Windows environment variables', function () {
            var pathWithEnvVar = '%PROGRAMFILES%\\swipl\\bin\\swipl.exe';
            var expanded = platformUtils_js_1.PlatformUtils.expandEnvironmentVariables(pathWithEnvVar);
            if (process.env.PROGRAMFILES) {
                assert.ok(expanded.includes(process.env.PROGRAMFILES));
                assert.ok(!expanded.includes('%PROGRAMFILES%'));
            }
        });
        test('should handle Windows drive letters', function () {
            var drivePaths = ['C:\\test', 'D:\\test', 'E:\\test'];
            drivePaths.forEach(function (drivePath) {
                var normalized = platformUtils_js_1.PlatformUtils.normalizePath(drivePath);
                assert.ok(normalized.match(/^[A-Z]:\\/));
            });
        });
        test('should join paths correctly', function () {
            var joined = platformUtils_js_1.PlatformUtils.joinPath('C:\\Program Files', 'swipl', 'bin', 'swipl.exe');
            assert.ok(joined.includes('\\'));
            assert.ok(joined.endsWith('swipl.exe'));
        });
    });
    suite('Executable Detection', function () {
        test('should detect Windows executable paths', function () {
            var executablePaths = platformUtils_js_1.PlatformUtils.getExecutablePaths();
            assert.ok(executablePaths.length > 0);
            assert.ok(executablePaths.every(function (path) { return path.endsWith('.exe'); }));
            assert.ok(executablePaths.some(function (path) { return path.includes('Program Files'); }));
        });
        test('should find SWI-Prolog executable on Windows', function () {
            return __awaiter(this, void 0, void 0, function () {
                var finder, result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(10000); // Increase timeout for executable detection
                            finder = new executableFinder_js_1.ExecutableFinder();
                            return [4 /*yield*/, finder.findSwiplExecutable()];
                        case 1:
                            result = _a.sent();
                            // Test should pass whether SWI-Prolog is installed or not
                            if (result.found) {
                                assert.ok(result.path);
                                assert.ok(result.path.endsWith('.exe'));
                                assert.ok(result.detectionMethod);
                                if (result.permissions) {
                                    // On Windows, executable permission is usually true if file exists
                                    assert.strictEqual(typeof result.permissions.executable, 'boolean');
                                }
                            }
                            else {
                                assert.ok(result.issues);
                                assert.ok(result.issues.length > 0);
                            }
                            return [2 /*return*/];
                    }
                });
            });
        });
        test('should validate Windows executable paths', function () {
            return __awaiter(this, void 0, void 0, function () {
                var finder, invalidResult, textFileResult;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(5000);
                            finder = new executableFinder_js_1.ExecutableFinder();
                            return [4 /*yield*/, finder.validateExecutable('C:\\nonexistent\\swipl.exe')];
                        case 1:
                            invalidResult = _a.sent();
                            assert.strictEqual(invalidResult.found, false);
                            assert.ok(invalidResult.issues);
                            return [4 /*yield*/, finder.validateExecutable('C:\\Windows\\System32\\drivers\\etc\\hosts')];
                        case 2:
                            textFileResult = _a.sent();
                            return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists('C:\\Windows\\System32\\drivers\\etc\\hosts')];
                        case 3:
                            if (_a.sent()) {
                                assert.strictEqual(textFileResult.found, false);
                                assert.ok(textFileResult.issues);
                            }
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    suite('Package Manager Integration', function () {
        test('should detect Windows package managers', function () {
            return __awaiter(this, void 0, void 0, function () {
                var packageManager, availableManagers;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(15000); // Package manager detection can be slow
                            packageManager = packageManagerIntegration_js_1.PackageManagerIntegration.getInstance();
                            return [4 /*yield*/, packageManager.detectAvailableManagers()];
                        case 1:
                            availableManagers = _a.sent();
                            // Should detect at least one Windows package manager or none
                            availableManagers.forEach(function (manager) {
                                assert.ok(['winget', 'chocolatey', 'scoop'].includes(manager.name));
                                assert.strictEqual(manager.isAvailable, true);
                                assert.ok(manager.installCommand.length > 0);
                            });
                            return [2 /*return*/];
                    }
                });
            });
        });
        test('should provide Windows installation suggestions', function () {
            return __awaiter(this, void 0, void 0, function () {
                var packageManager, suggestions;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(5000);
                            packageManager = packageManagerIntegration_js_1.PackageManagerIntegration.getInstance();
                            return [4 /*yield*/, packageManager.getInstallationSuggestions()];
                        case 1:
                            suggestions = _a.sent();
                            assert.ok(suggestions.length > 0);
                            assert.ok(suggestions.some(function (s) { return s.includes('Windows'); }));
                            assert.ok(suggestions.some(function (s) { return s.includes('choco') || s.includes('winget') || s.includes('scoop'); }));
                            return [2 /*return*/];
                    }
                });
            });
        });
        test('should get Windows-specific recommendations', function () {
            var packageManager = packageManagerIntegration_js_1.PackageManagerIntegration.getInstance();
            var recommendations = packageManager.getRecommendedManagers();
            assert.deepStrictEqual(recommendations, ['winget', 'chocolatey', 'scoop']);
        });
    });
    suite('Environment Variables', function () {
        test('should handle Windows environment variables', function () {
            var envVars = platformUtils_js_1.PlatformUtils.getEnvironmentVariables();
            assert.ok(envVars.crossPlatform.includes('PATH'));
            assert.ok(envVars.platformSpecific.includes('PROGRAMFILES'));
            assert.ok(envVars.platformSpecific.includes('APPDATA'));
            assert.ok(envVars.platformSpecific.includes('USERPROFILE'));
        });
        test('should expand Windows-style environment variables', function () {
            // Test Windows-style %VAR% expansion
            var testCases = [
                {
                    input: '%USERPROFILE%\\Documents',
                    expected: process.env.USERPROFILE
                        ? process.env.USERPROFILE + '\\Documents'
                        : '%USERPROFILE%\\Documents',
                },
                {
                    input: '%TEMP%\\test.txt',
                    expected: process.env.TEMP ? process.env.TEMP + '\\test.txt' : '%TEMP%\\test.txt',
                },
                {
                    input: 'C:\\test\\%USERNAME%',
                    expected: process.env.USERNAME
                        ? 'C:\\test\\' + process.env.USERNAME
                        : 'C:\\test\\%USERNAME%',
                },
            ];
            testCases.forEach(function (testCase) {
                var result = platformUtils_js_1.PlatformUtils.expandEnvironmentVariables(testCase.input);
                if (testCase.expected !== testCase.input) {
                    // Environment variable should be expanded
                    assert.strictEqual(result, testCase.expected);
                }
            });
        });
    });
    suite('File System Operations', function () {
        test('should check file existence on Windows', function () { return __awaiter(void 0, void 0, void 0, function () {
            var systemFiles, _i, systemFiles_1, file, exists;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        systemFiles = [
                            'C:\\Windows\\System32\\kernel32.dll',
                            'C:\\Windows\\System32\\cmd.exe',
                            'C:\\Windows\\System32\\notepad.exe',
                        ];
                        _i = 0, systemFiles_1 = systemFiles;
                        _a.label = 1;
                    case 1:
                        if (!(_i < systemFiles_1.length)) return [3 /*break*/, 4];
                        file = systemFiles_1[_i];
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists(file)];
                    case 2:
                        exists = _a.sent();
                        // These files should exist on any Windows system
                        assert.strictEqual(exists, true, "".concat(file, " should exist on Windows"));
                        _a.label = 3;
                    case 3:
                        _i++;
                        return [3 /*break*/, 1];
                    case 4: return [2 /*return*/];
                }
            });
        }); });
        test('should check executable permissions on Windows', function () { return __awaiter(void 0, void 0, void 0, function () {
            var executables, _i, executables_1, exe, isExecutable;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        executables = ['C:\\Windows\\System32\\cmd.exe', 'C:\\Windows\\System32\\notepad.exe'];
                        _i = 0, executables_1 = executables;
                        _a.label = 1;
                    case 1:
                        if (!(_i < executables_1.length)) return [3 /*break*/, 5];
                        exe = executables_1[_i];
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists(exe)];
                    case 2:
                        if (!_a.sent()) return [3 /*break*/, 4];
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.isExecutable(exe)];
                    case 3:
                        isExecutable = _a.sent();
                        assert.strictEqual(isExecutable, true, "".concat(exe, " should be executable"));
                        _a.label = 4;
                    case 4:
                        _i++;
                        return [3 /*break*/, 1];
                    case 5: return [2 /*return*/];
                }
            });
        }); });
        test('should handle Windows file attributes', function () { return __awaiter(void 0, void 0, void 0, function () {
            var systemDirs, _i, systemDirs_1, dir, exists;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        systemDirs = ['C:\\Windows', 'C:\\Program Files', 'C:\\Users'];
                        _i = 0, systemDirs_1 = systemDirs;
                        _a.label = 1;
                    case 1:
                        if (!(_i < systemDirs_1.length)) return [3 /*break*/, 4];
                        dir = systemDirs_1[_i];
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists(dir)];
                    case 2:
                        exists = _a.sent();
                        assert.strictEqual(exists, true, "".concat(dir, " should exist"));
                        _a.label = 3;
                    case 3:
                        _i++;
                        return [3 /*break*/, 1];
                    case 4: return [2 /*return*/];
                }
            });
        }); });
    });
    suite('Configuration', function () {
        test('should provide Windows-specific configuration location', function () {
            var configLocation = platformUtils_js_1.PlatformUtils.getConfigurationLocation();
            assert.ok(configLocation.includes('AppData') || configLocation.includes('%APPDATA%'));
        });
        test('should provide Windows temp directory', function () {
            var tempDir = platformUtils_js_1.PlatformUtils.getTempDirectory();
            assert.ok(tempDir.length > 0);
            // Should be a valid Windows path
            assert.ok(tempDir.match(/^[A-Z]:\\/i) || tempDir.includes('temp') || tempDir.includes('tmp'));
        });
        test('should provide Windows home directory', function () {
            var homeDir = platformUtils_js_1.PlatformUtils.getHomeDirectory();
            assert.ok(homeDir.length > 0);
            // Should be a valid Windows path
            assert.ok(homeDir.match(/^[A-Z]:\\/i));
        });
    });
    suite('Platform Info', function () {
        test('should provide comprehensive Windows platform info', function () {
            var info = platformUtils_js_1.PlatformUtils.getPlatformInfo();
            assert.strictEqual(info.platform, 'windows');
            assert.ok(['x64', 'arm64', 'x86'].includes(info.architecture));
            assert.ok(info.osVersion.length > 0);
            assert.ok(info.nodeVersion.startsWith('v'));
            assert.strictEqual(info.pathSeparator, '\\');
            assert.strictEqual(info.executableExtension, '.exe');
            assert.ok(info.homeDirectory.length > 0);
            assert.ok(info.tempDirectory.length > 0);
        });
    });
    suite('Error Handling', function () {
        test('should handle Windows-specific path errors gracefully', function () { return __awaiter(void 0, void 0, void 0, function () {
            var invalidPaths, _i, invalidPaths_1, invalidPath, exists, isExecutable;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        invalidPaths = [
                            'C:\\invalid\\path\\that\\does\\not\\exist.exe',
                            '\\\\invalid-server\\share\\file.exe',
                            'Z:\\nonexistent\\drive\\file.exe',
                        ];
                        _i = 0, invalidPaths_1 = invalidPaths;
                        _a.label = 1;
                    case 1:
                        if (!(_i < invalidPaths_1.length)) return [3 /*break*/, 5];
                        invalidPath = invalidPaths_1[_i];
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists(invalidPath)];
                    case 2:
                        exists = _a.sent();
                        assert.strictEqual(exists, false);
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.isExecutable(invalidPath)];
                    case 3:
                        isExecutable = _a.sent();
                        assert.strictEqual(isExecutable, false);
                        _a.label = 4;
                    case 4:
                        _i++;
                        return [3 /*break*/, 1];
                    case 5: return [2 /*return*/];
                }
            });
        }); });
        test('should handle Windows permission errors', function () { return __awaiter(void 0, void 0, void 0, function () {
            var restrictedFiles, _i, restrictedFiles_1, file, exists, isExecutable;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        restrictedFiles = [
                            'C:\\Windows\\System32\\config\\SAM',
                            'C:\\Windows\\System32\\config\\SECURITY',
                        ];
                        _i = 0, restrictedFiles_1 = restrictedFiles;
                        _a.label = 1;
                    case 1:
                        if (!(_i < restrictedFiles_1.length)) return [3 /*break*/, 5];
                        file = restrictedFiles_1[_i];
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists(file)];
                    case 2:
                        exists = _a.sent();
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.isExecutable(file)];
                    case 3:
                        isExecutable = _a.sent();
                        // Results may vary based on permissions, but should not throw
                        assert.strictEqual(typeof exists, 'boolean');
                        assert.strictEqual(typeof isExecutable, 'boolean');
                        _a.label = 4;
                    case 4:
                        _i++;
                        return [3 /*break*/, 1];
                    case 5: return [2 /*return*/];
                }
            });
        }); });
    });
});
