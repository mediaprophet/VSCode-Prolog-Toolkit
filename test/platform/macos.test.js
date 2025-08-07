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
 * macOS-specific platform tests
 */
suite('macOS Platform Tests', function () {
    // Skip tests if not running on macOS
    var isMacOS = os.platform() === 'darwin';
    suiteSetup(function () {
        if (!isMacOS) {
            this.skip();
        }
    });
    suite('Platform Detection', function () {
        test('should detect macOS platform correctly', function () {
            var platform = platformUtils_js_1.PlatformUtils.getPlatform();
            assert.strictEqual(platform, 'macos');
        });
        test('should detect architecture correctly', function () {
            var arch = platformUtils_js_1.PlatformUtils.getArchitecture();
            assert.ok(['x64', 'arm64'].includes(arch));
        });
        test('should provide macOS-specific defaults', function () {
            var defaults = platformUtils_js_1.PlatformUtils.getPlatformDefaults();
            assert.strictEqual(defaults.pathSeparator, '/');
            assert.strictEqual(defaults.executableExtension, '');
            assert.ok(defaults.executablePaths.some(function (path) { return path.includes('/usr/local/bin'); }));
            assert.ok(defaults.executablePaths.some(function (path) { return path.includes('/opt/homebrew/bin'); }));
        });
        test('should handle Apple Silicon vs Intel differences', function () {
            var arch = platformUtils_js_1.PlatformUtils.getArchitecture();
            var executablePaths = platformUtils_js_1.PlatformUtils.getExecutablePaths();
            if (arch === 'arm64') {
                // Apple Silicon should include Homebrew paths
                assert.ok(executablePaths.some(function (path) { return path.includes('/opt/homebrew'); }));
            }
            else {
                // Intel Macs should include traditional paths
                assert.ok(executablePaths.some(function (path) { return path.includes('/usr/local'); }));
            }
        });
    });
    suite('Path Handling', function () {
        test('should normalize Unix paths correctly', function () {
            var testPaths = [
                '/usr/local/bin/swipl',
                '/opt/homebrew/bin/swipl',
                '../relative/path/file',
                '~/Documents/test.pl',
                '/Applications/SWI-Prolog.app/Contents/MacOS/swipl',
            ];
            testPaths.forEach(function (testPath) {
                var normalized = platformUtils_js_1.PlatformUtils.normalizePath(testPath);
                assert.ok(normalized.length > 0);
                // Should not contain double slashes
                assert.ok(!normalized.includes('//'));
            });
        });
        test('should expand home directory (~) correctly', function () {
            var homeDir = os.homedir();
            var pathWithTilde = '~/Documents/test.pl';
            var normalized = platformUtils_js_1.PlatformUtils.normalizePath(pathWithTilde);
            assert.ok(normalized.startsWith(homeDir));
            assert.ok(!normalized.includes('~'));
        });
        test('should expand Unix environment variables', function () {
            var pathWithEnvVar = '$HOME/Documents/test.pl';
            var expanded = platformUtils_js_1.PlatformUtils.expandEnvironmentVariables(pathWithEnvVar);
            if (process.env.HOME) {
                assert.ok(expanded.includes(process.env.HOME));
                assert.ok(!expanded.includes('$HOME'));
            }
        });
        test('should handle macOS application bundle paths', function () {
            var appPath = '/Applications/SWI-Prolog.app/Contents/MacOS/swipl';
            var normalized = platformUtils_js_1.PlatformUtils.normalizePath(appPath);
            assert.ok(normalized.includes('/Applications/'));
            assert.ok(normalized.includes('.app/Contents/MacOS/'));
        });
        test('should join paths correctly', function () {
            var joined = platformUtils_js_1.PlatformUtils.joinPath('/usr', 'local', 'bin', 'swipl');
            assert.strictEqual(joined, '/usr/local/bin/swipl');
        });
    });
    suite('Executable Detection', function () {
        test('should detect macOS executable paths', function () {
            var executablePaths = platformUtils_js_1.PlatformUtils.getExecutablePaths();
            assert.ok(executablePaths.length > 0);
            assert.ok(executablePaths.every(function (path) { return !path.endsWith('.exe'); }));
            assert.ok(executablePaths.some(function (path) { return path.includes('/usr/local/bin') || path.includes('/opt/homebrew/bin'); }));
        });
        test('should find SWI-Prolog executable on macOS', function () {
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
                                assert.ok(!result.path.endsWith('.exe'));
                                assert.ok(result.detectionMethod);
                                if (result.permissions) {
                                    assert.strictEqual(typeof result.permissions.executable, 'boolean');
                                    assert.strictEqual(typeof result.permissions.readable, 'boolean');
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
        test('should validate macOS executable paths', function () {
            return __awaiter(this, void 0, void 0, function () {
                var finder, invalidResult, textFileResult;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(5000);
                            finder = new executableFinder_js_1.ExecutableFinder();
                            return [4 /*yield*/, finder.validateExecutable('/nonexistent/swipl')];
                        case 1:
                            invalidResult = _a.sent();
                            assert.strictEqual(invalidResult.found, false);
                            assert.ok(invalidResult.issues);
                            return [4 /*yield*/, finder.validateExecutable('/etc/hosts')];
                        case 2:
                            textFileResult = _a.sent();
                            return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists('/etc/hosts')];
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
        test('should handle macOS-specific executable locations', function () {
            return __awaiter(this, void 0, void 0, function () {
                var commonLocations, finder, _i, commonLocations_1, location_1, result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(5000);
                            commonLocations = [
                                '/usr/local/bin/swipl',
                                '/opt/homebrew/bin/swipl',
                                '/Applications/SWI-Prolog.app/Contents/MacOS/swipl',
                                '/opt/local/bin/swipl', // MacPorts
                            ];
                            finder = new executableFinder_js_1.ExecutableFinder();
                            _i = 0, commonLocations_1 = commonLocations;
                            _a.label = 1;
                        case 1:
                            if (!(_i < commonLocations_1.length)) return [3 /*break*/, 4];
                            location_1 = commonLocations_1[_i];
                            return [4 /*yield*/, finder.validateExecutable(location_1)];
                        case 2:
                            result = _a.sent();
                            // Should not throw errors, regardless of whether file exists
                            assert.strictEqual(typeof result.found, 'boolean');
                            _a.label = 3;
                        case 3:
                            _i++;
                            return [3 /*break*/, 1];
                        case 4: return [2 /*return*/];
                    }
                });
            });
        });
    });
    suite('Package Manager Integration', function () {
        test('should detect macOS package managers', function () {
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
                            // Should detect available macOS package managers
                            availableManagers.forEach(function (manager) {
                                assert.ok(['homebrew', 'macports'].includes(manager.name));
                                assert.strictEqual(manager.isAvailable, true);
                                assert.ok(manager.installCommand.length > 0);
                            });
                            return [2 /*return*/];
                    }
                });
            });
        });
        test('should provide macOS installation suggestions', function () {
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
                            assert.ok(suggestions.some(function (s) { return s.includes('macOS'); }));
                            assert.ok(suggestions.some(function (s) { return s.includes('brew') || s.includes('port'); }));
                            return [2 /*return*/];
                    }
                });
            });
        });
        test('should get macOS-specific recommendations', function () {
            var packageManager = packageManagerIntegration_js_1.PackageManagerIntegration.getInstance();
            var recommendations = packageManager.getRecommendedManagers();
            assert.deepStrictEqual(recommendations, ['homebrew', 'macports']);
        });
        test('should handle Apple Silicon vs Intel package managers', function () {
            return __awaiter(this, void 0, void 0, function () {
                var packageManager, availableManagers, homebrewManager, arch;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(10000);
                            packageManager = packageManagerIntegration_js_1.PackageManagerIntegration.getInstance();
                            return [4 /*yield*/, packageManager.detectAvailableManagers()];
                        case 1:
                            availableManagers = _a.sent();
                            homebrewManager = availableManagers.find(function (m) { return m.name === 'homebrew'; });
                            if (homebrewManager) {
                                arch = platformUtils_js_1.PlatformUtils.getArchitecture();
                                if (arch === 'arm64') {
                                    // Apple Silicon should use /opt/homebrew
                                    assert.ok(homebrewManager.installCommand.includes('brew'));
                                }
                                else {
                                    // Intel should use /usr/local
                                    assert.ok(homebrewManager.installCommand.includes('brew'));
                                }
                            }
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    suite('Environment Variables', function () {
        test('should handle Unix environment variables', function () {
            var envVars = platformUtils_js_1.PlatformUtils.getEnvironmentVariables();
            assert.ok(envVars.crossPlatform.includes('PATH'));
            assert.ok(envVars.platformSpecific.includes('HOME'));
            assert.ok(envVars.platformSpecific.includes('XDG_CONFIG_HOME'));
            assert.ok(envVars.platformSpecific.includes('TMPDIR'));
        });
        test('should expand Unix-style environment variables', function () {
            // Test Unix-style $VAR and ${VAR} expansion
            var testCases = [
                {
                    input: '$HOME/Documents',
                    expected: process.env.HOME ? process.env.HOME + '/Documents' : '$HOME/Documents',
                },
                {
                    input: '${HOME}/test.txt',
                    expected: process.env.HOME ? process.env.HOME + '/test.txt' : '${HOME}/test.txt',
                },
                {
                    input: '/tmp/$USER',
                    expected: process.env.USER ? '/tmp/' + process.env.USER : '/tmp/$USER',
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
        test('should check file existence on macOS', function () { return __awaiter(void 0, void 0, void 0, function () {
            var systemFiles, _i, systemFiles_1, file, exists;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        systemFiles = [
                            '/bin/bash',
                            '/bin/zsh',
                            '/usr/bin/which',
                            '/System/Library/CoreServices/Finder.app',
                        ];
                        _i = 0, systemFiles_1 = systemFiles;
                        _a.label = 1;
                    case 1:
                        if (!(_i < systemFiles_1.length)) return [3 /*break*/, 4];
                        file = systemFiles_1[_i];
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists(file)];
                    case 2:
                        exists = _a.sent();
                        // These files should exist on any macOS system
                        assert.strictEqual(exists, true, "".concat(file, " should exist on macOS"));
                        _a.label = 3;
                    case 3:
                        _i++;
                        return [3 /*break*/, 1];
                    case 4: return [2 /*return*/];
                }
            });
        }); });
        test('should check executable permissions on macOS', function () { return __awaiter(void 0, void 0, void 0, function () {
            var executables, _i, executables_1, exe, isExecutable;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        executables = ['/bin/bash', '/bin/zsh', '/usr/bin/which'];
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
        test('should handle macOS file permissions correctly', function () { return __awaiter(void 0, void 0, void 0, function () {
            var nonExecutables, _i, nonExecutables_1, file, isExecutable;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        nonExecutables = ['/etc/hosts', '/etc/passwd'];
                        _i = 0, nonExecutables_1 = nonExecutables;
                        _a.label = 1;
                    case 1:
                        if (!(_i < nonExecutables_1.length)) return [3 /*break*/, 5];
                        file = nonExecutables_1[_i];
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists(file)];
                    case 2:
                        if (!_a.sent()) return [3 /*break*/, 4];
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.isExecutable(file)];
                    case 3:
                        isExecutable = _a.sent();
                        assert.strictEqual(isExecutable, false, "".concat(file, " should not be executable"));
                        _a.label = 4;
                    case 4:
                        _i++;
                        return [3 /*break*/, 1];
                    case 5: return [2 /*return*/];
                }
            });
        }); });
        test('should handle macOS application bundles', function () { return __awaiter(void 0, void 0, void 0, function () {
            var apps, _i, apps_1, app, exists, isExecutable;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        apps = ['/System/Library/CoreServices/Finder.app', '/Applications/Safari.app'];
                        _i = 0, apps_1 = apps;
                        _a.label = 1;
                    case 1:
                        if (!(_i < apps_1.length)) return [3 /*break*/, 5];
                        app = apps_1[_i];
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists(app)];
                    case 2:
                        exists = _a.sent();
                        if (!exists) return [3 /*break*/, 4];
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.isExecutable(app)];
                    case 3:
                        isExecutable = _a.sent();
                        // Result may vary, but should not throw
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
    suite('Configuration', function () {
        test('should provide macOS-specific configuration location', function () {
            var configLocation = platformUtils_js_1.PlatformUtils.getConfigurationLocation();
            assert.ok(configLocation.includes('Library/Application Support') ||
                configLocation.includes('~/Library'));
        });
        test('should provide macOS temp directory', function () {
            var tempDir = platformUtils_js_1.PlatformUtils.getTempDirectory();
            assert.ok(tempDir.length > 0);
            // Should be a valid Unix path
            assert.ok(tempDir.startsWith('/') || tempDir.includes('tmp'));
        });
        test('should provide macOS home directory', function () {
            var homeDir = platformUtils_js_1.PlatformUtils.getHomeDirectory();
            assert.ok(homeDir.length > 0);
            // Should be a valid Unix path
            assert.ok(homeDir.startsWith('/'));
            assert.ok(homeDir.includes('/Users/'));
        });
    });
    suite('Platform Info', function () {
        test('should provide comprehensive macOS platform info', function () {
            var info = platformUtils_js_1.PlatformUtils.getPlatformInfo();
            assert.strictEqual(info.platform, 'macos');
            assert.ok(['x64', 'arm64'].includes(info.architecture));
            assert.ok(info.osVersion.length > 0);
            assert.ok(info.nodeVersion.startsWith('v'));
            assert.strictEqual(info.pathSeparator, '/');
            assert.strictEqual(info.executableExtension, '');
            assert.ok(info.homeDirectory.length > 0);
            assert.ok(info.tempDirectory.length > 0);
        });
        test('should detect macOS version correctly', function () {
            var info = platformUtils_js_1.PlatformUtils.getPlatformInfo();
            // macOS version should be in format like "21.6.0" (Darwin version)
            assert.ok(info.osVersion.match(/^\d+\.\d+\.\d+$/));
        });
    });
    suite('macOS-Specific Features', function () {
        test('should handle Gatekeeper and security restrictions', function () { return __awaiter(void 0, void 0, void 0, function () {
            var restrictedPaths, _i, restrictedPaths_1, restrictedPath, exists;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        restrictedPaths = ['/System/Library/Extensions', '/usr/libexec'];
                        _i = 0, restrictedPaths_1 = restrictedPaths;
                        _a.label = 1;
                    case 1:
                        if (!(_i < restrictedPaths_1.length)) return [3 /*break*/, 4];
                        restrictedPath = restrictedPaths_1[_i];
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists(restrictedPath)];
                    case 2:
                        exists = _a.sent();
                        assert.strictEqual(typeof exists, 'boolean');
                        _a.label = 3;
                    case 3:
                        _i++;
                        return [3 /*break*/, 1];
                    case 4: return [2 /*return*/];
                }
            });
        }); });
        test('should handle case-sensitive vs case-insensitive filesystems', function () { return __awaiter(void 0, void 0, void 0, function () {
            var testPath, lowerPath, normalizedUpper, normalizedLower;
            return __generator(this, function (_a) {
                testPath = '/tmp/CaseSensitiveTest';
                lowerPath = '/tmp/casesensitivetest';
                normalizedUpper = platformUtils_js_1.PlatformUtils.normalizePath(testPath);
                normalizedLower = platformUtils_js_1.PlatformUtils.normalizePath(lowerPath);
                assert.ok(normalizedUpper.length > 0);
                assert.ok(normalizedLower.length > 0);
                return [2 /*return*/];
            });
        }); });
        test('should handle macOS-specific directories', function () { return __awaiter(void 0, void 0, void 0, function () {
            var macOSDirs, _i, macOSDirs_1, dir, exists;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        macOSDirs = ['/Applications', '/Library', '/System', '/Users', '/Volumes'];
                        _i = 0, macOSDirs_1 = macOSDirs;
                        _a.label = 1;
                    case 1:
                        if (!(_i < macOSDirs_1.length)) return [3 /*break*/, 4];
                        dir = macOSDirs_1[_i];
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists(dir)];
                    case 2:
                        exists = _a.sent();
                        assert.strictEqual(exists, true, "".concat(dir, " should exist on macOS"));
                        _a.label = 3;
                    case 3:
                        _i++;
                        return [3 /*break*/, 1];
                    case 4: return [2 /*return*/];
                }
            });
        }); });
    });
    suite('Error Handling', function () {
        test('should handle macOS-specific path errors gracefully', function () { return __awaiter(void 0, void 0, void 0, function () {
            var invalidPaths, _i, invalidPaths_1, invalidPath, exists, isExecutable;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        invalidPaths = [
                            '/nonexistent/path/that/does/not/exist',
                            '/System/Library/NonExistent/file',
                            '/private/var/nonexistent',
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
        test('should handle macOS permission errors', function () { return __awaiter(void 0, void 0, void 0, function () {
            var restrictedFiles, _i, restrictedFiles_1, file, exists, isExecutable;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        restrictedFiles = [
                            '/private/var/db/dslocal/nodes/Default/users/root.plist',
                            '/System/Library/Extensions',
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
        test('should handle SIP (System Integrity Protection) restrictions', function () { return __awaiter(void 0, void 0, void 0, function () {
            var sipProtectedPaths, _i, sipProtectedPaths_1, sipPath, exists;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        sipProtectedPaths = ['/System/Library/Frameworks', '/usr/bin/codesign'];
                        _i = 0, sipProtectedPaths_1 = sipProtectedPaths;
                        _a.label = 1;
                    case 1:
                        if (!(_i < sipProtectedPaths_1.length)) return [3 /*break*/, 4];
                        sipPath = sipProtectedPaths_1[_i];
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists(sipPath)];
                    case 2:
                        exists = _a.sent();
                        // Should not throw errors
                        assert.strictEqual(typeof exists, 'boolean');
                        _a.label = 3;
                    case 3:
                        _i++;
                        return [3 /*break*/, 1];
                    case 4: return [2 /*return*/];
                }
            });
        }); });
    });
});
