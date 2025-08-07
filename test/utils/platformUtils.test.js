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
var path = require("path");
var platformUtils_js_1 = require("../../src/utils/platformUtils.js");
/**
 * Platform utilities comprehensive test suite
 */
suite('Platform Utils Tests', function () {
    var currentPlatform = os.platform();
    suite('Platform Detection', function () {
        test('should detect current platform correctly', function () {
            var detectedPlatform = platformUtils_js_1.PlatformUtils.getPlatform();
            // Map Node.js platform names to our platform names
            var expectedPlatform = currentPlatform === 'win32'
                ? 'windows'
                : currentPlatform === 'darwin'
                    ? 'macos'
                    : currentPlatform === 'linux'
                        ? 'linux'
                        : 'unknown';
            assert.strictEqual(detectedPlatform, expectedPlatform);
        });
        test('should detect architecture correctly', function () {
            var arch = platformUtils_js_1.PlatformUtils.getArchitecture();
            var validArchitectures = ['x64', 'arm64', 'x86', 'arm'];
            assert.ok(validArchitectures.includes(arch), "Invalid architecture: ".concat(arch));
        });
        test('should provide consistent platform info', function () {
            var info = platformUtils_js_1.PlatformUtils.getPlatformInfo();
            assert.ok(info.platform);
            assert.ok(info.architecture);
            assert.ok(info.osVersion);
            assert.ok(info.nodeVersion);
            assert.ok(typeof info.pathSeparator === 'string');
            assert.ok(typeof info.executableExtension === 'string');
            assert.ok(info.homeDirectory);
            assert.ok(info.tempDirectory);
        });
        test('should detect if running on Windows', function () {
            var isWindows = platformUtils_js_1.PlatformUtils.getPlatform() === 'windows';
            assert.strictEqual(isWindows, currentPlatform === 'win32');
        });
        test('should detect if running on macOS', function () {
            var isMacOS = platformUtils_js_1.PlatformUtils.getPlatform() === 'macos';
            assert.strictEqual(isMacOS, currentPlatform === 'darwin');
        });
        test('should detect if running on Linux', function () {
            var isLinux = platformUtils_js_1.PlatformUtils.getPlatform() === 'linux';
            assert.strictEqual(isLinux, currentPlatform === 'linux');
        });
    });
    suite('Path Handling', function () {
        test('should normalize paths correctly for current platform', function () {
            var testPaths = [
                'simple/path',
                './relative/path',
                '../parent/path',
                '/absolute/path',
                '~/home/path',
            ];
            // Add Windows-specific paths if on Windows
            if (platformUtils_js_1.PlatformUtils.getPlatform() === 'windows') {
                testPaths.push('C:\\Windows\\System32', 'C:/mixed/slashes/path', '\\\\server\\share\\file', '%USERPROFILE%\\Documents');
            }
            testPaths.forEach(function (testPath) {
                var normalized = platformUtils_js_1.PlatformUtils.normalizePath(testPath);
                assert.ok(normalized.length > 0);
                // Should use correct path separator for platform
                if (platformUtils_js_1.PlatformUtils.getPlatform() === 'windows') {
                    // Windows paths should use backslashes (except UNC paths)
                    if (!testPath.startsWith('\\\\')) {
                        assert.ok(!normalized.includes('/') || normalized.includes('://'));
                    }
                }
                else {
                    // Unix paths should use forward slashes
                    assert.ok(!normalized.includes('\\'));
                }
            });
        });
        test('should expand environment variables correctly', function () {
            var testCases = platformUtils_js_1.PlatformUtils.getPlatform() === 'windows'
                ? [
                    { input: '%USERPROFILE%\\test', envVar: 'USERPROFILE' },
                    { input: '%TEMP%\\file.txt', envVar: 'TEMP' },
                    { input: '%PATH%', envVar: 'PATH' },
                ]
                : [
                    { input: '$HOME/test', envVar: 'HOME' },
                    { input: '${HOME}/file.txt', envVar: 'HOME' },
                    { input: '$PATH', envVar: 'PATH' },
                ];
            testCases.forEach(function (testCase) {
                var expanded = platformUtils_js_1.PlatformUtils.expandEnvironmentVariables(testCase.input);
                if (process.env[testCase.envVar]) {
                    // Environment variable should be expanded
                    assert.ok(!expanded.includes(platformUtils_js_1.PlatformUtils.getPlatform() === 'windows' ? '%' : '$'));
                    assert.ok(expanded.includes(process.env[testCase.envVar]));
                }
            });
        });
        test('should handle home directory expansion', function () {
            var homeDir = os.homedir();
            var pathWithTilde = '~/test/file.txt';
            var normalized = platformUtils_js_1.PlatformUtils.normalizePath(pathWithTilde);
            assert.ok(normalized.includes(homeDir));
            assert.ok(!normalized.includes('~'));
        });
        test('should join paths correctly', function () {
            var parts = ['usr', 'local', 'bin', 'swipl'];
            var joined = platformUtils_js_1.PlatformUtils.joinPath.apply(platformUtils_js_1.PlatformUtils, parts);
            // Should use correct separator for platform
            var expectedSeparator = platformUtils_js_1.PlatformUtils.getPlatform() === 'windows' ? '\\' : '/';
            assert.ok(joined.includes(expectedSeparator));
            // Should contain all parts
            parts.forEach(function (part) {
                assert.ok(joined.includes(part));
            });
        });
        test('should get correct path separator', function () {
            var separator = platformUtils_js_1.PlatformUtils.getPathSeparator();
            var expectedSeparator = platformUtils_js_1.PlatformUtils.getPlatform() === 'windows' ? '\\' : '/';
            assert.strictEqual(separator, expectedSeparator);
        });
    });
    suite('Platform Defaults', function () {
        test('should provide platform-specific defaults', function () {
            var defaults = platformUtils_js_1.PlatformUtils.getPlatformDefaults();
            assert.ok(typeof defaults.pathSeparator === 'string');
            assert.ok(typeof defaults.executableExtension === 'string');
            assert.ok(Array.isArray(defaults.executablePaths));
            assert.ok(defaults.executablePaths.length > 0);
            if (platformUtils_js_1.PlatformUtils.getPlatform() === 'windows') {
                assert.strictEqual(defaults.pathSeparator, '\\');
                assert.strictEqual(defaults.executableExtension, '.exe');
                assert.ok(defaults.executablePaths.some(function (p) { return p.includes('Program Files'); }));
            }
            else {
                assert.strictEqual(defaults.pathSeparator, '/');
                assert.strictEqual(defaults.executableExtension, '');
                assert.ok(defaults.executablePaths.some(function (p) { return p.includes('/usr/bin'); }));
            }
        });
        test('should provide executable paths for current platform', function () {
            var executablePaths = platformUtils_js_1.PlatformUtils.getExecutablePaths();
            assert.ok(Array.isArray(executablePaths));
            assert.ok(executablePaths.length > 0);
            // All paths should be absolute
            executablePaths.forEach(function (execPath) {
                assert.ok(path.isAbsolute(execPath) || execPath.includes('~'));
            });
        });
        test('should provide correct executable extension', function () {
            var extension = platformUtils_js_1.PlatformUtils.getExecutableExtension();
            if (platformUtils_js_1.PlatformUtils.getPlatform() === 'windows') {
                assert.strictEqual(extension, '.exe');
            }
            else {
                assert.strictEqual(extension, '');
            }
        });
    });
    suite('Environment Variables', function () {
        test('should provide environment variable lists', function () {
            var envVars = platformUtils_js_1.PlatformUtils.getEnvironmentVariables();
            assert.ok(Array.isArray(envVars.crossPlatform));
            assert.ok(Array.isArray(envVars.platformSpecific));
            // Should include common cross-platform variables
            assert.ok(envVars.crossPlatform.includes('PATH'));
            assert.ok(envVars.crossPlatform.includes('HOME') || envVars.crossPlatform.includes('USERPROFILE'));
            // Should include platform-specific variables
            if (platformUtils_js_1.PlatformUtils.getPlatform() === 'windows') {
                assert.ok(envVars.platformSpecific.includes('USERPROFILE'));
                assert.ok(envVars.platformSpecific.includes('TEMP'));
                assert.ok(envVars.platformSpecific.includes('APPDATA'));
            }
            else {
                assert.ok(envVars.platformSpecific.includes('HOME'));
                assert.ok(envVars.platformSpecific.includes('TMPDIR') || envVars.platformSpecific.includes('TMP'));
            }
        });
        test('should expand multiple environment variables', function () {
            var input = platformUtils_js_1.PlatformUtils.getPlatform() === 'windows'
                ? '%USERPROFILE%\\%USERNAME%\\test'
                : '$HOME/$USER/test';
            var expanded = platformUtils_js_1.PlatformUtils.expandEnvironmentVariables(input);
            // Should not contain variable markers after expansion
            if (platformUtils_js_1.PlatformUtils.getPlatform() === 'windows') {
                assert.ok(!expanded.includes('%'));
            }
            else {
                assert.ok(!expanded.includes('$'));
            }
        });
    });
    suite('File System Operations', function () {
        test('should check file existence', function () { return __awaiter(void 0, void 0, void 0, function () {
            var currentFile, exists, nonExistent, notExists;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        currentFile = __filename;
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists(currentFile)];
                    case 1:
                        exists = _a.sent();
                        assert.strictEqual(exists, true);
                        nonExistent = path.join(__dirname, 'nonexistent-file-12345.txt');
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists(nonExistent)];
                    case 2:
                        notExists = _a.sent();
                        assert.strictEqual(notExists, false);
                        return [2 /*return*/];
                }
            });
        }); });
        test('should check executable permissions', function () { return __awaiter(void 0, void 0, void 0, function () {
            var nodeExecutable, isExecutable, currentFile, isNotExecutable;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        nodeExecutable = process.execPath;
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.isExecutable(nodeExecutable)];
                    case 1:
                        isExecutable = _a.sent();
                        assert.strictEqual(isExecutable, true);
                        currentFile = __filename;
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.isExecutable(currentFile)];
                    case 2:
                        isNotExecutable = _a.sent();
                        assert.strictEqual(isNotExecutable, false);
                        return [2 /*return*/];
                }
            });
        }); });
        test('should handle non-existent files gracefully', function () { return __awaiter(void 0, void 0, void 0, function () {
            var nonExistent, exists, isExecutable;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        nonExistent = path.join(__dirname, 'nonexistent-file-12345.txt');
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists(nonExistent)];
                    case 1:
                        exists = _a.sent();
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.isExecutable(nonExistent)];
                    case 2:
                        isExecutable = _a.sent();
                        assert.strictEqual(exists, false);
                        assert.strictEqual(isExecutable, false);
                        return [2 /*return*/];
                }
            });
        }); });
    });
    suite('Directory Operations', function () {
        test('should get home directory', function () {
            var homeDir = platformUtils_js_1.PlatformUtils.getHomeDirectory();
            var osHomeDir = os.homedir();
            assert.strictEqual(homeDir, osHomeDir);
            assert.ok(homeDir.length > 0);
        });
        test('should get temp directory', function () {
            var tempDir = platformUtils_js_1.PlatformUtils.getTempDirectory();
            var osTempDir = os.tmpdir();
            assert.strictEqual(tempDir, osTempDir);
            assert.ok(tempDir.length > 0);
        });
        test('should get configuration location', function () {
            var configLocation = platformUtils_js_1.PlatformUtils.getConfigurationLocation();
            assert.ok(configLocation.length > 0);
            assert.ok(path.isAbsolute(configLocation) || configLocation.includes('~'));
            if (platformUtils_js_1.PlatformUtils.getPlatform() === 'windows') {
                assert.ok(configLocation.includes('AppData') || configLocation.includes('%'));
            }
            else if (platformUtils_js_1.PlatformUtils.getPlatform() === 'macos') {
                assert.ok(configLocation.includes('Library') || configLocation.includes('~'));
            }
            else {
                assert.ok(configLocation.includes('.config') || configLocation.includes('~'));
            }
        });
    });
    suite('Cross-Platform Compatibility', function () {
        test('should handle mixed path separators', function () {
            var mixedPath = 'some/path\\with/mixed\\separators';
            var normalized = platformUtils_js_1.PlatformUtils.normalizePath(mixedPath);
            // Should use consistent separators
            var separator = platformUtils_js_1.PlatformUtils.getPathSeparator();
            var wrongSeparator = separator === '/' ? '\\' : '/';
            // UNC paths on Windows are an exception
            if (!(platformUtils_js_1.PlatformUtils.getPlatform() === 'windows' && normalized.startsWith('\\\\'))) {
                assert.ok(!normalized.includes(wrongSeparator));
            }
        });
        test('should handle empty and null inputs gracefully', function () {
            // Empty string
            var emptyNormalized = platformUtils_js_1.PlatformUtils.normalizePath('');
            assert.strictEqual(emptyNormalized, '');
            var emptyExpanded = platformUtils_js_1.PlatformUtils.expandEnvironmentVariables('');
            assert.strictEqual(emptyExpanded, '');
            // Whitespace
            var whitespaceNormalized = platformUtils_js_1.PlatformUtils.normalizePath('   ');
            assert.strictEqual(whitespaceNormalized.trim(), '');
        });
        test('should handle special characters in paths', function () {
            var specialPaths = [
                'path with spaces',
                'path-with-dashes',
                'path_with_underscores',
                'path.with.dots',
                'path(with)parentheses',
            ];
            specialPaths.forEach(function (specialPath) {
                var normalized = platformUtils_js_1.PlatformUtils.normalizePath(specialPath);
                assert.ok(normalized.length > 0);
                // Should preserve special characters
                assert.ok(normalized.includes(specialPath) || normalized.includes(path.normalize(specialPath)));
            });
        });
    });
    suite('Error Handling', function () {
        test('should handle invalid paths gracefully', function () { return __awaiter(void 0, void 0, void 0, function () {
            var invalidPaths, _i, invalidPaths_1, invalidPath, exists, isExecutable;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        invalidPaths = ['', '   ', '\0invalid\0path', 'path\nwith\nnewlines'];
                        _i = 0, invalidPaths_1 = invalidPaths;
                        _a.label = 1;
                    case 1:
                        if (!(_i < invalidPaths_1.length)) return [3 /*break*/, 5];
                        invalidPath = invalidPaths_1[_i];
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists(invalidPath)];
                    case 2:
                        exists = _a.sent();
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.isExecutable(invalidPath)];
                    case 3:
                        isExecutable = _a.sent();
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
        test('should handle permission errors gracefully', function () { return __awaiter(void 0, void 0, void 0, function () {
            var restrictedPaths, _i, restrictedPaths_1, restrictedPath, exists, isExecutable;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        restrictedPaths = platformUtils_js_1.PlatformUtils.getPlatform() === 'windows'
                            ? ['C:\\Windows\\System32\\config\\SAM', 'C:\\pagefile.sys']
                            : ['/etc/shadow', '/root/.bashrc'];
                        _i = 0, restrictedPaths_1 = restrictedPaths;
                        _a.label = 1;
                    case 1:
                        if (!(_i < restrictedPaths_1.length)) return [3 /*break*/, 5];
                        restrictedPath = restrictedPaths_1[_i];
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists(restrictedPath)];
                    case 2:
                        exists = _a.sent();
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.isExecutable(restrictedPath)];
                    case 3:
                        isExecutable = _a.sent();
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
        test('should handle network paths appropriately', function () { return __awaiter(void 0, void 0, void 0, function () {
            var networkPaths, _i, networkPaths_1, networkPath, exists;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        networkPaths = platformUtils_js_1.PlatformUtils.getPlatform() === 'windows'
                            ? ['\\\\nonexistent\\share\\file', '\\\\localhost\\c$\\Windows']
                            : ['/mnt/nonexistent', '/net/nonexistent'];
                        _i = 0, networkPaths_1 = networkPaths;
                        _a.label = 1;
                    case 1:
                        if (!(_i < networkPaths_1.length)) return [3 /*break*/, 4];
                        networkPath = networkPaths_1[_i];
                        return [4 /*yield*/, platformUtils_js_1.PlatformUtils.pathExists(networkPath)];
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
    });
    suite('Integration', function () {
        test('should work with Node.js path module', function () {
            var testPath = platformUtils_js_1.PlatformUtils.joinPath('test', 'path', 'file.txt');
            var nodePath = path.join('test', 'path', 'file.txt');
            // Results should be equivalent
            assert.strictEqual(path.normalize(testPath), path.normalize(nodePath));
        });
        test('should work with Node.js os module', function () {
            var platformInfo = platformUtils_js_1.PlatformUtils.getPlatformInfo();
            // Should match Node.js os module results
            assert.strictEqual(platformInfo.homeDirectory, os.homedir());
            assert.strictEqual(platformInfo.tempDirectory, os.tmpdir());
            assert.ok(platformInfo.nodeVersion.startsWith('v'));
        });
        test('should provide consistent results across calls', function () {
            // Multiple calls should return identical results
            var platform1 = platformUtils_js_1.PlatformUtils.getPlatform();
            var platform2 = platformUtils_js_1.PlatformUtils.getPlatform();
            assert.strictEqual(platform1, platform2);
            var arch1 = platformUtils_js_1.PlatformUtils.getArchitecture();
            var arch2 = platformUtils_js_1.PlatformUtils.getArchitecture();
            assert.strictEqual(arch1, arch2);
            var info1 = platformUtils_js_1.PlatformUtils.getPlatformInfo();
            var info2 = platformUtils_js_1.PlatformUtils.getPlatformInfo();
            assert.deepStrictEqual(info1, info2);
        });
    });
});
