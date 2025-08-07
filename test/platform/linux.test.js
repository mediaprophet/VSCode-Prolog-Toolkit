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
var platformUtils_1 = require("../../src/utils/platformUtils");
var executableFinder_1 = require("../../src/utils/executableFinder");
var packageManagerIntegration_1 = require("../../src/features/packageManagerIntegration");
/**
 * Linux-specific platform tests
 */
suite('Linux Platform Tests', function () {
    // Skip tests if not running on Linux
    var isLinux = os.platform() === 'linux';
    suiteSetup(function () {
        if (!isLinux) {
            this.skip();
        }
    });
    suite('Platform Detection', function () {
        test('should detect Linux platform correctly', function () {
            var platform = platformUtils_1.PlatformUtils.getPlatform();
            assert.strictEqual(platform, 'linux');
        });
        test('should detect architecture correctly', function () {
            var arch = platformUtils_1.PlatformUtils.getArchitecture();
            assert.ok(['x64', 'arm64', 'x86'].includes(arch));
        });
        test('should provide Linux-specific defaults', function () {
            var defaults = platformUtils_1.PlatformUtils.getPlatformDefaults();
            assert.strictEqual(defaults.pathSeparator, '/');
            assert.strictEqual(defaults.executableExtension, '');
            assert.ok(defaults.executablePaths.some(function (path) { return path.includes('/usr/bin'); }));
            assert.ok(defaults.executablePaths.some(function (path) { return path.includes('/usr/local/bin'); }));
        });
    });
    suite('Path Handling', function () {
        test('should normalize Unix paths correctly', function () {
            var testPaths = [
                '/usr/bin/swipl',
                '/usr/local/bin/swipl',
                '/opt/swipl/bin/swipl',
                '../relative/path/file',
                '~/.local/bin/swipl',
                '/snap/bin/swi-prolog',
            ];
            testPaths.forEach(function (testPath) {
                var normalized = platformUtils_1.PlatformUtils.normalizePath(testPath);
                assert.ok(normalized.length > 0);
                // Should not contain double slashes
                assert.ok(!normalized.includes('//'));
            });
        });
        test('should expand home directory (~) correctly', function () {
            var homeDir = os.homedir();
            var pathWithTilde = '~/.local/bin/swipl';
            var normalized = platformUtils_1.PlatformUtils.normalizePath(pathWithTilde);
            assert.ok(normalized.startsWith(homeDir));
            assert.ok(!normalized.includes('~'));
        });
        test('should expand Unix environment variables', function () {
            var pathWithEnvVar = '$HOME/.local/bin/swipl';
            var expanded = platformUtils_1.PlatformUtils.expandEnvironmentVariables(pathWithEnvVar);
            if (process.env.HOME) {
                assert.ok(expanded.includes(process.env.HOME));
                assert.ok(!expanded.includes('$HOME'));
            }
        });
        test('should handle Linux-specific paths', function () {
            var linuxPaths = [
                '/usr/lib/swi-prolog/bin/x86_64-linux/swipl',
                '/snap/bin/swi-prolog',
                '/var/lib/flatpak/exports/bin/org.swi_prolog.SWI-Prolog',
            ];
            linuxPaths.forEach(function (linuxPath) {
                var normalized = platformUtils_1.PlatformUtils.normalizePath(linuxPath);
                assert.ok(normalized.startsWith('/'));
                assert.ok(!normalized.includes('//'));
            });
        });
        test('should join paths correctly', function () {
            var joined = platformUtils_1.PlatformUtils.joinPath('/usr', 'local', 'bin', 'swipl');
            assert.strictEqual(joined, '/usr/local/bin/swipl');
        });
    });
    suite('Executable Detection', function () {
        test('should detect Linux executable paths', function () {
            var executablePaths = platformUtils_1.PlatformUtils.getExecutablePaths();
            assert.ok(executablePaths.length > 0);
            assert.ok(executablePaths.every(function (path) { return !path.endsWith('.exe'); }));
            assert.ok(executablePaths.some(function (path) { return path.includes('/usr/bin'); }));
            assert.ok(executablePaths.some(function (path) { return path.includes('/usr/local/bin'); }));
        });
        test('should find SWI-Prolog executable on Linux', function () {
            return __awaiter(this, void 0, void 0, function () {
                var finder, result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(10000); // Increase timeout for executable detection
                            finder = new executableFinder_1.ExecutableFinder();
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
        test('should validate Linux executable paths', function () {
            return __awaiter(this, void 0, void 0, function () {
                var finder, invalidResult, textFileResult;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(5000);
                            finder = new executableFinder_1.ExecutableFinder();
                            return [4 /*yield*/, finder.validateExecutable('/nonexistent/swipl')];
                        case 1:
                            invalidResult = _a.sent();
                            assert.strictEqual(invalidResult.found, false);
                            assert.ok(invalidResult.issues);
                            return [4 /*yield*/, finder.validateExecutable('/etc/hosts')];
                        case 2:
                            textFileResult = _a.sent();
                            return [4 /*yield*/, platformUtils_1.PlatformUtils.pathExists('/etc/hosts')];
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
        test('should handle Linux-specific executable locations', function () {
            return __awaiter(this, void 0, void 0, function () {
                var commonLocations, finder, _i, commonLocations_1, location_1, result;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(5000);
                            commonLocations = [
                                '/usr/bin/swipl',
                                '/usr/local/bin/swipl',
                                '/opt/swipl/bin/swipl',
                                '/snap/bin/swi-prolog',
                                '/usr/lib/swi-prolog/bin/x86_64-linux/swipl',
                            ];
                            finder = new executableFinder_1.ExecutableFinder();
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
        test('should detect Linux package managers', function () {
            return __awaiter(this, void 0, void 0, function () {
                var packageManager, availableManagers;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(15000); // Package manager detection can be slow
                            packageManager = packageManagerIntegration_1.PackageManagerIntegration.getInstance();
                            return [4 /*yield*/, packageManager.detectAvailableManagers()];
                        case 1:
                            availableManagers = _a.sent();
                            // Should detect available Linux package managers
                            availableManagers.forEach(function (manager) {
                                assert.ok(['apt', 'dnf', 'yum', 'pacman', 'zypper', 'snap', 'flatpak'].includes(manager.name));
                                assert.strictEqual(manager.isAvailable, true);
                                assert.ok(manager.installCommand.length > 0);
                            });
                            return [2 /*return*/];
                    }
                });
            });
        });
        test('should provide Linux installation suggestions', function () {
            return __awaiter(this, void 0, void 0, function () {
                var packageManager, suggestions;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(5000);
                            packageManager = packageManagerIntegration_1.PackageManagerIntegration.getInstance();
                            return [4 /*yield*/, packageManager.getInstallationSuggestions()];
                        case 1:
                            suggestions = _a.sent();
                            assert.ok(suggestions.length > 0);
                            assert.ok(suggestions.some(function (s) { return s.includes('Linux'); }));
                            assert.ok(suggestions.some(function (s) { return s.includes('apt') || s.includes('yum') || s.includes('dnf') || s.includes('pacman'); }));
                            return [2 /*return*/];
                    }
                });
            });
        });
        test('should get Linux-specific recommendations', function () {
            var packageManager = packageManagerIntegration_1.PackageManagerIntegration.getInstance();
            var recommendations = packageManager.getRecommendedManagers();
            assert.deepStrictEqual(recommendations, [
                'apt',
                'dnf',
                'yum',
                'pacman',
                'zypper',
                'snap',
                'flatpak',
            ]);
        });
        test('should handle distribution-specific package managers', function () {
            return __awaiter(this, void 0, void 0, function () {
                var packageManager, availableManagers, managerNames, commonManagers_1, hasCommonManager;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            this.timeout(10000);
                            packageManager = packageManagerIntegration_1.PackageManagerIntegration.getInstance();
                            return [4 /*yield*/, packageManager.detectAvailableManagers()];
                        case 1:
                            availableManagers = _a.sent();
                            managerNames = availableManagers.map(function (m) { return m.name; });
                            // At least one package manager should be available on any Linux system
                            if (managerNames.length > 0) {
                                commonManagers_1 = ['apt', 'dnf', 'yum', 'pacman', 'zypper'];
                                hasCommonManager = managerNames.some(function (name) { return commonManagers_1.includes(name); });
                                // Should have at least one traditional package manager or snap/flatpak
                                assert.ok(hasCommonManager || managerNames.includes('snap') || managerNames.includes('flatpak'));
                            }
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    suite('Environment Variables', function () {
        test('should handle Unix environment variables', function () {
            var envVars = platformUtils_1.PlatformUtils.getEnvironmentVariables();
            assert.ok(envVars.crossPlatform.includes('PATH'));
            assert.ok(envVars.platformSpecific.includes('HOME'));
            assert.ok(envVars.platformSpecific.includes('XDG_CONFIG_HOME'));
            assert.ok(envVars.platformSpecific.includes('TMPDIR'));
        });
        test('should expand Unix-style environment variables', function () {
            // Test Unix-style $VAR and ${VAR} expansion
            var testCases = [
                {
                    input: '$HOME/.local/bin',
                    expected: process.env.HOME ? process.env.HOME + '/.local/bin' : '$HOME/.local/bin',
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
                var result = platformUtils_1.PlatformUtils.expandEnvironmentVariables(testCase.input);
                if (testCase.expected !== testCase.input) {
                    // Environment variable should be expanded
                    assert.strictEqual(result, testCase.expected);
                }
            });
        });
        test('should handle XDG Base Directory specification', function () {
            var envVars = platformUtils_1.PlatformUtils.getEnvironmentVariables();
            // XDG variables should be included
            assert.ok(envVars.platformSpecific.includes('XDG_CONFIG_HOME'));
            assert.ok(envVars.platformSpecific.includes('XDG_DATA_HOME'));
        });
    });
    suite('File System Operations', function () {
        test('should check file existence on Linux', function () { return __awaiter(void 0, void 0, void 0, function () {
            var systemFiles, _i, systemFiles_1, file, exists;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        systemFiles = ['/bin/bash', '/usr/bin/which', '/etc/passwd', '/proc/version'];
                        _i = 0, systemFiles_1 = systemFiles;
                        _a.label = 1;
                    case 1:
                        if (!(_i < systemFiles_1.length)) return [3 /*break*/, 4];
                        file = systemFiles_1[_i];
                        return [4 /*yield*/, platformUtils_1.PlatformUtils.pathExists(file)];
                    case 2:
                        exists = _a.sent();
                        // These files should exist on any Linux system
                        assert.strictEqual(exists, true, "".concat(file, " should exist on Linux"));
                        _a.label = 3;
                    case 3:
                        _i++;
                        return [3 /*break*/, 1];
                    case 4: return [2 /*return*/];
                }
            });
        }); });
        test('should check executable permissions on Linux', function () { return __awaiter(void 0, void 0, void 0, function () {
            var executables, _i, executables_1, exe, isExecutable;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        executables = ['/bin/bash', '/usr/bin/which', '/bin/ls'];
                        _i = 0, executables_1 = executables;
                        _a.label = 1;
                    case 1:
                        if (!(_i < executables_1.length)) return [3 /*break*/, 5];
                        exe = executables_1[_i];
                        return [4 /*yield*/, platformUtils_1.PlatformUtils.pathExists(exe)];
                    case 2:
                        if (!_a.sent()) return [3 /*break*/, 4];
                        return [4 /*yield*/, platformUtils_1.PlatformUtils.isExecutable(exe)];
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
        test('should handle Linux file permissions correctly', function () { return __awaiter(void 0, void 0, void 0, function () {
            var nonExecutables, _i, nonExecutables_1, file, isExecutable;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        nonExecutables = ['/etc/hosts', '/etc/passwd', '/proc/version'];
                        _i = 0, nonExecutables_1 = nonExecutables;
                        _a.label = 1;
                    case 1:
                        if (!(_i < nonExecutables_1.length)) return [3 /*break*/, 5];
                        file = nonExecutables_1[_i];
                        return [4 /*yield*/, platformUtils_1.PlatformUtils.pathExists(file)];
                    case 2:
                        if (!_a.sent()) return [3 /*break*/, 4];
                        return [4 /*yield*/, platformUtils_1.PlatformUtils.isExecutable(file)];
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
        test('should handle symbolic links', function () { return __awaiter(void 0, void 0, void 0, function () {
            var symlinks, _i, symlinks_1, symlink, isExecutable;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        symlinks = [
                            '/bin/sh', // Often a symlink to bash or dash
                            '/usr/bin/python3', // Often a symlink
                        ];
                        _i = 0, symlinks_1 = symlinks;
                        _a.label = 1;
                    case 1:
                        if (!(_i < symlinks_1.length)) return [3 /*break*/, 5];
                        symlink = symlinks_1[_i];
                        return [4 /*yield*/, platformUtils_1.PlatformUtils.pathExists(symlink)];
                    case 2:
                        if (!_a.sent()) return [3 /*break*/, 4];
                        return [4 /*yield*/, platformUtils_1.PlatformUtils.isExecutable(symlink)];
                    case 3:
                        isExecutable = _a.sent();
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
        test('should provide Linux-specific configuration location', function () {
            var configLocation = platformUtils_1.PlatformUtils.getConfigurationLocation();
            assert.ok(configLocation.includes('.config') || configLocation.includes('~/.config'));
        });
        test('should provide Linux temp directory', function () {
            var tempDir = platformUtils_1.PlatformUtils.getTempDirectory();
            assert.ok(tempDir.length > 0);
            // Should be a valid Unix path
            assert.ok(tempDir.startsWith('/') || tempDir.includes('tmp'));
        });
        test('should provide Linux home directory', function () {
            var homeDir = platformUtils_1.PlatformUtils.getHomeDirectory();
            assert.ok(homeDir.length > 0);
            // Should be a valid Unix path
            assert.ok(homeDir.startsWith('/'));
            assert.ok(homeDir.includes('/home/') || homeDir.includes('/root'));
        });
    });
    suite('Platform Info', function () {
        test('should provide comprehensive Linux platform info', function () {
            var info = platformUtils_1.PlatformUtils.getPlatformInfo();
            assert.strictEqual(info.platform, 'linux');
            assert.ok(['x64', 'arm64', 'x86'].includes(info.architecture));
            assert.ok(info.osVersion.length > 0);
            assert.ok(info.nodeVersion.startsWith('v'));
            assert.strictEqual(info.pathSeparator, '/');
            assert.strictEqual(info.executableExtension, '');
            assert.ok(info.homeDirectory.length > 0);
            assert.ok(info.tempDirectory.length > 0);
        });
        test('should detect Linux kernel version', function () {
            var info = platformUtils_1.PlatformUtils.getPlatformInfo();
            // Linux version should be kernel version like "5.4.0-74-generic"
            assert.ok(info.osVersion.match(/^\d+\.\d+/));
        });
    });
    suite('Linux-Specific Features', function () {
        test('should handle different Linux distributions', function () { return __awaiter(void 0, void 0, void 0, function () {
            var distroSpecificPaths, _i, distroSpecificPaths_1, distroPath, exists;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        distroSpecificPaths = [
                            '/etc/debian_version', // Debian/Ubuntu
                            '/etc/redhat-release', // Red Hat/CentOS/Fedora
                            '/etc/arch-release', // Arch Linux
                            '/etc/suse-release', // openSUSE
                            '/etc/os-release', // Standard across distributions
                        ];
                        _i = 0, distroSpecificPaths_1 = distroSpecificPaths;
                        _a.label = 1;
                    case 1:
                        if (!(_i < distroSpecificPaths_1.length)) return [3 /*break*/, 4];
                        distroPath = distroSpecificPaths_1[_i];
                        return [4 /*yield*/, platformUtils_1.PlatformUtils.pathExists(distroPath)];
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
        test('should handle snap and flatpak paths', function () { return __awaiter(void 0, void 0, void 0, function () {
            var snapFlatpakPaths, _i, snapFlatpakPaths_1, snapPath, exists;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        snapFlatpakPaths = [
                            '/snap',
                            '/var/lib/snapd',
                            '/var/lib/flatpak',
                            '/home/' + process.env.USER + '/.local/share/flatpak',
                        ];
                        _i = 0, snapFlatpakPaths_1 = snapFlatpakPaths;
                        _a.label = 1;
                    case 1:
                        if (!(_i < snapFlatpakPaths_1.length)) return [3 /*break*/, 4];
                        snapPath = snapFlatpakPaths_1[_i];
                        return [4 /*yield*/, platformUtils_1.PlatformUtils.pathExists(snapPath)];
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
        test('should handle container environments', function () { return __awaiter(void 0, void 0, void 0, function () {
            var containerPaths, _i, containerPaths_1, containerPath, exists;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        containerPaths = [
                            '/.dockerenv', // Docker
                            '/run/.containerenv', // Podman
                            '/proc/1/cgroup', // Container detection
                        ];
                        _i = 0, containerPaths_1 = containerPaths;
                        _a.label = 1;
                    case 1:
                        if (!(_i < containerPaths_1.length)) return [3 /*break*/, 4];
                        containerPath = containerPaths_1[_i];
                        return [4 /*yield*/, platformUtils_1.PlatformUtils.pathExists(containerPath)];
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
        test('should handle different filesystem types', function () { return __awaiter(void 0, void 0, void 0, function () {
            var mountPoints, _i, mountPoints_1, mountPoint, exists;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        mountPoints = ['/proc', '/sys', '/dev', '/tmp'];
                        _i = 0, mountPoints_1 = mountPoints;
                        _a.label = 1;
                    case 1:
                        if (!(_i < mountPoints_1.length)) return [3 /*break*/, 4];
                        mountPoint = mountPoints_1[_i];
                        return [4 /*yield*/, platformUtils_1.PlatformUtils.pathExists(mountPoint)];
                    case 2:
                        exists = _a.sent();
                        // These should exist on any Linux system
                        assert.strictEqual(exists, true, "".concat(mountPoint, " should exist on Linux"));
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
        test('should handle Linux-specific path errors gracefully', function () { return __awaiter(void 0, void 0, void 0, function () {
            var invalidPaths, _i, invalidPaths_1, invalidPath, exists, isExecutable;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        invalidPaths = [
                            '/nonexistent/path/that/does/not/exist',
                            '/root/restricted/file', // May not be accessible
                            '/sys/nonexistent/file',
                        ];
                        _i = 0, invalidPaths_1 = invalidPaths;
                        _a.label = 1;
                    case 1:
                        if (!(_i < invalidPaths_1.length)) return [3 /*break*/, 5];
                        invalidPath = invalidPaths_1[_i];
                        return [4 /*yield*/, platformUtils_1.PlatformUtils.pathExists(invalidPath)];
                    case 2:
                        exists = _a.sent();
                        return [4 /*yield*/, platformUtils_1.PlatformUtils.isExecutable(invalidPath)];
                    case 3:
                        isExecutable = _a.sent();
                        // Should not throw errors
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
        test('should handle Linux permission errors', function () { return __awaiter(void 0, void 0, void 0, function () {
            var restrictedFiles, _i, restrictedFiles_1, file, exists, isExecutable;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        restrictedFiles = ['/etc/shadow', '/root/.bashrc', '/proc/kcore'];
                        _i = 0, restrictedFiles_1 = restrictedFiles;
                        _a.label = 1;
                    case 1:
                        if (!(_i < restrictedFiles_1.length)) return [3 /*break*/, 5];
                        file = restrictedFiles_1[_i];
                        return [4 /*yield*/, platformUtils_1.PlatformUtils.pathExists(file)];
                    case 2:
                        exists = _a.sent();
                        return [4 /*yield*/, platformUtils_1.PlatformUtils.isExecutable(file)];
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
        test('should handle SELinux and AppArmor restrictions', function () { return __awaiter(void 0, void 0, void 0, function () {
            var securityPaths, _i, securityPaths_1, secPath, exists;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        securityPaths = ['/sys/fs/selinux', '/sys/kernel/security/apparmor'];
                        _i = 0, securityPaths_1 = securityPaths;
                        _a.label = 1;
                    case 1:
                        if (!(_i < securityPaths_1.length)) return [3 /*break*/, 4];
                        secPath = securityPaths_1[_i];
                        return [4 /*yield*/, platformUtils_1.PlatformUtils.pathExists(secPath)];
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
        test('should handle network filesystems', function () { return __awaiter(void 0, void 0, void 0, function () {
            var networkPaths, _i, networkPaths_1, netPath, exists;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        networkPaths = ['/mnt', '/media'];
                        _i = 0, networkPaths_1 = networkPaths;
                        _a.label = 1;
                    case 1:
                        if (!(_i < networkPaths_1.length)) return [3 /*break*/, 4];
                        netPath = networkPaths_1[_i];
                        return [4 /*yield*/, platformUtils_1.PlatformUtils.pathExists(netPath)];
                    case 2:
                        exists = _a.sent();
                        // Should handle network filesystems gracefully
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
