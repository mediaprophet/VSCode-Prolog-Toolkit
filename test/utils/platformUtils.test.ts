import * as assert from 'assert';
import * as os from 'os';
import * as path from 'path';
import { PlatformUtils } from '../../src/utils/platformUtils.js';

/**
 * Platform utilities comprehensive test suite
 */
suite('Platform Utils Tests', () => {
  const currentPlatform = os.platform();

  suite('Platform Detection', () => {
    test('should detect current platform correctly', () => {
      const detectedPlatform = PlatformUtils.getPlatform();

      // Map Node.js platform names to our platform names
      const expectedPlatform =
        currentPlatform === 'win32'
          ? 'windows'
          : currentPlatform === 'darwin'
            ? 'macos'
            : currentPlatform === 'linux'
              ? 'linux'
              : 'unknown';

      assert.strictEqual(detectedPlatform, expectedPlatform);
    });

    test('should detect architecture correctly', () => {
      const arch = PlatformUtils.getArchitecture();
      const validArchitectures = ['x64', 'arm64', 'x86', 'arm'];

      assert.ok(validArchitectures.includes(arch), `Invalid architecture: ${arch}`);
    });

    test('should provide consistent platform info', () => {
      const info = PlatformUtils.getPlatformInfo();

      assert.ok(info.platform);
      assert.ok(info.architecture);
      assert.ok(info.osVersion);
      assert.ok(info.nodeVersion);
      assert.ok(typeof info.pathSeparator === 'string');
      assert.ok(typeof info.executableExtension === 'string');
      assert.ok(info.homeDirectory);
      assert.ok(info.tempDirectory);
    });

    test('should detect if running on Windows', () => {
      const isWindows = PlatformUtils.getPlatform() === 'windows';
      assert.strictEqual(isWindows, currentPlatform === 'win32');
    });

    test('should detect if running on macOS', () => {
      const isMacOS = PlatformUtils.getPlatform() === 'macos';
      assert.strictEqual(isMacOS, currentPlatform === 'darwin');
    });

    test('should detect if running on Linux', () => {
      const isLinux = PlatformUtils.getPlatform() === 'linux';
      assert.strictEqual(isLinux, currentPlatform === 'linux');
    });
  });

  suite('Path Handling', () => {
    test('should normalize paths correctly for current platform', () => {
      const testPaths = [
        'simple/path',
        './relative/path',
        '../parent/path',
        '/absolute/path',
        '~/home/path',
      ];

      // Add Windows-specific paths if on Windows
      if (PlatformUtils.getPlatform() === 'windows') {
        testPaths.push(
          'C:\\Windows\\System32',
          'C:/mixed/slashes/path',
          '\\\\server\\share\\file',
          '%USERPROFILE%\\Documents'
        );
      }

      testPaths.forEach(testPath => {
        const normalized = PlatformUtils.normalizePath(testPath);
        assert.ok(normalized.length > 0);

        // Should use correct path separator for platform
        if (PlatformUtils.getPlatform() === 'windows') {
          // Windows paths should use backslashes (except UNC paths)
          if (!testPath.startsWith('\\\\')) {
            assert.ok(!normalized.includes('/') || normalized.includes('://'));
          }
        } else {
          // Unix paths should use forward slashes
          assert.ok(!normalized.includes('\\'));
        }
      });
    });

    test('should expand environment variables correctly', () => {
      const testCases =
        PlatformUtils.getPlatform() === 'windows'
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

      testCases.forEach(testCase => {
        const expanded = PlatformUtils.expandEnvironmentVariables(testCase.input);

        if (process.env[testCase.envVar]) {
          // Environment variable should be expanded
          assert.ok(!expanded.includes(PlatformUtils.getPlatform() === 'windows' ? '%' : '$'));
          assert.ok(expanded.includes(process.env[testCase.envVar]!));
        }
      });
    });

    test('should handle home directory expansion', () => {
      const homeDir = os.homedir();
      const pathWithTilde = '~/test/file.txt';
      const normalized = PlatformUtils.normalizePath(pathWithTilde);

      assert.ok(normalized.includes(homeDir));
      assert.ok(!normalized.includes('~'));
    });

    test('should join paths correctly', () => {
      const parts = ['usr', 'local', 'bin', 'swipl'];
      const joined = PlatformUtils.joinPath(...parts);

      // Should use correct separator for platform
      const expectedSeparator = PlatformUtils.getPlatform() === 'windows' ? '\\' : '/';
      assert.ok(joined.includes(expectedSeparator));

      // Should contain all parts
      parts.forEach(part => {
        assert.ok(joined.includes(part));
      });
    });

    test('should get correct path separator', () => {
      const separator = PlatformUtils.getPathSeparator();
      const expectedSeparator = PlatformUtils.getPlatform() === 'windows' ? '\\' : '/';
      assert.strictEqual(separator, expectedSeparator);
    });
  });

  suite('Platform Defaults', () => {
    test('should provide platform-specific defaults', () => {
      const defaults = PlatformUtils.getPlatformDefaults();

      assert.ok(typeof defaults.pathSeparator === 'string');
      assert.ok(typeof defaults.executableExtension === 'string');
      assert.ok(Array.isArray(defaults.executablePaths));
      assert.ok(defaults.executablePaths.length > 0);

      if (PlatformUtils.getPlatform() === 'windows') {
        assert.strictEqual(defaults.pathSeparator, '\\');
        assert.strictEqual(defaults.executableExtension, '.exe');
        assert.ok(defaults.executablePaths.some(p => p.includes('Program Files')));
      } else {
        assert.strictEqual(defaults.pathSeparator, '/');
        assert.strictEqual(defaults.executableExtension, '');
        assert.ok(defaults.executablePaths.some(p => p.includes('/usr/bin')));
      }
    });

    test('should provide executable paths for current platform', () => {
      const executablePaths = PlatformUtils.getExecutablePaths();

      assert.ok(Array.isArray(executablePaths));
      assert.ok(executablePaths.length > 0);

      // All paths should be absolute
      executablePaths.forEach(execPath => {
        assert.ok(path.isAbsolute(execPath) || execPath.includes('~'));
      });
    });

    test('should provide correct executable extension', () => {
      const extension = PlatformUtils.getExecutableExtension();

      if (PlatformUtils.getPlatform() === 'windows') {
        assert.strictEqual(extension, '.exe');
      } else {
        assert.strictEqual(extension, '');
      }
    });
  });

  suite('Environment Variables', () => {
    test('should provide environment variable lists', () => {
      const envVars = PlatformUtils.getEnvironmentVariables();

      assert.ok(Array.isArray(envVars.crossPlatform));
      assert.ok(Array.isArray(envVars.platformSpecific));

      // Should include common cross-platform variables
      assert.ok(envVars.crossPlatform.includes('PATH'));
      assert.ok(
        envVars.crossPlatform.includes('HOME') || envVars.crossPlatform.includes('USERPROFILE')
      );

      // Should include platform-specific variables
      if (PlatformUtils.getPlatform() === 'windows') {
        assert.ok(envVars.platformSpecific.includes('USERPROFILE'));
        assert.ok(envVars.platformSpecific.includes('TEMP'));
        assert.ok(envVars.platformSpecific.includes('APPDATA'));
      } else {
        assert.ok(envVars.platformSpecific.includes('HOME'));
        assert.ok(
          envVars.platformSpecific.includes('TMPDIR') || envVars.platformSpecific.includes('TMP')
        );
      }
    });

    test('should expand multiple environment variables', () => {
      const input =
        PlatformUtils.getPlatform() === 'windows'
          ? '%USERPROFILE%\\%USERNAME%\\test'
          : '$HOME/$USER/test';

      const expanded = PlatformUtils.expandEnvironmentVariables(input);

      // Should not contain variable markers after expansion
      if (PlatformUtils.getPlatform() === 'windows') {
        assert.ok(!expanded.includes('%'));
      } else {
        assert.ok(!expanded.includes('$'));
      }
    });
  });

  suite('File System Operations', () => {
    test('should check file existence', async () => {
      // Test with current file (should exist)
      const currentFile = __filename;
      const exists = await PlatformUtils.pathExists(currentFile);
      assert.strictEqual(exists, true);

      // Test with non-existent file
      const nonExistent = path.join(__dirname, 'nonexistent-file-12345.txt');
      const notExists = await PlatformUtils.pathExists(nonExistent);
      assert.strictEqual(notExists, false);
    });

    test('should check executable permissions', async () => {
      // Test with current Node.js executable
      const nodeExecutable = process.execPath;
      const isExecutable = await PlatformUtils.isExecutable(nodeExecutable);
      assert.strictEqual(isExecutable, true);

      // Test with current test file (should not be executable)
      const currentFile = __filename;
      const isNotExecutable = await PlatformUtils.isExecutable(currentFile);
      assert.strictEqual(isNotExecutable, false);
    });

    test('should handle non-existent files gracefully', async () => {
      const nonExistent = path.join(__dirname, 'nonexistent-file-12345.txt');

      const exists = await PlatformUtils.pathExists(nonExistent);
      const isExecutable = await PlatformUtils.isExecutable(nonExistent);

      assert.strictEqual(exists, false);
      assert.strictEqual(isExecutable, false);
    });
  });

  suite('Directory Operations', () => {
    test('should get home directory', () => {
      const homeDir = PlatformUtils.getHomeDirectory();
      const osHomeDir = os.homedir();

      assert.strictEqual(homeDir, osHomeDir);
      assert.ok(homeDir.length > 0);
    });

    test('should get temp directory', () => {
      const tempDir = PlatformUtils.getTempDirectory();
      const osTempDir = os.tmpdir();

      assert.strictEqual(tempDir, osTempDir);
      assert.ok(tempDir.length > 0);
    });

    test('should get configuration location', () => {
      const configLocation = PlatformUtils.getConfigurationLocation();

      assert.ok(configLocation.length > 0);
      assert.ok(path.isAbsolute(configLocation) || configLocation.includes('~'));

      if (PlatformUtils.getPlatform() === 'windows') {
        assert.ok(configLocation.includes('AppData') || configLocation.includes('%'));
      } else if (PlatformUtils.getPlatform() === 'macos') {
        assert.ok(configLocation.includes('Library') || configLocation.includes('~'));
      } else {
        assert.ok(configLocation.includes('.config') || configLocation.includes('~'));
      }
    });
  });

  suite('Cross-Platform Compatibility', () => {
    test('should handle mixed path separators', () => {
      const mixedPath = 'some/path\\with/mixed\\separators';
      const normalized = PlatformUtils.normalizePath(mixedPath);

      // Should use consistent separators
      const separator = PlatformUtils.getPathSeparator();
      const wrongSeparator = separator === '/' ? '\\' : '/';

      // UNC paths on Windows are an exception
      if (!(PlatformUtils.getPlatform() === 'windows' && normalized.startsWith('\\\\'))) {
        assert.ok(!normalized.includes(wrongSeparator));
      }
    });

    test('should handle empty and null inputs gracefully', () => {
      // Empty string
      const emptyNormalized = PlatformUtils.normalizePath('');
      assert.strictEqual(emptyNormalized, '');

      const emptyExpanded = PlatformUtils.expandEnvironmentVariables('');
      assert.strictEqual(emptyExpanded, '');

      // Whitespace
      const whitespaceNormalized = PlatformUtils.normalizePath('   ');
      assert.strictEqual(whitespaceNormalized.trim(), '');
    });

    test('should handle special characters in paths', () => {
      const specialPaths = [
        'path with spaces',
        'path-with-dashes',
        'path_with_underscores',
        'path.with.dots',
        'path(with)parentheses',
      ];

      specialPaths.forEach(specialPath => {
        const normalized = PlatformUtils.normalizePath(specialPath);
        assert.ok(normalized.length > 0);
        // Should preserve special characters
        assert.ok(
          normalized.includes(specialPath) || normalized.includes(path.normalize(specialPath))
        );
      });
    });
  });

  suite('Error Handling', () => {
    test('should handle invalid paths gracefully', async () => {
      const invalidPaths = ['', '   ', '\0invalid\0path', 'path\nwith\nnewlines'];

      for (const invalidPath of invalidPaths) {
        // Should not throw errors
        const exists = await PlatformUtils.pathExists(invalidPath);
        const isExecutable = await PlatformUtils.isExecutable(invalidPath);

        assert.strictEqual(typeof exists, 'boolean');
        assert.strictEqual(typeof isExecutable, 'boolean');
      }
    });

    test('should handle permission errors gracefully', async () => {
      // Try to access system files that might have restricted permissions
      const restrictedPaths =
        PlatformUtils.getPlatform() === 'windows'
          ? ['C:\\Windows\\System32\\config\\SAM', 'C:\\pagefile.sys']
          : ['/etc/shadow', '/root/.bashrc'];

      for (const restrictedPath of restrictedPaths) {
        // Should not throw errors, even if access is denied
        const exists = await PlatformUtils.pathExists(restrictedPath);
        const isExecutable = await PlatformUtils.isExecutable(restrictedPath);

        assert.strictEqual(typeof exists, 'boolean');
        assert.strictEqual(typeof isExecutable, 'boolean');
      }
    });

    test('should handle network paths appropriately', async () => {
      const networkPaths =
        PlatformUtils.getPlatform() === 'windows'
          ? ['\\\\nonexistent\\share\\file', '\\\\localhost\\c$\\Windows']
          : ['/mnt/nonexistent', '/net/nonexistent'];

      for (const networkPath of networkPaths) {
        // Should handle network paths without throwing
        const exists = await PlatformUtils.pathExists(networkPath);
        assert.strictEqual(typeof exists, 'boolean');
      }
    });
  });

  suite('Integration', () => {
    test('should work with Node.js path module', () => {
      const testPath = PlatformUtils.joinPath('test', 'path', 'file.txt');
      const nodePath = path.join('test', 'path', 'file.txt');

      // Results should be equivalent
      assert.strictEqual(path.normalize(testPath), path.normalize(nodePath));
    });

    test('should work with Node.js os module', () => {
      const platformInfo = PlatformUtils.getPlatformInfo();

      // Should match Node.js os module results
      assert.strictEqual(platformInfo.homeDirectory, os.homedir());
      assert.strictEqual(platformInfo.tempDirectory, os.tmpdir());
      assert.ok(platformInfo.nodeVersion.startsWith('v'));
    });

    test('should provide consistent results across calls', () => {
      // Multiple calls should return identical results
      const platform1 = PlatformUtils.getPlatform();
      const platform2 = PlatformUtils.getPlatform();
      assert.strictEqual(platform1, platform2);

      const arch1 = PlatformUtils.getArchitecture();
      const arch2 = PlatformUtils.getArchitecture();
      assert.strictEqual(arch1, arch2);

      const info1 = PlatformUtils.getPlatformInfo();
      const info2 = PlatformUtils.getPlatformInfo();
      assert.deepStrictEqual(info1, info2);
    });
  });
});
