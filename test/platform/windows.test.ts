import * as assert from 'assert';
import * as os from 'os';
import { PackageManagerIntegration } from '../../src/features/packageManagerIntegration.js';
import { ExecutableFinder } from '../../src/utils/executableFinder.js';
import { PlatformUtils } from '../../src/utils/platformUtils.js';

/**
 * Windows-specific platform tests
 */
suite('Windows Platform Tests', () => {
  // Skip tests if not running on Windows
  const isWindows = os.platform() === 'win32';

  suiteSetup(function () {
    if (!isWindows) {
      this.skip();
    }
  });

  suite('Platform Detection', () => {
    test('should detect Windows platform correctly', () => {
      const platform = PlatformUtils.getPlatform();
      assert.strictEqual(platform, 'windows');
    });

    test('should detect architecture correctly', () => {
      const arch = PlatformUtils.getArchitecture();
      assert.ok(['x64', 'arm64', 'x86'].includes(arch));
    });

    test('should provide Windows-specific defaults', () => {
      const defaults = PlatformUtils.getPlatformDefaults();
      assert.strictEqual(defaults.pathSeparator, '\\');
      assert.strictEqual(defaults.executableExtension, '.exe');
      assert.ok(defaults.executablePaths.some(path => path.includes('.exe')));
    });
  });

  suite('Path Handling', () => {
    test('should normalize Windows paths correctly', () => {
      const testPaths = [
        'C:\\Program Files\\swipl\\bin\\swipl.exe',
        'C:/Program Files/swipl/bin/swipl.exe',
        '..\\relative\\path\\file.exe',
        '../relative/path/file.exe',
      ];

      testPaths.forEach(testPath => {
        const normalized = PlatformUtils.normalizePath(testPath);
        assert.ok(normalized.length > 0);
        // Should handle both forward and backward slashes
        assert.ok(!normalized.includes('//') && !normalized.includes('\\\\'));
      });
    });

    test('should handle UNC paths correctly', () => {
      const uncPath = '\\\\server\\share\\file.exe';
      const normalized = PlatformUtils.normalizePath(uncPath);
      assert.ok(normalized.startsWith('\\\\'));
    });

    test('should expand Windows environment variables', () => {
      const pathWithEnvVar = '%PROGRAMFILES%\\swipl\\bin\\swipl.exe';
      const expanded = PlatformUtils.expandEnvironmentVariables(pathWithEnvVar);

      if (process.env.PROGRAMFILES) {
        assert.ok(expanded.includes(process.env.PROGRAMFILES));
        assert.ok(!expanded.includes('%PROGRAMFILES%'));
      }
    });

    test('should handle Windows drive letters', () => {
      const drivePaths = ['C:\\test', 'D:\\test', 'E:\\test'];

      drivePaths.forEach(drivePath => {
        const normalized = PlatformUtils.normalizePath(drivePath);
        assert.ok(normalized.match(/^[A-Z]:\\/));
      });
    });

    test('should join paths correctly', () => {
      const joined = PlatformUtils.joinPath('C:\\Program Files', 'swipl', 'bin', 'swipl.exe');
      assert.ok(joined.includes('\\'));
      assert.ok(joined.endsWith('swipl.exe'));
    });
  });

  suite('Executable Detection', () => {
    test('should detect Windows executable paths', () => {
      const executablePaths = PlatformUtils.getExecutablePaths();
      assert.ok(executablePaths.length > 0);
      assert.ok(executablePaths.every(path => path.endsWith('.exe')));
      assert.ok(executablePaths.some(path => path.includes('Program Files')));
    });

    test('should find SWI-Prolog executable on Windows', async function () {
      this.timeout(10000); // Increase timeout for executable detection

      const finder = new ExecutableFinder();
      const result = await finder.findSwiplExecutable();

      // Test should pass whether SWI-Prolog is installed or not
      if (result.found) {
        assert.ok(result.path);
        assert.ok(result.path.endsWith('.exe'));
        assert.ok(result.detectionMethod);

        if (result.permissions) {
          // On Windows, executable permission is usually true if file exists
          assert.strictEqual(typeof result.permissions.executable, 'boolean');
        }
      } else {
        assert.ok(result.issues);
        assert.ok(result.issues.length > 0);
      }
    });

    test('should validate Windows executable paths', async function () {
      this.timeout(5000);

      const finder = new ExecutableFinder();

      // Test with invalid path
      const invalidResult = await finder.validateExecutable('C:\\nonexistent\\swipl.exe');
      assert.strictEqual(invalidResult.found, false);
      assert.ok(invalidResult.issues);

      // Test with non-executable file (if exists)
      const textFileResult = await finder.validateExecutable(
        'C:\\Windows\\System32\\drivers\\etc\\hosts'
      );
      if (await PlatformUtils.pathExists('C:\\Windows\\System32\\drivers\\etc\\hosts')) {
        assert.strictEqual(textFileResult.found, false);
        assert.ok(textFileResult.issues);
      }
    });
  });

  suite('Package Manager Integration', () => {
    test('should detect Windows package managers', async function () {
      this.timeout(15000); // Package manager detection can be slow

      const packageManager = PackageManagerIntegration.getInstance();
      const availableManagers = await packageManager.detectAvailableManagers();

      // Should detect at least one Windows package manager or none
      availableManagers.forEach(manager => {
        assert.ok(['winget', 'chocolatey', 'scoop'].includes(manager.name));
        assert.strictEqual(manager.isAvailable, true);
        assert.ok(manager.installCommand.length > 0);
      });
    });

    test('should provide Windows installation suggestions', async function () {
      this.timeout(5000);

      const packageManager = PackageManagerIntegration.getInstance();
      const suggestions = await packageManager.getInstallationSuggestions();

      assert.ok(suggestions.length > 0);
      assert.ok(suggestions.some(s => s.includes('Windows')));
      assert.ok(
        suggestions.some(s => s.includes('choco') || s.includes('winget') || s.includes('scoop'))
      );
    });

    test('should get Windows-specific recommendations', () => {
      const packageManager = PackageManagerIntegration.getInstance();
      const recommendations = packageManager.getRecommendedManagers();

      assert.deepStrictEqual(recommendations, ['winget', 'chocolatey', 'scoop']);
    });
  });

  suite('Environment Variables', () => {
    test('should handle Windows environment variables', () => {
      const envVars = PlatformUtils.getEnvironmentVariables();

      assert.ok(envVars.crossPlatform.includes('PATH'));
      assert.ok(envVars.platformSpecific.includes('PROGRAMFILES'));
      assert.ok(envVars.platformSpecific.includes('APPDATA'));
      assert.ok(envVars.platformSpecific.includes('USERPROFILE'));
    });

    test('should expand Windows-style environment variables', () => {
      // Test Windows-style %VAR% expansion
      const testCases = [
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

      testCases.forEach(testCase => {
        const result = PlatformUtils.expandEnvironmentVariables(testCase.input);
        if (testCase.expected !== testCase.input) {
          // Environment variable should be expanded
          assert.strictEqual(result, testCase.expected);
        }
      });
    });
  });

  suite('File System Operations', () => {
    test('should check file existence on Windows', async () => {
      // Test with known Windows system files
      const systemFiles = [
        'C:\\Windows\\System32\\kernel32.dll',
        'C:\\Windows\\System32\\cmd.exe',
        'C:\\Windows\\System32\\notepad.exe',
      ];

      for (const file of systemFiles) {
        const exists = await PlatformUtils.pathExists(file);
        // These files should exist on any Windows system
        assert.strictEqual(exists, true, `${file} should exist on Windows`);
      }
    });

    test('should check executable permissions on Windows', async () => {
      // Test with Windows executables
      const executables = ['C:\\Windows\\System32\\cmd.exe', 'C:\\Windows\\System32\\notepad.exe'];

      for (const exe of executables) {
        if (await PlatformUtils.pathExists(exe)) {
          const isExecutable = await PlatformUtils.isExecutable(exe);
          assert.strictEqual(isExecutable, true, `${exe} should be executable`);
        }
      }
    });

    test('should handle Windows file attributes', async () => {
      // Test with system directories
      const systemDirs = ['C:\\Windows', 'C:\\Program Files', 'C:\\Users'];

      for (const dir of systemDirs) {
        const exists = await PlatformUtils.pathExists(dir);
        assert.strictEqual(exists, true, `${dir} should exist`);
      }
    });
  });

  suite('Configuration', () => {
    test('should provide Windows-specific configuration location', () => {
      const configLocation = PlatformUtils.getConfigurationLocation();
      assert.ok(configLocation.includes('AppData') || configLocation.includes('%APPDATA%'));
    });

    test('should provide Windows temp directory', () => {
      const tempDir = PlatformUtils.getTempDirectory();
      assert.ok(tempDir.length > 0);
      // Should be a valid Windows path
      assert.ok(tempDir.match(/^[A-Z]:\\/i) || tempDir.includes('temp') || tempDir.includes('tmp'));
    });

    test('should provide Windows home directory', () => {
      const homeDir = PlatformUtils.getHomeDirectory();
      assert.ok(homeDir.length > 0);
      // Should be a valid Windows path
      assert.ok(homeDir.match(/^[A-Z]:\\/i));
    });
  });

  suite('Platform Info', () => {
    test('should provide comprehensive Windows platform info', () => {
      const info = PlatformUtils.getPlatformInfo();

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

  suite('Error Handling', () => {
    test('should handle Windows-specific path errors gracefully', async () => {
      // Test with invalid Windows paths
      const invalidPaths = [
        'C:\\invalid\\path\\that\\does\\not\\exist.exe',
        '\\\\invalid-server\\share\\file.exe',
        'Z:\\nonexistent\\drive\\file.exe',
      ];

      for (const invalidPath of invalidPaths) {
        const exists = await PlatformUtils.pathExists(invalidPath);
        assert.strictEqual(exists, false);

        const isExecutable = await PlatformUtils.isExecutable(invalidPath);
        assert.strictEqual(isExecutable, false);
      }
    });

    test('should handle Windows permission errors', async () => {
      // Test with system files that might have restricted access
      const restrictedFiles = [
        'C:\\Windows\\System32\\config\\SAM',
        'C:\\Windows\\System32\\config\\SECURITY',
      ];

      for (const file of restrictedFiles) {
        // These operations should not throw errors, just return false
        const exists = await PlatformUtils.pathExists(file);
        const isExecutable = await PlatformUtils.isExecutable(file);

        // Results may vary based on permissions, but should not throw
        assert.strictEqual(typeof exists, 'boolean');
        assert.strictEqual(typeof isExecutable, 'boolean');
      }
    });
  });
});
