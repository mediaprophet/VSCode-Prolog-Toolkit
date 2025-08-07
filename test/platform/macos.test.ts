import * as assert from 'assert';
import * as os from 'os';
import { PackageManagerIntegration } from '../../src/features/packageManagerIntegration.js';
import { ExecutableFinder } from '../../src/utils/executableFinder.js';
import { PlatformUtils } from '../../src/utils/platformUtils.js';

/**
 * macOS-specific platform tests
 */
suite('macOS Platform Tests', () => {
  // Skip tests if not running on macOS
  const isMacOS = os.platform() === 'darwin';

  suiteSetup(function () {
    if (!isMacOS) {
      this.skip();
    }
  });

  suite('Platform Detection', () => {
    test('should detect macOS platform correctly', () => {
      const platform = PlatformUtils.getPlatform();
      assert.strictEqual(platform, 'macos');
    });

    test('should detect architecture correctly', () => {
      const arch = PlatformUtils.getArchitecture();
      assert.ok(['x64', 'arm64'].includes(arch));
    });

    test('should provide macOS-specific defaults', () => {
      const defaults = PlatformUtils.getPlatformDefaults();
      assert.strictEqual(defaults.pathSeparator, '/');
      assert.strictEqual(defaults.executableExtension, '');
      assert.ok(defaults.executablePaths.some(path => path.includes('/usr/local/bin')));
      assert.ok(defaults.executablePaths.some(path => path.includes('/opt/homebrew/bin')));
    });

    test('should handle Apple Silicon vs Intel differences', () => {
      const arch = PlatformUtils.getArchitecture();
      const executablePaths = PlatformUtils.getExecutablePaths();

      if (arch === 'arm64') {
        // Apple Silicon should include Homebrew paths
        assert.ok(executablePaths.some(path => path.includes('/opt/homebrew')));
      } else {
        // Intel Macs should include traditional paths
        assert.ok(executablePaths.some(path => path.includes('/usr/local')));
      }
    });
  });

  suite('Path Handling', () => {
    test('should normalize Unix paths correctly', () => {
      const testPaths = [
        '/usr/local/bin/swipl',
        '/opt/homebrew/bin/swipl',
        '../relative/path/file',
        '~/Documents/test.pl',
        '/Applications/SWI-Prolog.app/Contents/MacOS/swipl',
      ];

      testPaths.forEach(testPath => {
        const normalized = PlatformUtils.normalizePath(testPath);
        assert.ok(normalized.length > 0);
        // Should not contain double slashes
        assert.ok(!normalized.includes('//'));
      });
    });

    test('should expand home directory (~) correctly', () => {
      const homeDir = os.homedir();
      const pathWithTilde = '~/Documents/test.pl';
      const normalized = PlatformUtils.normalizePath(pathWithTilde);

      assert.ok(normalized.startsWith(homeDir));
      assert.ok(!normalized.includes('~'));
    });

    test('should expand Unix environment variables', () => {
      const pathWithEnvVar = '$HOME/Documents/test.pl';
      const expanded = PlatformUtils.expandEnvironmentVariables(pathWithEnvVar);

      if (process.env.HOME) {
        assert.ok(expanded.includes(process.env.HOME));
        assert.ok(!expanded.includes('$HOME'));
      }
    });

    test('should handle macOS application bundle paths', () => {
      const appPath = '/Applications/SWI-Prolog.app/Contents/MacOS/swipl';
      const normalized = PlatformUtils.normalizePath(appPath);

      assert.ok(normalized.includes('/Applications/'));
      assert.ok(normalized.includes('.app/Contents/MacOS/'));
    });

    test('should join paths correctly', () => {
      const joined = PlatformUtils.joinPath('/usr', 'local', 'bin', 'swipl');
      assert.strictEqual(joined, '/usr/local/bin/swipl');
    });
  });

  suite('Executable Detection', () => {
    test('should detect macOS executable paths', () => {
      const executablePaths = PlatformUtils.getExecutablePaths();
      assert.ok(executablePaths.length > 0);
      assert.ok(executablePaths.every(path => !path.endsWith('.exe')));
      assert.ok(
        executablePaths.some(
          path => path.includes('/usr/local/bin') || path.includes('/opt/homebrew/bin')
        )
      );
    });

    test('should find SWI-Prolog executable on macOS', async function () {
      this.timeout(10000); // Increase timeout for executable detection

      const finder = new ExecutableFinder();
      const result = await finder.findSwiplExecutable();

      // Test should pass whether SWI-Prolog is installed or not
      if (result.found) {
        assert.ok(result.path);
        assert.ok(!result.path.endsWith('.exe'));
        assert.ok(result.detectionMethod);

        if (result.permissions) {
          assert.strictEqual(typeof result.permissions.executable, 'boolean');
          assert.strictEqual(typeof result.permissions.readable, 'boolean');
        }
      } else {
        assert.ok(result.issues);
        assert.ok(result.issues.length > 0);
      }
    });

    test('should validate macOS executable paths', async function () {
      this.timeout(5000);

      const finder = new ExecutableFinder();

      // Test with invalid path
      const invalidResult = await finder.validateExecutable('/nonexistent/swipl');
      assert.strictEqual(invalidResult.found, false);
      assert.ok(invalidResult.issues);

      // Test with non-executable file (if exists)
      const textFileResult = await finder.validateExecutable('/etc/hosts');
      if (await PlatformUtils.pathExists('/etc/hosts')) {
        assert.strictEqual(textFileResult.found, false);
        assert.ok(textFileResult.issues);
      }
    });

    test('should handle macOS-specific executable locations', async function () {
      this.timeout(5000);

      const commonLocations = [
        '/usr/local/bin/swipl',
        '/opt/homebrew/bin/swipl',
        '/Applications/SWI-Prolog.app/Contents/MacOS/swipl',
        '/opt/local/bin/swipl', // MacPorts
      ];

      const finder = new ExecutableFinder();

      for (const location of commonLocations) {
        const result = await finder.validateExecutable(location);
        // Should not throw errors, regardless of whether file exists
        assert.strictEqual(typeof result.found, 'boolean');
      }
    });
  });

  suite('Package Manager Integration', () => {
    test('should detect macOS package managers', async function () {
      this.timeout(15000); // Package manager detection can be slow

      const packageManager = PackageManagerIntegration.getInstance();
      const availableManagers = await packageManager.detectAvailableManagers();

      // Should detect available macOS package managers
      availableManagers.forEach(manager => {
        assert.ok(['homebrew', 'macports'].includes(manager.name));
        assert.strictEqual(manager.isAvailable, true);
        assert.ok(manager.installCommand.length > 0);
      });
    });

    test('should provide macOS installation suggestions', async function () {
      this.timeout(5000);

      const packageManager = PackageManagerIntegration.getInstance();
      const suggestions = await packageManager.getInstallationSuggestions();

      assert.ok(suggestions.length > 0);
      assert.ok(suggestions.some(s => s.includes('macOS')));
      assert.ok(suggestions.some(s => s.includes('brew') || s.includes('port')));
    });

    test('should get macOS-specific recommendations', () => {
      const packageManager = PackageManagerIntegration.getInstance();
      const recommendations = packageManager.getRecommendedManagers();

      assert.deepStrictEqual(recommendations, ['homebrew', 'macports']);
    });

    test('should handle Apple Silicon vs Intel package managers', async function () {
      this.timeout(10000);

      const packageManager = PackageManagerIntegration.getInstance();
      const availableManagers = await packageManager.detectAvailableManagers();

      const homebrewManager = availableManagers.find(m => m.name === 'homebrew');
      if (homebrewManager) {
        const arch = PlatformUtils.getArchitecture();
        if (arch === 'arm64') {
          // Apple Silicon should use /opt/homebrew
          assert.ok(homebrewManager.installCommand.includes('brew'));
        } else {
          // Intel should use /usr/local
          assert.ok(homebrewManager.installCommand.includes('brew'));
        }
      }
    });
  });

  suite('Environment Variables', () => {
    test('should handle Unix environment variables', () => {
      const envVars = PlatformUtils.getEnvironmentVariables();

      assert.ok(envVars.crossPlatform.includes('PATH'));
      assert.ok(envVars.platformSpecific.includes('HOME'));
      assert.ok(envVars.platformSpecific.includes('XDG_CONFIG_HOME'));
      assert.ok(envVars.platformSpecific.includes('TMPDIR'));
    });

    test('should expand Unix-style environment variables', () => {
      // Test Unix-style $VAR and ${VAR} expansion
      const testCases = [
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
    test('should check file existence on macOS', async () => {
      // Test with known macOS system files
      const systemFiles = [
        '/bin/bash',
        '/bin/zsh',
        '/usr/bin/which',
        '/System/Library/CoreServices/Finder.app',
      ];

      for (const file of systemFiles) {
        const exists = await PlatformUtils.pathExists(file);
        // These files should exist on any macOS system
        assert.strictEqual(exists, true, `${file} should exist on macOS`);
      }
    });

    test('should check executable permissions on macOS', async () => {
      // Test with macOS executables
      const executables = ['/bin/bash', '/bin/zsh', '/usr/bin/which'];

      for (const exe of executables) {
        if (await PlatformUtils.pathExists(exe)) {
          const isExecutable = await PlatformUtils.isExecutable(exe);
          assert.strictEqual(isExecutable, true, `${exe} should be executable`);
        }
      }
    });

    test('should handle macOS file permissions correctly', async () => {
      // Test with files that should not be executable
      const nonExecutables = ['/etc/hosts', '/etc/passwd'];

      for (const file of nonExecutables) {
        if (await PlatformUtils.pathExists(file)) {
          const isExecutable = await PlatformUtils.isExecutable(file);
          assert.strictEqual(isExecutable, false, `${file} should not be executable`);
        }
      }
    });

    test('should handle macOS application bundles', async () => {
      // Test with common macOS applications
      const apps = ['/System/Library/CoreServices/Finder.app', '/Applications/Safari.app'];

      for (const app of apps) {
        const exists = await PlatformUtils.pathExists(app);
        if (exists) {
          // App bundles are directories, not executable files
          const isExecutable = await PlatformUtils.isExecutable(app);
          // Result may vary, but should not throw
          assert.strictEqual(typeof isExecutable, 'boolean');
        }
      }
    });
  });

  suite('Configuration', () => {
    test('should provide macOS-specific configuration location', () => {
      const configLocation = PlatformUtils.getConfigurationLocation();
      assert.ok(
        configLocation.includes('Library/Application Support') ||
          configLocation.includes('~/Library')
      );
    });

    test('should provide macOS temp directory', () => {
      const tempDir = PlatformUtils.getTempDirectory();
      assert.ok(tempDir.length > 0);
      // Should be a valid Unix path
      assert.ok(tempDir.startsWith('/') || tempDir.includes('tmp'));
    });

    test('should provide macOS home directory', () => {
      const homeDir = PlatformUtils.getHomeDirectory();
      assert.ok(homeDir.length > 0);
      // Should be a valid Unix path
      assert.ok(homeDir.startsWith('/'));
      assert.ok(homeDir.includes('/Users/'));
    });
  });

  suite('Platform Info', () => {
    test('should provide comprehensive macOS platform info', () => {
      const info = PlatformUtils.getPlatformInfo();

      assert.strictEqual(info.platform, 'macos');
      assert.ok(['x64', 'arm64'].includes(info.architecture));
      assert.ok(info.osVersion.length > 0);
      assert.ok(info.nodeVersion.startsWith('v'));
      assert.strictEqual(info.pathSeparator, '/');
      assert.strictEqual(info.executableExtension, '');
      assert.ok(info.homeDirectory.length > 0);
      assert.ok(info.tempDirectory.length > 0);
    });

    test('should detect macOS version correctly', () => {
      const info = PlatformUtils.getPlatformInfo();
      // macOS version should be in format like "21.6.0" (Darwin version)
      assert.ok(info.osVersion.match(/^\d+\.\d+\.\d+$/));
    });
  });

  suite('macOS-Specific Features', () => {
    test('should handle Gatekeeper and security restrictions', async () => {
      // Test that security-related operations don't throw errors
      const restrictedPaths = ['/System/Library/Extensions', '/usr/libexec'];

      for (const restrictedPath of restrictedPaths) {
        // These operations should not throw errors
        const exists = await PlatformUtils.pathExists(restrictedPath);
        assert.strictEqual(typeof exists, 'boolean');
      }
    });

    test('should handle case-sensitive vs case-insensitive filesystems', async () => {
      // macOS can have either case-sensitive or case-insensitive filesystems
      const testPath = '/tmp/CaseSensitiveTest';
      const lowerPath = '/tmp/casesensitivetest';

      // This test just ensures our path operations work regardless of case sensitivity
      const normalizedUpper = PlatformUtils.normalizePath(testPath);
      const normalizedLower = PlatformUtils.normalizePath(lowerPath);

      assert.ok(normalizedUpper.length > 0);
      assert.ok(normalizedLower.length > 0);
    });

    test('should handle macOS-specific directories', async () => {
      // Test macOS-specific directory structure
      const macOSDirs = ['/Applications', '/Library', '/System', '/Users', '/Volumes'];

      for (const dir of macOSDirs) {
        const exists = await PlatformUtils.pathExists(dir);
        assert.strictEqual(exists, true, `${dir} should exist on macOS`);
      }
    });
  });

  suite('Error Handling', () => {
    test('should handle macOS-specific path errors gracefully', async () => {
      // Test with invalid macOS paths
      const invalidPaths = [
        '/nonexistent/path/that/does/not/exist',
        '/System/Library/NonExistent/file',
        '/private/var/nonexistent',
      ];

      for (const invalidPath of invalidPaths) {
        const exists = await PlatformUtils.pathExists(invalidPath);
        assert.strictEqual(exists, false);

        const isExecutable = await PlatformUtils.isExecutable(invalidPath);
        assert.strictEqual(isExecutable, false);
      }
    });

    test('should handle macOS permission errors', async () => {
      // Test with system files that might have restricted access
      const restrictedFiles = [
        '/private/var/db/dslocal/nodes/Default/users/root.plist',
        '/System/Library/Extensions',
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

    test('should handle SIP (System Integrity Protection) restrictions', async () => {
      // Test with SIP-protected paths
      const sipProtectedPaths = ['/System/Library/Frameworks', '/usr/bin/codesign'];

      for (const sipPath of sipProtectedPaths) {
        // Operations should work but may have limited access
        const exists = await PlatformUtils.pathExists(sipPath);
        // Should not throw errors
        assert.strictEqual(typeof exists, 'boolean');
      }
    });
  });
});
