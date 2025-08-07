import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import * as os from 'os';
import { PlatformUtils } from '../../src/utils/platformUtils';
import { ExecutableFinder } from '../../src/utils/executableFinder';
import { PackageManagerIntegration } from '../../src/features/packageManagerIntegration';

/**
 * Linux-specific platform tests
 */
suite('Linux Platform Tests', () => {
  // Skip tests if not running on Linux
  const isLinux = os.platform() === 'linux';

  suiteSetup(function () {
    if (!isLinux) {
      this.skip();
    }
  });

  suite('Platform Detection', () => {
    test('should detect Linux platform correctly', () => {
      const platform = PlatformUtils.getPlatform();
      assert.strictEqual(platform, 'linux');
    });

    test('should detect architecture correctly', () => {
      const arch = PlatformUtils.getArchitecture();
      assert.ok(['x64', 'arm64', 'x86'].includes(arch));
    });

    test('should provide Linux-specific defaults', () => {
      const defaults = PlatformUtils.getPlatformDefaults();
      assert.strictEqual(defaults.pathSeparator, '/');
      assert.strictEqual(defaults.executableExtension, '');
      assert.ok(defaults.executablePaths.some(path => path.includes('/usr/bin')));
      assert.ok(defaults.executablePaths.some(path => path.includes('/usr/local/bin')));
    });
  });

  suite('Path Handling', () => {
    test('should normalize Unix paths correctly', () => {
      const testPaths = [
        '/usr/bin/swipl',
        '/usr/local/bin/swipl',
        '/opt/swipl/bin/swipl',
        '../relative/path/file',
        '~/.local/bin/swipl',
        '/snap/bin/swi-prolog',
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
      const pathWithTilde = '~/.local/bin/swipl';
      const normalized = PlatformUtils.normalizePath(pathWithTilde);

      assert.ok(normalized.startsWith(homeDir));
      assert.ok(!normalized.includes('~'));
    });

    test('should expand Unix environment variables', () => {
      const pathWithEnvVar = '$HOME/.local/bin/swipl';
      const expanded = PlatformUtils.expandEnvironmentVariables(pathWithEnvVar);

      if (process.env.HOME) {
        assert.ok(expanded.includes(process.env.HOME));
        assert.ok(!expanded.includes('$HOME'));
      }
    });

    test('should handle Linux-specific paths', () => {
      const linuxPaths = [
        '/usr/lib/swi-prolog/bin/x86_64-linux/swipl',
        '/snap/bin/swi-prolog',
        '/var/lib/flatpak/exports/bin/org.swi_prolog.SWI-Prolog',
      ];

      linuxPaths.forEach(linuxPath => {
        const normalized = PlatformUtils.normalizePath(linuxPath);
        assert.ok(normalized.startsWith('/'));
        assert.ok(!normalized.includes('//'));
      });
    });

    test('should join paths correctly', () => {
      const joined = PlatformUtils.joinPath('/usr', 'local', 'bin', 'swipl');
      assert.strictEqual(joined, '/usr/local/bin/swipl');
    });
  });

  suite('Executable Detection', () => {
    test('should detect Linux executable paths', () => {
      const executablePaths = PlatformUtils.getExecutablePaths();
      assert.ok(executablePaths.length > 0);
      assert.ok(executablePaths.every(path => !path.endsWith('.exe')));
      assert.ok(executablePaths.some(path => path.includes('/usr/bin')));
      assert.ok(executablePaths.some(path => path.includes('/usr/local/bin')));
    });

    test('should find SWI-Prolog executable on Linux', async function () {
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

    test('should validate Linux executable paths', async function () {
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

    test('should handle Linux-specific executable locations', async function () {
      this.timeout(5000);

      const commonLocations = [
        '/usr/bin/swipl',
        '/usr/local/bin/swipl',
        '/opt/swipl/bin/swipl',
        '/snap/bin/swi-prolog',
        '/usr/lib/swi-prolog/bin/x86_64-linux/swipl',
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
    test('should detect Linux package managers', async function () {
      this.timeout(15000); // Package manager detection can be slow

      const packageManager = PackageManagerIntegration.getInstance();
      const availableManagers = await packageManager.detectAvailableManagers();

      // Should detect available Linux package managers
      availableManagers.forEach(manager => {
        assert.ok(
          ['apt', 'dnf', 'yum', 'pacman', 'zypper', 'snap', 'flatpak'].includes(manager.name)
        );
        assert.strictEqual(manager.isAvailable, true);
        assert.ok(manager.installCommand.length > 0);
      });
    });

    test('should provide Linux installation suggestions', async function () {
      this.timeout(5000);

      const packageManager = PackageManagerIntegration.getInstance();
      const suggestions = await packageManager.getInstallationSuggestions();

      assert.ok(suggestions.length > 0);
      assert.ok(suggestions.some(s => s.includes('Linux')));
      assert.ok(
        suggestions.some(
          s => s.includes('apt') || s.includes('yum') || s.includes('dnf') || s.includes('pacman')
        )
      );
    });

    test('should get Linux-specific recommendations', () => {
      const packageManager = PackageManagerIntegration.getInstance();
      const recommendations = packageManager.getRecommendedManagers();

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

    test('should handle distribution-specific package managers', async function () {
      this.timeout(10000);

      const packageManager = PackageManagerIntegration.getInstance();
      const availableManagers = await packageManager.detectAvailableManagers();

      // Different distributions should have different package managers
      const managerNames = availableManagers.map(m => m.name);

      // At least one package manager should be available on any Linux system
      if (managerNames.length > 0) {
        // Common package managers
        const commonManagers = ['apt', 'dnf', 'yum', 'pacman', 'zypper'];
        const hasCommonManager = managerNames.some(name => commonManagers.includes(name));

        // Should have at least one traditional package manager or snap/flatpak
        assert.ok(
          hasCommonManager || managerNames.includes('snap') || managerNames.includes('flatpak')
        );
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

      testCases.forEach(testCase => {
        const result = PlatformUtils.expandEnvironmentVariables(testCase.input);
        if (testCase.expected !== testCase.input) {
          // Environment variable should be expanded
          assert.strictEqual(result, testCase.expected);
        }
      });
    });

    test('should handle XDG Base Directory specification', () => {
      const envVars = PlatformUtils.getEnvironmentVariables();

      // XDG variables should be included
      assert.ok(envVars.platformSpecific.includes('XDG_CONFIG_HOME'));
      assert.ok(envVars.platformSpecific.includes('XDG_DATA_HOME'));
    });
  });

  suite('File System Operations', () => {
    test('should check file existence on Linux', async () => {
      // Test with known Linux system files
      const systemFiles = ['/bin/bash', '/usr/bin/which', '/etc/passwd', '/proc/version'];

      for (const file of systemFiles) {
        const exists = await PlatformUtils.pathExists(file);
        // These files should exist on any Linux system
        assert.strictEqual(exists, true, `${file} should exist on Linux`);
      }
    });

    test('should check executable permissions on Linux', async () => {
      // Test with Linux executables
      const executables = ['/bin/bash', '/usr/bin/which', '/bin/ls'];

      for (const exe of executables) {
        if (await PlatformUtils.pathExists(exe)) {
          const isExecutable = await PlatformUtils.isExecutable(exe);
          assert.strictEqual(isExecutable, true, `${exe} should be executable`);
        }
      }
    });

    test('should handle Linux file permissions correctly', async () => {
      // Test with files that should not be executable
      const nonExecutables = ['/etc/hosts', '/etc/passwd', '/proc/version'];

      for (const file of nonExecutables) {
        if (await PlatformUtils.pathExists(file)) {
          const isExecutable = await PlatformUtils.isExecutable(file);
          assert.strictEqual(isExecutable, false, `${file} should not be executable`);
        }
      }
    });

    test('should handle symbolic links', async () => {
      // Test with common symbolic links on Linux
      const symlinks = [
        '/bin/sh', // Often a symlink to bash or dash
        '/usr/bin/python3', // Often a symlink
      ];

      for (const symlink of symlinks) {
        if (await PlatformUtils.pathExists(symlink)) {
          // Should handle symlinks transparently
          const isExecutable = await PlatformUtils.isExecutable(symlink);
          assert.strictEqual(typeof isExecutable, 'boolean');
        }
      }
    });
  });

  suite('Configuration', () => {
    test('should provide Linux-specific configuration location', () => {
      const configLocation = PlatformUtils.getConfigurationLocation();
      assert.ok(configLocation.includes('.config') || configLocation.includes('~/.config'));
    });

    test('should provide Linux temp directory', () => {
      const tempDir = PlatformUtils.getTempDirectory();
      assert.ok(tempDir.length > 0);
      // Should be a valid Unix path
      assert.ok(tempDir.startsWith('/') || tempDir.includes('tmp'));
    });

    test('should provide Linux home directory', () => {
      const homeDir = PlatformUtils.getHomeDirectory();
      assert.ok(homeDir.length > 0);
      // Should be a valid Unix path
      assert.ok(homeDir.startsWith('/'));
      assert.ok(homeDir.includes('/home/') || homeDir.includes('/root'));
    });
  });

  suite('Platform Info', () => {
    test('should provide comprehensive Linux platform info', () => {
      const info = PlatformUtils.getPlatformInfo();

      assert.strictEqual(info.platform, 'linux');
      assert.ok(['x64', 'arm64', 'x86'].includes(info.architecture));
      assert.ok(info.osVersion.length > 0);
      assert.ok(info.nodeVersion.startsWith('v'));
      assert.strictEqual(info.pathSeparator, '/');
      assert.strictEqual(info.executableExtension, '');
      assert.ok(info.homeDirectory.length > 0);
      assert.ok(info.tempDirectory.length > 0);
    });

    test('should detect Linux kernel version', () => {
      const info = PlatformUtils.getPlatformInfo();
      // Linux version should be kernel version like "5.4.0-74-generic"
      assert.ok(info.osVersion.match(/^\d+\.\d+/));
    });
  });

  suite('Linux-Specific Features', () => {
    test('should handle different Linux distributions', async () => {
      // Test that distribution-specific paths don't cause errors
      const distroSpecificPaths = [
        '/etc/debian_version', // Debian/Ubuntu
        '/etc/redhat-release', // Red Hat/CentOS/Fedora
        '/etc/arch-release', // Arch Linux
        '/etc/suse-release', // openSUSE
        '/etc/os-release', // Standard across distributions
      ];

      for (const distroPath of distroSpecificPaths) {
        // These operations should not throw errors
        const exists = await PlatformUtils.pathExists(distroPath);
        assert.strictEqual(typeof exists, 'boolean');
      }
    });

    test('should handle snap and flatpak paths', async () => {
      const snapFlatpakPaths = [
        '/snap',
        '/var/lib/snapd',
        '/var/lib/flatpak',
        '/home/' + process.env.USER + '/.local/share/flatpak',
      ];

      for (const snapPath of snapFlatpakPaths) {
        // These paths may or may not exist depending on the system
        const exists = await PlatformUtils.pathExists(snapPath);
        assert.strictEqual(typeof exists, 'boolean');
      }
    });

    test('should handle container environments', async () => {
      // Test that container-specific paths don't cause issues
      const containerPaths = [
        '/.dockerenv', // Docker
        '/run/.containerenv', // Podman
        '/proc/1/cgroup', // Container detection
      ];

      for (const containerPath of containerPaths) {
        const exists = await PlatformUtils.pathExists(containerPath);
        assert.strictEqual(typeof exists, 'boolean');
      }
    });

    test('should handle different filesystem types', async () => {
      // Test with different mount points that might exist
      const mountPoints = ['/proc', '/sys', '/dev', '/tmp'];

      for (const mountPoint of mountPoints) {
        const exists = await PlatformUtils.pathExists(mountPoint);
        // These should exist on any Linux system
        assert.strictEqual(exists, true, `${mountPoint} should exist on Linux`);
      }
    });
  });

  suite('Error Handling', () => {
    test('should handle Linux-specific path errors gracefully', async () => {
      // Test with invalid Linux paths
      const invalidPaths = [
        '/nonexistent/path/that/does/not/exist',
        '/root/restricted/file', // May not be accessible
        '/sys/nonexistent/file',
      ];

      for (const invalidPath of invalidPaths) {
        const exists = await PlatformUtils.pathExists(invalidPath);
        const isExecutable = await PlatformUtils.isExecutable(invalidPath);

        // Should not throw errors
        assert.strictEqual(typeof exists, 'boolean');
        assert.strictEqual(typeof isExecutable, 'boolean');
      }
    });

    test('should handle Linux permission errors', async () => {
      // Test with system files that might have restricted access
      const restrictedFiles = ['/etc/shadow', '/root/.bashrc', '/proc/kcore'];

      for (const file of restrictedFiles) {
        // These operations should not throw errors, just return false
        const exists = await PlatformUtils.pathExists(file);
        const isExecutable = await PlatformUtils.isExecutable(file);

        // Results may vary based on permissions, but should not throw
        assert.strictEqual(typeof exists, 'boolean');
        assert.strictEqual(typeof isExecutable, 'boolean');
      }
    });

    test('should handle SELinux and AppArmor restrictions', async () => {
      // Test with security-enhanced paths
      const securityPaths = ['/sys/fs/selinux', '/sys/kernel/security/apparmor'];

      for (const secPath of securityPaths) {
        // Operations should work but may have limited access
        const exists = await PlatformUtils.pathExists(secPath);
        // Should not throw errors
        assert.strictEqual(typeof exists, 'boolean');
      }
    });

    test('should handle network filesystems', async () => {
      // Test with potential network mount points
      const networkPaths = ['/mnt', '/media'];

      for (const netPath of networkPaths) {
        const exists = await PlatformUtils.pathExists(netPath);
        // Should handle network filesystems gracefully
        assert.strictEqual(typeof exists, 'boolean');
      }
    });
  });
});
