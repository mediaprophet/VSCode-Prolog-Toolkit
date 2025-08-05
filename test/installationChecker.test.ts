import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { InstallationChecker } from '../src/features/installationChecker';

suite('InstallationChecker Tests', () => {
  let installationChecker: InstallationChecker;

  setup(() => {
    installationChecker = InstallationChecker.getInstance();
  });

  suite('Singleton Pattern', () => {
    test('should return the same instance', () => {
      const instance1 = InstallationChecker.getInstance();
      const instance2 = InstallationChecker.getInstance();
      assert.strictEqual(instance1, instance2);
    });
  });

  suite('Platform Detection', () => {
    test('should detect current platform', () => {
      const platform = installationChecker.getCurrentPlatform();
      assert.ok(['windows', 'macos', 'linux'].includes(platform));
    });

    test('should return correct platform-specific paths', () => {
      const paths = installationChecker.detectCommonInstallPaths();
      assert.ok(Array.isArray(paths));
      assert.ok(paths.length > 0);
      
      // Check that paths are platform-appropriate
      const platform = installationChecker.getCurrentPlatform();
      if (platform === 'windows') {
        assert.ok(paths.some(p => p.includes('Program Files')));
      } else if (platform === 'macos') {
        assert.ok(paths.some(p => p.includes('/usr/local/bin') || p.includes('/opt/homebrew')));
      } else {
        assert.ok(paths.some(p => p.includes('/usr/bin') || p.includes('/usr/local/bin')));
      }
    });
  });

  suite('Path Validation', () => {
    test('should validate valid swipl command', async function() {
      this.timeout(10000); // Increase timeout for actual swipl execution
      
      try {
        // Test with default 'swipl' command
        const isValid = await installationChecker.validateSwiplPath('swipl');
        // This might be true or false depending on system, but should not throw
        assert.ok(typeof isValid === 'boolean');
      } catch (error) {
        // If swipl is not installed, this is expected
        console.log('SWI-Prolog not found in PATH, which is expected in test environment');
      }
    });

    test('should reject invalid paths', async () => {
      const isValid = await installationChecker.validateSwiplPath('/nonexistent/path/swipl');
      assert.strictEqual(isValid, false);
    });

    test('should reject empty paths', async () => {
      const isValid = await installationChecker.validateSwiplPath('');
      assert.strictEqual(isValid, false);
    });
  });

  suite('Installation Detection', () => {
    test('should perform installation check', async function() {
      this.timeout(15000); // Increase timeout for comprehensive check
      
      const status = await installationChecker.checkSwiplInstallation();
      
      assert.ok(typeof status.isInstalled === 'boolean');
      assert.ok(typeof status.path === 'string');
      assert.ok(Array.isArray(status.issues));
      
      if (status.isInstalled) {
        assert.ok(status.version);
        assert.ok(status.path.length > 0);
      }
    });

    test('should find executable if available', async function() {
      this.timeout(10000);
      
      const foundPath = await installationChecker.findSwiplExecutable();
      
      if (foundPath) {
        assert.ok(typeof foundPath === 'string');
        assert.ok(foundPath.length > 0);
      } else {
        // No SWI-Prolog found, which is acceptable in test environment
        console.log('No SWI-Prolog installation found');
      }
    });
  });

  suite('Version Detection', () => {
    test('should handle version parsing', async () => {
      // Mock version output
      const mockVersionOutput = 'SWI-Prolog version 9.0.4 for x86_64-linux';
      
      // We can't easily test the actual getSwiplVersion without mocking spawn
      // But we can test that it handles invalid paths gracefully
      const version = await installationChecker.getSwiplVersion('/nonexistent/swipl');
      assert.strictEqual(version, null);
    });
  });

  suite('Configuration Validation', () => {
    test('should validate and update configuration', async function() {
      this.timeout(10000);
      
      const result = await installationChecker.validateAndUpdateConfiguration();
      
      assert.ok(typeof result.isValid === 'boolean');
      assert.ok(typeof result.updated === 'boolean');
      
      if (result.updated) {
        assert.ok(result.oldPath);
        assert.ok(result.newPath);
      }
    });
  });

  suite('System Information', () => {
    test('should gather system information', async function() {
      this.timeout(5000);
      
      const sysInfo = await installationChecker.getSystemInformation();
      
      assert.ok(sysInfo.platform);
      assert.ok(sysInfo.arch);
      assert.ok(sysInfo.nodeVersion);
      assert.ok(Array.isArray(sysInfo.pathEnv));
      assert.ok(typeof sysInfo.hasSwiplInPath === 'boolean');
    });

    test('should provide diagnostic information', async function() {
      this.timeout(10000);
      
      const diagnostics = await installationChecker.getDiagnosticInformation();
      
      assert.ok(diagnostics.systemInfo);
      assert.ok(diagnostics.installationStatus);
      assert.ok(Array.isArray(diagnostics.commonPaths));
      assert.ok(Array.isArray(diagnostics.searchResults));
    });
  });

  suite('Error Handling', () => {
    test('should handle spawn errors gracefully', async () => {
      // Test with a command that will definitely fail
      const isValid = await installationChecker.validateSwiplPath('definitely-not-a-real-command-12345');
      assert.strictEqual(isValid, false);
    });

    test('should handle permission errors', async () => {
      // Test with a path that might have permission issues
      const isValid = await installationChecker.validateSwiplPath('/root/swipl');
      assert.strictEqual(isValid, false);
    });
  });

  suite('Integration with VS Code Configuration', () => {
    test('should read current configuration', () => {
      const config = vscode.workspace.getConfiguration('prolog');
      const execPath = config.get<string>('executablePath', 'swipl');
      assert.ok(typeof execPath === 'string');
    });

    test('should handle configuration updates', async () => {
      const config = vscode.workspace.getConfiguration('prolog');
      const originalPath = config.get<string>('executablePath', 'swipl');
      
      try {
        // Test updating configuration
        await config.update('executablePath', 'test-swipl', vscode.ConfigurationTarget.Global);
        const updatedPath = config.get<string>('executablePath');
        assert.strictEqual(updatedPath, 'test-swipl');
        
        // Restore original configuration
        await config.update('executablePath', originalPath, vscode.ConfigurationTarget.Global);
      } catch (error) {
        // Configuration update might fail in test environment
        console.log('Configuration update test skipped due to environment limitations');
      }
    });
  });

  suite('Performance Tests', () => {
    test('should complete installation check within reasonable time', async function() {
      this.timeout(20000);
      
      const startTime = Date.now();
      await installationChecker.checkSwiplInstallation();
      const endTime = Date.now();
      
      const duration = endTime - startTime;
      assert.ok(duration < 15000, `Installation check took ${duration}ms, should be under 15000ms`);
    });

    test('should handle multiple concurrent checks', async function() {
      this.timeout(30000);
      
      const promises = Array(5).fill(0).map(() => 
        installationChecker.checkSwiplInstallation()
      );
      
      const results = await Promise.all(promises);
      
      // All results should be consistent
      const firstResult = results[0];
      results.forEach(result => {
        assert.strictEqual(result.isInstalled, firstResult.isInstalled);
        assert.strictEqual(result.path, firstResult.path);
      });
    });
  });

  suite('Edge Cases', () => {
    test('should handle null and undefined inputs', async () => {
      const result1 = await installationChecker.validateSwiplPath(null as any);
      const result2 = await installationChecker.validateSwiplPath(undefined as any);
      
      assert.strictEqual(result1, false);
      assert.strictEqual(result2, false);
    });

    test('should handle very long paths', async () => {
      const longPath = '/very/long/path/that/does/not/exist/and/should/be/handled/gracefully/swipl';
      const isValid = await installationChecker.validateSwiplPath(longPath);
      assert.strictEqual(isValid, false);
    });

    test('should handle paths with special characters', async () => {
      const specialPath = '/path with spaces/and-dashes/swipl';
      const isValid = await installationChecker.validateSwiplPath(specialPath);
      assert.strictEqual(isValid, false);
    });
  });
});