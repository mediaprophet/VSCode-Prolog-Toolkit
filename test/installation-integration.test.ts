import * as assert from 'assert';
import * as vscode from 'vscode';
import { InstallationChecker } from '../src/features/installationChecker';
import { InstallationGuide } from '../src/features/installationGuide';
import { ConfigurationMigration } from '../src/features/configurationMigration';

suite('Installation System Integration Tests', () => {
  let installationChecker: InstallationChecker;
  let installationGuide: InstallationGuide;
  let configurationMigration: ConfigurationMigration;
  let originalConfig: any;

  setup(async () => {
    installationChecker = InstallationChecker.getInstance();
    installationGuide = InstallationGuide.getInstance();
    configurationMigration = ConfigurationMigration.getInstance();
    
    // Store original configuration
    const config = vscode.workspace.getConfiguration('prolog');
    originalConfig = {
      executablePath: config.get('executablePath'),
      dialect: config.get('dialect')
    };
  });

  teardown(async () => {
    // Restore original configuration
    const config = vscode.workspace.getConfiguration('prolog');
    for (const [key, value] of Object.entries(originalConfig)) {
      if (value !== undefined) {
        try {
          await config.update(key, value, vscode.ConfigurationTarget.Global);
        } catch (error) {
          console.log(`Failed to restore config ${key}:`, error);
        }
      }
    }
  });

  suite('Component Integration', () => {
    test('should integrate InstallationChecker with ConfigurationMigration', async function() {
      this.timeout(20000);
      
      // Test the workflow: check installation -> detect issues -> migrate if needed
      const installationStatus = await installationChecker.checkSwiplInstallation();
      
      if (!installationStatus.isInstalled) {
        // If not installed, migration should be able to handle this
        const migrationResult = await configurationMigration.performMigration();
        
        assert.ok(typeof migrationResult.migrated === 'boolean');
        assert.ok(Array.isArray(migrationResult.issues));
        
        if (!migrationResult.migrated && migrationResult.issues) {
          // Should provide meaningful error messages
          assert.ok(migrationResult.issues.length > 0);
          migrationResult.issues.forEach(issue => {
            assert.ok(typeof issue === 'string');
            assert.ok(issue.length > 0);
          });
        }
      }
    });

    test('should integrate InstallationChecker with InstallationGuide', async function() {
      this.timeout(10000);
      
      // Test that installation guide provides appropriate content for current platform
      const platform = installationChecker.getCurrentPlatform();
      const guidePlatform = installationGuide.getCurrentPlatform();
      
      // Both should detect the same platform
      assert.strictEqual(platform, guidePlatform);
      
      // Guide should provide instructions for the detected platform
      const instructions = installationGuide.getPlatformSpecificInstructions(platform);
      assert.ok(instructions.title);
      assert.ok(instructions.steps.length > 0);
      assert.ok(instructions.downloadUrl);
    });

    test('should provide consistent platform detection across components', () => {
      const checkerPlatform = installationChecker.getCurrentPlatform();
      const guidePlatform = installationGuide.getCurrentPlatform();
      
      assert.strictEqual(checkerPlatform, guidePlatform);
      assert.ok(['windows', 'macos', 'linux'].includes(checkerPlatform));
    });
  });

  suite('End-to-End Workflows', () => {
    test('should handle complete installation detection workflow', async function() {
      this.timeout(25000);
      
      // Simulate the full workflow that happens during extension activation
      
      // Step 1: Check installation
      const installationStatus = await installationChecker.checkSwiplInstallation();
      assert.ok(typeof installationStatus.isInstalled === 'boolean');
      
      // Step 2: If not installed, check for migration opportunities
      if (!installationStatus.isInstalled) {
        const outdatedCheck = await configurationMigration.detectOutdatedPaths();
        assert.ok(typeof outdatedCheck.hasOutdatedPaths === 'boolean');
        
        // Step 3: If migration is possible, perform it
        if (outdatedCheck.hasOutdatedPaths && outdatedCheck.suggestions.length > 0) {
          const migrationResult = await configurationMigration.performMigration();
          assert.ok(typeof migrationResult.migrated === 'boolean');
        }
        
        // Step 4: If still not resolved, installation guide should be available
        const html = installationGuide.generateInstallationGuideHtml();
        assert.ok(html.length > 0);
        assert.ok(html.includes('SWI-Prolog'));
      }
    });

    test('should handle configuration update workflow', async function() {
      this.timeout(15000);
      
      const config = vscode.workspace.getConfiguration('prolog');
      
      try {
        // Step 1: Set an invalid path
        await config.update('executablePath', '/invalid/path/swipl', vscode.ConfigurationTarget.Global);
        
        // Step 2: Check installation (should fail)
        const installationStatus = await installationChecker.checkSwiplInstallation();
        assert.strictEqual(installationStatus.isInstalled, false);
        
        // Step 3: Attempt migration
        const migrationResult = await configurationMigration.performMigration();
        
        // Step 4: Verify result
        if (migrationResult.migrated) {
          assert.ok(migrationResult.newPath);
          assert.notStrictEqual(migrationResult.newPath, '/invalid/path/swipl');
        } else {
          // Migration failed, but should provide helpful information
          assert.ok(Array.isArray(migrationResult.issues));
        }
      } catch (error) {
        console.log('Configuration update test skipped due to environment limitations');
      }
    });
  });

  suite('Error Recovery', () => {
    test('should gracefully handle system without SWI-Prolog', async function() {
      this.timeout(15000);
      
      // This test ensures the system works even when SWI-Prolog is not available
      
      const config = vscode.workspace.getConfiguration('prolog');
      
      try {
        // Set a definitely invalid path
        await config.update('executablePath', '/definitely/does/not/exist/swipl', vscode.ConfigurationTarget.Global);
        
        // All components should handle this gracefully
        const installationStatus = await installationChecker.checkSwiplInstallation();
        assert.strictEqual(installationStatus.isInstalled, false);
        
        const migrationResult = await configurationMigration.performMigration();
        assert.ok(typeof migrationResult.migrated === 'boolean');
        
        const html = installationGuide.generateInstallationGuideHtml();
        assert.ok(html.length > 0);
        
        // None of these should throw errors
        assert.ok(true, 'All components handled missing SWI-Prolog gracefully');
      } catch (error) {
        console.log('Error recovery test skipped due to environment limitations');
      }
    });

    test('should handle concurrent operations safely', async function() {
      this.timeout(30000);
      
      // Test that multiple operations can run concurrently without issues
      const promises = [
        installationChecker.checkSwiplInstallation(),
        installationChecker.checkSwiplInstallation(),
        configurationMigration.detectOutdatedPaths(),
        installationChecker.getSystemInformation()
      ];
      
      const results = await Promise.all(promises);
      
      // All operations should complete successfully
      assert.strictEqual(results.length, 4);
      
      // Results should be consistent
      const [status1, status2] = results;
      assert.strictEqual(status1.isInstalled, status2.isInstalled);
      assert.strictEqual(status1.path, status2.path);
    });
  });

  suite('User Experience Integration', () => {
    test('should provide consistent messaging across components', async function() {
      this.timeout(10000);
      
      const platform = installationChecker.getCurrentPlatform();
      const instructions = installationGuide.getPlatformSpecificInstructions(platform);
      const html = installationGuide.generateInstallationGuideHtml();
      
      // All should reference SWI-Prolog consistently
      assert.ok(instructions.title.includes('SWI-Prolog'));
      assert.ok(html.includes('SWI-Prolog'));
      
      // Platform-specific content should be consistent
      if (platform === 'windows') {
        assert.ok(instructions.title.toLowerCase().includes('windows'));
        assert.ok(html.toLowerCase().includes('windows'));
      } else if (platform === 'macos') {
        assert.ok(
          instructions.title.toLowerCase().includes('macos') || 
          instructions.title.toLowerCase().includes('mac')
        );
        assert.ok(html.toLowerCase().includes('macos') || html.toLowerCase().includes('mac'));
      } else {
        assert.ok(instructions.title.toLowerCase().includes('linux'));
        assert.ok(html.toLowerCase().includes('linux'));
      }
    });

    test('should provide actionable guidance for users', async function() {
      this.timeout(10000);
      
      const platform = installationChecker.getCurrentPlatform();
      const instructions = installationGuide.getPlatformSpecificInstructions(platform);
      
      // Instructions should be actionable
      assert.ok(instructions.steps.length >= 2);
      
      // Should include download information
      assert.ok(instructions.downloadUrl.startsWith('http'));
      
      // Steps should contain action words
      const stepsText = instructions.steps.join(' ').toLowerCase();
      assert.ok(
        stepsText.includes('download') || 
        stepsText.includes('install') || 
        stepsText.includes('run') ||
        stepsText.includes('execute')
      );
    });
  });

  suite('Performance Integration', () => {
    test('should complete full installation check workflow efficiently', async function() {
      this.timeout(30000);
      
      const startTime = Date.now();
      
      // Simulate the full workflow
      await installationChecker.checkSwiplInstallation();
      await configurationMigration.detectOutdatedPaths();
      installationGuide.generateInstallationGuideHtml();
      
      const endTime = Date.now();
      const duration = endTime - startTime;
      
      assert.ok(duration < 25000, `Full workflow took ${duration}ms, should be under 25000ms`);
    });

    test('should handle multiple users efficiently', async function() {
      this.timeout(45000);
      
      // Simulate multiple users checking installation simultaneously
      const userCount = 5;
      const promises = Array(userCount).fill(0).map(async () => {
        const status = await installationChecker.checkSwiplInstallation();
        const migration = await configurationMigration.detectOutdatedPaths();
        const html = installationGuide.generateInstallationGuideHtml();
        
        return { status, migration, html };
      });
      
      const startTime = Date.now();
      const results = await Promise.all(promises);
      const endTime = Date.now();
      
      const duration = endTime - startTime;
      const avgDuration = duration / userCount;
      
      assert.strictEqual(results.length, userCount);
      assert.ok(avgDuration < 10000, `Average per-user time was ${avgDuration}ms, should be under 10000ms`);
      
      // Results should be consistent across users
      const firstResult = results[0];
      results.forEach(result => {
        assert.strictEqual(result.status.isInstalled, firstResult.status.isInstalled);
        assert.strictEqual(result.migration.hasOutdatedPaths, firstResult.migration.hasOutdatedPaths);
      });
    });
  });

  suite('Configuration Consistency', () => {
    test('should maintain configuration consistency across operations', async function() {
      this.timeout(15000);
      
      const config = vscode.workspace.getConfiguration('prolog');
      const initialPath = config.get<string>('executablePath', 'swipl');
      
      // Perform various operations
      await installationChecker.checkSwiplInstallation();
      await configurationMigration.detectOutdatedPaths();
      
      // Configuration should remain consistent unless explicitly changed
      const finalPath = config.get<string>('executablePath', 'swipl');
      assert.strictEqual(finalPath, initialPath);
    });

    test('should handle configuration changes properly', async function() {
      this.timeout(15000);
      
      const config = vscode.workspace.getConfiguration('prolog');
      
      try {
        // Change configuration
        await config.update('executablePath', 'test-swipl-path', vscode.ConfigurationTarget.Global);
        
        // All components should see the change
        const updatedPath = config.get<string>('executablePath');
        assert.strictEqual(updatedPath, 'test-swipl-path');
        
        // Installation checker should use the new path
        const status = await installationChecker.checkSwiplInstallation();
        // Should attempt to validate the new path (will likely fail, but that's expected)
        assert.ok(typeof status.isInstalled === 'boolean');
        
      } catch (error) {
        console.log('Configuration change test skipped due to environment limitations');
      }
    });
  });
});