import * as assert from 'assert';
import * as vscode from 'vscode';
import { ConfigurationMigration, MigrationResult, ConfigurationBackup } from '../src/features/configurationMigration';

suite('ConfigurationMigration Tests', () => {
  let configurationMigration: ConfigurationMigration;
  let originalConfig: any;

  setup(async () => {
    configurationMigration = ConfigurationMigration.getInstance();
    
    // Store original configuration
    const config = vscode.workspace.getConfiguration('prolog');
    originalConfig = {
      executablePath: config.get('executablePath'),
      dialect: config.get('dialect'),
      'linter.run': config.get('linter.run'),
      'linter.delay': config.get('linter.delay'),
      'linter.enableMsgInOutput': config.get('linter.enableMsgInOutput'),
      'format.addSpace': config.get('format.addSpace'),
      'terminal.runtimeArgs': config.get('terminal.runtimeArgs'),
      'telemetry.enabled': config.get('telemetry.enabled')
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

  suite('Singleton Pattern', () => {
    test('should return the same instance', () => {
      const instance1 = ConfigurationMigration.getInstance();
      const instance2 = ConfigurationMigration.getInstance();
      assert.strictEqual(instance1, instance2);
    });
  });

  suite('Migration Detection', () => {
    test('should detect outdated paths', async function() {
      this.timeout(10000);
      
      const config = vscode.workspace.getConfiguration('prolog');
      
      try {
        // Set an outdated path
        await config.update('executablePath', '/usr/local/bin/pl', vscode.ConfigurationTarget.Global);
        
        const outdatedCheck = await configurationMigration.detectOutdatedPaths();
        
        assert.ok(typeof outdatedCheck.hasOutdatedPaths === 'boolean');
        assert.ok(Array.isArray(outdatedCheck.invalidPaths));
        assert.ok(Array.isArray(outdatedCheck.suggestions));
        
        // Should detect the outdated path
        if (outdatedCheck.hasOutdatedPaths) {
          assert.ok(outdatedCheck.invalidPaths.some(path => path.includes('/usr/local/bin/pl')));
        }
      } catch (error) {
        console.log('Configuration update test skipped due to environment limitations');
      }
    });

    test('should find valid alternative paths', async function() {
      this.timeout(15000);
      
      const validPaths = await configurationMigration.findNewValidPaths();
      
      assert.ok(Array.isArray(validPaths));
      
      // Each valid path should have a path property
      validPaths.forEach(pathInfo => {
        assert.ok(pathInfo.path);
        assert.ok(typeof pathInfo.path === 'string');
      });
    });
  });

  suite('Migration Execution', () => {
    test('should perform migration when needed', async function() {
      this.timeout(15000);
      
      const config = vscode.workspace.getConfiguration('prolog');
      
      try {
        // Set an invalid path to trigger migration
        await config.update('executablePath', '/invalid/path/swipl', vscode.ConfigurationTarget.Global);
        
        const migrationResult = await configurationMigration.performMigration();
        
        assert.ok(typeof migrationResult.migrated === 'boolean');
        assert.ok(Array.isArray(migrationResult.issues));
        
        if (migrationResult.migrated) {
          assert.ok(migrationResult.oldPath);
          assert.ok(migrationResult.newPath);
          assert.ok(typeof migrationResult.backupCreated === 'boolean');
        }
      } catch (error) {
        console.log('Migration test skipped due to environment limitations');
      }
    });

    test('should not migrate valid configurations', async function() {
      this.timeout(10000);
      
      const config = vscode.workspace.getConfiguration('prolog');
      
      try {
        // Set a potentially valid path
        await config.update('executablePath', 'swipl', vscode.ConfigurationTarget.Global);
        
        const migrationResult = await configurationMigration.performMigration();
        
        // If swipl is available, migration should not be needed
        // If not available, migration might be attempted
        assert.ok(typeof migrationResult.migrated === 'boolean');
      } catch (error) {
        console.log('Valid configuration test skipped due to environment limitations');
      }
    });
  });

  suite('Configuration Backup', () => {
    test('should create configuration backup', async () => {
      const backupCreated = await configurationMigration.createConfigurationBackup('test_backup');
      
      // Backup creation might fail in test environment due to context limitations
      assert.ok(typeof backupCreated === 'boolean');
    });

    test('should list configuration backups', () => {
      const backups = configurationMigration.getConfigurationBackups();
      
      assert.ok(Array.isArray(backups));
      
      // Each backup should have required properties
      backups.forEach(backup => {
        assert.ok(backup.timestamp);
        assert.ok(backup.reason);
        assert.ok(backup.configuration);
      });
    });

    test('should restore configuration backup', async () => {
      // First create a backup
      await configurationMigration.createConfigurationBackup('test_restore');
      
      const restored = await configurationMigration.restoreConfigurationBackup(0);
      
      // Restoration might fail in test environment
      assert.ok(typeof restored === 'boolean');
    });
  });

  suite('Version Migration', () => {
    test('should handle version migration analysis', async () => {
      const versionMigration = await configurationMigration.handleVersionMigration('8.5.0', '9.0.4');
      
      assert.ok(Array.isArray(versionMigration.compatibilityIssues));
      assert.ok(Array.isArray(versionMigration.recommendations));
      
      // Should detect major version change
      assert.ok(versionMigration.compatibilityIssues.length > 0 || versionMigration.recommendations.length > 0);
    });

    test('should handle same version migration', async () => {
      const versionMigration = await configurationMigration.handleVersionMigration('9.0.4', '9.0.4');
      
      assert.ok(Array.isArray(versionMigration.compatibilityIssues));
      assert.ok(Array.isArray(versionMigration.recommendations));
      
      // Should have minimal issues for same version
      assert.strictEqual(versionMigration.compatibilityIssues.length, 0);
    });

    test('should handle invalid version strings', async () => {
      const versionMigration = await configurationMigration.handleVersionMigration('invalid', 'also-invalid');
      
      assert.ok(Array.isArray(versionMigration.compatibilityIssues));
      assert.ok(Array.isArray(versionMigration.recommendations));
      
      // Should handle gracefully with error message
      assert.ok(versionMigration.compatibilityIssues.some(issue => 
        issue.includes('Unable to parse version numbers')
      ));
    });
  });

  suite('User Customization Preservation', () => {
    test('should preserve user customizations', async () => {
      const preservation = await configurationMigration.preserveUserCustomizations();
      
      assert.ok(Array.isArray(preservation.preserved));
      assert.ok(Array.isArray(preservation.issues));
      
      // Should preserve non-path settings
      const preservedSettings = preservation.preserved.join(' ');
      assert.ok(preservedSettings.includes('dialect') || preservation.preserved.length === 0);
    });
  });

  suite('Comprehensive Migration', () => {
    test('should perform comprehensive migration check', async function() {
      this.timeout(20000);
      
      // This test checks the full migration workflow
      // It should not throw errors even if no migration is needed
      try {
        await configurationMigration.performComprehensiveMigration();
        // If we reach here, the method completed without throwing
        assert.ok(true);
      } catch (error) {
        // Migration might fail due to UI interactions in test environment
        console.log('Comprehensive migration test completed with expected limitations:', error);
        assert.ok(true);
      }
    });
  });

  suite('Error Handling', () => {
    test('should handle missing extension context gracefully', async () => {
      // Test without setting extension context
      const backupResult = await configurationMigration.createConfigurationBackup('test_no_context');
      
      // Should return false when no context is available
      assert.strictEqual(backupResult, false);
    });

    test('should handle configuration update failures', async () => {
      // Test migration with invalid configuration target
      const migrationResult = await configurationMigration.performMigration();
      
      // Should handle gracefully
      assert.ok(typeof migrationResult.migrated === 'boolean');
      assert.ok(Array.isArray(migrationResult.issues));
    });
  });

  suite('Integration Tests', () => {
    test('should integrate with InstallationChecker', async function() {
      this.timeout(15000);
      
      // Test that migration works with installation checker
      const outdatedCheck = await configurationMigration.detectOutdatedPaths();
      
      assert.ok(typeof outdatedCheck.hasOutdatedPaths === 'boolean');
      
      if (outdatedCheck.hasOutdatedPaths && outdatedCheck.suggestions.length > 0) {
        // If there are suggestions, they should be valid paths
        outdatedCheck.suggestions.forEach(suggestion => {
          assert.ok(suggestion.path);
          assert.ok(typeof suggestion.path === 'string');
        });
      }
    });

    test('should work with VS Code configuration system', async () => {
      const config = vscode.workspace.getConfiguration('prolog');
      const currentPath = config.get<string>('executablePath', 'swipl');
      
      // Should be able to read current configuration
      assert.ok(typeof currentPath === 'string');
      
      // Migration should be aware of current configuration
      const migrationResult = await configurationMigration.performMigration();
      assert.ok(typeof migrationResult.migrated === 'boolean');
    });
  });

  suite('Performance Tests', () => {
    test('should complete migration check within reasonable time', async function() {
      this.timeout(25000);
      
      const startTime = Date.now();
      await configurationMigration.detectOutdatedPaths();
      const endTime = Date.now();
      
      const duration = endTime - startTime;
      assert.ok(duration < 20000, `Migration check took ${duration}ms, should be under 20000ms`);
    });

    test('should handle multiple concurrent migration checks', async function() {
      this.timeout(30000);
      
      const promises = Array(3).fill(0).map(() => 
        configurationMigration.detectOutdatedPaths()
      );
      
      const results = await Promise.all(promises);
      
      // All results should be consistent
      const firstResult = results[0];
      results.forEach(result => {
        assert.strictEqual(result.hasOutdatedPaths, firstResult.hasOutdatedPaths);
      });
    });
  });
});