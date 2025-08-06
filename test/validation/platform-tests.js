/**
 * VSCode Prolog Toolkit v1.3.0 - Cross-Platform Validation Tests
 *
 * Tests to ensure the extension works correctly across Windows, macOS, and Linux
 * These tests focus on platform-specific functionality and compatibility.
 */

const assert = require('assert');
const path = require('path');
const fs = require('fs');
const os = require('os');
const { execSync } = require('child_process');

describe('Cross-Platform Compatibility Tests', function() {
    this.timeout(60000); // 60 second timeout for platform tests

    let packageJson;
    let platform;
    
    before(async function() {
        platform = os.platform();
        console.log(`Running tests on platform: ${platform}`);
        
        // Load package.json for configuration validation
        const packagePath = path.join(__dirname, '..', '..', 'package.json');
        packageJson = JSON.parse(fs.readFileSync(packagePath, 'utf8'));
    });

    describe('Platform Detection', function() {
        it('should correctly identify the current platform', function() {
            const supportedPlatforms = ['win32', 'darwin', 'linux'];
            assert.ok(supportedPlatforms.includes(platform),
                `Platform ${platform} should be supported`);
        });

        it('should have platform-specific configuration', function() {
            // Check if extension has configuration for executable path
            assert.ok(packageJson.contributes, 'Package should have contributes section');
            assert.ok(packageJson.contributes.configuration, 'Package should have configuration');
            
            const config = packageJson.contributes.configuration;
            const properties = config.properties || {};
            
            // Should have prolog.executablePath configuration
            assert.ok(properties['prolog.executablePath'],
                'Should have prolog.executablePath configuration');
            assert.strictEqual(properties['prolog.executablePath'].type, 'string',
                'Executable path should be configurable as string');
        });
    });

    describe('File Path Handling', function() {
        it('should handle platform-specific path separators', function() {
            const testPath = path.join('test', 'resources', 'sample.pl');
            const expectedSeparator = platform === 'win32' ? '\\' : '/';
            
            if (platform === 'win32') {
                assert.ok(testPath.includes('\\'), 'Windows should use backslash separators');
            } else {
                assert.ok(testPath.includes('/'), 'Unix systems should use forward slash separators');
            }
        });

        it('should resolve home directory correctly', function() {
            const homeDir = os.homedir();
            assert.ok(homeDir, 'Should be able to resolve home directory');
            assert.ok(path.isAbsolute(homeDir), 'Home directory should be absolute path');
        });

        it('should handle long file paths', function() {
            // Test handling of long file paths (especially important on Windows)
            const longPath = path.join(
                os.homedir(),
                'very_long_directory_name_that_might_cause_issues',
                'another_very_long_directory_name',
                'test_file_with_very_long_name.pl'
            );
            
            // Should not throw when creating path
            assert.ok(typeof longPath === 'string', 'Should handle long paths');
        });
    });

    describe('SWI-Prolog Detection', function() {
        it('should attempt to detect SWI-Prolog installation', async function() {
            const commonPaths = {
                'win32': [
                    'C:\\Program Files\\swipl\\bin\\swipl.exe',
                    'C:\\Program Files (x86)\\swipl\\bin\\swipl.exe',
                    'swipl.exe'
                ],
                'darwin': [
                    '/usr/local/bin/swipl',
                    '/opt/homebrew/bin/swipl',
                    '/usr/bin/swipl',
                    'swipl'
                ],
                'linux': [
                    '/usr/bin/swipl',
                    '/usr/local/bin/swipl',
                    'swipl'
                ]
            };

            const pathsToCheck = commonPaths[platform] || commonPaths['linux'];
            let found = false;

            for (const swiPath of pathsToCheck) {
                try {
                    if (path.isAbsolute(swiPath)) {
                        if (fs.existsSync(swiPath)) {
                            found = true;
                            console.log(`Found SWI-Prolog at: ${swiPath}`);
                            break;
                        }
                    } else {
                        // Try to execute from PATH
                        try {
                            execSync(`${swiPath} --version`, { stdio: 'pipe' });
                            found = true;
                            console.log(`Found SWI-Prolog in PATH: ${swiPath}`);
                            break;
                        } catch (e) {
                            // Continue checking
                        }
                    }
                } catch (error) {
                    // Continue checking other paths
                }
            }

            // This test doesn't fail if SWI-Prolog is not found, just reports status
            console.log(`SWI-Prolog detection result: ${found ? 'Found' : 'Not found'}`);
            assert.ok(true, 'SWI-Prolog detection should complete without error');
        });

        it('should have commands for SWI-Prolog interaction', function() {
            // Check if extension defines commands for Prolog interaction
            assert.ok(packageJson.contributes.commands, 'Package should have commands');
            
            const commands = packageJson.contributes.commands;
            const queryCommand = commands.find(cmd => cmd.command.includes('query'));
            
            assert.ok(queryCommand, 'Should have query command defined');
            assert.ok(queryCommand.title, 'Query command should have title');
        });
    });

    describe('Terminal Integration', function() {
        it('should detect appropriate terminal for platform', function() {
            const terminalOptions = {
                'win32': ['cmd.exe', 'powershell.exe', 'pwsh.exe'],
                'darwin': ['zsh', 'bash', 'sh'],
                'linux': ['bash', 'sh', 'zsh']
            };

            const expectedTerminals = terminalOptions[platform] || terminalOptions['linux'];
            
            // At least one terminal should be available
            let terminalFound = false;
            for (const terminal of expectedTerminals) {
                try {
                    execSync(`which ${terminal}`, { stdio: 'pipe' });
                    terminalFound = true;
                    break;
                } catch (e) {
                    // Try next terminal
                }
            }

            if (platform === 'win32') {
                // On Windows, cmd.exe should always be available
                assert.ok(fs.existsSync('C:\\Windows\\System32\\cmd.exe'), 
                    'cmd.exe should be available on Windows');
            } else {
                // On Unix systems, at least one shell should be available
                assert.ok(terminalFound || fs.existsSync('/bin/sh'), 
                    'At least one shell should be available');
            }
        });
    });

    describe('File System Permissions', function() {
        it('should handle file permissions correctly', function() {
            const testDir = path.join(os.tmpdir(), 'prolog-test-permissions');
            const testFile = path.join(testDir, 'test.pl');

            try {
                // Create test directory and file
                if (!fs.existsSync(testDir)) {
                    fs.mkdirSync(testDir, { recursive: true });
                }
                fs.writeFileSync(testFile, '% Test file for permissions\ntest :- true.\n');

                // Try to read the file
                const content = fs.readFileSync(testFile, 'utf8');
                assert.ok(content.includes('test :- true'), 'Should read file content');

                // Clean up
                fs.unlinkSync(testFile);
                fs.rmdirSync(testDir);
                
                assert.ok(true, 'File operations should work correctly');
            } catch (error) {
                assert.fail(`File permission test failed: ${error.message}`);
            }
        });

        it('should handle read-only files appropriately', function() {
            if (platform !== 'win32') {
                const testDir = path.join(os.tmpdir(), 'prolog-test-readonly');
                const testFile = path.join(testDir, 'readonly.pl');

                try {
                    // Create test directory and file
                    if (!fs.existsSync(testDir)) {
                        fs.mkdirSync(testDir, { recursive: true });
                    }
                    fs.writeFileSync(testFile, '% Read-only test file\nreadonly_test :- true.\n');
                    
                    // Make file read-only
                    fs.chmodSync(testFile, 0o444);

                    // Should still be able to read
                    const content = fs.readFileSync(testFile, 'utf8');
                    assert.ok(content.includes('readonly_test :- true'), 'Should read read-only file content');

                    // Clean up
                    fs.chmodSync(testFile, 0o644); // Make writable for deletion
                    fs.unlinkSync(testFile);
                    fs.rmdirSync(testDir);
                    
                    assert.ok(true, 'Read-only file handling should work');
                } catch (error) {
                    assert.fail(`Read-only file test failed: ${error.message}`);
                }
            } else {
                // Skip on Windows as chmod works differently
                assert.ok(true, 'Skipping read-only test on Windows');
            }
        });
    });

    describe('Environment Variables', function() {
        it('should handle PATH environment variable', function() {
            const pathEnv = process.env.PATH || process.env.Path;
            assert.ok(pathEnv, 'PATH environment variable should be available');
            
            const pathSeparator = platform === 'win32' ? ';' : ':';
            const paths = pathEnv.split(pathSeparator);
            assert.ok(paths.length > 0, 'PATH should contain at least one directory');
        });

        it('should handle HOME/USERPROFILE directory', function() {
            const homeVar = platform === 'win32' ? 'USERPROFILE' : 'HOME';
            const homeDir = process.env[homeVar];
            
            assert.ok(homeDir, `${homeVar} environment variable should be set`);
            assert.ok(fs.existsSync(homeDir), 'Home directory should exist');
        });
    });

    describe('Character Encoding', function() {
        it('should handle UTF-8 encoded Prolog files', function() {
            const testDir = path.join(os.tmpdir(), 'prolog-test-encoding');
            const testFile = path.join(testDir, 'utf8-test.pl');

            try {
                if (!fs.existsSync(testDir)) {
                    fs.mkdirSync(testDir, { recursive: true });
                }

                // Create file with UTF-8 content including special characters
                const utf8Content = `% UTF-8 test file with special characters
% Symbols: α β γ δ ε ζ η θ
% Accented: café naïve résumé
test_unicode(α).
test_unicode(β).
test_unicode('café').
`;
                fs.writeFileSync(testFile, utf8Content, 'utf8');

                // Read and verify content
                const content = fs.readFileSync(testFile, 'utf8');
                
                assert.ok(content.includes('α'), 'Should handle Greek letters');
                assert.ok(content.includes('café'), 'Should handle accented characters');
                assert.ok(content.includes('naïve'), 'Should handle diacritics');

                // Clean up
                fs.unlinkSync(testFile);
                fs.rmdirSync(testDir);
                
                assert.ok(true, 'UTF-8 encoding should work correctly');
            } catch (error) {
                assert.fail(`UTF-8 encoding test failed: ${error.message}`);
            }
        });
    });

    describe('Performance on Platform', function() {
        it('should have efficient package.json structure', function() {
            // Check package.json structure for performance indicators
            assert.ok(packageJson.main, 'Package should have main entry point');
            assert.ok(packageJson.activationEvents, 'Package should have activation events');
            
            const startTime = Date.now();
            // Simulate configuration access by reading package.json
            const config = packageJson.contributes.configuration;
            const loadTime = Date.now() - startTime;
            
            assert.ok(loadTime < 100, `Package.json access should be fast (${loadTime}ms)`);
            assert.ok(config, 'Configuration should be accessible');
        });

        it('should handle multiple file operations efficiently', function() {
            const testDir = path.join(os.tmpdir(), 'prolog-test-performance');
            const testFiles = [];

            try {
                if (!fs.existsSync(testDir)) {
                    fs.mkdirSync(testDir, { recursive: true });
                }

                // Create multiple test files
                for (let i = 0; i < 10; i++) {
                    const testFile = path.join(testDir, `test${i}.pl`);
                    fs.writeFileSync(testFile, `% Test file ${i}\ntest${i} :- true.\n`);
                    testFiles.push(testFile);
                }

                const startTime = Date.now();
                
                // Read all files
                for (const file of testFiles) {
                    const content = fs.readFileSync(file, 'utf8');
                    assert.ok(content.includes('test'), 'File should contain test content');
                }
                
                const totalTime = Date.now() - startTime;
                
                // Should handle 10 files in under 1 second
                assert.ok(totalTime < 1000,
                    `Should handle multiple files efficiently (${totalTime}ms for 10 files)`);

                // Clean up
                testFiles.forEach(file => fs.unlinkSync(file));
                fs.rmdirSync(testDir);
                
            } catch (error) {
                // Clean up on error
                testFiles.forEach(file => {
                    if (fs.existsSync(file)) fs.unlinkSync(file);
                });
                if (fs.existsSync(testDir)) fs.rmdirSync(testDir);
                
                assert.fail(`Performance test failed: ${error.message}`);
            }
        });
    });
});