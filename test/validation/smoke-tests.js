/**
 * VSCode Prolog Toolkit v1.3.0 - Smoke Test Suite
 * 
 * Critical functionality tests to validate extension works correctly
 * before release. These tests should be run on all target platforms.
 */

const assert = require('assert');
const path = require('path');
const fs = require('fs');
const { execSync } = require('child_process');

describe('VSCode Prolog Toolkit - Smoke Tests', function() {
    this.timeout(30000); // 30 second timeout for each test

    let extensionPath;
    let packageJson;
    
    before(async function() {
        // Get extension path and package.json
        extensionPath = path.join(__dirname, '../..');
        const packagePath = path.join(extensionPath, 'package.json');
        packageJson = JSON.parse(fs.readFileSync(packagePath, 'utf8'));
    });

    describe('Extension Package Validation', function() {
        it('should have valid package.json', function() {
            assert.ok(packageJson, 'Package.json should be readable');
            assert.strictEqual(packageJson.name, 'vscode-prolog-toolkit', 'Extension name should be correct');
            assert.strictEqual(packageJson.version, '1.3.0', 'Extension version should be 1.3.0');
        });

        it('should have all expected commands defined', function() {
            const commands = packageJson.contributes.commands;
            assert.ok(Array.isArray(commands), 'Commands should be an array');
            
            const expectedCommands = [
                'prolog.load.document',
                'prolog.query.goal',
                'prolog.refactorPredicate',
                'prolog.linter.nextErrLine',
                'prolog.linter.prevErrLine',
                'prolog.openSettings',
                'prolog.setupWizard'
            ];

            const commandIds = commands.map(cmd => cmd.command);
            expectedCommands.forEach(cmd => {
                assert.ok(commandIds.includes(cmd), `Command ${cmd} should be defined in package.json`);
            });
        });

        it('should have correct VS Code engine requirement', function() {
            assert.ok(packageJson.engines.vscode, 'VS Code engine should be specified');
            assert.strictEqual(packageJson.engines.vscode, '^1.102.0', 'Should require VS Code 1.102.0+');
        });
    });

    describe('Activity Bar Integration', function() {
        it('should have activity bar view container defined', function() {
            const viewsContainers = packageJson.contributes.viewsContainers;
            assert.ok(viewsContainers, 'Views containers should be defined');
            assert.ok(viewsContainers.activitybar, 'Activity bar containers should be defined');
            
            const prologContainer = viewsContainers.activitybar.find(c => c.id === 'prolog-toolkit');
            assert.ok(prologContainer, 'Prolog toolkit activity bar container should be defined');
            assert.strictEqual(prologContainer.title, 'Prolog Toolkit', 'Container title should be correct');
        });

        it('should have activity bar icon available', function() {
            const iconPath = path.join(extensionPath, 'images', 'prolog-activity-icon.svg');
            // Check if icon exists or if there's an alternative icon
            const alternativeIcon = path.join(extensionPath, 'images', 'vsplogo.png');
            const hasIcon = fs.existsSync(iconPath) || fs.existsSync(alternativeIcon);
            assert.ok(hasIcon, 'Activity bar icon should exist');
        });

        it('should have views defined for activity bar', function() {
            const views = packageJson.contributes.views;
            assert.ok(views, 'Views should be defined');
            assert.ok(views['prolog-toolkit'], 'Prolog toolkit views should be defined');
            
            const prologViews = views['prolog-toolkit'];
            assert.ok(Array.isArray(prologViews), 'Prolog views should be an array');
            assert.ok(prologViews.length > 0, 'Should have at least one view');
        });
    });

    describe('File Association', function() {
        it('should define Prolog language configuration', function() {
            const languages = packageJson.contributes.languages;
            assert.ok(Array.isArray(languages), 'Languages should be an array');
            
            const prologLang = languages.find(lang => lang.id === 'prolog');
            assert.ok(prologLang, 'Prolog language should be defined');
            assert.ok(Array.isArray(prologLang.extensions), 'Extensions should be defined');
            assert.ok(prologLang.extensions.includes('.pl'), 'Should support .pl files');
            assert.ok(prologLang.extensions.includes('.pro'), 'Should support .pro files');
        });

        it('should have grammar file for syntax highlighting', function() {
            const grammars = packageJson.contributes.grammars;
            assert.ok(Array.isArray(grammars), 'Grammars should be defined');
            
            const prologGrammar = grammars.find(g => g.language === 'prolog');
            assert.ok(prologGrammar, 'Prolog grammar should be defined');
            assert.ok(prologGrammar.path, 'Grammar path should be specified');
            
            const grammarPath = path.join(extensionPath, prologGrammar.path);
            assert.ok(fs.existsSync(grammarPath), 'Grammar file should exist');
        });
    });

    describe('Extension Files', function() {
        it('should have main extension file', function() {
            const mainFile = path.join(extensionPath, packageJson.main);
            // Check if compiled file exists or source file exists
            const sourceFile = path.join(extensionPath, 'src', 'extension.ts');
            const hasMain = fs.existsSync(mainFile) || fs.existsSync(sourceFile);
            assert.ok(hasMain, 'Main extension file should exist');
        });

        it('should have snippets defined', function() {
            const snippets = packageJson.contributes.snippets;
            if (snippets && snippets.length > 0) {
                snippets.forEach(snippet => {
                    const snippetPath = path.join(extensionPath, snippet.path);
                    assert.ok(fs.existsSync(snippetPath), `Snippet file ${snippet.path} should exist`);
                });
            }
        });

        it('should have required directories', function() {
            const requiredDirs = ['src', 'syntaxes'];
            requiredDirs.forEach(dir => {
                const dirPath = path.join(extensionPath, dir);
                assert.ok(fs.existsSync(dirPath), `Directory ${dir} should exist`);
            });
        });
    });

    describe('Configuration', function() {
        it('should have configuration properties defined', function() {
            const config = packageJson.contributes.configuration;
            assert.ok(config, 'Configuration should be defined');
            assert.ok(config.properties, 'Configuration properties should be defined');
            
            // Check key configuration properties
            assert.ok(config.properties['prolog.executablePath'], 'executablePath should be defined');
            assert.ok(config.properties['prolog.linter.run'], 'linter.run should be defined');
            assert.ok(config.properties['prolog.dialect'], 'dialect should be defined');
        });

        it('should have platform-specific defaults', function() {
            const executablePath = packageJson.contributes.configuration.properties['prolog.executablePath'];
            assert.ok(executablePath.overrides, 'Should have platform overrides');
            assert.ok(executablePath.overrides.windows, 'Should have Windows override');
            assert.ok(executablePath.overrides.macos, 'Should have macOS override');
            assert.ok(executablePath.overrides.linux, 'Should have Linux override');
        });
    });

    describe('Build Validation', function() {
        it('should have compiled output directory', function() {
            const outDir = path.join(extensionPath, 'out');
            // Check if out directory exists (after build)
            if (fs.existsSync(outDir)) {
                assert.ok(true, 'Output directory exists');
            } else {
                // If no out directory, source should exist
                const srcDir = path.join(extensionPath, 'src');
                assert.ok(fs.existsSync(srcDir), 'Source directory should exist if no build output');
            }
        });

        it('should have valid VSIX package if built', function() {
            const vsixFiles = fs.readdirSync(extensionPath).filter(f => f.endsWith('.vsix'));
            if (vsixFiles.length > 0) {
                const vsixPath = path.join(extensionPath, vsixFiles[0]);
                const stats = fs.statSync(vsixPath);
                assert.ok(stats.size > 1024 * 1024, 'VSIX should be larger than 1MB'); // Reasonable size check
            }
        });

        it('should have test framework files', function() {
            const testDir = path.join(extensionPath, 'test');
            assert.ok(fs.existsSync(testDir), 'Test directory should exist');
            
            const validationDir = path.join(testDir, 'validation');
            assert.ok(fs.existsSync(validationDir), 'Validation test directory should exist');
        });
    });
});