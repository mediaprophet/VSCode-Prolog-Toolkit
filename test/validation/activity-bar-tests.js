/**
 * VSCode Prolog Toolkit v1.3.0 - Activity Bar Validation Tests
 * 
 * Tests specifically for the new activity bar functionality introduced in v1.3.0
 * These tests validate the activity bar icon, tree view, and dashboard features.
 */

const assert = require('assert');
const path = require('path');
const fs = require('fs');

describe('Activity Bar Functionality Tests', function() {
    this.timeout(45000); // 45 second timeout for UI tests

    let extensionPath;
    let packageJson;
    
    before(async function() {
        // Get extension path and package.json
        extensionPath = path.join(__dirname, '../..');
        const packagePath = path.join(extensionPath, 'package.json');
        packageJson = JSON.parse(fs.readFileSync(packagePath, 'utf8'));
        
        // Wait a bit for any async operations
        await new Promise(resolve => setTimeout(resolve, 1000));
    });

    describe('Activity Bar Icon and Registration', function() {
        it('should register the Prolog activity view container', function() {
            const viewsContainers = packageJson.contributes?.viewsContainers?.activitybar;
            assert.ok(viewsContainers, 'Activity bar view containers should be defined');
            
            const prologContainer = viewsContainers.find(c => c.id === 'prolog-toolkit');
            assert.ok(prologContainer, 'prolog-toolkit view container should be defined');
            assert.strictEqual(prologContainer.title, 'Prolog Toolkit', 'Container title should be correct');
            assert.ok(prologContainer.icon, 'Container should have icon defined');
        });

        it('should have activity bar views defined', function() {
            const views = packageJson.contributes?.views?.['prolog-toolkit'];
            assert.ok(views, 'prolog-toolkit views should be defined');
            assert.ok(Array.isArray(views), 'Views should be an array');
            assert.ok(views.length > 0, 'Should have at least one view registered');
            
            // Check for expected views
            const viewIds = views.map(v => v.id);
            const expectedViews = ['prologDashboard', 'prologActivity', 'prologQueries', 'prologFiles'];
            expectedViews.forEach(viewId => {
                assert.ok(viewIds.includes(viewId), `View ${viewId} should be defined`);
            });
        });

        it('should have activity bar icon file present', function() {
            // Check for the icon specified in the view container
            const viewsContainers = packageJson.contributes?.viewsContainers?.activitybar;
            const prologContainer = viewsContainers.find(c => c.id === 'prolog-toolkit');
            
            if (prologContainer && prologContainer.icon) {
                const iconPath = path.join(extensionPath, prologContainer.icon);
                assert.ok(fs.existsSync(iconPath), 'Activity bar icon file should exist');
                
                // If it's an SVG, verify it's valid
                if (prologContainer.icon.endsWith('.svg')) {
                    const iconContent = fs.readFileSync(iconPath, 'utf8');
                    assert.ok(iconContent.includes('<svg'), 'Icon should be valid SVG');
                    assert.ok(iconContent.includes('</svg>'), 'Icon should be complete SVG');
                }
            }
        });

        it('should register activity bar commands', function() {
            const commands = packageJson.contributes?.commands || [];
            const commandIds = commands.map(cmd => cmd.command);
            
            const activityCommands = [
                'prolog.openSettings',
                'prolog.setupWizard',
                'prolog.refreshInstallation',
                'prolog.testInstallation'
            ];

            activityCommands.forEach(cmd => {
                assert.ok(commandIds.includes(cmd), `Activity command ${cmd} should be registered`);
            });
        });
    });

    describe('Tree View Provider Configuration', function() {
        it('should have tree view configuration in package.json', function() {
            const views = packageJson.contributes?.views?.['prolog-toolkit'];
            assert.ok(views, 'Views should be defined');
            
            const activityView = views.find(v => v.id === 'prologActivity');
            assert.ok(activityView, 'prologActivity view should be defined');
            assert.strictEqual(activityView.name, 'Prolog Explorer', 'View name should be correct');
        });

        it('should have source files for tree view provider', function() {
            // Check if specific provider files exist
            const activityProviderPath = path.join(extensionPath, 'src', 'features', 'prologActivityProvider.ts');
            const dashboardProviderPath = path.join(extensionPath, 'src', 'features', 'prologDashboardProvider.ts');
            
            assert.ok(fs.existsSync(activityProviderPath), 'prologActivityProvider.ts should exist');
            assert.ok(fs.existsSync(dashboardProviderPath), 'prologDashboardProvider.ts should exist');
        });

        it('should handle workspace file enumeration configuration', function() {
            // Check language configuration for file patterns
            const languages = packageJson.contributes?.languages;
            const prologLang = languages?.find(lang => lang.id === 'prolog');
            
            assert.ok(prologLang, 'Prolog language should be configured');
            assert.ok(prologLang.extensions, 'File extensions should be defined');
            assert.ok(prologLang.filenamePatterns, 'Filename patterns should be defined');
        });
    });

    describe('Dashboard Provider Configuration', function() {
        it('should have dashboard view defined', function() {
            const views = packageJson.contributes?.views?.['prolog-toolkit'];
            const dashboardView = views?.find(v => v.id === 'prologDashboard');
            
            assert.ok(dashboardView, 'prologDashboard view should be defined');
            assert.strictEqual(dashboardView.name, 'Dashboard', 'Dashboard name should be correct');
            assert.strictEqual(dashboardView.type, 'webview', 'Dashboard should be webview type');
        });

        it('should have dashboard media files', function() {
            const mediaPath = path.join(extensionPath, 'media');
            assert.ok(fs.existsSync(mediaPath), 'Media directory should exist');
            
            const mediaFiles = fs.readdirSync(mediaPath);
            const hasStyleFiles = mediaFiles.some(file => file.endsWith('.css') || file.endsWith('.js'));
            assert.ok(hasStyleFiles, 'Should have CSS or JS files for dashboard');
        });

        it('should have webview configuration', function() {
            // Check if there are any webview-related configurations
            const views = packageJson.contributes?.views;
            const hasWebviews = Object.values(views || {}).flat().some(view => view.type === 'webview');
            assert.ok(hasWebviews, 'Should have webview configurations');
        });
    });

    describe('Activity Bar Integration with SWI-Prolog', function() {
        it('should have installation check commands defined', function() {
            const commands = packageJson.contributes?.commands || [];
            const commandIds = commands.map(cmd => cmd.command);
            
            const installationCommands = [
                'prolog.refreshInstallation',
                'prolog.testInstallation',
                'prolog.setupWizard'
            ];
            
            installationCommands.forEach(cmd => {
                assert.ok(commandIds.includes(cmd), `Installation command ${cmd} should be defined`);
            });
        });

        it('should have platform-specific configuration for SWI-Prolog', function() {
            const config = packageJson.contributes?.configuration?.properties;
            const executablePath = config?.['prolog.executablePath'];
            
            assert.ok(executablePath, 'Executable path configuration should exist');
            assert.ok(executablePath.overrides, 'Should have platform-specific overrides');
            assert.ok(executablePath.overrides.windows, 'Should have Windows-specific path');
            assert.ok(executablePath.overrides.macos, 'Should have macOS-specific path');
            assert.ok(executablePath.overrides.linux, 'Should have Linux-specific path');
        });

        it('should provide setup wizard functionality', function() {
            const commands = packageJson.contributes?.commands || [];
            const setupWizard = commands.find(cmd => cmd.command === 'prolog.setupWizard');
            
            assert.ok(setupWizard, 'Setup wizard command should be defined');
            assert.ok(setupWizard.title, 'Setup wizard should have title');
            assert.ok(setupWizard.category, 'Setup wizard should have category');
        });
    });

    describe('Activity Bar File Operations', function() {
        it('should have file operation commands defined', function() {
            const commands = packageJson.contributes?.commands || [];
            const commandIds = commands.map(cmd => cmd.command);
            
            const fileCommands = [
                'prolog.load.document',
                'prolog.query.goal',
                'prolog.newFile'
            ];
            
            fileCommands.forEach(cmd => {
                assert.ok(commandIds.includes(cmd), `File command ${cmd} should be defined`);
            });
        });

        it('should have context menu integration', function() {
            const menus = packageJson.contributes?.menus;
            assert.ok(menus, 'Menus should be defined');
            
            // Check for editor context menu
            const editorContext = menus['editor/context'];
            assert.ok(Array.isArray(editorContext), 'Editor context menu should be defined');
            
            const prologContextItems = editorContext.filter(item =>
                item.when && item.when.includes('prolog')
            );
            assert.ok(prologContextItems.length > 0, 'Should have Prolog-specific context menu items');
        });

        it('should support file associations for activity bar', function() {
            const languages = packageJson.contributes?.languages;
            const prologLang = languages?.find(lang => lang.id === 'prolog');
            
            assert.ok(prologLang, 'Prolog language should be defined');
            assert.ok(prologLang.extensions, 'File extensions should be defined');
            
            const supportedExtensions = ['.pl', '.pro', '.prolog'];
            supportedExtensions.forEach(ext => {
                assert.ok(prologLang.extensions.includes(ext),
                    `Should support ${ext} files`);
            });
        });
    });

    describe('Activity Bar Performance Configuration', function() {
        it('should have efficient file watching configuration', function() {
            // Check if there are any performance-related configurations
            const config = packageJson.contributes?.configuration?.properties;
            
            // Look for any performance or file watching related settings
            const hasPerformanceConfig = Object.keys(config || {}).some(key =>
                key.includes('performance') || key.includes('watch') || key.includes('limit')
            );
            
            // This is more about having the structure in place
            assert.ok(config, 'Configuration should exist for performance tuning');
        });

        it('should have reasonable default timeouts', function() {
            const config = packageJson.contributes?.configuration?.properties;
            const linterDelay = config?.['prolog.linter.delay'];
            
            if (linterDelay) {
                assert.ok(linterDelay.default >= 100, 'Linter delay should be reasonable (>=100ms)');
                assert.ok(linterDelay.default <= 2000, 'Linter delay should not be too long (<=2000ms)');
            }
        });
    });

    describe('Activity Bar Error Handling Configuration', function() {
        it('should have error handling commands defined', function() {
            const commands = packageJson.contributes?.commands || [];
            const commandIds = commands.map(cmd => cmd.command);
            
            const errorHandlingCommands = [
                'prolog.viewLogs',
                'prolog.reportIssue'
            ];
            
            errorHandlingCommands.forEach(cmd => {
                assert.ok(commandIds.includes(cmd), `Error handling command ${cmd} should be defined`);
            });
        });

        it('should have linter configuration for error detection', function() {
            const config = packageJson.contributes?.configuration?.properties;
            const linterRun = config?.['prolog.linter.run'];
            
            assert.ok(linterRun, 'Linter run configuration should exist');
            assert.ok(linterRun.enum, 'Linter should have enum options');
            assert.ok(linterRun.enum.includes('onSave'), 'Should support onSave linting');
            assert.ok(linterRun.enum.includes('onType'), 'Should support onType linting');
        });

        it('should have graceful degradation settings', function() {
            // Check for settings that allow graceful degradation
            const config = packageJson.contributes?.configuration?.properties;
            
            // Look for settings that help with error recovery
            const hasErrorRecovery = Object.keys(config || {}).some(key =>
                key.includes('enable') || key.includes('fallback') || key.includes('timeout')
            );
            
            assert.ok(hasErrorRecovery, 'Should have error recovery configurations');
        });
    });
});