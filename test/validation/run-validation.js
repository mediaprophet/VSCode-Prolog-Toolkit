/**
 * VSCode Prolog Toolkit v1.3.0 - Validation Test Runner
 * 
 * Main script to run all validation tests for the extension.
 * This script coordinates the execution of smoke tests, activity bar tests,
 * and platform-specific tests.
 */

const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs');
const os = require('os');

class ValidationRunner {
    constructor() {
        this.results = {
            total: 0,
            passed: 0,
            failed: 0,
            skipped: 0,
            errors: []
        };
        this.startTime = Date.now();
        this.platform = os.platform();
    }

    log(message, level = 'INFO') {
        const timestamp = new Date().toISOString();
        const prefix = `[${timestamp}] [${level}]`;
        console.log(`${prefix} ${message}`);
    }

    async runTestSuite(suiteName, testFile) {
        this.log(`Starting ${suiteName}...`);
        
        try {
            const testPath = path.join(__dirname, testFile);
            if (!fs.existsSync(testPath)) {
                throw new Error(`Test file not found: ${testPath}`);
            }

            // Run the test using Mocha
            const mochaPath = path.join(__dirname, '../../node_modules/.bin/mocha');
            const command = `"${mochaPath}" "${testPath}" --reporter json`;
            
            this.log(`Executing: ${command}`);
            const output = execSync(command, {
                encoding: 'utf8',
                cwd: path.join(__dirname, '../..'),
                timeout: 120000, // 2 minute timeout
                stdio: ['pipe', 'pipe', 'pipe'] // Capture all output
            });

            // Try to parse JSON, handling cases where console output is mixed in
            let results;
            try {
                results = JSON.parse(output);
            } catch (parseError) {
                // If JSON parsing fails, try to extract JSON from the output
                const lines = output.split('\n');
                let jsonStart = -1;
                let jsonEnd = -1;
                
                for (let i = 0; i < lines.length; i++) {
                    if (lines[i].trim().startsWith('{')) {
                        jsonStart = i;
                        break;
                    }
                }
                
                for (let i = lines.length - 1; i >= 0; i--) {
                    if (lines[i].trim().endsWith('}')) {
                        jsonEnd = i;
                        break;
                    }
                }
                
                if (jsonStart >= 0 && jsonEnd >= 0) {
                    const jsonLines = lines.slice(jsonStart, jsonEnd + 1);
                    const jsonString = jsonLines.join('\n');
                    try {
                        results = JSON.parse(jsonString);
                    } catch (secondParseError) {
                        throw new Error(`Failed to parse test output as JSON: ${parseError.message}`);
                    }
                } else {
                    throw new Error(`No valid JSON found in test output: ${parseError.message}`);
                }
            }
            this.processResults(suiteName, results);
            
        } catch (error) {
            this.log(`Error running ${suiteName}: ${error.message}`, 'ERROR');
            this.results.errors.push({
                suite: suiteName,
                error: error.message,
                stack: error.stack
            });
        }
    }

    processResults(suiteName, mochaResults) {
        const { stats, tests } = mochaResults;
        
        this.log(`${suiteName} Results:`);
        this.log(`  Total: ${stats.tests}`);
        this.log(`  Passed: ${stats.passes}`);
        this.log(`  Failed: ${stats.failures}`);
        this.log(`  Skipped: ${stats.pending}`);
        this.log(`  Duration: ${stats.duration}ms`);

        this.results.total += stats.tests;
        this.results.passed += stats.passes;
        this.results.failed += stats.failures;
        this.results.skipped += stats.pending;

        // Log failed tests
        if (stats.failures > 0) {
            this.log(`Failed tests in ${suiteName}:`, 'ERROR');
            tests.filter(test => test.err).forEach(test => {
                this.log(`  - ${test.title}: ${test.err.message}`, 'ERROR');
            });
        }
    }

    async runPreValidationChecks() {
        this.log('Running pre-validation checks...');
        
        // Check if VS Code is available
        try {
            execSync('code --version', { stdio: 'pipe' });
            this.log('✓ VS Code CLI available');
        } catch (error) {
            this.log('⚠ VS Code CLI not available - some tests may fail', 'WARN');
        }

        // Check if Node.js version is compatible
        const nodeVersion = process.version;
        this.log(`Node.js version: ${nodeVersion}`);
        
        const majorVersion = parseInt(nodeVersion.slice(1).split('.')[0]);
        if (majorVersion < 18) {
            this.log('⚠ Node.js version may be too old (recommend 18+)', 'WARN');
        } else {
            this.log('✓ Node.js version compatible');
        }

        // Check platform
        this.log(`Platform: ${this.platform}`);
        const supportedPlatforms = ['win32', 'darwin', 'linux'];
        if (supportedPlatforms.includes(this.platform)) {
            this.log('✓ Platform supported');
        } else {
            this.log('⚠ Platform may not be fully supported', 'WARN');
        }

        // Check if test resources exist
        const resourcesDir = path.join(__dirname, '../resources');
        if (fs.existsSync(resourcesDir)) {
            this.log('✓ Test resources directory found');
            
            const requiredFiles = ['sample-basic.pl', 'sample-complex.pl'];
            requiredFiles.forEach(file => {
                const filePath = path.join(resourcesDir, file);
                if (fs.existsSync(filePath)) {
                    this.log(`✓ Test resource found: ${file}`);
                } else {
                    this.log(`⚠ Test resource missing: ${file}`, 'WARN');
                }
            });
        } else {
            this.log('⚠ Test resources directory not found', 'WARN');
        }
    }

    async runAllValidationTests() {
        this.log('='.repeat(60));
        this.log('VSCode Prolog Toolkit v1.3.0 - Validation Test Suite');
        this.log('='.repeat(60));
        
        await this.runPreValidationChecks();
        
        this.log('Starting validation test execution...');

        // Run test suites in order
        const testSuites = [
            { name: 'Smoke Tests', file: 'smoke-tests.js' },
            { name: 'Activity Bar Tests', file: 'activity-bar-tests.js' },
            { name: 'Platform Tests', file: 'platform-tests.js' }
        ];

        for (const suite of testSuites) {
            await this.runTestSuite(suite.name, suite.file);
            this.log(''); // Empty line for readability
        }

        this.generateReport();
    }

    generateReport() {
        const duration = Date.now() - this.startTime;
        const durationSeconds = (duration / 1000).toFixed(2);
        
        this.log('='.repeat(60));
        this.log('VALIDATION TEST SUMMARY');
        this.log('='.repeat(60));
        
        this.log(`Platform: ${this.platform}`);
        this.log(`Total Duration: ${durationSeconds}s`);
        this.log(`Total Tests: ${this.results.total}`);
        this.log(`Passed: ${this.results.passed}`);
        this.log(`Failed: ${this.results.failed}`);
        this.log(`Skipped: ${this.results.skipped}`);
        
        const successRate = this.results.total > 0 
            ? ((this.results.passed / this.results.total) * 100).toFixed(1)
            : '0.0';
        this.log(`Success Rate: ${successRate}%`);

        if (this.results.errors.length > 0) {
            this.log('');
            this.log('ERRORS ENCOUNTERED:');
            this.results.errors.forEach((error, index) => {
                this.log(`${index + 1}. ${error.suite}: ${error.error}`);
            });
        }

        // Generate recommendations
        this.generateRecommendations();

        // Save detailed report
        this.saveDetailedReport();
    }

    generateRecommendations() {
        this.log('');
        this.log('RECOMMENDATIONS:');
        
        if (this.results.failed === 0) {
            this.log('✓ All tests passed! Extension appears ready for release.');
        } else {
            this.log(`⚠ ${this.results.failed} test(s) failed. Review failures before release.`);
        }

        if (this.results.skipped > 0) {
            this.log(`ℹ ${this.results.skipped} test(s) were skipped. Consider investigating why.`);
        }

        const successRate = this.results.total > 0 
            ? (this.results.passed / this.results.total) * 100
            : 0;

        if (successRate < 90) {
            this.log('⚠ Success rate below 90%. Significant issues may exist.');
        } else if (successRate < 95) {
            this.log('⚠ Success rate below 95%. Minor issues should be addressed.');
        } else {
            this.log('✓ High success rate indicates good extension quality.');
        }
    }

    saveDetailedReport() {
        const reportData = {
            timestamp: new Date().toISOString(),
            platform: this.platform,
            nodeVersion: process.version,
            duration: Date.now() - this.startTime,
            results: this.results,
            environment: {
                cwd: process.cwd(),
                arch: os.arch(),
                totalmem: os.totalmem(),
                freemem: os.freemem()
            }
        };

        const reportPath = path.join(__dirname, `validation-report-${Date.now()}.json`);
        
        try {
            fs.writeFileSync(reportPath, JSON.stringify(reportData, null, 2));
            this.log(`Detailed report saved to: ${reportPath}`);
        } catch (error) {
            this.log(`Failed to save detailed report: ${error.message}`, 'ERROR');
        }
    }
}

// Main execution
async function main() {
    const runner = new ValidationRunner();
    
    try {
        await runner.runAllValidationTests();
        
        // Exit with appropriate code
        const exitCode = runner.results.failed > 0 ? 1 : 0;
        process.exit(exitCode);
        
    } catch (error) {
        console.error('Fatal error during validation:', error);
        process.exit(2);
    }
}

// Run if called directly
if (require.main === module) {
    main().catch(error => {
        console.error('Unhandled error:', error);
        process.exit(2);
    });
}

module.exports = ValidationRunner;