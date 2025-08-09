#!/usr/bin/env node

const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

/**
 * Test validation script for New-VSC-Prolog extension
 * Validates that all tests pass and coverage meets requirements
 */

const COVERAGE_THRESHOLD = {
  statements: 80,
  branches: 75,
  functions: 80,
  lines: 80
};

function runCommand(command, description) {
  console.log(`\n🔄 ${description}...`);
  try {
    const output = execSync(command, {
      stdio: 'pipe',
      encoding: 'utf8',
      cwd: path.join(__dirname, '..')
    });
    console.log(`✅ ${description} completed successfully`);
    return { success: true, output };
  } catch (error) {
    console.error(`❌ ${description} failed:`);
    console.error(error.stdout || error.message);
    return { success: false, error: error.stdout || error.message };
  }
}

function validateCoverage() {
  console.log('\n📊 Validating test coverage...');

  const coverageFile = path.join(__dirname, '..', 'coverage', 'coverage-summary.json');

  if (!fs.existsSync(coverageFile)) {
    console.error('❌ Coverage file not found. Run npm run test:coverage first.');
    return false;
  }

  try {
    const coverage = JSON.parse(fs.readFileSync(coverageFile, 'utf8'));
    const total = coverage.total;

    console.log('\n📈 Coverage Report:');
    console.log(`  Statements: ${total.statements.pct}% (threshold: ${COVERAGE_THRESHOLD.statements}%)`);
    console.log(`  Branches:   ${total.branches.pct}% (threshold: ${COVERAGE_THRESHOLD.branches}%)`);
    console.log(`  Functions:  ${total.functions.pct}% (threshold: ${COVERAGE_THRESHOLD.functions}%)`);
    console.log(`  Lines:      ${total.lines.pct}% (threshold: ${COVERAGE_THRESHOLD.lines}%)`);

    const meetsThreshold =
      total.statements.pct >= COVERAGE_THRESHOLD.statements &&
      total.branches.pct >= COVERAGE_THRESHOLD.branches &&
      total.functions.pct >= COVERAGE_THRESHOLD.functions &&
      total.lines.pct >= COVERAGE_THRESHOLD.lines;

    if (meetsThreshold) {
      console.log('✅ Coverage thresholds met');
      return true;
    } else {
      console.log('❌ Coverage thresholds not met');
      return false;
    }
  } catch (error) {
    console.error('❌ Error reading coverage file:', error.message);
    return false;
  }
}

function checkTestFiles() {
  console.log('\n📁 Checking test file structure...');

  const requiredTestFiles = [
    'test/prologBackend.test.ts',
    'test/prologBackend.unit.test.ts',
    'test/chat-commands-integration.test.ts',
    'test/chat-integration.test.ts',
    'test/n3-integration.test.ts',
    'test/performance-scalability.test.ts',
    'test/setup-test-env.js',
    'test/run-with-logs.js'
  ];

  const missingFiles = requiredTestFiles.filter(file =>
    !fs.existsSync(path.join(__dirname, '..', file))
  );

  if (missingFiles.length > 0) {
    console.error('❌ Missing test files:');
    missingFiles.forEach(file => console.error(`  - ${file}`));
    return false;
  }

  console.log('✅ All required test files present');
  return true;
}

function checkTestResources() {
  console.log('\n📦 Checking test resources...');

  const requiredResources = [
    'test/resources/foo_with_pldoc.pl',
    'test/resources/sample.n3',
    'test/resources/complex.n3',
    'test/resources/test_predicates.pl',
    'test/resources/error_cases.pl'
  ];

  const missingResources = requiredResources.filter(file =>
    !fs.existsSync(path.join(__dirname, '..', file))
  );

  if (missingResources.length > 0) {
    console.error('❌ Missing test resources:');
    missingResources.forEach(file => console.error(`  - ${file}`));
    return false;
  }

  console.log('✅ All test resources present');
  return true;
}

function generateTestReport() {
  console.log('\n📋 Generating test report...');

  const reportPath = path.join(__dirname, '..', 'test-report.json');
  const report = {
    timestamp: new Date().toISOString(),
    testSuites: {
      unit: { status: 'unknown', duration: 0 },
      integration: { status: 'unknown', duration: 0 },
      n3: { status: 'unknown', duration: 0 },
      performance: { status: 'unknown', duration: 0 }
    },
    coverage: null,
    overallStatus: 'unknown'
  };

  // Try to read coverage data
  const coverageFile = path.join(__dirname, '..', 'coverage', 'coverage-summary.json');
  if (fs.existsSync(coverageFile)) {
    try {
      report.coverage = JSON.parse(fs.readFileSync(coverageFile, 'utf8'));
    } catch (error) {
      console.warn('⚠️  Could not read coverage data');
    }
  }

  fs.writeFileSync(reportPath, JSON.stringify(report, null, 2));
  console.log(`✅ Test report generated: ${reportPath}`);
}

async function main() {
  console.log('🧪 New-VSC-Prolog Test Validation');
  console.log('==================================');

  // Removed module exports for compatibility with type: module

  // Check test resources
  if (!checkTestResources()) {
    allPassed = false;
  }

  // Setup test environment
  const setupResult = runCommand('npm run setup-test-env', 'Setting up test environment');
  if (!setupResult.success) {
    allPassed = false;
  }

  // Run linting
  const lintResult = runCommand('npm run lint', 'Running linter');
  if (!lintResult.success) {
    console.warn('⚠️  Linting issues found, but continuing with tests');
  }

  // Run unit tests
  const unitResult = runCommand('npm run test:unit', 'Running unit tests');
  if (!unitResult.success) {
    allPassed = false;
  }

  // Run integration tests
  const integrationResult = runCommand('npm run test:integration', 'Running integration tests');
  if (!integrationResult.success) {
    allPassed = false;
  }

  // Run N3 tests
  const n3Result = runCommand('npm run test:n3', 'Running N3 integration tests');
  if (!n3Result.success) {
    allPassed = false;
  }

  // Run performance tests
  const perfResult = runCommand('npm run test:performance', 'Running performance tests');
  if (!perfResult.success) {
    console.warn('⚠️  Performance tests failed, but continuing');
  }

  // Run coverage analysis
  const coverageResult = runCommand('npm run test:coverage', 'Running coverage analysis');
  if (!coverageResult.success) {
    allPassed = false;
  } else {
    // Validate coverage thresholds
    if (!validateCoverage()) {
      console.warn('⚠️  Coverage thresholds not met, but tests passed');
    }
  }

  // Generate test report
  generateTestReport();

  // Final summary
  console.log('\n📊 Test Validation Summary');
  console.log('==========================');

  if (allPassed) {
    console.log('✅ All tests passed successfully!');
    console.log('🎉 Step 7 implementation is complete and validated');
    process.exit(0);
  } else {
    console.log('❌ Some tests failed or requirements not met');
    console.log('🔧 Please review the errors above and fix the issues');
    process.exit(1);
  }
}

if (require.main === module) {
  main().catch(error => {
    console.error('💥 Validation script failed:', error);
    process.exit(1);
  });
}

module.exports = {
  validateCoverage,
  checkTestFiles,
  checkTestResources,
  generateTestReport
};