#!/usr/bin/env node
// scripts/precommit.js
// Cross-platform pre-commit script for Husky: runs lint, format, and test, outputs results, and exits nonzero on failure.

const { spawnSync } = require('child_process');
const path = require('path');

function runScript(command, args, label) {
  console.log(`\n=== Running: ${label} ===`);
  const result = spawnSync(command, args, { stdio: 'inherit', shell: process.platform === 'win32' });
  if (result.status !== 0) {
    console.error(`\n✖ ${label} failed.`);
    process.exit(result.status || 1);
  }
  console.log(`\n✔ ${label} passed.`);
}

try {
  runScript('npm', ['run', 'lint'], 'Lint');
  runScript('npm', ['run', 'format', '--', '--check'], 'Format Check');
  runScript('npm', ['test'], 'Tests');
  console.log('\nAll pre-commit checks passed.');
} catch (err) {
  console.error('\nPre-commit checks failed:', err);
  process.exit(1);
}
