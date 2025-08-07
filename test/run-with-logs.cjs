// Clean dist/, compile, then run tests with unique log/probs files, splitting logs if >800 lines
const { spawn, execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

// 1. Delete all files in dist/
const distDir = path.join(__dirname, '..', 'dist');
if (fs.existsSync(distDir)) {
  for (const f of fs.readdirSync(distDir)) {
    fs.rmSync(path.join(distDir, f), { recursive: true, force: true });
  }
}

// 2. Compile TypeScript (always one-time build, never watch mode)
console.log('Compiling TypeScript...');
execSync('npx tsc', { stdio: 'inherit' });

// 3. Prepare log/probs filenames
const now = new Date();
const pad = n => n.toString().padStart(2, '0');
const datetime = `${now.getFullYear()}${pad(now.getMonth() + 1)}${pad(now.getDate())}_${pad(now.getHours())}${pad(now.getMinutes())}${pad(now.getSeconds())}`;
const logsDir = path.join(__dirname, 'logs');
if (!fs.existsSync(logsDir)) fs.mkdirSync(logsDir);
const base = `prologBackend.test.${datetime}`;
const logFile = path.join(logsDir, `${base}.log`);
const problemsFile = path.join(logsDir, `${base}.problems.json`);

// 4. Support selective test running via command-line argument
// Usage: node run-with-logs.cjs [grepPattern]
const grepPattern = process.argv.slice(2).join(' ').trim();

// Recursively find all .test.ts and .test.js files in the test directory (excluding node_modules, logs, temp, utils, validation, platform, resources, .vscode)
function findTestFiles(dir) {
  const skipDirs = new Set(['node_modules', 'logs', 'temp', 'utils', 'validation', 'platform', 'resources', '.vscode']);
  let results = [];
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    if (entry.isDirectory()) {
      if (!skipDirs.has(entry.name)) {
        results = results.concat(findTestFiles(path.join(dir, entry.name)));
      }
    } else if (entry.isFile() && /\.test\.(ts|js)$/.test(entry.name)) {
      results.push(path.join(dir, entry.name));
    }
  }
  return results;
}

const testDir = path.join(__dirname);
const testFiles = findTestFiles(testDir).sort();
let allLogLines = [];
let allProblems = [];
let allLogFiles = [];

function runTestFile(testFile, cb) {
  const mochaArgs = ['mocha', '--require', 'ts-node/register', testFile];
  if (grepPattern) {
    mochaArgs.push('--grep');
    mochaArgs.push(grepPattern);
  }
  const mocha = spawn('npx', mochaArgs, { shell: true });
  let logLines = [];
  mocha.stdout.on('data', data => {
    process.stdout.write(data);
    logLines.push(...data.toString().split(/\r?\n/));
  });
  mocha.stderr.on('data', data => {
    process.stderr.write(data);
    logLines.push(...data.toString().split(/\r?\n/));
  });
  mocha.on('close', code => {
    // Split log if >800 lines
    let logFiles = [];
    for (let i = 0; i < logLines.length; i += 800) {
      const chunk = logLines.slice(i, i + 800).join('\n');
      const chunkFile = logFiles.length === 0 ? logFile.replace('.log', `.${path.basename(testFile)}.log`) : logFile.replace('.log', `.${path.basename(testFile)}.${logFiles.length + 1}.log`);
      fs.writeFileSync(chunkFile, chunk, 'utf8');
      logFiles.push(path.basename(chunkFile));
    }
    // Parse problems from log
    const problems = [];
    let currentTest = null;
    logLines.forEach(line => {
      const match = line.match(/^\s*\d+\) (.+)$/);
      if (match) currentTest = match[1];
      const timeout = line.match(/Timeout of \d+ms exceeded/);
      if (timeout && currentTest) {
        problems.push({ test: currentTest, error: line.trim(), file: testFile });
        currentTest = null;
      }
      const assertion = line.match(/AssertionError: (.+)/);
      if (assertion && currentTest) {
        problems.push({ test: currentTest, error: assertion[1], file: testFile });
        currentTest = null;
      }
    });
    allLogLines = allLogLines.concat(logLines);
    allProblems = allProblems.concat(problems);
    allLogFiles = allLogFiles.concat(logFiles);
    cb(code);
  });
}

function runAllTests(i) {
  if (i >= testFiles.length) {
    // Write problems file
    fs.writeFileSync(problemsFile, JSON.stringify({ logFiles: allLogFiles, problems: allProblems }, null, 2), 'utf8');
    // Output summary to terminal
    if (allProblems.length) {
      console.log('\nTest Problems:');
      allProblems.forEach(p => {
        console.log(`- ${p.test}: ${p.error}`);
      });
      console.log(`\nSee logs: ${allLogFiles.join(', ')}`);
      console.log(`Problems file: ${path.basename(problemsFile)}`);
    } else {
      console.log('\nAll tests passed.');
    }
    process.exit(0);
    return;
  }
  runTestFile(testFiles[i], () => runAllTests(i + 1));
}

runAllTests(0);
