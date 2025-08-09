
// VSCode Prolog Toolkit: esbuild build script
//
// CHECKLIST: When adding new entry points (LSP server, backend, debug adapter, webview, CLI, etc.),
// 1. Add each entry point to the build list below.
// 2. Ensure output paths match runtime import expectations.
// 3. Keep this list in sync with package.json and extension activation events.
//
// Example entry points:
//   - src/features/lsp/server.ts         → out/pub/features/lsp/server.js
//   - src/prologBackend.ts               → out/pub/prologBackend.js
//   - src/features/prologDebugger.ts     → out/pub/features/prologDebugger.js
//   - src/features/prologDebugSession.ts → out/pub/features/prologDebugSession.js
//   - src/extension.ts                   → out/pub/extension.js

// To automate: Add new entry points to the array below.

import fs from 'fs';
import path from 'path';

// Auto-discover all main entry points in src/ (excluding test, types, utils)
function discoverSrcEntryPoints() {
  const srcDir = 'src';
  const outDir = 'out/pub';
  const entries = [];
  function walk(dir) {
    for (const file of fs.readdirSync(dir)) {
      const full = path.join(dir, file);
      if (fs.statSync(full).isDirectory()) {
        // Skip test, types, utils, and node_modules
        if (/test|types|utils|node_modules|__mocks__|__tests__/i.test(file)) continue;
        walk(full);
      } else if (file.endsWith('.ts')) {
        // Exclude .d.ts and test files
        if (file.endsWith('.d.ts') || /test|spec|mock/i.test(file)) continue;
        // Output path mirrors src/ structure under out/pub/
        const rel = path.relative(srcDir, full);
        entries.push({
          src: full.replace(/\\/g, '/'),
          out: path.join(outDir, rel).replace(/\\/g, '/').replace(/\.ts$/, '.js'),
        });
      }
    }
  }
  walk(srcDir);
  return entries;
}

// Auto-discover all webview-ui/*.ts files
function discoverWebviewUiEntryPoints() {
  const webviewUiDir = 'webview-ui';
  const webviewUiOutDir = 'out/webview-ui';
  const entries = [];
  if (fs.existsSync(webviewUiDir)) {
    for (const file of fs.readdirSync(webviewUiDir)) {
      if (file.endsWith('.ts')) {
        entries.push({
          src: `${webviewUiDir}/${file}`,
          out: `${webviewUiOutDir}/${file.replace(/\.ts$/, '.js')}`,
        });
      }
    }
  }
  return entries;
}

const entryPoints = [
  ...discoverSrcEntryPoints(),
  ...discoverWebviewUiEntryPoints(),
];



import esbuild from 'esbuild';
import { copyDirSync } from './scripts/copyDirSync.js';


const commonOptions = {
  bundle: false, // Do not bundle, preserve import structure
  platform: 'node',
  sourcemap: true,
  target: 'es2022',
  format: 'cjs',
  logLevel: 'info',
};


// Build all entry points
for (const { src, out } of entryPoints) {
  await esbuild.build({
    entryPoints: [src],
    outfile: out,
    ...commonOptions,
  });
}


// Copy media folder to out/pub/media after build
const srcMedia = path.resolve('media');
const destMedia = path.resolve('out/pub/media');
copyDirSync(srcMedia, destMedia);


// Copy only static assets (not .ts files) from webview-ui to out/webview-ui
const srcWebviewUI = path.resolve('webview-ui');
const destWebviewUI = path.resolve('out/webview-ui');
if (fs.existsSync(srcWebviewUI)) {
  fs.mkdirSync(destWebviewUI, { recursive: true });
  for (const file of fs.readdirSync(srcWebviewUI)) {
    if (!file.endsWith('.ts')) {
      fs.copyFileSync(path.join(srcWebviewUI, file), path.join(destWebviewUI, file));
    }
  }
}
