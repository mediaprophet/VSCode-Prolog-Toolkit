
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

// Main extension/server entry points
const entryPoints = [
  { src: 'src/features/lsp/server.ts', out: 'out/pub/features/lsp/server.js' },
  { src: 'src/prologBackend.ts', out: 'out/pub/prologBackend.js' },
  { src: 'src/features/prologDebugger.ts', out: 'out/pub/features/prologDebugger.js' },
  { src: 'src/features/prologDebugSession.ts', out: 'out/pub/features/prologDebugSession.js' },
  { src: 'src/extension.ts', out: 'out/pub/extension.js' },
];

// Add all webview-ui/*.ts files as entry points (output as .js in out/webview-ui/)
const webviewUiDir = 'webview-ui';
const webviewUiOutDir = 'out/webview-ui';
if (fs.existsSync(webviewUiDir)) {
  for (const file of fs.readdirSync(webviewUiDir)) {
    if (file.endsWith('.ts')) {
      entryPoints.push({
        src: `${webviewUiDir}/${file}`,
        out: `${webviewUiOutDir}/${file.replace(/\.ts$/, '.js')}`,
      });
    }
  }
}


import esbuild from 'esbuild';
import path from 'path';
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
