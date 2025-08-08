
// Copy prolog_json_server.pl to out/pub after build
import { copyFileSync, existsSync, mkdirSync, readdirSync, statSync } from 'fs';
import { dirname, join } from 'path';

const src = join('src', 'prolog_json_server.pl');
const dest = join('out', 'pub', 'prolog_json_server.pl');

function copyDirRecursive(srcDir, destDir) {
  mkdirSync(destDir, { recursive: true });
  for (const entry of readdirSync(srcDir)) {
    const srcPath = join(srcDir, entry);
    const destPath = join(destDir, entry);
    if (statSync(srcPath).isDirectory()) {
      copyDirRecursive(srcPath, destPath);
    } else {
      copyFileSync(srcPath, destPath);
    }
  }
}

if (!existsSync(src)) {
  console.error(`[esbuild-copy] Source file not found: ${src}`);
  process.exit(1);
}

mkdirSync(dirname(dest), { recursive: true });
copyFileSync(src, dest);
console.log(`[esbuild-copy] Copied ${src} -> ${dest}`);

// Copy prolog-resources directory if it exists
const resourcesSrc = join('prolog-resources');
const resourcesDest = join('out', 'pub', 'prolog-resources');
if (existsSync(resourcesSrc)) {
  copyDirRecursive(resourcesSrc, resourcesDest);
  console.log(`[esbuild-copy] Copied ${resourcesSrc} -> ${resourcesDest}`);
}

// Copy mcp-server/dist to out/pub/mcp-server after build
const mcpServerDistSrc = join('mcp-server', 'dist');
const mcpServerDistDest = join('out', 'pub', 'mcp-server');
if (existsSync(mcpServerDistSrc)) {
  copyDirRecursive(mcpServerDistSrc, mcpServerDistDest);
  console.log(`[esbuild-copy] Copied ${mcpServerDistSrc} -> ${mcpServerDistDest}`);
}
