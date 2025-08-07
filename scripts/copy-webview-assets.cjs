// scripts/copy-webview-assets.cjs
// Copies webview-ui/build/assets/* to the root for packaging (or to out/ if needed)

const fs = require('fs');
const path = require('path');

const srcDir = path.join(__dirname, '..', 'webview-ui', 'build', 'assets');
const destDir = path.join(__dirname, '..', 'webview-ui', 'build', 'assets'); // Change if you want to copy elsewhere

if (!fs.existsSync(srcDir)) {
  console.error('Source directory does not exist:', srcDir);
  process.exit(1);
}

if (!fs.existsSync(destDir)) {
  fs.mkdirSync(destDir, { recursive: true });
}

fs.readdirSync(srcDir).forEach(file => {
  const srcFile = path.join(srcDir, file);
  const destFile = path.join(destDir, file);
  fs.copyFileSync(srcFile, destFile);
  console.log(`Copied ${srcFile} -> ${destFile}`);
});

console.log('Webview assets copied successfully.');
