import esbuild from 'esbuild';

await esbuild.build({
  entryPoints: ['src/extension.ts'],
  bundle: true,
  outfile: 'out/pub/extension.js',
  external: ['vscode'],
  format: 'esm',
  platform: 'node',
  target: 'node18',
  sourcemap: true
}).catch(() => process.exit(1));

console.log('✅ Build finished!');
