const { spawn } = require('child_process');
const path = require('path');

// Adjust these paths as needed for your environment
const swiplPath = 'swipl';
const serverPath = path.resolve(__dirname, 'src', 'prolog_json_server.pl').replace(/\\/g, '/');
const port = 3050;

const args = [
  '-q',
  '-f',
  'none',
  '-g',
  `consult('${serverPath}'), main(${port})`
];

console.log('Spawning SWI-Prolog with:', swiplPath, args.join(' '));

const proc = spawn(swiplPath, args, {
  stdio: ['ignore', 'pipe', 'pipe']
});

proc.stdout.on('data', data => {
  console.log('[STDOUT]', data.toString());
});

proc.stderr.on('data', data => {
  console.error('[STDERR]', data.toString());
});

proc.on('exit', (code, signal) => {
  console.log(`[EXIT] code=${code} signal=${signal}`);
});

// Optionally, test the HTTP server after a short delay
setTimeout(() => {
  const http = require('http');
  http.get(`http://localhost:${port}/`, res => {
    let body = '';
    res.on('data', chunk => body += chunk);
    res.on('end', () => {
      console.log('[HTTP RESPONSE]', body);
      proc.kill();
    });
  }).on('error', err => {
    console.error('[HTTP ERROR]', err);
    proc.kill();
  });
}, 2000);
