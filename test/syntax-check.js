// Diagnostic script to validate syntax errors in test file
const fs = require('fs');
const path = require('path');

console.log('[DIAGNOSTIC] Checking test file syntax...');

try {
  // Try to require the test file to check for syntax errors
  const testFile = path.join(__dirname, 'prologBackend.test.ts');
  const content = fs.readFileSync(testFile, 'utf8');
  
  console.log('[DIAGNOSTIC] File read successfully');
  console.log('[DIAGNOSTIC] File length:', content.length, 'characters');
  
  // Check for brace matching
  let braceCount = 0;
  let lineNum = 0;
  const lines = content.split('\n');
  
  for (const line of lines) {
    lineNum++;
    for (const char of line) {
      if (char === '{') braceCount++;
      if (char === '}') braceCount--;
    }
    
    // Log problematic lines
    if (lineNum >= 22 && lineNum <= 50) {
      console.log(`[DIAGNOSTIC] Line ${lineNum}: ${line.trim()} (brace count: ${braceCount})`);
    }
  }
  
  console.log('[DIAGNOSTIC] Final brace count:', braceCount);
  
  if (braceCount !== 0) {
    console.log('[DIAGNOSTIC] ❌ SYNTAX ERROR: Unmatched braces detected!');
  } else {
    console.log('[DIAGNOSTIC] ✅ Brace matching appears correct');
  }
  
} catch (error) {
  console.log('[DIAGNOSTIC] ❌ Error reading/parsing file:', error.message);
}