// Diagnostic script to validate test structure and ordering
const fs = require('fs');
const path = require('path');

console.log('[DIAGNOSTIC] Checking test structure and ordering...');

try {
  const testFile = path.join(__dirname, 'prologBackend.test.ts');
  const content = fs.readFileSync(testFile, 'utf8');
  
  // Extract test cases and their order
  const testMatches = content.match(/it\('\[(\d+)\][^']*'/g);
  
  if (testMatches) {
    console.log('[DIAGNOSTIC] Found test cases:');
    const testNumbers = [];
    
    testMatches.forEach((match, index) => {
      const numberMatch = match.match(/\[(\d+)\]/);
      if (numberMatch) {
        const testNum = parseInt(numberMatch[1]);
        testNumbers.push(testNum);
        console.log(`  ${index + 1}. Test [${testNum}]: ${match}`);
      }
    });
    
    console.log('\n[DIAGNOSTIC] Test execution order:', testNumbers.join(' -> '));
    
    // Check if tests are in logical order
    const sortedNumbers = [...testNumbers].sort((a, b) => a - b);
    const isOrdered = JSON.stringify(testNumbers) === JSON.stringify(sortedNumbers);
    
    if (isOrdered) {
      console.log('[DIAGNOSTIC] ✅ Tests are in sequential order');
    } else {
      console.log('[DIAGNOSTIC] ⚠️  Tests are NOT in sequential order');
      console.log('[DIAGNOSTIC] Expected order:', sortedNumbers.join(' -> '));
    }
    
    // Check for missing test numbers
    const expectedRange = Array.from({length: Math.max(...testNumbers)}, (_, i) => i + 1);
    const missing = expectedRange.filter(num => !testNumbers.includes(num));
    
    if (missing.length > 0) {
      console.log('[DIAGNOSTIC] ⚠️  Missing test numbers:', missing.join(', '));
    } else {
      console.log('[DIAGNOSTIC] ✅ No missing test numbers');
    }
    
  } else {
    console.log('[DIAGNOSTIC] ❌ No test cases found');
  }
  
  // Check describe block structure
  const describeCount = (content.match(/describe\(/g) || []).length;
  const describeEndCount = (content.match(/}\);[\s]*$/gm) || []).length;
  
  console.log(`\n[DIAGNOSTIC] Describe blocks: ${describeCount}`);
  console.log(`[DIAGNOSTIC] Describe block endings: ${describeEndCount}`);
  
  if (describeCount === 1 && describeEndCount >= 1) {
    console.log('[DIAGNOSTIC] ✅ Test structure appears correct');
  } else {
    console.log('[DIAGNOSTIC] ⚠️  Test structure may have issues');
  }
  
} catch (error) {
  console.log('[DIAGNOSTIC] ❌ Error analyzing structure:', error.message);
}