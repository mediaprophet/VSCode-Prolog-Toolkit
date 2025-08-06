import { expect } from 'chai';
import { PrologBackend } from '../../out/src/prologBackend.js';

describe('AI Copilot Prolog Support - Unit', function () {
  let backend: PrologBackend;

  beforeEach(async function () {
    backend = new PrologBackend();
    await backend.start();
  });

  afterEach(async function () {
    await backend.stop();
  });

  it('should sanitize and reject malformed input', async function () {
    try {
      await backend.sendRequest('query', { goal: 'member(X, [a,b,c' }); // missing closing ]
      throw new Error('Should have thrown');
    } catch (err: any) {
      expect(err.message).to.match(/syntax|parse/i);
    }
  });

  it('should enforce timeouts for long-running queries', async function () {
    this.timeout(3000);
    try {
      await backend.sendRequest('query', { goal: 'repeat,repeat,repeat,repeat,repeat,repeat,repeat,repeat,repeat,repeat', timeoutMs: 100 });
      throw new Error('Should have timed out');
    } catch (err: any) {
      expect(err.message).to.match(/timeout/i);
    }
  });

  it('should recover automatically if the Prolog process crashes', async function () {
    // Simulate crash
    await backend['prologProcess']?.kill();
    // Should restart on next query
    const result = await backend.sendRequest('query', { goal: 'member(X, [1,2])' });
    expect(result.success).to.be.true;
    expect(result.results[0]).to.have.property('X');
  });

  it('should queue and process multiple requests safely', async function () {
    const promises = [
      backend.sendRequest('query', { goal: 'member(X, [a,b])' }),
      backend.sendRequest('query', { goal: 'member(Y, [1,2])' })
    ];
    const results = await Promise.all(promises);
    expect(results[0].success).to.be.true;
    expect(results[1].success).to.be.true;
  });
});
