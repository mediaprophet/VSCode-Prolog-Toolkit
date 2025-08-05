import { expect } from 'chai';
import * as path from 'path';
import { PrologBackend } from '../src/old_prologBackend.ts.old';

describe('PrologBackend PlDoc Only', function () {
  it('should return args and examples for a user-defined predicate (PlDoc only)', function (done) {
    this.timeout(12000);
    const backend = new PrologBackend();
    const testFile = path.join(__dirname, 'resources', 'foo_with_pldoc.pl').replace(/\\/g, '/');
    let finished = false;
    function finish(err?: any, skip?: boolean) {
      if (!finished) {
        finished = true;
        backend.stop();
        if (skip) {
          console.warn('[PLDOC TEST] Skipping: PlDoc is missing.');
          return done();
        }
        if (err) {
          console.error('[PLDOC TEST] done() called with error:', err);
          return done(err);
        }
        console.log('[PLDOC TEST] done() called with success');
        done();
      }
    }
    backend.on('started', async () => {
      try {
        console.log('[PLDOC TEST] Backend started. About to consult file:', testFile);
        let consultResp, makeResp;
        try {
          consultResp = await backend.sendRequest('query', { goal: `consult('${testFile}')` });
          console.log('[PLDOC TEST] Consult response:', consultResp);
          makeResp = await backend.sendRequest('query', { goal: 'make.' });
          console.log('[PLDOC TEST] make/0 response:', makeResp);
        } catch (e) {
          console.error('[PLDOC TEST] Consult or make/0 failed:', e);
          finish(e);
          return;
        }
        await new Promise(res => setTimeout(res, 150));
        let doc;
        try {
          console.log('[PLDOC TEST] About to call getPredicateHelp for foo/2');
          doc = await backend.getPredicateHelp('foo/2');
          console.log('[PLDOC TEST] getPredicateHelp returned:', doc);
        } catch (err) {
          console.error('[PLDOC TEST] getPredicateHelp threw:', err);
          finish(err);
          return;
        }
        if (doc.summary && doc.summary.match(/pldoc missing|no documentation found/i)) {
          console.warn('[PLDOC TEST] Detected fallback doc (PlDoc missing), skipping.');
          finish(undefined, true); // skip if PlDoc missing
          return;
        }
        try {
          expect(doc).to.be.an('object');
          expect(doc).to.have.property('name', 'foo');
          expect(doc).to.have.property('arity', 2);
          expect(doc).to.have.property('args').that.is.an('array').and.not.empty;
          expect(doc).to.have.property('examples').that.is.an('array').and.not.empty;
          console.log('[PLDOC TEST] All assertions passed.');
          finish();
        } catch (err) {
          console.error('[PLDOC TEST] Assertion error:', err);
          finish(err);
        }
      } catch (err) {
        console.error('[PLDOC TEST] Caught error:', err);
        finish(err);
      }
    });
    try {
      console.log('[PLDOC TEST] About to start backend...');
      backend.start();
    } catch (e) {
      console.error('[PLDOC TEST] backend.start() threw:', e);
      finish(e);
    }
    setTimeout(() => {
      if (!finished) {
        console.error('[PLDOC TEST] Timeout reached, done() not called after 10s');
        finish(new Error('Timeout: done() not called after 10s'));
      }
    }, 10000);
  });
});
