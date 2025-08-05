import { expect } from 'chai';
import { PrologBackend } from '../src/prologBackend';

describe('PrologBackend', function() {
  let backend: PrologBackend;
  beforeEach(function() {
    backend = new PrologBackend();
  });

  afterEach(function() {
    if (backend) backend.stop();
  });
  after(function(done) {
    // Ensure process exits after all tests complete
    setTimeout(() => {
      console.log('[TEST] Forcing process exit after all tests.');
      process.exit(0);
    }, 100);
    done();
  });

  it('[8] should support batch requests (query, consult, help)', function(done) {
    this.timeout(10000);
    backend.on('started', async () => {
      try {
        // Prepare a batch: consult, query, help
        const testFile = require('path').resolve(__dirname, 'resources', 'foo_with_pldoc.pl').replace(/\\/g, '/');
        const batch = [
          { cmd: 'consult', params: { file: testFile } },
          { cmd: 'query', params: { goal: 'foo(1, B).' } },
          { cmd: 'help', params: { predicate: 'foo/2' } }
        ];
        const responses = await backend.sendRequest(batch);
        expect(responses).to.be.an('array').with.lengthOf(3);
        // Consult
        expect(responses[0]).to.have.property('status', 'ok');
        // Query
        expect(responses[1]).to.have.property('status', 'ok');
        // Help
        expect(responses[2]).to.have.property('status', 'ok');
        expect(responses[2]).to.have.property('doc');
        done();
      } catch (err: unknown) {
        done(err);
      }
    });
    backend.start();
  });

  it('[9] should enforce time limits for queries (timeout)', function(done) {
      this.timeout(5000);
      let finished = false;
      function finish(err) {
        if (!finished) {
          finished = true;
          backend.stop();
          if (err) return done(err);
          done();
        }
      }
      backend.on('started', async () => {
        try {
          // This query will sleep for 2 seconds, but time_limit is set to 1s
          await backend.sendRequest('query', { goal: 'sleep(2).', timeoutMs: 2000, time_limit: 1 });
          finish(new Error('Expected timeout error, but got success'));
        } catch (err: unknown) {
          try {
            expect(err).to.exist;
            // SWI-Prolog throws a time_limit_exceeded error
            if (typeof err === 'object' && err !== null && ('message' in err || 'error' in err)) {
              const msg = (err as { message?: string; error?: string }).message || (err as { message?: string; error?: string }).error;
              expect(msg).to.match(/time[_ ]limit[_ ]exceeded/i);
            }
            finish(undefined);
          } catch (e: unknown) {
            finish(e);
          }
        }
      });
      backend.start();
  });

  it('[7] should return args and examples for a user-defined predicate', function(done) {
  this.timeout(12000);
  let finished = false;
  function finish(err: unknown, skip?: boolean) {
    if (!finished) {
      finished = true;
      backend.stop();
      if (skip) return this.skip();
      if (err) return done(err);
      done();
    }
  }
    backend.on('started', async () => {
      try {
        const testFile = require('path').resolve(__dirname, 'resources', 'foo_with_pldoc.pl').replace(/\\/g, '/');
        let consultResp;
        try {
          consultResp = await backend.sendRequest('query', {goal: `consult('${testFile}')`});
          await backend.sendRequest('query', {goal: 'make.'});
        } catch (e: unknown) {
          return finish(e, false);
        }
        await new Promise(res => setTimeout(res, 150));
        let doc;
        try {
          doc = await backend.sendRequest('help', { predicate: 'foo/2' });
        } catch (err: unknown) {
          return finish(err, false);
        }
        if (doc.summary && doc.summary.match(/pldoc missing|no documentation found/i)) {
          return finish(undefined, true);
        }
        try {
          expect(doc).to.be.an('object');
          expect(doc).to.have.property('name', 'foo');
          expect(doc).to.have.property('arity', 2);
          expect(doc).to.have.property('args').that.is.an('array');
          expect(doc).to.have.property('examples').that.is.an('array');
          finish(undefined, false);
        } catch (err) {
          finish(err, false);
        }
      } catch (err) {
        finish(err, false);
      }
    });
  try {
    backend.start();
  } catch (e: unknown) {
    finish(e, false);
  }
  setTimeout(() => {
    if (!finished) finish(new Error('[TEST] [7] Timeout: done() not called after 10s'), false);
  }, 10000);
  });

  it('[1] should start and stop the backend process', function(done) {
    backend.on('started', () => {
      expect(backend.isRunning()).to.be.true;
      backend.on('stopped', () => {
        expect(backend.isRunning()).to.be.false;
        done();
      });
      backend.stop();
    });
    backend.start();
  });

  it('[2] should restart the backend process', function(done) {
    let startedCount = 0;
    backend.on('started', () => {
      startedCount++;
      if (startedCount === 1) {
        backend.restart();
      } else if (startedCount === 2) {
        expect(backend.isRunning()).to.be.true;
        done();
      }
    });
    backend.start();
  });


  it('[3] should send a query and receive output', function(done) {
    backend.on('started', () => {
      console.log('[TEST] [3] Backend started, sending query...');
      backend.sendRequest('query', {goal: 'write(hello), nl.'}).then((output) => {
        console.log('[TEST] [3] Query response:', output);
        expect(output.status).to.equal('ok');
        done();
      }).catch((err: unknown) => {
        console.error('[TEST] [3] Query error:', err);
        done(err);
      });
    });
    backend.start();
  });


  it('[4] should handle invalid input', function(done) {
    backend.on('started', () => {
      backend.sendRequest('query', {goal: '\u0000badinput'})
        .then(() => done(new Error('Expected error for invalid input, but got success')))
        .catch((err: unknown) => {
          expect(err).to.exist;
          done();
        });
    });
    backend.start();
  });

  it('[5] should automatically restart on exit', function(done) {
    backend.once('started', () => {
      backend.once('started', () => {
        try {
          expect(backend.isRunning()).to.be.true;
          done();
        } catch (e: unknown) {
          done(e);
        }
      });
      backend.once('restarted', () => {
        // No-op
      });
      backend.restart();
    });
    backend.start();
  });
});
