import { assert } from 'chai';
import * as path from 'path';
import { PrologBackend } from '../src/old_prologBackend.ts.old';

describe('PrologBackend PlDoc Test', function () {
    it('should return args and examples for a user-defined predicate (PlDoc)', function (done) {
        this.timeout(12000);
        const backend = new PrologBackend();
        const testFile = path.join(__dirname, 'resources', 'foo_with_pldoc.pl');
        let finished = false;
        function finish(err) {
            if (!finished) {
                finished = true;
                backend.stop();
                if (err) {
                    console.error('[PLDOC TEST] done() called with error:', err);
                    done(err);
                } else {
                    console.log('[PLDOC TEST] done() called with success');
                    done();
                }
            }
        }
        backend.on('started', () => {
            console.log('[PLDOC TEST] Backend started. About to consult file:', testFile);
            backend.sendRequest('query', { goal: `consult('${testFile.replace(/\\/g, '/')}')` })
                .then(() => {
                    setTimeout(() => {
                        console.log('[PLDOC TEST] About to call getPredicateHelp for foo/2');
                        backend.getPredicateHelp('foo/2').then(doc => {
                            console.log('[PLDOC TEST] getPredicateHelp returned:', doc);
                            if (doc.summary && doc.summary.includes('PlDoc missing')) {
                                console.warn('[PLDOC TEST] Skipping: PlDoc not available.');
                                finish(undefined);
                                return;
                            }
                            try {
                                assert.isArray(doc.args);
                                assert.isArray(doc.examples);
                                assert.isAbove(doc.args.length, 0, 'args should not be empty');
                                assert.isAbove(doc.examples.length, 0, 'examples should not be empty');
                                finish(undefined);
                            } catch (err) {
                                finish(err);
                            }
                        }).catch(finish);
                    }, 200);
                })
                .catch(finish);
        });
        backend.start();
        setTimeout(() => {
            if (!finished) {
                console.error('[PLDOC TEST] Timeout reached, done() not called after 10s');
                finish(new Error('Timeout: done() not called after 10s'));
            }
        }, 10000);
    });
});
