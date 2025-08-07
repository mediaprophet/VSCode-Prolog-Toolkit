"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var chai_1 = require("chai");
var path = require("path");
var prologBackend_js_1 = require("../src/prologBackend.js");
describe('PrologBackend PlDoc Test', function () {
    it('should return args and examples for a user-defined predicate (PlDoc)', function (done) {
        this.timeout(12000);
        var backend = new prologBackend_js_1.PrologBackend();
        var testFile = path.join(__dirname, 'resources', 'foo_with_pldoc.pl');
        var finished = false;
        function finish(err) {
            if (!finished) {
                finished = true;
                backend.stop();
                if (err) {
                    console.error('[PLDOC TEST] done() called with error:', err);
                    done(err);
                }
                else {
                    console.log('[PLDOC TEST] done() called with success');
                    done();
                }
            }
        }
        backend.on('started', function () {
            console.log('[PLDOC TEST] Backend started. About to consult file:', testFile);
            backend
                .sendRequest('query', { goal: "consult('".concat(testFile.replace(/\\/g, '/'), "')") })
                .then(function () {
                setTimeout(function () {
                    console.log('[PLDOC TEST] About to call getPredicateHelp for foo/2');
                    backend
                        .getPredicateHelp('foo/2')
                        .then(function (doc) {
                        console.log('[PLDOC TEST] getPredicateHelp returned:', doc);
                        if (doc.summary && doc.summary.includes('PlDoc missing')) {
                            console.warn('[PLDOC TEST] Skipping: PlDoc not available.');
                            finish(undefined);
                            return;
                        }
                        try {
                            chai_1.assert.isArray(doc.args);
                            chai_1.assert.isArray(doc.examples);
                            chai_1.assert.isAbove(doc.args.length, 0, 'args should not be empty');
                            chai_1.assert.isAbove(doc.examples.length, 0, 'examples should not be empty');
                            finish(undefined);
                        }
                        catch (err) {
                            finish(err);
                        }
                    })
                        .catch(finish);
                }, 200);
            })
                .catch(finish);
        });
        backend.start();
        setTimeout(function () {
            if (!finished) {
                console.error('[PLDOC TEST] Timeout reached, done() not called after 10s');
                finish(new Error('Timeout: done() not called after 10s'));
            }
        }, 10000);
    });
});
