import { expect } from 'chai';
import * as fs from 'fs';
import * as path from 'path';
import { PrologBackend } from '../src/prologBackend.js';

describe('Chat Commands Integration Tests', function () {
  this.timeout(30000); // Extended timeout for comprehensive testing

  let backend: PrologBackend;
  const testPort = 3062; // Use unique port for this test suite

  before(async function () {
    // Initialize backend for testing
    backend = new PrologBackend({
      swiplPath: 'swipl',
      port: testPort,
    });

    // Start backend and wait for it to be ready
    return new Promise<void>((resolve, reject) => {
      const timeout = setTimeout(() => {
        reject(new Error('Backend startup timeout'));
      }, 20000);

      const onReady = () => {
        clearTimeout(timeout);
        backend.off('ready', onReady);
        backend.off('error', onError);
        backend.off('started', onStarted);
        resolve();
      };

      const onStarted = () => {
        clearTimeout(timeout);
        backend.off('ready', onReady);
        backend.off('error', onError);
        backend.off('started', onStarted);
        resolve();
      };

      const onError = (error: any) => {
        clearTimeout(timeout);
        backend.off('ready', onReady);
        backend.off('error', onError);
        backend.off('started', onStarted);
        reject(error);
      };

      backend.on('ready', onReady);
      backend.on('started', onStarted);
      backend.on('error', onError);

      backend.start();
    });
  });

  after(function () {
    if (backend) {
      backend.stop(true);
    }
  });

  describe('/query command', function () {
    it('should execute simple arithmetic queries', async function () {
      const response = await backend.sendRequest('query', {
        goal: 'X is 2 + 3',
        timeoutMs: 5000,
      });

      expect(response.status).to.equal('ok');
      expect(response.results).to.be.an('array');
      expect(response.results.length).to.be.greaterThan(0);
      expect(response.results[0]).to.have.property('X', 5);
    });

    it('should execute list membership queries', async function () {
      const response = await backend.sendRequest('query', {
        goal: 'member(X, [a, b, c])',
        timeoutMs: 5000,
      });

      expect(response.status).to.equal('ok');
      expect(response.results).to.be.an('array');
      expect(response.results.length).to.equal(3);
    });

    it('should handle queries with no solutions', async function () {
      const response = await backend.sendRequest('query', {
        goal: 'member(z, [a, b, c])',
        timeoutMs: 5000,
      });

      expect(response.status).to.equal('ok');
      expect(response.results).to.be.an('array');
      expect(response.results.length).to.equal(0);
    });

    it('should handle syntax errors gracefully', async function () {
      const response = await backend.sendRequest('query', {
        goal: 'invalid_syntax(',
        timeoutMs: 5000,
      });

      expect(response.status).to.equal('error');
      expect(response.error).to.be.a('string');
    });

    it('should handle timeout for long-running queries', async function () {
      try {
        await backend.sendRequest('query', {
          goal: 'sleep(10)',
          timeoutMs: 2000,
        });
        expect.fail('Should have timed out');
      } catch (error) {
        expect(error).to.exist;
      }
    });
  });

  describe('/consult command', function () {
    it('should consult existing Prolog file', async function () {
      const testFile = path.join(__dirname, 'resources', 'foo_with_pldoc.pl');

      const response = await backend.sendRequest('consult', {
        file: testFile,
        timeoutMs: 10000,
      });

      expect(response.status).to.equal('ok');
    });

    it('should handle non-existent files gracefully', async function () {
      const response = await backend.sendRequest('consult', {
        file: 'nonexistent.pl',
        timeoutMs: 5000,
      });

      expect(response.status).to.equal('error');
      expect(response.error).to.be.a('string');
    });

    it('should handle files with syntax errors', async function () {
      // Create a temporary file with syntax errors
      const tempFile = path.join(__dirname, 'temp', 'syntax_error.pl');
      const tempDir = path.dirname(tempFile);

      if (!fs.existsSync(tempDir)) {
        fs.mkdirSync(tempDir, { recursive: true });
      }

      fs.writeFileSync(tempFile, 'invalid_syntax(.\n');

      try {
        const response = await backend.sendRequest('consult', {
          file: tempFile,
          timeoutMs: 5000,
        });

        expect(response.status).to.equal('error');
        expect(response.error).to.be.a('string');
      } finally {
        // Clean up
        if (fs.existsSync(tempFile)) {
          fs.unlinkSync(tempFile);
        }
      }
    });
  });

  describe('/help command', function () {
    it('should provide help for built-in predicates', async function () {
      const response = await backend.sendRequest('help', {
        predicate: 'member/2',
        timeoutMs: 10000,
      });

      expect(response.status).to.equal('ok');
      expect(response.doc).to.be.an('object');
      expect(response.doc.name).to.equal('member');
      expect(response.doc.arity).to.equal(2);
    });

    it('should provide help for user-defined predicates', async function () {
      // First consult the test file
      const testFile = path.join(__dirname, 'resources', 'foo_with_pldoc.pl');
      await backend.sendRequest('consult', { file: testFile, timeoutMs: 5000 });

      const response = await backend.sendRequest('help', {
        predicate: 'foo/2',
        timeoutMs: 10000,
      });

      expect(response.status).to.equal('ok');
      expect(response.doc).to.be.an('object');
      expect(response.doc.name).to.equal('foo');
      expect(response.doc.arity).to.equal(2);
    });

    it('should handle non-existent predicates', async function () {
      const response = await backend.sendRequest('help', {
        predicate: 'nonexistent_predicate/99',
        timeoutMs: 5000,
      });

      expect(response.status).to.equal('error');
    });

    it('should handle malformed predicate indicators', async function () {
      const response = await backend.sendRequest('help', {
        predicate: 'invalid_format',
        timeoutMs: 5000,
      });

      expect(response.status).to.equal('error');
    });
  });

  describe('/status command', function () {
    it('should return backend status information', async function () {
      const response = await backend.sendRequest('status', {
        timeoutMs: 5000,
      });

      expect(response.status).to.equal('ok');
      expect(response).to.have.property('backend_status');
      expect(response).to.have.property('version');
      expect(response).to.have.property('uptime');
    });
  });

  describe('N3 Commands', function () {
    describe('/n3_load command', function () {
      it('should load N3 content from string', async function () {
        const n3Content = `
                    @prefix : <http://example.org/> .
                    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                    
                    :socrates a :Person .
                    :plato a :Person .
                `;

        const response = await backend.sendRequest('n3_load', {
          content: n3Content,
          timeoutMs: 10000,
        });

        expect(response.status).to.equal('ok');
        expect(response.triples_count).to.be.greaterThan(0);
      });

      it('should load N3 content from file', async function () {
        const n3File = path.join(__dirname, 'resources', 'sample.n3');

        const response = await backend.sendRequest('n3_load', {
          file: n3File,
          timeoutMs: 10000,
        });

        expect(response.status).to.equal('ok');
        expect(response.triples_count).to.be.greaterThan(0);
      });

      it('should handle invalid N3 syntax', async function () {
        const invalidN3 = 'invalid n3 syntax @#$%';

        const response = await backend.sendRequest('n3_load', {
          content: invalidN3,
          timeoutMs: 5000,
        });

        expect(response.status).to.equal('error');
      });

      it('should validate file extensions', async function () {
        const response = await backend.sendRequest('n3_load', {
          file: 'invalid.txt',
          timeoutMs: 5000,
        });

        expect(response.status).to.equal('error');
      });
    });

    describe('/n3_list command', function () {
      beforeEach(async function () {
        // Load test data
        const n3Content = `
                    @prefix : <http://example.org/> .
                    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                    
                    :socrates a :Person .
                    :plato a :Person .
                    :aristotle a :Person .
                `;

        await backend.sendRequest('n3_load', {
          content: n3Content,
          timeoutMs: 5000,
        });
      });

      it('should list loaded triples', async function () {
        const response = await backend.sendRequest('n3_list', {
          limit: 10,
          format: 'readable',
          timeoutMs: 5000,
        });

        expect(response.status).to.equal('ok');
        expect(response.triples).to.be.an('array');
        expect(response.total_count).to.be.greaterThan(0);
      });

      it('should respect limit parameter', async function () {
        const response = await backend.sendRequest('n3_list', {
          limit: 2,
          format: 'readable',
          timeoutMs: 5000,
        });

        expect(response.status).to.equal('ok');
        expect(response.triples.length).to.be.at.most(2);
      });

      it('should support different output formats', async function () {
        const response = await backend.sendRequest('n3_list', {
          limit: 5,
          format: 'turtle',
          timeoutMs: 5000,
        });

        expect(response.status).to.equal('ok');
        expect(response.triples).to.be.an('array');
      });
    });

    describe('/n3_reason command', function () {
      beforeEach(async function () {
        // Load test data with reasoning rules
        const n3Content = `
                    @prefix : <http://example.org/> .
                    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
                    
                    :socrates a :Person .
                    :Person rdfs:subClassOf :Mortal .
                    
                    { ?x a :Person } => { ?x a :Mortal } .
                `;

        await backend.sendRequest('n3_load', {
          content: n3Content,
          timeoutMs: 5000,
        });
      });

      it('should perform general reasoning', async function () {
        const response = await backend.sendRequest('n3_reason', {
          goal: '',
          timeoutMs: 15000,
        });

        expect(response.status).to.equal('ok');
        expect(response.inferred_triples || response.results).to.exist;
      });

      it('should reason with specific goals', async function () {
        const response = await backend.sendRequest('n3_reason', {
          goal: 'rdf(socrates, type, Mortal)',
          timeoutMs: 10000,
        });

        expect(response.status).to.equal('ok');
      });
    });

    describe('/n3_explain command', function () {
      beforeEach(async function () {
        // Load test data
        const n3Content = `
                    @prefix : <http://example.org/> .
                    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                    
                    :socrates a :Person .
                `;

        await backend.sendRequest('n3_load', {
          content: n3Content,
          timeoutMs: 5000,
        });
      });

      it('should generate proof explanations', async function () {
        const response = await backend.sendRequest('n3_explain', {
          goal: 'rdf(socrates, type, Person)',
          timeoutMs: 10000,
        });

        expect(response.status).to.equal('ok');
        expect(response.proof).to.exist;
        expect(response.goal).to.equal('rdf(socrates, type, Person)');
      });

      it('should handle unprovable goals', async function () {
        const response = await backend.sendRequest('n3_explain', {
          goal: 'rdf(nonexistent, type, Something)',
          timeoutMs: 5000,
        });

        // Should either succeed with empty proof or fail gracefully
        expect(['ok', 'error']).to.include(response.status);
      });
    });
  });

  describe('Batch Operations', function () {
    it('should handle batch requests with mixed commands', async function () {
      const testFile = path.join(__dirname, 'resources', 'foo_with_pldoc.pl');

      const batch = [
        { cmd: 'consult', params: { file: testFile } },
        { cmd: 'query', params: { goal: 'foo(1, X)' } },
        { cmd: 'help', params: { predicate: 'foo/2' } },
      ];

      const responses = await backend.sendRequest(batch);

      expect(responses).to.be.an('array').with.lengthOf(3);
      expect(responses[0].status).to.equal('ok'); // consult
      expect(responses[1].status).to.equal('ok'); // query
      expect(responses[2].status).to.equal('ok'); // help
    });

    it('should handle batch requests with errors', async function () {
      const batch = [
        { cmd: 'query', params: { goal: 'X is 1 + 1' } },
        { cmd: 'query', params: { goal: 'invalid_syntax(' } },
        { cmd: 'query', params: { goal: 'Y is 2 + 2' } },
      ];

      const responses = await backend.sendRequest(batch);

      expect(responses).to.be.an('array').with.lengthOf(3);
      expect(responses[0].status).to.equal('ok');
      expect(responses[1].status).to.equal('error');
      expect(responses[2].status).to.equal('ok');
    });
  });

  describe('Error Handling and Edge Cases', function () {
    it('should handle empty requests', async function () {
      try {
        await backend.sendRequest('', {});
        expect.fail('Should have thrown an error');
      } catch (error) {
        expect(error).to.exist;
      }
    });

    it('should handle invalid command types', async function () {
      try {
        await backend.sendRequest('invalid_command', {});
        expect.fail('Should have thrown an error');
      } catch (error) {
        expect(error).to.exist;
      }
    });

    it('should handle requests with missing parameters', async function () {
      const response = await backend.sendRequest('query', {});
      expect(response.status).to.equal('error');
    });

    it('should handle very large query results', async function () {
      const response = await backend.sendRequest('query', {
        goal: 'between(1, 1000, X)',
        timeoutMs: 10000,
      });

      expect(response.status).to.equal('ok');
      expect(response.results).to.be.an('array');
      expect(response.results.length).to.equal(1000);
    });
  });
});
