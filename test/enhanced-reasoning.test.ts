import assert from 'assert';
import { PrologBackend } from '../src/prologBackend.js';

describe('Enhanced Reasoning Features Tests', function () {
  this.timeout(30000);

  let backend: PrologBackend;
  const testPort = 3062; // Use different port for tests

  before(async function () {
    // Initialize backend for testing
    backend = new PrologBackend({
      swiplPath: 'swipl',
      port: testPort,
    });

    // Start backend and wait for it to be ready
    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        reject(new Error('Backend startup timeout'));
      }, 15000);

      backend.on('ready', () => {
        clearTimeout(timeout);
        resolve(undefined);
      });

      backend.on('error', error => {
        clearTimeout(timeout);
        reject(error);
      });

      backend.start();
    });
  });

  after(function () {
    if (backend) {
      backend.stop(true);
    }
  });

  describe('Constraint Logic Programming (CLP) Tests', function () {
    describe('CLP(FD) - Finite Domain Constraints', function () {
      it('should solve basic arithmetic constraints', async function () {
        const response = await backend.sendRequest('clp_solve', {
          domain: 'fd',
          variables: ['X', 'Y'],
          constraints: ['X #= Y + 1', 'X #< 10', 'Y #> 0'],
          timeoutMs: 10000,
        });

        assert.strictEqual(response.status, 'ok');
        assert.strictEqual(response.domain, 'fd');
        assert(Array.isArray(response.solution), 'Should return solution array');
        assert(response.solution.length === 2, 'Should have solutions for X and Y');
      });

      it('should solve all_different constraint', async function () {
        const response = await backend.sendRequest('clp_solve', {
          domain: 'fd',
          variables: ['A', 'B', 'C'],
          constraints: ['all_different([A, B, C])', 'A in 1..3', 'B in 1..3', 'C in 1..3'],
          timeoutMs: 10000,
        });

        assert.strictEqual(response.status, 'ok');
        assert(response.solution.length === 3, 'Should have solutions for A, B, C');
      });

      it('should handle unsatisfiable constraints', async function () {
        const response = await backend.sendRequest('clp_solve', {
          domain: 'fd',
          variables: ['X'],
          constraints: ['X #> 10', 'X #< 5'],
          timeoutMs: 5000,
        });

        // Should either fail gracefully or return error
        assert(
          response.status === 'error' ||
            (response.status === 'ok' && response.solution.length === 0)
        );
      });
    });

    describe('CLP(R) - Real Number Constraints', function () {
      it('should solve real number constraints', async function () {
        const response = await backend.sendRequest('clp_solve', {
          domain: 'r',
          variables: ['X', 'Y'],
          constraints: ['X =:= Y * 2.5', 'Y >= 1.0', 'X =< 10.0'],
          timeoutMs: 10000,
        });

        assert.strictEqual(response.status, 'ok');
        assert.strictEqual(response.domain, 'r');
        assert(response.solution.length === 2, 'Should have solutions for X and Y');
      });
    });

    describe('CLP Constraint Management', function () {
      it('should add and store constraints', async function () {
        const response = await backend.sendRequest('clp_constraint', {
          domain: 'fd',
          constraint: 'X #> 0',
          timeoutMs: 5000,
        });

        assert.strictEqual(response.status, 'ok');
        assert.strictEqual(response.domain, 'fd');
        assert(response.message.includes('successfully'), 'Should confirm constraint addition');
      });

      it('should validate constraint domains', async function () {
        const response = await backend.sendRequest('clp_constraint', {
          domain: 'invalid_domain',
          constraint: 'X #> 0',
          timeoutMs: 5000,
        });

        assert.strictEqual(response.status, 'error');
      });
    });
  });

  describe('Probabilistic Logic Tests', function () {
    beforeEach(async function () {
      // Add some probabilistic facts for testing
      await backend.sendRequest('probabilistic_fact', {
        fact: 'weather(sunny)',
        probability: 0.7,
        timeoutMs: 5000,
      });

      await backend.sendRequest('probabilistic_fact', {
        fact: 'weather(rainy)',
        probability: 0.3,
        timeoutMs: 5000,
      });
    });

    it('should add probabilistic facts', async function () {
      const response = await backend.sendRequest('probabilistic_fact', {
        fact: 'likes(john, pizza)',
        probability: 0.8,
        timeoutMs: 5000,
      });

      assert.strictEqual(response.status, 'ok');
      assert.strictEqual(response.probability, 0.8);
      assert(response.message.includes('successfully'), 'Should confirm fact addition');
    });

    it('should validate probability values', async function () {
      const response = await backend.sendRequest('probabilistic_fact', {
        fact: 'invalid_prob_fact',
        probability: 1.5, // Invalid probability > 1
        timeoutMs: 5000,
      });

      assert.strictEqual(response.status, 'error');
    });

    it('should perform probabilistic inference', async function () {
      const response = await backend.sendRequest('probabilistic_query', {
        goal: 'weather(sunny)',
        method: 'monte_carlo',
        samples: 100,
        timeoutMs: 10000,
      });

      assert.strictEqual(response.status, 'ok');
      assert(typeof response.probability === 'number', 'Should return probability');
      assert(
        response.probability >= 0 && response.probability <= 1,
        'Probability should be between 0 and 1'
      );
      assert.strictEqual(response.method, 'monte_carlo');
      assert.strictEqual(response.samples, 100);
      assert(response.evidence, 'Should provide evidence');
    });

    it('should handle complex probabilistic queries', async function () {
      // Add more complex probabilistic facts
      await backend.sendRequest('probabilistic_fact', {
        fact: 'traffic(heavy)',
        probability: 0.4,
        timeoutMs: 5000,
      });

      const response = await backend.sendRequest('probabilistic_query', {
        goal: 'traffic(heavy)',
        method: 'monte_carlo',
        samples: 500,
        timeoutMs: 10000,
      });

      assert.strictEqual(response.status, 'ok');
      assert(response.evidence.success_count >= 0, 'Should have success count');
      assert(response.evidence.total_samples === 500, 'Should match requested samples');
    });
  });

  describe('User-defined Logic Modules Tests', function () {
    it('should register a logic module', async function () {
      const rules = ['mortal(X) :- human(X)', 'human(socrates)', 'human(plato)'];

      const response = await backend.sendRequest('logic_module_register', {
        name: 'philosophy',
        rules: rules,
        meta_interpreter: 'default',
        timeoutMs: 10000,
      });

      assert.strictEqual(response.status, 'ok');
      assert.strictEqual(response.name, 'philosophy');
      assert.strictEqual(response.meta_interpreter, 'default');
      assert(response.message.includes('successfully'), 'Should confirm registration');
    });

    it('should query a registered logic module', async function () {
      // First register a module
      const rules = [
        'animal(X) :- mammal(X)',
        'mammal(dog)',
        'mammal(cat)',
        'bird(eagle)',
        'animal(X) :- bird(X)',
      ];

      await backend.sendRequest('logic_module_register', {
        name: 'animals',
        rules: rules,
        meta_interpreter: 'default',
        timeoutMs: 5000,
      });

      // Query the module
      const response = await backend.sendRequest('logic_module_query', {
        module: 'animals',
        goal: 'animal(X)',
        timeoutMs: 10000,
      });

      assert.strictEqual(response.status, 'ok');
      assert.strictEqual(response.module, 'animals');
      assert(Array.isArray(response.results), 'Should return results array');
      assert(response.count >= 0, 'Should have result count');
      assert(Array.isArray(response.proof_trace), 'Should return proof trace');
    });

    it('should list registered logic modules', async function () {
      // Register a couple of modules first
      await backend.sendRequest('logic_module_register', {
        name: 'test_module_1',
        rules: ['fact1(a)', 'fact2(b)'],
        meta_interpreter: 'default',
        timeoutMs: 5000,
      });

      await backend.sendRequest('logic_module_register', {
        name: 'test_module_2',
        rules: ['rule1(X) :- fact1(X)'],
        meta_interpreter: 'custom',
        timeoutMs: 5000,
      });

      const response = await backend.sendRequest('logic_module_list', {
        timeoutMs: 5000,
      });

      assert.strictEqual(response.status, 'ok');
      assert(Array.isArray(response.modules), 'Should return modules array');
      assert(response.count >= 2, 'Should have at least 2 modules');

      // Check module structure
      const module = response.modules.find((m: any) => m.name === 'test_module_1');
      assert(module, 'Should find test_module_1');
      assert(typeof module.rules_count === 'number', 'Should have rules count');
      assert(module.meta_interpreter, 'Should have meta interpreter');
    });

    it('should handle custom meta-interpreter', async function () {
      const rules = ['custom_rule(X) :- base_fact(X)', 'base_fact(test_value)'];

      const response = await backend.sendRequest('logic_module_register', {
        name: 'custom_meta_test',
        rules: rules,
        meta_interpreter: 'custom',
        timeoutMs: 10000,
      });

      assert.strictEqual(response.status, 'ok');
      assert.strictEqual(response.meta_interpreter, 'custom');
    });

    it('should validate module names', async function () {
      const response = await backend.sendRequest('logic_module_register', {
        name: 123, // Invalid name (not string/atom)
        rules: ['test_rule'],
        timeoutMs: 5000,
      });

      assert.strictEqual(response.status, 'error');
    });

    it('should handle queries to non-existent modules', async function () {
      const response = await backend.sendRequest('logic_module_query', {
        module: 'non_existent_module',
        goal: 'some_goal',
        timeoutMs: 5000,
      });

      assert.strictEqual(response.status, 'error');
    });
  });

  describe('Integration Tests', function () {
    it('should combine CLP with probabilistic reasoning', async function () {
      // Add probabilistic constraint
      await backend.sendRequest('probabilistic_fact', {
        fact: 'constraint_holds(X)',
        probability: 0.6,
        timeoutMs: 5000,
      });

      // This is a conceptual test - in practice, integration would be more complex
      const clpResponse = await backend.sendRequest('clp_solve', {
        domain: 'fd',
        variables: ['X'],
        constraints: ['X in 1..10'],
        timeoutMs: 5000,
      });

      const probResponse = await backend.sendRequest('probabilistic_query', {
        goal: 'constraint_holds(5)',
        method: 'monte_carlo',
        samples: 100,
        timeoutMs: 5000,
      });

      assert.strictEqual(clpResponse.status, 'ok');
      assert.strictEqual(probResponse.status, 'ok');
    });

    it('should use logic modules with N3 reasoning', async function () {
      // Register a module that could work with N3 data
      const rules = [
        'semantic_relation(X, Y) :- rdf(X, related_to, Y)',
        'inferred_fact(X) :- semantic_relation(X, _)',
      ];

      const response = await backend.sendRequest('logic_module_register', {
        name: 'semantic_reasoning',
        rules: rules,
        meta_interpreter: 'default',
        timeoutMs: 10000,
      });

      assert.strictEqual(response.status, 'ok');

      // Query the module
      const queryResponse = await backend.sendRequest('logic_module_query', {
        module: 'semantic_reasoning',
        goal: 'inferred_fact(X)',
        timeoutMs: 5000,
      });

      assert.strictEqual(queryResponse.status, 'ok');
    });
  });

  describe('Error Handling and Edge Cases', function () {
    it('should handle malformed CLP constraints', async function () {
      const response = await backend.sendRequest('clp_solve', {
        domain: 'fd',
        variables: ['X'],
        constraints: ['invalid constraint syntax'],
        timeoutMs: 5000,
      });

      assert.strictEqual(response.status, 'error');
    });

    it('should handle invalid probabilistic goals', async function () {
      const response = await backend.sendRequest('probabilistic_query', {
        goal: 'invalid goal syntax @#$',
        method: 'monte_carlo',
        samples: 100,
        timeoutMs: 5000,
      });

      assert.strictEqual(response.status, 'error');
    });

    it('should handle malformed logic module rules', async function () {
      const response = await backend.sendRequest('logic_module_register', {
        name: 'malformed_test',
        rules: ['invalid rule syntax @#$'],
        timeoutMs: 5000,
      });

      assert.strictEqual(response.status, 'error');
    });

    it('should handle missing required parameters', async function () {
      const responses = await Promise.all([
        backend.sendRequest('clp_solve', { domain: 'fd' }), // Missing variables and constraints
        backend.sendRequest('probabilistic_fact', { fact: 'test' }), // Missing probability
        backend.sendRequest('logic_module_register', { name: 'test' }), // Missing rules
      ]);

      responses.forEach(response => {
        assert.strictEqual(response.status, 'error');
      });
    });
  });
});
