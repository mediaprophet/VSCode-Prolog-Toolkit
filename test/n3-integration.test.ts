import * as assert from 'assert';
import * as path from 'path';
import { PrologBackend } from '../src/prologBackend';

describe('N3 Integration Tests', function() {
    this.timeout(30000);
    
    let backend: PrologBackend;
    const testPort = 3061; // Use different port for tests
    
    before(async function() {
        // Initialize backend for testing
        backend = new PrologBackend({
            swiplPath: 'swipl',
            port: testPort
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
            
            backend.on('error', (error) => {
                clearTimeout(timeout);
                reject(error);
            });
            
            backend.start();
        });
    });
    
    after(function() {
        if (backend) {
            backend.stop(true);
        }
    });
    
    describe('N3 Load Command', function() {
        it('should load N3 content successfully', async function() {
            const n3Content = `
                @prefix : <http://example.org/> .
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
                
                :socrates a :Person .
                :Person rdfs:subClassOf :Mortal .
                
                { ?x a :Person } => { ?x a :Mortal } .
            `;
            
            const response = await backend.sendRequest('n3_load', {
                content: n3Content,
                timeoutMs: 10000
            });
            
            assert.strictEqual(response.status, 'ok');
            assert(response.triples_count > 0, 'Should load some triples');
        });
        
        it('should load N3 file successfully', async function() {
            const filePath = path.join(__dirname, 'resources', 'sample.n3');
            
            const response = await backend.sendRequest('n3_load', {
                file: filePath,
                timeoutMs: 10000
            });
            
            assert.strictEqual(response.status, 'ok');
            assert(response.triples_count > 0, 'Should load some triples');
        });
        
        it('should handle invalid N3 content', async function() {
            const invalidContent = 'invalid n3 content @#$%';
            
            const response = await backend.sendRequest('n3_load', {
                content: invalidContent,
                timeoutMs: 5000
            });
            
            assert.strictEqual(response.status, 'error');
        });
    });
    
    describe('N3 List Command', function() {
        beforeEach(async function() {
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
                timeoutMs: 5000
            });
        });
        
        it('should list loaded triples', async function() {
            const response = await backend.sendRequest('n3_list', {
                limit: 10,
                format: 'readable',
                timeoutMs: 5000
            });
            
            assert.strictEqual(response.status, 'ok');
            assert(Array.isArray(response.triples), 'Should return array of triples');
            assert(response.total_count > 0, 'Should have some triples');
        });
        
        it('should respect limit parameter', async function() {
            const response = await backend.sendRequest('n3_list', {
                limit: 2,
                format: 'readable',
                timeoutMs: 5000
            });
            
            assert.strictEqual(response.status, 'ok');
            assert(response.triples.length <= 2, 'Should respect limit');
        });
    });
    
    describe('N3 Reason Command', function() {
        beforeEach(async function() {
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
                timeoutMs: 5000
            });
        });
        
        it('should perform general reasoning', async function() {
            const response = await backend.sendRequest('n3_reason', {
                goal: '',
                timeoutMs: 10000
            });
            
            assert.strictEqual(response.status, 'ok');
            assert(response.inferred_triples || response.results, 'Should return reasoning results');
        });
        
        it('should reason with specific goal', async function() {
            const response = await backend.sendRequest('n3_reason', {
                goal: 'rdf(socrates, type, Mortal)',
                timeoutMs: 10000
            });
            
            assert.strictEqual(response.status, 'ok');
        });
    });
    
    describe('N3 Explain Command', function() {
        beforeEach(async function() {
            // Load test data
            const n3Content = `
                @prefix : <http://example.org/> .
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
                
                :socrates a :Person .
                :Person rdfs:subClassOf :Mortal .
            `;
            
            await backend.sendRequest('n3_load', {
                content: n3Content,
                timeoutMs: 5000
            });
        });
        
        it('should generate proof explanation', async function() {
            const response = await backend.sendRequest('n3_explain', {
                goal: 'rdf(socrates, type, Person)',
                timeoutMs: 10000
            });
            
            assert.strictEqual(response.status, 'ok');
            assert(response.proof, 'Should return proof tree');
            assert.strictEqual(response.goal, 'rdf(socrates, type, Person)');
        });
        
        it('should handle goals that cannot be proven', async function() {
            const response = await backend.sendRequest('n3_explain', {
                goal: 'rdf(nonexistent, type, Something)',
                timeoutMs: 5000
            });
            
            // Should either succeed with empty proof or fail gracefully
            assert(response.status === 'ok' || response.status === 'error');
        });
    });
    
    describe('N3 Input Validation', function() {
        it('should validate file extensions', async function() {
            const response = await backend.sendRequest('n3_load', {
                file: 'invalid.txt',
                timeoutMs: 5000
            });
            
            assert.strictEqual(response.status, 'error');
        });
        
        it('should handle missing files', async function() {
            const response = await backend.sendRequest('n3_load', {
                file: 'nonexistent.n3',
                timeoutMs: 5000
            });
            
            assert.strictEqual(response.status, 'error');
        });
        
        it('should validate content size', async function() {
            const largeContent = 'a'.repeat(2000000); // 2MB content
            
            const response = await backend.sendRequest('n3_load', {
                content: largeContent,
                timeoutMs: 5000
            });
            
            assert.strictEqual(response.status, 'error');
        });
    });
    
    describe('N3 Prefix Formatting', function() {
        beforeEach(async function() {
            const n3Content = `
                @prefix : <http://example.org/> .
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
                
                :socrates a :Person .
            `;
            
            await backend.sendRequest('n3_load', {
                content: n3Content,
                timeoutMs: 5000
            });
        });
        
        it('should format URIs with readable prefixes', async function() {
            const response = await backend.sendRequest('n3_list', {
                limit: 5,
                format: 'readable',
                timeoutMs: 5000
            });
            
            assert.strictEqual(response.status, 'ok');
            
            // Check if any triple uses readable prefixes
            const hasReadablePrefix = response.triples.some((triple: any) => 
                triple.subject.startsWith(':') || 
                triple.predicate.startsWith('rdf:') ||
                triple.object.startsWith(':')
            );
            
            assert(hasReadablePrefix, 'Should use readable prefixes');
        });
    });
});