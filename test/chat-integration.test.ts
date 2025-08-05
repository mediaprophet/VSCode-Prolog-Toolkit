import { expect } from 'chai';
import { PrologBackend } from '../src/prologBackend';

describe('Chat Integration Tests', function() {
  this.timeout(20000); // Increase timeout for backend startup
  
  let backend: PrologBackend;

  before(async function() {
    // Initialize backend for testing
    backend = new PrologBackend({
      swiplPath: 'swipl',
      port: 3061 // Use different port to avoid conflicts
    });

    // Start backend and wait for it to be ready
    return new Promise<void>((resolve, reject) => {
      const timeout = setTimeout(() => {
        reject(new Error('Backend startup timeout'));
      }, 15000);

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

  after(function() {
    if (backend) {
      backend.stop(true);
    }
  });

  it('should handle version command', async function() {
    const response = await backend.sendRequest('version');
    expect(response.status).to.equal('ok');
    expect(response.version).to.be.a('number');
  });

  it('should handle simple query command', async function() {
    const response = await backend.sendRequest('query', {
      goal: 'member(X, [1,2,3])',
      timeoutMs: 5000
    });
    expect(response.status).to.equal('ok');
    expect(response.results).to.be.an('array');
  });

  it('should handle help command', async function() {
    const response = await backend.sendRequest('help', {
      predicate: 'member/2',
      timeoutMs: 5000
    });
    expect(response.status).to.equal('ok');
    expect(response.doc).to.be.an('object');
    expect(response.doc.name).to.equal('member');
    expect(response.doc.arity).to.equal(2);
  });

  it('should handle consult command with error gracefully', async function() {
    const response = await backend.sendRequest('consult', {
      file: 'nonexistent.pl',
      timeoutMs: 5000
    });
    // Should return error status for non-existent file
    expect(response.status).to.equal('error');
  });

  it('should handle invalid query gracefully', async function() {
    const response = await backend.sendRequest('query', {
      goal: 'invalid_syntax(',
      timeoutMs: 5000
    });
    expect(response.status).to.equal('error');
  });
});