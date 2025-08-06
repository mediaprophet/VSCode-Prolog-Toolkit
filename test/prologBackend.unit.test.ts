import * as axios from 'axios';
import { expect } from 'chai';
import { ChildProcess } from 'child_process';
import * as sinon from 'sinon';
import { PrologBackend } from '../src/prologBackend.js';

describe('PrologBackend Unit Tests (Mocked)', function () {
  let backend: PrologBackend;
  let mockChildProcess: sinon.SinonStubbedInstance<ChildProcess>;
  let axiosStub: sinon.SinonStub;

  beforeEach(function () {
    // Create a mock child process
    mockChildProcess = {
      pid: 12345,
      kill: sinon.stub(),
      on: sinon.stub(),
      stdout: {
        on: sinon.stub(),
        pipe: sinon.stub()
      },
      stderr: {
        on: sinon.stub(),
        pipe: sinon.stub()
      },
      stdin: {
        write: sinon.stub(),
        end: sinon.stub()
      }
    } as any;

    // Stub axios for HTTP requests
    axiosStub = sinon.stub(axios, 'default');

    // Create backend instance
    backend = new PrologBackend({
      swiplPath: 'swipl',
      port: 3063
    });

    // Replace the spawn method to return our mock
    sinon.stub(require('child_process'), 'spawn').returns(mockChildProcess);
  });

  afterEach(function () {
    sinon.restore();
    if (backend) {
      backend.stop(true);
    }
  });

  describe('Backend Lifecycle', function () {
    it('should initialize with default configuration', function () {
      const defaultBackend = new PrologBackend();
      expect(defaultBackend).to.be.instanceOf(PrologBackend);
    });

    it('should initialize with custom configuration', function () {
      const config = {
        swiplPath: '/custom/path/swipl',
        port: 9999,
        args: ['--quiet']
      };

      const customBackend = new PrologBackend(config);
      expect(customBackend).to.be.instanceOf(PrologBackend);
    });

    it('should start the backend process', function (done) {
      backend.on('started', () => {
        expect(mockChildProcess.on).to.have.been.called;
        done();
      });

      backend.start();

      // Simulate process started
      const startedCallback = mockChildProcess.on.getCall(0).args[1];
      startedCallback();
    });

    it('should handle process exit and restart', function (done) {
      let restartCount = 0;

      backend.on('started', () => {
        restartCount++;
        if (restartCount === 2) {
          done(); // Second start means restart worked
        }
      });

      backend.start();

      // Simulate first start
      const startedCallback = mockChildProcess.on.getCall(0).args[1];
      startedCallback();

      // Simulate process exit
      const exitCallback = mockChildProcess.on.getCall(1).args[1];
      exitCallback(1); // Exit with error code
    });

    it('should stop the backend process', function (done) {
      backend.on('stopped', () => {
        expect(mockChildProcess.kill).to.have.been.called;
        done();
      });

      backend.start();

      // Simulate started
      const startedCallback = mockChildProcess.on.getCall(0).args[1];
      startedCallback();

      backend.stop();
    });
  });

  describe('Request Handling', function () {
    beforeEach(function () {
      // Setup successful HTTP response mock
      axiosStub.resolves({
        data: {
          status: 'ok',
          results: [{ X: 5 }]
        }
      });
    });

    it('should send HTTP requests to Prolog server', async function () {
      backend.start();

      // Simulate backend ready
      const startedCallback = mockChildProcess.on.getCall(0).args[1];
      startedCallback();

      const response = await backend.sendRequest('query', {
        goal: 'X is 2 + 3'
      });

      expect(axiosStub).to.have.been.calledOnce;
      expect(response.status).to.equal('ok');
    });

    it('should handle HTTP request failures', async function () {
      axiosStub.rejects(new Error('Connection refused'));

      backend.start();

      // Simulate backend ready
      const startedCallback = mockChildProcess.on.getCall(0).args[1];
      startedCallback();

      try {
        await backend.sendRequest('query', { goal: 'test' });
        expect.fail('Should have thrown an error');
      } catch (error) {
        expect(error.message).to.include('Connection refused');
      }
    });

    it('should validate request parameters', async function () {
      backend.start();

      // Simulate backend ready
      const startedCallback = mockChildProcess.on.getCall(0).args[1];
      startedCallback();

      try {
        await backend.sendRequest('query', {});
        expect.fail('Should have thrown validation error');
      } catch (error) {
        expect(error).to.exist;
      }
    });

    it('should handle batch requests', async function () {
      axiosStub.resolves({
        data: [
          { status: 'ok', results: [] },
          { status: 'ok', results: [{ X: 5 }] }
        ]
      });

      backend.start();

      // Simulate backend ready
      const startedCallback = mockChildProcess.on.getCall(0).args[1];
      startedCallback();

      const batch = [
        { cmd: 'consult', params: { file: 'test.pl' } },
        { cmd: 'query', params: { goal: 'X is 2 + 3' } }
      ];

      const responses = await backend.sendRequest(batch);

      expect(responses).to.be.an('array').with.lengthOf(2);
      expect(axiosStub).to.have.been.calledOnce;
    });
  });

  describe('Input Validation and Security', function () {
    beforeEach(function () {
      backend.start();

      // Simulate backend ready
      const startedCallback = mockChildProcess.on.getCall(0).args[1];
      startedCallback();
    });

    it('should sanitize dangerous input characters', async function () {
      axiosStub.resolves({ data: { status: 'error', error: 'Invalid input' } });

      const response = await backend.sendRequest('query', {
        goal: 'test\u0000dangerous'
      });

      expect(response.status).to.equal('error');
    });

    it('should validate command types', async function () {
      try {
        await backend.sendRequest('invalid_command' as any, {});
        expect.fail('Should have thrown validation error');
      } catch (error) {
        expect(error).to.exist;
      }
    });

    it('should enforce timeout limits', async function () {
      // Mock a delayed response
      axiosStub.returns(new Promise(resolve => {
        setTimeout(() => resolve({ data: { status: 'ok' } }), 10000);
      }));

      try {
        await backend.sendRequest('query', {
          goal: 'test',
          timeoutMs: 1000
        });
        expect.fail('Should have timed out');
      } catch (error) {
        expect(error.message).to.include('timeout');
      }
    });
  });

  describe('Error Recovery', function () {
    it('should recover from process crashes', function (done) {
      let crashCount = 0;

      backend.on('error', () => {
        crashCount++;
      });

      backend.on('restarted', () => {
        expect(crashCount).to.be.greaterThan(0);
        done();
      });

      backend.start();

      // Simulate process crash
      const errorCallback = mockChildProcess.on.getCall(2).args[1];
      errorCallback(new Error('Process crashed'));
    });

    it('should handle multiple rapid restart attempts', function (done) {
      let restartCount = 0;

      backend.on('started', () => {
        restartCount++;
        if (restartCount >= 3) {
          done();
        }
      });

      backend.start();

      // Simulate multiple rapid crashes
      for (let i = 0; i < 3; i++) {
        setTimeout(() => {
          const startedCallback = mockChildProcess.on.getCall(0).args[1];
          startedCallback();

          if (i < 2) {
            const exitCallback = mockChildProcess.on.getCall(1).args[1];
            exitCallback(1);
          }
        }, i * 100);
      }
    });
  });

  describe('Health Checks', function () {
    beforeEach(function () {
      backend.start();

      // Simulate backend ready
      const startedCallback = mockChildProcess.on.getCall(0).args[1];
      startedCallback();
    });

    it('should perform health checks', async function () {
      axiosStub.resolves({
        data: {
          status: 'ok',
          version: 1,
          uptime: 1000
        }
      });

      const health = await backend.sendRequest('status', {});

      expect(health.status).to.equal('ok');
      expect(health.version).to.be.a('number');
    });

    it('should detect unhealthy backend', async function () {
      axiosStub.rejects(new Error('Health check failed'));

      try {
        await backend.sendRequest('status', {});
        expect.fail('Should have detected unhealthy backend');
      } catch (error) {
        expect(error.message).to.include('Health check failed');
      }
    });
  });

  describe('Configuration Management', function () {
    it('should use default configuration values', function () {
      const defaultBackend = new PrologBackend();
      expect(defaultBackend).to.be.instanceOf(PrologBackend);
    });

    it('should override default configuration', function () {
      const config = {
        swiplPath: '/custom/swipl',
        port: 8888,
        args: ['--custom-arg']
      };

      const customBackend = new PrologBackend(config);
      expect(customBackend).to.be.instanceOf(PrologBackend);
    });

    it('should validate configuration parameters', function () {
      expect(() => {
        new PrologBackend({ port: -1 } as any);
      }).to.throw();

      expect(() => {
        new PrologBackend({ port: 70000 } as any);
      }).to.throw();
    });
  });

  describe('Event Handling', function () {
    it('should emit lifecycle events', function (done) {
      const events: string[] = [];

      backend.on('starting', () => events.push('starting'));
      backend.on('started', () => events.push('started'));
      backend.on('ready', () => {
        events.push('ready');
        expect(events).to.include.members(['starting', 'started', 'ready']);
        done();
      });

      backend.start();

      // Simulate event sequence
      setTimeout(() => {
        const startedCallback = mockChildProcess.on.getCall(0).args[1];
        startedCallback();
      }, 10);
    });

    it('should emit error events', function (done) {
      backend.on('error', (error) => {
        expect(error).to.be.instanceOf(Error);
        done();
      });

      backend.start();

      // Simulate error
      const errorCallback = mockChildProcess.on.getCall(2).args[1];
      errorCallback(new Error('Test error'));
    });
  });

  describe('Resource Management', function () {
    it('should clean up resources on stop', function () {
      backend.start();
      backend.stop(true);

      expect(mockChildProcess.kill).to.have.been.called;
    });

    it('should handle graceful shutdown', function (done) {
      backend.on('stopped', () => {
        done();
      });

      backend.start();

      // Simulate started
      const startedCallback = mockChildProcess.on.getCall(0).args[1];
      startedCallback();

      backend.stop();
    });

    it('should handle forced shutdown', function () {
      backend.start();
      backend.stop(true);

      expect(mockChildProcess.kill).to.have.been.calledWith('SIGKILL');
    });
  });
});