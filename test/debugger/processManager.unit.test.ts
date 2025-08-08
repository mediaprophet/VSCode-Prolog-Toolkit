import { expect } from 'chai';
import { ProcessManager, PrologProcessOptions } from '../../src/features/debugger/ProcessManager';

describe('ProcessManager', function () {
  this.timeout(20000);

  it('should start and kill a Prolog process (mocked)', async function () {
    const options: PrologProcessOptions = {
      runtimeExecutable: 'node',
      runtimeArgs: ['-e', 'setTimeout(()=>{}, 10000)'],
      cwd: process.cwd(),
      timeoutMs: 2000,
      maxRetries: 1,
    };
    const pm = new ProcessManager(options);
    let started = false;
    pm.on('process', proc => {
      started = true;
      expect(proc.pid).to.be.a('number');
      pm.kill();
    });
    await pm.start();
    expect(started).to.be.true;
  });

  it('should retry and emit error on failure', async function () {
    const options: PrologProcessOptions = {
      runtimeExecutable: 'nonexistent_executable',
      runtimeArgs: [],
      cwd: process.cwd(),
      timeoutMs: 1000,
      maxRetries: 1,
    };
    const pm = new ProcessManager(options);
    let errorEmitted = false;
    pm.on('error', err => {
      errorEmitted = true;
      expect(err).to.be.instanceOf(Error);
    });
    await pm.start();
    expect(errorEmitted).to.be.true;
  });
});
