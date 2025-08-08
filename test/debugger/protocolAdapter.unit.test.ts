import { expect } from 'chai';
import { ProtocolAdapter } from '../../src/features/debugger/ProtocolAdapter';

describe('ProtocolAdapter', () => {
  it('should emit breakpoints event for valid breakpoints message', done => {
    const adapter = new ProtocolAdapter();
    const msg = JSON.stringify({ response: { breakpoints: [{ id: 1, line: 2 }] } });
    adapter.on('breakpoints', data => {
      expect(data).to.be.an('array');
      expect(data[0].id).to.equal(1);
      done();
    });
    adapter.handleProtocolData(msg);
  });

  it('should emit error for invalid JSON', done => {
    const adapter = new ProtocolAdapter();
    adapter.on('error', err => {
      expect(err).to.be.instanceOf(Error);
      done();
    });
    adapter.handleProtocolData('not a json');
  });

  it('should retry and emit error on repeated failure', async () => {
    const adapter = new ProtocolAdapter();
    let errorEmitted = false;
    adapter.on('error', () => {
      errorEmitted = true;
    });
    await adapter.handleProtocolDataWithRetry('not a json', 100, 1).catch(() => { });
    expect(errorEmitted).to.be.true;
  });
});
