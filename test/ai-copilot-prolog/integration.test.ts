import axios from 'axios';
import { expect } from 'chai';
import { PrologBackend } from '../../out/src/prologBackend.js';

describe('AI Copilot Prolog Support - Integration', function () {
  let backend: PrologBackend;
  const baseUrl = 'http://localhost:9080';

  before(async function () {
    backend = new PrologBackend();
    await backend.start();
  });

  after(async function () {
    await backend.stop();
  });

  it('should execute a simple Prolog query and return variable bindings', async function () {
    const response = await axios.post(`${baseUrl}/query`, {
      goal: 'member(X, [a,b,c])',
      options: { maxSolutions: 3 }
    });
    expect(response.data.success).to.be.true;
    expect(response.data.results).to.be.an('array').with.length(3);
    expect(response.data.results[0]).to.have.property('X');
  });

  it('should consult a Prolog file and query a predicate', async function () {
    const consultResp = await axios.post(`${baseUrl}/consult`, {
      file: 'test/resources/simple.pl'
    });
    expect(consultResp.data.success).to.be.true;
    const queryResp = await axios.post(`${baseUrl}/query`, {
      goal: 'parent(john, X)',
      options: { maxSolutions: 2 }
    });
    expect(queryResp.data.success).to.be.true;
    expect(queryResp.data.results[0]).to.have.property('X');
  });

  it('should return documentation for a built-in predicate', async function () {
    const response = await axios.post(`${baseUrl}/help`, {
      predicate: 'member/2'
    });
    expect(response.data.success).to.be.true;
    expect(response.data.doc).to.include('member');
  });

  it('should load and reason over N3 data', async function () {
    const n3Content = '@prefix : <http://example.org/> . :socrates a :Person . { ?x a :Person } => { ?x a :Mortal } .';
    const loadResp = await axios.post(`${baseUrl}/n3_load`, {
      content: n3Content
    });
    expect(loadResp.data.success).to.be.true;
    const reasonResp = await axios.post(`${baseUrl}/n3_reason`, {});
    expect(reasonResp.data.success).to.be.true;
    expect(reasonResp.data.results).to.satisfy((arr: any[]) => arr.some(t => t.includes('Mortal')));
  });

  it('should handle errors and return structured error messages', async function () {
    const response = await axios.post(`${baseUrl}/query`, {
      goal: 'this_is_not_a_predicate(,)',
      options: {}
    });
    expect(response.data.success).to.be.false;
    expect(response.data.error).to.have.property('message');
    expect(response.data.error).to.have.property('code');
  });
});
