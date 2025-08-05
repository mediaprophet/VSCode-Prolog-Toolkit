import { expect } from 'chai';
import { describe, it, beforeEach } from 'mocha';
import { OutputFormatter, PrologResult, FormattingOptions } from '../src/features/outputFormatter';

describe('OutputFormatter', () => {
  let formatter: OutputFormatter;

  beforeEach(() => {
    formatter = new OutputFormatter();
  });

  describe('formatQueryResult', () => {
    it('should format success result with no bindings', () => {
      const result: PrologResult = {
        type: 'success',
        message: 'Query completed'
      };

      const formatted = formatter.formatQueryResult(result);
      expect(formatted).to.include('✅ **Query succeeded**');
      expect(formatted).to.include('Query completed');
    });

    it('should format success result with single binding', () => {
      const result: PrologResult = {
        type: 'success',
        bindings: [{ X: 'hello', Y: 42 }]
      };

      const formatted = formatter.formatQueryResult(result);
      expect(formatted).to.include('✅ **Query succeeded:**');
      expect(formatted).to.include('X = hello');
      expect(formatted).to.include('Y = 42');
      expect(formatted).to.include('```prolog');
    });

    it('should format multiple bindings as table', () => {
      const result: PrologResult = {
        type: 'multiple',
        bindings: [
          { X: 'a', Y: 1 },
          { X: 'b', Y: 2 },
          { X: 'c', Y: 3 }
        ]
      };

      const formatted = formatter.formatQueryResult(result);
      expect(formatted).to.include('✅ **Query succeeded** (3 solutions)');
      expect(formatted).to.include('**Solution 1:**');
      expect(formatted).to.include('**Solution 2:**');
      expect(formatted).to.include('**Solution 3:**');
    });

    it('should format failure result', () => {
      const result: PrologResult = {
        type: 'failure',
        message: 'No solutions found'
      };

      const formatted = formatter.formatQueryResult(result);
      expect(formatted).to.include('❌ **Query failed**');
      expect(formatted).to.include('No solutions found');
    });

    it('should format error result', () => {
      const result: PrologResult = {
        type: 'error',
        error: 'syntax_error(operator_expected)',
        message: 'Check your syntax'
      };

      const formatted = formatter.formatQueryResult(result);
      expect(formatted).to.include('🚫 **Query error**');
      expect(formatted).to.include('syntax_error(operator_expected)');
      expect(formatted).to.include('Check your syntax');
    });
  });

  describe('formatVariableBindings', () => {
    it('should handle empty bindings', () => {
      const formatted = formatter.formatVariableBindings([]);
      expect(formatted).to.include('✅ **Query succeeded** (no variable bindings)');
    });

    it('should format single binding', () => {
      const bindings = [{ Name: 'john', Age: 25 }];
      const formatted = formatter.formatVariableBindings(bindings);
      
      expect(formatted).to.include('Name = john');
      expect(formatted).to.include('Age = 25');
    });

    it('should format multiple bindings with table', () => {
      const bindings = [
        { Name: 'john', Age: 25 },
        { Name: 'jane', Age: 30 }
      ];
      const formatted = formatter.formatVariableBindings(bindings);
      
      expect(formatted).to.include('2 solutions');
      expect(formatted).to.include('**Solution 1:**');
      expect(formatted).to.include('**Solution 2:**');
    });

    it('should limit results when too many', () => {
      const bindings = Array.from({ length: 100 }, (_, i) => ({ X: i }));
      const formatted = formatter.formatVariableBindings(bindings);
      
      expect(formatted).to.include('100 solutions');
      expect(formatted).to.include('... and 50 more solution');
    });
  });

  describe('formatValue', () => {
    it('should format atoms correctly', () => {
      const formatter = new OutputFormatter();
      expect((formatter as unknown as { formatValue: (value: unknown) => string }).formatValue('hello')).to.equal('hello');
      expect((formatter as unknown as { formatValue: (value: unknown) => string }).formatValue('Hello World')).to.equal("'Hello World'");
    });

    it('should format numbers correctly', () => {
      const formatter = new OutputFormatter();
      expect((formatter as unknown as { formatValue: (value: unknown) => string }).formatValue(42)).to.equal('42');
      expect((formatter as unknown as { formatValue: (value: unknown) => string }).formatValue(3.14)).to.equal('3.14');
    });

    it('should format booleans correctly', () => {
      const formatter = new OutputFormatter();
      expect((formatter as unknown as { formatValue: (value: unknown) => string }).formatValue(true)).to.equal('true');
      expect((formatter as unknown as { formatValue: (value: unknown) => string }).formatValue(false)).to.equal('false');
    });

    it('should format arrays correctly', () => {
      const formatter = new OutputFormatter();
      expect((formatter as unknown as { formatValue: (value: unknown) => string }).formatValue([1, 2, 3])).to.equal('[1, 2, 3]');
      expect((formatter as unknown as { formatValue: (value: unknown) => string }).formatValue(['a', 'b'])).to.equal('[a, b]');
    });

    it('should format compound terms correctly', () => {
      const formatter = new OutputFormatter();
      const compound = { functor: 'person', args: ['john', 25] };
      expect((formatter as unknown as { formatValue: (value: unknown) => string }).formatValue(compound)).to.equal('person(john, 25)');
    });

    it('should handle null and undefined', () => {
      const formatter = new OutputFormatter();
      expect((formatter as unknown as { formatValue: (value: unknown) => string }).formatValue(null)).to.equal('_');
      expect((formatter as unknown as { formatValue: (value: unknown) => string }).formatValue(undefined)).to.equal('_');
    });
  });

  describe('formatPrologCode', () => {
    it('should format code with title', () => {
      const code = 'parent(tom, bob).\nparent(bob, liz).';
      const formatted = formatter.formatPrologCode(code, 'Family Facts');
      
      expect(formatted).to.include('**Family Facts:**');
      expect(formatted).to.include('```prolog');
      expect(formatted).to.include('parent(tom, bob)');
    });

    it('should format code without title', () => {
      const code = 'member(X, [X|_]).';
      const formatted = formatter.formatPrologCode(code);
      
      expect(formatted).to.include('```prolog');
      expect(formatted).to.include('member(X, [X|_])');
    });
  });

  describe('formatHelpText', () => {
    it('should format help documentation', () => {
      const predicate = 'member/2';
      const docs = 'member(?Elem, ?List)\n\nTrue if Elem is a member of List.\n\n?- member(X, [1,2,3]).\nX = 1 ;\nX = 2 ;\nX = 3.';
      
      const formatted = formatter.formatHelpText(predicate, docs);
      
      expect(formatted).to.include('📖 **Help for `member/2`:**');
      expect(formatted).to.include('True if Elem is a member of List');
      expect(formatted).to.include('```prolog');
      expect(formatted).to.include('?- member(X, [1,2,3])');
    });
  });

  describe('formatStreamingOutput', () => {
    it('should format first chunk with total count', () => {
      const chunk = [{ X: 1 }, { X: 2 }];
      const formatted = formatter.formatStreamingOutput(chunk, true, false, 100);
      
      expect(formatted).to.include('📊 **Streaming results** (100 total)');
      expect(formatted).to.include('⏳ *Loading more results...*');
    });

    it('should format last chunk', () => {
      const chunk = [{ X: 99 }, { X: 100 }];
      const formatted = formatter.formatStreamingOutput(chunk, false, true);
      
      expect(formatted).to.include('✅ **Streaming complete**');
    });

    it('should format middle chunk', () => {
      const chunk = [{ X: 50 }, { X: 51 }];
      const formatted = formatter.formatStreamingOutput(chunk, false, false);
      
      expect(formatted).to.include('⏳ *Loading more results...*');
    });
  });

  describe('truncateOutput', () => {
    it('should not truncate short output', () => {
      const shortOutput = 'This is a short output';
      const truncated = formatter.truncateOutput(shortOutput, 100);
      expect(truncated).to.equal(shortOutput);
    });

    it('should truncate long output', () => {
      const longOutput = 'A'.repeat(3000);
      const truncated = formatter.truncateOutput(longOutput, 2000);
      
      expect(truncated.length).to.be.lessThan(longOutput.length);
      expect(truncated).to.include('... (output truncated)');
    });

    it('should truncate at newline when possible', () => {
      const output = 'Line 1\n' + 'A'.repeat(2000) + '\nLine 3';
      const truncated = formatter.truncateOutput(output, 1500);
      
      expect(truncated).to.include('... (output truncated)');
      expect(truncated).to.not.include('Line 3');
    });
  });

  describe('formatting options', () => {
    it('should respect compact mode', () => {
      const compactFormatter = new OutputFormatter({ compactMode: true });
      const bindings = [
        { X: 'a', Y: 1 },
        { X: 'b', Y: 2 }
      ];
      
      const formatted = compactFormatter.formatVariableBindings(bindings);
      expect(formatted).to.include('|');  // Table format
    });

    it('should respect maxResults option', () => {
      const limitedFormatter = new OutputFormatter({ maxResults: 2 });
      const bindings = Array.from({ length: 5 }, (_, i) => ({ X: i }));
      
      const formatted = limitedFormatter.formatVariableBindings(bindings);
      expect(formatted).to.include('... and 3 more solution');
    });

    it('should respect useCodeBlocks option', () => {
      const noCodeFormatter = new OutputFormatter({ useCodeBlocks: false });
      const result: PrologResult = {
        type: 'success',
        bindings: [{ X: 'test' }]
      };
      
      const formatted = noCodeFormatter.formatQueryResult(result);
      expect(formatted).to.not.include('```');
    });

    it('should update options', () => {
      formatter.updateOptions({ maxResults: 10, compactMode: true });
      const options = formatter.getOptions();
      
      expect(options.maxResults).to.equal(10);
      expect(options.compactMode).to.be.true;
    });
  });

  describe('complex data structures', () => {
    it('should format nested lists', () => {
      const bindings = [{ Matrix: [[1, 2], [3, 4]] }];
      const formatted = formatter.formatVariableBindings(bindings);
      
      expect(formatted).to.include('Matrix = [[1, 2], [3, 4]]');
    });

    it('should format complex compound terms', () => {
      const bindings = [{
        Person: {
          functor: 'person',
          args: [
            'john',
            { functor: 'address', args: ['123 Main St', 'Anytown'] }
          ]
        }
      }];
      
      const formatted = formatter.formatVariableBindings(bindings);
      expect(formatted).to.include('person(john, address(123 Main St, Anytown))');
    });

    it('should handle mixed data types', () => {
      const bindings = [{
        Mixed: {
          functor: 'data',
          args: [42, 'hello', [1, 2, 3], true]
        }
      }];
      
      const formatted = formatter.formatVariableBindings(bindings);
      expect(formatted).to.include('data(42, hello, [1, 2, 3], true)');
    });
  });

  describe('edge cases', () => {
    it('should handle empty variable names', () => {
      const bindings = [{ '': 'value' }];
      const formatted = formatter.formatVariableBindings(bindings);
      expect(formatted).to.include(' = value');
    });

    it('should handle very long variable names', () => {
      const longName = 'A'.repeat(100);
      const bindings = [{ [longName]: 'value' }];
      const formatted = formatter.formatVariableBindings(bindings);
      expect(formatted).to.include('value');
    });

    it('should handle circular references safely', () => {
      const circular: { functor: string; args: unknown[] } = { functor: 'circular', args: [] };
      circular.args.push(circular);
      
      const bindings = [{ Circular: circular }];
      // Should not throw an error
      expect(() => formatter.formatVariableBindings(bindings)).to.not.throw();
    });

    it('should handle special characters in atoms', () => {
      const bindings = [{ 
        Special: "'hello world'",
        Unicode: '🚀',
        Quotes: '"quoted"'
      }];
      
      const formatted = formatter.formatVariableBindings(bindings);
      expect(formatted).to.include("'hello world'");
      expect(formatted).to.include('🚀');
      expect(formatted).to.include('"quoted"');
    });
  });
});