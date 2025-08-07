"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var chai_1 = require("chai");
var mocha_1 = require("mocha");
var outputFormatter_js_1 = require("../src/features/outputFormatter.js");
(0, mocha_1.describe)('OutputFormatter', function () {
    var formatter;
    (0, mocha_1.beforeEach)(function () {
        formatter = new outputFormatter_js_1.OutputFormatter();
    });
    (0, mocha_1.describe)('formatQueryResult', function () {
        (0, mocha_1.it)('should format success result with no bindings', function () {
            var result = {
                type: 'success',
                message: 'Query completed',
            };
            var formatted = formatter.formatQueryResult(result);
            (0, chai_1.expect)(formatted).to.include('✅ **Query succeeded**');
            (0, chai_1.expect)(formatted).to.include('Query completed');
        });
        (0, mocha_1.it)('should format success result with single binding', function () {
            var result = {
                type: 'success',
                bindings: [{ X: 'hello', Y: 42 }],
            };
            var formatted = formatter.formatQueryResult(result);
            (0, chai_1.expect)(formatted).to.include('✅ **Query succeeded:**');
            (0, chai_1.expect)(formatted).to.include('X = hello');
            (0, chai_1.expect)(formatted).to.include('Y = 42');
            (0, chai_1.expect)(formatted).to.include('```prolog');
        });
        (0, mocha_1.it)('should format multiple bindings as table', function () {
            var result = {
                type: 'multiple',
                bindings: [
                    { X: 'a', Y: 1 },
                    { X: 'b', Y: 2 },
                    { X: 'c', Y: 3 },
                ],
            };
            var formatted = formatter.formatQueryResult(result);
            (0, chai_1.expect)(formatted).to.include('✅ **Query succeeded** (3 solutions)');
            (0, chai_1.expect)(formatted).to.include('**Solution 1:**');
            (0, chai_1.expect)(formatted).to.include('**Solution 2:**');
            (0, chai_1.expect)(formatted).to.include('**Solution 3:**');
        });
        (0, mocha_1.it)('should format failure result', function () {
            var result = {
                type: 'failure',
                message: 'No solutions found',
            };
            var formatted = formatter.formatQueryResult(result);
            (0, chai_1.expect)(formatted).to.include('❌ **Query failed**');
            (0, chai_1.expect)(formatted).to.include('No solutions found');
        });
        (0, mocha_1.it)('should format error result', function () {
            var result = {
                type: 'error',
                error: 'syntax_error(operator_expected)',
                message: 'Check your syntax',
            };
            var formatted = formatter.formatQueryResult(result);
            (0, chai_1.expect)(formatted).to.include('🚫 **Query error**');
            (0, chai_1.expect)(formatted).to.include('syntax_error(operator_expected)');
            (0, chai_1.expect)(formatted).to.include('Check your syntax');
        });
    });
    (0, mocha_1.describe)('formatVariableBindings', function () {
        (0, mocha_1.it)('should handle empty bindings', function () {
            var formatted = formatter.formatVariableBindings([]);
            (0, chai_1.expect)(formatted).to.include('✅ **Query succeeded** (no variable bindings)');
        });
        (0, mocha_1.it)('should format single binding', function () {
            var bindings = [{ Name: 'john', Age: 25 }];
            var formatted = formatter.formatVariableBindings(bindings);
            (0, chai_1.expect)(formatted).to.include('Name = john');
            (0, chai_1.expect)(formatted).to.include('Age = 25');
        });
        (0, mocha_1.it)('should format multiple bindings with table', function () {
            var bindings = [
                { Name: 'john', Age: 25 },
                { Name: 'jane', Age: 30 },
            ];
            var formatted = formatter.formatVariableBindings(bindings);
            (0, chai_1.expect)(formatted).to.include('2 solutions');
            (0, chai_1.expect)(formatted).to.include('**Solution 1:**');
            (0, chai_1.expect)(formatted).to.include('**Solution 2:**');
        });
        (0, mocha_1.it)('should limit results when too many', function () {
            var bindings = Array.from({ length: 100 }, function (_, i) { return ({ X: i }); });
            var formatted = formatter.formatVariableBindings(bindings);
            (0, chai_1.expect)(formatted).to.include('100 solutions');
            (0, chai_1.expect)(formatted).to.include('... and 50 more solution');
        });
    });
    (0, mocha_1.describe)('formatValue', function () {
        (0, mocha_1.it)('should format atoms correctly', function () {
            var formatter = new outputFormatter_js_1.OutputFormatter();
            (0, chai_1.expect)(formatter.formatValue('hello')).to.equal('hello');
            (0, chai_1.expect)(formatter.formatValue('Hello World')).to.equal("'Hello World'");
        });
        (0, mocha_1.it)('should format numbers correctly', function () {
            var formatter = new outputFormatter_js_1.OutputFormatter();
            (0, chai_1.expect)(formatter.formatValue(42)).to.equal('42');
            (0, chai_1.expect)(formatter.formatValue(3.14)).to.equal('3.14');
        });
        (0, mocha_1.it)('should format booleans correctly', function () {
            var formatter = new outputFormatter_js_1.OutputFormatter();
            (0, chai_1.expect)(formatter.formatValue(true)).to.equal('true');
            (0, chai_1.expect)(formatter.formatValue(false)).to.equal('false');
        });
        (0, mocha_1.it)('should format arrays correctly', function () {
            var formatter = new outputFormatter_js_1.OutputFormatter();
            (0, chai_1.expect)(formatter.formatValue([1, 2, 3])).to.equal('[1, 2, 3]');
            (0, chai_1.expect)(formatter.formatValue([
                'a',
                'b',
            ])).to.equal('[a, b]');
        });
        (0, mocha_1.it)('should format compound terms correctly', function () {
            var formatter = new outputFormatter_js_1.OutputFormatter();
            var compound = { functor: 'person', args: ['john', 25] };
            (0, chai_1.expect)(formatter.formatValue(compound)).to.equal('person(john, 25)');
        });
        (0, mocha_1.it)('should handle null and undefined', function () {
            var formatter = new outputFormatter_js_1.OutputFormatter();
            (0, chai_1.expect)(formatter.formatValue(null)).to.equal('_');
            (0, chai_1.expect)(formatter.formatValue(undefined)).to.equal('_');
        });
    });
    (0, mocha_1.describe)('formatPrologCode', function () {
        (0, mocha_1.it)('should format code with title', function () {
            var code = 'parent(tom, bob).\nparent(bob, liz).';
            var formatted = formatter.formatPrologCode(code, 'Family Facts');
            (0, chai_1.expect)(formatted).to.include('**Family Facts:**');
            (0, chai_1.expect)(formatted).to.include('```prolog');
            (0, chai_1.expect)(formatted).to.include('parent(tom, bob)');
        });
        (0, mocha_1.it)('should format code without title', function () {
            var code = 'member(X, [X|_]).';
            var formatted = formatter.formatPrologCode(code);
            (0, chai_1.expect)(formatted).to.include('```prolog');
            (0, chai_1.expect)(formatted).to.include('member(X, [X|_])');
        });
    });
    (0, mocha_1.describe)('formatHelpText', function () {
        (0, mocha_1.it)('should format help documentation', function () {
            var predicate = 'member/2';
            var docs = 'member(?Elem, ?List)\n\nTrue if Elem is a member of List.\n\n?- member(X, [1,2,3]).\nX = 1 ;\nX = 2 ;\nX = 3.';
            var formatted = formatter.formatHelpText(predicate, docs);
            (0, chai_1.expect)(formatted).to.include('📖 **Help for `member/2`:**');
            (0, chai_1.expect)(formatted).to.include('True if Elem is a member of List');
            (0, chai_1.expect)(formatted).to.include('```prolog');
            (0, chai_1.expect)(formatted).to.include('?- member(X, [1,2,3])');
        });
    });
    (0, mocha_1.describe)('formatStreamingOutput', function () {
        (0, mocha_1.it)('should format first chunk with total count', function () {
            var chunk = [{ X: 1 }, { X: 2 }];
            var formatted = formatter.formatStreamingOutput(chunk, true, false, 100);
            (0, chai_1.expect)(formatted).to.include('📊 **Streaming results** (100 total)');
            (0, chai_1.expect)(formatted).to.include('⏳ *Loading more results...*');
        });
        (0, mocha_1.it)('should format last chunk', function () {
            var chunk = [{ X: 99 }, { X: 100 }];
            var formatted = formatter.formatStreamingOutput(chunk, false, true);
            (0, chai_1.expect)(formatted).to.include('✅ **Streaming complete**');
        });
        (0, mocha_1.it)('should format middle chunk', function () {
            var chunk = [{ X: 50 }, { X: 51 }];
            var formatted = formatter.formatStreamingOutput(chunk, false, false);
            (0, chai_1.expect)(formatted).to.include('⏳ *Loading more results...*');
        });
    });
    (0, mocha_1.describe)('truncateOutput', function () {
        (0, mocha_1.it)('should not truncate short output', function () {
            var shortOutput = 'This is a short output';
            var truncated = formatter.truncateOutput(shortOutput, 100);
            (0, chai_1.expect)(truncated).to.equal(shortOutput);
        });
        (0, mocha_1.it)('should truncate long output', function () {
            var longOutput = 'A'.repeat(3000);
            var truncated = formatter.truncateOutput(longOutput, 2000);
            (0, chai_1.expect)(truncated.length).to.be.lessThan(longOutput.length);
            (0, chai_1.expect)(truncated).to.include('... (output truncated)');
        });
        (0, mocha_1.it)('should truncate at newline when possible', function () {
            var output = 'Line 1\n' + 'A'.repeat(2000) + '\nLine 3';
            var truncated = formatter.truncateOutput(output, 1500);
            (0, chai_1.expect)(truncated).to.include('... (output truncated)');
            (0, chai_1.expect)(truncated).to.not.include('Line 3');
        });
    });
    (0, mocha_1.describe)('formatting options', function () {
        (0, mocha_1.it)('should respect compact mode', function () {
            var compactFormatter = new outputFormatter_js_1.OutputFormatter({ compactMode: true });
            var bindings = [
                { X: 'a', Y: 1 },
                { X: 'b', Y: 2 },
            ];
            var formatted = compactFormatter.formatVariableBindings(bindings);
            (0, chai_1.expect)(formatted).to.include('|'); // Table format
        });
        (0, mocha_1.it)('should respect maxResults option', function () {
            var limitedFormatter = new outputFormatter_js_1.OutputFormatter({ maxResults: 2 });
            var bindings = Array.from({ length: 5 }, function (_, i) { return ({ X: i }); });
            var formatted = limitedFormatter.formatVariableBindings(bindings);
            (0, chai_1.expect)(formatted).to.include('... and 3 more solution');
        });
        (0, mocha_1.it)('should respect useCodeBlocks option', function () {
            var noCodeFormatter = new outputFormatter_js_1.OutputFormatter({ useCodeBlocks: false });
            var result = {
                type: 'success',
                bindings: [{ X: 'test' }],
            };
            var formatted = noCodeFormatter.formatQueryResult(result);
            (0, chai_1.expect)(formatted).to.not.include('```');
        });
        (0, mocha_1.it)('should update options', function () {
            formatter.updateOptions({ maxResults: 10, compactMode: true });
            var options = formatter.getOptions();
            (0, chai_1.expect)(options.maxResults).to.equal(10);
            (0, chai_1.expect)(options.compactMode).to.be.true;
        });
    });
    (0, mocha_1.describe)('complex data structures', function () {
        (0, mocha_1.it)('should format nested lists', function () {
            var bindings = [
                {
                    Matrix: [
                        [1, 2],
                        [3, 4],
                    ],
                },
            ];
            var formatted = formatter.formatVariableBindings(bindings);
            (0, chai_1.expect)(formatted).to.include('Matrix = [[1, 2], [3, 4]]');
        });
        (0, mocha_1.it)('should format complex compound terms', function () {
            var bindings = [
                {
                    Person: {
                        functor: 'person',
                        args: ['john', { functor: 'address', args: ['123 Main St', 'Anytown'] }],
                    },
                },
            ];
            var formatted = formatter.formatVariableBindings(bindings);
            (0, chai_1.expect)(formatted).to.include('person(john, address(123 Main St, Anytown))');
        });
        (0, mocha_1.it)('should handle mixed data types', function () {
            var bindings = [
                {
                    Mixed: {
                        functor: 'data',
                        args: [42, 'hello', [1, 2, 3], true],
                    },
                },
            ];
            var formatted = formatter.formatVariableBindings(bindings);
            (0, chai_1.expect)(formatted).to.include('data(42, hello, [1, 2, 3], true)');
        });
    });
    (0, mocha_1.describe)('edge cases', function () {
        (0, mocha_1.it)('should handle empty variable names', function () {
            var bindings = [{ '': 'value' }];
            var formatted = formatter.formatVariableBindings(bindings);
            (0, chai_1.expect)(formatted).to.include(' = value');
        });
        (0, mocha_1.it)('should handle very long variable names', function () {
            var _a;
            var longName = 'A'.repeat(100);
            var bindings = [(_a = {}, _a[longName] = 'value', _a)];
            var formatted = formatter.formatVariableBindings(bindings);
            (0, chai_1.expect)(formatted).to.include('value');
        });
        (0, mocha_1.it)('should handle circular references safely', function () {
            var circular = { functor: 'circular', args: [] };
            circular.args.push(circular);
            var bindings = [{ Circular: circular }];
            // Should not throw an error
            (0, chai_1.expect)(function () { return formatter.formatVariableBindings(bindings); }).to.not.throw();
        });
        (0, mocha_1.it)('should handle special characters in atoms', function () {
            var bindings = [
                {
                    Special: "'hello world'",
                    Unicode: '🚀',
                    Quotes: '"quoted"',
                },
            ];
            var formatted = formatter.formatVariableBindings(bindings);
            (0, chai_1.expect)(formatted).to.include("'hello world'");
            (0, chai_1.expect)(formatted).to.include('🚀');
            (0, chai_1.expect)(formatted).to.include('"quoted"');
        });
    });
});
