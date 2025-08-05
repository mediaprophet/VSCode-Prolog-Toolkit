import { expect } from 'chai';
import { describe, it } from 'mocha';

// Simple integration test for Step 4 components
describe('Step 4: Output Formatting and Error Handling Integration', () => {
  it('should be able to import OutputFormatter', async () => {
    const { OutputFormatter } = await import('../src/features/outputFormatter');
    expect(OutputFormatter).to.be.a('function');
    
    const formatter = new OutputFormatter();
    expect(formatter).to.be.an('object');
    expect(formatter.formatQueryResult).to.be.a('function');
  });

  it('should be able to import ErrorHandler', async () => {
    const { ErrorHandler } = await import('../src/features/errorHandler');
    expect(ErrorHandler).to.be.a('function');
    
    const errorHandler = new ErrorHandler();
    expect(errorHandler).to.be.an('object');
    expect(errorHandler.handleError).to.be.a('function');
  });

  it('should format a simple success result', async () => {
    const { OutputFormatter } = await import('../src/features/outputFormatter');
    const formatter = new OutputFormatter();
    
    const result = {
      type: 'success' as const,
      bindings: [{ X: 'hello', Y: 42 }]
    };
    
    const formatted = formatter.formatQueryResult(result);
    expect(formatted).to.include('✅');
    expect(formatted).to.include('Query succeeded');
    expect(formatted).to.include('X = hello');
    expect(formatted).to.include('Y = 42');
  });

  it('should handle a simple error', async () => {
    const { ErrorHandler } = await import('../src/features/errorHandler');
    const errorHandler = new ErrorHandler();
    
    const error = 'syntax error: missing period';
    const result = errorHandler.handleError(error);
    
    expect(result.code).to.equal('SYNTAX_ERROR');
    expect(result.type).to.equal('syntax');
    expect(result.message).to.include('syntax error');
    expect(result.suggestion).to.include('Check your Prolog syntax');
  });

  it('should format error for display', async () => {
    const { ErrorHandler } = await import('../src/features/errorHandler');
    const errorHandler = new ErrorHandler();
    
    const error = errorHandler.handleError('type_error(integer, atom)');
    const formatted = errorHandler.formatError(error);
    
    expect(formatted).to.include('⚡ **Runtime Error**');
    expect(formatted).to.include('**Message:**');
    expect(formatted).to.include('💡 **Suggestion:**');
  });

  it('should handle streaming output', async () => {
    const { StreamingHandler } = await import('../src/features/streamingHandler');
    const streamer = new StreamingHandler({ chunkSize: 2, maxTotalResults: 10 });
    
    expect(streamer).to.be.an('object');
    expect(streamer.startStreaming).to.be.a('function');
    expect(streamer.streaming).to.be.false;
  });

  it('should support localization', async () => {
    const { LocalizationManager } = await import('../src/features/localization');
    const locManager = new LocalizationManager('/fake/path');
    
    expect(locManager).to.be.an('object');
    expect(locManager.getString).to.be.a('function');
    
    // Test default English strings
    const errorString = locManager.getString('errors.syntaxError');
    expect(errorString).to.include('Syntax error');
    
    const queryString = locManager.getString('query.succeeded');
    expect(queryString).to.include('Query succeeded');
  });

  it('should integrate formatter with error handler', async () => {
    const { OutputFormatter } = await import('../src/features/outputFormatter');
    const { ErrorHandler } = await import('../src/features/errorHandler');
    
    const formatter = new OutputFormatter();
    const errorHandler = new ErrorHandler();
    
    // Test error result formatting
    const errorResult = {
      type: 'error' as const,
      error: 'existence_error(procedure, unknown/1)',
      message: 'Predicate not found'
    };
    
    const formattedError = formatter.formatQueryResult(errorResult);
    expect(formattedError).to.include('🚫 **Query error**');
    expect(formattedError).to.include('existence_error');
    
    // Test error handling
    const handledError = errorHandler.handleError('existence_error(procedure, unknown/1)');
    expect(handledError.code).to.equal('EXISTENCE_ERROR');
    expect(handledError.suggestion).to.include('Check if the predicate is defined');
  });

  it('should handle complex variable bindings', async () => {
    const { OutputFormatter } = await import('../src/features/outputFormatter');
    const formatter = new OutputFormatter();
    
    const result = {
      type: 'multiple' as const,
      bindings: [
        { Name: 'john', Age: 25, Hobbies: ['reading', 'coding'] },
        { Name: 'jane', Age: 30, Hobbies: ['music', 'art'] },
        { Name: 'bob', Age: 35, Hobbies: ['sports'] }
      ]
    };
    
    const formatted = formatter.formatQueryResult(result);
    expect(formatted).to.include('3 solutions');
    expect(formatted).to.include('**Solution 1:**');
    expect(formatted).to.include('Name = john');
    expect(formatted).to.include('Hobbies = [reading, coding]');
  });

  it('should truncate very long output', async () => {
    const { OutputFormatter } = await import('../src/features/outputFormatter');
    const formatter = new OutputFormatter();
    
    const longOutput = 'A'.repeat(3000);
    const truncated = formatter.truncateOutput(longOutput, 1000);
    
    expect(truncated.length).to.be.lessThan(longOutput.length);
    expect(truncated).to.include('... (output truncated)');
  });

  it('should provide user-friendly error messages', async () => {
    const { ErrorHandler } = await import('../src/features/errorHandler');
    const errorHandler = new ErrorHandler();
    
    // Test common user mistakes
    const missingPeriod = errorHandler.createUserFriendlyError('?- parent(X, Y)', 'syntax error');
    expect(missingPeriod.code).to.equal('MISSING_PERIOD');
    expect(missingPeriod.message).to.include('Query should end with a period');
    
    const unmatchedParen = errorHandler.createUserFriendlyError('parent(tom, bob', 'syntax error');
    expect(unmatchedParen.code).to.equal('UNMATCHED_PARENTHESES');
    expect(unmatchedParen.message).to.include('Missing closing parenthesis');
  });
});