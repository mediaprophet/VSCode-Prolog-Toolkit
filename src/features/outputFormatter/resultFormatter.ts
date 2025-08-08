import { BindingsFormatter } from './bindingsFormatter';
import { FormattingOptions, PrologResult } from './types';

export class ResultFormatter {
  private bindingsFormatter: BindingsFormatter;
  constructor(private options: Required<FormattingOptions>) {
    this.bindingsFormatter = new BindingsFormatter(options);
  }

  format(result: PrologResult): string {
    switch (result.type) {
      case 'success':
        return this.formatSuccess(result);
      case 'failure':
        return this.formatFailure(result);
      case 'error':
        return this.formatError(result);
      case 'multiple':
        return this.formatMultiple(result);
      default:
        return this.formatUnknown(result);
    }
  }

  private formatSuccess(result: PrologResult): string {
    if (result.bindings && result.bindings.length > 0) {
      return this.bindingsFormatter.format(result.bindings);
    }
    return '✅ **Query succeeded**\n' + (result.message ? `\n${result.message}\n` : '');
  }

  private formatFailure(result: PrologResult): string {
    return '❌ **Query failed**\n' + (result.message ? `\n${result.message}\n` : '');
  }

  private formatError(result: PrologResult): string {
    let output = '🚫 **Query error**\n\n';
    if (result.error) {
      if (this.options.useCodeBlocks) {
        output += '```\n' + result.error + '\n```\n';
      } else {
        output += result.error + '\n';
      }
    }
    if (result.message) {
      output += '\n' + result.message + '\n';
    }
    return output;
  }

  private formatMultiple(result: PrologResult): string {
    if (result.bindings) {
      return this.bindingsFormatter.format(result.bindings);
    }
    return `✅ **Query succeeded** (${result.count || 0} results)\n`;
  }

  private formatUnknown(result: PrologResult): string {
    return `⚠️ **Unknown result type**: ${JSON.stringify(result)}\n`;
  }
}
