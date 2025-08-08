import { FormattingOptions } from './types';

export class BindingsFormatter {
  constructor(private options: Required<FormattingOptions>) { }

  format(bindings: Record<string, any>[]): string {
    if (!bindings || bindings.length === 0) {
      return '✅ **Query succeeded** (no variable bindings)\n';
    }
    if (bindings.length === 1 && bindings[0]) {
      return this.formatSingleBinding(bindings[0]);
    }
    return this.formatMultipleBindings(bindings);
  }

  private formatSingleBinding(binding: Record<string, any>): string {
    const variables = Object.keys(binding);
    if (variables.length === 0) {
      return '✅ **Query succeeded** (no variables)\n';
    }
    let result = '✅ **Query succeeded:**\n\n';
    if (this.options.useCodeBlocks) result += '```prolog\n';
    for (const [variable, value] of Object.entries(binding)) {
      result += `${variable} = ${this.formatValue(value)}\n`;
    }
    if (this.options.useCodeBlocks) result += '```\n';
    return result;
  }

  private formatMultipleBindings(bindings: Record<string, any>[]): string {
    const allVariables = new Set<string>();
    bindings.forEach(binding => {
      Object.keys(binding).forEach(variable => allVariables.add(variable));
    });
    const variables = Array.from(allVariables).sort();
    const displayCount = Math.min(bindings.length, this.options.maxResults);
    const hasMore = bindings.length > this.options.maxResults;
    let result = `✅ **Query succeeded** (${bindings.length} solution${bindings.length === 1 ? '' : 's'}):\n\n`;
    if (variables.length === 0) {
      result += `Found ${bindings.length} solution${bindings.length === 1 ? '' : 's'} with no variable bindings.\n`;
      return result;
    }
    if (this.options.compactMode && variables.length <= 3) {
      result += this.formatCompactTable(bindings.slice(0, displayCount), variables);
    } else {
      result += this.formatDetailedList(bindings.slice(0, displayCount), variables);
    }
    if (hasMore) {
      result += `\n*... and ${bindings.length - displayCount} more solution${bindings.length - displayCount === 1 ? '' : 's'}*\n`;
    }
    return result;
  }

  private formatCompactTable(bindings: Record<string, any>[], variables: string[]): string {
    let result = '';
    if (this.options.useCodeBlocks) result += '```\n';
    const headers = variables.map(v => v.padEnd(12)).join(' | ');
    result += `| ${headers} |\n`;
    result += `|${variables.map(() => '-------------').join('|')}|\n`;
    for (let i = 0; i < bindings.length; i++) {
      const binding = bindings[i];
      if (binding) {
        const row = variables
          .map(variable => {
            const value = binding[variable];
            const formatted = value !== undefined ? this.formatValue(value) : '-';
            return formatted.substring(0, 12).padEnd(12);
          })
          .join(' | ');
        result += `| ${row} |\n`;
      }
    }
    if (this.options.useCodeBlocks) result += '```\n';
    return result;
  }

  private formatDetailedList(bindings: Record<string, any>[], variables: string[]): string {
    let result = '';
    for (let i = 0; i < bindings.length; i++) {
      const binding = bindings[i];
      if (binding) {
        result += `**Solution ${i + 1}:**\n`;
        if (this.options.useCodeBlocks) result += '```prolog\n';
        for (const variable of variables) {
          const value = binding[variable];
          if (value !== undefined) {
            result += `${variable} = ${this.formatValue(value)}\n`;
          }
        }
        if (this.options.useCodeBlocks) result += '```\n';
        result += '\n';
      }
    }
    return result;
  }

  private formatValue(value: any): string {
    if (value === null || value === undefined) return '_';
    if (typeof value === 'string') {
      if (value.includes(' ') || value.includes('(') || /^[A-Z]/.test(value)) return `'${value}'`;
      return value;
    }
    if (typeof value === 'number') return value.toString();
    if (typeof value === 'boolean') return value ? 'true' : 'false';
    if (Array.isArray(value)) return `[${value.map(v => this.formatValue(v)).join(', ')}]`;
    if (typeof value === 'object') {
      if (value.functor && value.args) {
        const args = value.args.map((arg: any) => this.formatValue(arg)).join(', ');
        return `${value.functor}(${args})`;
      }
      return JSON.stringify(value);
    }
    return String(value);
  }
}
