import { MarkdownString } from 'vscode';

export interface PrologResult {
  type: 'success' | 'failure' | 'error' | 'multiple';
  bindings?: Record<string, any>[];
  message?: string;
  error?: string;
  count?: number;
  hasMore?: boolean;
}

export interface FormattingOptions {
  maxResults?: number;
  maxLineLength?: number;
  useCodeBlocks?: boolean;
  showLineNumbers?: boolean;
  highlightVariables?: boolean;
  compactMode?: boolean;
  locale?: string;
}

export class OutputFormatter {
  private options: Required<FormattingOptions>;

  constructor(options: FormattingOptions = {}) {
    this.options = {
      maxResults: options.maxResults ?? 50,
      maxLineLength: options.maxLineLength ?? 120,
      useCodeBlocks: options.useCodeBlocks ?? true,
      showLineNumbers: options.showLineNumbers ?? false,
      highlightVariables: options.highlightVariables ?? true,
      compactMode: options.compactMode ?? false,
      locale: options.locale ?? 'en'
    };
  }

  /**
   * Format Prolog query results for chat display
   */
  formatQueryResult(result: PrologResult): string {
    switch (result.type) {
      case 'success':
        return this.formatSuccessResult(result);
      case 'failure':
        return this.formatFailureResult(result);
      case 'error':
        return this.formatErrorResult(result);
      case 'multiple':
        return this.formatMultipleResults(result);
      default:
        return this.formatUnknownResult(result);
    }
  }

  /**
   * Format variable bindings as a readable table or list
   */
  formatVariableBindings(bindings: Record<string, any>[]): string {
    if (!bindings || bindings.length === 0) {
      return '✅ **Query succeeded** (no variable bindings)\n';
    }

    if (bindings.length === 1 && bindings[0]) {
      return this.formatSingleBinding(bindings[0]);
    }

    return this.formatMultipleBindings(bindings);
  }

  /**
   * Format a single variable binding
   */
  private formatSingleBinding(binding: Record<string, any>): string {
    const variables = Object.keys(binding);
    if (variables.length === 0) {
      return '✅ **Query succeeded** (no variables)\n';
    }

    let result = '✅ **Query succeeded:**\n\n';
    
    if (this.options.useCodeBlocks) {
      result += '```prolog\n';
    }

    for (const [variable, value] of Object.entries(binding)) {
      const formattedValue = this.formatValue(value);
      if (this.options.highlightVariables) {
        result += `${variable} = ${formattedValue}\n`;
      } else {
        result += `${variable} = ${formattedValue}\n`;
      }
    }

    if (this.options.useCodeBlocks) {
      result += '```\n';
    }

    return result;
  }

  /**
   * Format multiple variable bindings as a table
   */
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

    // Use table format for multiple solutions
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

  /**
   * Format bindings as a compact table
   */
  private formatCompactTable(bindings: Record<string, any>[], variables: string[]): string {
    let result = '';

    if (this.options.useCodeBlocks) {
      result += '```\n';
    }

    // Header
    const headers = variables.map(v => v.padEnd(12)).join(' | ');
    result += `| ${headers} |\n`;
    result += `|${variables.map(() => '-------------').join('|')}|\n`;

    // Rows
    for (let i = 0; i < bindings.length; i++) {
      const binding = bindings[i];
      if (binding) {
        const row = variables.map(variable => {
          const value = binding[variable];
          const formatted = value !== undefined ? this.formatValue(value) : '-';
          return formatted.substring(0, 12).padEnd(12);
        }).join(' | ');
        
        result += `| ${row} |\n`;
      }
    }

    if (this.options.useCodeBlocks) {
      result += '```\n';
    }

    return result;
  }

  /**
   * Format bindings as a detailed list
   */
  private formatDetailedList(bindings: Record<string, any>[], variables: string[]): string {
    let result = '';

    for (let i = 0; i < bindings.length; i++) {
      const binding = bindings[i];
      if (binding) {
        result += `**Solution ${i + 1}:**\n`;
        
        if (this.options.useCodeBlocks) {
          result += '```prolog\n';
        }

        for (const variable of variables) {
          const value = binding[variable];
          if (value !== undefined) {
            const formattedValue = this.formatValue(value);
            result += `${variable} = ${formattedValue}\n`;
          }
        }

        if (this.options.useCodeBlocks) {
          result += '```\n';
        }
        
        result += '\n';
      }
    }

    return result;
  }

  /**
   * Format a Prolog value for display
   */
  private formatValue(value: any): string {
    if (value === null || value === undefined) {
      return '_';
    }

    if (typeof value === 'string') {
      // Handle quoted atoms and strings
      if (value.includes(' ') || value.includes('(') || /^[A-Z]/.test(value)) {
        return `'${value}'`;
      }
      return value;
    }

    if (typeof value === 'number') {
      return value.toString();
    }

    if (typeof value === 'boolean') {
      return value ? 'true' : 'false';
    }

    if (Array.isArray(value)) {
      return `[${value.map(v => this.formatValue(v)).join(', ')}]`;
    }

    if (typeof value === 'object') {
      // Handle compound terms
      if (value.functor && value.args) {
        const args = value.args.map((arg: any) => this.formatValue(arg)).join(', ');
        return `${value.functor}(${args})`;
      }
      
      // Handle other objects
      return JSON.stringify(value);
    }

    return String(value);
  }

  /**
   * Format success result
   */
  private formatSuccessResult(result: PrologResult): string {
    if (result.bindings && result.bindings.length > 0) {
      return this.formatVariableBindings(result.bindings);
    }
    
    return '✅ **Query succeeded**\n' + (result.message ? `\n${result.message}\n` : '');
  }

  /**
   * Format failure result
   */
  private formatFailureResult(result: PrologResult): string {
    return '❌ **Query failed**\n' + (result.message ? `\n${result.message}\n` : '');
  }

  /**
   * Format error result
   */
  private formatErrorResult(result: PrologResult): string {
    let output = '🚫 **Query error**\n\n';
    
    if (result.error) {
      if (this.options.useCodeBlocks) {
        output += '```\n';
        output += result.error;
        output += '\n```\n';
      } else {
        output += result.error + '\n';
      }
    }
    
    if (result.message) {
      output += '\n' + result.message + '\n';
    }
    
    return output;
  }

  /**
   * Format multiple results
   */
  private formatMultipleResults(result: PrologResult): string {
    if (result.bindings) {
      return this.formatVariableBindings(result.bindings);
    }
    
    return `✅ **Query succeeded** (${result.count || 0} results)\n`;
  }

  /**
   * Format unknown result type
   */
  private formatUnknownResult(result: PrologResult): string {
    return `⚠️ **Unknown result type**: ${JSON.stringify(result)}\n`;
  }

  /**
   * Format Prolog code with syntax highlighting
   */
  formatPrologCode(code: string, title?: string): string {
    let result = '';
    
    if (title) {
      result += `**${title}:**\n\n`;
    }
    
    if (this.options.useCodeBlocks) {
      result += '```prolog\n';
      result += code;
      if (!code.endsWith('\n')) {
        result += '\n';
      }
      result += '```\n';
    } else {
      result += code + '\n';
    }
    
    return result;
  }

  /**
   * Format help documentation
   */
  formatHelpText(predicate: string, documentation: string): string {
    let result = `📖 **Help for \`${predicate}\`:**\n\n`;
    
    // Parse and format the documentation
    const lines = documentation.split('\n');
    let inCodeBlock = false;
    
    for (const line of lines) {
      const trimmed = line.trim();
      
      // Detect code examples
      if (trimmed.startsWith('?-') || trimmed.includes(':-') || trimmed.match(/^[a-z][a-zA-Z0-9_]*\(/)) {
        if (!inCodeBlock && this.options.useCodeBlocks) {
          result += '```prolog\n';
          inCodeBlock = true;
        }
        result += line + '\n';
      } else {
        if (inCodeBlock && this.options.useCodeBlocks) {
          result += '```\n';
          inCodeBlock = false;
        }
        result += line + '\n';
      }
    }
    
    if (inCodeBlock && this.options.useCodeBlocks) {
      result += '```\n';
    }
    
    return result;
  }

  /**
   * Format streaming output for large results
   */
  formatStreamingOutput(chunk: any[], isFirst: boolean, isLast: boolean, totalCount?: number, chunkIndex?: number): string {
    let result = '';
    
    if (isFirst && totalCount !== undefined) {
      result += `📊 **Streaming results** (${totalCount} total):\n\n`;
    }
    
    if (chunk.length > 0) {
      // Add chunk header for multi-chunk results
      if (!isFirst && chunkIndex !== undefined) {
        result += `\n**Chunk ${chunkIndex + 1}:**\n`;
      }
      
      // Format this chunk with streaming-optimized settings
      const chunkResult: PrologResult = {
        type: 'multiple',
        bindings: chunk,
        count: chunk.length
      };
      
      // Use compact mode for streaming to reduce visual clutter
      const originalCompactMode = this.options.compactMode;
      this.options.compactMode = totalCount ? totalCount > 50 : chunk.length > 20;
      
      result += this.formatQueryResult(chunkResult);
      
      // Restore original compact mode setting
      this.options.compactMode = originalCompactMode;
    }
    
    if (isLast) {
      result += '\n✅ **Streaming complete**\n';
    } else {
      result += '\n⏳ *Loading more results...*\n';
    }
    
    return result;
  }

  /**
   * Format paginated output with navigation info
   */
  formatPaginatedOutput(
    results: any[],
    pagination: {
      total_count: number;
      offset: number;
      limit: number;
      has_more: boolean;
      next_offset?: number | null;
    }
  ): string {
    let result = '';
    
    const { total_count, offset, limit, has_more, next_offset } = pagination;
    const endIndex = offset + results.length;
    
    result += `📄 **Page Results** (${offset + 1}-${endIndex} of ${total_count}):\n\n`;
    
    if (results.length > 0) {
      // Use appropriate formatting based on result size
      const useCompactFormat = results.length > 15 || total_count > 100;
      
      if (useCompactFormat) {
        result += '```\n';
        results.forEach((item, index) => {
          const globalIndex = offset + index + 1;
          if (typeof item === 'object' && item !== null) {
            result += `${globalIndex}. `;
            Object.entries(item).forEach(([key, value], entryIndex) => {
              if (entryIndex > 0) result += ', ';
              result += `${key}=${value}`;
            });
            result += '\n';
          } else {
            result += `${globalIndex}. ${item}\n`;
          }
        });
        result += '```\n';
      } else {
        // Table format for smaller results
        result += '| # | Result |\n|---|--------|\n';
        results.forEach((item, index) => {
          const globalIndex = offset + index + 1;
          const itemStr = typeof item === 'object' ? JSON.stringify(item) : String(item);
          result += `| ${globalIndex} | \`${itemStr}\` |\n`;
        });
      }
    }
    
    // Add navigation info
    result += `\n**Navigation:**\n`;
    result += `- Current: ${offset + 1}-${endIndex} of ${total_count}\n`;
    if (has_more && next_offset !== null) {
      result += `- Next page: Use \`--offset ${next_offset} --limit ${limit}\`\n`;
    }
    if (offset > 0) {
      const prevOffset = Math.max(0, offset - limit);
      result += `- Previous page: Use \`--offset ${prevOffset} --limit ${limit}\`\n`;
    }
    
    return result;
  }

  /**
   * Format large result set with performance optimizations
   */
  formatLargeResultSet(results: any[], totalCount?: number, chunkSize?: number): string {
    const displayCount = results.length;
    const actualTotal = totalCount || displayCount;
    
    let result = '';
    
    if (actualTotal > displayCount) {
      result += `📊 **Large Result Set** (showing first ${displayCount} of ${actualTotal}):\n\n`;
      result += `*💡 Performance tip: Results are automatically chunked for better performance*\n\n`;
    } else {
      result += `📊 **Results** (${displayCount} total):\n\n`;
    }
    
    // Use performance-optimized formatting
    const useUltraCompact = displayCount > 100;
    const useCompact = displayCount > 30;
    
    if (useUltraCompact) {
      // Ultra-compact format for very large results
      result += '```\n';
      results.slice(0, 50).forEach((item, index) => {
        result += `${index + 1}. ${this.formatValueCompact(item)}\n`;
      });
      if (displayCount > 50) {
        result += `... and ${displayCount - 50} more results\n`;
      }
      result += '```\n';
    } else if (useCompact) {
      // Compact format
      result += '```\n';
      results.forEach((item, index) => {
        result += `${index + 1}. ${this.formatValueCompact(item)}\n`;
      });
      result += '```\n';
    } else {
      // Standard format for smaller results
      const resultObj: PrologResult = {
        type: 'multiple',
        bindings: results,
        count: displayCount
      };
      result += this.formatQueryResult(resultObj);
    }
    
    return result;
  }

  /**
   * Format value in compact form
   */
  private formatValueCompact(value: any): string {
    if (typeof value === 'object' && value !== null) {
      if (Array.isArray(value)) {
        return `[${value.map(v => this.formatValue(v)).join(',')}]`;
      } else {
        const entries = Object.entries(value);
        if (entries.length <= 3) {
          return entries.map(([k, v]) => `${k}=${this.formatValue(v)}`).join(', ');
        } else {
          return `{${entries.length} bindings}`;
        }
      }
    }
    return this.formatValue(value);
  }

  /**
   * Truncate long output with continuation indicator
   */
  truncateOutput(output: string, maxLength: number = 2000): string {
    if (output.length <= maxLength) {
      return output;
    }
    
    const truncated = output.substring(0, maxLength - 50);
    const lastNewline = truncated.lastIndexOf('\n');
    const cutPoint = lastNewline > maxLength - 200 ? lastNewline : truncated.length;
    
    return truncated.substring(0, cutPoint) + '\n\n*... (output truncated)*\n';
  }

  /**
   * Update formatting options
   */
  updateOptions(newOptions: Partial<FormattingOptions>): void {
    this.options = { ...this.options, ...newOptions };
  }

  /**
   * Get current formatting options
   */
  getOptions(): FormattingOptions {
    return { ...this.options };
  }
}

/**
 * Default formatter instance
 */
export const defaultFormatter = new OutputFormatter();

/**
 * Create a compact formatter for inline results
 */
export const compactFormatter = new OutputFormatter({
  compactMode: true,
  maxResults: 10,
  useCodeBlocks: false
});

/**
 * Create a detailed formatter for comprehensive output
 */
export const detailedFormatter = new OutputFormatter({
  compactMode: false,
  maxResults: 100,
  useCodeBlocks: true,
  showLineNumbers: true,
  highlightVariables: true
});