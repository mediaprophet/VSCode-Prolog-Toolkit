import { BindingsFormatter } from './bindingsFormatter';
import { CodeFormatter } from './codeFormatter';
import { HelpFormatter } from './helpFormatter';
import { LargeResultFormatter } from './largeResultFormatter';
import { PaginationFormatter } from './paginationFormatter';
import { ResultFormatter } from './resultFormatter';
import { StreamingFormatter } from './streamingFormatter';
import { FormattingOptions, PrologResult } from './types';

export class OutputFormatter {
  private options: Required<FormattingOptions>;
  private bindingsFormatter: BindingsFormatter;
  private resultFormatter: ResultFormatter;
  private codeFormatter: CodeFormatter;
  private helpFormatter: HelpFormatter;
  private streamingFormatter: StreamingFormatter;
  private paginationFormatter: PaginationFormatter;
  private largeResultFormatter: LargeResultFormatter;

  constructor(options: FormattingOptions = {}) {
    this.options = {
      maxResults: options.maxResults ?? 50,
      maxLineLength: options.maxLineLength ?? 120,
      useCodeBlocks: options.useCodeBlocks ?? true,
      showLineNumbers: options.showLineNumbers ?? false,
      highlightVariables: options.highlightVariables ?? true,
      compactMode: options.compactMode ?? false,
      locale: options.locale ?? 'en',
    };
    this.bindingsFormatter = new BindingsFormatter(this.options);
    this.resultFormatter = new ResultFormatter(this.options);
    this.codeFormatter = new CodeFormatter(this.options);
    this.helpFormatter = new HelpFormatter(this.options);
    this.streamingFormatter = new StreamingFormatter(this.options);
    this.paginationFormatter = new PaginationFormatter(this.options);
    this.largeResultFormatter = new LargeResultFormatter(this.options);
  }

  formatQueryResult(result: PrologResult): string {
    return this.resultFormatter.format(result);
  }

  formatVariableBindings(bindings: Record<string, any>[]): string {
    return this.bindingsFormatter.format(bindings);
  }

  formatPrologCode(code: string, title?: string): string {
    return this.codeFormatter.formatPrologCode(code, title);
  }

  formatHelpText(predicate: string, documentation: string): string {
    return this.helpFormatter.formatHelpText(predicate, documentation);
  }

  formatStreamingOutput(
    chunk: any[],
    isFirst: boolean,
    isLast: boolean,
    totalCount?: number,
    chunkIndex?: number
  ): string {
    return this.streamingFormatter.formatStreamingOutput(chunk, isFirst, isLast, totalCount, chunkIndex);
  }

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
    return this.paginationFormatter.formatPaginatedOutput(results, pagination);
  }

  formatLargeResultSet(results: any[], totalCount?: number): string {
    return this.largeResultFormatter.formatLargeResultSet(results, totalCount);
  }

  updateOptions(newOptions: Partial<FormattingOptions>): void {
    this.options = { ...this.options, ...newOptions };
  }

  getOptions(): FormattingOptions {
    return { ...this.options };
  }
}

export const defaultFormatter = new OutputFormatter();
export const compactFormatter = new OutputFormatter({ compactMode: true, maxResults: 10, useCodeBlocks: false });
export const detailedFormatter = new OutputFormatter({ compactMode: false, maxResults: 100, useCodeBlocks: true, showLineNumbers: true, highlightVariables: true });
