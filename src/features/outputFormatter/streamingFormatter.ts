import { ResultFormatter } from './resultFormatter';
import { FormattingOptions, PrologResult } from './types';

export class StreamingFormatter {
  private resultFormatter: ResultFormatter;
  constructor(private options: Required<FormattingOptions>) {
    this.resultFormatter = new ResultFormatter(options);
  }

  formatStreamingOutput(
    chunk: any[],
    isFirst: boolean,
    isLast: boolean,
    totalCount?: number,
    chunkIndex?: number
  ): string {
    let result = '';
    if (isFirst && totalCount !== undefined) {
      result += `📊 **Streaming results** (${totalCount} total):\n\n`;
    }
    if (chunk.length > 0) {
      if (!isFirst && chunkIndex !== undefined) {
        result += `\n**Chunk ${chunkIndex + 1}:**\n`;
      }
      const chunkResult: PrologResult = {
        type: 'multiple',
        bindings: chunk,
        count: chunk.length,
      };
      const originalCompactMode = this.options.compactMode;
      this.options.compactMode = totalCount ? totalCount > 50 : chunk.length > 20;
      result += this.resultFormatter.format(chunkResult);
      this.options.compactMode = originalCompactMode;
    }
    if (isLast) {
      result += '\n✅ **Streaming complete**\n';
    } else {
      result += '\n⏳ *Loading more results...*\n';
    }
    return result;
  }
}
