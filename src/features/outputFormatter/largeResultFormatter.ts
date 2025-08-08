import { ResultFormatter } from './resultFormatter';
import { FormattingOptions, PrologResult } from './types';

export class LargeResultFormatter {
  private resultFormatter: ResultFormatter;
  constructor(private options: Required<FormattingOptions>) {
    this.resultFormatter = new ResultFormatter(options);
  }

  formatLargeResultSet(results: any[], totalCount?: number): string {
    const displayCount = results.length;
    const actualTotal = totalCount || displayCount;
    let result = '';
    if (actualTotal > displayCount) {
      result += `📊 **Large Result Set** (showing first ${displayCount} of ${actualTotal}):\n\n`;
      result += `*💡 Performance tip: Results are automatically chunked for better performance*\n\n`;
    } else {
      result += `📊 **Results** (${displayCount} total):\n\n`;
    }
    const useUltraCompact = displayCount > 100;
    const useCompact = displayCount > 30;
    if (useUltraCompact) {
      result += '```\n';
      results.slice(0, 50).forEach((item, index) => {
        result += `${index + 1}. ${this.formatValueCompact(item)}\n`;
      });
      if (displayCount > 50) {
        result += `... and ${displayCount - 50} more results\n`;
      }
      result += '```\n';
    } else if (useCompact) {
      result += '```\n';
      results.forEach((item, index) => {
        result += `${index + 1}. ${this.formatValueCompact(item)}\n`;
      });
      result += '```\n';
    } else {
      const resultObj: PrologResult = {
        type: 'multiple',
        bindings: results,
        count: displayCount,
      };
      result += this.resultFormatter.format(resultObj);
    }
    return result;
  }

  private formatValueCompact(value: any): string {
    if (typeof value === 'object' && value !== null) {
      if (Array.isArray(value)) {
        return `[${value.map(v => this.resultFormatter['bindingsFormatter']['formatValue'](v)).join(',')}]`;
      } else {
        const entries = Object.entries(value);
        if (entries.length <= 3) {
          return entries.map(([k, v]) => `${k}=${this.resultFormatter['bindingsFormatter']['formatValue'](v)}`).join(', ');
        } else {
          return `{${entries.length} bindings}`;
        }
      }
    }
    return this.resultFormatter['bindingsFormatter']['formatValue'](value);
  }
}
