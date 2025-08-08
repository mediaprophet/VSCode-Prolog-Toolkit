import { FormattingOptions } from './types';

export class PaginationFormatter {
  constructor(private options: Required<FormattingOptions>) { }

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
        result += '| # | Result |\n|---|--------|\n';
        results.forEach((item, index) => {
          const globalIndex = offset + index + 1;
          const itemStr = typeof item === 'object' ? JSON.stringify(item) : String(item);
          result += `| ${globalIndex} | \`${itemStr}\` |\n`;
        });
      }
    }
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
}
