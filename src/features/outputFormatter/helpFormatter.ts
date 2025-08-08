import { FormattingOptions } from './types';

export class HelpFormatter {
  constructor(private options: Required<FormattingOptions>) { }

  formatHelpText(predicate: string, documentation: string): string {
    let result = `📖 **Help for \`${predicate}\`:**\n\n`;
    const lines = documentation.split('\n');
    let inCodeBlock = false;
    for (const line of lines) {
      const trimmed = line.trim();
      if (
        trimmed.startsWith('?-') ||
        trimmed.includes(':-') ||
        trimmed.match(/^[a-z][a-zA-Z0-9_]*\(/)
      ) {
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
}
