import { FormattingOptions } from './types';

export class CodeFormatter {
  constructor(private options: Required<FormattingOptions>) { }

  formatPrologCode(code: string, title?: string): string {
    let result = '';
    if (title) result += `**${title}:**\n\n`;
    if (this.options.useCodeBlocks) {
      result += '```prolog\n' + code;
      if (!code.endsWith('\n')) result += '\n';
      result += '```\n';
    } else {
      result += code + '\n';
    }
    return result;
  }
}
