import { ChatResponseStream } from 'vscode';

// Format help response
export function formatHelpResponse(doc: any, stream: ChatResponseStream) {
  stream.markdown(`# ${doc.name}/${doc.arity}\n\n`);

  // Status badges
  const badges = [];
  if (doc.is_builtin) badges.push('**Built-in**');
  if (doc.module && doc.module !== 'user') badges.push(`**Module**: \`${doc.module}\``);
  if (doc.deterministic) badges.push('**Deterministic**');

  if (badges.length > 0) {
    stream.markdown(`${badges.join(' | ')}\n\n`);
  }

  if (doc.summary) {
    stream.markdown(`## Description\n\n${doc.summary}\n\n`);
  }

  if (doc.args && doc.args.length > 0) {
    stream.markdown('## Arguments\n\n');
    doc.args.forEach((arg: any, index: number) => {
      stream.markdown(`**${index + 1}. ${arg.name}** - ${arg.description}\n\n`);
    });
  }

  if (doc.examples && doc.examples.length > 0) {
    stream.markdown('## Examples\n\n');
    doc.examples.forEach((example: string, index: number) => {
      stream.markdown(`**Example ${index + 1}:**\n\`\`\`prolog\n${example}\n\`\`\`\n\n`);
    });
  }

  if (doc.see_also && doc.see_also.length > 0) {
    stream.markdown('## See Also\n\n');
    doc.see_also.forEach((related: string) => {
      stream.markdown(`- \`${related}\`\n`);
    });
    stream.markdown('\n');
  }
}
