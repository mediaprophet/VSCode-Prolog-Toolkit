import {
  CancellationToken,
  InlineCompletionContext,
  InlineCompletionItem,
  InlineCompletionItemProvider,
  Position,
  Range,
  TextDocument
} from 'vscode';
import { ChatCommandRegistry } from './chatCommandRegistry';

/**
 * Provides inline completions for the chat input box, suggesting commands and argument hints.
 */
export class ChatInputCompletionProvider implements InlineCompletionItemProvider {
  private registry: ChatCommandRegistry;

  constructor(registry: ChatCommandRegistry) {
    this.registry = registry;
  }

  async provideInlineCompletionItems(
    document: TextDocument,
    position: Position,
    context: InlineCompletionContext,
    token: CancellationToken
  ): Promise<InlineCompletionItem[]> {
    const line = document.lineAt(position.line).text.substring(0, position.character);
    const completions: InlineCompletionItem[] = [];

    // Suggest commands when user types '/'
    if (/^\s*\/?\w*$/.test(line)) {
      for (const cmd of this.registry.getAllCommands()) {
        completions.push(
          new InlineCompletionItem(
            cmd.name + ' ',
            new Range(position.line, 0, position.line, position.character)
          )
        );
      }
      return completions;
    }

    // Suggest argument hints if a command is detected
    const match = line.match(/^(\/\w+)(\s+)(.*)$/);
    if (match && match[1]) {
      const command = match[1];
      const handler = this.registry.findHandler(command);
      if (handler && handler.arguments && handler.arguments.length > 0) {
        const argString = match[3] || '';
        const providedArgs = argString.trim().length > 0 ? argString.trim().split(/\s+/).filter(Boolean) : [];
        if (providedArgs.length < handler.arguments.length) {
          const nextArg = handler.arguments[providedArgs.length];
          if (nextArg) {
            completions.push(
              new InlineCompletionItem(
                (providedArgs.length > 0 ? ' ' : '') + `<${nextArg.name}> `,
                new Range(position.line, position.character, position.line, position.character)
              )
            );
          }
        }
      }
    }

    return completions;
  }
}
