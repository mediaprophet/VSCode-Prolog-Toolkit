import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommandRegistry } from '../chatCommandRegistry';
import { ChatCommand, ChatCommandContext } from './types';

export class ListCommandsCommand implements ChatCommand {
  name = '/list-commands';
  description = 'List all available chat commands.';
  arguments = [];
  private registry: ChatCommandRegistry;

  constructor(registry: ChatCommandRegistry) {
    this.registry = registry;
  }

  canHandle(command: string): boolean {
    return command === '/list-commands' || command === 'list-commands';
  }

  async handle(
    args: string,
    request: ChatRequest,
    stream: ChatResponseStream,
    token: CancellationToken,
    context: ChatCommandContext
  ): Promise<void> {
    const commands = this.registry.getAllCommands();
    stream.markdown('**Available Chat Commands:**\n');
    for (const cmd of commands) {
      stream.markdown(`- \`${cmd.name}\`: ${cmd.description || ''}`);
    }
    stream.markdown('\nType `/help <command>` for more info.');
  }
}
