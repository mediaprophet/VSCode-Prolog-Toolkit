import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommandRegistry } from '../chatCommandRegistry';
import { ChatCommand, ChatCommandContext } from './types';

export class UnregisterCommandCommand implements ChatCommand {
  name = '/unregister_command';
  description = 'Unregister a chat command by name.';
  arguments = [
    {
      name: 'command',
      type: 'string',
      description: 'The name of the command to unregister.',
      example: '/my_echo',
      required: true
    }
  ];
  canHandle(command: string): boolean {
    return command === 'unregister_command' || command === '/unregister_command';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext) {
    // args: commandName
    const registry = (context as any).registry as ChatCommandRegistry;
    const name = args.trim();
    if (!name) {
      stream.markdown('Please provide a command name.');
      return;
    }
    if (!registry) {
      stream.markdown('Command registry not available.');
      return;
    }
    registry.unregisterCommand(name);
    stream.markdown(`Unregistered command: **${name}**`);
  }
}
