import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommandRegistry } from '../chatCommandRegistry';
import { ChatCommand, ChatCommandContext } from './types';

// Simple dynamic echo command for demonstration
class DynamicEchoCommand implements ChatCommand {
  name: string;
  description: string;
  constructor(name: string) {
    this.name = name.startsWith('/') ? name : '/' + name;
    this.description = `Dynamic echo command for ${this.name}`;
  }
  canHandle(command: string): boolean { return command === this.name || command === this.name.replace(/^\//, ''); }
  async handle(args: string, request: ChatRequest, stream: ChatResponseStream) {
    stream.markdown(`[${this.name}] Echo: ${args}`);
  }
}

export class RegisterCommandCommand implements ChatCommand {
  name = '/register_command';
  description = 'Register a new dynamic echo command by name.';
  arguments = [
    {
      name: 'command',
      type: 'string',
      description: 'The name of the new command to register.',
      example: 'my_echo',
      required: true
    }
  ];

  canHandle(command: string): boolean {
    return command === '/register_command' || command === 'register_command';
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
    registry.registerCommand(new DynamicEchoCommand(name));
    stream.markdown(`Registered new command: **${name}** (echo handler)`);
  }
}
