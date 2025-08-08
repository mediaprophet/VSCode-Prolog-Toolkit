import { CancellationToken, ChatRequest, ChatResponseStream, Task, tasks } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class RunTaskCommand implements ChatCommand {
  name = '/run_task';
  description = 'Run a VS Code task by name or type.';
  arguments = [
    {
      name: 'task',
      type: 'string',
      description: 'The name or type of the VS Code task to run.',
      example: 'build',
      required: true
    }
  ];
  canHandle(command: string): boolean {
    return command === 'run_task' || command === '/run_task';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    try {
      const allTasks = await tasks.fetchTasks();
      const arg = args.trim();
      let matched: Task | undefined = allTasks.find(t => t.name === arg || t.definition.type === arg);
      if (!matched) {
        stream.markdown(`No task found matching: ${arg}`);
        return;
      }
      await tasks.executeTask(matched);
      stream.markdown(`Task **${matched.name}** started.`);
    } catch (err) {
      stream.markdown('Failed to run task: ' + err);
    }
  }
}
