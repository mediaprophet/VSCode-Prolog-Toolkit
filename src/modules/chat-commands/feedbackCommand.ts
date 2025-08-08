import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class FeedbackCommand implements ChatCommand {
  name = '/feedback';
  description = 'Send feedback to the extension authors.';
  arguments = [
    {
      name: 'message',
      type: 'string',
      description: 'Your feedback message.',
      example: 'Great extension!',
      required: true
    }
  ];
  canHandle(command: string): boolean {
    return command === 'feedback' || command === '/feedback';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    // Log feedback as a telemetry event
    if (context.telemetry) {
      context.telemetry.collect({
        command: 'feedback',
        success: true,
        error: args.trim() ? undefined : 'No feedback provided',
        timestamp: Date.now(),
      });
    }
    stream.markdown('Thank you for your feedback!');
  }
}
