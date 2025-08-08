import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';

export interface ChatCommandContext {
  prologBackend: any;
  telemetry: any;
  activeFile?: {
    fileName: string;
    languageId: string;
    isDirty: boolean;
    lineCount: number;
    selection: any;
    content?: string;
  } | undefined;
  workspaceFolders?: string[];
  settings?: Record<string, any>;
  chatHistory?: Array<{ prompt: string; response: string; timestamp: number }>;
}

export interface ChatCommandArgument {
  /** Argument name (e.g. 'goal', 'file') */
  name: string;
  /** Argument type (e.g. 'string', 'file', 'number', 'boolean') */
  type: string;
  /** Description of the argument */
  description: string;
  /** Example usage for this argument */
  example?: string;
  /** Whether this argument is required */
  required?: boolean;
}

export interface ChatCommand {
  /**
   * The primary name of the command (e.g. '/query').
   */
  name: string;
  /**
   * A short description of what the command does.
   */
  description?: string;
  /**
   * Argument metadata for autocomplete and usage hints.
   */
  arguments?: ChatCommandArgument[];
  canHandle(command: string): boolean;
  handle(
    args: string,
    request: ChatRequest,
    stream: ChatResponseStream,
    token: CancellationToken,
    context: ChatCommandContext
  ): Promise<void>;
}
