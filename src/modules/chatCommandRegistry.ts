import { ActiveFileCommand } from './chat-commands/activeFileCommand';
import { BatchCommand } from './chat-commands/batchCommand';
import { ClpCommand } from './chat-commands/clpCommand';
import { ConsultCommand } from './chat-commands/consultCommand';
import { DebugCommand } from './chat-commands/debugCommand';
import { DescribeCommand } from './chat-commands/describeCommand';
import { EnvCommand } from './chat-commands/envCommand';
import { ErrorsCommand } from './chat-commands/errorsCommand';
import { FeedbackCommand } from './chat-commands/feedbackCommand';
import { HelpCommand } from './chat-commands/helpCommand';
import { HistoryCommand } from './chat-commands/historyCommand';
import { InstallCommand } from './chat-commands/installCommand';
import { ListCommandsCommand } from './chat-commands/listCommandsCommand';
import { ListFilesCommand } from './chat-commands/listFilesCommand';
import { LogsCommand } from './chat-commands/logsCommand';
import { N3ExplainCommand } from './chat-commands/n3ExplainCommand';
import { N3ListCommand } from './chat-commands/n3ListCommand';
import { N3LoadCommand } from './chat-commands/n3LoadCommand';
import { N3ReasonCommand } from './chat-commands/n3ReasonCommand';
import { OpenSettingsCommand } from './chat-commands/openSettingsCommand';
import { ProbabilisticCommand } from './chat-commands/probabilisticCommand';
import { ProblemsCommand } from './chat-commands/problemsCommand';
import { ProgressCommand } from './chat-commands/progressCommand';
import { QueryCommand } from './chat-commands/queryCommand';
import { RegisterCommandCommand } from './chat-commands/registerCommandCommand';
import { ReloadBackendCommand } from './chat-commands/reloadBackendCommand';
import { RestartDebugCommand } from './chat-commands/restartDebugCommand';
import { RunTaskCommand } from './chat-commands/runTaskCommand';
import { StatusCommand } from './chat-commands/statusCommand';
import { SubscribeCommand } from './chat-commands/subscribeCommand';
import { SuggestCommand } from './chat-commands/suggestCommand';
import { TelemetryStatsCommand } from './chat-commands/telemetryStatsCommand';
import { TroubleshootCommand } from './chat-commands/troubleshootCommand';
import { ChatCommand } from './chat-commands/types';
import { UnregisterCommandCommand } from './chat-commands/unregisterCommandCommand';

export class ChatCommandRegistry {
  private commands: ChatCommand[];

  constructor() {
    this.commands = [];
    this.registerAllCommands();
    // Register the list-commands command with access to this registry (only once, after all others)
    this.commands.push(new ListCommandsCommand(this));
  }

  /**
   * Helper to register all built-in commands in a single place for maintainability.
   */
  private registerAllCommands() {
    this.commands.push(
      new QueryCommand(),
      new ConsultCommand(),
      new HelpCommand(),
      new StatusCommand(),
      new N3LoadCommand(),
      new N3ListCommand(),
      new N3ReasonCommand(),
      new N3ExplainCommand(),
      new ClpCommand(),
      new ProbabilisticCommand(),
      new BatchCommand(),
      new HistoryCommand(),
      new EnvCommand(),
      new ListFilesCommand(),
      new ActiveFileCommand(),
      new DebugCommand(),
      new TroubleshootCommand(),
      new LogsCommand(),
      new ErrorsCommand(),
      new RunTaskCommand(),
      new InstallCommand(),
      new ReloadBackendCommand(),
      new RestartDebugCommand(),
      new OpenSettingsCommand(),
      new FeedbackCommand(),
      new TelemetryStatsCommand(),
      new SuggestCommand(),
      new ProgressCommand(),
      new SubscribeCommand(),
      new RegisterCommandCommand(),
      new UnregisterCommandCommand(),
      new DescribeCommand(),
      new ProblemsCommand(),
    );
  }

  /**
   * Returns all registered chat commands.
   */
  getAllCommands(): ChatCommand[] {
    return this.commands;
  }

  findHandler(command: string): ChatCommand | undefined {
    return this.commands.find(cmd => cmd.canHandle(command));
  }

  registerCommand(cmd: ChatCommand) {
    this.commands.push(cmd);
  }

  unregisterCommand(commandName: string) {
    this.commands = this.commands.filter(cmd => !cmd.canHandle(commandName));
  }
}
