
import { BreakpointManager } from './BreakpointManager';
import { OutputFilter } from './OutputFilter';
import { ProcessManager, PrologProcessOptions } from './ProcessManager';
import { ProtocolAdapter } from './ProtocolAdapter';
import { StackFrameManager } from './StackFrameManager';
import { VariableManager } from './VariableManager';

export class DebuggerController {
  public readonly processManager: ProcessManager;
  public readonly protocolAdapter: ProtocolAdapter;
  public readonly breakpointManager: BreakpointManager;
  public readonly stackFrameManager: StackFrameManager;
  public readonly variableManager: VariableManager;
  public readonly outputFilter: typeof OutputFilter;

  constructor(options: PrologProcessOptions) {
    this.processManager = new ProcessManager(options);
    this.protocolAdapter = new ProtocolAdapter();
    this.breakpointManager = new BreakpointManager();
    this.stackFrameManager = new StackFrameManager();
    this.variableManager = new VariableManager();
    this.outputFilter = OutputFilter;
  }

  // Example: expose a clean API for the debug session
  public startProcess(): Promise<void> {
    return this.processManager.start();
  }

  public killProcess(): void {
    this.processManager.kill();
  }

  // Add more orchestrating methods as needed
}
