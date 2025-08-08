import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

export class ClpCommand implements ChatCommand {
  name = '/clp';
  description = 'Solve a CLP (Constraint Logic Programming) problem.';
  arguments = [
    {
      name: 'constraints',
      type: 'string',
      description: 'CLP constraints (comma-separated, e.g. X+Y#=10, X#>Y)',
      example: 'X+Y#=10, X#>Y',
      required: true
    },
    {
      name: 'domain',
      type: 'string',
      description: 'Constraint domain (fd, r, q)',
      example: 'fd',
      required: false
    },
    {
      name: 'variables',
      type: 'string',
      description: 'Variables to solve for (comma-separated, e.g. X,Y)',
      example: 'X,Y',
      required: false
    }
  ];

  canHandle(command: string) {
    return command === '/clp' || command === 'clp';
  }

  async handle(
    args: string,
    request: ChatRequest,
    stream: ChatResponseStream,
    token: CancellationToken,
    context: ChatCommandContext
  ) {
    const backend = context.prologBackend;
    if (!backend) {
      stream.markdown('⚠️ **Backend Error**: Prolog backend not initialized.');
      return;
    }
    // Parse args: constraints, domain, variables
    const [constraintsRaw, domainRaw, variablesRaw] = args.split('|').map(s => s.trim());
    const constraints = constraintsRaw ? constraintsRaw.split(',').map(s => s.trim()) : [];
    const domain = domainRaw || 'fd';
    const variables = variablesRaw ? variablesRaw.split(',').map(s => s.trim()) : [];
    try {
      const result = await backend.sendRequest('clp_constraint_solving', {
        constraints,
        domain,
        variables
      });
      stream.markdown('**CLP Solution:**\n' + '```json\n' + JSON.stringify(result, null, 2) + '\n```');
    } catch (err) {
      stream.markdown('❌ **Error running CLP:** ' + (err instanceof Error ? err.message : String(err)));
    }
  }
}
