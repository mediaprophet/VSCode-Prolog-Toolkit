import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
  RevealOutputChannelOn
} from 'vscode-languageclient/node';
import { ExtensionContext, workspace, window } from 'vscode';
import * as path from 'path';

export class PrologLSPClient {
  private client: LanguageClient | null = null;
  private context: ExtensionContext;

  constructor(context: ExtensionContext) {
    this.context = context;
  }

  public async start(): Promise<void> {
    if (this.client) {
      return; // Already started
    }

    try {
      // The server is implemented as a separate Node.js module
      const serverModule = this.context.asAbsolutePath(
        path.join('out', 'pub', 'features', 'prologLSPServer.js')
      );

      // The debug options for the server
      const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

      // If the extension is launched in debug mode then the debug server options are used
      // Otherwise the run options are used
      const serverOptions: ServerOptions = {
        run: { module: serverModule, transport: TransportKind.ipc },
        debug: {
          module: serverModule,
          transport: TransportKind.ipc,
          options: debugOptions
        }
      };

      // Options to control the language client
      const clientOptions: LanguageClientOptions = {
        // Register the server for Prolog documents
        documentSelector: [
          { scheme: 'file', language: 'prolog' },
          { scheme: 'untitled', language: 'prolog' }
        ],
        synchronize: {
          // Notify the server about file changes to '.pl', '.pro', '.prolog' files contained in the workspace
          fileEvents: workspace.createFileSystemWatcher('**/*.{pl,pro,prolog,plt,ecl}')
        },
        revealOutputChannelOn: RevealOutputChannelOn.Never,
        initializationOptions: {
          // Pass configuration to the server
          settings: workspace.getConfiguration('prolog')
        },
        middleware: {
          // Add custom middleware for enhanced functionality
          provideCompletionItem: async (document, position, context, token, next) => {
            const result = await next(document, position, context, token);
            
            // Enhance completions with custom logic if needed
            if (Array.isArray(result)) {
              return this.enhanceCompletions(result, document, position);
            }
            
            return result;
          },
          
          provideHover: async (document, position, token, next) => {
            const result = await next(document, position, token);
            
            // Enhance hover information
            if (result) {
              return this.enhanceHover(result, document, position);
            }
            
            return result;
          }
        }
      };

      // Create the language client and start the client.
      this.client = new LanguageClient(
        'prologLSP',
        'Prolog Language Server',
        serverOptions,
        clientOptions
      );

      // Register client event handlers
      this.registerEventHandlers();

      // Start the client. This will also launch the server
      await this.client.start();
      
      window.showInformationMessage('Prolog LSP Server started successfully');
      
    } catch (error) {
      window.showErrorMessage(`Failed to start Prolog LSP Server: ${error}`);
      throw error;
    }
  }

  public async stop(): Promise<void> {
    if (this.client) {
      await this.client.stop();
      this.client = null;
    }
  }

  public async restart(): Promise<void> {
    await this.stop();
    await this.start();
  }

  public isRunning(): boolean {
    return this.client !== null && this.client.state === 2; // Running state
  }

  private registerEventHandlers(): void {
    if (!this.client) {
      return;
    }

    // Handle server ready - use the client.start() promise instead of onReady()
    // The onReady() method has been removed in newer versions of vscode-languageclient
    // We'll register custom handlers after the client starts successfully
    
    // Handle server errors
    this.client.onDidChangeState((event) => {
      console.log(`LSP Client state changed: ${event.oldState} -> ${event.newState}`);
      
      if (event.newState === 2) { // Running state - register handlers when client becomes ready
        console.log('Prolog LSP Server is ready');
        this.registerCustomHandlers();
      } else if (event.newState === 3) { // Stopped state
        window.showWarningMessage('Prolog LSP Server stopped unexpectedly');
      }
    });
  }

  private registerCustomHandlers(): void {
    if (!this.client) {
      return;
    }

    // Register custom request handlers for enhanced functionality
    this.client.onRequest('prolog/queryResult', (params) => {
      // Handle query results from server
      return this.handleQueryResult(params);
    });

    this.client.onRequest('prolog/helpResult', (params) => {
      // Handle help results from server
      return this.handleHelpResult(params);
    });

    this.client.onNotification('prolog/diagnosticsUpdate', (params) => {
      // Handle diagnostic updates
      this.handleDiagnosticsUpdate(params);
    });
  }

  private enhanceCompletions(completions: any[], document: any, position: any): any[] {
    // Add context-aware completions
    const text = document.getText();
    const line = text.split('\n')[position.line];
    
    // Add N3/RDF completions if in semantic web context
    if (this.isSemanticWebContext(text)) {
      const semanticCompletions = this.getSemanticWebCompletions();
      completions.push(...semanticCompletions);
    }
    
    // Add project-specific completions
    const projectCompletions = this.getProjectSpecificCompletions(document);
    completions.push(...projectCompletions);
    
    return completions;
  }

  private enhanceHover(hover: any, document: any, position: any): any {
    // Enhance hover with additional information
    const word = this.getWordAtPosition(document.getText(), position);
    
    if (word && this.isBuiltinPredicate(word)) {
      // Add links to documentation
      if (hover.contents) {
        const enhanced = hover.contents + '\n\n[📖 View full documentation](https://www.swi-prolog.org/pldoc/man?predicate=' + word + ')';
        return { ...hover, contents: enhanced };
      }
    }
    
    return hover;
  }

  private isSemanticWebContext(text: string): boolean {
    return text.includes('@prefix') || text.includes('rdf:') || text.includes('rdfs:') || text.includes('owl:');
  }

  private getSemanticWebCompletions(): any[] {
    return [
      {
        label: 'rdf_load',
        kind: 3, // Function
        detail: 'rdf_load/2',
        documentation: 'Load RDF data from file or URL'
      },
      {
        label: 'rdf_assert',
        kind: 3,
        detail: 'rdf_assert/3',
        documentation: 'Assert RDF triple'
      },
      {
        label: 'rdf_retract',
        kind: 3,
        detail: 'rdf_retract/3',
        documentation: 'Retract RDF triple'
      }
    ];
  }

  private getProjectSpecificCompletions(document: any): any[] {
    // Analyze project structure and provide relevant completions
    const workspaceFolder = workspace.getWorkspaceFolder(document.uri);
    if (!workspaceFolder) {
      return [];
    }

    // This could be enhanced to scan project files for predicates
    return [];
  }

  private getWordAtPosition(text: string, position: any): string | null {
    const lines = text.split('\n');
    const line = lines[position.line];
    if (!line) {
      return null;
    }

    const char = position.character;
    let start = char;
    let end = char;

    while (start > 0 && /[a-zA-Z0-9_]/.test(line[start - 1])) {
      start--;
    }
    while (end < line.length && /[a-zA-Z0-9_]/.test(line[end])) {
      end++;
    }

    return start < end ? line.substring(start, end) : null;
  }

  private isBuiltinPredicate(predicate: string): boolean {
    const builtins = [
      'member', 'append', 'length', 'reverse', 'sort', 'findall', 'bagof', 'setof',
      'assert', 'retract', 'write', 'writeln', 'nl', 'is', 'var', 'nonvar'
    ];
    return builtins.includes(predicate);
  }

  private handleQueryResult(params: any): any {
    // Handle query results from server
    console.log('Query result received:', params);
    return null;
  }

  private handleHelpResult(params: any): any {
    // Handle help results from server
    console.log('Help result received:', params);
    return null;
  }

  private handleDiagnosticsUpdate(params: any): void {
    // Handle diagnostic updates
    console.log('Diagnostics updated:', params);
  }

  // Public API for external use
  public async executeQuery(query: string): Promise<any> {
    if (!this.client) {
      throw new Error('LSP client not started');
    }

    return await this.client.sendRequest('prolog/executeQuery', { query });
  }

  public async getHelp(predicate: string): Promise<any> {
    if (!this.client) {
      throw new Error('LSP client not started');
    }

    return await this.client.sendRequest('prolog/getHelp', { predicate });
  }

  public async consultFile(filePath: string): Promise<any> {
    if (!this.client) {
      throw new Error('LSP client not started');
    }

    return await this.client.sendRequest('prolog/consultFile', { filePath });
  }
}