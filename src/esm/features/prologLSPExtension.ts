import type {
  CancellationToken,
  CodeActionContext,
  DiagnosticCollection,
  ExtensionContext,
  ProviderResult,
  TextDocument,
} from 'vscode';
import {
  CodeAction,
  CodeActionKind,
  commands,
  CompletionItem,
  CompletionItemKind,
  Diagnostic,
  DiagnosticSeverity,
  languages,
  MarkdownString,
  Position,
  Range,
  Uri,
  window,
  workspace,
} from 'vscode';
import { PrologBackend } from '../prologBackend.js';
/**
 * This works alongside existing language providers to enhance the development experience
 */
export class PrologLSPExtension {
  private backend: PrologBackend | null = null;
  private diagnosticCollection: DiagnosticCollection;
  private context: ExtensionContext;

  constructor(context: ExtensionContext, backend: PrologBackend | null) {
    this.context = context;
    this.backend = backend;
    this.diagnosticCollection = languages.createDiagnosticCollection('prolog-lsp');
    context.subscriptions.push(this.diagnosticCollection);
  }

  /**
   * Register all LSP extension features
   */
  public registerFeatures() {
    // Register custom commands for LSP-style operations
    this.registerCustomCommands();

    // Register enhanced code actions
    this.registerCodeActionProvider();

    // Register enhanced completion provider
    this.registerEnhancedCompletionProvider();

    // Register N3 diagnostics
    this.registerN3DiagnosticsProvider();
  }

  /**
   * Register custom LSP-style commands
   */
  private registerCustomCommands() {
    // Custom query execution command
    const queryCommand = commands.registerCommand(
      'prolog.lsp.executeQuery',
      async (query?: string) => {
        if (!query) {
          query = await window.showInputBox({
            prompt: 'Enter Prolog query',
            placeHolder: 'member(X, [1,2,3])',
          });
        }

        if (query && this.backend?.isRunning()) {
          try {
            const response = await this.backend.sendRequest('query', {
              goal: query,
              timeoutMs: 10000,
            });

            if (response.status === 'ok') {
              this.showQueryResults(response.results || []);
            } else {
              window.showErrorMessage(`Query failed: ${response.error}`);
            }
          } catch (error) {
            window.showErrorMessage(`Query error: ${error}`);
          }
        }
      }
    );

    // Custom help lookup command
    const helpCommand = commands.registerCommand(
      'prolog.lsp.getHelp',
      async (predicate?: string) => {
        if (!predicate) {
          predicate = await window.showInputBox({
            prompt: 'Enter predicate name',
            placeHolder: 'member/2',
          });
        }

        if (predicate && this.backend?.isRunning()) {
          try {
            const response = await this.backend.sendRequest('help', {
              predicate,
              timeoutMs: 5000,
            });

            if (response.status === 'ok' && response.doc) {
              this.showHelpDocumentation(response.doc);
            } else {
              window.showInformationMessage(`No help found for ${predicate}`);
            }
          } catch (error) {
            window.showErrorMessage(`Help lookup error: ${error}`);
          }
        }
      }
    );

    // N3 diagnostics command
    const n3DiagnosticsCommand = commands.registerCommand(
      'prolog.lsp.runN3Diagnostics',
      async (uri?: Uri) => {
        const document = uri
          ? await workspace.openTextDocument(uri)
          : window.activeTextEditor?.document;

        if (document && document.languageId === 'prolog') {
          await this.runN3Diagnostics(document);
        }
      }
    );

    this.context.subscriptions.push(queryCommand, helpCommand, n3DiagnosticsCommand);
  }

  /**
   * Register enhanced code action provider
   */
  private registerCodeActionProvider() {
    const provider = languages.registerCodeActionsProvider('prolog', {
      provideCodeActions: (
        document: TextDocument,
        range: Range,
        context: CodeActionContext,
        token: CancellationToken
      ): ProviderResult<CodeAction[]> => {
        const actions: CodeAction[] = [];

        // Add query execution action
        const queryAction = new CodeAction('Execute as Query', CodeActionKind.Empty);
        queryAction.command = {
          command: 'prolog.lsp.executeQuery',
          title: 'Execute as Query',
          arguments: [document.getText(range)],
        };
        actions.push(queryAction);

        // Add help lookup action for predicates
        const text = document.getText(range);
        const predicateMatch = text.match(/([a-z][a-zA-Z0-9_]*)\s*\(/);
        if (predicateMatch) {
          const helpAction = new CodeAction('Get Help', CodeActionKind.Empty);
          helpAction.command = {
            command: 'prolog.lsp.getHelp',
            title: 'Get Help',
            arguments: [predicateMatch[1]],
          };
          actions.push(helpAction);
        }

        // Add N3 diagnostics action
        if (this.isN3Content(document.getText())) {
          const n3Action = new CodeAction('Run N3 Diagnostics', CodeActionKind.Source);
          n3Action.command = {
            command: 'prolog.lsp.runN3Diagnostics',
            title: 'Run N3 Diagnostics',
            arguments: [document.uri],
          };
          actions.push(n3Action);
        }

        return actions;
      },
    });

    this.context.subscriptions.push(provider);
  }

  /**
   * Register enhanced completion provider with backend integration
   */
  private registerEnhancedCompletionProvider() {
    const provider = languages.registerCompletionItemProvider(
      'prolog',
      {
        provideCompletionItems: async (
          document: TextDocument,
          position: Position,
          token: CancellationToken
        ): Promise<CompletionItem[]> => {
          const items: CompletionItem[] = [];

          // Get context around cursor
          const line = document.lineAt(position);
          const prefix = line.text.substring(0, position.character);

          // Add built-in predicate completions
          if (this.backend?.isRunning()) {
            try {
              // This would require extending the backend to support predicate listing
              const builtins = await this.getBuiltinPredicates();
              builtins.forEach(predicate => {
                const item = new CompletionItem(predicate.name, CompletionItemKind.Function);
                item.detail = predicate.arity
                  ? `${predicate.name}/${predicate.arity}`
                  : predicate.name;
                item.documentation = new MarkdownString(
                  predicate.description || 'Built-in predicate'
                );
                items.push(item);
              });
            } catch (_error) {
              // Fallback to static completions
            }
          }

          // Add N3 specific completions if in N3 context
          if (this.isN3Content(document.getText())) {
            items.push(...this.getN3Completions());
          }

          return items;
        },
      },
      '(',
      ',',
      ' '
    );

    this.context.subscriptions.push(provider);
  }

  /**
   * Register N3 diagnostics provider
   */
  private registerN3DiagnosticsProvider() {
    // Run diagnostics on document changes
    const disposable = workspace.onDidChangeTextDocument(async event => {
      if (event.document.languageId === 'prolog' && this.isN3Content(event.document.getText())) {
        await this.runN3Diagnostics(event.document);
      }
    });

    this.context.subscriptions.push(disposable);
  }

  /**
   * Run N3 diagnostics on a document
   */
  private async runN3Diagnostics(document: TextDocument) {
    if (!this.backend?.isRunning()) {
      return;
    }

    try {
      const diagnostics: Diagnostic[] = [];
      const content = document.getText();

      // Try to load N3 content and check for errors
      const response = await this.backend.sendRequest('n3_load', {
        content,
        validate: true,
        timeoutMs: 5000,
      });

      if (response.status === 'error') {
        // Parse error information and create diagnostics
        const diagnostic = new Diagnostic(
          new Range(0, 0, 0, 0), // Would need better error location parsing
          response.error || 'N3 validation error',
          DiagnosticSeverity.Error
        );
        diagnostic.source = 'prolog-n3';
        diagnostics.push(diagnostic);
      }

      // Check for common N3 issues
      diagnostics.push(...this.checkN3CommonIssues(document));

      this.diagnosticCollection.set(document.uri, diagnostics);
    } catch (error) {
      console.error('N3 diagnostics error:', error);
    }
  }

  /**
   * Check for common N3 issues
   */
  private checkN3CommonIssues(document: TextDocument): Diagnostic[] {
    const diagnostics: Diagnostic[] = [];
    const text = document.getText();
    const lines = text.split('\n');

    lines.forEach((line, index) => {
      // Check for missing prefixes
      if (
        line.includes(':') &&
        !line.includes('@prefix') &&
        !text.includes(`@prefix ${line.split(':')[0]}:`)
      ) {
        const diagnostic = new Diagnostic(
          new Range(index, 0, index, line.length),
          `Undefined prefix: ${line.split(':')[0]}`,
          DiagnosticSeverity.Warning
        );
        diagnostic.source = 'prolog-n3';
        diagnostics.push(diagnostic);
      }

      // Check for malformed triples
      if (line.trim().endsWith('.') && !line.includes('@') && line.split(/\s+/).length < 3) {
        const diagnostic = new Diagnostic(
          new Range(index, 0, index, line.length),
          'Incomplete triple statement',
          DiagnosticSeverity.Error
        );
        diagnostic.source = 'prolog-n3';
        diagnostics.push(diagnostic);
      }
    });

    return diagnostics;
  }

  /**
   * Show query results in a user-friendly way
   */
  private showQueryResults(results: any[]) {
    if (results.length === 0) {
      window.showInformationMessage('Query succeeded with no results');
      return;
    }

    // Format results for display
    const formatted = results
      .map((result, index) => {
        if (typeof result === 'object' && result !== null) {
          const bindings = Object.entries(result)
            .map(([key, value]) => `${key} = ${value}`)
            .join(', ');
          return `Solution ${index + 1}: ${bindings}`;
        }
        return `Solution ${index + 1}: ${JSON.stringify(result)}`;
      })
      .join('\n');

    // Show in output channel or information message
    window.showInformationMessage(`Query Results:\n${formatted}`, { modal: true });
  }

  /**
   * Show help documentation
   */
  private showHelpDocumentation(doc: any) {
    const content = `# ${doc.name}/${doc.arity}\n\n${doc.summary || 'No description available'}`;

    // Could create a webview or show in information message
    window.showInformationMessage(content, { modal: true });
  }

  /**
   * Check if document contains N3 content
   */
  private isN3Content(text: string): boolean {
    return (
      text.includes('@prefix') ||
      text.includes('@base') ||
      text.includes('rdf:') ||
      text.includes('rdfs:')
    );
  }

  /**
   * Get built-in predicates (would need backend support)
   */
  private async getBuiltinPredicates(): Promise<
    Array<{ name: string; arity?: number; description?: string }>
  > {
    // This would require extending the backend to list predicates
    // For now, return a static list of common predicates
    return [
      { name: 'member', arity: 2, description: 'True if Elem is a member of List' },
      {
        name: 'append',
        arity: 3,
        description: 'True if List3 is the concatenation of List1 and List2',
      },
      { name: 'findall', arity: 3, description: 'Find all solutions to Goal' },
      { name: 'bagof', arity: 3, description: 'Collect solutions to Goal' },
      { name: 'setof', arity: 3, description: 'Collect unique solutions to Goal' },
      { name: 'length', arity: 2, description: 'True if Length is the length of List' },
      { name: 'reverse', arity: 2, description: 'True if List2 is the reverse of List1' },
      { name: 'sort', arity: 2, description: 'True if Sorted is the sorted version of List' },
    ];
  }

  /**
   * Get N3-specific completions
   */
  private getN3Completions(): CompletionItem[] {
    const items: CompletionItem[] = [];

    // Common N3 prefixes
    const prefixes = [
      { name: '@prefix rdf:', detail: 'RDF namespace' },
      { name: '@prefix rdfs:', detail: 'RDF Schema namespace' },
      { name: '@prefix owl:', detail: 'OWL namespace' },
      { name: '@prefix xsd:', detail: 'XML Schema namespace' },
    ];

    prefixes.forEach(prefix => {
      const item = new CompletionItem(prefix.name, CompletionItemKind.Keyword);
      item.detail = prefix.detail;
      items.push(item);
    });

    // Common N3 properties
    const properties = [
      'rdf:type',
      'rdfs:label',
      'rdfs:comment',
      'owl:sameAs',
      'owl:differentFrom',
    ];

    properties.forEach(prop => {
      const item = new CompletionItem(prop, CompletionItemKind.Property);
      item.detail = 'N3 property';
      items.push(item);
    });

    return items;
  }

  /**
   * Dispose of resources
   */
  public dispose() {
    this.diagnosticCollection.dispose();
  }
}
