import {
  createConnection,
  TextDocuments,
  Diagnostic,
  DiagnosticSeverity,
  ProposedFeatures,
  InitializeParams,
  DidChangeConfigurationNotification,
  CompletionItem,
  CompletionItemKind,
  TextDocumentPositionParams,
  TextDocumentSyncKind,
  InitializeResult,
  HoverParams,
  Hover,
  MarkupKind,
  CodeActionParams,
  CodeAction,
  CodeActionKind,
  Command,
  ExecuteCommandParams,
  DefinitionParams,
  Definition,
  Location,
  Range,
  Position,
  DocumentSymbolParams,
  DocumentSymbol,
  SymbolKind,
  WorkspaceSymbolParams,
  SymbolInformation,
  ReferenceParams,
  DocumentHighlightParams,
  DocumentHighlight,
  DocumentHighlightKind,
  SignatureHelpParams,
  SignatureHelp,
  SignatureInformation,
  ParameterInformation,
  DocumentFormattingParams,
  TextEdit,
  DocumentRangeFormattingParams,
  RenameParams,
  WorkspaceEdit,
  PrepareRenameParams,
  FoldingRangeParams,
  FoldingRange,
  FoldingRangeKind,
  SemanticTokensParams,
  SemanticTokens,
  SemanticTokensBuilder,
  SemanticTokensLegend,
  DocumentColorParams,
  ColorInformation,
  Color,
  ColorPresentationParams,
  ColorPresentation,
  CallHierarchyPrepareParams,
  CallHierarchyItem,
  CallHierarchyIncomingCallsParams,
  CallHierarchyIncomingCall,
  CallHierarchyOutgoingCallsParams,
  CallHierarchyOutgoingCall,
  LinkedEditingRangeParams,
  LinkedEditingRanges,
  MonikerParams,
  Moniker,
  TypeDefinitionParams,
  ImplementationParams,
  DeclarationParams,
  SelectionRangeParams,
  SelectionRange
} from 'vscode-languageserver/node';

import { TextDocument } from 'vscode-languageserver-textdocument';
import { URI } from 'vscode-uri';
import * as fs from 'fs';
import * as path from 'path';
import { PrologBackend } from '../prologBackend';

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;

// Prolog backend instance
let prologBackend: PrologBackend | null = null;

// Document settings cache
interface PrologSettings {
  executablePath: string;
  dialect: 'swi' | 'ecl';
  linter: {
    run: 'onSave' | 'onType' | 'never';
    delay: number;
    enableMsgInOutput: boolean;
  };
  format: {
    addSpace: boolean;
  };
  terminal: {
    runtimeArgs: string[];
  };
}

const defaultSettings: PrologSettings = {
  executablePath: 'swipl',
  dialect: 'swi',
  linter: {
    run: 'onType',
    delay: 500,
    enableMsgInOutput: false
  },
  format: {
    addSpace: true
  },
  terminal: {
    runtimeArgs: []
  }
};

let globalSettings: PrologSettings = defaultSettings;
const documentSettings: Map<string, Thenable<PrologSettings>> = new Map();

// Semantic tokens legend
const semanticTokensLegend: SemanticTokensLegend = {
  tokenTypes: [
    'namespace', 'type', 'class', 'enum', 'interface', 'struct', 'typeParameter',
    'parameter', 'variable', 'property', 'enumMember', 'event', 'function',
    'method', 'macro', 'keyword', 'modifier', 'comment', 'string', 'number',
    'regexp', 'operator', 'decorator'
  ],
  tokenModifiers: [
    'declaration', 'definition', 'readonly', 'static', 'deprecated', 'abstract',
    'async', 'modification', 'documentation', 'defaultLibrary'
  ]
};

connection.onInitialize((params: InitializeParams) => {
  const capabilities = params.capabilities;

  // Does the client support the `workspace/configuration` request?
  hasConfigurationCapability = !!(
    capabilities.workspace && !!capabilities.workspace.configuration
  );
  hasWorkspaceFolderCapability = !!(
    capabilities.workspace && !!capabilities.workspace.workspaceFolders
  );
  hasDiagnosticRelatedInformationCapability = !!(
    capabilities.textDocument &&
    capabilities.textDocument.publishDiagnostics &&
    capabilities.textDocument.publishDiagnostics.relatedInformation
  );

  const result: InitializeResult = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      // Tell the client that this server supports code completion.
      completionProvider: {
        resolveProvider: true,
        triggerCharacters: ['(', ',', ' ', '.', ':', '-', '_']
      },
      // Hover support
      hoverProvider: true,
      // Code actions
      codeActionProvider: {
        codeActionKinds: [
          CodeActionKind.QuickFix,
          CodeActionKind.Refactor,
          CodeActionKind.Source,
          CodeActionKind.SourceOrganizeImports
        ]
      },
      // Execute command
      executeCommandProvider: {
        commands: [
          'prolog.executeQuery',
          'prolog.consultFile',
          'prolog.getHelp',
          'prolog.runN3Diagnostics',
          'prolog.formatDocument',
          'prolog.organizeImports'
        ]
      },
      // Definition provider
      definitionProvider: true,
      // Document symbol provider
      documentSymbolProvider: true,
      // Workspace symbol provider
      workspaceSymbolProvider: true,
      // References provider
      referencesProvider: true,
      // Document highlight provider
      documentHighlightProvider: true,
      // Signature help provider
      signatureHelpProvider: {
        triggerCharacters: ['(', ',']
      },
      // Document formatting
      documentFormattingProvider: true,
      documentRangeFormattingProvider: true,
      // Rename provider
      renameProvider: {
        prepareProvider: true
      },
      // Folding range provider
      foldingRangeProvider: true,
      // Semantic tokens provider
      semanticTokensProvider: {
        legend: semanticTokensLegend,
        range: true,
        full: {
          delta: true
        }
      },
      // Document color provider
      colorProvider: true,
      // Call hierarchy provider
      callHierarchyProvider: true,
      // Linked editing range provider
      linkedEditingRangeProvider: true,
      // Moniker provider
      monikerProvider: true,
      // Type definition provider
      typeDefinitionProvider: true,
      // Implementation provider
      implementationProvider: true,
      // Declaration provider
      declarationProvider: true,
      // Selection range provider
      selectionRangeProvider: true
    }
  };

  if (hasWorkspaceFolderCapability) {
    result.capabilities.workspace = {
      workspaceFolders: {
        supported: true
      }
    };
  }

  return result;
});

connection.onInitialized(() => {
  if (hasConfigurationCapability) {
    // Register for all configuration changes.
    connection.client.register(DidChangeConfigurationNotification.type, undefined);
  }
  if (hasWorkspaceFolderCapability) {
    connection.workspace.onDidChangeWorkspaceFolders(_event => {
      connection.console.log('Workspace folder change event received.');
    });
  }

  // Initialize Prolog backend
  initializePrologBackend();
});

async function initializePrologBackend() {
  try {
    const settings = await getGlobalSettings();
    prologBackend = new PrologBackend({
      swiplPath: settings.executablePath,
      port: 3061, // Different port for LSP server
      streamingEnabled: true,
      maxResultsPerChunk: 50
    });

    prologBackend.on('ready', () => {
      connection.console.log('Prolog backend ready for LSP server');
    });

    prologBackend.on('error', (error) => {
      connection.console.error(`Prolog backend error: ${error}`);
    });

    prologBackend.start();
  } catch (error: unknown) {
    connection.console.error(`Failed to initialize Prolog backend: ${error}`);
  }
}

connection.onDidChangeConfiguration(change => {
  if (hasConfigurationCapability) {
    // Reset all cached document settings
    documentSettings.clear();
  } else {
    globalSettings = <PrologSettings>(
      (change.settings.prolog || defaultSettings)
    );
  }

  // Revalidate all open text documents
  documents.all().forEach(validateTextDocument);
});

function getDocumentSettings(resource: string): Thenable<PrologSettings> {
  if (!hasConfigurationCapability) {
    return Promise.resolve(globalSettings);
  }
  let result = documentSettings.get(resource);
  if (!result) {
    result = connection.workspace.getConfiguration({
      scopeUri: resource,
      section: 'prolog'
    });
    documentSettings.set(resource, result);
  }
  return result;
}

function getGlobalSettings(): Thenable<PrologSettings> {
  if (!hasConfigurationCapability) {
    return Promise.resolve(globalSettings);
  }
  return connection.workspace.getConfiguration('prolog');
}

// Only keep settings for open documents
documents.onDidClose(e => {
  documentSettings.delete(e.document.uri);
});

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent(change => {
  validateTextDocument(change.document);
});

async function validateTextDocument(textDocument: TextDocument): Promise<void> {
  const settings = await getDocumentSettings(textDocument.uri);
  
  if (settings.linter.run === 'never') {
    return;
  }

  const diagnostics: Diagnostic[] = [];
  const text = textDocument.getText();
  const lines = text.split('\n');

  // Basic Prolog syntax validation
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const trimmedLine = line.trim();
    
    if (trimmedLine === '' || trimmedLine.startsWith('%')) {
      continue; // Skip empty lines and comments
    }

    // Check for common syntax errors
    const diagnosticsForLine = validatePrologLine(line, i, textDocument);
    diagnostics.push(...diagnosticsForLine);
  }

  // Advanced validation using Prolog backend
  if (prologBackend?.isRunning()) {
    try {
      const backendDiagnostics = await validateWithBackend(textDocument);
      diagnostics.push(...backendDiagnostics);
    } catch (error: unknown) {
      connection.console.error(`Backend validation error: ${error}`);
    }
  }

  // Send the computed diagnostics to VSCode.
  connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
}

function validatePrologLine(line: string, lineNumber: number, document: TextDocument): Diagnostic[] {
  const diagnostics: Diagnostic[] = [];
  const trimmedLine = line.trim();

  // Check for unmatched parentheses
  const openParens = (line.match(/\(/g) || []).length;
  const closeParens = (line.match(/\)/g) || []).length;
  
  if (openParens !== closeParens) {
    const diagnostic: Diagnostic = {
      severity: DiagnosticSeverity.Error,
      range: {
        start: { line: lineNumber, character: 0 },
        end: { line: lineNumber, character: line.length }
      },
      message: `Unmatched parentheses: ${openParens} opening, ${closeParens} closing`,
      source: 'prolog-lsp'
    };
    diagnostics.push(diagnostic);
  }

  // Check for missing periods at end of clauses
  if (trimmedLine.length > 0 && 
      !trimmedLine.endsWith('.') && 
      !trimmedLine.endsWith(':-') &&
      !trimmedLine.startsWith('?-') &&
      !trimmedLine.startsWith(':-')) {
    
    const diagnostic: Diagnostic = {
      severity: DiagnosticSeverity.Warning,
      range: {
        start: { line: lineNumber, character: line.length - 1 },
        end: { line: lineNumber, character: line.length }
      },
      message: 'Clause should end with a period',
      source: 'prolog-lsp'
    };
    diagnostics.push(diagnostic);
  }

  // Check for undefined predicates (basic heuristic)
  const predicateMatch = trimmedLine.match(/^([a-z][a-zA-Z0-9_]*)\s*\(/);
  if (predicateMatch) {
    const predicate = predicateMatch[1];
    if (isLikelyUndefinedPredicate(predicate)) {
      const diagnostic: Diagnostic = {
        severity: DiagnosticSeverity.Information,
        range: {
          start: { line: lineNumber, character: line.indexOf(predicate) },
          end: { line: lineNumber, character: line.indexOf(predicate) + predicate.length }
        },
        message: `Predicate '${predicate}' may be undefined`,
        source: 'prolog-lsp'
      };
      diagnostics.push(diagnostic);
    }
  }

  return diagnostics;
}

function isLikelyUndefinedPredicate(predicate: string): boolean {
  // List of common built-in predicates
  const builtins = [
    'member', 'append', 'length', 'reverse', 'sort', 'findall', 'bagof', 'setof',
    'assert', 'retract', 'asserta', 'assertz', 'retracta', 'retractall',
    'write', 'writeln', 'read', 'get', 'put', 'nl', 'tab',
    'is', 'var', 'nonvar', 'atom', 'number', 'compound', 'atomic',
    'functor', 'arg', 'univ', 'copy_term', 'numbervars',
    'call', 'once', 'ignore', 'forall', 'between', 'succ',
    'true', 'fail', 'halt', 'abort', 'trace', 'notrace', 'spy', 'nospy'
  ];
  
  return !builtins.includes(predicate);
}

async function validateWithBackend(document: TextDocument): Promise<Diagnostic[]> {
  if (!prologBackend?.isRunning()) {
    return [];
  }

  try {
    // Try to consult the document content
    const response = await prologBackend.sendRequest('validate', {
      content: document.getText(),
      uri: document.uri,
      timeoutMs: 5000
    });

    if (response.status === 'error' && response.errors) {
      return response.errors.map((error: { line?: number; column?: number; length?: number; message?: string }) => ({
        severity: DiagnosticSeverity.Error,
        range: {
          start: { line: error.line || 0, character: error.column || 0 },
          end: { line: error.line || 0, character: (error.column || 0) + (error.length || 1) }
        },
        message: error.message || 'Validation error',
        source: 'prolog-backend'
      }));
    }
  } catch (error: unknown) {
    // Backend validation failed, return empty array
  }

  return [];
}

// Completion provider
connection.onCompletion(
  async (_textDocumentPosition: TextDocumentPositionParams): Promise<CompletionItem[]> => {
    const document = documents.get(_textDocumentPosition.textDocument.uri);
    if (!document) {
      return [];
    }

    const position = _textDocumentPosition.position;
    const text = document.getText();
    const lines = text.split('\n');
    const currentLine = lines[position.line] || '';
    const prefix = currentLine.substring(0, position.character);

    const completions: CompletionItem[] = [];

    // Built-in predicates
    const builtinPredicates = [
      { name: 'member', arity: 2, description: 'True if Elem is a member of List' },
      { name: 'append', arity: 3, description: 'True if List3 is the concatenation of List1 and List2' },
      { name: 'length', arity: 2, description: 'True if Length is the length of List' },
      { name: 'reverse', arity: 2, description: 'True if List2 is the reverse of List1' },
      { name: 'sort', arity: 2, description: 'True if Sorted is the sorted version of List' },
      { name: 'findall', arity: 3, description: 'Find all solutions to Goal' },
      { name: 'bagof', arity: 3, description: 'Collect solutions to Goal' },
      { name: 'setof', arity: 3, description: 'Collect unique solutions to Goal' },
      { name: 'assert', arity: 1, description: 'Add clause to database' },
      { name: 'retract', arity: 1, description: 'Remove clause from database' },
      { name: 'write', arity: 1, description: 'Write term to output' },
      { name: 'writeln', arity: 1, description: 'Write term followed by newline' },
      { name: 'nl', arity: 0, description: 'Write newline to output' },
      { name: 'is', arity: 2, description: 'Arithmetic evaluation' },
      { name: 'var', arity: 1, description: 'True if argument is unbound variable' },
      { name: 'nonvar', arity: 1, description: 'True if argument is not unbound variable' },
      { name: 'atom', arity: 1, description: 'True if argument is an atom' },
      { name: 'number', arity: 1, description: 'True if argument is a number' },
      { name: 'compound', arity: 1, description: 'True if argument is a compound term' },
      { name: 'functor', arity: 3, description: 'Relate compound term to functor name and arity' },
      { name: 'arg', arity: 3, description: 'Extract argument from compound term' },
      { name: 'univ', arity: 2, description: 'Convert between term and list representation' },
      { name: 'call', arity: 1, description: 'Call goal dynamically' },
      { name: 'once', arity: 1, description: 'Succeed at most once' },
      { name: 'forall', arity: 2, description: 'For all solutions of Condition, Action must succeed' },
      { name: 'between', arity: 3, description: 'Generate integers between bounds' },
      { name: 'succ', arity: 2, description: 'Successor relation for integers' }
    ];

    builtinPredicates.forEach(pred => {
      const item: CompletionItem = {
        label: pred.name,
        kind: CompletionItemKind.Function,
        detail: `${pred.name}/${pred.arity}`,
        documentation: pred.description,
        insertText: pred.arity > 0 ? `${pred.name}(` : pred.name
      };
      completions.push(item);
    });

    // N3/RDF completions if in N3 context
    if (isN3Context(text)) {
      const n3Completions = getN3Completions();
      completions.push(...n3Completions);
    }

    // Dynamic completions from backend
    if (prologBackend?.isRunning()) {
      try {
        const dynamicCompletions = await getDynamicCompletions(document, position);
        completions.push(...dynamicCompletions);
      } catch (error: unknown) {
        // Ignore errors in dynamic completion
      }
    }

    return completions;
  }
);

function isN3Context(text: string): boolean {
  return text.includes('@prefix') || text.includes('@base') || 
         text.includes('rdf:') || text.includes('rdfs:') || text.includes('owl:');
}

function getN3Completions(): CompletionItem[] {
  const completions: CompletionItem[] = [];

  // N3 prefixes
  const prefixes = [
    { name: '@prefix rdf:', detail: '<http://www.w3.org/1999/02/22-rdf-syntax-ns#>' },
    { name: '@prefix rdfs:', detail: '<http://www.w3.org/2000/01/rdf-schema#>' },
    { name: '@prefix owl:', detail: '<http://www.w3.org/2002/07/owl#>' },
    { name: '@prefix xsd:', detail: '<http://www.w3.org/2001/XMLSchema#>' },
    { name: '@prefix foaf:', detail: '<http://xmlns.com/foaf/0.1/>' },
    { name: '@prefix dc:', detail: '<http://purl.org/dc/elements/1.1/>' }
  ];

  prefixes.forEach(prefix => {
    completions.push({
      label: prefix.name,
      kind: CompletionItemKind.Keyword,
      detail: prefix.detail,
      insertText: `${prefix.name} ${prefix.detail} .`
    });
  });

  // Common RDF properties
  const properties = [
    'rdf:type', 'rdfs:label', 'rdfs:comment', 'rdfs:seeAlso', 'rdfs:isDefinedBy',
    'owl:sameAs', 'owl:differentFrom', 'owl:equivalentClass', 'owl:equivalentProperty',
    'foaf:name', 'foaf:knows', 'foaf:mbox', 'dc:title', 'dc:creator', 'dc:date'
  ];

  properties.forEach(prop => {
    completions.push({
      label: prop,
      kind: CompletionItemKind.Property,
      detail: 'RDF property'
    });
  });

  return completions;
}

async function getDynamicCompletions(document: TextDocument, position: Position): Promise<CompletionItem[]> {
  if (!prologBackend?.isRunning()) {
    return [];
  }

  try {
    const response = await prologBackend.sendRequest('completions', {
      uri: document.uri,
      content: document.getText(),
      line: position.line,
      character: position.character,
      timeoutMs: 2000
    });

    if (response.status === 'ok' && response.completions) {
      return response.completions.map((comp: { label: string; kind?: string; detail?: string; documentation?: string; insertText?: string }) => ({
        label: comp.label,
        kind: getCompletionItemKind(comp.kind),
        detail: comp.detail,
        documentation: comp.documentation,
        insertText: comp.insertText || comp.label
      }));
    }
  } catch (error: unknown) {
    // Ignore errors
  }

  return [];
}

function getCompletionItemKind(kind: string): CompletionItemKind {
  switch (kind) {
    case 'predicate': return CompletionItemKind.Function;
    case 'variable': return CompletionItemKind.Variable;
    case 'atom': return CompletionItemKind.Constant;
    case 'module': return CompletionItemKind.Module;
    case 'operator': return CompletionItemKind.Operator;
    default: return CompletionItemKind.Text;
  }
}

// Completion resolve
connection.onCompletionResolve(
  (item: CompletionItem): CompletionItem => {
    // Add more detailed documentation if needed
    return item;
  }
);

// Hover provider
connection.onHover(
  async (params: HoverParams): Promise<Hover | null> => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
      return null;
    }

    const position = params.position;
    const text = document.getText();
    const word = getWordAtPosition(text, position);

    if (!word) {
      return null;
    }

    // Try to get help from backend
    if (prologBackend?.isRunning()) {
      try {
        const response = await prologBackend.sendRequest('help', {
          predicate: word,
          timeoutMs: 3000
        });

        if (response.status === 'ok' && response.doc) {
          const doc = response.doc;
          const markdown = formatHelpAsMarkdown(doc);
          
          return {
            contents: {
              kind: MarkupKind.Markdown,
              value: markdown
            }
          };
        }
      } catch (error) {
        // Fall back to static help
      }
    }

    // Static help for common predicates
    const staticHelp = getStaticHelp(word);
    if (staticHelp) {
      return {
        contents: {
          kind: MarkupKind.Markdown,
          value: staticHelp
        }
      };
    }

    return null;
  }
);

function getWordAtPosition(text: string, position: Position): string | null {
  const lines = text.split('\n');
  const line = lines[position.line];
  if (!line) {
    return null;
  }

  const char = position.character;
  let start = char;
  let end = char;

  // Find word boundaries
  while (start > 0 && /[a-zA-Z0-9_]/.test(line[start - 1])) {
    start--;
  }
  while (end < line.length && /[a-zA-Z0-9_]/.test(line[end])) {
    end++;
  }

  return start < end ? line.substring(start, end) : null;
}

function formatHelpAsMarkdown(doc: { name: string; arity: number; summary?: string; args?: Array<{ name: string; description: string }>; examples?: string[] }): string {
  let markdown = `# ${doc.name}/${doc.arity}\n\n`;
  
  if (doc.summary) {
    markdown += `${doc.summary}\n\n`;
  }
  
  if (doc.args && doc.args.length > 0) {
    markdown += '## Arguments\n\n';
    doc.args.forEach((arg: any, index: number) => {
      markdown += `**${index + 1}. ${arg.name}** - ${arg.description}\n\n`;
    });
  }
  
  if (doc.examples && doc.examples.length > 0) {
    markdown += '## Examples\n\n';
    doc.examples.forEach((example: string) => {
      markdown += `\`\`\`prolog\n${example}\n\`\`\`\n\n`;
    });
  }
  
  return markdown;
}

function getStaticHelp(word: string): string | null {
  const staticHelp: Record<string, string> = {
    'member': '# member/2\n\nTrue if Elem is a member of List.\n\n```prolog\nmember(X, [1,2,3]).\n```',
    'append': '# append/3\n\nTrue if List3 is the concatenation of List1 and List2.\n\n```prolog\nappend([1,2], [3,4], X).\n```',
    'length': '# length/2\n\nTrue if Length is the length of List.\n\n```prolog\nlength([1,2,3], X).\n```',
    'findall': '# findall/3\n\nFind all solutions to Goal.\n\n```prolog\nfindall(X, member(X, [1,2,3]), L).\n```'
  };
  
  return staticHelp[word] || null;
}

// Code actions provider
connection.onCodeAction(
  async (params: CodeActionParams): Promise<CodeAction[]> => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
      return [];
    }

    const actions: CodeAction[] = [];
    const range = params.range;
    const selectedText = document.getText(range);

    // Quick fix actions for diagnostics
    for (const diagnostic of params.context.diagnostics) {
      if (diagnostic.source === 'prolog-lsp') {
        if (diagnostic.message.includes('missing period')) {
          const fix: CodeAction = {
            title: 'Add missing period',
            kind: CodeActionKind.QuickFix,
            diagnostics: [diagnostic],
            edit: {
              changes: {
                [params.textDocument.uri]: [{
                  range: {
                    start: diagnostic.range.end,
                    end: diagnostic.range.end
                  },
                  newText: '.'
                }]
              }
            }
          };
          actions.push(fix);
        }
      }
    }

    // Refactor actions
    if (selectedText.trim()) {
      // Execute as query
      const executeAction: CodeAction = {
        title: 'Execute as Prolog query',
        kind: CodeActionKind.Source,
        command: {
          title: 'Execute Query',
          command: 'prolog.executeQuery',
          arguments: [selectedText.trim()]
        }
      };
      actions.push(executeAction);

      // Get help for predicate
      const predicateMatch = selectedText.match(/([a-z][a-zA-Z0-9_]*)/);
      if (predicateMatch) {
        const helpAction: CodeAction = {
          title: `Get help for ${predicateMatch[1]}`,
          kind: CodeActionKind.Source,
          command: {
            title: 'Get Help',
            command: 'prolog.getHelp',
            arguments: [predicateMatch[1]]
          }
        };
        actions.push(helpAction);
      }
    }

    // Source actions
    const formatAction: CodeAction = {
      title: 'Format document',
      kind: CodeActionKind.SourceOrganizeImports,
      command: {
        title: 'Format Document',
        command: 'prolog.formatDocument',
        arguments: [params.textDocument.uri]
      }
    };
    actions.push(formatAction);

    return actions;
  }
);

// Execute command handler
connection.onExecuteCommand(
  async (params: ExecuteCommandParams): Promise<any> => {
    const command = params.command;
    const args = params.arguments || [];

    switch (command) {
      case 'prolog.executeQuery':
        if (args.length > 0 && prologBackend?.isRunning()) {
          try {
            const response = await prologBackend.sendRequest('query', {
              goal: args[0],
              timeoutMs: 10000
            });
            connection.window.showInformationMessage(
              response.status === 'ok'
                ? `Query result: ${JSON.stringify(response.results)}`
                : `Query failed: ${response.error}`
            );
            return response;
          } catch (error: unknown) {
            connection.window.showErrorMessage(`Query error: ${error}`);
          }
        }
        break;

      case 'prolog.consultFile':
        if (args.length > 0 && prologBackend?.isRunning()) {
          try {
            const response = await prologBackend.sendRequest('consult', {
              file: args[0],
              timeoutMs: 15000
            });
            connection.window.showInformationMessage(
              response.status === 'ok'
                ? `File consulted: ${args[0]}`
                : `Consult failed: ${response.error}`
            );
            return response;
          } catch (error: unknown) {
            connection.window.showErrorMessage(`Consult error: ${error}`);
          }
        }
        break;

      case 'prolog.getHelp':
        if (args.length > 0 && prologBackend?.isRunning()) {
          try {
            const response = await prologBackend.sendRequest('help', {
              predicate: args[0],
              timeoutMs: 5000
            });
            if (response.status === 'ok' && response.doc) {
              const markdown = formatHelpAsMarkdown(response.doc);
              connection.window.showInformationMessage(`Help for ${args[0]}: ${response.doc.summary || 'No description'}`);
            }
            return response;
          } catch (error: unknown) {
            connection.window.showErrorMessage(`Help error: ${error}`);
          }
        }
        break;

      case 'prolog.runN3Diagnostics':
        if (args.length > 0) {
          const document = documents.get(args[0]);
          if (document) {
            await validateTextDocument(document);
            connection.window.showInformationMessage('N3 diagnostics completed');
          }
        }
        break;

      case 'prolog.formatDocument':
        if (args.length > 0) {
          const document = documents.get(args[0]);
          if (document) {
            const formatted = await formatPrologDocument(document);
            return formatted;
          }
        }
        break;

      case 'prolog.organizeImports':
        if (args.length > 0) {
          const document = documents.get(args[0]);
          if (document) {
            const organized = await organizeImports(document);
            return organized;
          }
        }
        break;
    }

    return null;
  }
);

// Definition provider
connection.onDefinition(
  async (params: DefinitionParams): Promise<Definition | null> => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
      return null;
    }

    const position = params.position;
    const word = getWordAtPosition(document.getText(), position);
    
    if (!word) {
      return null;
    }

    // Try to find definition using backend
    if (prologBackend?.isRunning()) {
      try {
        const response = await prologBackend.sendRequest('definition', {
          predicate: word,
          uri: params.textDocument.uri,
          line: position.line,
          character: position.character,
          timeoutMs: 3000
        });

        if (response.status === 'ok' && response.locations) {
          return response.locations.map((loc: { uri: string; line: number; character: number }) => ({
            uri: loc.uri,
            range: {
              start: { line: loc.line, character: loc.character },
              end: { line: loc.line, character: loc.character + word.length }
            }
          }));
        }
      } catch (error: unknown) {
        // Fall back to local search
      }
    }

    // Local definition search
    return findLocalDefinition(document, word);
  }
);

function findLocalDefinition(document: TextDocument, predicate: string): Location[] {
  const text = document.getText();
  const lines = text.split('\n');
  const locations: Location[] = [];

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const regex = new RegExp(`^\\s*${predicate}\\s*\\(`, 'g');
    const match = regex.exec(line);
    
    if (match) {
      locations.push({
        uri: document.uri,
        range: {
          start: { line: i, character: match.index },
          end: { line: i, character: match.index + predicate.length }
        }
      });
    }
  }

  return locations;
}

// Document symbol provider
connection.onDocumentSymbol(
  async (params: DocumentSymbolParams): Promise<DocumentSymbol[]> => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
      return [];
    }

    const symbols: DocumentSymbol[] = [];
    const text = document.getText();
    const lines = text.split('\n');

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();
      
      if (line.startsWith('%') || line === '') {
        continue;
      }

      // Match predicate definitions
      const predicateMatch = line.match(/^([a-z][a-zA-Z0-9_]*)\s*\(/);
      if (predicateMatch) {
        const name = predicateMatch[1];
        const arity = countArity(line);
        
        symbols.push({
          name: `${name}/${arity}`,
          kind: SymbolKind.Function,
          range: {
            start: { line: i, character: 0 },
            end: { line: i, character: line.length }
          },
          selectionRange: {
            start: { line: i, character: 0 },
            end: { line: i, character: name.length }
          }
        });
      }

      // Match directives
      const directiveMatch = line.match(/^:-\s*(.+)\./);
      if (directiveMatch) {
        symbols.push({
          name: `:- ${directiveMatch[1]}`,
          kind: SymbolKind.Namespace,
          range: {
            start: { line: i, character: 0 },
            end: { line: i, character: line.length }
          },
          selectionRange: {
            start: { line: i, character: 0 },
            end: { line: i, character: 2 }
          }
        });
      }
    }

    return symbols;
  }
);

function countArity(line: string): number {
  const match = line.match(/\(([^)]*)\)/);
  if (!match) return 0;
  
  const args = match[1].trim();
  if (args === '') return 0;
  
  // Simple arity counting (doesn't handle nested structures perfectly)
  return args.split(',').length;
}

// Workspace symbol provider
connection.onWorkspaceSymbol(
  async (params: WorkspaceSymbolParams): Promise<SymbolInformation[]> => {
    const symbols: SymbolInformation[] = [];
    const query = params.query.toLowerCase();

    // Search through all open documents
    for (const document of documents.all()) {
      const documentSymbols = await connection.sendRequest('textDocument/documentSymbol', {
        textDocument: { uri: document.uri }
      }) as DocumentSymbol[];

      for (const symbol of documentSymbols) {
        if (symbol.name.toLowerCase().includes(query)) {
          symbols.push({
            name: symbol.name,
            kind: symbol.kind,
            location: {
              uri: document.uri,
              range: symbol.range
            }
          });
        }
      }
    }

    return symbols;
  }
);

// References provider
connection.onReferences(
  async (params: ReferenceParams): Promise<Location[]> => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
      return [];
    }

    const position = params.position;
    const word = getWordAtPosition(document.getText(), position);
    
    if (!word) {
      return [];
    }

    const locations: Location[] = [];
    
    // Search in current document
    const text = document.getText();
    const lines = text.split('\n');
    
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const regex = new RegExp(`\\b${word}\\b`, 'g');
      let match;
      
      while ((match = regex.exec(line)) !== null) {
        locations.push({
          uri: document.uri,
          range: {
            start: { line: i, character: match.index },
            end: { line: i, character: match.index + word.length }
          }
        });
      }
    }

    return locations;
  }
);

// Document highlight provider
connection.onDocumentHighlight(
  async (params: DocumentHighlightParams): Promise<DocumentHighlight[]> => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
      return [];
    }

    const position = params.position;
    const word = getWordAtPosition(document.getText(), position);
    
    if (!word) {
      return [];
    }

    const highlights: DocumentHighlight[] = [];
    const text = document.getText();
    const lines = text.split('\n');
    
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const regex = new RegExp(`\\b${word}\\b`, 'g');
      let match;
      
      while ((match = regex.exec(line)) !== null) {
        highlights.push({
          range: {
            start: { line: i, character: match.index },
            end: { line: i, character: match.index + word.length }
          },
          kind: DocumentHighlightKind.Text
        });
      }
    }

    return highlights;
  }
);

// Signature help provider
connection.onSignatureHelp(
  async (params: SignatureHelpParams): Promise<SignatureHelp | null> => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
      return null;
    }

    const position = params.position;
    const text = document.getText();
    const line = text.split('\n')[position.line];
    
    // Find the predicate being called
    const beforeCursor = line.substring(0, position.character);
    const predicateMatch = beforeCursor.match(/([a-z][a-zA-Z0-9_]*)\s*\([^)]*$/);
    
    if (!predicateMatch) {
      return null;
    }

    const predicate = predicateMatch[1];
    
    // Get signature information
    const signature = getPredicateSignature(predicate);
    if (!signature) {
      return null;
    }

    // Count current parameter
    const openParen = beforeCursor.lastIndexOf('(');
    const args = beforeCursor.substring(openParen + 1);
    const parameterIndex = args.split(',').length - 1;

    return {
      signatures: [signature],
      activeSignature: 0,
      activeParameter: Math.min(parameterIndex, signature.parameters.length - 1)
    };
  }
);

function getPredicateSignature(predicate: string): SignatureInformation | null {
  const signatures: Record<string, SignatureInformation> = {
    'member': {
      label: 'member(Elem, List)',
      documentation: 'True if Elem is a member of List',
      parameters: [
        { label: 'Elem', documentation: 'Element to check' },
        { label: 'List', documentation: 'List to search in' }
      ]
    },
    'append': {
      label: 'append(List1, List2, List3)',
      documentation: 'True if List3 is the concatenation of List1 and List2',
      parameters: [
        { label: 'List1', documentation: 'First list' },
        { label: 'List2', documentation: 'Second list' },
        { label: 'List3', documentation: 'Concatenated result' }
      ]
    },
    'findall': {
      label: 'findall(Template, Goal, List)',
      documentation: 'Find all solutions to Goal',
      parameters: [
        { label: 'Template', documentation: 'Template for solutions' },
        { label: 'Goal', documentation: 'Goal to solve' },
        { label: 'List', documentation: 'List of solutions' }
      ]
    }
  };

  return signatures[predicate] || null;
}

// Document formatting provider
connection.onDocumentFormatting(
  async (params: DocumentFormattingParams): Promise<TextEdit[]> => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
      return [];
    }

    return await formatPrologDocument(document);
  }
);

connection.onDocumentRangeFormatting(
  async (params: DocumentRangeFormattingParams): Promise<TextEdit[]> => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
      return [];
    }

    return await formatPrologDocumentRange(document, params.range);
  }
);

async function formatPrologDocument(document: TextDocument): Promise<TextEdit[]> {
  const settings = await getDocumentSettings(document.uri);
  const text = document.getText();
  const lines = text.split('\n');
  const edits: TextEdit[] = [];

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const formatted = formatPrologLine(line, settings);
    
    if (formatted !== line) {
      edits.push({
        range: {
          start: { line: i, character: 0 },
          end: { line: i, character: line.length }
        },
        newText: formatted
      });
    }
  }

  return edits;
}

async function formatPrologDocumentRange(document: TextDocument, range: Range): Promise<TextEdit[]> {
  const settings = await getDocumentSettings(document.uri);
  const text = document.getText(range);
  const lines = text.split('\n');
  const edits: TextEdit[] = [];

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const formatted = formatPrologLine(line, settings);
    
    if (formatted !== line) {
      edits.push({
        range: {
          start: { line: range.start.line + i, character: 0 },
          end: { line: range.start.line + i, character: line.length }
        },
        newText: formatted
      });
    }
  }

  return edits;
}

function formatPrologLine(line: string, settings: PrologSettings): string {
  let formatted = line;

  // Add spaces after commas if enabled
  if (settings.format.addSpace) {
    formatted = formatted.replace(/,(?!\s)/g, ', ');
  }

  // Basic indentation for clauses
  const trimmed = formatted.trim();
  if (trimmed.startsWith(':-') || trimmed.startsWith('?-')) {
    // Directives and queries - no extra indentation
    return trimmed;
  } else if (trimmed.includes(':-')) {
    // Rules - no extra indentation for head
    return trimmed;
  } else if (trimmed.match(/^\s*[a-z]/)) {
    // Facts - no extra indentation
    return trimmed;
  }

  return formatted;
}

async function organizeImports(document: TextDocument): Promise<TextEdit[]> {
  const text = document.getText();
  const lines = text.split('\n');
  const imports: string[] = [];
  const otherLines: string[] = [];
  const edits: TextEdit[] = [];

  // Separate imports from other content
  for (const line of lines) {
    const trimmed = line.trim();
    if (trimmed.startsWith(':- use_module(') || trimmed.startsWith(':- include(')) {
      imports.push(line);
    } else {
      otherLines.push(line);
    }
  }

  // Sort imports
  imports.sort();

  // Create new content
  const newContent = [...imports, '', ...otherLines].join('\n');
  
  if (newContent !== text) {
    edits.push({
      range: {
        start: { line: 0, character: 0 },
        end: { line: lines.length - 1, character: lines[lines.length - 1].length }
      },
      newText: newContent
    });
  }

  return edits;
}

// Rename provider
connection.onPrepareRename(
  async (params: PrepareRenameParams): Promise<Range | null> => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
      return null;
    }

    const position = params.position;
    const word = getWordAtPosition(document.getText(), position);
    
    if (!word || !/^[a-z][a-zA-Z0-9_]*$/.test(word)) {
      return null;
    }

    const line = document.getText().split('\n')[position.line];
    const start = line.indexOf(word, position.character - word.length);
    
    return {
      start: { line: position.line, character: start },
      end: { line: position.line, character: start + word.length }
    };
  }
);

connection.onRenameRequest(
  async (params: RenameParams): Promise<WorkspaceEdit | null> => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
      return null;
    }

    const position = params.position;
    const oldName = getWordAtPosition(document.getText(), position);
    const newName = params.newName;
    
    if (!oldName || !newName) {
      return null;
    }

    const changes: { [uri: string]: TextEdit[] } = {};
    const text = document.getText();
    const lines = text.split('\n');
    const edits: TextEdit[] = [];

    // Find all occurrences in the document
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const regex = new RegExp(`\\b${oldName}\\b`, 'g');
      let match;
      
      while ((match = regex.exec(line)) !== null) {
        edits.push({
          range: {
            start: { line: i, character: match.index },
            end: { line: i, character: match.index + oldName.length }
          },
          newText: newName
        });
      }
    }

    changes[document.uri] = edits;

    return { changes };
  }
);

// Folding range provider
connection.onFoldingRanges(
  async (params: FoldingRangeParams): Promise<FoldingRange[]> => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
      return [];
    }

    const ranges: FoldingRange[] = [];
    const text = document.getText();
    const lines = text.split('\n');

    let commentStart = -1;
    let ruleStart = -1;

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();

      // Handle comment blocks
      if (line.startsWith('/*')) {
        commentStart = i;
      } else if (line.endsWith('*/') && commentStart >= 0) {
        ranges.push({
          startLine: commentStart,
          endLine: i,
          kind: FoldingRangeKind.Comment
        });
        commentStart = -1;
      }

      // Handle multi-line rules
      if (line.includes(':-') && !line.endsWith('.')) {
        ruleStart = i;
      } else if (ruleStart >= 0 && line.endsWith('.')) {
        if (i > ruleStart) {
          ranges.push({
            startLine: ruleStart,
            endLine: i
          });
        }
        ruleStart = -1;
      }
    }

    return ranges;
  }
);

// Semantic tokens provider
connection.onSemanticTokensFull(
  async (params: SemanticTokensParams): Promise<SemanticTokens> => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
      return { data: [] };
    }

    const builder = new SemanticTokensBuilder(semanticTokensLegend);
    const text = document.getText();
    const lines = text.split('\n');

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      
      // Tokenize the line
      tokenizePrologLine(line, i, builder);
    }

    return builder.build();
  }
);

function tokenizePrologLine(line: string, lineNumber: number, builder: SemanticTokensBuilder) {
  // Comments
  const commentMatch = line.match(/%.*$/);
  if (commentMatch) {
    const start = line.indexOf(commentMatch[0]);
    builder.push(lineNumber, start, commentMatch[0].length, 17, 0); // comment token
  }

  // Strings
  const stringRegex = /'([^'\\]|\\.)*'|"([^"\\]|\\.)*"/g;
  let stringMatch;
  while ((stringMatch = stringRegex.exec(line)) !== null) {
    builder.push(lineNumber, stringMatch.index, stringMatch[0].length, 18, 0); // string token
  }

  // Numbers
  const numberRegex = /\b\d+(\.\d+)?\b/g;
  let numberMatch;
  while ((numberMatch = numberRegex.exec(line)) !== null) {
    builder.push(lineNumber, numberMatch.index, numberMatch[0].length, 19, 0); // number token
  }

  // Predicates
  const predicateRegex = /\b[a-z][a-zA-Z0-9_]*(?=\s*\()/g;
  let predicateMatch;
  while ((predicateMatch = predicateRegex.exec(line)) !== null) {
    builder.push(lineNumber, predicateMatch.index, predicateMatch[0].length, 12, 0); // function token
  }

  // Variables
  const variableRegex = /\b[A-Z_][a-zA-Z0-9_]*\b/g;
  let variableMatch;
  while ((variableMatch = variableRegex.exec(line)) !== null) {
    builder.push(lineNumber, variableMatch.index, variableMatch[0].length, 8, 0); // variable token
  }

  // Operators
  const operatorRegex = /:-|->|;|,|\+|\-|\*|\/|=|\\=|==|\\==|@<|@>|@=<|@>=/g;
  let operatorMatch;
  while ((operatorMatch = operatorRegex.exec(line)) !== null) {
    builder.push(lineNumber, operatorMatch.index, operatorMatch[0].length, 21, 0); // operator token
  }
}

// Additional LSP features can be added here...

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();

export { connection, documents, prologBackend };