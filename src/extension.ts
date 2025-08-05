"use strict";
// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import {
  commands,
  DocumentFilter,
  ExtensionContext,
  Terminal,
  TextDocument,
  window,
  languages,
  DocumentHighlightProvider,
  Location,
  Disposable,
  TextEdit,
  workspace,
  chat,
  ChatRequest,
  ChatResponseStream,
  CancellationToken,
  MarkdownString,
  ThemeIcon,
  ChatResult,
  ChatResultFeedback,
  ProgressLocation,
  Progress
} from "vscode";
import * as path from "path";
import PrologTerminal from "./features/prologTerminal";
import { loadEditHelpers } from "./features/editHelpers";
import { Utils } from "./utils/utils";
import { PlatformUtils, getPlatform, getPlatformDefaults } from "./utils/platformUtils";
import PrologHoverProvider from "./features/hoverProvider";
import PrologDocumentHighlightProvider from "./features/documentHighlightProvider";
import { SnippetUpdater, SnippetUpdaterController, PrologCompletionProvider } from "./features/updateSnippets";
import { PrologFormatter } from "./features/prologFormatter";
import { PrologDebugger } from "./features/prologDebugger";
import { PrologDefinitionProvider } from "./features/definitionProvider";
import { PrologReferenceProvider } from "./features/referenceProvider";
import PrologLinter from "./features/prologLinter";
import { PrologRefactor } from "./features/prologRefactor";
import jsesc from "jsesc";
import * as fs from "fs";
import { PrologBackend } from "./prologBackend";
import { PrologLSPExtension } from "./features/prologLSPExtension";
import { PrologLSPClient } from "./features/prologLSPClient";
import { MultiIDESupport } from "./features/multiIDESupport";
import { ApiServer, ApiServerConfig } from "./features/apiServer";
import { ExternalWebSocketManager, ExternalWebSocketConfig } from "./features/externalWebSocketManager";
import { AuthConfig } from "./features/apiMiddleware";
import { SettingsWebviewProvider } from "./features/settingsWebviewProvider";
import { InstallationChecker } from "./features/installationChecker";
import { InstallationGuide } from "./features/installationGuide";
import { ConfigurationMigration } from "./features/configurationMigration";

// Global backend instance
let prologBackend: PrologBackend | null = null;

// Global LSP client instance
let prologLSPClient: PrologLSPClient | null = null;

// Global API server and WebSocket manager instances
let apiServer: ApiServer | null = null;
let externalWebSocketManager: ExternalWebSocketManager | null = null;

// Telemetry interface (privacy-respecting)
interface TelemetryData {
  command: string;
  success: boolean;
  error?: string;
  timestamp: number;
}

// Simple telemetry collector (can be disabled via settings)
class TelemetryCollector {
  private enabled: boolean = false;
  private data: TelemetryData[] = [];

  constructor() {
    const config = workspace.getConfiguration('prolog');
    this.enabled = config.get<boolean>('telemetry.enabled', false);
  }

  collect(data: TelemetryData) {
    if (this.enabled && this.data.length < 1000) { // Limit storage
      this.data.push(data);
    }
  }

  getStats() {
    if (!this.enabled) return null;
    const commands = this.data.reduce((acc, item) => {
      acc[item.command] = (acc[item.command] || 0) + 1;
      return acc;
    }, {} as Record<string, number>);
    return { totalCommands: this.data.length, commands };
  }
}

const telemetry = new TelemetryCollector();

// Chat participant handler
async function handleChatRequest(
  request: ChatRequest,
  context: any, // ChatContext type not available in current vscode API
  stream: ChatResponseStream,
  token: CancellationToken
): Promise<ChatResult> {
  const startTime = Date.now();
  let command = 'unknown';
  
  try {
    // Ensure backend is running
    if (!prologBackend) {
      stream.markdown('⚠️ **Backend Error**: Prolog backend not initialized. Please restart VS Code.');
      telemetry.collect({ command: 'init_error', success: false, error: 'backend_not_initialized', timestamp: startTime });
      return { metadata: { command: 'error' } };
    }

    if (!prologBackend.isRunning()) {
      stream.markdown('🔄 **Starting Prolog backend...**\n\n');
      await startBackendWithProgress(stream);
    }

    // Parse command from request
    const message = request.prompt.trim();
    const parts = message.split(/\s+/);
    command = parts[0]?.toLowerCase() || 'unknown';

    // Handle cancellation
    if (token.isCancellationRequested) {
      stream.markdown('❌ **Request cancelled**');
      telemetry.collect({ command, success: false, error: 'cancelled', timestamp: startTime });
      return { metadata: { command: 'cancelled' } };
    }

    // Route to appropriate handler
    switch (command) {
      case '/query':
      case 'query':
        await handleQueryCommand(parts.slice(1).join(' '), stream, token);
        break;
      case '/consult':
      case 'consult':
        await handleConsultCommand(parts.slice(1).join(' '), stream, token);
        break;
      case '/help':
      case 'help':
        await handleHelpCommand(parts.slice(1).join(' '), stream, token);
        break;
      case '/status':
      case 'status':
        await handleStatusCommand(stream);
        break;
      case '/n3_load':
      case 'n3_load':
        await handleN3LoadCommand(parts.slice(1).join(' '), stream, token);
        break;
      case '/n3_list':
      case 'n3_list':
        await handleN3ListCommand(parts.slice(1).join(' '), stream, token);
        break;
      case '/n3_reason':
      case 'n3_reason':
        await handleN3ReasonCommand(parts.slice(1).join(' '), stream, token);
        break;
      case '/n3_explain':
      case 'n3_explain':
        await handleN3ExplainCommand(parts.slice(1).join(' '), stream, token);
        break;
      default:
        // Treat as a general query if no command prefix
        if (message.trim()) {
          await handleQueryCommand(message, stream, token);
        } else {
          stream.markdown(getHelpMessage());
        }
        break;
    }

    telemetry.collect({ command, success: true, timestamp: startTime });
    return { metadata: { command } };

  } catch (error) {
    const errorMsg = error instanceof Error ? error.message : String(error);
    stream.markdown(`❌ **Error**: ${errorMsg}`);
    telemetry.collect({ command, success: false, error: errorMsg, timestamp: startTime });
    return { metadata: { command: 'error' } };
  }
}

// Start backend with progress indication
async function startBackendWithProgress(stream: ChatResponseStream): Promise<void> {
  return new Promise((resolve, reject) => {
    if (!prologBackend) {
      reject(new Error('Backend not initialized'));
      return;
    }

    const timeout = setTimeout(() => {
      reject(new Error('Backend startup timeout (30s)'));
    }, 30000);

    const onReady = () => {
      clearTimeout(timeout);
      prologBackend?.off('ready', onReady);
      prologBackend?.off('error', onError);
      stream.markdown('✅ **Prolog backend ready**\n\n');
      resolve();
    };

    const onError = (error: any) => {
      clearTimeout(timeout);
      prologBackend?.off('ready', onReady);
      prologBackend?.off('error', onError);
      reject(new Error(`Backend startup failed: ${error}`));
    };

    prologBackend.on('ready', onReady);
    prologBackend.on('error', onError);
    prologBackend.start();
  });
}

// Handle query commands with retry logic and progress indicators
async function handleQueryCommand(query: string, stream: ChatResponseStream, token: CancellationToken) {
  if (!query.trim()) {
    stream.markdown('❌ **Error**: Please provide a query. Example: `/query member(X, [1,2,3])`');
    return;
  }

  stream.markdown(`🔍 **Querying**: \`${query}\`\n\n`);

  const maxRetries = 3;
  let lastError: any = null;

  for (let attempt = 1; attempt <= maxRetries; attempt++) {
    if (token.isCancellationRequested) {
      stream.markdown('❌ **Query cancelled**');
      return;
    }

    try {
      if (!prologBackend?.isRunning()) {
        throw new Error('Backend not running');
      }

      // Show progress for potentially long-running queries
      stream.progress('🔄 Executing query...');

      // Use streaming request for better performance with large result sets
      const response = await prologBackend.sendStreamingRequest('query', {
        goal: query,
        timeoutMs: 30000, // Increased timeout for large queries
        streaming: true,
        max_results_per_chunk: 50
      }, (chunk, isFirst, isLast) => {
        // Handle streaming chunks
        if (isFirst) {
          stream.progress('📊 Processing results...');
        }
        if (!isLast) {
          stream.progress(`📊 Processing chunk ${chunk.chunk_info?.chunk_index + 1}...`);
        }
      });

      if (response.status === 'ok') {
        if (response.results) {
          // Check if this is a large result set
          const isLargeResult = response.streaming_info?.is_large_result || response.total_chunks > 1;
          
          if (isLargeResult) {
            const totalResults = response.streaming_info?.total_count || response.results.length;
            const chunkSize = response.streaming_info?.chunk_size || response.results.length;
            
            stream.markdown(`✅ **Large Result Set** (${totalResults} total results, showing first ${chunkSize}):\n\n`);
            stream.markdown('*💡 Tip: Results are automatically chunked for better performance. Use pagination commands for more results.*\n\n');
          } else {
            stream.markdown('✅ **Results**:\n\n');
          }
          
          formatQueryResults(response.results, stream, response.streaming_info);
          
          // Show pagination info if available
          if (response.streaming_info?.has_more) {
            stream.markdown(`\n📄 **Pagination**: Showing ${response.streaming_info.chunk_size} of ${response.streaming_info.total_count} results`);
            stream.markdown('\n*Use `/query_next` to see more results or increase chunk size with `/query --chunk-size 100 your_query`*');
          }
        } else if (response.output) {
          stream.markdown('✅ **Output**:\n\n');
          stream.markdown(`\`\`\`\n${response.output}\n\`\`\``);
        } else {
          stream.markdown('✅ **Query succeeded** (no output)');
        }
        return;
      } else {
        throw new Error(response.error || 'Query failed');
      }
    } catch (error) {
      lastError = error;
      const errorMsg = error instanceof Error ? error.message : String(error);
      
      if (attempt < maxRetries && !token.isCancellationRequested) {
        stream.markdown(`⚠️ **Attempt ${attempt} failed**: ${errorMsg}. Retrying...\n\n`);
        stream.progress(`🔄 Retrying (${attempt + 1}/${maxRetries})...`);
        await new Promise(resolve => setTimeout(resolve, 1000 * attempt)); // Exponential backoff
      }
    }
  }

  // All retries failed
  const errorMsg = lastError instanceof Error ? lastError.message : String(lastError);
  stream.markdown(`❌ **Query failed after ${maxRetries} attempts**: ${errorMsg}`);
  
  // Offer fallback
  stream.markdown('\n💡 **Fallback**: You can try running the query directly in a Prolog terminal.');
}

// Handle consult commands
async function handleConsultCommand(filePath: string, stream: ChatResponseStream, token: CancellationToken) {
  if (!filePath.trim()) {
    stream.markdown('❌ **Error**: Please provide a file path. Example: `/consult myfile.pl`');
    return;
  }

  stream.markdown(`📁 **Consulting**: \`${filePath}\`\n\n`);

  try {
    if (!prologBackend?.isRunning()) {
      throw new Error('Backend not running');
    }

    const response = await prologBackend.sendRequest('consult', {
      file: filePath,
      timeoutMs: 15000
    });

    if (response.status === 'ok') {
      stream.markdown(`✅ **File consulted successfully**: \`${filePath}\``);
    } else {
      throw new Error(response.error || 'Consult failed');
    }
  } catch (error) {
    const errorMsg = error instanceof Error ? error.message : String(error);
    stream.markdown(`❌ **Consult failed**: ${errorMsg}`);
    
    // Offer fallback
    stream.markdown('\n💡 **Fallback**: You can try loading the file using the "Prolog: load document" command.');
  }
}

// Handle help commands
async function handleHelpCommand(predicate: string, stream: ChatResponseStream, token: CancellationToken) {
  if (!predicate.trim()) {
    stream.markdown(getHelpMessage());
    return;
  }

  stream.markdown(`📖 **Getting help for**: \`${predicate}\`\n\n`);

  try {
    if (!prologBackend?.isRunning()) {
      throw new Error('Backend not running');
    }

    const response = await prologBackend.sendRequest('help', {
      predicate: predicate,
      timeoutMs: 5000
    });

    if (response.status === 'ok' && response.doc) {
      formatHelpResponse(response.doc, stream);
    } else {
      throw new Error(response.error || 'Help not found');
    }
  } catch (error) {
    const errorMsg = error instanceof Error ? error.message : String(error);
    stream.markdown(`❌ **Help lookup failed**: ${errorMsg}`);
    stream.markdown(`\n💡 **Fallback**: Try searching the SWI-Prolog documentation online for \`${predicate}\`.`);
  }
}

// Handle status command
async function handleStatusCommand(stream: ChatResponseStream) {
  stream.markdown('📊 **Prolog Extension Status**\n\n');
  
  if (prologBackend) {
    const isRunning = prologBackend.isRunning();
    stream.markdown(`- **Backend**: ${isRunning ? '✅ Running' : '❌ Stopped'}\n`);
    
    if (isRunning) {
      try {
        const response = await prologBackend.sendRequest('version', { timeoutMs: 2000 });
        if (response.status === 'ok') {
          stream.markdown(`- **SWI-Prolog Version**: ${response.version}\n`);
        }
      } catch (error) {
        stream.markdown(`- **Version Check**: ❌ Failed\n`);
      }
    }
  } else {
    stream.markdown('- **Backend**: ❌ Not initialized\n');
  }

  const config = workspace.getConfiguration('prolog');
  stream.markdown(`- **Executable Path**: \`${config.get('executablePath', 'swipl')}\`\n`);
  stream.markdown(`- **Dialect**: ${config.get('dialect', 'swi')}\n`);
  
  const stats = telemetry.getStats();
  if (stats) {
    stream.markdown(`- **Commands Used**: ${stats.totalCommands}\n`);
  }
}

// Format query results with streaming info
function formatQueryResults(results: any[], stream: ChatResponseStream, streamingInfo?: any) {
  if (results.length === 0) {
    stream.markdown('🔍 *No results found.*');
    return;
  }

  if (results.length === 1 && Array.isArray(results[0]) && results[0].length === 0) {
    stream.markdown('✅ **true** (goal succeeded with no variable bindings)');
    return;
  }

  // Count total bindings for better presentation
  let totalBindings = 0;
  results.forEach(result => {
    if (Array.isArray(result)) {
      totalBindings += result.length;
    } else if (typeof result === 'object' && result !== null) {
      totalBindings += Object.keys(result).length;
    }
  });

  const displayCount = results.length;
  const totalCount = streamingInfo?.total_count || displayCount;
  
  if (streamingInfo?.is_large_result) {
    stream.markdown(`📊 **Showing ${displayCount} of ${totalCount} solution${totalCount > 1 ? 's' : ''}** with ${totalBindings} variable binding${totalBindings > 1 ? 's' : ''}:\n\n`);
  } else {
    stream.markdown(`📊 **Found ${displayCount} solution${displayCount > 1 ? 's' : ''}** with ${totalBindings} variable binding${totalBindings > 1 ? 's' : ''}:\n\n`);
  }
  
  // Use more compact format for large result sets
  const useCompactFormat = displayCount > 20 || streamingInfo?.is_large_result;
  
  if (useCompactFormat) {
    // Compact format for large results
    stream.markdown('```\n');
    results.forEach((result, solutionIndex) => {
      stream.markdown(`Solution ${solutionIndex + 1}:\n`);
      if (Array.isArray(result) && result.length > 0) {
        result.forEach((binding: any) => {
          if (typeof binding === 'object' && binding !== null) {
            Object.entries(binding).forEach(([variable, value]) => {
              stream.markdown(`  ${variable} = ${value}\n`);
            });
          }
        });
      } else if (typeof result === 'object' && result !== null) {
        Object.entries(result).forEach(([variable, value]) => {
          stream.markdown(`  ${variable} = ${value}\n`);
        });
      }
      stream.markdown('\n');
    });
    stream.markdown('```\n');
  } else {
    // Table format for smaller results
    stream.markdown('| Variable | Value | Solution |\n|----------|-------|----------|\n');
    
    results.forEach((result, solutionIndex) => {
      if (Array.isArray(result) && result.length > 0) {
        result.forEach((binding: any) => {
          if (typeof binding === 'object' && binding !== null) {
            Object.entries(binding).forEach(([variable, value]) => {
              stream.markdown(`| **${variable}** | \`${value}\` | ${solutionIndex + 1} |\n`);
            });
          }
        });
      } else if (typeof result === 'object' && result !== null) {
        Object.entries(result).forEach(([variable, value]) => {
          stream.markdown(`| **${variable}** | \`${value}\` | ${solutionIndex + 1} |\n`);
        });
      }
    });
  }
}

// Format help response (simplified version without problematic Unicode)
function formatHelpResponse(doc: any, stream: ChatResponseStream) {
  stream.markdown(`# ${doc.name}/${doc.arity}\n\n`);
  
  // Status badges
  const badges = [];
  if (doc.is_builtin) badges.push('**Built-in**');
  if (doc.module && doc.module !== 'user') badges.push(`**Module**: \`${doc.module}\``);
  if (doc.deterministic) badges.push('**Deterministic**');
  
  if (badges.length > 0) {
    stream.markdown(`${badges.join(' | ')}\n\n`);
  }
  
  if (doc.summary) {
    stream.markdown(`## Description\n\n${doc.summary}\n\n`);
  }
  
  if (doc.args && doc.args.length > 0) {
    stream.markdown('## Arguments\n\n');
    doc.args.forEach((arg: any, index: number) => {
      stream.markdown(`**${index + 1}. ${arg.name}** - ${arg.description}\n\n`);
    });
  }
  
  if (doc.examples && doc.examples.length > 0) {
    stream.markdown('## Examples\n\n');
    doc.examples.forEach((example: string, index: number) => {
      stream.markdown(`**Example ${index + 1}:**\n\`\`\`prolog\n${example}\n\`\`\`\n\n`);
    });
  }
  
  if (doc.see_also && doc.see_also.length > 0) {
    stream.markdown('## See Also\n\n');
    doc.see_also.forEach((related: string) => {
      stream.markdown(`- \`${related}\`\n`);
    });
    stream.markdown('\n');
  }
}

// Handle N3 load commands
async function handleN3LoadCommand(args: string, stream: ChatResponseStream, token: CancellationToken) {
  const parts = args.trim().split(/\s+/);
  
  if (parts.length === 0 || !parts[0]) {
    stream.markdown('❌ **Error**: Please provide a file path or N3 content. Examples:\n- `/n3_load sample.n3`\n- `/n3_load --content "@prefix : <http://example.org/> . :socrates a :Person ."`');
    return;
  }

  try {
    if (!prologBackend?.isRunning()) {
      throw new Error('Backend not running');
    }

    let requestParams: any;
    
    if (parts[0] === '--content') {
      const content = parts.slice(1).join(' ');
      stream.markdown(`📝 **Loading N3 content**...\n\n`);
      requestParams = { content, timeoutMs: 15000 };
    } else {
      const filePath = parts[0];
      stream.markdown(`📁 **Loading N3 file**: \`${filePath}\`\n\n`);
      requestParams = { file: filePath, timeoutMs: 15000 };
    }

    const response = await prologBackend.sendRequest('n3_load', requestParams);

    if (response.status === 'ok') {
      stream.markdown(`✅ **N3 data loaded successfully**\n- **Triples loaded**: ${response.triples_count}\n`);
    } else {
      throw new Error(response.error || 'N3 load failed');
    }
  } catch (error) {
    const errorMsg = error instanceof Error ? error.message : String(error);
    stream.markdown(`❌ **N3 load failed**: ${errorMsg}`);
  }
}

// Handle N3 list commands with pagination
async function handleN3ListCommand(args: string, stream: ChatResponseStream, token: CancellationToken) {
  try {
    if (!prologBackend?.isRunning()) {
      throw new Error('Backend not running');
    }

    const parts = args.trim().split(/\s+/);
    let limit = 50;
    let offset = 0;
    let format = 'readable';
    
    // Parse arguments
    for (let i = 0; i < parts.length; i++) {
      if (parts[i] === '--limit' && i + 1 < parts.length) {
        const nextPart = parts[i + 1];
        limit = nextPart ? parseInt(nextPart) || 50 : 50;
      } else if (parts[i] === '--offset' && i + 1 < parts.length) {
        const nextPart = parts[i + 1];
        offset = nextPart ? parseInt(nextPart) || 0 : 0;
      } else if (parts[i] === '--format' && i + 1 < parts.length) {
        const nextPart = parts[i + 1];
        format = nextPart || 'readable';
      }
    }

    stream.markdown(`📋 **Listing N3 triples** (limit: ${limit}, offset: ${offset}, format: ${format})...\n\n`);
    stream.progress('🔄 Fetching triples...');

    const response = await prologBackend.sendRequest('n3_list', {
      limit,
      offset,
      format,
      streaming: true,
      timeoutMs: 15000
    });

    if (response.status === 'ok') {
      // Handle paginated response
      if (response.pagination) {
        const { total_count, returned_count, has_more, next_offset } = response.pagination;
        stream.markdown(`✅ **Found ${total_count} triples** (showing ${returned_count}, offset: ${offset}):\n\n`);
        
        if (response.triples && response.triples.length > 0) {
          // Use compact format for large result sets
          if (returned_count > 20) {
            stream.markdown('```\n');
            response.triples.forEach((triple: any, index: number) => {
              stream.markdown(`${offset + index + 1}. ${triple.subject} ${triple.predicate} ${triple.object}\n`);
            });
            stream.markdown('```\n');
          } else {
            stream.markdown('| # | Subject | Predicate | Object |\n|---|---------|-----------|--------|\n');
            response.triples.forEach((triple: any, index: number) => {
              stream.markdown(`| ${offset + index + 1} | \`${triple.subject}\` | \`${triple.predicate}\` | \`${triple.object}\` |\n`);
            });
          }
          
          // Show pagination controls
          if (has_more) {
            stream.markdown(`\n📄 **Pagination**: Showing ${offset + 1}-${offset + returned_count} of ${total_count} triples`);
            stream.markdown(`\n*Use \`/n3_list --offset ${next_offset} --limit ${limit}\` for next page*`);
          }
        } else {
          stream.markdown('*No triples found.*');
        }
      } else {
        // Legacy response format
        const total_count = response.total_count || 0;
        const returned_count = response.returned_count || 0;
        stream.markdown(`✅ **Found ${total_count} triples** (showing ${returned_count}):\n\n`);
        
        if (response.triples && response.triples.length > 0) {
          stream.markdown('| Subject | Predicate | Object |\n|---------|-----------|--------|\n');
          response.triples.forEach((triple: any) => {
            stream.markdown(`| \`${triple.subject}\` | \`${triple.predicate}\` | \`${triple.object}\` |\n`);
          });
        } else {
          stream.markdown('*No triples found.*');
        }
      }
    } else {
      throw new Error(response.error || 'N3 list failed');
    }
  } catch (error) {
    const errorMsg = error instanceof Error ? error.message : String(error);
    stream.markdown(`❌ **N3 list failed**: ${errorMsg}`);
  }
}

// Handle N3 reason commands
async function handleN3ReasonCommand(args: string, stream: ChatResponseStream, token: CancellationToken) {
  try {
    if (!prologBackend?.isRunning()) {
      throw new Error('Backend not running');
    }

    const goal = args.trim();
    
    if (goal) {
      stream.markdown(`🧠 **N3 reasoning with goal**: \`${goal}\`\n\n`);
    } else {
      stream.markdown(`🧠 **N3 reasoning** (finding all inferred triples)...\n\n`);
    }

    const response = await prologBackend.sendRequest('n3_reason', {
      goal: goal || '',
      timeoutMs: 15000
    });

    if (response.status === 'ok') {
      if (response.results) {
        stream.markdown(`✅ **Reasoning results** (${response.count} found):\n\n`);
        response.results.forEach((result: any, index: number) => {
          stream.markdown(`**Result ${index + 1}**: \`${JSON.stringify(result)}\`\n`);
        });
      } else if (response.inferred_triples) {
        stream.markdown(`✅ **Inferred triples** (${response.count} found):\n\n`);
        if (response.inferred_triples.length > 0) {
          stream.markdown('| Subject | Predicate | Object |\n|---------|-----------|--------|\n');
          response.inferred_triples.forEach((triple: any) => {
            stream.markdown(`| \`${triple.subject}\` | \`${triple.predicate}\` | \`${triple.object}\` |\n`);
          });
        } else {
          stream.markdown('*No new triples inferred.*');
        }
      }
    } else {
      throw new Error(response.error || 'N3 reasoning failed');
    }
  } catch (error) {
    const errorMsg = error instanceof Error ? error.message : String(error);
    stream.markdown(`❌ **N3 reasoning failed**: ${errorMsg}`);
  }
}

// Handle N3 explain commands
async function handleN3ExplainCommand(args: string, stream: ChatResponseStream, token: CancellationToken) {
  const goal = args.trim();
  
  if (!goal) {
    stream.markdown('❌ **Error**: Please provide a goal to explain. Example: `/n3_explain rdf(socrates, type, Mortal)`');
    return;
  }

  try {
    if (!prologBackend?.isRunning()) {
      throw new Error('Backend not running');
    }

    stream.markdown(`🔍 **Explaining goal**: \`${goal}\`\n\n`);

    const response = await prologBackend.sendRequest('n3_explain', {
      goal,
      timeoutMs: 15000
    });

    if (response.status === 'ok') {
      stream.markdown(`✅ **Proof explanation for**: \`${response.goal}\`\n\n`);
      formatProofTree(response.proof, stream, 0);
    } else {
      throw new Error(response.error || 'N3 explanation failed');
    }
  } catch (error) {
    const errorMsg = error instanceof Error ? error.message : String(error);
    stream.markdown(`❌ **N3 explanation failed**: ${errorMsg}`);
  }
}

// Format proof tree for display
function formatProofTree(proof: any, stream: ChatResponseStream, depth: number) {
  const indent = '  '.repeat(depth);
  
  if (proof.type === 'proof') {
    stream.markdown(`${indent}🎯 **Goal**: \`${proof.goal}\`\n`);
    if (proof.subproofs && proof.subproofs.length > 0) {
      proof.subproofs.forEach((subproof: any) => {
        formatProofTree(subproof, stream, depth + 1);
      });
    }
  } else if (proof.type === 'fact') {
    stream.markdown(`${indent}📋 **Fact**: \`${proof.fact}\`\n`);
  } else if (proof.type === 'inference') {
    stream.markdown(`${indent}🔗 **Inference**: \`${proof.inference}\`\n`);
  } else if (proof.type === 'builtin') {
    stream.markdown(`${indent}⚙️ **Builtin**: \`${proof.builtin}\`\n`);
  }
}

// Get help message
function getHelpMessage(): string {
  return `# 🤖 Prolog Assistant

Welcome! I'm your intelligent Prolog companion, ready to help with queries, file consultation, documentation lookup, and advanced N3 semantic reasoning.

---

## 🔍 **Core Commands**

### **\`/query <goal>\`** 
Execute Prolog queries with intelligent result formatting
- **Example**: \`/query member(X, [1,2,3])\`
- **Example**: \`/query append([1,2], [3,4], Result)\`
- **Tip**: You can omit \`/query\` for simple queries

### **\`/consult <file>\`**
Load and consult Prolog files into the knowledge base
- **Example**: \`/consult family.pl\`
- **Example**: \`/consult ./src/rules.pl\`

### **\`/help <predicate>\`**
Get comprehensive documentation for predicates
- **Example**: \`/help member/2\`
- **Example**: \`/help findall/3\`
- **Example**: \`/help append\`

### **\`/status\`**
Check backend health and configuration
- Shows SWI-Prolog version, backend status, and usage statistics

---

## 🧠 **N3 Semantic Web Commands**

### **\`/n3_load <file|--content>\`**
Load N3/Turtle semantic data for reasoning
- **File**: \`/n3_load sample.n3\`
- **Inline**: \`/n3_load --content "@prefix : <http://example.org/> . :socrates a :Person ."\`

### **\`/n3_list [--limit N] [--format readable]\`**
Browse loaded triples with smart formatting
- **Example**: \`/n3_list --limit 20\`
- **Example**: \`/n3_list --format readable\`

### **\`/n3_reason [goal]\`**
Perform intelligent N3 reasoning and inference
- **All inferences**: \`/n3_reason\`
- **Specific goal**: \`/n3_reason rdf(X, type, Mortal)\`

### **\`/n3_explain <goal>\`**
Generate detailed proof trees with step-by-step explanations
- **Example**: \`/n3_explain rdf(socrates, type, Mortal)\`

---

## 💡 **Pro Tips**

- 🚀 **Quick queries**: Just type your goal directly (no \`/query\` needed)
- ⏹️ **Cancel operations**: Use Ctrl+C for long-running tasks
- 🔧 **Troubleshooting**: Run \`/status\` if commands aren't working
- 🏷️ **Readable URIs**: N3 commands show prefixed names (e.g., \`:socrates\`)
- 📊 **Rich results**: Variable bindings displayed in formatted tables
- 🔄 **Auto-retry**: Failed queries automatically retry with exponential backoff

---

## 🎯 **Quick Start Examples**

\`\`\`prolog
# Basic Prolog
member(X, [a,b,c])
append([1,2], [3,4], X)

# File operations
/consult family.pl
/help findall/3

# Semantic reasoning
/n3_load sample.n3
/n3_reason
/n3_explain rdf(socrates, type, Mortal)
\`\`\`

---

*Ready to explore the power of logic programming and semantic reasoning? Just ask me anything!* ✨`;
}

// initialisation of workspace
async function initForDialect(context: ExtensionContext) {
  // get the user preferences for the extention
  const section = workspace.getConfiguration("prolog");
  const dialect = section.get<string>("dialect");
  
  // Use platform-aware executable path resolution
  let exec = section.get<string>("executablePath", "");
  if (!exec) {
    exec = getPlatformDefaults().defaultExecutablePath;
  }
  exec = PlatformUtils.normalizePath(exec);
  
  Utils.LINTERTRIGGER = section.get<string>("linter.run") || "never";
  Utils.FORMATENABLED = section.get<boolean>("format.enabled") || false;
  Utils.DIALECT = dialect || "swi";
  Utils.RUNTIMEPATH = jsesc(exec);
  
  // Use platform-aware path construction
  const exPath = PlatformUtils.normalizePath(context.extensionPath);
  Utils.EXPATH = exPath;
  
  // check if the dialect links have already been done
  const diaFile = PlatformUtils.joinPath(exPath, ".vscode", "dialect.json");
  
  let lastDialect = '';
  try {
    if (await PlatformUtils.pathExists(diaFile)) {
      const dialectContent = fs.readFileSync(diaFile, 'utf8');
      lastDialect = JSON.parse(dialectContent).dialect;
    }
  } catch (err) {
    // File doesn't exist or is invalid, continue with setup
    console.log('[Platform] Dialect file not found or invalid, proceeding with setup');
  }
  
  if (lastDialect === dialect) {
    return;
  }

  // creating links for the right dialect using platform-aware paths
  const symLinks = [
    {
      path: PlatformUtils.joinPath(exPath, "syntaxes"),
      srcFile: `prolog.${dialect}.tmLanguage.json`,
      targetFile: "prolog.tmLanguage.json"
    },
    {
      path: PlatformUtils.joinPath(exPath, "snippets"),
      srcFile: `prolog.${dialect}.json`,
      targetFile: "prolog.json"
    }
  ];
  
  await Promise.all(
    symLinks.map(async link => {
      const srcPath = PlatformUtils.joinPath(link.path, link.srcFile);
      const targetPath = PlatformUtils.joinPath(link.path, link.targetFile);
      
      // remove old link
      try {
        if (await PlatformUtils.pathExists(targetPath)) {
          fs.unlinkSync(targetPath);
        }
      } catch (err) {
        // Ignore errors when removing non-existent files
      }
      
      // make link
      try {
        // Try to create symlink, fallback to copy if symlink fails
        try {
          fs.symlinkSync(srcPath, targetPath);
        } catch (symlinkErr) {
          // If symlink fails (e.g., on Windows without admin), copy the file
          fs.copyFileSync(srcPath, targetPath);
        }
      } catch (err) {
        const platform = getPlatform();
        const errorMsg = platform === 'windows'
          ? "VSC-Prolog failed in initialization. Try running VS Code as administrator or enable Developer Mode in Windows Settings."
          : "VSC-Prolog failed in initialization. Check file permissions.";
        window.showErrorMessage(errorMsg);
        throw (err);
      }
    })
  );
  
  // Ensure .vscode directory exists
  const vscodeDirPath = PlatformUtils.joinPath(exPath, ".vscode");
  if (!await PlatformUtils.pathExists(vscodeDirPath)) {
    fs.mkdirSync(vscodeDirPath, { recursive: true });
  }
  
  // write the dialect to the json for later initialisation
  fs.writeFileSync(diaFile, JSON.stringify({ dialect: dialect }));
}

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export async function activate(context: ExtensionContext) {
  console.log('Congratulations, your extension "vsc-prolog" is now active! :)');

  // Check SWI-Prolog installation before proceeding
  await checkAndHandleInstallation(context);

  // initialisation of workspace
  await initForDialect(context);

  // filter the files to process
  const PROLOG_MODE: DocumentFilter = { language: "prolog", scheme: "file" };

  // initialisation of utils class and load snippets file with it's predicates
  Utils.init(context);
  // automatic indent on change
  loadEditHelpers(context.subscriptions);
  // extention special commands
  let myCommands = [
    {
      command: "prolog.load.document",
      callback: () => {
        PrologTerminal.loadDocument();
      }
    },
    {
      command: "prolog.query.goal",
      callback: () => {
        PrologTerminal.queryGoalUnderCursor();
      }
    },
    {
      command: "prolog.refactorPredicate",
      callback: () => {
        new PrologRefactor().refactorPredUnderCursor();
      }
    },
    {
      command: "prolog.openSettings",
      callback: () => {
        // This will be handled by the webview view registration
      }
    },
    {
      command: "prolog.setupWizard",
      callback: async () => {
        const installationGuide = InstallationGuide.getInstance();
        await installationGuide.runSetupWizard();
      }
    }
  ];
  // error detection and possible patch
  let linter: PrologLinter | undefined;
  if (Utils.LINTERTRIGGER !== "never") {
    linter = new PrologLinter(context);
    linter.activate();
    // extention special commands for linter
    myCommands = myCommands.concat([
      {
        command: "prolog.linter.nextErrLine",
        callback: () => {
          linter?.nextErrLine();
        }
      },
      {
        command: "prolog.linter.prevErrLine",
        callback: () => {
          linter?.prevErrLine();
        }
      }
    ]);
  }
  // register commands
  myCommands.map(command => {
    context.subscriptions.push(
      commands.registerCommand(command.command, command.callback)
    );
  });
  // if linter is not prohibited by the user
  if (Utils.LINTERTRIGGER !== "never" && linter) {
    context.subscriptions.push(
      languages.registerCodeActionsProvider(PROLOG_MODE, linter)
    );
  }
  // Hover provider
  context.subscriptions.push(
    languages.registerHoverProvider(PROLOG_MODE, new PrologHoverProvider())
  );
  //Highlight provider
  context.subscriptions.push(
    languages.registerDocumentHighlightProvider(
      PROLOG_MODE,
      new PrologDocumentHighlightProvider()
    )
  );
  // Definition provider (go to definition command)
  context.subscriptions.push(
    languages.registerDefinitionProvider(
      PROLOG_MODE,
      new PrologDefinitionProvider()
    )
  );
  // Reference provider (find all references command)
  context.subscriptions.push(
    languages.registerReferenceProvider(
      PROLOG_MODE,
      new PrologReferenceProvider()
    )
  );
  // create prolog terminal (load file command)
  context.subscriptions.push(PrologTerminal.init());
  //PrologDebugger;

  // add created predicate to the snippet
  let snippetUpdater = new SnippetUpdater();
  context.subscriptions.push(new SnippetUpdaterController(snippetUpdater));
  context.subscriptions.push(snippetUpdater);

  // auto completion provider
  context.subscriptions.push(
    languages.registerCompletionItemProvider(PROLOG_MODE, new PrologCompletionProvider())
  );
  
  // file formating provider
  context.subscriptions.push(
    languages.registerDocumentRangeFormattingEditProvider(
      PROLOG_MODE,
      new PrologFormatter()
    )
  );
  context.subscriptions.push(
    languages.registerDocumentFormattingEditProvider(PROLOG_MODE, new PrologFormatter())
  );

  // Initialize Prolog backend
  const config = workspace.getConfiguration('prolog');
  const swiplPath = config.get<string>('executablePath', 'swipl');
  
  prologBackend = new PrologBackend({
    swiplPath,
    port: 3060,
    streamingEnabled: true,
    maxResultsPerChunk: 50
  });

  // Initialize API server if enabled
  const apiServerEnabled = config.get<boolean>('apiServer.enabled', false);
  if (apiServerEnabled) {
    try {
      const apiServerConfig: ApiServerConfig = {
        enabled: true,
        port: config.get<number>('apiServer.port', 8080),
        host: config.get<string>('apiServer.host', 'localhost'),
        corsOrigins: config.get<string[]>('apiServer.corsOrigins', ['http://localhost:*']),
        maxConnections: config.get<number>('apiServer.maxConnections', 100),
        requestTimeout: config.get<number>('apiServer.requestTimeout', 60000),
        rateLimiting: {
          enabled: config.get<boolean>('apiServer.rateLimiting.enabled', true),
          requestsPerMinute: config.get<number>('apiServer.rateLimiting.requestsPerMinute', 60),
          burstLimit: config.get<number>('apiServer.rateLimiting.burstLimit', 10)
        },
        auth: createAuthConfig(config)
      };

      apiServer = new ApiServer({
        config: apiServerConfig,
        prologBackend
      });

      // Initialize external WebSocket manager if enabled
      const wsEnabled = config.get<boolean>('webSocketServer.enabled', true);
      if (wsEnabled) {
        const wsConfig: ExternalWebSocketConfig = {
          enabled: true,
          port: config.get<number>('webSocketServer.port', 8081),
          maxConnections: config.get<number>('webSocketServer.maxConnections', 50),
          heartbeatInterval: config.get<number>('webSocketServer.heartbeatInterval', 30),
          auth: apiServerConfig.auth
        };

        externalWebSocketManager = new ExternalWebSocketManager(
          wsConfig,
          prologBackend.getNotificationManager()
        );
      }

      console.log('[Extension] API server and WebSocket manager initialized');
    } catch (error) {
      console.error('[Extension] Failed to initialize API server:', error);
      window.showErrorMessage(`Failed to initialize API server: ${error}`);
    }
  }

  // Initialize LSP Extension (legacy support)
  const lspExtension = new PrologLSPExtension(context, prologBackend);
  lspExtension.registerFeatures();
  context.subscriptions.push(lspExtension);

  // Initialize full LSP Client
  prologLSPClient = new PrologLSPClient(context);
  
  // Start LSP client
  try {
    await prologLSPClient.start();
    console.log('[Extension] Prolog LSP Client started successfully');
  } catch (error) {
    console.error('[Extension] Failed to start Prolog LSP Client:', error);
    window.showWarningMessage('Prolog LSP Client failed to start. Some features may not be available.');
  }

  // Generate multi-IDE configurations
  const workspaceFolder = workspace.workspaceFolders?.[0];
  if (workspaceFolder) {
    try {
      await MultiIDESupport.generateIDEConfigurations(workspaceFolder.uri.fsPath);
      MultiIDESupport.generateLaunchConfigurations(workspaceFolder.uri.fsPath);
      
      // Detect available IDEs
      const availableIDEs = await MultiIDESupport.detectAvailableIDEs();
      if (availableIDEs.length > 1) {
        console.log('[Extension] Detected IDEs:', availableIDEs.join(', '));
      }
    } catch (error) {
      console.error('[Extension] Failed to generate multi-IDE configurations:', error);
    }
  }

  // Set up backend event handlers
  prologBackend.on('ready', () => {
    console.log('[Extension] Prolog backend ready');
    window.showInformationMessage('Prolog backend started successfully');
  });

  prologBackend.on('stopped', () => {
    console.log('[Extension] Prolog backend stopped');
    window.showWarningMessage('Prolog backend stopped');
  });

  prologBackend.on('restarted', () => {
    console.log('[Extension] Prolog backend restarted');
    window.showInformationMessage('Prolog backend restarted successfully');
  });

  prologBackend.on('error', (error) => {
    console.error('[Extension] Prolog backend error:', error);
    window.showErrorMessage(`Prolog backend error: ${error}`);
  });

  // Register chat participant with enhanced followup provider
  const chatParticipant = chat.createChatParticipant('prolog', handleChatRequest);
  chatParticipant.iconPath = new ThemeIcon('symbol-class');
  chatParticipant.followupProvider = {
    provideFollowups(result: ChatResult, context: any, token: CancellationToken) {
      const followups = [];
      
      // Context-aware followups based on command type
      if (result.metadata?.command === 'query') {
        followups.push({
          prompt: '/status',
          label: '🔧 Check backend status'
        });
        followups.push({
          prompt: '/help findall/3',
          label: '📖 Learn about findall/3'
        });
      }
      
      if (result.metadata?.command === 'consult') {
        followups.push({
          prompt: '/query',
          label: '🔍 Run a query'
        });
      }
      
      if (result.metadata?.command === 'error') {
        followups.push({
          prompt: '/status',
          label: '🚨 Check what went wrong'
        });
        followups.push({
          prompt: '/help',
          label: '❓ Show help'
        });
      }
      
      if (result.metadata?.command === 'n3_load') {
        followups.push({
          prompt: '/n3_list --limit 10',
          label: '📋 List loaded triples'
        });
        followups.push({
          prompt: '/n3_reason',
          label: '🧠 Start reasoning'
        });
      }
      
      // Always include help as fallback
      followups.push({
        prompt: '/help',
        label: '💡 Show all commands'
      });
      
      return followups;
    }
  };

  context.subscriptions.push(chatParticipant);

  // Register settings webview provider
  const settingsProvider = new SettingsWebviewProvider(context.extensionUri);
  context.subscriptions.push(
    window.registerWebviewViewProvider(SettingsWebviewProvider.viewType, settingsProvider)
  );

  // Start backend automatically (with error handling)
  try {
    prologBackend.start();
  } catch (error) {
    console.error('[Extension] Failed to start Prolog backend:', error);
    // Don't show error immediately - let user trigger it via chat if needed
  }
}

// Installation checking and handling
async function checkAndHandleInstallation(context: ExtensionContext): Promise<void> {
  try {
    const installationChecker = InstallationChecker.getInstance();
    const configurationMigration = ConfigurationMigration.getInstance();
    
    // Set extension context for configuration migration
    configurationMigration.setExtensionContext(context);
    
    // Check for configuration migration needs first
    await configurationMigration.performComprehensiveMigration();
    
    const installationStatus = await installationChecker.checkSwiplInstallation();
    
    if (!installationStatus.isInstalled) {
      // SWI-Prolog not found - show user-friendly guidance
      const action = await window.showWarningMessage(
        'SWI-Prolog is not installed or not found in the configured path. The Prolog extension requires SWI-Prolog to function properly.',
        'Install SWI-Prolog',
        'Setup Wizard',
        'Configure Path',
        'Continue Anyway'
      );
      
      switch (action) {
        case 'Install SWI-Prolog':
          await showInstallationInstructions();
          break;
        case 'Setup Wizard':
          await commands.executeCommand('prolog.setupWizard');
          break;
        case 'Configure Path':
          await commands.executeCommand('workbench.action.openSettings', 'prolog.executablePath');
          break;
        case 'Continue Anyway':
          window.showInformationMessage('Some Prolog features may not work without SWI-Prolog installed.');
          break;
      }
    } else if (installationStatus.issues && installationStatus.issues.length > 0) {
      // Installation found but has issues
      const issueMessage = installationStatus.issues.join('; ');
      const action = await window.showInformationMessage(
        `SWI-Prolog found at ${installationStatus.path} (version ${installationStatus.version}), but there are configuration issues: ${issueMessage}`,
        'Fix Configuration',
        'Ignore'
      );
      
      if (action === 'Fix Configuration') {
        const updateResult = await installationChecker.validateAndUpdateConfiguration();
        if (updateResult.updated) {
          window.showInformationMessage(
            `Configuration updated: SWI-Prolog path changed from '${updateResult.oldPath}' to '${updateResult.newPath}'`
          );
        }
      }
    } else {
      // Installation is good - show success message only in development mode
      console.log(`SWI-Prolog found: ${installationStatus.path} (version ${installationStatus.version})`);
    }
  } catch (error) {
    console.error('Error checking SWI-Prolog installation:', error);
    window.showErrorMessage('Failed to check SWI-Prolog installation. Some features may not work properly.');
  }
}

// Show platform-specific installation instructions using InstallationGuide
async function showInstallationInstructions(): Promise<void> {
  const installationGuide = InstallationGuide.getInstance();
  await installationGuide.showInstallationGuideDialog();
}

// Helper function to create auth configuration from VSCode settings
function createAuthConfig(config: any): AuthConfig {
  const method = config.get('apiServer.auth.method', 'api_key') as string;
  // Ensure method is one of the valid values
  const validMethods = ['api_key', 'jwt_token', 'local_only', 'oauth2'] as const;
  const authMethod = validMethods.includes(method as any) ? method as typeof validMethods[number] : 'api_key';
  
  return {
    method: authMethod,
    apiKeys: config.get('apiServer.auth.apiKeys', []) as string[],
    jwtSecret: config.get('apiServer.auth.jwtSecret', '') as string,
    localOnly: config.get('apiServer.auth.localOnly', true) as boolean,
    oauth2: {
      providers: config.get('apiServer.auth.oauth2.providers', ['google', 'github']) as string[],
      clientId: config.get('apiServer.auth.oauth2.clientId', '') as string,
      clientSecret: config.get('apiServer.auth.oauth2.clientSecret', '') as string,
      redirectUri: config.get('apiServer.auth.oauth2.redirectUri', '') as string,
      scope: config.get('apiServer.auth.oauth2.scope', 'read') as string
    },
    roles: {
      admin: config.get('apiServer.auth.roles.admin', []) as string[],
      agent: config.get('apiServer.auth.roles.agent', []) as string[],
      readonly: config.get('apiServer.auth.roles.readonly', []) as string[],
      limited: config.get('apiServer.auth.roles.limited', []) as string[]
    },
    quotas: {
      admin: {
        requestsPerMinute: config.get('apiServer.auth.quotas.admin.requestsPerMinute', 1000) as number,
        maxConcurrentSessions: config.get('apiServer.auth.quotas.admin.maxConcurrentSessions', 50) as number
      },
      agent: {
        requestsPerMinute: config.get('apiServer.auth.quotas.agent.requestsPerMinute', 300) as number,
        maxConcurrentSessions: config.get('apiServer.auth.quotas.agent.maxConcurrentSessions', 20) as number
      },
      readonly: {
        requestsPerMinute: config.get('apiServer.auth.quotas.readonly.requestsPerMinute', 100) as number,
        maxConcurrentSessions: config.get('apiServer.auth.quotas.readonly.maxConcurrentSessions', 10) as number
      },
      limited: {
        requestsPerMinute: config.get('apiServer.auth.quotas.limited.requestsPerMinute', 30) as number,
        maxConcurrentSessions: config.get('apiServer.auth.quotas.limited.maxConcurrentSessions', 5) as number
      }
    }
  };
}

// this method is called when your extension is deactivated
export async function deactivate() {
  // Stop API server
  if (apiServer) {
    try {
      await apiServer.stop();
      console.log('[Extension] API server stopped');
    } catch (error) {
      console.error('[Extension] Error stopping API server:', error);
    }
    apiServer = null;
  }

  // Stop external WebSocket manager
  if (externalWebSocketManager) {
    try {
      await externalWebSocketManager.stop();
      console.log('[Extension] External WebSocket manager stopped');
    } catch (error) {
      console.error('[Extension] Error stopping WebSocket manager:', error);
    }
    externalWebSocketManager = null;
  }

  // Stop LSP client
  if (prologLSPClient) {
    try {
      await prologLSPClient.stop();
      console.log('[Extension] Prolog LSP Client stopped');
    } catch (error) {
      console.error('[Extension] Error stopping LSP Client:', error);
    }
    prologLSPClient = null;
  }

  // Stop backend
  if (prologBackend) {
    prologBackend.stop(true);
    prologBackend = null;
  }
}