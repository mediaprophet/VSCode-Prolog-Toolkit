'use strict';

import {
  ChatRequest,
  ChatResponseStream,
  CancellationToken,
  ChatResult,
  workspace,
} from 'vscode';
import { PrologBackend } from '../prologBackend';
import { TelemetryCollector, TelemetryData } from './telemetryCollector';

export class ChatHandler {
  private prologBackend: PrologBackend | null = null;
  private telemetry: TelemetryCollector;

  constructor(prologBackend: PrologBackend | null, telemetry: TelemetryCollector) {
    this.prologBackend = prologBackend;
    this.telemetry = telemetry;
  }

  // Update backend reference
  updateBackend(backend: PrologBackend | null) {
    this.prologBackend = backend;
  }

  // Chat participant handler
  async handleChatRequest(
    request: ChatRequest,
    context: any, // ChatContext type not available in current vscode API
    stream: ChatResponseStream,
    _token: CancellationToken
  ): Promise<ChatResult> {
    const startTime = Date.now();
    let command = 'unknown';

    try {
      // Ensure backend is running
      if (!this.prologBackend) {
        stream.markdown(
          '⚠️ **Backend Error**: Prolog backend not initialized. Please restart VS Code.'
        );
        this.telemetry.collect({
          command: 'init_error',
          success: false,
          error: 'backend_not_initialized',
          timestamp: startTime,
        });
        return { metadata: { command: 'error' } };
      }

      if (!this.prologBackend.isRunning()) {
        stream.markdown('🔄 **Starting Prolog backend...**\n\n');
        await this.startBackendWithProgress(stream);
      }

      // Parse command from request
      const message = request.prompt.trim();
      const parts = message.split(/\s+/);
      command = parts[0]?.toLowerCase() || 'unknown';

      // Handle cancellation
      if (_token.isCancellationRequested) {
        stream.markdown('❌ **Request cancelled**');
        this.telemetry.collect({ command, success: false, error: 'cancelled', timestamp: startTime });
        return { metadata: { command: 'cancelled' } };
      }

      // Route to appropriate handler
      switch (command) {
        case '/query':
        case 'query':
          await this.handleQueryCommand(parts.slice(1).join(' '), stream, _token);
          break;
        case '/consult':
        case 'consult':
          await this.handleConsultCommand(parts.slice(1).join(' '), stream, _token);
          break;
        case '/help':
        case 'help':
          await this.handleHelpCommand(parts.slice(1).join(' '), stream, _token);
          break;
        case '/status':
        case 'status':
          await this.handleStatusCommand(stream);
          break;
        case '/n3_load':
        case 'n3_load':
          await this.handleN3LoadCommand(parts.slice(1).join(' '), stream, _token);
          break;
        case '/n3_list':
        case 'n3_list':
          await this.handleN3ListCommand(parts.slice(1).join(' '), stream, _token);
          break;
        case '/n3_reason':
        case 'n3_reason':
          await this.handleN3ReasonCommand(parts.slice(1).join(' '), stream, _token);
          break;
        case '/n3_explain':
        case 'n3_explain':
          await this.handleN3ExplainCommand(parts.slice(1).join(' '), stream, _token);
          break;
        default:
          // Treat as a general query if no command prefix
          if (message.trim()) {
            await this.handleQueryCommand(message, stream, _token);
          } else {
            stream.markdown(this.getHelpMessage());
          }
          break;
      }

      this.telemetry.collect({ command, success: true, timestamp: startTime });
      return { metadata: { command } };
    } catch (error) {
      const errorMsg = error instanceof Error ? error.message : String(error);
      stream.markdown(`❌ **Error**: ${errorMsg}`);
      this.telemetry.collect({ command, success: false, error: errorMsg, timestamp: startTime });
      return { metadata: { command: 'error' } };
    }
  }

  // Start backend with progress indication
  private async startBackendWithProgress(stream: ChatResponseStream): Promise<void> {
    return new Promise((resolve, reject) => {
      if (!this.prologBackend) {
        reject(new Error('Backend not initialized'));
        return;
      }

      const timeout = setTimeout(() => {
        reject(new Error('Backend startup timeout (30s)'));
      }, 30000);

      const onReady = () => {
        clearTimeout(timeout);
        this.prologBackend?.off('ready', onReady);
        this.prologBackend?.off('error', onError);
        stream.markdown('✅ **Prolog backend ready**\n\n');
        resolve();
      };

      const onError = (error: any) => {
        clearTimeout(timeout);
        this.prologBackend?.off('ready', onReady);
        this.prologBackend?.off('error', onError);
        reject(new Error(`Backend startup failed: ${error}`));
      };

      this.prologBackend.on('ready', onReady);
      this.prologBackend.on('error', onError);
      this.prologBackend.start();
    });
  }

  // Handle query commands with retry logic and progress indicators
  private async handleQueryCommand(
    query: string,
    stream: ChatResponseStream,
    _token: CancellationToken
  ) {
    if (!query.trim()) {
      stream.markdown('❌ **Error**: Please provide a query. Example: `/query member(X, [1,2,3])`');
      return;
    }

    stream.markdown(`🔍 **Querying**: \`${query}\`\n\n`);

    const maxRetries = 3;
    let lastError: any = null;

    for (let attempt = 1; attempt <= maxRetries; attempt++) {
      if (_token.isCancellationRequested) {
        stream.markdown('❌ **Query cancelled**');
        return;
      }

      try {
        if (!this.prologBackend?.isRunning()) {
          throw new Error('Backend not running');
        }

        // Show progress for potentially long-running queries
        stream.progress('🔄 Executing query...');

        // Use streaming request for better performance with large result sets
        const response = await this.prologBackend.sendStreamingRequest(
          'query',
          {
            goal: query,
            timeoutMs: 30000, // Increased timeout for large queries
            streaming: true,
            max_results_per_chunk: 50,
          },
          (chunk, isFirst, isLast) => {
            // Handle streaming chunks
            if (isFirst) {
              stream.progress('📊 Processing results...');
            }
            if (!isLast) {
              stream.progress(`📊 Processing chunk ${chunk.chunk_info?.chunk_index + 1}...`);
            }
          }
        );

        if (response.status === 'ok') {
          if (response.results) {
            // Check if this is a large result set
            const isLargeResult =
              response.streaming_info?.is_large_result || response.total_chunks > 1;

            if (isLargeResult) {
              const totalResults = response.streaming_info?.total_count || response.results.length;
              const chunkSize = response.streaming_info?.chunk_size || response.results.length;

              stream.markdown(
                `✅ **Large Result Set** (${totalResults} total results, showing first ${chunkSize}):\n\n`
              );
              stream.markdown(
                '*💡 Tip: Results are automatically chunked for better performance. Use pagination commands for more results.*\n\n'
              );
            } else {
              stream.markdown('✅ **Results**:\n\n');
            }

            this.formatQueryResults(response.results, stream, response.streaming_info);

            // Show pagination info if available
            if (response.streaming_info?.has_more) {
              stream.markdown(
                `\n📄 **Pagination**: Showing ${response.streaming_info.chunk_size} of ${response.streaming_info.total_count} results`
              );
              stream.markdown(
                '\n*Use `/query_next` to see more results or increase chunk size with `/query --chunk-size 100 your_query`*'
              );
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

        if (attempt < maxRetries && !_token.isCancellationRequested) {
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
    stream.markdown(
      '\n💡 **Fallback**: You can try running the query directly in a Prolog terminal.'
    );
  }

  // Handle consult commands
  private async handleConsultCommand(
    filePath: string,
    stream: ChatResponseStream,
    __token: CancellationToken
  ) {
    if (!filePath.trim()) {
      stream.markdown('❌ **Error**: Please provide a file path. Example: `/consult myfile.pl`');
      return;
    }

    stream.markdown(`📁 **Consulting**: \`${filePath}\`\n\n`);

    try {
      if (!this.prologBackend?.isRunning()) {
        throw new Error('Backend not running');
      }

      const response = await this.prologBackend.sendRequest('consult', {
        file: filePath,
        timeoutMs: 15000,
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
      stream.markdown(
        '\n💡 **Fallback**: You can try loading the file using the "Prolog: load document" command.'
      );
    }
  }

  // Handle help commands
  private async handleHelpCommand(
    predicate: string,
    stream: ChatResponseStream,
    __token: CancellationToken
  ) {
    if (!predicate.trim()) {
      stream.markdown(this.getHelpMessage());
      return;
    }

    stream.markdown(`📖 **Getting help for**: \`${predicate}\`\n\n`);

    try {
      if (!this.prologBackend?.isRunning()) {
        throw new Error('Backend not running');
      }

      const response = await this.prologBackend.sendRequest('help', {
        predicate: predicate,
        timeoutMs: 5000,
      });

      if (response.status === 'ok' && response.doc) {
        this.formatHelpResponse(response.doc, stream);
      } else {
        throw new Error(response.error || 'Help not found');
      }
    } catch (error) {
      const errorMsg = error instanceof Error ? error.message : String(error);
      stream.markdown(`❌ **Help lookup failed**: ${errorMsg}`);
      stream.markdown(
        `\n💡 **Fallback**: Try searching the SWI-Prolog documentation online for \`${predicate}\`.`
      );
    }
  }

  // Handle status command
  private async handleStatusCommand(stream: ChatResponseStream) {
    stream.markdown('📊 **Prolog Extension Status**\n\n');

    if (this.prologBackend) {
      const isRunning = this.prologBackend.isRunning();
      stream.markdown(`- **Backend**: ${isRunning ? '✅ Running' : '❌ Stopped'}\n`);

      if (isRunning) {
        try {
          const response = await this.prologBackend.sendRequest('version', { timeoutMs: 2000 });
          if (response.status === 'ok') {
            stream.markdown(`- **SWI-Prolog Version**: ${response.version}\n`);
          }
        } catch (error) {
          const errorMsg = error instanceof Error ? error.message : String(error);
          console.warn('[Extension] Version check failed:', errorMsg);
          stream.markdown(`- **Version Check**: ❌ Failed (${errorMsg})\n`);
        }
      }
    } else {
      stream.markdown('- **Backend**: ❌ Not initialized\n');
    }

    const config = workspace.getConfiguration('prolog');
    stream.markdown(`- **Executable Path**: \`${config.get('executablePath', 'swipl')}\`\n`);
    stream.markdown(`- **Dialect**: ${config.get('dialect', 'swi')}\n`);

    const stats = this.telemetry.getStats();
    if (stats) {
      stream.markdown(`- **Commands Used**: ${stats.totalCommands}\n`);
    }
  }

  // Handle N3 load commands
  private async handleN3LoadCommand(
    args: string,
    stream: ChatResponseStream,
    __token: CancellationToken
  ) {
    const parts = args.trim().split(/\s+/);

    if (parts.length === 0 || !parts[0]) {
      stream.markdown(
        '❌ **Error**: Please provide a file path or N3 content. Examples:\n- `/n3_load sample.n3`\n- `/n3_load --content "@prefix : <http://example.org/> . :socrates a :Person ."`'
      );
      return;
    }

    try {
      if (!this.prologBackend?.isRunning()) {
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

      const response = await this.prologBackend.sendRequest('n3_load', requestParams);

      if (response.status === 'ok') {
        stream.markdown(
          `✅ **N3 data loaded successfully**\n- **Triples loaded**: ${response.triples_count}\n`
        );
      } else {
        throw new Error(response.error || 'N3 load failed');
      }
    } catch (error) {
      const errorMsg = error instanceof Error ? error.message : String(error);
      stream.markdown(`❌ **N3 load failed**: ${errorMsg}`);
    }
  }

  // Handle N3 list commands with pagination
  private async handleN3ListCommand(
    args: string,
    stream: ChatResponseStream,
    __token: CancellationToken
  ) {
    try {
      if (!this.prologBackend?.isRunning()) {
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

      stream.markdown(
        `📋 **Listing N3 triples** (limit: ${limit}, offset: ${offset}, format: ${format})...\n\n`
      );
      stream.progress('🔄 Fetching triples...');

      const response = await this.prologBackend.sendRequest('n3_list', {
        limit,
        offset,
        format,
        streaming: true,
        timeoutMs: 15000,
      });

      if (response.status === 'ok') {
        // Handle paginated response
        if (response.pagination) {
          const { total_count, returned_count, has_more, next_offset } = response.pagination;
          stream.markdown(
            `✅ **Found ${total_count} triples** (showing ${returned_count}, offset: ${offset}):\n\n`
          );

          if (response.triples && response.triples.length > 0) {
            // Use compact format for large result sets
            if (returned_count > 20) {
              stream.markdown('```\n');
              response.triples.forEach((triple: any, index: number) => {
                stream.markdown(
                  `${offset + index + 1}. ${triple.subject} ${triple.predicate} ${triple.object}\n`
                );
              });
              stream.markdown('```\n');
            } else {
              stream.markdown(
                '| # | Subject | Predicate | Object |\n|---|---------|-----------|--------|\n'
              );
              response.triples.forEach((triple: any, index: number) => {
                stream.markdown(
                  `| ${offset + index + 1} | \`${triple.subject}\` | \`${triple.predicate}\` | \`${triple.object}\` |\n`
                );
              });
            }

            // Show pagination controls
            if (has_more) {
              stream.markdown(
                `\n📄 **Pagination**: Showing ${offset + 1}-${offset + returned_count} of ${total_count} triples`
              );
              stream.markdown(
                `\n*Use \`/n3_list --offset ${next_offset} --limit ${limit}\` for next page*`
              );
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
              stream.markdown(
                `| \`${triple.subject}\` | \`${triple.predicate}\` | \`${triple.object}\` |\n`
              );
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
  private async handleN3ReasonCommand(
    args: string,
    stream: ChatResponseStream,
    __token: CancellationToken
  ) {
    try {
      if (!this.prologBackend?.isRunning()) {
        throw new Error('Backend not running');
      }

      const goal = args.trim();

      if (goal) {
        stream.markdown(`🧠 **N3 reasoning with goal**: \`${goal}\`\n\n`);
      } else {
        stream.markdown(`🧠 **N3 reasoning** (finding all inferred triples)...\n\n`);
      }

      const response = await this.prologBackend.sendRequest('n3_reason', {
        goal: goal || '',
        timeoutMs: 15000,
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
              stream.markdown(
                `| \`${triple.subject}\` | \`${triple.predicate}\` | \`${triple.object}\` |\n`
              );
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
  private async handleN3ExplainCommand(
    args: string,
    stream: ChatResponseStream,
    __token: CancellationToken
  ) {
    const goal = args.trim();

    if (!goal) {
      stream.markdown(
        '❌ **Error**: Please provide a goal to explain. Example: `/n3_explain rdf(socrates, type, Mortal)`'
      );
      return;
    }

    try {
      if (!this.prologBackend?.isRunning()) {
        throw new Error('Backend not running');
      }

      stream.markdown(`🔍 **Explaining goal**: \`${goal}\`\n\n`);

      const response = await this.prologBackend.sendRequest('n3_explain', {
        goal,
        timeoutMs: 15000,
      });

      if (response.status === 'ok') {
        stream.markdown(`✅ **Proof explanation for**: \`${response.goal}\`\n\n`);
        this.formatProofTree(response.proof, stream, 0);
      } else {
        throw new Error(response.error || 'N3 explanation failed');
      }
    } catch (error) {
      const errorMsg = error instanceof Error ? error.message : String(error);
      stream.markdown(`❌ **N3 explanation failed**: ${errorMsg}`);
    }
  }

  // Format query results with streaming info
  private formatQueryResults(results: any[], stream: ChatResponseStream, streamingInfo?: any) {
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
      stream.markdown(
        `📊 **Showing ${displayCount} of ${totalCount} solution${totalCount > 1 ? 's' : ''}** with ${totalBindings} variable binding${totalBindings > 1 ? 's' : ''}:\n\n`
      );
    } else {
      stream.markdown(
        `📊 **Found ${displayCount} solution${displayCount > 1 ? 's' : ''}** with ${totalBindings} variable binding${totalBindings > 1 ? 's' : ''}:\n\n`
      );
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
  private formatHelpResponse(doc: any, stream: ChatResponseStream) {
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

  // Format proof tree for display
  private formatProofTree(proof: any, stream: ChatResponseStream, depth: number) {
    const indent = '  '.repeat(depth);

    if (proof.type === 'proof') {
      stream.markdown(`${indent}🎯 **Goal**: \`${proof.goal}\`\n`);
      if (proof.subproofs && proof.subproofs.length > 0) {
        proof.subproofs.forEach((subproof: any) => {
          this.formatProofTree(subproof, stream, depth + 1);
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
  private getHelpMessage(): string {
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
}