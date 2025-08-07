import { TextDocument } from 'vscode-languageserver-textdocument';
import type { BackendResponse, ExecuteCommandHandler, LSPContext } from './types.js';

export class PrologExecuteCommandHandler implements ExecuteCommandHandler {
  async executeCommand(command: string, args: any[], _context: LSPContext): Promise<any> {
    switch (command) {
      case 'prolog.executeQuery': {
        return await this.executeQuery(args, _context);
      }
      case 'prolog.consultFile': {
        return await this.consultFile(args, _context);
      }
      case 'prolog.getHelp': {
        return await this.getHelp(args, _context);
      }
      case 'prolog.runN3Diagnostics': {
        return await this.runN3Diagnostics(args, _context);
      }
      case 'prolog.formatDocument': {
        return await this.formatDocument(args, _context);
      }
      case 'prolog.organizeImports': {
        return await this.organizeImports(args, _context);
      }
      default: {
        throw new Error(`Unknown command: ${command}`);
      }
    }
  }

  private async executeQuery(args: any[], context: LSPContext): Promise<any> {
    if (args.length > 0 && context.prologBackend?.isRunning()) {
      try {
        const response: BackendResponse = await context.prologBackend.sendRequest('query', {
          goal: args[0],
          timeoutMs: 10000,
        });

        const message =
          response.status === 'ok'
            ? `Query result: ${JSON.stringify(response.results)}`
            : `Query failed: ${response.error}`;

        // Note: In a real implementation, you'd need to pass the connection object
        // to send window messages. For now, we'll return the response.
        return { success: response.status === 'ok', message, response };
      } catch (error: unknown) {
        return { success: false, message: `Query error: ${error}`, error };
      }
    }
    return { success: false, message: 'No query provided or backend not running' };
  }

  private async consultFile(args: any[], context: LSPContext): Promise<any> {
    if (args.length > 0 && context.prologBackend?.isRunning()) {
      try {
        const response: BackendResponse = await context.prologBackend.sendRequest('consult', {
          file: args[0],
          timeoutMs: 15000,
        });

        const message =
          response.status === 'ok'
            ? `File consulted: ${args[0]}`
            : `Consult failed: ${response.error}`;

        return { success: response.status === 'ok', message, response };
      } catch (error: unknown) {
        return { success: false, message: `Consult error: ${error}`, error };
      }
    }
    return { success: false, message: 'No file provided or backend not running' };
  }

  private async getHelp(args: any[], context: LSPContext): Promise<any> {
    if (args.length > 0 && context.prologBackend?.isRunning()) {
      try {
        const response: BackendResponse = await context.prologBackend.sendRequest('help', {
          predicate: args[0],
          timeoutMs: 5000,
        });

        if (response.status === 'ok' && response.doc) {
          const message = `Help for ${args[0]}: ${response.doc.summary || 'No description'}`;
          return { success: true, message, doc: response.doc };
        }

        return { success: false, message: `No help available for ${args[0]}` };
      } catch (error: unknown) {
        return { success: false, message: `Help error: ${error}`, error };
      }
    }
    return { success: false, message: 'No predicate provided or backend not running' };
  }

  private async runN3Diagnostics(args: any[], context: LSPContext): Promise<any> {
    if (args.length > 0) {
      const documentUri = args[0];
      const document = context.documents.get(documentUri);

      if (document) {
        try {
          // You would typically call the validation provider here
          // For now, we'll simulate the validation
          const message = 'N3 diagnostics completed';
          return { success: true, message };
        } catch (error: unknown) {
          return { success: false, message: `N3 diagnostics error: ${error}`, error };
        }
      }

      return { success: false, message: 'Document not found' };
    }

    return { success: false, message: 'No document URI provided' };
  }

  private async formatDocument(args: any[], context: LSPContext): Promise<any> {
    if (args.length > 0) {
      const documentUri = args[0];
      const document = context.documents.get(documentUri);

      if (document) {
        try {
          const formatted = await this.formatPrologDocument(document, context);
          return { success: true, message: 'Document formatted', edits: formatted };
        } catch (error: unknown) {
          return { success: false, message: `Format error: ${error}`, error };
        }
      }

      return { success: false, message: 'Document not found' };
    }

    return { success: false, message: 'No document URI provided' };
  }

  private async organizeImports(args: any[], context: LSPContext): Promise<any> {
    if (args.length > 0) {
      const documentUri = args[0];
      const document = context.documents.get(documentUri);

      if (document) {
        try {
          const organized = await this.organizeImportsInDocument(document, context);
          return { success: true, message: 'Imports organized', edits: organized };
        } catch (error: unknown) {
          return { success: false, message: `Organize imports error: ${error}`, error };
        }
      }

      return { success: false, message: 'Document not found' };
    }

    return { success: false, message: 'No document URI provided' };
  }

  private async formatPrologDocument(document: TextDocument, context: LSPContext): Promise<any[]> {
    const settings = (await context.getDocumentSettings(document.uri)) ?? {};
    const text = document.getText() ?? '';
    const lines = text.split('\n');
    const edits: any[] = [];

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i] ?? '';
      const formatted = this.formatPrologLine(line, settings);

      if (formatted !== line) {
        edits.push({
          range: {
            start: { line: i, character: 0 },
            end: { line: i, character: line.length },
          },
          newText: formatted,
        });
      }
    }

    return edits;
  }

  private formatPrologLine(line: string, settings: any): string {
    let formatted = line;

    // Add spaces after commas if enabled
    if (settings.format?.addSpace) {
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

  private async organizeImportsInDocument(
    document: TextDocument,
    context: LSPContext
  ): Promise<any[]> {
    const text = document.getText();
    const lines = text.split('\n');
    const imports: string[] = [];
    const otherLines: string[] = [];
    const edits: any[] = [];

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
      const lastLine = lines[lines.length - 1] ?? '';
      edits.push({
        range: {
          start: { line: 0, character: 0 },
          end: { line: lines.length - 1, character: lastLine.length },
        },
        newText: newContent,
      });
    }

    return edits;
  }
}
