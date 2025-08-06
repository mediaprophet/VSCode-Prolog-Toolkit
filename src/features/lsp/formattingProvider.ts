import { TextDocument } from 'vscode-languageserver-textdocument';
import { TextEdit, Range } from 'vscode-languageserver/node';
import { FormattingProvider, LSPContext, PrologSettings } from './types';

export class PrologFormattingProvider implements FormattingProvider {
  async formatDocument(document: TextDocument, context: LSPContext): Promise<TextEdit[]> {
    const settings = await context.getDocumentSettings(document.uri);
    const text = document.getText();
    const lines = text.split('\n');
    const edits: TextEdit[] = [];

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const formatted = this.formatPrologLine(line, settings, i, lines);

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

  async formatRange(document: TextDocument, range: Range, context: LSPContext): Promise<TextEdit[]> {
    const settings = await context.getDocumentSettings(document.uri);
    const text = document.getText(range);
    const lines = text.split('\n');
    const edits: TextEdit[] = [];
    const documentLines = document.getText().split('\n');

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const documentLineIndex = range.start.line + i;
      const formatted = this.formatPrologLine(line, settings, documentLineIndex, documentLines);

      if (formatted !== line) {
        edits.push({
          range: {
            start: { line: range.start.line + i, character: 0 },
            end: { line: range.start.line + i, character: line.length },
          },
          newText: formatted,
        });
      }
    }

    return edits;
  }

  private formatPrologLine(line: string, settings: PrologSettings, lineIndex: number, allLines: string[]): string {
    let formatted = line;

    // Skip empty lines and preserve them as-is
    if (line.trim() === '') {
      return line;
    }

    // Skip comment-only lines but clean up spacing
    if (line.trim().startsWith('%')) {
      return this.formatComment(line);
    }

    // Add spaces after commas if enabled
    if (settings.format.addSpace) {
      formatted = this.addSpacesAfterCommas(formatted);
    }

    // Format operators
    formatted = this.formatOperators(formatted);

    // Handle indentation
    formatted = this.formatIndentation(formatted, lineIndex, allLines);

    // Clean up extra whitespace
    formatted = this.cleanupWhitespace(formatted);

    return formatted;
  }

  private formatComment(line: string): string {
    const match = line.match(/^(\s*)(%.*)$/);
    if (match) {
      const [, indentation, comment] = match;
      // Ensure single space after % if there's content
      const cleanComment = comment.replace(/^%\s*/, '% ').replace(/^% $/, '%');
      return indentation + cleanComment;
    }
    return line;
  }

  private addSpacesAfterCommas(text: string): string {
    // Add spaces after commas, but not inside strings or when already spaced
    return text.replace(/,(?!\s)(?![^"]*"(?:[^"]*"[^"]*")*[^"]*$)(?![^']*'(?:[^']*'[^']*')*[^']*$)/g, ', ');
  }

  private formatOperators(text: string): string {
    let formatted = text;

    // Format common Prolog operators with proper spacing
    const operators = [
      { pattern: /\s*:-\s*/g, replacement: ' :- ' },
      { pattern: /\s*->\s*/g, replacement: ' -> ' },
      { pattern: /\s*;\s*/g, replacement: ' ; ' },
      { pattern: /\s*\\\+\s*/g, replacement: ' \\+ ' },
      { pattern: /\s*==\s*/g, replacement: ' == ' },
      { pattern: /\s*\\==\s*/g, replacement: ' \\== ' },
      { pattern: /\s*=\s*/g, replacement: ' = ' },
      { pattern: /\s*\\=\s*/g, replacement: ' \\= ' },
      { pattern: /\s*@<\s*/g, replacement: ' @< ' },
      { pattern: /\s*@>\s*/g, replacement: ' @> ' },
      { pattern: /\s*@=<\s*/g, replacement: ' @=< ' },
      { pattern: /\s*@>=\s*/g, replacement: ' @>= ' },
      { pattern: /\s*=<\s*/g, replacement: ' =< ' },
      { pattern: /\s*>=\s*/g, replacement: ' >= ' },
      { pattern: /\s*<\s*/g, replacement: ' < ' },
      { pattern: /\s*>\s*/g, replacement: ' > ' },
      { pattern: /\s*is\s+/g, replacement: ' is ' },
    ];

    // Apply operator formatting, but avoid strings
    for (const { pattern, replacement } of operators) {
      // Simple approach: don't format inside strings
      if (!this.containsUnescapedQuotes(formatted)) {
        formatted = formatted.replace(pattern, replacement);
      } else {
        // More complex: preserve strings while formatting operators
        formatted = this.formatOperatorsPreservingStrings(formatted, pattern, replacement);
      }
    }

    return formatted;
  }

  private containsUnescapedQuotes(text: string): boolean {
    return /(?<!\\)["']/.test(text);
  }

  private formatOperatorsPreservingStrings(text: string, pattern: RegExp, replacement: string): string {
    // Split by strings and format only non-string parts
    const parts: Array<{text: string, isString: boolean}> = [];
    let current = '';
    let inString = false;
    let stringChar = '';

    for (let i = 0; i < text.length; i++) {
      const char = text[i];
      
      if (!inString && (char === '"' || char === "'")) {
        if (current) {
          parts.push({text: current, isString: false});
          current = '';
        }
        inString = true;
        stringChar = char;
        current = char;
      } else if (inString && char === stringChar && text[i - 1] !== '\\') {
        current += char;
        parts.push({text: current, isString: true});
        current = '';
        inString = false;
        stringChar = '';
      } else {
        current += char;
      }
    }

    if (current) {
      parts.push({text: current, isString: inString});
    }

    // Format only non-string parts
    return parts.map(part => 
      part.isString ? part.text : part.text.replace(pattern, replacement)
    ).join('');
  }

  private formatIndentation(text: string, lineIndex: number, allLines: string[]): string {
    const trimmed = text.trim();
    
    if (trimmed === '') {
      return '';
    }

    // Determine appropriate indentation
    let indentLevel = 0;
    const indentSize = 2; // spaces per indent level

    // Rules for indentation:
    // 1. Directives (:-) at start - no indentation
    // 2. Facts - no indentation  
    // 3. Rule heads - no indentation
    // 4. Rule bodies - indent
    // 5. Continuation lines - additional indent

    if (trimmed.startsWith(':-')) {
      // Directives - no indentation
      indentLevel = 0;
    } else if (this.isRuleHead(trimmed)) {
      // Rule head - no indentation
      indentLevel = 0;
    } else if (this.isRuleBody(trimmed, lineIndex, allLines)) {
      // Rule body - indent once
      indentLevel = 1;
    } else if (this.isContinuationLine(trimmed, lineIndex, allLines)) {
      // Continuation line - additional indent
      indentLevel = 2;
    } else {
      // Facts and other constructs - no indentation
      indentLevel = 0;
    }

    const indent = ' '.repeat(indentLevel * indentSize);
    return indent + trimmed;
  }

  private isRuleHead(line: string): boolean {
    // A rule head contains :- but doesn't start with it
    return line.includes(':-') && !line.startsWith(':-');
  }

  private isRuleBody(line: string, lineIndex: number, allLines: string[]): boolean {
    // Check if this line is part of a rule body
    // Look backwards for a rule head
    for (let i = lineIndex - 1; i >= 0; i--) {
      const prevLine = allLines[i].trim();
      if (prevLine === '' || prevLine.startsWith('%')) {
        continue;
      }
      
      if (this.isRuleHead(prevLine)) {
        return !line.includes(':-') && !line.startsWith(':-');
      }
      
      // If we hit another rule or directive, this isn't a rule body
      if (prevLine.includes(':-') || this.looksLikeFact(prevLine)) {
        break;
      }
    }
    
    return false;
  }

  private isContinuationLine(line: string, lineIndex: number, allLines: string[]): boolean {
    if (lineIndex === 0) return false;
    
    const prevLine = allLines[lineIndex - 1].trim();
    
    // If previous line doesn't end with a period, this might be a continuation
    return prevLine.length > 0 && 
           !prevLine.endsWith('.') && 
           !prevLine.startsWith('%') &&
           (line.startsWith(',') || line.startsWith(';') || line.startsWith('->'));
  }

  private looksLikeFact(line: string): boolean {
    // A fact is a line that ends with a period and doesn't contain :-
    return line.endsWith('.') && !line.includes(':-');
  }

  private cleanupWhitespace(text: string): string {
    // Remove trailing whitespace
    let cleaned = text.replace(/\s+$/, '');
    
    // Normalize multiple spaces to single spaces (except indentation)
    const match = cleaned.match(/^(\s*)(.*)/);
    if (match) {
      const [, indentation, content] = match;
      const normalizedContent = content.replace(/\s+/g, ' ');
      cleaned = indentation + normalizedContent;
    }
    
    return cleaned;
  }

  // Additional utility methods for specific formatting tasks

  public formatClause(clause: string, settings: PrologSettings): string {
    return this.formatPrologLine(clause, settings, 0, [clause]);
  }

  public formatTerm(term: string, settings: PrologSettings): string {
    let formatted = term;
    
    if (settings.format.addSpace) {
      formatted = this.addSpacesAfterCommas(formatted);
    }
    
    formatted = this.formatOperators(formatted);
    formatted = this.cleanupWhitespace(formatted);
    
    return formatted;
  }

  public formatGoal(goal: string, settings: PrologSettings): string {
    return this.formatTerm(goal, settings);
  }

  // Method to organize imports (used by code actions)
  public organizeImports(document: TextDocument): TextEdit[] {
    const text = document.getText();
    const lines = text.split('\n');
    const imports: Array<{line: string, index: number}> = [];
    const otherLines: Array<{line: string, index: number}> = [];
    
    // Separate imports from other content
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const trimmed = line.trim();
      
      if (trimmed.startsWith(':- use_module(') || trimmed.startsWith(':- include(')) {
        imports.push({line, index: i});
      } else {
        otherLines.push({line, index: i});
      }
    }

    if (imports.length === 0) {
      return []; // No imports to organize
    }

    // Sort imports alphabetically
    imports.sort((a, b) => a.line.localeCompare(b.line));

    // Create the new content structure
    const sortedImportLines = imports.map(imp => imp.line);
    const nonImportLines = otherLines.map(other => other.line);
    
    // Find the first non-empty, non-comment line after imports
    let insertionPoint = 0;
    for (const other of otherLines) {
      if (other.line.trim() && !other.line.trim().startsWith('%')) {
        insertionPoint = other.index;
        break;
      }
    }

    // Create edits to reorganize
    const edits: TextEdit[] = [];

    // Remove old import lines
    for (const imp of imports) {
      edits.push({
        range: {
          start: { line: imp.index, character: 0 },
          end: { line: imp.index + 1, character: 0 },
        },
        newText: '',
      });
    }

    // Insert sorted imports at the beginning
    const importText = sortedImportLines.join('\n') + '\n\n';
    edits.push({
      range: {
        start: { line: 0, character: 0 },
        end: { line: 0, character: 0 },
      },
      newText: importText,
    });

    return edits;
  }
}