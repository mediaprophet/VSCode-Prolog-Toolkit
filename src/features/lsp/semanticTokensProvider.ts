import { TextDocument } from 'vscode-languageserver-textdocument';
import { SemanticTokens, SemanticTokensBuilder } from 'vscode-languageserver/node';
import { SemanticTokensProvider, LSPContext, semanticTokensLegend } from './types';

export class PrologSemanticTokensProvider implements SemanticTokensProvider {
  async provideSemanticTokens(document: TextDocument, context: LSPContext): Promise<SemanticTokens> {
    const builder = new SemanticTokensBuilder();
    const text = document.getText();
    const lines = text.split('\n');

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      this.tokenizePrologLine(line, i, builder);
    }

    return builder.build();
  }

  private tokenizePrologLine(line: string, lineNumber: number, builder: SemanticTokensBuilder) {
    // Comments
    const commentMatch = line.match(/%.*$/);
    if (commentMatch) {
      const start = line.indexOf(commentMatch[0]);
      builder.push(lineNumber, start, commentMatch[0].length, this.getTokenType('comment'), 0);
    }

    // Block comments
    const blockCommentStart = line.indexOf('/*');
    const blockCommentEnd = line.indexOf('*/');
    if (blockCommentStart !== -1) {
      const length = blockCommentEnd !== -1 ? blockCommentEnd - blockCommentStart + 2 : line.length - blockCommentStart;
      builder.push(lineNumber, blockCommentStart, length, this.getTokenType('comment'), 0);
    }

    // Strings (single and double quoted)
    const stringRegex = /'([^'\\]|\\.)*'|"([^"\\]|\\.)*"/g;
    let stringMatch;
    while ((stringMatch = stringRegex.exec(line)) !== null) {
      builder.push(lineNumber, stringMatch.index, stringMatch[0].length, this.getTokenType('string'), 0);
    }

    // Numbers (integers and floats)
    const numberRegex = /\b\d+(\.\d+)?\b/g;
    let numberMatch;
    while ((numberMatch = numberRegex.exec(line)) !== null) {
      builder.push(lineNumber, numberMatch.index, numberMatch[0].length, this.getTokenType('number'), 0);
    }

    // Variables (starting with uppercase or underscore)
    const variableRegex = /\b[A-Z_][a-zA-Z0-9_]*\b/g;
    let variableMatch;
    while ((variableMatch = variableRegex.exec(line)) !== null) {
      // Skip if it's inside a string or comment
      if (!this.isInStringOrComment(line, variableMatch.index)) {
        const modifiers = this.getVariableModifiers(variableMatch[0]);
        builder.push(lineNumber, variableMatch.index, variableMatch[0].length, this.getTokenType('variable'), modifiers);
      }
    }

    // Predicates (atoms followed by parentheses)
    const predicateRegex = /\b[a-z][a-zA-Z0-9_]*(?=\s*\()/g;
    let predicateMatch;
    while ((predicateMatch = predicateRegex.exec(line)) !== null) {
      if (!this.isInStringOrComment(line, predicateMatch.index)) {
        const modifiers = this.getPredicateModifiers(predicateMatch[0], line, predicateMatch.index);
        builder.push(lineNumber, predicateMatch.index, predicateMatch[0].length, this.getTokenType('function'), modifiers);
      }
    }

    // Atoms (quoted atoms)
    const quotedAtomRegex = /'[a-zA-Z0-9_][a-zA-Z0-9_]*'/g;
    let atomMatch;
    while ((atomMatch = quotedAtomRegex.exec(line)) !== null) {
      builder.push(lineNumber, atomMatch.index, atomMatch[0].length, this.getTokenType('property'), 0);
    }

    // Operators
    const operatorRegex = /:-|-->|->|;|,|\+|-|\*|\/|=|\\=|==|\\==|@<|@>|@=<|@>=|=<|>=|<|>|\\\+|is\b|\^/g;
    let operatorMatch;
    while ((operatorMatch = operatorRegex.exec(line)) !== null) {
      if (!this.isInStringOrComment(line, operatorMatch.index)) {
        builder.push(lineNumber, operatorMatch.index, operatorMatch[0].length, this.getTokenType('operator'), 0);
      }
    }

    // Keywords and built-in predicates
    const keywordRegex = /\b(true|fail|cut|halt|abort|trace|notrace|spy|nospy|if|then|else|when|once|ignore|forall)\b/g;
    let keywordMatch;
    while ((keywordMatch = keywordRegex.exec(line)) !== null) {
      if (!this.isInStringOrComment(line, keywordMatch.index)) {
        builder.push(lineNumber, keywordMatch.index, keywordMatch[0].length, this.getTokenType('keyword'), 0);
      }
    }

    // Module-related constructs
    const moduleRegex = /\b(module|use_module|export|public|dynamic|multifile|discontiguous|meta_predicate)\b/g;
    let moduleMatch;
    while ((moduleMatch = moduleRegex.exec(line)) !== null) {
      if (!this.isInStringOrComment(line, moduleMatch.index)) {
        builder.push(lineNumber, moduleMatch.index, moduleMatch[0].length, this.getTokenType('namespace'), 0);
      }
    }

    // Type testing predicates
    const typeRegex = /\b(var|nonvar|atom|number|integer|float|compound|atomic|callable|ground|cyclic_term|acyclic_term)\b/g;
    let typeMatch;
    while ((typeMatch = typeRegex.exec(line)) !== null) {
      if (!this.isInStringOrComment(line, typeMatch.index)) {
        builder.push(lineNumber, typeMatch.index, typeMatch[0].length, this.getTokenType('type'), 0);
      }
    }

    // DCG-specific tokens
    if (line.includes('-->')) {
      const dcgTokens = this.tokenizeDCGRule(line, lineNumber);
      dcgTokens.forEach(token => {
        builder.push(lineNumber, token.start, token.length, token.tokenType, token.modifiers);
      });
    }

    // List constructs
    const listRegex = /\[|\]/g;
    let listMatch;
    while ((listMatch = listRegex.exec(line)) !== null) {
      if (!this.isInStringOrComment(line, listMatch.index)) {
        builder.push(lineNumber, listMatch.index, listMatch[0].length, this.getTokenType('operator'), 0);
      }
    }

    // Functor separators and parentheses
    const functorRegex = /[(){}]/g;
    let functorMatch;
    while ((functorMatch = functorRegex.exec(line)) !== null) {
      if (!this.isInStringOrComment(line, functorMatch.index)) {
        builder.push(lineNumber, functorMatch.index, functorMatch[0].length, this.getTokenType('operator'), 0);
      }
    }
  }

  private getTokenType(type: string): number {
    const index = semanticTokensLegend.tokenTypes.indexOf(type);
    return index >= 0 ? index : 0;
  }

  private getTokenModifier(modifier: string): number {
    return Math.pow(2, semanticTokensLegend.tokenModifiers.indexOf(modifier));
  }

  private getVariableModifiers(variable: string): number {
    let modifiers = 0;
    
    // Anonymous variables (starting with _)
    if (variable.startsWith('_')) {
      // Don't add special modifiers for anonymous variables
    }
    
    // Singleton variables (just _)
    if (variable === '_') {
      modifiers |= this.getTokenModifier('readonly');
    }

    return modifiers;
  }

  private getPredicateModifiers(predicate: string, line: string, index: number): number {
    let modifiers = 0;
    
    // Check if it's a predicate definition (at start of line or after :-)
    const beforePredicate = line.substring(0, index).trim();
    if (beforePredicate === '' || beforePredicate === ':-') {
      modifiers |= this.getTokenModifier('definition');
    }

    // Check if it's a built-in predicate
    if (this.isBuiltinPredicate(predicate)) {
      modifiers |= this.getTokenModifier('defaultLibrary');
    }

    return modifiers;
  }

  private isBuiltinPredicate(predicate: string): boolean {
    const builtins = [
      'member', 'append', 'length', 'reverse', 'sort', 'msort', 'keysort',
      'findall', 'bagof', 'setof', 'forall', 'aggregate', 'aggregate_all',
      'assert', 'asserta', 'assertz', 'retract', 'retractall', 'abolish',
      'write', 'writeln', 'writeq', 'write_term', 'format',
      'read', 'read_term', 'get', 'get_char', 'peek_char', 'put', 'put_char',
      'open', 'close', 'flush_output', 'stream_property',
      'functor', 'arg', 'univ', 'copy_term', 'numbervars', 'term_variables',
      'call', 'apply', 'maplist', 'include', 'exclude', 'partition',
      'between', 'succ', 'plus', 'abs', 'max', 'min', 'gcd',
      'atom_chars', 'atom_codes', 'atom_string', 'string_chars',
      'phrase', 'phrase_from_chars'
    ];

    return builtins.includes(predicate);
  }

  private isInStringOrComment(line: string, index: number): boolean {
    // Check if the position is inside a string
    let inString = false;
    let stringChar = '';
    let escaped = false;

    for (let i = 0; i < index && i < line.length; i++) {
      const char = line[i];
      
      if (escaped) {
        escaped = false;
        continue;
      }

      if (char === '\\') {
        escaped = true;
        continue;
      }

      if (!inString && (char === '"' || char === "'")) {
        inString = true;
        stringChar = char;
      } else if (inString && char === stringChar) {
        inString = false;
        stringChar = '';
      }
    }

    if (inString) {
      return true;
    }

    // Check if the position is inside a comment
    const commentStart = line.indexOf('%');
    if (commentStart !== -1 && index >= commentStart) {
      return true;
    }

    const blockCommentStart = line.indexOf('/*');
    const blockCommentEnd = line.indexOf('*/');
    if (blockCommentStart !== -1 && index >= blockCommentStart) {
      if (blockCommentEnd === -1 || index <= blockCommentEnd) {
        return true;
      }
    }

    return false;
  }

  private tokenizeDCGRule(line: string, lineNumber: number): Array<{start: number, length: number, tokenType: number, modifiers: number}> {
    const tokens: Array<{start: number, length: number, tokenType: number, modifiers: number}> = [];
    
    // Tokenize DCG-specific constructs
    const dcgArrowIndex = line.indexOf('-->');
    if (dcgArrowIndex !== -1) {
      tokens.push({
        start: dcgArrowIndex,
        length: 3,
        tokenType: this.getTokenType('operator'),
        modifiers: 0
      });
    }

    // Terminal symbols in DCG rules (usually in square brackets)
    const terminalRegex = /\[[^\]]*\]/g;
    let terminalMatch;
    while ((terminalMatch = terminalRegex.exec(line)) !== null) {
      tokens.push({
        start: terminalMatch.index,
        length: terminalMatch[0].length,
        tokenType: this.getTokenType('string'),
        modifiers: 0
      });
    }

    return tokens;
  }

  // Utility method to provide semantic tokens for a specific range
  public async provideSemanticTokensRange(
    document: TextDocument,
    startLine: number,
    endLine: number,
    context: LSPContext
  ): Promise<SemanticTokens> {
    const builder = new SemanticTokensBuilder();
    const lines = document.getText().split('\n');

    const actualEndLine = Math.min(endLine, lines.length - 1);
    
    for (let i = startLine; i <= actualEndLine; i++) {
      const line = lines[i];
      this.tokenizePrologLine(line, i, builder);
    }

    return builder.build();
  }

  // Method to get token information at a specific position
  public getTokenAtPosition(document: TextDocument, line: number, character: number): {type: string, modifiers: string[]} | null {
    const lines = document.getText().split('\n');
    if (line >= lines.length) {
      return null;
    }

    const lineText = lines[line];
    if (character >= lineText.length) {
      return null;
    }

    // This would require implementing token parsing at specific position
    // For now, return null as it would need the full semantic analysis
    return null;
  }

  // Method to validate semantic token legend consistency
  public validateLegend(): boolean {
    const requiredTypes = ['comment', 'string', 'number', 'variable', 'function', 'operator', 'keyword', 'type', 'namespace', 'property'];
    const requiredModifiers = ['definition', 'readonly', 'defaultLibrary'];
    
    const hasAllTypes = requiredTypes.every(type => semanticTokensLegend.tokenTypes.includes(type));
    const hasAllModifiers = requiredModifiers.every(mod => semanticTokensLegend.tokenModifiers.includes(mod));
    
    return hasAllTypes && hasAllModifiers;
  }
}