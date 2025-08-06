import { TextDocument } from 'vscode-languageserver-textdocument';
import { FoldingRange, FoldingRangeKind } from 'vscode-languageserver/node';
import { FoldingProvider, LSPContext } from './types';

export class PrologFoldingProvider implements FoldingProvider {
  async provideFoldingRanges(document: TextDocument, context: LSPContext): Promise<FoldingRange[]> {
    const ranges: FoldingRange[] = [];
    const text = document.getText();
    const lines = text.split('\n');

    let commentStart = -1;
    let ruleStart = -1;
    let clauseGroupStart = -1;
    let currentPredicateName = '';

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const trimmed = line.trim();

      // Handle block comments /* ... */
      if (this.startsBlockComment(trimmed)) {
        commentStart = i;
      } else if (this.endsBlockComment(trimmed) && commentStart >= 0) {
        if (i > commentStart) {
          ranges.push({
            startLine: commentStart,
            endLine: i,
            kind: FoldingRangeKind.Comment,
          });
        }
        commentStart = -1;
      }

      // Handle line comment groups
      if (trimmed.startsWith('%') && trimmed.length > 1) {
        if (commentStart === -1) {
          commentStart = i;
        }
      } else if (commentStart >= 0 && !trimmed.startsWith('%')) {
        // End of comment group
        if (i - 1 > commentStart) {
          ranges.push({
            startLine: commentStart,
            endLine: i - 1,
            kind: FoldingRangeKind.Comment,
          });
        }
        commentStart = -1;
      }

      // Handle multi-line rules
      if (this.isRuleHead(trimmed)) {
        ruleStart = i;
      } else if (ruleStart >= 0 && this.endsRule(trimmed)) {
        if (i > ruleStart) {
          ranges.push({
            startLine: ruleStart,
            endLine: i,
          });
        }
        ruleStart = -1;
      }

      // Handle predicate clause groups (multiple clauses for same predicate)
      const predicateName = this.extractPredicateName(trimmed);
      if (predicateName) {
        if (currentPredicateName === predicateName) {
          // Same predicate, continue the group
          continue;
        } else {
          // Different predicate
          if (clauseGroupStart >= 0 && i - 1 > clauseGroupStart) {
            // End previous group if it spans multiple lines
            ranges.push({
              startLine: clauseGroupStart,
              endLine: i - 1,
            });
          }
          // Start new group
          clauseGroupStart = i;
          currentPredicateName = predicateName;
        }
      } else if (trimmed === '' || trimmed.startsWith('%')) {
        // Empty line or comment - continue current group
        continue;
      } else {
        // Non-predicate line - end current group
        if (clauseGroupStart >= 0 && i - 1 > clauseGroupStart) {
          ranges.push({
            startLine: clauseGroupStart,
            endLine: i - 1,
          });
        }
        clauseGroupStart = -1;
        currentPredicateName = '';
      }

      // Handle module declarations and use_module blocks
      if (this.isModuleDeclaration(trimmed)) {
        const moduleEnd = this.findModuleEnd(lines, i);
        if (moduleEnd > i) {
          ranges.push({
            startLine: i,
            endLine: moduleEnd,
          });
        }
      }

      // Handle DCG (Definite Clause Grammar) rules
      if (this.isDCGRule(trimmed)) {
        const dcgEnd = this.findDCGRuleEnd(lines, i);
        if (dcgEnd > i) {
          ranges.push({
            startLine: i,
            endLine: dcgEnd,
          });
        }
      }

      // Handle list constructs
      if (this.startsListConstruct(trimmed)) {
        const listEnd = this.findListConstructEnd(lines, i);
        if (listEnd > i) {
          ranges.push({
            startLine: i,
            endLine: listEnd,
          });
        }
      }
    }

    // Handle end-of-file cases
    if (commentStart >= 0 && lines.length - 1 > commentStart) {
      ranges.push({
        startLine: commentStart,
        endLine: lines.length - 1,
        kind: FoldingRangeKind.Comment,
      });
    }

    if (ruleStart >= 0 && lines.length - 1 > ruleStart) {
      ranges.push({
        startLine: ruleStart,
        endLine: lines.length - 1,
      });
    }

    if (clauseGroupStart >= 0 && lines.length - 1 > clauseGroupStart) {
      ranges.push({
        startLine: clauseGroupStart,
        endLine: lines.length - 1,
      });
    }

    // Remove overlapping ranges and sort
    return this.cleanupRanges(ranges);
  }

  private startsBlockComment(line: string): boolean {
    return line.includes('/*') && !line.includes('*/');
  }

  private endsBlockComment(line: string): boolean {
    return line.includes('*/');
  }

  private isRuleHead(line: string): boolean {
    // A rule head contains :- but doesn't start with it
    return line.includes(':-') && !line.startsWith(':-') && !line.endsWith('.');
  }

  private endsRule(line: string): boolean {
    return line.endsWith('.');
  }

  private extractPredicateName(line: string): string | null {
    const match = line.match(/^([a-z][a-zA-Z0-9_]*)\s*\(/);
    return match ? match[1] : null;
  }

  private isModuleDeclaration(line: string): boolean {
    return line.startsWith(':- module(') || line.startsWith(':-module(');
  }

  private findModuleEnd(lines: string[], start: number): number {
    // Find the end of module exports or the next non-directive line
    for (let i = start + 1; i < lines.length; i++) {
      const trimmed = lines[i].trim();
      if (trimmed === '' || trimmed.startsWith('%')) {
        continue;
      }
      if (!trimmed.startsWith(':-') && !trimmed.includes('export(')) {
        return i - 1;
      }
    }
    return start;
  }

  private isDCGRule(line: string): boolean {
    return line.includes('-->');
  }

  private findDCGRuleEnd(lines: string[], start: number): number {
    // DCG rules can span multiple lines until a period
    for (let i = start; i < lines.length; i++) {
      const trimmed = lines[i].trim();
      if (trimmed.endsWith('.')) {
        return i;
      }
    }
    return start;
  }

  private startsListConstruct(line: string): boolean {
    // Look for complex list constructs that might span multiple lines
    const openBrackets = (line.match(/\[/g) || []).length;
    const closeBrackets = (line.match(/\]/g) || []).length;
    return openBrackets > closeBrackets && openBrackets > 0;
  }

  private findListConstructEnd(lines: string[], start: number): number {
    let bracketCount = 0;
    
    // Count brackets from start line
    const startLine = lines[start];
    bracketCount += (startLine.match(/\[/g) || []).length;
    bracketCount -= (startLine.match(/\]/g) || []).length;

    if (bracketCount <= 0) {
      return start;
    }

    // Continue counting in subsequent lines
    for (let i = start + 1; i < lines.length; i++) {
      const line = lines[i];
      bracketCount += (line.match(/\[/g) || []).length;
      bracketCount -= (line.match(/\]/g) || []).length;
      
      if (bracketCount <= 0) {
        return i;
      }
    }

    return start;
  }

  private cleanupRanges(ranges: FoldingRange[]): FoldingRange[] {
    // Sort ranges by start line
    ranges.sort((a, b) => a.startLine - b.startLine);

    // Remove overlapping ranges, keeping the larger ones
    const cleaned: FoldingRange[] = [];
    
    for (const range of ranges) {
      // Check if this range overlaps with any existing range
      let shouldAdd = true;
      
      for (let i = 0; i < cleaned.length; i++) {
        const existing = cleaned[i];
        
        if (this.rangesOverlap(range, existing)) {
          // Keep the larger range
          if (this.getRangeSize(range) > this.getRangeSize(existing)) {
            cleaned.splice(i, 1);
            i--;
          } else {
            shouldAdd = false;
            break;
          }
        }
      }
      
      if (shouldAdd) {
        cleaned.push(range);
      }
    }

    return cleaned;
  }

  private rangesOverlap(range1: FoldingRange, range2: FoldingRange): boolean {
    return !(range1.endLine < range2.startLine || range2.endLine < range1.startLine);
  }

  private getRangeSize(range: FoldingRange): number {
    return range.endLine - range.startLine;
  }

  // Additional utility methods

  // Get foldable regions of a specific type
  public async getFoldableRegions(
    document: TextDocument,
    kind?: FoldingRangeKind,
    context?: LSPContext
  ): Promise<FoldingRange[]> {
    const allRanges = await this.provideFoldingRanges(document, context!);
    
    if (!kind) {
      return allRanges;
    }
    
    return allRanges.filter(range => range.kind === kind);
  }

  // Get comment blocks only
  public async getCommentBlocks(document: TextDocument, context: LSPContext): Promise<FoldingRange[]> {
    return this.getFoldableRegions(document, FoldingRangeKind.Comment, context);
  }

  // Get predicate clause groups
  public async getPredicateGroups(document: TextDocument, context: LSPContext): Promise<FoldingRange[]> {
    const allRanges = await this.provideFoldingRanges(document, context);
    return allRanges.filter(range => !range.kind); // Ranges without kind are typically code blocks
  }

  // Check if a line is within a foldable region
  public isLineInFoldableRegion(line: number, ranges: FoldingRange[]): boolean {
    return ranges.some(range => line >= range.startLine && line <= range.endLine);
  }
}