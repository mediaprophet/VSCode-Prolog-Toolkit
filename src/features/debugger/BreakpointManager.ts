
import { DebugProtocol } from '@vscode/debugprotocol';
import * as fs from 'fs';

export interface IBreakPoint {
  sourceFile: string;
  line: number;
  column?: number;
  condition?: string;
  hitCondition?: string;
  id?: number | undefined;
}

interface ISourceLineLocations {
  [sourceFile: string]: number[];
}

export class BreakpointManager {
  private _breakpoints: IBreakPoint[] = [];
  private _functionBreakpoints: string[] = [];
  private _sourceLineLocations: ISourceLineLocations = {};

  public setBreakpoints(source: string, breakpoints: DebugProtocol.Breakpoint[]): void {
    this._breakpoints = breakpoints
      .filter(bp => typeof bp.line === 'number')
      .map(bp => ({
        sourceFile: source,
        line: bp.line as number,
        column: (bp as any).column,
        condition: (bp as any).condition,
        hitCondition: (bp as any).hitCondition,
        id: bp.id,
      }));
  }

  public getBreakpoints(): IBreakPoint[] {
    return this._breakpoints;
  }

  public clearBreakpoints(): void {
    this._breakpoints = [];
  }

  public setFunctionBreakpoints(predicates: string[]): void {
    this._functionBreakpoints = predicates;
  }

  public getFunctionBreakpoints(): string[] {
    return this._functionBreakpoints;
  }

  public clearFunctionBreakpoints(): void {
    this._functionBreakpoints = [];
  }

  // Persistence (could be extended to use workspace storage)
  public saveBreakpoints(filePath: string): void {
    try {
      fs.writeFileSync(filePath, JSON.stringify(this._breakpoints, null, 2));
      console.log(`[BreakpointManager] Breakpoints saved to ${filePath}`);
    } catch (err) {
      console.error(`[BreakpointManager] Error saving breakpoints to ${filePath}:`, err);
    }
  }

  public loadBreakpoints(filePath: string): void {
    try {
      if (fs.existsSync(filePath)) {
        const data = fs.readFileSync(filePath, 'utf-8');
        this._breakpoints = JSON.parse(data);
        console.log(`[BreakpointManager] Breakpoints loaded from ${filePath}`);
      }
    } catch (err) {
      console.error(`[BreakpointManager] Error loading breakpoints from ${filePath}:`, err);
    }
  }

  // Source mapping utilities
  private getSourceLineLocations(source: string): void {
    if (this._sourceLineLocations[source]) {
      return;
    }
    try {
      const lines = fs.readFileSync(source).toString().split('\n');
      const lengths = lines.map(line => line.length + 1);
      lengths.unshift(0);
      for (let i = 1; i < lengths.length; i++) {
        if (typeof lengths[i] === 'number' && typeof lengths[i - 1] === 'number') {
          lengths[i] = (lengths[i] ?? 0) + (lengths[i - 1] ?? 0);
        }
      }
      this._sourceLineLocations[source] = lengths;
    } catch (err) {
      console.error(`[BreakpointManager] Error reading source file for line locations: ${source}`, err);
    }
  }

  public fromStartCharToLineChar(source: string, startChar: number): { file: string; line: number; startChar: number } {
    this.getSourceLineLocations(source);
    let i = 0;
    const locations = this._sourceLineLocations[source];
    if (!locations) {
      return { file: source, line: 1, startChar };
    }
    for (; Array.isArray(locations ?? []) && typeof (locations?.[i]) === 'number' && locations?.[i] !== undefined && (locations?.[i] as number) < startChar; i++);
    return {
      file: source,
      line: i + 1,
      startChar: startChar - (locations[i] ?? 0),
    };
  }
}
