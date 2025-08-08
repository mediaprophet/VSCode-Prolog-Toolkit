
export type OutputType = 'stdout' | 'stderr' | 'debug' | 'user';

export class OutputFilter {
  // Predefined regex patterns to filter off specific types of output
  private static readonly filterPatterns: RegExp[] = [
    /^$/, // Empty line
    /^TermToBeEvaluated/,
    /^EvalTermAtom/,
    /^EvalVarNames/,
    /^E =/,
    /^true\./,
  ];

  public static shouldFilter(data: string): boolean {
    const filtered = OutputFilter.filterPatterns.some(reg => reg.test(data));
    if (filtered) {
      console.log('[OutputFilter] Filtered output:', data);
    }
    return filtered;
  }

  public static routeOutput(data: string, type: OutputType, logFn: (msg: string, type: OutputType) => void): void {
    if (!OutputFilter.shouldFilter(data)) {
      try {
        logFn(data, type);
        console.log(`[OutputFilter] Routed output [${type}]:`, data);
      } catch (err) {
        console.error(`[OutputFilter] Error routing output [${type}]:`, err, data);
      }
    } else {
      // Optionally warn on unexpected output types
      if (!['stdout', 'stderr', 'debug', 'user'].includes(type)) {
        console.warn(`[OutputFilter] Unknown output type: ${type}`, data);
      }
    }
  }
}
