

export interface IStackFrame {
  id: number;
  level: number;
  name: string;
  file: string;
  line: number;
  column: number;
}

export class StackFrameManager {
  private _stackFrames: IStackFrame[] = [];

  public addStackFrame(frame: IStackFrame): void {
    try {
      this._stackFrames.unshift(frame);
    } catch (err) {
      console.error('[StackFrameManager] Error adding stack frame:', err, frame);
    }
  }

  public getStackFrames(): IStackFrame[] {
    return this._stackFrames;
  }

  public clearStackFrames(): void {
    this._stackFrames = [];
  }

  public getCurrentFrame(): IStackFrame | undefined {
    return this._stackFrames[0];
  }

  public highlightSource(frame: IStackFrame): { file: string; line: number; column: number } {
    return {
      file: frame.file,
      line: frame.line,
      column: frame.column,
    };
  }
}
