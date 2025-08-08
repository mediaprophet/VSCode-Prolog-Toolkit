// Position and document helpers extracted from utils.ts
import { Position } from 'vscode';

export class PositionUtils {
  static findLineColForByte(doc: string, index: number): Position {
    const lines = doc.split('\n');
    let totalLength = 0;
    let lineStartPos = 0;
    for (let lineNo = 0; lineNo < lines.length; lineNo++) {
      totalLength += lines[lineNo].length + 1;
      if (index < totalLength) {
        const colNo = index - lineStartPos;
        return new Position(lineNo, colNo);
      }
      lineStartPos = totalLength;
    }
    return new Position(0, 0);
  }
}
