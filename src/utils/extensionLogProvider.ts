import * as fs from 'fs';
import * as path from 'path';

export class ExtensionLogProvider {
  private static logFilePath = path.join(process.cwd(), 'logs', 'extension.log');

  static log(message: string) {
    const timestamp = new Date().toISOString();
    const entry = `[${timestamp}] ${message}\n`;
    fs.appendFileSync(this.logFilePath, entry, { encoding: 'utf8' });
  }

  static getLogContents(): string {
    if (fs.existsSync(this.logFilePath)) {
      return fs.readFileSync(this.logFilePath, 'utf8');
    }
    return '';
  }

  static clearLog() {
    fs.writeFileSync(this.logFilePath, '', { encoding: 'utf8' });
  }
}
