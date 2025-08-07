'use strict';

import { workspace } from 'vscode';

// Telemetry interface (privacy-respecting)
export interface TelemetryData {
  command: string;
  success: boolean;
  error?: string;
  timestamp: number;
}

// Simple telemetry collector (can be disabled via settings)
export class TelemetryCollector {
  private enabled: boolean = false;
  private data: TelemetryData[] = [];

  constructor() {
    const config = workspace.getConfiguration('prolog');
    this.enabled = config.get<boolean>('telemetry.enabled', false);
  }

  collect(data: TelemetryData) {
    if (this.enabled && this.data.length < 1000) {
      // Limit storage
      this.data.push(data);
    }
  }

  getStats() {
    if (!this.enabled) return null;
    const commands = this.data.reduce(
      (acc, item) => {
        acc[item.command] = (acc[item.command] || 0) + 1;
        return acc;
      },
      {} as Record<string, number>
    );
    return { totalCommands: this.data.length, commands };
  }
}
