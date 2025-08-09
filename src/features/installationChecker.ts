// Minimal singleton stub for InstallationChecker
export class InstallationChecker {
  private static _instance: InstallationChecker;

  private constructor() { }

  static getInstance(): InstallationChecker {
    if (!InstallationChecker._instance) {
      InstallationChecker._instance = new InstallationChecker();
    }
    return InstallationChecker._instance;
  }

  async checkSwiplInstallation() {
    // Stub: return a default status
    return { isInstalled: false, issues: ['Not implemented'] };
  }

  async findSwiplExecutable() {
    // Stub: return undefined
    return undefined;
  }

  async getSwiplVersion(_path?: string) {
    // Stub: return a fake version
    return '0.0.0';
  }

  async validateSwiplPathDetailed(_path: string) {
    // Stub: return a default result
    return { found: false, path: _path, permissions: 'unknown', issues: ['Not implemented'] };
  }
}
