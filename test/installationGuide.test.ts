import assert from 'assert';
import { InstallationGuide } from '../src/features/installationGuide.js';

suite('InstallationGuide Tests', () => {
  let installationGuide: InstallationGuide;

  setup(() => {
    installationGuide = InstallationGuide.getInstance();
  });

  suite('Singleton Pattern', () => {
    test('should return the same instance', () => {
      const instance1 = InstallationGuide.getInstance();
      const instance2 = InstallationGuide.getInstance();
      assert.strictEqual(instance1, instance2);
    });
  });

  suite('Public API', () => {
    test('should have showInstallationGuideDialog and runSetupWizard methods', () => {
      assert.strictEqual(typeof installationGuide.showInstallationGuideDialog, 'function');
      assert.strictEqual(typeof installationGuide.runSetupWizard, 'function');
    });
  });
});
