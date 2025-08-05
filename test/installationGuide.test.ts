import * as assert from 'assert';
import * as vscode from 'vscode';
import { InstallationGuide } from '../src/features/installationGuide';

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

  suite('Platform Detection', () => {
    test('should detect current platform correctly', () => {
      const platform = installationGuide.getCurrentPlatform();
      assert.ok(['windows', 'macos', 'linux'].includes(platform));
    });

    test('should provide platform-specific installation instructions', () => {
      const platform = installationGuide.getCurrentPlatform();
      const instructions = installationGuide.getPlatformSpecificInstructions(platform);
      
      assert.ok(instructions.title);
      assert.ok(instructions.steps);
      assert.ok(Array.isArray(instructions.steps));
      assert.ok(instructions.steps.length > 0);
      assert.ok(instructions.downloadUrl);
      
      // Check platform-specific content
      if (platform === 'windows') {
        assert.ok(instructions.title.toLowerCase().includes('windows'));
        assert.ok(instructions.downloadUrl.includes('windows') || instructions.downloadUrl.includes('.exe'));
      } else if (platform === 'macos') {
        assert.ok(instructions.title.toLowerCase().includes('macos') || instructions.title.toLowerCase().includes('mac'));
        assert.ok(instructions.downloadUrl.includes('macos') || instructions.downloadUrl.includes('darwin'));
      } else {
        assert.ok(instructions.title.toLowerCase().includes('linux'));
        assert.ok(instructions.downloadUrl.includes('linux') || instructions.steps.some(step => 
          step.toLowerCase().includes('apt') || step.toLowerCase().includes('yum') || step.toLowerCase().includes('pacman')
        ));
      }
    });
  });

  suite('HTML Generation', () => {
    test('should generate valid HTML for installation guide', () => {
      const html = installationGuide.generateInstallationGuideHtml();
      
      assert.ok(typeof html === 'string');
      assert.ok(html.length > 0);
      
      // Check for essential HTML structure
      assert.ok(html.includes('<!DOCTYPE html>'));
      assert.ok(html.includes('<html'));
      assert.ok(html.includes('<head>'));
      assert.ok(html.includes('<body>'));
      assert.ok(html.includes('</html>'));
      
      // Check for installation-specific content
      assert.ok(html.includes('SWI-Prolog'));
      assert.ok(html.includes('installation') || html.includes('Installation'));
      
      // Check for platform detection
      const platform = installationGuide.getCurrentPlatform();
      if (platform === 'windows') {
        assert.ok(html.includes('Windows') || html.includes('windows'));
      } else if (platform === 'macos') {
        assert.ok(html.includes('macOS') || html.includes('Mac'));
      } else {
        assert.ok(html.includes('Linux') || html.includes('linux'));
      }
    });

    test('should generate valid HTML for setup wizard', () => {
      const html = installationGuide.generateSetupWizardHtml();
      
      assert.ok(typeof html === 'string');
      assert.ok(html.length > 0);
      
      // Check for essential HTML structure
      assert.ok(html.includes('<!DOCTYPE html>'));
      assert.ok(html.includes('<html'));
      assert.ok(html.includes('<head>'));
      assert.ok(html.includes('<body>'));
      assert.ok(html.includes('</html>'));
      
      // Check for wizard-specific content
      assert.ok(html.includes('Setup Wizard') || html.includes('setup wizard'));
      assert.ok(html.includes('button') || html.includes('input'));
      
      // Check for JavaScript functionality
      assert.ok(html.includes('<script>'));
      assert.ok(html.includes('vscode.postMessage') || html.includes('acquireVsCodeApi'));
    });

    test('should generate valid HTML for success dialog', () => {
      const html = installationGuide.generateSuccessDialogHtml('9.0.4', '/usr/local/bin/swipl');
      
      assert.ok(typeof html === 'string');
      assert.ok(html.length > 0);
      
      // Check for success-specific content
      assert.ok(html.includes('success') || html.includes('Success'));
      assert.ok(html.includes('9.0.4'));
      assert.ok(html.includes('/usr/local/bin/swipl'));
      
      // Check for essential HTML structure
      assert.ok(html.includes('<!DOCTYPE html>'));
      assert.ok(html.includes('</html>'));
    });
  });

  suite('CSS Generation', () => {
    test('should generate valid CSS styles', () => {
      const css = installationGuide.generateStyles();
      
      assert.ok(typeof css === 'string');
      assert.ok(css.length > 0);
      
      // Check for CSS syntax
      assert.ok(css.includes('{') && css.includes('}'));
      assert.ok(css.includes(':') && css.includes(';'));
      
      // Check for common CSS properties
      assert.ok(css.includes('color') || css.includes('background') || css.includes('font'));
    });
  });

  suite('JavaScript Generation', () => {
    test('should generate valid JavaScript for installation guide', () => {
      const js = installationGuide.generateInstallationGuideScript();
      
      assert.ok(typeof js === 'string');
      assert.ok(js.length > 0);
      
      // Check for VS Code API usage
      assert.ok(js.includes('acquireVsCodeApi') || js.includes('vscode'));
      
      // Check for event handling
      assert.ok(js.includes('addEventListener') || js.includes('onclick'));
    });

    test('should generate valid JavaScript for setup wizard', () => {
      const js = installationGuide.generateSetupWizardScript();
      
      assert.ok(typeof js === 'string');
      assert.ok(js.length > 0);
      
      // Check for VS Code API usage
      assert.ok(js.includes('acquireVsCodeApi') || js.includes('vscode'));
      
      // Check for wizard-specific functionality
      assert.ok(js.includes('postMessage') || js.includes('message'));
    });
  });

  suite('Error Handling', () => {
    test('should handle invalid platform gracefully', () => {
      const instructions = installationGuide.getPlatformSpecificInstructions('invalid-platform' as any);
      
      // Should fallback to a default or handle gracefully
      assert.ok(instructions.title);
      assert.ok(instructions.steps);
      assert.ok(Array.isArray(instructions.steps));
    });

    test('should handle missing parameters in HTML generation', () => {
      const html = installationGuide.generateSuccessDialogHtml('', '');
      
      assert.ok(typeof html === 'string');
      assert.ok(html.length > 0);
      
      // Should still generate valid HTML even with empty parameters
      assert.ok(html.includes('<!DOCTYPE html>'));
      assert.ok(html.includes('</html>'));
    });
  });

  suite('Integration Tests', () => {
    test('should work with VS Code webview system', () => {
      // Test that the HTML can be used in a webview
      const html = installationGuide.generateInstallationGuideHtml();
      
      // Check for webview-compatible content
      assert.ok(!html.includes('file://'));
      assert.ok(!html.includes('http://') || html.includes('https://'));
      
      // Check for VS Code API integration
      assert.ok(html.includes('acquireVsCodeApi') || html.includes('vscode-webview'));
    });

    test('should generate content suitable for different VS Code themes', () => {
      const html = installationGuide.generateInstallationGuideHtml();
      const css = installationGuide.generateStyles();
      
      // Should use VS Code CSS variables or be theme-neutral
      assert.ok(css.includes('var(--vscode-') || 
               css.includes('#ffffff') || 
               css.includes('#000000') ||
               css.includes('inherit'));
    });
  });

  suite('Content Validation', () => {
    test('should provide comprehensive installation steps', () => {
      const platforms = ['windows', 'macos', 'linux'] as const;
      
      platforms.forEach(platform => {
        const instructions = installationGuide.getPlatformSpecificInstructions(platform);
        
        // Each platform should have multiple steps
        assert.ok(instructions.steps.length >= 2, `${platform} should have at least 2 installation steps`);
        
        // Steps should be non-empty strings
        instructions.steps.forEach((step, index) => {
          assert.ok(typeof step === 'string', `Step ${index} for ${platform} should be a string`);
          assert.ok(step.trim().length > 0, `Step ${index} for ${platform} should not be empty`);
        });
        
        // Should have a valid download URL
        assert.ok(instructions.downloadUrl.startsWith('http'), `${platform} should have a valid download URL`);
      });
    });

    test('should include package manager instructions for Linux', () => {
      const instructions = installationGuide.getPlatformSpecificInstructions('linux');
      
      const stepsText = instructions.steps.join(' ').toLowerCase();
      
      // Should mention at least one package manager
      assert.ok(
        stepsText.includes('apt') || 
        stepsText.includes('yum') || 
        stepsText.includes('dnf') || 
        stepsText.includes('pacman') || 
        stepsText.includes('zypper'),
        'Linux instructions should mention package managers'
      );
    });

    test('should include Homebrew instructions for macOS', () => {
      const instructions = installationGuide.getPlatformSpecificInstructions('macos');
      
      const stepsText = instructions.steps.join(' ').toLowerCase();
      
      // Should mention Homebrew or MacPorts
      assert.ok(
        stepsText.includes('homebrew') || 
        stepsText.includes('brew') || 
        stepsText.includes('macports') ||
        stepsText.includes('installer'),
        'macOS instructions should mention Homebrew or installer'
      );
    });
  });

  suite('Accessibility', () => {
    test('should generate accessible HTML', () => {
      const html = installationGuide.generateInstallationGuideHtml();
      
      // Check for accessibility features
      assert.ok(html.includes('alt=') || html.includes('aria-') || html.includes('role='));
      
      // Should have proper heading structure
      assert.ok(html.includes('<h1') || html.includes('<h2'));
      
      // Should have semantic HTML
      assert.ok(html.includes('<main') || html.includes('<section') || html.includes('<article'));
    });

    test('should have keyboard navigation support', () => {
      const js = installationGuide.generateSetupWizardScript();
      
      // Should handle keyboard events
      assert.ok(js.includes('keydown') || js.includes('keypress') || js.includes('tabindex'));
    });
  });

  suite('Performance', () => {
    test('should generate HTML quickly', () => {
      const startTime = Date.now();
      
      for (let i = 0; i < 10; i++) {
        installationGuide.generateInstallationGuideHtml();
      }
      
      const endTime = Date.now();
      const duration = endTime - startTime;
      
      assert.ok(duration < 1000, `HTML generation took ${duration}ms for 10 iterations, should be under 1000ms`);
    });

    test('should generate reasonable-sized HTML', () => {
      const html = installationGuide.generateInstallationGuideHtml();
      
      // Should not be too large (under 100KB)
      assert.ok(html.length < 100000, `HTML size is ${html.length} characters, should be under 100000`);
      
      // Should not be too small (over 1KB)
      assert.ok(html.length > 1000, `HTML size is ${html.length} characters, should be over 1000`);
    });
  });
});