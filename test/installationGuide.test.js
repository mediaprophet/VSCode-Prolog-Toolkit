"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var assert_1 = require("assert");
var installationGuide_js_1 = require("../src/features/installationGuide.js");
suite('InstallationGuide Tests', function () {
    var installationGuide;
    setup(function () {
        installationGuide = installationGuide_js_1.InstallationGuide.getInstance();
    });
    suite('Singleton Pattern', function () {
        test('should return the same instance', function () {
            var instance1 = installationGuide_js_1.InstallationGuide.getInstance();
            var instance2 = installationGuide_js_1.InstallationGuide.getInstance();
            assert_1.default.strictEqual(instance1, instance2);
        });
    });
    suite('Public API', function () {
        test('should have showInstallationGuideDialog and runSetupWizard methods', function () {
            assert_1.default.strictEqual(typeof installationGuide.showInstallationGuideDialog, 'function');
            assert_1.default.strictEqual(typeof installationGuide.runSetupWizard, 'function');
        });
    });
});
