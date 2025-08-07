"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g = Object.create((typeof Iterator === "function" ? Iterator : Object).prototype);
    return g.next = verb(0), g["throw"] = verb(1), g["return"] = verb(2), typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (g && (g = 0, op[0] && (_ = 0)), _) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
Object.defineProperty(exports, "__esModule", { value: true });
var chai_1 = require("chai");
var vscode = require("vscode");
var settingsWebviewProvider_js_1 = require("../src/features/settingsWebviewProvider.js");
describe('Settings UI Tests', function () {
    var settingsProvider;
    var mockExtensionUri;
    before(function () {
        mockExtensionUri = vscode.Uri.file('/mock/extension/path');
        settingsProvider = new settingsWebviewProvider_js_1.SettingsWebviewProvider(mockExtensionUri);
    });
    describe('SettingsWebviewProvider', function () {
        it('should create settings provider instance', function () {
            (0, chai_1.expect)(settingsProvider).to.be.instanceOf(settingsWebviewProvider_js_1.SettingsWebviewProvider);
        });
        it('should have correct viewType', function () {
            (0, chai_1.expect)(settingsWebviewProvider_js_1.SettingsWebviewProvider.viewType).to.equal('prologSettings');
        });
        it('should handle webview resolution', function () {
            // Mock webview view
            var mockWebviewView = {
                webview: {
                    options: {},
                    html: '',
                    onDidReceiveMessage: function () { return ({ dispose: function () { } }); },
                    postMessage: function () { return Promise.resolve(true); },
                    asWebviewUri: function (uri) { return uri; },
                },
            };
            // This should not throw
            (0, chai_1.expect)(function () {
                settingsProvider.resolveWebviewView(mockWebviewView, {}, {});
            }).to.not.throw();
        });
    });
    describe('Settings Configuration', function () {
        it('should have all required configuration properties', function () {
            var config = vscode.workspace.getConfiguration('prolog');
            // Test core settings
            (0, chai_1.expect)(config.has('executablePath')).to.be.true;
            (0, chai_1.expect)(config.has('dialect')).to.be.true;
            // Test linter settings
            (0, chai_1.expect)(config.has('linter.run')).to.be.true;
            (0, chai_1.expect)(config.has('linter.delay')).to.be.true;
            (0, chai_1.expect)(config.has('linter.enableMsgInOutput')).to.be.true;
            // Test formatter settings
            (0, chai_1.expect)(config.has('format.addSpace')).to.be.true;
            // Test API server settings
            (0, chai_1.expect)(config.has('apiServer.enabled')).to.be.true;
            (0, chai_1.expect)(config.has('apiServer.port')).to.be.true;
            (0, chai_1.expect)(config.has('apiServer.host')).to.be.true;
            // Test WebSocket server settings
            (0, chai_1.expect)(config.has('webSocketServer.enabled')).to.be.true;
            (0, chai_1.expect)(config.has('webSocketServer.port')).to.be.true;
            // Test telemetry settings
            (0, chai_1.expect)(config.has('telemetry.enabled')).to.be.true;
        });
        it('should have correct default values', function () {
            var config = vscode.workspace.getConfiguration('prolog');
            (0, chai_1.expect)(config.get('executablePath')).to.equal('/usr/bin/swipl');
            (0, chai_1.expect)(config.get('dialect')).to.equal('swi');
            (0, chai_1.expect)(config.get('linter.run')).to.equal('onType');
            (0, chai_1.expect)(config.get('linter.delay')).to.equal(500);
            (0, chai_1.expect)(config.get('format.addSpace')).to.equal(true);
            (0, chai_1.expect)(config.get('apiServer.enabled')).to.equal(false);
            (0, chai_1.expect)(config.get('apiServer.port')).to.equal(8080);
            (0, chai_1.expect)(config.get('webSocketServer.enabled')).to.equal(true);
            (0, chai_1.expect)(config.get('webSocketServer.port')).to.equal(8081);
            (0, chai_1.expect)(config.get('telemetry.enabled')).to.equal(false);
        });
    });
    describe('Settings Validation', function () {
        it('should validate port numbers correctly', function () {
            // Valid ports
            (0, chai_1.expect)(isValidPort(8080)).to.be.true;
            (0, chai_1.expect)(isValidPort(1024)).to.be.true;
            (0, chai_1.expect)(isValidPort(65535)).to.be.true;
            // Invalid ports
            (0, chai_1.expect)(isValidPort(1023)).to.be.false;
            (0, chai_1.expect)(isValidPort(65536)).to.be.false;
            (0, chai_1.expect)(isValidPort(-1)).to.be.false;
        });
        it('should validate positive numbers correctly', function () {
            (0, chai_1.expect)(isPositiveNumber(1)).to.be.true;
            (0, chai_1.expect)(isPositiveNumber(500)).to.be.true;
            (0, chai_1.expect)(isPositiveNumber(0)).to.be.false;
            (0, chai_1.expect)(isPositiveNumber(-1)).to.be.false;
        });
        it('should validate JSON arrays correctly', function () {
            (0, chai_1.expect)(isValidJSON('[]')).to.be.true;
            (0, chai_1.expect)(isValidJSON('["item1", "item2"]')).to.be.true;
            (0, chai_1.expect)(isValidJSON('{"key": "value"}')).to.be.true;
            (0, chai_1.expect)(isValidJSON('invalid json')).to.be.false;
            (0, chai_1.expect)(isValidJSON('[invalid')).to.be.false;
        });
    });
    describe('Settings Commands', function () {
        it('should register prolog.openSettings command', function () { return __awaiter(void 0, void 0, void 0, function () {
            var commands;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, vscode.commands.getCommands()];
                    case 1:
                        commands = _a.sent();
                        (0, chai_1.expect)(commands).to.include('prolog.openSettings');
                        return [2 /*return*/];
                }
            });
        }); });
    });
});
// Helper validation functions (these would normally be in the main code)
function isValidPort(port) {
    return port >= 1024 && port <= 65535;
}
function isPositiveNumber(value) {
    return value > 0;
}
function isValidJSON(value) {
    try {
        JSON.parse(value);
        return true;
    }
    catch (e) {
        return false;
    }
}
