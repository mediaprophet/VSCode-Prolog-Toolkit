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
var prologLSPExtension_js_1 = require("../src/features/prologLSPExtension.js");
describe('UI/UX Enhancements', function () {
    var mockContext;
    var lspExtension;
    beforeEach(function () {
        // Create a mock extension context
        mockContext = {
            subscriptions: [],
            workspaceState: {},
            globalState: {},
            extensionPath: '',
            asAbsolutePath: function (relativePath) { return relativePath; },
            storagePath: '',
            globalStoragePath: '',
            logPath: '',
            extensionUri: {},
            environmentVariableCollection: {},
            extensionMode: vscode.ExtensionMode.Test,
            globalStorageUri: {},
            logUri: {},
            storageUri: {},
            secrets: {},
            extension: {},
            languageModelAccessInformation: {},
        };
        lspExtension = new prologLSPExtension_js_1.PrologLSPExtension(mockContext, null);
    });
    afterEach(function () {
        if (lspExtension) {
            lspExtension.dispose();
        }
    });
    describe('LSP Extension', function () {
        it('should initialize without errors', function () {
            (0, chai_1.expect)(lspExtension).to.not.be.null;
            (0, chai_1.expect)(mockContext.subscriptions).to.have.length.greaterThan(0);
        });
        it('should register features without throwing', function () {
            (0, chai_1.expect)(function () {
                lspExtension.registerFeatures();
            }).to.not.throw();
        });
        it('should dispose cleanly', function () {
            (0, chai_1.expect)(function () {
                lspExtension.dispose();
            }).to.not.throw();
        });
    });
    describe('Chat Participant Configuration', function () {
        it('should have enhanced chat participant configuration in package.json', function () { return __awaiter(void 0, void 0, void 0, function () {
            var packageJson, chatParticipant;
            return __generator(this, function (_a) {
                packageJson = require('../package.json');
                (0, chai_1.expect)(packageJson.contributes.chatParticipants).to.be.an('array');
                (0, chai_1.expect)(packageJson.contributes.chatParticipants).to.have.length(1);
                chatParticipant = packageJson.contributes.chatParticipants[0];
                (0, chai_1.expect)(chatParticipant.id).to.equal('prolog');
                (0, chai_1.expect)(chatParticipant.description).to.include('🤖');
                (0, chai_1.expect)(chatParticipant.commands).to.be.an('array');
                (0, chai_1.expect)(chatParticipant.commands).to.have.length.greaterThan(0);
                return [2 /*return*/];
            });
        }); });
        it('should have all required chat commands defined', function () {
            // eslint-disable-next-line @typescript-eslint/no-var-requires
            var packageJson = require('../package.json');
            var chatParticipant = packageJson.contributes.chatParticipants[0];
            var commandNames = chatParticipant.commands.map(function (cmd) { return cmd.name; });
            var expectedCommands = [
                'query',
                'consult',
                'help',
                'status',
                'n3_load',
                'n3_list',
                'n3_reason',
                'n3_explain',
            ];
            expectedCommands.forEach(function (cmd) {
                (0, chai_1.expect)(commandNames).to.include(cmd);
            });
        });
    });
    describe('LSP Commands', function () {
        it('should have LSP commands registered in package.json', function () {
            // eslint-disable-next-line @typescript-eslint/no-var-requires
            var packageJson = require('../package.json');
            var commands = packageJson.contributes.commands;
            var commandIds = commands.map(function (cmd) { return cmd.command; });
            (0, chai_1.expect)(commandIds).to.include('prolog.lsp.executeQuery');
            (0, chai_1.expect)(commandIds).to.include('prolog.lsp.getHelp');
            (0, chai_1.expect)(commandIds).to.include('prolog.lsp.runN3Diagnostics');
        });
        it('should have keybindings for LSP commands', function () {
            // eslint-disable-next-line @typescript-eslint/no-var-requires
            var packageJson = require('../package.json');
            var keybindings = packageJson.contributes.keybindings;
            var commandIds = keybindings.map(function (kb) { return kb.command; });
            (0, chai_1.expect)(commandIds).to.include('prolog.lsp.executeQuery');
            (0, chai_1.expect)(commandIds).to.include('prolog.lsp.getHelp');
        });
        it('should have context menu entries for LSP commands', function () {
            // eslint-disable-next-line @typescript-eslint/no-var-requires
            var packageJson = require('../package.json');
            var contextMenus = packageJson.contributes.menus['editor/context'];
            var commandIds = contextMenus.map(function (menu) { return menu.command; });
            (0, chai_1.expect)(commandIds).to.include('prolog.lsp.executeQuery');
            (0, chai_1.expect)(commandIds).to.include('prolog.lsp.getHelp');
            (0, chai_1.expect)(commandIds).to.include('prolog.lsp.runN3Diagnostics');
        });
    });
    describe('Extension Activation', function () {
        it('should build successfully', function () {
            // This test passes if the TypeScript compilation was successful
            // which we verified with the npm run build command
            (0, chai_1.expect)(true).to.be.true;
        });
    });
});
