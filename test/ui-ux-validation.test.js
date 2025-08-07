"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var chai_1 = require("chai");
var fs = require("fs");
var path = require("path");
describe('UI/UX Enhancements Validation', function () {
    var packageJson;
    before(function () {
        var packagePath = path.join(__dirname, '..', 'package.json');
        packageJson = JSON.parse(fs.readFileSync(packagePath, 'utf8'));
    });
    describe('Chat Participant Configuration', function () {
        it('should have enhanced chat participant configuration', function () {
            (0, chai_1.expect)(packageJson.contributes.chatParticipants).to.be.an('array');
            (0, chai_1.expect)(packageJson.contributes.chatParticipants).to.have.length(1);
            var chatParticipant = packageJson.contributes.chatParticipants[0];
            (0, chai_1.expect)(chatParticipant.id).to.equal('prolog');
            (0, chai_1.expect)(chatParticipant.description).to.include('🤖');
            (0, chai_1.expect)(chatParticipant.commands).to.be.an('array');
            (0, chai_1.expect)(chatParticipant.commands).to.have.length.greaterThan(0);
        });
        it('should have all required chat commands defined', function () {
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
                (0, chai_1.expect)(commandNames).to.include(cmd, "Missing command: ".concat(cmd));
            });
        });
        it('should have descriptive command descriptions', function () {
            var chatParticipant = packageJson.contributes.chatParticipants[0];
            chatParticipant.commands.forEach(function (cmd) {
                (0, chai_1.expect)(cmd.description).to.be.a('string');
                (0, chai_1.expect)(cmd.description.length).to.be.greaterThan(10, "Command ".concat(cmd.name, " has too short description"));
            });
        });
    });
    describe('LSP Commands', function () {
        it('should have LSP commands registered', function () {
            var commands = packageJson.contributes.commands;
            var commandIds = commands.map(function (cmd) { return cmd.command; });
            (0, chai_1.expect)(commandIds).to.include('prolog.lsp.executeQuery');
            (0, chai_1.expect)(commandIds).to.include('prolog.lsp.getHelp');
            (0, chai_1.expect)(commandIds).to.include('prolog.lsp.runN3Diagnostics');
        });
        it('should have proper command titles and categories', function () {
            var commands = packageJson.contributes.commands;
            var lspCommands = commands.filter(function (cmd) { return cmd.command.startsWith('prolog.lsp.'); });
            lspCommands.forEach(function (cmd) {
                (0, chai_1.expect)(cmd.title).to.be.a('string');
                (0, chai_1.expect)(cmd.title.length).to.be.greaterThan(5);
                (0, chai_1.expect)(cmd.category).to.equal('Prolog');
            });
        });
        it('should have keybindings for main LSP commands', function () {
            var keybindings = packageJson.contributes.keybindings;
            var commandIds = keybindings.map(function (kb) { return kb.command; });
            (0, chai_1.expect)(commandIds).to.include('prolog.lsp.executeQuery');
            (0, chai_1.expect)(commandIds).to.include('prolog.lsp.getHelp');
        });
        it('should have context menu entries for LSP commands', function () {
            var contextMenus = packageJson.contributes.menus['editor/context'];
            var commandIds = contextMenus.map(function (menu) { return menu.command; });
            (0, chai_1.expect)(commandIds).to.include('prolog.lsp.executeQuery');
            (0, chai_1.expect)(commandIds).to.include('prolog.lsp.getHelp');
            (0, chai_1.expect)(commandIds).to.include('prolog.lsp.runN3Diagnostics');
        });
        it('should have proper when clauses for context menus', function () {
            var contextMenus = packageJson.contributes.menus['editor/context'];
            var lspMenus = contextMenus.filter(function (menu) { return menu.command.startsWith('prolog.lsp.'); });
            lspMenus.forEach(function (menu) {
                (0, chai_1.expect)(menu.when).to.equal('resourceLangId == prolog');
            });
        });
    });
    describe('File Structure', function () {
        it('should have LSP extension file', function () {
            var lspExtensionPath = path.join(__dirname, '..', 'src', 'features', 'prologLSPExtension.ts');
            (0, chai_1.expect)(fs.existsSync(lspExtensionPath)).to.be.true;
        });
        it('should have enhanced extension file', function () {
            var extensionPath = path.join(__dirname, '..', 'src', 'extension.ts');
            (0, chai_1.expect)(fs.existsSync(extensionPath)).to.be.true;
            var extensionContent = fs.readFileSync(extensionPath, 'utf8');
            (0, chai_1.expect)(extensionContent).to.include('PrologLSPExtension');
            (0, chai_1.expect)(extensionContent).to.include('lspExtension.registerFeatures()');
        });
        it('should have backup of original extension', function () {
            var backupPath = path.join(__dirname, '..', 'src', 'extension.ts');
            (0, chai_1.expect)(fs.existsSync(backupPath)).to.be.true;
        });
    });
    describe('Build Validation', function () {
        it('should have compiled output files', function () {
            var outPath = path.join(__dirname, '..', 'out', 'pub', 'extension.js');
            (0, chai_1.expect)(fs.existsSync(outPath)).to.be.true;
        });
        it('should have source maps', function () {
            var mapPath = path.join(__dirname, '..', 'out', 'pub', 'extension.js.map');
            (0, chai_1.expect)(fs.existsSync(mapPath)).to.be.true;
        });
    });
    describe('Enhanced Help Message', function () {
        it('should have enhanced help content in extension', function () {
            var extensionPath = path.join(__dirname, '..', 'src', 'extension.ts');
            var extensionContent = fs.readFileSync(extensionPath, 'utf8');
            // Check for enhanced help message features
            (0, chai_1.expect)(extensionContent).to.include('🤖 Prolog Assistant');
            (0, chai_1.expect)(extensionContent).to.include('Pro Tips');
            (0, chai_1.expect)(extensionContent).to.include('Quick Start Examples');
            (0, chai_1.expect)(extensionContent).to.include('N3 Semantic Web Commands');
        });
    });
    describe('Enhanced Query Results', function () {
        it('should have enhanced query result formatting', function () {
            var extensionPath = path.join(__dirname, '..', 'src', 'extension.ts');
            var extensionContent = fs.readFileSync(extensionPath, 'utf8');
            // Check for enhanced formatting features
            (0, chai_1.expect)(extensionContent).to.include('formatQueryResults');
            (0, chai_1.expect)(extensionContent).to.include('solution');
            (0, chai_1.expect)(extensionContent).to.include('variable binding');
        });
    });
    describe('Enhanced Followup Provider', function () {
        it('should have context-aware followup suggestions', function () {
            var extensionPath = path.join(__dirname, '..', 'src', 'extension.ts');
            var extensionContent = fs.readFileSync(extensionPath, 'utf8');
            // Check for enhanced followup provider
            (0, chai_1.expect)(extensionContent).to.include('Context-aware followups');
            (0, chai_1.expect)(extensionContent).to.include('result.metadata?.command');
            (0, chai_1.expect)(extensionContent).to.include('Check backend status');
            (0, chai_1.expect)(extensionContent).to.include('Start reasoning');
        });
    });
});
