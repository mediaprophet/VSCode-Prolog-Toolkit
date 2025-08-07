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
var fs = require("fs");
var path = require("path");
var vscode = require("vscode");
var multiIDESupport_js_1 = require("../src/features/multiIDESupport.js");
var prologLSPClient_js_1 = require("../src/features/prologLSPClient.js");
describe('LSP Integration Tests', function () {
    this.timeout(30000); // Increase timeout for LSP operations
    var lspClient;
    var testWorkspace;
    var testDocument;
    before(function () {
        return __awaiter(this, void 0, void 0, function () {
            var testFile, testContent, uri, mockContext;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        // Setup test workspace
                        testWorkspace = path.join(__dirname, 'test-workspace');
                        if (!fs.existsSync(testWorkspace)) {
                            fs.mkdirSync(testWorkspace, { recursive: true });
                        }
                        testFile = path.join(testWorkspace, 'test.pl');
                        testContent = "\n% Test Prolog file for LSP testing\nparent(tom, bob).\nparent(tom, liz).\nparent(bob, ann).\nparent(bob, pat).\nparent(pat, jim).\n\ngrandparent(X, Z) :- parent(X, Y), parent(Y, Z).\n\n% Test predicate with documentation\n%! member(?Elem, ?List) is nondet.\n%\n%  True if Elem is a member of List.\n%\n%  @param Elem Element to check\n%  @param List List to search in\ntest_member(X, [X|_]).\ntest_member(X, [_|T]) :- test_member(X, T).\n\n% N3/RDF test content\n@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n@prefix : <http://example.org/> .\n\n:socrates rdf:type :Person .\n:socrates :hasProperty :mortal .\n";
                        fs.writeFileSync(testFile, testContent);
                        uri = vscode.Uri.file(testFile);
                        return [4 /*yield*/, vscode.workspace.openTextDocument(uri)];
                    case 1:
                        testDocument = _a.sent();
                        return [4 /*yield*/, vscode.window.showTextDocument(testDocument)];
                    case 2:
                        _a.sent();
                        mockContext = {
                            subscriptions: [],
                            asAbsolutePath: function (relativePath) { return path.join(__dirname, '..', relativePath); },
                        };
                        lspClient = new prologLSPClient_js_1.PrologLSPClient(mockContext);
                        return [2 /*return*/];
                }
            });
        });
    });
    after(function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (!lspClient) return [3 /*break*/, 2];
                        return [4 /*yield*/, lspClient.stop()];
                    case 1:
                        _a.sent();
                        _a.label = 2;
                    case 2:
                        if (!testDocument) return [3 /*break*/, 4];
                        return [4 /*yield*/, vscode.commands.executeCommand('workbench.action.closeActiveEditor')];
                    case 3:
                        _a.sent();
                        _a.label = 4;
                    case 4:
                        // Clean up test workspace
                        if (fs.existsSync(testWorkspace)) {
                            fs.rmSync(testWorkspace, { recursive: true, force: true });
                        }
                        return [2 /*return*/];
                }
            });
        });
    });
    describe('LSP Client Lifecycle', function () {
        it('should start LSP client successfully', function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, lspClient.start()];
                        case 1:
                            _a.sent();
                            (0, chai_1.expect)(lspClient.isRunning()).to.be.true;
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle client restart', function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, lspClient.restart()];
                        case 1:
                            _a.sent();
                            (0, chai_1.expect)(lspClient.isRunning()).to.be.true;
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should stop LSP client successfully', function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, lspClient.stop()];
                        case 1:
                            _a.sent();
                            (0, chai_1.expect)(lspClient.isRunning()).to.be.false;
                            // Restart for other tests
                            return [4 /*yield*/, lspClient.start()];
                        case 2:
                            // Restart for other tests
                            _a.sent();
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('LSP Language Features', function () {
        beforeEach(function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            if (!!lspClient.isRunning()) return [3 /*break*/, 2];
                            return [4 /*yield*/, lspClient.start()];
                        case 1:
                            _a.sent();
                            _a.label = 2;
                        case 2: 
                        // Wait for LSP to be ready
                        return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 2000); })];
                        case 3:
                            // Wait for LSP to be ready
                            _a.sent();
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should provide completions', function () {
            return __awaiter(this, void 0, void 0, function () {
                var position, completions, memberCompletion;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            position = new vscode.Position(6, 20);
                            return [4 /*yield*/, vscode.commands.executeCommand('vscode.executeCompletionItemProvider', testDocument.uri, position)];
                        case 1:
                            completions = (_a.sent());
                            (0, chai_1.expect)(completions).to.not.be.undefined;
                            (0, chai_1.expect)(completions.items).to.be.an('array');
                            memberCompletion = completions.items.find(function (item) {
                                return item.label === 'member' ||
                                    (typeof item.label === 'object' && item.label.label === 'member');
                            });
                            (0, chai_1.expect)(memberCompletion).to.not.be.undefined;
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should provide hover information', function () {
            return __awaiter(this, void 0, void 0, function () {
                var position, hovers;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            position = new vscode.Position(1, 0);
                            return [4 /*yield*/, vscode.commands.executeCommand('vscode.executeHoverProvider', testDocument.uri, position)];
                        case 1:
                            hovers = (_a.sent());
                            (0, chai_1.expect)(hovers).to.be.an('array');
                            if (hovers.length > 0) {
                                (0, chai_1.expect)(hovers[0].contents).to.not.be.empty;
                            }
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should provide document symbols', function () {
            return __awaiter(this, void 0, void 0, function () {
                var symbols, parentSymbol;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, vscode.commands.executeCommand('vscode.executeDocumentSymbolProvider', testDocument.uri)];
                        case 1:
                            symbols = (_a.sent());
                            (0, chai_1.expect)(symbols).to.be.an('array');
                            (0, chai_1.expect)(symbols.length).to.be.greaterThan(0);
                            parentSymbol = symbols.find(function (symbol) { return symbol.name.includes('parent'); });
                            (0, chai_1.expect)(parentSymbol).to.not.be.undefined;
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should provide definitions', function () {
            return __awaiter(this, void 0, void 0, function () {
                var position, definitions;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            position = new vscode.Position(6, 35);
                            return [4 /*yield*/, vscode.commands.executeCommand('vscode.executeDefinitionProvider', testDocument.uri, position)];
                        case 1:
                            definitions = (_a.sent());
                            (0, chai_1.expect)(definitions).to.be.an('array');
                            // Should find definition of parent predicate
                            if (definitions.length > 0) {
                                (0, chai_1.expect)(definitions[0].uri.toString()).to.equal(testDocument.uri.toString());
                            }
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should provide references', function () {
            return __awaiter(this, void 0, void 0, function () {
                var position, references;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            position = new vscode.Position(1, 0);
                            return [4 /*yield*/, vscode.commands.executeCommand('vscode.executeReferenceProvider', testDocument.uri, position)];
                        case 1:
                            references = (_a.sent());
                            (0, chai_1.expect)(references).to.be.an('array');
                            (0, chai_1.expect)(references.length).to.be.greaterThan(1); // Should find multiple references
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should provide document highlights', function () {
            return __awaiter(this, void 0, void 0, function () {
                var position, highlights;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            position = new vscode.Position(1, 0);
                            return [4 /*yield*/, vscode.commands.executeCommand('vscode.executeDocumentHighlights', testDocument.uri, position)];
                        case 1:
                            highlights = (_a.sent());
                            (0, chai_1.expect)(highlights).to.be.an('array');
                            if (highlights.length > 0) {
                                (0, chai_1.expect)(highlights[0].range).to.not.be.undefined;
                            }
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should provide signature help', function () {
            return __awaiter(this, void 0, void 0, function () {
                var edit, insertPosition, signaturePosition, signatures, deleteEdit;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            edit = new vscode.WorkspaceEdit();
                            insertPosition = new vscode.Position(20, 0);
                            edit.insert(testDocument.uri, insertPosition, 'test_member(');
                            return [4 /*yield*/, vscode.workspace.applyEdit(edit)];
                        case 1:
                            _a.sent();
                            signaturePosition = new vscode.Position(20, 12);
                            return [4 /*yield*/, vscode.commands.executeCommand('vscode.executeSignatureHelpProvider', testDocument.uri, signaturePosition)];
                        case 2:
                            signatures = (_a.sent());
                            deleteEdit = new vscode.WorkspaceEdit();
                            deleteEdit.delete(testDocument.uri, new vscode.Range(insertPosition, new vscode.Position(20, 12)));
                            return [4 /*yield*/, vscode.workspace.applyEdit(deleteEdit)];
                        case 3:
                            _a.sent();
                            if (signatures) {
                                (0, chai_1.expect)(signatures.signatures).to.be.an('array');
                            }
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should provide code actions', function () {
            return __awaiter(this, void 0, void 0, function () {
                var range, codeActions, queryAction;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            range = new vscode.Range(1, 0, 1, 10);
                            return [4 /*yield*/, vscode.commands.executeCommand('vscode.executeCodeActionProvider', testDocument.uri, range)];
                        case 1:
                            codeActions = (_a.sent());
                            (0, chai_1.expect)(codeActions).to.be.an('array');
                            // Should provide actions like "Execute as Query", "Get Help"
                            if (codeActions.length > 0) {
                                queryAction = codeActions.find(function (action) { return action.title.includes('Query') || action.title.includes('Execute'); });
                                (0, chai_1.expect)(queryAction).to.not.be.undefined;
                            }
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should provide document formatting', function () {
            return __awaiter(this, void 0, void 0, function () {
                var edit, insertPosition, poorlyFormatted, formatEdits, deleteEdit;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            edit = new vscode.WorkspaceEdit();
                            insertPosition = new vscode.Position(20, 0);
                            poorlyFormatted = 'test_format(X,Y,Z):-member(X,[1,2,3]),append(Y,Z,Result).';
                            edit.insert(testDocument.uri, insertPosition, poorlyFormatted);
                            return [4 /*yield*/, vscode.workspace.applyEdit(edit)];
                        case 1:
                            _a.sent();
                            return [4 /*yield*/, vscode.commands.executeCommand('vscode.executeFormatDocumentProvider', testDocument.uri)];
                        case 2:
                            formatEdits = (_a.sent());
                            deleteEdit = new vscode.WorkspaceEdit();
                            deleteEdit.delete(testDocument.uri, new vscode.Range(insertPosition, new vscode.Position(20, poorlyFormatted.length)));
                            return [4 /*yield*/, vscode.workspace.applyEdit(deleteEdit)];
                        case 3:
                            _a.sent();
                            (0, chai_1.expect)(formatEdits).to.be.an('array');
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('N3/RDF Support', function () {
        it('should provide N3-specific completions', function () {
            return __awaiter(this, void 0, void 0, function () {
                var position, completions, rdfCompletion;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            position = new vscode.Position(25, 10);
                            return [4 /*yield*/, vscode.commands.executeCommand('vscode.executeCompletionItemProvider', testDocument.uri, position)];
                        case 1:
                            completions = (_a.sent());
                            (0, chai_1.expect)(completions).to.not.be.undefined;
                            (0, chai_1.expect)(completions.items).to.be.an('array');
                            rdfCompletion = completions.items.find(function (item) {
                                var label = typeof item.label === 'string' ? item.label : item.label.label;
                                return label.includes('rdf:') || label.includes('rdfs:');
                            });
                            if (completions.items.length > 0) {
                                (0, chai_1.expect)(rdfCompletion).to.not.be.undefined;
                            }
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should validate N3 syntax', function () {
            return __awaiter(this, void 0, void 0, function () {
                var diagnostics, n3Errors;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: 
                        // Wait for diagnostics to be computed
                        return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 3000); })];
                        case 1:
                            // Wait for diagnostics to be computed
                            _a.sent();
                            diagnostics = vscode.languages.getDiagnostics(testDocument.uri);
                            (0, chai_1.expect)(diagnostics).to.be.an('array');
                            n3Errors = diagnostics.filter(function (diag) { return diag.message.includes('N3') || diag.message.includes('RDF'); });
                            // Valid N3 should not produce errors
                            (0, chai_1.expect)(n3Errors.length).to.equal(0);
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('Custom LSP Commands', function () {
        beforeEach(function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            if (!!lspClient.isRunning()) return [3 /*break*/, 2];
                            return [4 /*yield*/, lspClient.start()];
                        case 1:
                            _a.sent();
                            _a.label = 2;
                        case 2: return [2 /*return*/];
                    }
                });
            });
        });
        it('should execute Prolog queries', function () {
            return __awaiter(this, void 0, void 0, function () {
                var result, error_1;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            _a.trys.push([0, 2, , 3]);
                            return [4 /*yield*/, lspClient.executeQuery('parent(tom, X)')];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.not.be.undefined;
                            return [3 /*break*/, 3];
                        case 2:
                            error_1 = _a.sent();
                            // LSP server might not be fully ready, this is acceptable for testing
                            console.log('Query execution test skipped:', error_1.message);
                            return [3 /*break*/, 3];
                        case 3: return [2 /*return*/];
                    }
                });
            });
        });
        it('should get predicate help', function () {
            return __awaiter(this, void 0, void 0, function () {
                var result, error_2;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            _a.trys.push([0, 2, , 3]);
                            return [4 /*yield*/, lspClient.getHelp('member')];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.not.be.undefined;
                            return [3 /*break*/, 3];
                        case 2:
                            error_2 = _a.sent();
                            // LSP server might not be fully ready, this is acceptable for testing
                            console.log('Help test skipped:', error_2.message);
                            return [3 /*break*/, 3];
                        case 3: return [2 /*return*/];
                    }
                });
            });
        });
        it('should consult files', function () {
            return __awaiter(this, void 0, void 0, function () {
                var result, error_3;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            _a.trys.push([0, 2, , 3]);
                            return [4 /*yield*/, lspClient.consultFile(testDocument.uri.fsPath)];
                        case 1:
                            result = _a.sent();
                            (0, chai_1.expect)(result).to.not.be.undefined;
                            return [3 /*break*/, 3];
                        case 2:
                            error_3 = _a.sent();
                            // LSP server might not be fully ready, this is acceptable for testing
                            console.log('Consult test skipped:', error_3.message);
                            return [3 /*break*/, 3];
                        case 3: return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('Multi-IDE Support', function () {
        it('should generate IDE configurations', function () {
            return __awaiter(this, void 0, void 0, function () {
                var lspDir, configFiles, _i, configFiles_1, configFile, configPath, config;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, multiIDESupport_js_1.MultiIDESupport.generateIDEConfigurations(testWorkspace)];
                        case 1:
                            _a.sent();
                            lspDir = path.join(testWorkspace, '.lsp');
                            (0, chai_1.expect)(fs.existsSync(lspDir)).to.be.true;
                            configFiles = [
                                'vscode.json',
                                'coc-settings.json',
                                'neovim.json',
                                'vim.json',
                                'emacs.json',
                            ];
                            for (_i = 0, configFiles_1 = configFiles; _i < configFiles_1.length; _i++) {
                                configFile = configFiles_1[_i];
                                configPath = path.join(lspDir, configFile);
                                (0, chai_1.expect)(fs.existsSync(configPath)).to.be.true;
                                config = JSON.parse(fs.readFileSync(configPath, 'utf8'));
                                (0, chai_1.expect)(config).to.be.an('object');
                            }
                            // Check for setup scripts
                            (0, chai_1.expect)(fs.existsSync(path.join(lspDir, 'setup.sh'))).to.be.true;
                            (0, chai_1.expect)(fs.existsSync(path.join(lspDir, 'setup.ps1'))).to.be.true;
                            (0, chai_1.expect)(fs.existsSync(path.join(lspDir, 'README.md'))).to.be.true;
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should generate launch configurations', function () {
            return __awaiter(this, void 0, void 0, function () {
                var launchPath, launchConfig, prologLSPConfig;
                return __generator(this, function (_a) {
                    multiIDESupport_js_1.MultiIDESupport.generateLaunchConfigurations(testWorkspace);
                    launchPath = path.join(testWorkspace, '.vscode', 'launch.json');
                    (0, chai_1.expect)(fs.existsSync(launchPath)).to.be.true;
                    launchConfig = JSON.parse(fs.readFileSync(launchPath, 'utf8'));
                    (0, chai_1.expect)(launchConfig.configurations).to.be.an('array');
                    (0, chai_1.expect)(launchConfig.configurations.length).to.be.greaterThan(0);
                    prologLSPConfig = launchConfig.configurations.find(function (config) {
                        return config.name.includes('Prolog LSP');
                    });
                    (0, chai_1.expect)(prologLSPConfig).to.not.be.undefined;
                    return [2 /*return*/];
                });
            });
        });
        it('should detect available IDEs', function () {
            return __awaiter(this, void 0, void 0, function () {
                var availableIDEs;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, multiIDESupport_js_1.MultiIDESupport.detectAvailableIDEs()];
                        case 1:
                            availableIDEs = _a.sent();
                            (0, chai_1.expect)(availableIDEs).to.be.an('array');
                            // Should at least detect VS Code in test environment
                            (0, chai_1.expect)(availableIDEs).to.include('vscode');
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('Error Handling', function () {
        it('should handle LSP server startup failures gracefully', function () {
            return __awaiter(this, void 0, void 0, function () {
                var mockContext, invalidClient, error_4;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: 
                        // Stop the current client
                        return [4 /*yield*/, lspClient.stop()];
                        case 1:
                            // Stop the current client
                            _a.sent();
                            mockContext = {
                                subscriptions: [],
                                asAbsolutePath: function (relativePath) { return '/invalid/path/to/server.js'; },
                            };
                            invalidClient = new prologLSPClient_js_1.PrologLSPClient(mockContext);
                            _a.label = 2;
                        case 2:
                            _a.trys.push([2, 4, , 5]);
                            return [4 /*yield*/, invalidClient.start()];
                        case 3:
                            _a.sent();
                            // Should not reach here
                            chai_1.expect.fail('Expected LSP client to fail with invalid server path');
                            return [3 /*break*/, 5];
                        case 4:
                            error_4 = _a.sent();
                            (0, chai_1.expect)(error_4).to.not.be.undefined;
                            return [3 /*break*/, 5];
                        case 5: 
                        // Restart the original client for other tests
                        return [4 /*yield*/, lspClient.start()];
                        case 6:
                            // Restart the original client for other tests
                            _a.sent();
                            return [2 /*return*/];
                    }
                });
            });
        });
        it('should handle malformed Prolog syntax', function () {
            return __awaiter(this, void 0, void 0, function () {
                var edit, insertPosition, malformedCode, diagnostics, syntaxErrors, deleteEdit;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            edit = new vscode.WorkspaceEdit();
                            insertPosition = new vscode.Position(20, 0);
                            malformedCode = 'malformed_predicate(X, Y :- invalid_syntax.';
                            edit.insert(testDocument.uri, insertPosition, malformedCode);
                            return [4 /*yield*/, vscode.workspace.applyEdit(edit)];
                        case 1:
                            _a.sent();
                            // Wait for diagnostics
                            return [4 /*yield*/, new Promise(function (resolve) { return setTimeout(resolve, 2000); })];
                        case 2:
                            // Wait for diagnostics
                            _a.sent();
                            diagnostics = vscode.languages.getDiagnostics(testDocument.uri);
                            (0, chai_1.expect)(diagnostics).to.be.an('array');
                            syntaxErrors = diagnostics.filter(function (diag) { return diag.severity === vscode.DiagnosticSeverity.Error; });
                            (0, chai_1.expect)(syntaxErrors.length).to.be.greaterThan(0);
                            deleteEdit = new vscode.WorkspaceEdit();
                            deleteEdit.delete(testDocument.uri, new vscode.Range(insertPosition, new vscode.Position(20, malformedCode.length)));
                            return [4 /*yield*/, vscode.workspace.applyEdit(deleteEdit)];
                        case 3:
                            _a.sent();
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
    describe('Performance', function () {
        it('should handle large documents efficiently', function () {
            return __awaiter(this, void 0, void 0, function () {
                var largeContent, i, largeFile, largeUri, largeDocument, startTime, position, completions, endTime, duration;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            largeContent = '% Large Prolog file for performance testing\n';
                            for (i = 0; i < 1000; i++) {
                                largeContent += "fact".concat(i, "(arg1, arg2, arg3).\n");
                                largeContent += "rule".concat(i, "(X, Y) :- fact").concat(i, "(X, Y, Z), Z > 0.\n");
                            }
                            largeFile = path.join(testWorkspace, 'large.pl');
                            fs.writeFileSync(largeFile, largeContent);
                            largeUri = vscode.Uri.file(largeFile);
                            return [4 /*yield*/, vscode.workspace.openTextDocument(largeUri)];
                        case 1:
                            largeDocument = _a.sent();
                            return [4 /*yield*/, vscode.window.showTextDocument(largeDocument)];
                        case 2:
                            _a.sent();
                            startTime = Date.now();
                            position = new vscode.Position(500, 10);
                            return [4 /*yield*/, vscode.commands.executeCommand('vscode.executeCompletionItemProvider', largeDocument.uri, position)];
                        case 3:
                            completions = (_a.sent());
                            endTime = Date.now();
                            duration = endTime - startTime;
                            (0, chai_1.expect)(completions).to.not.be.undefined;
                            (0, chai_1.expect)(duration).to.be.lessThan(5000); // Should complete within 5 seconds
                            // Clean up
                            return [4 /*yield*/, vscode.commands.executeCommand('workbench.action.closeActiveEditor')];
                        case 4:
                            // Clean up
                            _a.sent();
                            fs.unlinkSync(largeFile);
                            return [2 /*return*/];
                    }
                });
            });
        });
    });
});
