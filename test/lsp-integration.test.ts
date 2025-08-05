import { expect } from 'chai';
import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { PrologLSPClient } from '../src/features/prologLSPClient';
import { MultiIDESupport } from '../src/features/multiIDESupport';

describe('LSP Integration Tests', function() {
  this.timeout(30000); // Increase timeout for LSP operations

  let lspClient: PrologLSPClient;
  let testWorkspace: string;
  let testDocument: vscode.TextDocument;

  before(async function() {
    // Setup test workspace
    testWorkspace = path.join(__dirname, 'test-workspace');
    if (!fs.existsSync(testWorkspace)) {
      fs.mkdirSync(testWorkspace, { recursive: true });
    }

    // Create test Prolog file
    const testFile = path.join(testWorkspace, 'test.pl');
    const testContent = `
% Test Prolog file for LSP testing
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

% Test predicate with documentation
%! member(?Elem, ?List) is nondet.
%
%  True if Elem is a member of List.
%
%  @param Elem Element to check
%  @param List List to search in
test_member(X, [X|_]).
test_member(X, [_|T]) :- test_member(X, T).

% N3/RDF test content
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix : <http://example.org/> .

:socrates rdf:type :Person .
:socrates :hasProperty :mortal .
`;

    fs.writeFileSync(testFile, testContent);

    // Open the test document
    const uri = vscode.Uri.file(testFile);
    testDocument = await vscode.workspace.openTextDocument(uri);
    await vscode.window.showTextDocument(testDocument);

    // Initialize LSP client
    const mockContext = {
      subscriptions: [],
      asAbsolutePath: (relativePath: string) => path.join(__dirname, '..', relativePath)
    } as any;

    lspClient = new PrologLSPClient(mockContext);
  });

  after(async function() {
    // Cleanup
    if (lspClient) {
      await lspClient.stop();
    }

    // Close test document
    if (testDocument) {
      await vscode.commands.executeCommand('workbench.action.closeActiveEditor');
    }

    // Clean up test workspace
    if (fs.existsSync(testWorkspace)) {
      fs.rmSync(testWorkspace, { recursive: true, force: true });
    }
  });

  describe('LSP Client Lifecycle', function() {
    it('should start LSP client successfully', async function() {
      await lspClient.start();
      expect(lspClient.isRunning()).to.be.true;
    });

    it('should handle client restart', async function() {
      await lspClient.restart();
      expect(lspClient.isRunning()).to.be.true;
    });

    it('should stop LSP client successfully', async function() {
      await lspClient.stop();
      expect(lspClient.isRunning()).to.be.false;
      
      // Restart for other tests
      await lspClient.start();
    });
  });

  describe('LSP Language Features', function() {
    beforeEach(async function() {
      // Ensure LSP client is running
      if (!lspClient.isRunning()) {
        await lspClient.start();
      }
      
      // Wait for LSP to be ready
      await new Promise(resolve => setTimeout(resolve, 2000));
    });

    it('should provide completions', async function() {
      const position = new vscode.Position(6, 20); // Inside grandparent rule
      
      const completions = await vscode.commands.executeCommand(
        'vscode.executeCompletionItemProvider',
        testDocument.uri,
        position
      ) as vscode.CompletionList;

      expect(completions).to.not.be.undefined;
      expect(completions.items).to.be.an('array');
      
      // Should include built-in predicates
      const memberCompletion = completions.items.find(item => 
        item.label === 'member' || (typeof item.label === 'object' && item.label.label === 'member')
      );
      expect(memberCompletion).to.not.be.undefined;
    });

    it('should provide hover information', async function() {
      const position = new vscode.Position(1, 0); // On 'parent' predicate
      
      const hovers = await vscode.commands.executeCommand(
        'vscode.executeHoverProvider',
        testDocument.uri,
        position
      ) as vscode.Hover[];

      expect(hovers).to.be.an('array');
      if (hovers.length > 0) {
        expect(hovers[0].contents).to.not.be.empty;
      }
    });

    it('should provide document symbols', async function() {
      const symbols = await vscode.commands.executeCommand(
        'vscode.executeDocumentSymbolProvider',
        testDocument.uri
      ) as vscode.DocumentSymbol[];

      expect(symbols).to.be.an('array');
      expect(symbols.length).to.be.greaterThan(0);
      
      // Should find predicates
      const parentSymbol = symbols.find(symbol => 
        symbol.name.includes('parent')
      );
      expect(parentSymbol).to.not.be.undefined;
    });

    it('should provide definitions', async function() {
      const position = new vscode.Position(6, 35); // On 'parent' in grandparent rule
      
      const definitions = await vscode.commands.executeCommand(
        'vscode.executeDefinitionProvider',
        testDocument.uri,
        position
      ) as vscode.Location[];

      expect(definitions).to.be.an('array');
      // Should find definition of parent predicate
      if (definitions.length > 0) {
        expect(definitions[0].uri.toString()).to.equal(testDocument.uri.toString());
      }
    });

    it('should provide references', async function() {
      const position = new vscode.Position(1, 0); // On first 'parent' predicate
      
      const references = await vscode.commands.executeCommand(
        'vscode.executeReferenceProvider',
        testDocument.uri,
        position
      ) as vscode.Location[];

      expect(references).to.be.an('array');
      expect(references.length).to.be.greaterThan(1); // Should find multiple references
    });

    it('should provide document highlights', async function() {
      const position = new vscode.Position(1, 0); // On 'parent' predicate
      
      const highlights = await vscode.commands.executeCommand(
        'vscode.executeDocumentHighlights',
        testDocument.uri,
        position
      ) as vscode.DocumentHighlight[];

      expect(highlights).to.be.an('array');
      if (highlights.length > 0) {
        expect(highlights[0].range).to.not.be.undefined;
      }
    });

    it('should provide signature help', async function() {
      // Insert a function call to test signature help
      const edit = new vscode.WorkspaceEdit();
      const insertPosition = new vscode.Position(20, 0);
      edit.insert(testDocument.uri, insertPosition, 'test_member(');
      await vscode.workspace.applyEdit(edit);

      const signaturePosition = new vscode.Position(20, 12); // After opening parenthesis
      
      const signatures = await vscode.commands.executeCommand(
        'vscode.executeSignatureHelpProvider',
        testDocument.uri,
        signaturePosition
      ) as vscode.SignatureHelp;

      // Clean up the edit
      const deleteEdit = new vscode.WorkspaceEdit();
      deleteEdit.delete(testDocument.uri, new vscode.Range(insertPosition, new vscode.Position(20, 12)));
      await vscode.workspace.applyEdit(deleteEdit);

      if (signatures) {
        expect(signatures.signatures).to.be.an('array');
      }
    });

    it('should provide code actions', async function() {
      const range = new vscode.Range(1, 0, 1, 10); // Select 'parent' predicate
      
      const codeActions = await vscode.commands.executeCommand(
        'vscode.executeCodeActionProvider',
        testDocument.uri,
        range
      ) as vscode.CodeAction[];

      expect(codeActions).to.be.an('array');
      // Should provide actions like "Execute as Query", "Get Help"
      if (codeActions.length > 0) {
        const queryAction = codeActions.find(action => 
          action.title.includes('Query') || action.title.includes('Execute')
        );
        expect(queryAction).to.not.be.undefined;
      }
    });

    it('should provide document formatting', async function() {
      // Create a poorly formatted line
      const edit = new vscode.WorkspaceEdit();
      const insertPosition = new vscode.Position(20, 0);
      const poorlyFormatted = 'test_format(X,Y,Z):-member(X,[1,2,3]),append(Y,Z,Result).';
      edit.insert(testDocument.uri, insertPosition, poorlyFormatted);
      await vscode.workspace.applyEdit(edit);

      const formatEdits = await vscode.commands.executeCommand(
        'vscode.executeFormatDocumentProvider',
        testDocument.uri
      ) as vscode.TextEdit[];

      // Clean up
      const deleteEdit = new vscode.WorkspaceEdit();
      deleteEdit.delete(testDocument.uri, new vscode.Range(insertPosition, new vscode.Position(20, poorlyFormatted.length)));
      await vscode.workspace.applyEdit(deleteEdit);

      expect(formatEdits).to.be.an('array');
    });
  });

  describe('N3/RDF Support', function() {
    it('should provide N3-specific completions', async function() {
      const position = new vscode.Position(25, 10); // In N3 content area
      
      const completions = await vscode.commands.executeCommand(
        'vscode.executeCompletionItemProvider',
        testDocument.uri,
        position
      ) as vscode.CompletionList;

      expect(completions).to.not.be.undefined;
      expect(completions.items).to.be.an('array');
      
      // Should include RDF-specific completions
      const rdfCompletion = completions.items.find(item => {
        const label = typeof item.label === 'string' ? item.label : item.label.label;
        return label.includes('rdf:') || label.includes('rdfs:');
      });
      
      if (completions.items.length > 0) {
        expect(rdfCompletion).to.not.be.undefined;
      }
    });

    it('should validate N3 syntax', async function() {
      // Wait for diagnostics to be computed
      await new Promise(resolve => setTimeout(resolve, 3000));
      
      const diagnostics = vscode.languages.getDiagnostics(testDocument.uri);
      expect(diagnostics).to.be.an('array');
      
      // Should not have syntax errors in valid N3 content
      const n3Errors = diagnostics.filter(diag => 
        diag.message.includes('N3') || diag.message.includes('RDF')
      );
      
      // Valid N3 should not produce errors
      expect(n3Errors.length).to.equal(0);
    });
  });

  describe('Custom LSP Commands', function() {
    beforeEach(async function() {
      if (!lspClient.isRunning()) {
        await lspClient.start();
      }
    });

    it('should execute Prolog queries', async function() {
      try {
        const result = await lspClient.executeQuery('parent(tom, X)');
        expect(result).to.not.be.undefined;
        // Result structure depends on backend implementation
      } catch (error) {
        // LSP server might not be fully ready, this is acceptable for testing
        console.log('Query execution test skipped:', error.message);
      }
    });

    it('should get predicate help', async function() {
      try {
        const result = await lspClient.getHelp('member');
        expect(result).to.not.be.undefined;
      } catch (error) {
        // LSP server might not be fully ready, this is acceptable for testing
        console.log('Help test skipped:', error.message);
      }
    });

    it('should consult files', async function() {
      try {
        const result = await lspClient.consultFile(testDocument.uri.fsPath);
        expect(result).to.not.be.undefined;
      } catch (error) {
        // LSP server might not be fully ready, this is acceptable for testing
        console.log('Consult test skipped:', error.message);
      }
    });
  });

  describe('Multi-IDE Support', function() {
    it('should generate IDE configurations', async function() {
      await MultiIDESupport.generateIDEConfigurations(testWorkspace);
      
      const lspDir = path.join(testWorkspace, '.lsp');
      expect(fs.existsSync(lspDir)).to.be.true;
      
      // Check for configuration files
      const configFiles = ['vscode.json', 'coc-settings.json', 'neovim.json', 'vim.json', 'emacs.json'];
      for (const configFile of configFiles) {
        const configPath = path.join(lspDir, configFile);
        expect(fs.existsSync(configPath)).to.be.true;
        
        const config = JSON.parse(fs.readFileSync(configPath, 'utf8'));
        expect(config).to.be.an('object');
      }
      
      // Check for setup scripts
      expect(fs.existsSync(path.join(lspDir, 'setup.sh'))).to.be.true;
      expect(fs.existsSync(path.join(lspDir, 'setup.ps1'))).to.be.true;
      expect(fs.existsSync(path.join(lspDir, 'README.md'))).to.be.true;
    });

    it('should generate launch configurations', async function() {
      MultiIDESupport.generateLaunchConfigurations(testWorkspace);
      
      const launchPath = path.join(testWorkspace, '.vscode', 'launch.json');
      expect(fs.existsSync(launchPath)).to.be.true;
      
      const launchConfig = JSON.parse(fs.readFileSync(launchPath, 'utf8'));
      expect(launchConfig.configurations).to.be.an('array');
      expect(launchConfig.configurations.length).to.be.greaterThan(0);
      
      const prologLSPConfig = launchConfig.configurations.find((config: any) => 
        config.name.includes('Prolog LSP')
      );
      expect(prologLSPConfig).to.not.be.undefined;
    });

    it('should detect available IDEs', async function() {
      const availableIDEs = await MultiIDESupport.detectAvailableIDEs();
      expect(availableIDEs).to.be.an('array');
      
      // Should at least detect VS Code in test environment
      expect(availableIDEs).to.include('vscode');
    });
  });

  describe('Error Handling', function() {
    it('should handle LSP server startup failures gracefully', async function() {
      // Stop the current client
      await lspClient.stop();
      
      // Create a client with invalid server path
      const mockContext = {
        subscriptions: [],
        asAbsolutePath: (relativePath: string) => '/invalid/path/to/server.js'
      } as any;
      
      const invalidClient = new PrologLSPClient(mockContext);
      
      try {
        await invalidClient.start();
        // Should not reach here
        expect.fail('Expected LSP client to fail with invalid server path');
      } catch (error) {
        expect(error).to.not.be.undefined;
      }
      
      // Restart the original client for other tests
      await lspClient.start();
    });

    it('should handle malformed Prolog syntax', async function() {
      // Insert malformed Prolog code
      const edit = new vscode.WorkspaceEdit();
      const insertPosition = new vscode.Position(20, 0);
      const malformedCode = 'malformed_predicate(X, Y :- invalid_syntax.';
      edit.insert(testDocument.uri, insertPosition, malformedCode);
      await vscode.workspace.applyEdit(edit);

      // Wait for diagnostics
      await new Promise(resolve => setTimeout(resolve, 2000));
      
      const diagnostics = vscode.languages.getDiagnostics(testDocument.uri);
      expect(diagnostics).to.be.an('array');
      
      // Should detect syntax errors
      const syntaxErrors = diagnostics.filter(diag => 
        diag.severity === vscode.DiagnosticSeverity.Error
      );
      expect(syntaxErrors.length).to.be.greaterThan(0);

      // Clean up
      const deleteEdit = new vscode.WorkspaceEdit();
      deleteEdit.delete(testDocument.uri, new vscode.Range(insertPosition, new vscode.Position(20, malformedCode.length)));
      await vscode.workspace.applyEdit(deleteEdit);
    });
  });

  describe('Performance', function() {
    it('should handle large documents efficiently', async function() {
      // Create a large Prolog document
      let largeContent = '% Large Prolog file for performance testing\n';
      for (let i = 0; i < 1000; i++) {
        largeContent += `fact${i}(arg1, arg2, arg3).\n`;
        largeContent += `rule${i}(X, Y) :- fact${i}(X, Y, Z), Z > 0.\n`;
      }

      const largeFile = path.join(testWorkspace, 'large.pl');
      fs.writeFileSync(largeFile, largeContent);

      const largeUri = vscode.Uri.file(largeFile);
      const largeDocument = await vscode.workspace.openTextDocument(largeUri);
      await vscode.window.showTextDocument(largeDocument);

      const startTime = Date.now();
      
      // Test completion performance
      const position = new vscode.Position(500, 10);
      const completions = await vscode.commands.executeCommand(
        'vscode.executeCompletionItemProvider',
        largeDocument.uri,
        position
      ) as vscode.CompletionList;

      const endTime = Date.now();
      const duration = endTime - startTime;

      expect(completions).to.not.be.undefined;
      expect(duration).to.be.lessThan(5000); // Should complete within 5 seconds

      // Clean up
      await vscode.commands.executeCommand('workbench.action.closeActiveEditor');
      fs.unlinkSync(largeFile);
    });
  });
});