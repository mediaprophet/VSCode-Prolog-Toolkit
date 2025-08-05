# Prolog LSP Integration Guide

This guide covers the complete Language Server Protocol (LSP) implementation for the VSCode Prolog Toolkit, including multi-IDE support and advanced features.

## Overview

The Prolog LSP implementation provides a full-featured language server that supports:

- **Standard LSP Features**: All core LSP capabilities for rich IDE integration
- **Multi-IDE Support**: Configurations for 10+ popular editors and IDEs
- **Prolog-Specific Features**: Query execution, help system, and N3/RDF reasoning
- **Advanced Diagnostics**: Real-time syntax checking and semantic validation
- **Performance Optimization**: Efficient handling of large codebases

## Architecture

### Components

1. **PrologLSPServer** (`src/features/prologLSPServer.ts`)
   - Full LSP server implementation using `vscode-languageserver`
   - Handles all LSP protocol messages and requests
   - Integrates with Prolog backend for advanced features

2. **PrologLSPClient** (`src/features/prologLSPClient.ts`)
   - VS Code LSP client wrapper
   - Provides enhanced middleware and custom features
   - Manages client lifecycle and error handling

3. **MultiIDESupport** (`src/features/multiIDESupport.ts`)
   - Generates configurations for multiple IDEs
   - Provides setup scripts and documentation
   - Detects available IDEs on the system

4. **PrologLSPExtension** (`src/features/prologLSPExtension.ts`)
   - Legacy LSP-style extension (maintained for compatibility)
   - Custom commands and code actions
   - N3 diagnostics and validation

## Features

### Core LSP Features

#### Text Synchronization
- **Full Document Sync**: Complete document content synchronization
- **Incremental Updates**: Efficient delta-based updates
- **File Watching**: Automatic detection of file changes

#### Code Intelligence

##### Completions
```typescript
// Trigger: Ctrl+Space or automatic on typing
// Supports:
- Built-in predicates (member/2, append/3, findall/3, etc.)
- User-defined predicates from workspace
- N3/RDF vocabulary (rdf:, rdfs:, owl:)
- Context-aware suggestions
- Snippet completions with parameters
```

##### Hover Information
```prolog
% Hover over 'member' to see:
member(X, [1,2,3]).
% → Shows: member/2 - True if Elem is a member of List
%   With examples and parameter descriptions
```

##### Go to Definition
```prolog
% Ctrl+Click or F12 on predicate calls
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
%                    ^^^^^^ jumps to parent/2 definition
```

##### Find All References
```prolog
% Shift+F12 on predicate to find all usages
% Shows references across all workspace files
```

##### Document Symbols
```prolog
% Outline view shows:
├── parent/2 (4 clauses)
├── grandparent/2 (1 clause)
├── :- use_module(library(lists))
└── % Comments and sections
```

##### Workspace Symbols
```prolog
% Ctrl+T to search predicates across workspace
% Fuzzy search: "mem" finds member/2, remember/3, etc.
```

#### Diagnostics

##### Syntax Validation
```prolog
% Real-time error detection:
malformed_clause(X, Y :- invalid.  % ← Error: unmatched parentheses
missing_period(X, Y)               % ← Warning: missing period
```

##### Semantic Analysis
```prolog
% Advanced validation:
undefined_pred(X) :- unknown_predicate(X).  % ← Info: may be undefined
singleton_var(X, _Y) :- member(X, [1,2,3]). % ← Warning: singleton variable Y
```

##### N3/RDF Validation
```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
:socrates rdf:type :Person .
:socrates unknown:property :value .  % ← Warning: undefined prefix 'unknown'
```

#### Code Actions

##### Quick Fixes
- Add missing periods
- Fix unmatched parentheses
- Import missing modules
- Define undefined predicates

##### Refactoring
- Extract predicate
- Rename predicate across files
- Organize imports
- Format document

##### Source Actions
- Execute as Prolog query
- Get predicate help
- Run N3 diagnostics
- Generate documentation

#### Formatting

##### Document Formatting
```prolog
% Before formatting:
parent(tom,bob).parent(tom,liz).
grandparent(X,Z):-parent(X,Y),parent(Y,Z).

% After formatting (Ctrl+Shift+I):
parent(tom, bob).
parent(tom, liz).

grandparent(X, Z) :-
    parent(X, Y),
    parent(Y, Z).
```

##### Range Formatting
- Format selected text only
- Preserve surrounding code style
- Configurable formatting options

#### Signature Help

```prolog
% Type opening parenthesis to see parameter hints:
findall(|  % ← Shows: findall(Template, Goal, List)
%       ^    Template: term to collect
%            Goal: goal to solve
%            List: resulting list
```

#### Rename Symbol

```prolog
% F2 on predicate name renames across all files:
parent(X, Y) :- father(X, Y).  % Rename 'parent' to 'ancestor'
parent(X, Y) :- mother(X, Y).  % ← Also renamed automatically
```

### Advanced Features

#### Query Execution

```prolog
% Execute queries directly from editor:
% 1. Select query text
% 2. Right-click → "Execute as Prolog query"
% 3. View results in output panel

?- member(X, [1,2,3]).
% Results:
% X = 1 ;
% X = 2 ;
% X = 3.
```

#### Help System

```prolog
% Get instant help for predicates:
% 1. Right-click on predicate → "Get Help"
% 2. Or use Ctrl+Shift+H

member(X, List).  % ← Get help shows full documentation
```

#### N3/RDF Reasoning

```turtle
% Load N3 data and perform reasoning:
@prefix : <http://example.org/> .
:socrates rdf:type :Person .
:Person rdfs:subClassOf :Mortal .

% Query: ?- rdf(:socrates, rdf:type, :Mortal).
% Result: true (via RDFS inference)
```

## Multi-IDE Support

### Supported IDEs

| IDE/Editor | Status | Configuration | Features |
|------------|--------|---------------|----------|
| **VS Code** | ✅ Native | Automatic | All features |
| **Neovim** | ✅ Full | Built-in LSP | All features |
| **Vim** | ✅ Full | vim-lsp | All features |
| **Emacs** | ✅ Full | lsp-mode | All features |
| **Sublime Text** | ✅ Full | LSP package | All features |
| **IntelliJ IDEA** | ✅ Full | LSP4IJ | All features |
| **Eclipse** | ✅ Full | LSP4E | All features |
| **Theia** | ✅ Full | Extension | All features |
| **Atom** | ⚠️ Limited | atom-ide | Basic features |
| **Brackets** | ⚠️ Limited | LSP client | Basic features |

### Setup Instructions

#### VS Code (Native)
```bash
# Install from marketplace
code --install-extension mediaprophet.vscode-prolog-toolkit
```

#### Neovim (Built-in LSP)
```lua
-- Add to init.lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

if not configs.prolog_lsp then
  configs.prolog_lsp = {
    default_config = {
      cmd = {'node', './out/pub/features/prologLSPServer.js', '--stdio'},
      filetypes = {'prolog'},
      root_dir = lspconfig.util.root_pattern('.git', '*.pl'),
      settings = {
        prolog = {
          executablePath = 'swipl',
          dialect = 'swi'
        }
      }
    }
  }
end

lspconfig.prolog_lsp.setup{}
```

#### Vim (vim-lsp)
```vim
" Add to .vimrc
if executable('node')
  augroup LspProlog
    autocmd!
    autocmd User lsp_setup call lsp#register_server({
      \ 'name': 'prolog-lsp',
      \ 'cmd': {server_info->['node', './out/pub/features/prologLSPServer.js', '--stdio']},
      \ 'allowlist': ['prolog'],
      \ })
  augroup END
endif
```

#### Emacs (lsp-mode)
```elisp
;; Add to init.el
(use-package lsp-mode
  :hook (prolog-mode . lsp)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("node" "./out/pub/features/prologLSPServer.js" "--stdio"))
    :major-modes '(prolog-mode)
    :server-id 'prolog-lsp)))
```

#### Sublime Text
```json
// LSP.sublime-settings
{
  "clients": {
    "prolog-lsp": {
      "enabled": true,
      "command": ["node", "./out/pub/features/prologLSPServer.js", "--stdio"],
      "selector": "source.prolog"
    }
  }
}
```

### Automatic Configuration Generation

The extension automatically generates IDE configurations:

```bash
# Generates .lsp/ directory with:
├── vscode.json          # VS Code configuration
├── coc-settings.json    # coc.nvim configuration
├── neovim.json         # Neovim built-in LSP
├── vim.json            # vim-lsp configuration
├── emacs.json          # Emacs lsp-mode
├── sublime.json        # Sublime Text LSP
├── intellij.json       # IntelliJ IDEA
├── eclipse.json        # Eclipse LSP4E
├── theia.json          # Theia extension
├── setup.sh            # Unix setup script
├── setup.ps1           # Windows setup script
└── README.md           # Detailed instructions
```

## Configuration

### Server Settings

```json
{
  "prolog": {
    "executablePath": "swipl",
    "dialect": "swi",
    "linter": {
      "run": "onType",
      "delay": 500,
      "enableMsgInOutput": false
    },
    "format": {
      "addSpace": true
    },
    "lsp": {
      "enabled": true,
      "port": 3061,
      "logLevel": "info"
    }
  }
}
```

### Client Settings

```json
{
  "prolog.lsp": {
    "enabled": true,
    "serverPath": "./out/pub/features/prologLSPServer.js",
    "trace": {
      "server": "verbose"
    },
    "initializationOptions": {
      "enableSnippets": true,
      "enableFormatting": true,
      "enableDiagnostics": true,
      "enableSemanticTokens": true
    }
  }
}
```

## Development

### Building the LSP Server

```bash
# Install dependencies
npm install

# Build the server
npm run build

# Run tests
npm run test:lsp

# Debug the server
npm run debug:lsp
```

### Testing

```bash
# Run all LSP tests
npm run test:lsp

# Run specific test suites
npm run test:lsp -- --grep "completions"
npm run test:lsp -- --grep "diagnostics"
npm run test:lsp -- --grep "multi-ide"

# Performance tests
npm run test:lsp:performance
```

### Debugging

#### Server Debugging
```bash
# Start server in debug mode
node --inspect=9229 ./out/pub/features/prologLSPServer.js --stdio

# Attach debugger in VS Code
# Use launch configuration "Debug Prolog LSP Server"
```

#### Client Debugging
```json
// In VS Code settings.json
{
  "prolog.lsp.trace.server": "verbose",
  "prolog.lsp.logLevel": "debug"
}
```

#### Protocol Tracing
```bash
# Enable LSP protocol tracing
export LSP_TRACE=1
node ./out/pub/features/prologLSPServer.js --stdio
```

## Performance

### Optimization Features

1. **Incremental Parsing**: Only re-parse changed sections
2. **Lazy Loading**: Load predicates on-demand
3. **Caching**: Cache completion items and symbols
4. **Streaming**: Handle large result sets efficiently
5. **Debouncing**: Reduce unnecessary computations

### Performance Metrics

| Operation | Target Time | Typical Time |
|-----------|-------------|--------------|
| Completion | < 100ms | 50-80ms |
| Hover | < 50ms | 20-30ms |
| Diagnostics | < 200ms | 100-150ms |
| Formatting | < 500ms | 200-300ms |
| Go to Definition | < 100ms | 30-50ms |

### Large File Handling

```prolog
% Files with 10,000+ lines are handled efficiently:
% - Incremental parsing
% - Symbol indexing
% - Memory management
% - Progress reporting
```

## Troubleshooting

### Common Issues

#### LSP Server Not Starting
```bash
# Check Node.js installation
node --version  # Should be v14+

# Check server file exists
ls -la ./out/pub/features/prologLSPServer.js

# Check permissions
chmod +x ./out/pub/features/prologLSPServer.js

# Check logs
tail -f ~/.vscode/logs/*/exthost*/output.log
```

#### No Completions/Features
```bash
# Check LSP client connection
# VS Code: Developer Tools → Console
# Look for LSP connection messages

# Check server logs
# Enable trace.server: "verbose" in settings

# Verify file association
# Ensure .pl files are recognized as Prolog
```

#### Performance Issues
```bash
# Check workspace size
find . -name "*.pl" | wc -l

# Disable features temporarily
{
  "prolog.lsp.enableDiagnostics": false,
  "prolog.lsp.enableSemanticTokens": false
}

# Increase timeouts
{
  "prolog.lsp.timeout": 10000
}
```

### Diagnostic Commands

```bash
# Check LSP server status
curl -X POST http://localhost:3061/status

# Validate configuration
node -e "console.log(require('./package.json').contributes.configuration)"

# Test server directly
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' | \
  node ./out/pub/features/prologLSPServer.js --stdio
```

## Contributing

### Adding New Features

1. **Server Features**: Implement in `prologLSPServer.ts`
2. **Client Features**: Add to `prologLSPClient.ts`
3. **Multi-IDE**: Update `multiIDESupport.ts`
4. **Tests**: Add to `lsp-integration.test.ts`

### Code Style

```typescript
// Follow existing patterns:
connection.onRequest('textDocument/completion', async (params) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return [];
  
  // Implementation here
  return completions;
});
```

### Testing Guidelines

```typescript
// Test structure:
describe('Feature Name', function() {
  beforeEach(async function() {
    // Setup
  });
  
  it('should handle normal case', async function() {
    // Test implementation
    expect(result).to.satisfy(condition);
  });
  
  it('should handle edge cases', async function() {
    // Edge case testing
  });
});
```

## Roadmap

### Planned Features

- [ ] **Call Hierarchy**: Show predicate call chains
- [ ] **Type Definitions**: Navigate to type definitions
- [ ] **Implementation**: Find predicate implementations
- [ ] **Declaration**: Navigate to declarations
- [ ] **Color Provider**: Syntax highlighting colors
- [ ] **Linked Editing**: Synchronized editing
- [ ] **Moniker**: Cross-reference support
- [ ] **Selection Range**: Smart selection expansion

### Performance Improvements

- [ ] **WebAssembly**: Compile critical parts to WASM
- [ ] **Worker Threads**: Parallel processing
- [ ] **Persistent Cache**: Cross-session caching
- [ ] **Incremental Indexing**: Faster workspace scanning

### Multi-IDE Enhancements

- [ ] **More IDEs**: Support for additional editors
- [ ] **Auto-detection**: Automatic IDE configuration
- [ ] **Cloud IDEs**: Support for online editors
- [ ] **Mobile IDEs**: Support for mobile development

## License

This LSP implementation is part of the VSCode Prolog Toolkit and is licensed under the MIT License.

## Support

- **Documentation**: [GitHub Wiki](https://github.com/mediaprophet/VSCode-Prolog-Toolkit/wiki)
- **Issues**: [GitHub Issues](https://github.com/mediaprophet/VSCode-Prolog-Toolkit/issues)
- **Discussions**: [GitHub Discussions](https://github.com/mediaprophet/VSCode-Prolog-Toolkit/discussions)
- **Email**: [Support Email](mailto:support@example.com)