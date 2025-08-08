import { exec } from 'child_process';
import * as fs from 'fs';
import * as path from 'path';

/**
 * Multi-IDE Support for Prolog LSP
 * Provides configuration and setup for various IDEs and editors
 */
export class MultiIDESupport {
  private static readonly SUPPORTED_IDES = [
    'vscode',
    'vim',
    'neovim',
    'emacs',
    'sublime',
    'atom',
    'intellij',
    'eclipse',
    'theia',
    'coc-nvim',
  ];

  /**
   * Generate LSP configuration files for different IDEs
   */
  public static async generateIDEConfigurations(workspaceRoot: string): Promise<void> {
    const configs = {
      // VS Code configuration (already handled by extension)
      vscode: this.generateVSCodeConfig(),

      // Vim/Neovim with coc.nvim
      'coc-settings': this.generateCocNvimConfig(),

      // Neovim with built-in LSP
      neovim: this.generateNeovimConfig(),

      // Vim with vim-lsp
      vim: this.generateVimLspConfig(),

      // Emacs with lsp-mode
      emacs: this.generateEmacsConfig(),

      // Sublime Text with LSP package
      sublime: this.generateSublimeConfig(),

      // IntelliJ IDEA configuration
      intellij: this.generateIntelliJConfig(),

      // Eclipse configuration
      eclipse: this.generateEclipseConfig(),

      // Theia configuration
      theia: this.generateTheiaConfig(),
    };

    // Create .lsp directory for configurations
    const lspDir = path.join(workspaceRoot, '.lsp');
    if (!fs.existsSync(lspDir)) {
      fs.mkdirSync(lspDir, { recursive: true });
    }

    // Write configuration files
    for (const [ide, config] of Object.entries(configs)) {
      if (config) {
        const configPath = path.join(lspDir, `${ide}.json`);
        fs.writeFileSync(configPath, JSON.stringify(config, null, 2));
      }
    }

    // Generate setup scripts
    await this.generateSetupScripts(lspDir);

    // Generate documentation
    await this.generateIDEDocumentation(lspDir);
  }

  private static generateVSCodeConfig() {
    return {
      name: 'Prolog LSP for VS Code',
      description: 'Already configured through the VSCode Prolog Toolkit extension',
      status: 'native_support',
      setup: 'Install the VSCode Prolog Toolkit extension from the marketplace',
    };
  }

  private static generateCocNvimConfig() {
    return {
      languageserver: {
        prolog: {
          command: 'node',
          args: ['./out/pub/features/lsp/server.js', '--stdio'],
          filetypes: ['prolog'],
          rootPatterns: ['.git', '*.pl', '*.pro', '*.prolog'],
          settings: {
            prolog: {
              executablePath: 'swipl',
              dialect: 'swi',
              linter: {
                run: 'onType',
                delay: 500,
              },
            },
          },
          initializationOptions: {
            enableSnippets: true,
            enableFormatting: true,
            enableDiagnostics: true,
          },
        },
      },
    };
  }

  private static generateNeovimConfig() {
    return {
      setup_function: `
-- Neovim built-in LSP setup for Prolog
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

-- Define Prolog LSP configuration
if not configs.prolog_lsp then
  configs.prolog_lsp = {
    default_config = {
      cmd = {'node', './out/pub/features/prologLSPServer.js', '--stdio'},
      filetypes = {'prolog'},
      root_dir = lspconfig.util.root_pattern('.git', '*.pl', '*.pro', '*.prolog'),
      settings = {
        prolog = {
          executablePath = 'swipl',
          dialect = 'swi',
          linter = {
            run = 'onType',
            delay = 500
          }
        }
      }
    }
  }
end

-- Setup the LSP
lspconfig.prolog_lsp.setup({
  on_attach = function(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
    
    -- Mappings
    local opts = { noremap=true, silent=true }
    vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
  end,
  capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
})
      `,
      filetype_detection: `
-- Add Prolog filetype detection
vim.cmd([[
  augroup PrologFiletype
    autocmd!
    autocmd BufRead,BufNewFile *.pl,*.pro,*.prolog,*.plt,*.ecl set filetype=prolog
  augroup END
]])
      `,
    };
  }

  private static generateVimLspConfig() {
    return {
      vim_script: `
" Vim-LSP configuration for Prolog
if executable('node')
  augroup LspProlog
    autocmd!
    autocmd User lsp_setup call lsp#register_server({
      \\ 'name': 'prolog-lsp',
      \\ 'cmd': {server_info->['node', './out/pub/features/prologLSPServer.js', '--stdio']},
      \\ 'allowlist': ['prolog'],
      \\ 'root_uri': {server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), ['.git', '*.pl', '*.pro']))},
      \\ })
  augroup END
endif

" Prolog filetype detection
augroup PrologFiletype
  autocmd!
  autocmd BufRead,BufNewFile *.pl,*.pro,*.prolog,*.plt,*.ecl set filetype=prolog
augroup END

" Key mappings for LSP
function! s:on_lsp_buffer_enabled() abort
  setlocal omnifunc=lsp#complete
  setlocal signcolumn=yes
  if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
  nmap <buffer> gd <plug>(lsp-definition)
  nmap <buffer> gs <plug>(lsp-document-symbol-search)
  nmap <buffer> gS <plug>(lsp-workspace-symbol-search)
  nmap <buffer> gr <plug>(lsp-references)
  nmap <buffer> gi <plug>(lsp-implementation)
  nmap <buffer> gt <plug>(lsp-type-definition)
  nmap <buffer> <leader>rn <plug>(lsp-rename)
  nmap <buffer> [g <plug>(lsp-previous-diagnostic)
  nmap <buffer> ]g <plug>(lsp-next-diagnostic)
  nmap <buffer> K <plug>(lsp-hover)
endfunction

augroup lsp_install
  au!
  autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END
      `,
    };
  }

  private static generateEmacsConfig() {
    return {
      elisp_config: `
;; Emacs LSP configuration for Prolog
(use-package lsp-mode
  :hook (prolog-mode . lsp)
  :commands lsp
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("node" "./out/pub/features/prologLSPServer.js" "--stdio"))
    :new-connection (lsp-stdio-connection '("node" "./out/pub/features/lsp/server.js" "--stdio"))
    :major-modes '(prolog-mode)
    :server-id 'prolog-lsp
    :root-uri (lsp-workspace-root)
    :initialization-options '((enableSnippets . t)
                             (enableFormatting . t)
                             (enableDiagnostics . t)))))

;; Prolog mode setup
(use-package prolog
  :mode (("\\\\.pl\\\\'" . prolog-mode)
         ("\\\\.pro\\\\'" . prolog-mode)
         ("\\\\.prolog\\\\'" . prolog-mode)
         ("\\\\.plt\\\\'" . prolog-mode)
         ("\\\\.ecl\\\\'" . prolog-mode))
  :config
  (setq prolog-system 'swi))

;; LSP UI enhancements
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t))

;; Company completion
(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))
      `,
    };
  }

  private static generateSublimeConfig() {
    return {
      'LSP.sublime-settings': {
        clients: {
          'prolog-lsp': {
            enabled: true,
            command: ['node', './out/pub/features/lsp/server.js', '--stdio'],
            selector: 'source.prolog',
            settings: {
              prolog: {
                executablePath: 'swipl',
                dialect: 'swi',
                linter: {
                  run: 'onType',
                  delay: 500,
                },
              },
            },
            initializationOptions: {
              enableSnippets: true,
              enableFormatting: true,
              enableDiagnostics: true,
            },
          },
        },
      },
      'Prolog.sublime-syntax': `
%YAML 1.2
---
name: Prolog
file_extensions: [pl, pro, prolog, plt, ecl]
scope: source.prolog

contexts:
  main:
    - include: comments
    - include: strings
    - include: numbers
    - include: predicates
    - include: variables
    - include: operators

  comments:
    - match: '%.*$'
      scope: comment.line.prolog
    - match: '/\\*'
      push: block_comment

  block_comment:
    - meta_scope: comment.block.prolog
    - match: '\\*/'
      pop: true

  strings:
    - match: "'"
      push: single_quoted_string
    - match: '"'
      push: double_quoted_string

  single_quoted_string:
    - meta_scope: string.quoted.single.prolog
    - match: "'"
      pop: true

  double_quoted_string:
    - meta_scope: string.quoted.double.prolog
    - match: '"'
      pop: true

  numbers:
    - match: '\\b\\d+(\\.\\d+)?\\b'
      scope: constant.numeric.prolog

  predicates:
    - match: '\\b[a-z][a-zA-Z0-9_]*(?=\\s*\\()'
      scope: entity.name.function.prolog

  variables:
    - match: '\\b[A-Z_][a-zA-Z0-9_]*\\b'
      scope: variable.prolog

  operators:
    - match: ':-|->|;|,|\\+|\\-|\\*|/|=|\\\\=|==|\\\\==|@<|@>|@=<|@>='
      scope: keyword.operator.prolog
      `,
    };
  }

  private static generateIntelliJConfig() {
    return {
      plugin_xml: `
<idea-plugin>
  <id>com.example.prolog-lsp</id>
  <name>Prolog LSP Support</name>
  <version>1.0</version>
  <vendor>Prolog Toolkit</vendor>
  
  <description>
    Language Server Protocol support for Prolog in IntelliJ IDEA
  </description>
  
  <depends>com.intellij.modules.platform</depends>
  <depends>com.redhat.devtools.lsp4ij</depends>
  
  <extensions defaultExtensionNs="com.intellij">
    <fileType name="Prolog" 
              implementationClass="com.example.prolog.PrologFileType" 
              fieldName="INSTANCE" 
              language="Prolog" 
              extensions="pl;pro;prolog;plt;ecl"/>
              
    <lang.parserDefinition language="Prolog" 
                          implementationClass="com.example.prolog.PrologParserDefinition"/>
  </extensions>
  
  <extensions defaultExtensionNs="com.redhat.devtools.lsp4ij">
    <server id="prolog-lsp"
            name="Prolog Language Server"
            factoryClass="com.example.prolog.PrologLanguageServerFactory"/>
  </extensions>
</idea-plugin>
      `,
      server_factory: `
public class PrologLanguageServerFactory implements LanguageServerFactory {
    @Override
    public LanguageServerDefinition createLanguageServerDefinition(@NotNull Project project) {
        return new LanguageServerDefinition(
            "prolog-lsp",
            "Prolog Language Server",
            Arrays.asList("node", "./out/pub/features/prologLSPServer.js", "--stdio"),
              Arrays.asList("node", "./out/pub/features/lsp/server.js", "--stdio"),
            Arrays.asList("pl", "pro", "prolog", "plt", "ecl")
        );
    }
}
      `,
    };
  }

  private static generateEclipseConfig() {
    return {
      plugin_xml: `
<?xml version="1.0" encoding="UTF-8"?>
<plugin>
   <extension point="org.eclipse.lsp4e.languageServer">
      <server id="prolog.lsp"
              label="Prolog Language Server"
              class="org.eclipse.lsp4e.server.ProcessStreamConnectionProvider">
         <commandline>
            <arg>node</arg>
            <arg>./out/pub/features/prologLSPServer.js</arg>
              <arg>./out/pub/features/lsp/server.js</arg>
            <arg>--stdio</arg>
         </commandline>
      </server>
   </extension>
   
   <extension point="org.eclipse.lsp4e.languageServerMapping">
      <mapping languageId="prolog"
               serverId="prolog.lsp"
               contentTypeId="org.eclipse.prolog.contentType"/>
   </extension>
   
   <extension point="org.eclipse.core.contenttype.contentTypes">
      <content-type id="org.eclipse.prolog.contentType"
                    name="Prolog"
                    base-type="org.eclipse.core.runtime.text"
                    file-extensions="pl,pro,prolog,plt,ecl"/>
   </extension>
</plugin>
      `,
    };
  }

  private static generateTheiaConfig() {
    return {
      'package.json': {
        name: 'prolog-lsp-theia',
        version: '1.0.0',
        description: 'Prolog LSP support for Theia',
        dependencies: {
          '@theia/core': 'latest',
          '@theia/languages': 'latest',
          'vscode-languageserver-protocol': 'latest',
        },
        theiaExtensions: [
          {
            frontend: 'lib/browser/prolog-frontend-module',
            backend: 'lib/node/prolog-backend-module',
          },
        ],
      },
      frontend_module: `
import { ContainerModule } from 'inversify';
import { LanguageClientContribution } from '@theia/languages/lib/browser';
import { PrologLanguageClientContribution } from './prolog-language-client-contribution';

export default new ContainerModule(bind => {
    bind(LanguageClientContribution).to(PrologLanguageClientContribution).inSingletonScope();
});
      `,
      client_contribution: `
import { injectable } from 'inversify';
import { LanguageClientContribution, ILanguageClient } from '@theia/languages/lib/browser';
import { PROLOG_LANGUAGE_ID, PROLOG_LANGUAGE_NAME } from '../common';

@injectable()
export class PrologLanguageClientContribution implements LanguageClientContribution {
    readonly id = PROLOG_LANGUAGE_ID;
    readonly name = PROLOG_LANGUAGE_NAME;

    start(languageClient: ILanguageClient): void {
        const command = 'node';
        const args = ['./out/pub/features/prologLSPServer.js', '--stdio'];
    const args = ['./out/pub/features/lsp/server.js', '--stdio'];
        
        languageClient.start({
            command,
            args,
            options: {}
        });
    }
}
      `,
    };
  }

  private static async generateSetupScripts(lspDir: string): Promise<void> {
    // Bash setup script
    const bashScript = `#!/bin/bash
# Prolog LSP Setup Script

echo "Setting up Prolog LSP for multiple IDEs..."

# Check if Node.js is installed
if ! command -v node &> /dev/null; then
    echo "Error: Node.js is required but not installed."
    echo "Please install Node.js from https://nodejs.org/"
    exit 1
fi

# Check if SWI-Prolog is installed
if ! command -v swipl &> /dev/null; then
    echo "Warning: SWI-Prolog not found in PATH."
    echo "Please install SWI-Prolog from https://www.swi-prolog.org/"
fi

echo "Available IDE configurations:"
for config in *.json; do
    if [ -f "$config" ]; then
        ide_name=$(basename "$config" .json)
        echo "  - $ide_name"
    fi
done

echo ""
echo "To use with your IDE:"
echo "1. Copy the appropriate configuration file"
echo "2. Follow the setup instructions in the documentation"
echo "3. Restart your IDE"

echo "Setup complete!"
`;

    // PowerShell setup script
    const powershellScript = `# Prolog LSP Setup Script for Windows

Write-Host "Setting up Prolog LSP for multiple IDEs..." -ForegroundColor Green

# Check if Node.js is installed
try {
    $nodeVersion = node --version
    Write-Host "Node.js found: $nodeVersion" -ForegroundColor Green
} catch {
    Write-Host "Error: Node.js is required but not installed." -ForegroundColor Red
    Write-Host "Please install Node.js from https://nodejs.org/" -ForegroundColor Yellow
    exit 1
}

# Check if SWI-Prolog is installed
try {
    $swiplVersion = swipl --version
    Write-Host "SWI-Prolog found: $swiplVersion" -ForegroundColor Green
} catch {
    Write-Host "Warning: SWI-Prolog not found in PATH." -ForegroundColor Yellow
    Write-Host "Please install SWI-Prolog from https://www.swi-prolog.org/" -ForegroundColor Yellow
}

Write-Host "Available IDE configurations:" -ForegroundColor Cyan
Get-ChildItem -Filter "*.json" | ForEach-Object {
    $ideName = $_.BaseName
    Write-Host "  - $ideName" -ForegroundColor White
}

Write-Host ""
Write-Host "To use with your IDE:" -ForegroundColor Cyan
Write-Host "1. Copy the appropriate configuration file" -ForegroundColor White
Write-Host "2. Follow the setup instructions in the documentation" -ForegroundColor White
Write-Host "3. Restart your IDE" -ForegroundColor White

Write-Host "Setup complete!" -ForegroundColor Green
`;

    fs.writeFileSync(path.join(lspDir, 'setup.sh'), bashScript);
    fs.writeFileSync(path.join(lspDir, 'setup.ps1'), powershellScript);

    // Make bash script executable on Unix systems
    try {
      fs.chmodSync(path.join(lspDir, 'setup.sh'), 0o755);
    } catch (_error) {
      // Ignore chmod errors on Windows
    }
  }

  private static async generateIDEDocumentation(lspDir: string): Promise<void> {
    const documentation = `# Prolog LSP Multi-IDE Support

This directory contains configuration files and setup instructions for using the Prolog Language Server with various IDEs and editors.

## Supported IDEs

### VS Code
- **Status**: Native support through VSCode Prolog Toolkit extension
- **Setup**: Install the extension from the marketplace
- **Configuration**: Automatic

### Vim/Neovim
- **coc.nvim**: Copy \`coc-settings.json\` to your coc configuration
- **Built-in LSP (Neovim)**: Add the Lua configuration to your init.lua
- **vim-lsp**: Add the Vim script to your .vimrc

### Emacs
- **lsp-mode**: Add the Elisp configuration to your init.el
- **Requirements**: lsp-mode, lsp-ui, company-lsp packages

### Sublime Text
- **LSP Package**: Install the LSP package and copy the configuration
- **Syntax**: Copy the syntax definition for Prolog highlighting

### IntelliJ IDEA
- **Plugin**: Install LSP4IJ plugin
- **Configuration**: Use the provided plugin configuration

### Eclipse
- **LSP4E**: Install LSP4E plugin
- **Configuration**: Use the provided plugin.xml configuration

### Theia
- **Extension**: Use the provided Theia extension configuration
- **Build**: Follow Theia extension development guidelines

## Features Supported

All IDE configurations support the following LSP features:

- ✅ **Syntax Highlighting**: Prolog-specific syntax coloring
- ✅ **Code Completion**: Intelligent autocompletion for predicates
- ✅ **Hover Information**: Documentation on hover
- ✅ **Go to Definition**: Navigate to predicate definitions
- ✅ **Find References**: Find all uses of a predicate
- ✅ **Document Symbols**: Outline view of predicates
- ✅ **Workspace Symbols**: Search predicates across workspace
- ✅ **Diagnostics**: Real-time error and warning detection
- ✅ **Code Actions**: Quick fixes and refactoring
- ✅ **Document Formatting**: Automatic code formatting
- ✅ **Signature Help**: Parameter hints for predicates
- ✅ **Rename**: Rename predicates across files
- ✅ **Folding**: Code folding for clauses and comments
- ✅ **Semantic Tokens**: Enhanced syntax highlighting

## Advanced Features

### N3/RDF Support
- Semantic web reasoning with N3 notation
- RDF triple validation and completion
- RDFS inference support

### Query Execution
- Execute Prolog queries directly from the editor
- Interactive query results
- Query history and management

### Help System
- Integrated SWI-Prolog documentation
- Predicate help on demand
- Example code snippets

## Setup Instructions

1. **Prerequisites**:
   - Node.js (v14 or later)
   - SWI-Prolog (latest version)
   - Your preferred IDE/editor

2. **Installation**:
   - Run the setup script for your platform:
     - Linux/macOS: \`./setup.sh\`
     - Windows: \`./setup.ps1\`

3. **IDE Configuration**:
   - Copy the appropriate configuration file
   - Follow IDE-specific setup instructions
   - Restart your IDE

4. **Verification**:
   - Open a .pl file
   - Check that syntax highlighting works
   - Try code completion (Ctrl+Space)
   - Hover over a predicate for documentation

## Troubleshooting

### Common Issues

1. **LSP Server not starting**:
   - Check Node.js installation
   - Verify server path in configuration
   - Check IDE error logs

2. **No syntax highlighting**:
   - Verify file extension association
   - Check syntax definition installation

3. **No completions**:
   - Ensure LSP client is connected
   - Check server initialization logs

4. **SWI-Prolog not found**:
   - Add SWI-Prolog to system PATH
   - Update executablePath in configuration

### Getting Help

- Check the main extension documentation
- Review IDE-specific LSP documentation
- Report issues on the project repository

## Contributing

To add support for additional IDEs:

1. Create configuration files following LSP standards
2. Add setup instructions
3. Test with the target IDE
4. Submit a pull request

## License

This multi-IDE support is part of the VSCode Prolog Toolkit and follows the same MIT license.
`;

    fs.writeFileSync(path.join(lspDir, 'README.md'), documentation);
  }

  /**
   * Detect available IDEs on the system
   */
  public static async detectAvailableIDEs(): Promise<string[]> {
    const available: string[] = [];

    // Check for common IDE executables
    const ideChecks = [
      { name: 'vscode', commands: ['code', 'code-insiders'] },
      { name: 'vim', commands: ['vim'] },
      { name: 'neovim', commands: ['nvim'] },
      { name: 'emacs', commands: ['emacs'] },
      { name: 'sublime', commands: ['subl', 'sublime_text'] },
      { name: 'intellij', commands: ['idea', 'intellij-idea-ultimate'] },
    ];

    for (const ide of ideChecks) {
      for (const command of ide.commands) {
        try {
          await new Promise((resolve, reject) => {
            exec(`which ${command}`, (error: unknown) => {
              if (!error) {
                available.push(ide.name);
                resolve(true);
              } else {
                reject(error);
              }
            });
          });
          break; // Found one command for this IDE
        } catch (error: unknown) {
          // Command not found, continue
        }
      }
    }

    return [...new Set(available)]; // Remove duplicates
  }

  /**
   * Generate IDE-specific launch configurations
   */
  public static generateLaunchConfigurations(workspaceRoot: string): void {
    const launchConfig = {
      version: '0.2.0',
      configurations: [
        {
          name: 'Launch Prolog LSP Server',
          type: 'node',
          request: 'launch',
          program: '${workspaceFolder}/out/pub/features/lsp/server.js',
          args: ['--stdio'],
          console: 'integratedTerminal',
          internalConsoleOptions: 'neverOpen',
          env: {
            NODE_ENV: 'development',
          },
        },
        {
          name: 'Debug Prolog LSP Server',
          type: 'node',
          request: 'launch',
          // program: '${workspaceFolder}/out/pub/features/prologLSPServer.js',
          program: '${workspaceFolder}/out/pub/features/lsp/server.js',
          args: ['--stdio'],
          console: 'integratedTerminal',
          internalConsoleOptions: 'neverOpen',
          env: {
            NODE_ENV: 'development',
          },
          sourceMaps: true,
          outFiles: ['${workspaceFolder}/out/**/*.js'],
        },
      ],
    };

    const vscodeDir = path.join(workspaceRoot, '.vscode');
    if (!fs.existsSync(vscodeDir)) {
      fs.mkdirSync(vscodeDir, { recursive: true });
    }

    fs.writeFileSync(path.join(vscodeDir, 'launch.json'), JSON.stringify(launchConfig, null, 2));
  }
}
