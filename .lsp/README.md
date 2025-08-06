# Prolog LSP Multi-IDE Support

This directory contains configuration files and setup instructions for using the Prolog Language Server with various IDEs and editors.

## Supported IDEs

### VS Code
- **Status**: Native support through VSCode Prolog Toolkit extension
- **Setup**: Install the extension from the marketplace
- **Configuration**: Automatic

### Vim/Neovim
- **coc.nvim**: Copy `coc-settings.json` to your coc configuration
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
     - Linux/macOS: `./setup.sh`
     - Windows: `./setup.ps1`

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
