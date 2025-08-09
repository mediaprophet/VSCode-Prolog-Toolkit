# VS Code Extension API Requirements for Prolog Toolkit Migration

## Overview
This document defines the required VS Code Extension APIs, patterns, and best practices for the migration and robust implementation of all features in the Prolog Toolkit. It is intended for developers and AI agents working on the migration, ensuring all extension-backend, UI, and language tooling features are implemented to production-grade standards.

---

## 1. Extension Activation & Lifecycle
- Use `activate(context: vscode.ExtensionContext)` as the main entry point.
- Register all commands, providers, and disposables in the extension context.
- Implement robust error handling and logging for activation/deactivation.
- Support both workspace and single-file activation events (e.g., `onLanguage:prolog`).

## 2. Command Registration
- Use `vscode.commands.registerCommand` for all user-facing and internal commands.
- Prefix all commands with `prolog.` (e.g., `prolog.query`, `prolog.consult`).
- Document all commands in `package.json` and provide clear descriptions.

## 3. Language Features
- Implement providers for:
  - Definition (`vscode.DefinitionProvider`)
  - Hover (`vscode.HoverProvider`)
  - Completion (`vscode.CompletionItemProvider`)
  - Formatting (`vscode.DocumentFormattingEditProvider`)
  - Linting/Diagnostics (`vscode.DiagnosticCollection`)
  - Code Actions (`vscode.CodeActionProvider`)
  - Document Symbols (`vscode.DocumentSymbolProvider`)
- Register providers in `activate` and dispose on deactivation.
- Use `vscode.languages.register*Provider` APIs.

## 4. Backend Communication
- Use Node.js `child_process.spawn` to manage the Prolog backend process.
- Communicate via JSON over stdio (see protocol in `fullimplementation.md`).
- Implement robust process management: restarts, timeouts, error handling.
- Sanitize all input/output to/from the backend.
- Use VS Code's `OutputChannel` for backend logs and errors.

## 5. Chat & Interactive Features
- Register chat commands (e.g., `/query`, `/consult`, `/help`) using VS Code's Chat API (if available) or custom webview/chat UI.
- Route chat commands to backend via JSON protocol.
- Provide Markdown-formatted responses in chat.

## 6. Webview & UI Integration
- Use `vscode.WebviewPanel` for advanced UI (e.g., settings, help, chat, N3 diagnostics).
- Sanitize all HTML/JS in webviews; use `cspSource` for security.
- Use `vscode.window.show*` APIs for notifications, quick picks, and input boxes.

## 7. Configuration & Settings
- Define all user/workspace settings in `contributes.configuration` in `package.json`.
- Use `vscode.workspace.getConfiguration` to read/update settings.
- Listen for configuration changes and update features dynamically.

## 8. Testing & Logging
- Use Mocha/Chai for automated tests (`test/` folder).
- Log all test runs to unique files (see `copilot-instructions.md`).
- Use `vscode.window.createOutputChannel` for extension logs.
- Output all errors and problems to both terminal and structured problems files.

## 9. LSP & Advanced Protocols (Planned)
- Prepare for LSP integration using `vscode-languageclient`.
- Implement LSP server as a separate process/module.
- Ensure all language features are LSP-compatible.

## 10. Error Handling & Robustness
- Catch and log all exceptions in extension code.
- Provide user-friendly error messages via VS Code notifications.
- Implement fallback and recovery for backend failures.

## 11. Packaging & Distribution
- Use `vsce` for packaging and publishing.
- Ensure all dependencies are listed in `package.json`.
- Follow VS Code Marketplace requirements for metadata, icons, and changelogs.

## 12. Security & Permissions
- Minimize extension permissions; request only what is needed.
- Sanitize all user input and backend communication.
- Follow VS Code security best practices for webviews and process management.

---

## References
- [VS Code API Reference](https://code.visualstudio.com/api)
- [Extension Manifest (package.json)](https://code.visualstudio.com/api/references/extension-manifest)
- [Testing Extensions](https://code.visualstudio.com/api/working-with-extensions/testing-extension)
- [Webview Security](https://code.visualstudio.com/api/extension-guides/webview#security)
- See `copilot-instructions.md`, `fullimplementation.md`, and `processDocs.json` for project-specific requirements.

---

## Compliance Checklist
- [ ] All commands registered and documented
- [ ] All providers implemented and tested
- [ ] Backend process robustly managed
- [ ] All user input/output sanitized
- [ ] Automated tests and logs in place
- [ ] All features covered in migration tracking
- [ ] Security best practices followed

---

This document must be updated as new APIs or features are adopted during migration. All contributors and AI agents must follow these requirements for every feature and code change.
