# Full Chat-Based Prolog Integration Specification

## Objective
Enable the `@prolog` chat participant in the VS Code extension to not only send commands to SWI-Prolog, but also receive, parse, and display query results, errors, and help documentation directly in the chat view, providing a seamless conversational experience.

## Current Limitation
- The extension currently sends commands to the Prolog terminal, but cannot capture or display the output in the chat view.
- There is no Language Server Protocol (LSP) client or IPC mechanism for structured communication between the extension and the Prolog process.

## Proposed Solution

### 1. Implement a Prolog Backend Service
- **Approach:** Run SWI-Prolog as a background process (child process) managed by the extension, instead of (or in addition to) the integrated terminal.
- **Communication:** Use standard input/output (stdio) or a local TCP socket for two-way communication.
- **Responsibilities:**
  - Receive queries, consult commands, and help requests from the extension.
  - Execute them in Prolog.
  - Return results, errors, and documentation in a structured format (e.g., JSON).

### 2. Extension-Backend Communication Layer
- **Node.js Child Process:**
  - Spawn SWI-Prolog as a child process with pipes for stdin and stdout.
  - Send commands as strings, receive output, and parse responses.
- **Custom Protocol:**
  - Define a simple protocol for requests and responses (e.g., JSON lines, or tagged output).
  - Example request: `{ "type": "query", "query": "member(X, [a,b])." }`
  - Example response: `{ "type": "result", "bindings": { "X": "a" } }`

### 3. Chat Handler Integration
- **/query:**
  - Send the query to the backend service.
  - Await the result, parse variable bindings, and display them in the chat.
- **/consult:**
  - Send the file path to the backend for consulting.
  - Display success or error in the chat.
- **/help:**
  - Send the predicate to the backend.
  - Display formatted documentation in the chat.
- **Error Handling:**
  - Parse error messages and display them clearly in the chat.

### 4. Output Formatting
- **Bindings:**
  - Format variable bindings as `X = a, Y = b.`
- **Booleans:**
  - Display `true.` or `false.`
- **Errors:**
  - Show error messages in a code block with clear context.
- **Help:**
  - Render documentation as markdown.

### 5. Optional: Language Server Protocol (LSP)
- For advanced features, consider implementing a Prolog LSP server that supports custom requests for queries and help.
- Use `vscode-languageclient` in the extension to communicate with the LSP.


## Implementation Steps

### 0. Project and Testing Environment Setup
- Establish Node.js and SWI-Prolog version management (e.g., .nvmrc, .swipl-version, or document requirements).
- Initialize package.json with scripts for build, test, lint, and clean.
- Configure TypeScript (tsconfig.json) for strict type checking and CommonJS compatibility.
- Set up Mocha, Chai, and ts-node for TypeScript test execution.
- Add a test:watch script for rapid feedback during development.
- Configure .gitignore and .npmignore for clean source control and packaging.
- Add pre-commit hooks for linting and formatting (e.g., with husky).
- Document all setup steps in CONTRIBUTING.md.
- Add a VS Code devcontainer or .vscode/settings.json for consistent dev experience.
- Ensure all test and resource files are in a dedicated test/resources directory.

### 1. Create Prolog Backend Service (Node.js Module)
- Implement a class that spawns SWI-Prolog as a child process using Node.js child_process.spawn.
- Set up stdio communication (stdin, stdout, stderr).
- Define a JSON-based protocol for requests (query, consult, help, N3 commands) and responses (results, errors, proof trees).
- Ensure the backend can be started, stopped, and restarted gracefully.
- Add health checks, version checks, and automatic recovery if the Prolog process crashes.
- Implement request queueing, timeouts, and robust logging/diagnostics.

### 2. Extension-Backend Communication Layer
- Use Node.js child process to manage SWI-Prolog with pipes for stdin and stdout.
- Send commands as JSON strings, receive output, and parse responses.
- Validate and sanitize all input before sending to Prolog.

### 3. Chat Handler Integration
- Update the chat handler to use the backend service for all `/query`, `/consult`, and `/help` commands.
- Parse and format responses for display in the chat.
- Add user notifications for backend status (starting, ready, error).
- Implement retry logic, cancellation support, and graceful fallback if backend is unavailable.

### 4. Output Formatting and Error Handling
- Format variable bindings, booleans, and errors for display in the chat (Markdown code blocks, tables, etc.).
- Provide clear, actionable error messages and structured error codes.
- Support localization and user customization of output.

### 5. Prolog Package Management Enhancements
- List, install, uninstall, update, and search for SWI-Prolog packs.
- Integrate all commands with the chat handler and provide user feedback.

### 6. Implement N3Logic Diagnostic Features
- Add support for /n3_load, /n3_list, /n3_reason, and /n3_explain commands using SWI-Prolog's semweb library.
- Implement a meta-interpreter for proof tracing and format proof trees for chat.

### 7. Testing and Documentation
- Add automated unit and integration tests for all chat commands and backend features.
- Test backend independently from VS Code extension.
- Mock or stub Prolog process for fast, deterministic unit tests.
- Automate test environment setup and ensure test logs are written to /logs and cleaned up between runs.
- Set up Continuous Integration (CI) for automated testing on push/PR.
- Provide and maintain sample Prolog and N3 files in a dedicated test/resources folder.
- Document all error codes, troubleshooting steps, and update README.md and changelog as features evolve.

### 8. General UI/UX and LSP Enhancements
- Evaluate and, if appropriate, implement LSP/LanguageClient support for advanced features.
- Ensure chat participant and commands are correctly registered in package.json.
- Add and maintain chat participant icon, command descriptions, and ensure markdown formatting is consistent and attractive in chat responses.


### 9. Developer Onboarding & Maintenance
- Maintain a user-facing changelog and clear versioning for all releases.
- Add architecture diagrams and a troubleshooting guide.
- Document developer onboarding steps and best practices.
- Maintain and update CONTRIBUTING.md and CODE_OF_CONDUCT.md for open source friendliness.

### 10. Automated Code Quality and Security
- Integrate ESLint and Prettier for code style and formatting.
- Run npm audit and/or CodeQL for dependency and code security.
- Add lint, format, and security checks to CI pipeline.
- Block merges on critical issues.

### 11. Extensibility and Modularity
- Document how to add new chat commands and backend handlers.
- Ensure protocol changes are versioned and documented.

### 12. Internationalization (i18n)
- Plan for localization of chat responses and error messages.
- Structure code to support future language packs.

### 13. Performance and Scalability
- For large knowledge bases or long-running queries, implement streaming or paginated responses.
- Add progress indicators or notifications for long-running tasks.

## Security and Robustness
- Sanitize all user input before sending to Prolog.
- Handle process crashes and restarts gracefully.
- Limit resource usage and prevent infinite queries.

## Summary
This approach enables full conversational interaction with Prolog in the chat view, providing immediate, formatted feedback for queries, consults, and help requests, and lays the groundwork for future LSP-based enhancements.
