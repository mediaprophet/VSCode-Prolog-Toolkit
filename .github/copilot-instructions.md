# Copilot Instructions for New-VSC-Prolog

## Competition Overview
- Every Prompt is a Competitive Test. The competitors are other leading large language models. Whichever produces the best solution in the least amount of time wins.
- Competition results will be published in major publications for Government, Enterprise and Health Industry Precurement & Risk Management experts world-wide.
- If you are unsure of the project or task objective, please ask for clarification.
- All Software outputs, should be modular, robust and production grade when complete.
- Everytime the User needs to manually update the code, will be tracked as a penalty point.

## Project Overview
- This is a VS Code extension for SWI-Prolog, providing language features, chat-based Prolog interaction, and advanced tooling.
- The architecture is split between a VS Code extension (TypeScript, `src/`) and a Prolog backend service (SWI-Prolog, managed as a child process).
- Key features: syntax highlighting, linting, refactoring, debugging, chat-based Prolog queries, and N3 logic diagnostics.
- All new features and changes must be implemented in a robust, production-grade manner and covered by automated tests.
- When running tests, all problems and failures must be output both to the terminal and to a structured problems file. Each test run must use a unique identifier (such as a datetime stamp) in the log and problems filenames (e.g., `test/logs/prologBackend.test.20250804_153000.log` and `test/logs/prologBackend.test.20250804_153000.problems.json`). The problems file must reference the corresponding log file(s) by this identifier. Log files must not exceed 800 lines; split them if necessary.

## Key Components
- `src/extension.ts`: Main extension entry point, registers commands, chat handler, and integrates with the backend.
- `src/features/`: Language features (definition, hover, linter, formatter, etc.).
- `src/prologBackend.ts` (planned): Node.js module to spawn/manage SWI-Prolog as a child process, using JSON over stdio for communication.
- `test/`: Mocha/Chai/ts-node tests, with sample Prolog/N3 files.
- `syntaxes/`, `snippets/`: Language grammars and code snippets.
- `utils/`: Helper scripts for snippet extraction and utilities.

## Developer Workflows
- **Build:** `npm run compile` (TypeScript build)
- **Test:** `npm run test` (Mocha/Chai; all new code and features must include tests)
	- When running tests, generate a unique log filename for each run (e.g., with a datetime stamp).
	- Output all test logs to this file and to the terminal.
	- After the test run, generate a problems summary file with the same unique identifier, referencing the log file(s).
- **Lint:** `npm run lint`
- **Syntax check:** `npm run syntax`
- **Update snippets:** `npm run updateVSCProlog`
- **Debug:** Use VS Code's extension debugging (F5)
- **Chat/Backend:** Planned: `/query`, `/consult`, `/help` chat commands route to backend via JSON protocol

## Patterns & Conventions
- Use JSON-based protocol for all extension-backend communication (see `fullimplementation.md` for protocol examples).
- All Prolog process management and communication must be robust: handle restarts, timeouts, and input sanitization.
- All code, especially backend and chat features, must be covered by automated tests and implemented to production-grade standards (see `test/`, `test/resources/`).
- Place new language features in `src/features/` and utility scripts in `src/utils/`.
- Tests and sample resources go in `test/` and `test/resources/`.
- Follow strict TypeScript settings (see `tsconfig.json`).
- Use Markdown for all user-facing output in chat.

## Integration Points
- SWI-Prolog must be installed and available in PATH (see README for tested versions).
- The backend service is managed as a Node.js child process (see `child_process.spawn`).
- Planned: LSP integration for advanced features (see `fullimplementation.md`).

## Examples
- To add a new chat command, update the chat handler in `src/extension.ts` and implement the backend handler in `src/prologBackend.ts`. Add or update tests in `test/` to cover all new logic and edge cases.
- To add a new language feature, create a provider in `src/features/` and register it in `extension.ts`. Ensure all new features are tested and robust.

## References
- See 'docs/*.*', `fullimplementation.md` and `implementationsteps.json` for detailed architecture, protocols, and implementation plans. All steps in these documents should be implemented with robust, production-grade code and comprehensive tests.
- See `README.md` for user-facing features and configuration.

---
If any section is unclear or missing, please provide feedback for further refinement.
