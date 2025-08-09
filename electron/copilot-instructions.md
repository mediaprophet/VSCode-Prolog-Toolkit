# Copilot Instructions for Electron Migration

## Overview
This document provides instructions for GitHub Copilot and other AI agents to assist with the migration and implementation of VSCode Prolog Toolkit backend features into the Electron app structure. All work must be robust, modular, and production-grade, following the migration plans and documentation in `docs/` and `electron/buildDocs/`.

## Key Requirements
- **Modularization:** All backend logic must be split into well-defined modules under `electron/features/`, `electron/sdk/`, and `electron/utils/`.
- **API Exposure:** The Electron app must expose REST and WebSocket APIs as described in the documentation.
- **Security:** Implement authentication, RBAC, sandboxing, and auditing as per docs.
- **Testing:** All new code must be covered by automated tests. Test logs and problems files must use unique identifiers and reference each other.
- **Documentation:** Update and maintain migration status in `electron/buildDocs/` for each feature.
- **UI Integration:** The Electron web UI must provide chat, dashboard, analytics, and onboarding as described in the UI and installation guides.
- **MCP Protocol:** Implement and register all MCP tools/resources with schema validation.
- **Performance:** Support streaming, chunked processing, and progress indicators for large queries.

## Workflow
1. **Review** the relevant migration tracking file in `electron/buildDocs/` before starting work on any feature.
2. **Implement** the feature/module in the appropriate `electron/` subfolder, following the migration notes and integration points.
3. **Test** the feature with automated tests. Output logs and problems files as per the testing guide.
4. **Update** the migration tracking file with implementation notes, status, and test coverage.
5. **Document** any new APIs, endpoints, or UI components in the relevant docs.
6. **Repeat** for each feature until all migration tasks are complete.

## References
- `docs/processDocs.json` — Master list of features to migrate
- `electron/buildDocs/` — Per-feature migration status and notes
- `docs/` — Full documentation for all features and APIs
- `src/` — Legacy backend code for reference

## Best Practices
- Use strict TypeScript settings and robust error handling
- Prefer modular, testable code with clear separation of concerns
- Follow security and performance guidelines from the docs
- Keep all migration status and documentation up to date

---
_Last updated: 2025-08-09_
