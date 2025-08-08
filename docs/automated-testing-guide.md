# Automated Testing Guide

This guide describes the automated testing strategy for the VSCode Prolog Toolkit, including test structure, logging, and best practices for robust, production-grade code.

---

## Table of Contents
- [Overview](#overview)
- [Test Structure](#test-structure)
- [Running Tests](#running-tests)
- [Logging and Problems Files](#logging-and-problems-files)
- [Best Practices](#best-practices)
- [References](#references)

---

## Overview

All features and code changes must be covered by automated tests using Mocha/Chai and ts-node. Tests ensure reliability, prevent regressions, and validate integration between the extension and backend.

---

## Test Structure

- **Location:** All tests are in the `test/` directory.
- **Types:**
  - Unit tests for individual modules
  - Integration tests for extension-backend workflows
  - UI/command tests for chat and dashboard features
- **Resources:** Sample Prolog and N3 files in `test/resources/`.

---

## Running Tests

- **Command:** `npm run test`
- **Watch Mode:** `npm run test:watch`
- **Environment:** Ensure SWI-Prolog is installed and available in PATH.

---

## Logging and Problems Files

- Each test run generates a unique log file (e.g., `test/logs/prologBackend.test.20250808_153000.log`).
- A corresponding problems file (e.g., `test/logs/prologBackend.test.20250808_153000.problems.json`) summarizes all failures and references the log file.
- Log files are split if they exceed 800 lines.

---

## Best Practices

- Cover all new features and edge cases.
- Output all problems to both terminal and problems file.
- Use unique identifiers for each test run.
- Validate logs and problems files after each run.
- See `test/` for sample tests and resources.

---

## References
- [Troubleshooting Guide](./troubleshooting.md)
- [UI Integration Guide](./ui-integration-guide.md)
- [MCP Protocol Usage Guide](./mcp-protocol-usage-guide.md)

---
_Last updated: 2025-08-08_
