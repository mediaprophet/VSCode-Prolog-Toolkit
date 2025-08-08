# UI Integration Guide

This guide describes how the VSCode Prolog Toolkit integrates advanced Prolog and reasoning features into the VS Code user interface, including chat commands, dashboards, and activity bar panels.

---

## Table of Contents
- [Overview](#overview)
- [Chat Command Integration](#chat-command-integration)
- [Activity Bar & Dashboard Panels](#activity-bar--dashboard-panels)
- [Batch Query and Reasoning UI](#batch-query-and-reasoning-ui)
- [Query History and Analytics](#query-history-and-analytics)
- [Error Display and Troubleshooting](#error-display-and-troubleshooting)
- [Extending the UI](#extending-the-ui)
- [References](#references)

---

## Overview

The extension exposes all major MCP server features through intuitive UI elements:
- Chat-based command palette
- Activity bar dashboards
- Query history and analytics panels
- Contextual error and result display

---

## Chat Command Integration

- **Access:** Open the chat panel (View → Chat) or use the command palette (Ctrl+Shift+P → "Prolog: Chat").
- **Supported Commands:** `/batch`, `/clp`, `/probabilistic`, `/n3_reason`, `/history`, `/help`, and more (see [Chat Commands](./chat-commands.md)).
- **Usage:** Enter commands directly; results are shown inline with rich formatting.
- **Backend:** Commands are routed to the MCP server using a robust JSON protocol.

---

## Activity Bar & Dashboard Panels

- **Query History:** View recent queries, results, and analytics in the activity bar panel.
- **Batch/Dashboard:** Run and monitor batch queries and advanced reasoning tasks from dedicated dashboard panels.
- **Navigation:** Use the activity bar icons to switch between panels.

---

## Batch Query and Reasoning UI

- **Batch Query:** Submit multiple queries at once via the chat or dashboard.
- **Advanced Reasoning:** Access CLP, probabilistic, and N3 reasoning tools from both chat and dashboard.
- **Result Display:** Results are formatted as tables, JSON, or semantic graphs where appropriate.

---

## Query History and Analytics

- **History Panel:** Browse, filter, and re-run previous queries.
- **Analytics:** View statistics on query types, success rates, and performance.
- **Export:** Download query logs and results for further analysis.

---

## Error Display and Troubleshooting

- **Inline Errors:** Errors are shown inline in chat and dashboard panels with clear messages.
- **Problems Panel:** All errors are also logged to the VS Code Problems panel.
- **Troubleshooting:** See [Troubleshooting Guide](./troubleshooting.md) for common issues.

---

## Extending the UI

- **Add New Commands:** Register new chat commands in `src/modules/chatCommandRegistry.ts` and implement handlers in `src/modules/chat-commands/`.
- **Custom Panels:** Extend dashboard panels by adding new webview UI components in `webview-ui/`.
- **Testing:** All UI features should be covered by integration tests (see `test/`).

---

## References
- [Chat Commands](./chat-commands.md)
- [Batch & Advanced Reasoning Demo](../demo-use-cases/batch-advanced-reasoning-demo.md)
- [MCP Protocol Usage Guide](./mcp-protocol-usage-guide.md)
- [Troubleshooting Guide](./troubleshooting.md)

---
_Last updated: 2025-08-08_
