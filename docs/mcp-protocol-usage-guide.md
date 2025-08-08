# MCP Protocol Usage Guide

This guide explains how to interact with the Model Context Protocol (MCP) server, including request/response structure, supported tools/resources, and integration patterns for advanced Prolog and N3 reasoning workflows.

---

## Table of Contents
- [Overview](#overview)
- [Protocol Basics](#protocol-basics)
- [Request/Response Structure](#requestresponse-structure)
- [Supported Tools and Resources](#supported-tools-and-resources)
- [Example Usage](#example-usage)
- [Error Handling](#error-handling)
- [Integration Patterns](#integration-patterns)
- [References](#references)

---

## Overview

The MCP server provides a robust, JSON-based protocol for invoking Prolog, CLP, probabilistic, and N3 reasoning tools. It is designed for integration with VS Code extensions, chat interfaces, and external clients.

---

## Protocol Basics

- **Transport:** JSON over stdio (child process) or HTTP (future).
- **Schema Validation:** All requests and responses are validated using Zod schemas.
- **OpenAPI Spec:** See `openapi.yaml` for full API reference.

---

## Request/Response Structure

**Request Example:**
```json
{
  "tool": "prolog_query",
  "params": {
    "query": "member(X, [a,b,c])"
  },
  "id": "req-12345"
}
```

**Response Example:**
```json
{
  "id": "req-12345",
  "success": true,
  "result": [ { "X": "a" }, { "X": "b" }, { "X": "c" } ]
}
```

---

## Supported Tools and Resources

- **prolog_query:** Standard Prolog queries.
- **batch:** Batch execution of multiple queries.
- **clp:** Constraint Logic Programming (finite domains, reals).
- **probabilistic:** Probabilistic inference and sampling.
- **n3_reason:** N3/semantic web reasoning.
- **history:** Query/result history.
- **help:** Protocol and tool help.

See `api-reference.md` for full parameter details.

---

## Example Usage

### Batch Query
```json
{
  "tool": "batch",
  "params": {
    "queries": ["member(X,[1,2,3])", "length([a,b],X)"]
  },
  "id": "req-batch-1"
}
```

### CLP Query
```json
{
  "tool": "clp",
  "params": {
    "constraints": "X+Y#=10, X#>Y",
    "domain": "fd",
    "vars": ["X", "Y"]
  },
  "id": "req-clp-1"
}
```

### Probabilistic Query
```json
{
  "tool": "probabilistic",
  "params": {
    "query": "rain",
    "facts": "rain:0.3,sprinkler:0.5",
    "samples": 1000
  },
  "id": "req-prob-1"
}
```

---

## Error Handling

- All errors are returned with `success: false` and a descriptive `error` field.
- Example:
  ```json
  {
    "id": "req-err-1",
    "success": false,
    "error": "Invalid query syntax."
  }
  ```

---

## Integration Patterns

- **VS Code Extension:** Use Node.js child process to communicate with MCP server.
- **Chat Commands:** Route `/batch`, `/clp`, `/probabilistic`, `/n3_reason` to backend via JSON protocol.
- **External Clients:** (Planned) HTTP/REST API with same schema.

---

## References
- [OpenAPI Spec](./openapi.yaml)
- [API Reference](./api-reference.md)
- [Protocol Details](./protocol.md)
- [Chat Commands](./chat-commands.md)

---
_Last updated: 2025-08-08_
