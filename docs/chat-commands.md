# Prolog Toolkit Chat Commands Reference

This document lists all available chat commands in the VSCode Prolog Toolkit extension, with usage examples, argument formats, and expected outputs.

---

## Table of Contents
- [General Usage](#general-usage)
- [Available Chat Commands](#available-chat-commands)
  - [/query](#query)
  - [/clp](#clp)
  - [/probabilistic](#probabilistic)
  - [/batch](#batch)
  - [/history](#history)
  - [Other Commands](#other-commands)
- [Examples](#examples)
- [Troubleshooting](#troubleshooting)

---

## General Usage

- Open the chat panel in VS Code (Prolog Toolkit Activity Bar → Chat tab).
- Type a command (starting with `/`) and press Enter.
- Arguments are separated by `|` for multi-argument commands.

---

## Available Chat Commands

### /query
- **Description:** Run a Prolog query in the backend.
- **Usage:** `/query member(X, [1,2,3])`
- **Arguments:**
  - `goal` (string): The Prolog goal to execute.

### /clp
- **Description:** Solve a CLP (Constraint Logic Programming) problem.
- **Usage:** `/clp X+Y#=10, X#>Y | fd | X,Y`
- **Arguments:**
  - `constraints` (string, comma-separated): CLP constraints.
  - `domain` (string, optional): Constraint domain (`fd`, `r`, `q`).
  - `variables` (string, comma-separated, optional): Variables to solve for.

### /probabilistic
- **Description:** Run probabilistic inference.
- **Usage:** `/probabilistic rain | rain:0.3,sprinkler:0.5 | 1000`
- **Arguments:**
  - `query` (string): Query for probabilistic inference.
  - `facts` (string, comma-separated, optional): Probabilistic facts (`fact:probability`).
  - `samples` (number, optional): Number of Monte Carlo samples.

### /batch
- **Description:** Run a batch of Prolog queries.
- **Usage:** `/batch member(X,[1,2,3]);length([a,b],X)`
- **Arguments:**
  - `queries` (string, semicolon-separated): Batch queries.

### /history
- **Description:** Show recent Prolog query history.
- **Usage:** `/history`
- **Arguments:** None

### Other Commands
- `/help` — Show help and available commands.
- `/status` — Show backend status.
- `/n3_load`, `/n3_reason`, etc. — N3/semantic web logic commands.

---

## Examples

- **CLP Example:**
  - `/clp X+Y#=10, X#>Y | fd | X,Y`
- **Probabilistic Example:**
  - `/probabilistic rain | rain:0.3,sprinkler:0.5 | 1000`
- **Batch Example:**
  - `/batch member(X,[1,2,3]);length([a,b],X)`
- **History Example:**
  - `/history`

---

## Troubleshooting
- If a command returns an error, check your argument formatting.
- Ensure the Prolog backend is running (see `/status`).
- For more help, use `/help` or consult the [Enhanced Reasoning Guide](./enhanced-reasoning-guide.md).

---
_Last updated: 2025-08-08_
