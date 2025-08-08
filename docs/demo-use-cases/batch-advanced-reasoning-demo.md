# Batch Query and Advanced Reasoning Demo

This guide demonstrates how to use the VSCode Prolog Toolkit's batch query and advanced reasoning features (CLP, probabilistic, N3) via chat commands and the dashboard.

---

## Table of Contents
- [Batch Query Execution](#batch-query-execution)
- [CLP (Constraint Logic Programming) Example](#clp-constraint-logic-programming-example)
- [Probabilistic Reasoning Example](#probabilistic-reasoning-example)
- [N3 Semantic Reasoning Example](#n3-semantic-reasoning-example)
- [Viewing Query History](#viewing-query-history)

---

## Batch Query Execution

**Command:** `/batch`

- **Description:** Run multiple Prolog queries in a single batch.
- **Usage:**
  ```
  /batch member(X,[1,2,3]);length([a,b],X)
  ```
- **Expected Output:**
  ```json
  {
    "results": [
      { "query": "member(X,[1,2,3])", "results": [ ... ] },
      { "query": "length([a,b],X)", "results": [ ... ] }
    ]
  }
  ```

---

## CLP (Constraint Logic Programming) Example

**Command:** `/clp`

- **Description:** Solve CLP problems with constraints and domains.
- **Usage:**
  ```
  /clp X+Y#=10, X#>Y | fd | X,Y
  ```
- **Expected Output:**
  ```json
  {
    "success": true,
    "domain": "fd",
    "solution": [ { "X": 6, "Y": 4 }, ... ]
  }
  ```

---

## Probabilistic Reasoning Example

**Command:** `/probabilistic`

- **Description:** Run probabilistic inference with facts and samples.
- **Usage:**
  ```
  /probabilistic rain | rain:0.3,sprinkler:0.5 | 1000
  ```
- **Expected Output:**
  ```json
  {
    "success": true,
    "query": "rain",
    "probability": 0.3,
    "samples": 1000
  }
  ```

---

## N3 Semantic Reasoning Example

**Command:** `/n3_reason`

- **Description:** Run N3/semantic web reasoning on loaded data.
- **Usage:**
  ```
  /n3_reason :bob :knows :alice.
  ```
- **Expected Output:**
  ```json
  {
    "success": true,
    "inferences": [ ... ]
  }
  ```

---

## Viewing Query History

- **Command:** `/history`
- **Dashboard:** Use the Query History panel in the activity bar.
- **Description:** View recent queries, results, and analytics.

---
_Last updated: 2025-08-08_
