# Enhanced Reasoning Features Guide

This guide explains how to use the enhanced reasoning features in the VSC-Prolog extension, including Constraint Logic Programming (CLP), Probabilistic Logic, and User-defined Logic Modules.

## Overview

The extension now supports three major enhanced reasoning paradigms:

1. **Constraint Logic Programming (CLP)** - Solve constraint satisfaction problems over finite domains, real numbers, and rational numbers
2. **Probabilistic Logic** - Handle uncertain knowledge and perform probabilistic inference
3. **User-defined Logic Modules** - Create custom reasoning systems with specialized rules and meta-interpreters

These features extend the existing N3/Turtle reasoning capabilities to provide a comprehensive logic programming environment.

## Constraint Logic Programming (CLP)

CLP extends Prolog with constraint solving capabilities over different domains.

### Supported Domains

- **CLP(FD)** - Finite Domain constraints over integers
- **CLP(R)** - Real number constraints
- **CLP(Q)** - Rational number constraints

### CLP Commands

#### `/clp_solve` - Solve Constraint Problems

Solve constraint satisfaction problems by specifying variables, constraints, and the domain.

**Syntax:**
```json
{
  "cmd": "clp_solve",
  "domain": "fd|r|q",
  "variables": ["X", "Y", "Z"],
  "constraints": ["constraint1", "constraint2", ...]
}
```

**Examples:**

1. **Finite Domain Example:**
```json
{
  "cmd": "clp_solve",
  "domain": "fd",
  "variables": ["X", "Y"],
  "constraints": ["X #= Y + 1", "X #< 10", "Y #> 0"]
}
```

2. **All Different Constraint:**
```json
{
  "cmd": "clp_solve",
  "domain": "fd",
  "variables": ["A", "B", "C"],
  "constraints": ["all_different([A, B, C])", "A in 1..3", "B in 1..3", "C in 1..3"]
}
```

3. **Real Number Constraints:**
```json
{
  "cmd": "clp_solve",
  "domain": "r",
  "variables": ["X", "Y"],
  "constraints": ["X =:= Y * 2.5", "Y >= 1.0", "X =< 10.0"]
}
```

#### `/clp_constraint` - Add Persistent Constraints

Add constraints that persist across multiple solve operations.

**Syntax:**
```json
{
  "cmd": "clp_constraint",
  "domain": "fd|r|q",
  "constraint": "constraint_expression"
}
```

**Example:**
```json
{
  "cmd": "clp_constraint",
  "domain": "fd",
  "constraint": "X #> 0"
}
```

### CLP(FD) Constraint Types

- **Arithmetic:** `X #= Y + 1`, `X #\= Y`, `X #< Y`, `X #> Y`, `X #=< Y`, `X #>= Y`
- **Domain:** `X in 1..10`, `X in [1,3,5,7]`
- **Global:** `all_different([X, Y, Z])`, `sum([X, Y, Z], #=, 10)`

### CLP(R) and CLP(Q) Constraint Types

- **Arithmetic:** `X =:= Y + 1.5`, `X =\= Y`, `X < Y`, `X > Y`, `X =< Y`, `X >= Y`
- **Linear:** `2*X + 3*Y =:= 10`

## Probabilistic Logic

Handle uncertain knowledge and perform probabilistic inference using Monte Carlo sampling.

### Probabilistic Commands

#### `/probabilistic_fact` - Add Probabilistic Facts

Define facts with associated probabilities.

**Syntax:**
```json
{
  "cmd": "probabilistic_fact",
  "fact": "predicate(args)",
  "probability": 0.0-1.0
}
```

**Examples:**
```json
{
  "cmd": "probabilistic_fact",
  "fact": "weather(sunny)",
  "probability": 0.7
}
```

```json
{
  "cmd": "probabilistic_fact",
  "fact": "likes(john, pizza)",
  "probability": 0.8
}
```

#### `/probabilistic_query` - Probabilistic Inference

Perform probabilistic inference to estimate the probability of a goal.

**Syntax:**
```json
{
  "cmd": "probabilistic_query",
  "goal": "predicate(args)",
  "method": "monte_carlo",
  "samples": 1000
}
```

**Examples:**
```json
{
  "cmd": "probabilistic_query",
  "goal": "weather(sunny)",
  "method": "monte_carlo",
  "samples": 1000
}
```

**Response Format:**
```json
{
  "status": "ok",
  "goal": "weather(sunny)",
  "probability": 0.703,
  "evidence": {
    "success_count": 703,
    "total_samples": 1000
  },
  "method": "monte_carlo",
  "samples": 1000
}
```

### Probabilistic Reasoning Features

1. **Monte Carlo Sampling** - Estimate probabilities through random sampling
2. **Evidence Collection** - Track success rates and sample counts
3. **Uncertainty Quantification** - Handle uncertain knowledge systematically

## User-defined Logic Modules

Create custom reasoning systems with specialized rules and meta-interpreters.

### Logic Module Commands

#### `/logic_module_register` - Register Logic Module

Create a new logic module with custom rules and reasoning behavior.

**Syntax:**
```json
{
  "cmd": "logic_module_register",
  "name": "module_name",
  "rules": ["rule1", "rule2", ...],
  "meta_interpreter": "default|custom"
}
```

**Examples:**

1. **Philosophy Module:**
```json
{
  "cmd": "logic_module_register",
  "name": "philosophy",
  "rules": [
    "mortal(X) :- human(X)",
    "human(socrates)",
    "human(plato)",
    "wise(X) :- philosopher(X)",
    "philosopher(socrates)",
    "philosopher(plato)"
  ],
  "meta_interpreter": "default"
}
```

2. **Animal Classification:**
```json
{
  "cmd": "logic_module_register",
  "name": "animals",
  "rules": [
    "animal(X) :- mammal(X)",
    "animal(X) :- bird(X)",
    "mammal(dog)",
    "mammal(cat)",
    "bird(eagle)",
    "bird(sparrow)"
  ],
  "meta_interpreter": "default"
}
```

#### `/logic_module_query` - Query Logic Module

Execute queries against a registered logic module.

**Syntax:**
```json
{
  "cmd": "logic_module_query",
  "module": "module_name",
  "goal": "predicate(args)"
}
```

**Examples:**
```json
{
  "cmd": "logic_module_query",
  "module": "philosophy",
  "goal": "mortal(X)"
}
```

**Response Format:**
```json
{
  "status": "ok",
  "module": "philosophy",
  "goal": "mortal(X)",
  "results": ["mortal(socrates)", "mortal(plato)"],
  "count": 2,
  "proof_trace": [
    {"type": "proof", "rule_application": "..."},
    {"type": "proof", "rule_application": "..."}
  ]
}
```

#### `/logic_module_list` - List Logic Modules

Get information about all registered logic modules.

**Syntax:**
```json
{
  "cmd": "logic_module_list"
}
```

**Response Format:**
```json
{
  "status": "ok",
  "modules": [
    {
      "name": "philosophy",
      "rules_count": 6,
      "meta_interpreter": "default"
    },
    {
      "name": "animals",
      "rules_count": 6,
      "meta_interpreter": "default"
    }
  ],
  "count": 2
}
```

### Meta-Interpreters

Logic modules support different meta-interpreters:

1. **Default Meta-Interpreter** - Standard Prolog-style resolution with proof tracking
2. **Custom Meta-Interpreter** - Specialized reasoning behavior for domain-specific logic

## Integration Examples

### CLP with Probabilistic Reasoning

Combine constraint solving with probabilistic inference:

```json
// First, add probabilistic constraints
{
  "cmd": "probabilistic_fact",
  "fact": "constraint_satisfied(X)",
  "probability": 0.8
}

// Then solve CLP problem
{
  "cmd": "clp_solve",
  "domain": "fd",
  "variables": ["X"],
  "constraints": ["X in 1..10", "X #> 5"]
}

// Query probability of constraint satisfaction
{
  "cmd": "probabilistic_query",
  "goal": "constraint_satisfied(7)",
  "method": "monte_carlo",
  "samples": 1000
}
```

### Logic Modules with N3 Reasoning

Create logic modules that work with semantic web data:

```json
{
  "cmd": "logic_module_register",
  "name": "semantic_reasoning",
  "rules": [
    "semantic_relation(X, Y) :- rdf(X, 'http://example.org/related_to', Y)",
    "inferred_fact(X) :- semantic_relation(X, _)",
    "transitive_relation(X, Z) :- semantic_relation(X, Y), semantic_relation(Y, Z)"
  ],
  "meta_interpreter": "default"
}
```

## Error Handling

### Common Error Types

1. **Invalid Domain** - Unsupported CLP domain
2. **Malformed Constraints** - Syntax errors in constraint expressions
3. **Invalid Probability** - Probability values outside [0,1] range
4. **Module Not Found** - Querying non-existent logic module
5. **Rule Syntax Error** - Malformed Prolog rules

### Error Response Format

```json
{
  "status": "error",
  "error": "error_code",
  "message": "Detailed error description"
}
```

## Performance Considerations

### CLP Performance

- **Domain Size** - Smaller domains solve faster
- **Constraint Complexity** - Linear constraints are more efficient than non-linear
- **Variable Count** - Exponential complexity with variable count

### Probabilistic Logic Performance

- **Sample Count** - More samples give better accuracy but take longer
- **Goal Complexity** - Simple goals evaluate faster
- **Fact Count** - Large probabilistic knowledge bases slow down inference

### Logic Module Performance

- **Rule Count** - More rules increase query time
- **Rule Complexity** - Recursive rules can be expensive
- **Meta-Interpreter** - Custom interpreters may have overhead

## Best Practices

### CLP Best Practices

1. **Start Simple** - Begin with basic constraints and add complexity gradually
2. **Domain Selection** - Choose the most restrictive domain that fits your problem
3. **Constraint Ordering** - Place most restrictive constraints first
4. **Variable Domains** - Specify variable domains explicitly when possible

### Probabilistic Logic Best Practices

1. **Probability Validation** - Always validate probability values are in [0,1]
2. **Sample Size** - Use at least 1000 samples for reliable estimates
3. **Independence Assumptions** - Be aware of independence assumptions in Monte Carlo sampling
4. **Evidence Tracking** - Monitor success rates to validate model assumptions

### Logic Module Best Practices

1. **Module Organization** - Group related rules into coherent modules
2. **Rule Documentation** - Document complex rules with comments
3. **Testing** - Test modules with known examples before deployment
4. **Meta-Interpreter Selection** - Use default interpreter unless custom behavior is needed

## Troubleshooting

### CLP Issues

**Problem:** Constraints have no solution
- **Solution:** Check constraint consistency, relax overly restrictive constraints

**Problem:** Slow constraint solving
- **Solution:** Reduce domain size, simplify constraints, use more specific variable domains

### Probabilistic Logic Issues

**Problem:** Unexpected probability estimates
- **Solution:** Increase sample count, check probabilistic fact definitions, verify goal syntax

**Problem:** Slow probabilistic inference
- **Solution:** Reduce sample count, simplify goals, optimize probabilistic fact structure

### Logic Module Issues

**Problem:** Module registration fails
- **Solution:** Check rule syntax, validate module name, ensure rules are well-formed

**Problem:** Query returns no results
- **Solution:** Verify module exists, check goal syntax, ensure rules can derive the goal

## Integration with Existing Features

The enhanced reasoning features integrate seamlessly with existing extension capabilities:

- **N3/Turtle Reasoning** - Logic modules can work with RDF data
- **Session Management** - Enhanced reasoning state is preserved across sessions
- **Streaming/Pagination** - Large result sets are handled efficiently
- **WebSocket Notifications** - Real-time updates for long-running reasoning tasks
- **LSP Integration** - Enhanced reasoning features available through language server

## API Reference

### Request Format

All enhanced reasoning commands follow the standard JSON request format:

```json
{
  "cmd": "command_name",
  "id": "unique_request_id",
  "parameter1": "value1",
  "parameter2": "value2"
}
```

### Response Format

Standard response format with enhanced reasoning-specific fields:

```json
{
  "id": "unique_request_id",
  "status": "ok|error",
  "result_field1": "value1",
  "result_field2": "value2"
}
```

### Command Summary

| Command | Purpose | Key Parameters |
|---------|---------|----------------|
| `clp_solve` | Solve constraint problems | `domain`, `variables`, `constraints` |
| `clp_constraint` | Add persistent constraints | `domain`, `constraint` |
| `probabilistic_fact` | Add probabilistic facts | `fact`, `probability` |
| `probabilistic_query` | Probabilistic inference | `goal`, `method`, `samples` |
| `logic_module_register` | Register logic module | `name`, `rules`, `meta_interpreter` |
| `logic_module_query` | Query logic module | `module`, `goal` |
| `logic_module_list` | List logic modules | None |

This comprehensive enhanced reasoning system provides powerful tools for constraint solving, uncertainty handling, and custom logic development within the VSC-Prolog extension.