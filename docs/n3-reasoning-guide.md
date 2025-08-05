# N3 Logic Reasoning Guide

This guide explains how to use the N3 Logic reasoning features in the VSC-Prolog extension.

## Overview

The extension now supports N3 (Notation3) logic reasoning through chat commands. N3 is a logic-based web language that extends RDF with variables, quantification, and rules for expressing logical relationships.

## Available Commands

### `/n3_load` - Load N3 Data

Load N3/Turtle files or content into the reasoning engine.

**Syntax:**
- `/n3_load <file_path>` - Load from file
- `/n3_load --content "<n3_content>"` - Load from inline content

**Examples:**
```
/n3_load sample.n3
/n3_load --content "@prefix : <http://example.org/> . :socrates a :Person ."
```

**Supported formats:**
- N3 (.n3)
- Turtle (.ttl, .turtle)

### `/n3_list` - List Loaded Triples

Display the triples currently loaded in the knowledge base.

**Syntax:**
- `/n3_list` - List with default settings (limit 10, readable format)
- `/n3_list --limit <number>` - Specify maximum number of triples to show
- `/n3_list --format <format>` - Specify output format (readable/raw)

**Examples:**
```
/n3_list
/n3_list --limit 20
/n3_list --limit 50 --format readable
```

### `/n3_reason` - Perform Reasoning

Execute N3 reasoning to infer new knowledge from loaded data and rules.

**Syntax:**
- `/n3_reason` - General reasoning (find all inferred triples)
- `/n3_reason <goal>` - Reason about a specific goal

**Examples:**
```
/n3_reason
/n3_reason rdf(X, type, Mortal)
/n3_reason rdf(socrates, type, Mortal)
```

### `/n3_explain` - Explain Reasoning

Generate a proof tree showing how a conclusion was reached.

**Syntax:**
- `/n3_explain <goal>` - Explain how the goal can be proven

**Examples:**
```
/n3_explain rdf(socrates, type, Mortal)
/n3_explain rdf(plato, hasWisdom, true)
```

## N3 Syntax Examples

### Basic Triples
```n3
@prefix : <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

:socrates a :Person .
:plato a :Person .
:aristotle a :Person .
```

### Class Hierarchies
```n3
@prefix : <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

:Person rdfs:subClassOf :Mortal .
:Philosopher rdfs:subClassOf :Person .
```

### N3 Rules
```n3
# If someone is a Person, then they are Mortal
{ ?x a :Person } => { ?x a :Mortal } .

# If someone is a Philosopher, then they have wisdom
{ ?x a :Philosopher } => { ?x :hasWisdom true } .

# Teacher-student relationship inference
{ ?teacher :teacherOf ?student . ?teacher :hasWisdom true } => { ?student :learnedFrom ?teacher } .
```

## Reasoning Features

### 1. RDFS Inference
The system automatically performs RDFS (RDF Schema) inference:
- Subclass relationships (`rdfs:subClassOf`)
- Property hierarchies (`rdfs:subPropertyOf`)
- Domain and range constraints

### 2. N3 Rule Processing
Custom N3 rules are processed to infer new triples:
- Conditional logic with `{ premise } => { conclusion }`
- Variable binding across premises and conclusions
- Complex rule chaining

### 3. Meta-Interpretation
The `/n3_explain` command uses a meta-interpreter to trace proof steps:
- Shows how facts are used
- Displays rule applications
- Reveals inference chains

## Prefix Support

The system supports readable prefixes for common namespaces:

- `:` → `http://example.org/`
- `rdf:` → `http://www.w3.org/1999/02/22-rdf-syntax-ns#`
- `rdfs:` → `http://www.w3.org/2000/01/rdf-schema#`

Output automatically uses these prefixes for better readability.

## Error Handling

The system includes comprehensive error handling:

### Input Validation
- File existence and format checking
- Content size limits (< 1MB)
- Syntax validation for N3/Turtle

### Security Features
- Input sanitization to prevent injection attacks
- Resource limits to prevent runaway processes
- Timeout controls for long-running operations

## Example Workflow

1. **Load Knowledge Base:**
   ```
   /n3_load --content "@prefix : <http://example.org/> . @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> . @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> . :socrates a :Person . :Person rdfs:subClassOf :Mortal . { ?x a :Person } => { ?x a :Mortal } ."
   ```

2. **Verify Loaded Data:**
   ```
   /n3_list --limit 10
   ```

3. **Perform Reasoning:**
   ```
   /n3_reason
   ```

4. **Explain Specific Conclusions:**
   ```
   /n3_explain rdf(socrates, type, Mortal)
   ```

## Troubleshooting

### Common Issues

**Backend Not Running:**
- Check `/status` to verify backend status
- Restart VS Code if needed

**File Not Found:**
- Ensure file paths are relative to workspace
- Check file extensions (.n3, .ttl, .turtle)

**Syntax Errors:**
- Validate N3/Turtle syntax
- Check prefix declarations
- Ensure proper triple termination with `.`

**Performance Issues:**
- Use `--limit` parameter for large datasets
- Consider breaking large files into smaller chunks
- Monitor timeout settings

### Debug Tips

1. Start with simple examples
2. Use `/n3_list` to verify data loading
3. Test reasoning incrementally
4. Check proof trees with `/n3_explain`

## Integration with Prolog

The N3 reasoning system is built on top of SWI-Prolog's semantic web libraries:
- `library(semweb/rdf11)` - RDF 1.1 support
- `library(semweb/turtle)` - Turtle/N3 parsing
- `library(semweb/rdf_db)` - RDF database operations
- `library(semweb/rdfs)` - RDFS reasoning

This provides robust, standards-compliant N3 processing with excellent performance characteristics.