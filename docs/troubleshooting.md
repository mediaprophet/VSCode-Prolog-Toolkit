# Troubleshooting Guide

This guide helps you diagnose and resolve common issues with the New-VSC-Prolog extension.

## Table of Contents

- [Installation Issues](#installation-issues)
- [Backend Connection Problems](#backend-connection-problems)
- [Chat Commands Not Working](#chat-commands-not-working)
- [N3 Logic Issues](#n3-logic-issues)
- [Performance Problems](#performance-problems)
- [Error Codes Reference](#error-codes-reference)
- [Debugging Steps](#debugging-steps)
- [Getting Help](#getting-help)

## Installation Issues

### SWI-Prolog Not Found

**Symptoms:**
- Extension fails to start
- Error: "SWI-Prolog executable not found"
- Chat commands return "Backend not available"

**Solutions:**

1. **Install SWI-Prolog:**
   ```bash
   # Ubuntu/Debian
   sudo apt-get install swi-prolog
   
   # macOS
   brew install swi-prolog
   
   # Windows
   choco install swi-prolog
   ```

2. **Configure executable path:**
   - Open VS Code Settings (Ctrl+,)
   - Search for "prolog.executablePath"
   - Set the correct path to your SWI-Prolog installation:
     - Windows: `C:\Program Files\swipl\bin\swipl.exe`
     - macOS: `/usr/local/bin/swipl`
     - Linux: `/usr/bin/swipl`

3. **Verify installation:**
   ```bash
   swipl --version
   ```

### Extension Won't Activate

**Symptoms:**
- Extension appears installed but inactive
- No chat participant available
- Commands not registered

**Solutions:**

1. **Check VS Code version:**
   - Ensure VS Code version 1.86 or later
   - Update VS Code if necessary

2. **Reload window:**
   - Press Ctrl+Shift+P
   - Run "Developer: Reload Window"

3. **Check extension logs:**
   - Open Output panel (View → Output)
   - Select "New-VSC-Prolog" from dropdown
   - Look for error messages

## Backend Connection Problems

### Backend Startup Failures

**Error Code:** `BACKEND_START_FAILED`

**Symptoms:**
- Chat commands timeout
- Status shows "Backend not running"
- Extension logs show startup errors

**Solutions:**

1. **Check port availability:**
   ```bash
   # Check if port 3060 is in use
   netstat -an | grep 3060
   ```

2. **Configure different port:**
   - Add to VS Code settings:
   ```json
   {
     "prolog.backend.port": 3061
   }
   ```

3. **Check firewall settings:**
   - Ensure localhost connections are allowed
   - Add exception for SWI-Prolog if needed

### Connection Timeouts

**Error Code:** `CONNECTION_TIMEOUT`

**Symptoms:**
- Commands hang for 30+ seconds
- Intermittent "Request timeout" errors

**Solutions:**

1. **Increase timeout:**
   ```json
   {
     "prolog.backend.timeout": 15000
   }
   ```

2. **Check system resources:**
   - Monitor CPU and memory usage
   - Close unnecessary applications

3. **Restart backend:**
   - Use `/status` command in chat
   - If needed, reload VS Code window

## Chat Commands Not Working

### Query Command Issues

**Error Code:** `QUERY_EXECUTION_FAILED`

**Common Problems:**

1. **Syntax Errors:**
   ```prolog
   % Wrong
   @prolog /query member(X [1,2,3])
   
   % Correct
   @prolog /query member(X, [1,2,3])
   ```

2. **Undefined Predicates:**
   ```prolog
   % Load file first
   @prolog /consult myfile.pl
   @prolog /query my_predicate(X)
   ```

3. **Variable Scope:**
   ```prolog
   % Variables are query-scoped
   @prolog /query X = 5
   @prolog /query Y is X + 1  % X is undefined here
   ```

### Consult Command Issues

**Error Code:** `CONSULT_FAILED`

**Solutions:**

1. **Check file paths:**
   - Use absolute paths or paths relative to workspace
   - Ensure file exists and is readable

2. **File encoding:**
   - Ensure files are UTF-8 encoded
   - Check for BOM (Byte Order Mark) issues

3. **Syntax validation:**
   - Test file in SWI-Prolog directly
   - Fix syntax errors before consulting

### Help Command Issues

**Error Code:** `HELP_NOT_FOUND`

**Solutions:**

1. **Correct predicate format:**
   ```prolog
   % Correct formats
   @prolog /help member/2
   @prolog /help append/3
   ```

2. **Load documentation:**
   ```prolog
   % For user predicates, consult file first
   @prolog /consult myfile.pl
   @prolog /help my_predicate/2
   ```

## N3 Logic Issues

### N3 Loading Problems

**Error Code:** `N3_PARSE_ERROR`

**Common Issues:**

1. **Invalid syntax:**
   ```turtle
   # Wrong
   :socrates a Person .
   
   # Correct
   :socrates a :Person .
   ```

2. **Missing prefixes:**
   ```turtle
   # Add required prefixes
   @prefix : <http://example.org/> .
   @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
   ```

3. **File encoding:**
   - Ensure N3/Turtle files are UTF-8
   - Check for special characters

### Reasoning Issues

**Error Code:** `N3_REASONING_FAILED`

**Solutions:**

1. **Check rule syntax:**
   ```turtle
   # Correct rule format
   { ?x a :Person } => { ?x a :Mortal } .
   ```

2. **Verify data consistency:**
   - Check for contradictory statements
   - Ensure proper class hierarchies

3. **Memory limits:**
   - Large knowledge bases may exceed memory
   - Consider splitting into smaller files

## Performance Problems

### Slow Query Execution

**Symptoms:**
- Queries take >10 seconds
- System becomes unresponsive
- High CPU usage

**Solutions:**

1. **Optimize queries:**
   ```prolog
   % Use cuts to prevent backtracking
   factorial(0, 1) :- !.
   factorial(N, F) :- N > 0, ...
   ```

2. **Set time limits:**
   ```prolog
   @prolog /query call_with_time_limit(5, my_query(X))
   ```

3. **Index predicates:**
   ```prolog
   :- dynamic fact/2.
   :- index(fact(1, 0)).  % Index on first argument
   ```

### Memory Issues

**Error Code:** `OUT_OF_MEMORY`

**Solutions:**

1. **Increase stack limits:**
   ```json
   {
     "prolog.backend.args": ["--stack-limit=2g"]
   }
   ```

2. **Optimize data structures:**
   - Use difference lists for large lists
   - Avoid creating large intermediate structures

3. **Garbage collection:**
   ```prolog
   @prolog /query garbage_collect
   ```

## Error Codes Reference

### Backend Errors

| Code | Description | Severity | Action |
|------|-------------|----------|---------|
| `BACKEND_START_FAILED` | Backend process failed to start | Critical | Check SWI-Prolog installation |
| `BACKEND_CRASHED` | Backend process crashed unexpectedly | Critical | Check logs, restart extension |
| `CONNECTION_TIMEOUT` | Request timed out | Warning | Increase timeout, check query |
| `PORT_IN_USE` | Configured port is already in use | Error | Change port configuration |

### Query Errors

| Code | Description | Severity | Action |
|------|-------------|----------|---------|
| `SYNTAX_ERROR` | Invalid Prolog syntax | Error | Fix query syntax |
| `EXISTENCE_ERROR` | Predicate or file not found | Error | Check spelling, load required files |
| `TYPE_ERROR` | Wrong argument type | Error | Check argument types |
| `PERMISSION_ERROR` | Operation not permitted | Error | Check file permissions |

### N3 Logic Errors

| Code | Description | Severity | Action |
|------|-------------|----------|---------|
| `N3_PARSE_ERROR` | Invalid N3/Turtle syntax | Error | Fix N3 syntax |
| `N3_REASONING_FAILED` | Reasoning engine error | Error | Check rules and data |
| `N3_MEMORY_EXCEEDED` | Out of memory during reasoning | Warning | Reduce data size |

### System Errors

| Code | Description | Severity | Action |
|------|-------------|----------|---------|
| `OUT_OF_MEMORY` | System out of memory | Critical | Increase memory, optimize queries |
| `FILE_NOT_FOUND` | Required file missing | Error | Check file paths |
| `PERMISSION_DENIED` | File access denied | Error | Check file permissions |

## Debugging Steps

### Enable Debug Logging

1. **VS Code Settings:**
   ```json
   {
     "prolog.debug.enabled": true,
     "prolog.debug.level": "verbose"
   }
   ```

2. **Check logs:**
   - Output panel → "New-VSC-Prolog"
   - Test logs in `test/logs/` directory

### Collect Diagnostic Information

1. **System information:**
   ```bash
   # OS version
   uname -a  # Linux/macOS
   systeminfo  # Windows
   
   # SWI-Prolog version
   swipl --version
   
   # Node.js version
   node --version
   ```

2. **Extension information:**
   - VS Code version
   - Extension version
   - Configuration settings

### Test Backend Independently

1. **Manual backend test:**
   ```bash
   cd test
   node ../src/prologBackend.js
   ```

2. **HTTP endpoint test:**
   ```bash
   curl -X POST http://localhost:3060/query \
     -H "Content-Type: application/json" \
     -d '{"goal": "member(X, [1,2,3])"}'
   ```

## Getting Help

### Before Reporting Issues

1. **Check this troubleshooting guide**
2. **Search existing issues:** [GitHub Issues](https://github.com/AmauryRabouan/new-vsc-prolog/issues)
3. **Try with minimal example**
4. **Collect diagnostic information**

### Reporting Bugs

Include the following information:

1. **Environment:**
   - OS and version
   - VS Code version
   - Extension version
   - SWI-Prolog version

2. **Steps to reproduce:**
   - Exact commands used
   - Expected vs actual behavior
   - Error messages

3. **Logs:**
   - Extension output logs
   - Test logs (if applicable)
   - Backend logs

4. **Configuration:**
   - Relevant VS Code settings
   - Custom configuration files

### Community Support

- **GitHub Discussions:** General questions and discussions
- **Stack Overflow:** Tag questions with `swi-prolog` and `vscode`
- **SWI-Prolog Community:** For Prolog-specific questions

### Professional Support

For enterprise users requiring professional support, contact the maintainers through the GitHub repository.

## Frequently Asked Questions

### Q: Why is the chat participant not showing up?

**A:** Ensure you have VS Code 1.86+ and the extension is properly activated. Try reloading the window.

### Q: Can I use ECLiPSe instead of SWI-Prolog?

**A:** This extension is specifically designed for SWI-Prolog. ECLiPSe support has been discontinued.

### Q: How do I increase memory limits for large knowledge bases?

**A:** Add `"prolog.backend.args": ["--stack-limit=2g"]` to your VS Code settings.

### Q: Why are N3 reasoning results inconsistent?

**A:** Check for contradictory rules or data. N3 reasoning is sensitive to rule order and data consistency.

### Q: How do I backup my Prolog workspace?

**A:** Include `.pl`, `.n3`, and `.ttl` files in your version control. Configuration is stored in VS Code settings.