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

The extension now includes **intelligent installation detection and setup assistance** to help resolve installation issues automatically.

### SWI-Prolog Not Found

**Symptoms:**
- Extension fails to start
- Error: "SWI-Prolog executable not found"
- Chat commands return "Backend not available"
- Installation warning dialog appears on extension activation

**Automatic Solutions:**

1. **Use the Setup Wizard (Recommended):**
   - Press `Ctrl+Shift+P` and run `Prolog: Setup Wizard`
   - The wizard will automatically detect existing SWI-Prolog installations
   - Follow the guided setup process for your platform
   - Automatic path validation ensures proper configuration

2. **Automatic Installation Detection:**
   - The extension automatically scans common installation paths
   - Supports Windows, macOS, and Linux detection
   - Provides platform-specific installation guidance if SWI-Prolog is not found

3. **Interactive Installation Guide:**
   - Click "Install SWI-Prolog" when prompted
   - View platform-specific installation instructions in an interactive webview
   - Step-by-step guidance with download links and commands

**Manual Solutions:**

1. **Install SWI-Prolog:**
   ```bash
   # Ubuntu/Debian
   sudo apt-get install swi-prolog
   
   # macOS (Homebrew)
   brew install swi-prolog
   
   # macOS (MacPorts)
   sudo port install swi-prolog
   
   # Windows (Chocolatey)
   choco install swi-prolog
   
   # Windows (Scoop)
   scoop install swi-prolog
   ```

2. **Configure executable path:**
   - Open VS Code Settings (Ctrl+,)
   - Search for "prolog.executablePath"
   - Set the correct path to your SWI-Prolog installation:
     - **Windows:** `C:\Program Files\swipl\bin\swipl.exe` or `C:\swipl\bin\swipl.exe`
     - **macOS:** `/usr/local/bin/swipl`, `/opt/homebrew/bin/swipl`, or `/opt/local/bin/swipl`
     - **Linux:** `/usr/bin/swipl`, `/usr/local/bin/swipl`, or `/snap/bin/swipl`

3. **Verify installation:**
   ```bash
   swipl --version
   ```

### Configuration Migration Issues

**Symptoms:**
- Extension detects outdated SWI-Prolog paths
- Warning about invalid configuration
- Migration dialog appears on startup

**Automatic Solutions:**

1. **Automatic Migration:**
   - The extension automatically detects outdated or invalid paths
   - Offers to migrate to valid SWI-Prolog installations
   - Creates configuration backups before making changes

2. **Configuration Backup and Restore:**
   - All configuration changes are automatically backed up
   - Use "Undo Migration" option if automatic changes cause issues
   - Manual restore available through extension settings

**Manual Solutions:**

1. **Check Installation Status:**
   - Open the Prolog settings panel in VS Code
   - View current installation status and detected issues
   - Use "Test Installation" button to validate configuration

2. **Manual Path Update:**
   - Update `prolog.executablePath` in VS Code settings
   - The extension will validate the new path automatically
   - Error messages provide specific guidance for path issues

### Installation Path Detection

**Common Installation Paths:**

**Windows:**
- `C:\Program Files\swipl\bin\swipl.exe`
- `C:\swipl\bin\swipl.exe`
- `C:\Program Files (x86)\swipl\bin\swipl.exe`
- `%LOCALAPPDATA%\Programs\swipl\bin\swipl.exe`

**macOS:**
- `/usr/local/bin/swipl` (Homebrew Intel)
- `/opt/homebrew/bin/swipl` (Homebrew Apple Silicon)
- `/opt/local/bin/swipl` (MacPorts)
- `/Applications/SWI-Prolog.app/Contents/MacOS/swipl`

**Linux:**
- `/usr/bin/swipl` (System package)
- `/usr/local/bin/swipl` (Manual installation)
- `/snap/bin/swipl` (Snap package)
- `/opt/swipl/bin/swipl` (Custom installation)

### Installation Validation

**Automatic Validation:**
- The extension automatically validates SWI-Prolog installations
- Checks executable permissions and version compatibility
- Provides detailed diagnostic information for troubleshooting

**Manual Validation:**
1. **Test executable directly:**
   ```bash
   # Test if SWI-Prolog is accessible
   swipl --version
   
   # Test basic functionality
   echo "halt." | swipl -q
   ```

2. **Check PATH environment:**
   ```bash
   # Windows
   where swipl
   
   # macOS/Linux
   which swipl
   ```

3. **Verify permissions:**
   ```bash
   # Check if executable has proper permissions
   ls -la $(which swipl)
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

### Setup Wizard Issues

**Symptoms:**
- Setup wizard fails to detect SWI-Prolog
- Wizard shows "No installations found"
- Auto-detection not working properly

**Solutions:**

1. **Manual path specification:**
   - In the setup wizard, choose "Manual Configuration"
   - Browse to your SWI-Prolog installation directory
   - The wizard will validate the selected path

2. **Refresh detection:**
   - Click "Refresh" in the setup wizard
   - The wizard will re-scan common installation paths
   - Try after installing SWI-Prolog if not already installed

3. **Platform-specific issues:**
   - **Windows:** Ensure SWI-Prolog is installed in standard locations
   - **macOS:** Check both Intel and Apple Silicon Homebrew paths
   - **Linux:** Verify package manager installation completed successfully

### Installation Guide Dialog Issues

**Symptoms:**
- Installation guide doesn't open
- Webview shows blank content
- Download links not working

**Solutions:**

1. **Enable webview content:**
   - Check VS Code security settings
   - Ensure webviews are enabled
   - Try disabling extensions that might interfere with webviews

2. **Network connectivity:**
   - Ensure internet connection for download links
   - Check firewall settings for VS Code
   - Try accessing SWI-Prolog website directly

3. **Webview refresh:**
   - Close and reopen the installation guide
   - Use "Developer: Reload Window" if needed
   - Check browser console for JavaScript errors

### Configuration Migration Problems

**Symptoms:**
- Migration fails with permission errors
- Backup creation unsuccessful
- Settings not updating after migration

**Solutions:**

1. **Permission issues:**
   - Ensure VS Code has write permissions to settings
   - Run VS Code as administrator if necessary (Windows)
   - Check file permissions in VS Code settings directory

2. **Backup failures:**
   - Check available disk space
   - Verify VS Code extension storage permissions
   - Manual backup: copy current settings before migration

3. **Settings sync conflicts:**
   - Disable VS Code settings sync temporarily during migration
   - Manually resolve conflicts if settings sync is enabled
   - Re-enable sync after successful migration

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

### Installation Errors

| Code | Description | Severity | Action |
|------|-------------|----------|---------|
| `INSTALLATION_NOT_FOUND` | SWI-Prolog installation not detected | Critical | Run Setup Wizard or install SWI-Prolog |
| `INSTALLATION_INVALID` | SWI-Prolog installation is invalid or corrupted | Error | Reinstall SWI-Prolog or update path |
| `PATH_VALIDATION_FAILED` | Configured executable path is invalid | Error | Update prolog.executablePath setting |
| `VERSION_INCOMPATIBLE` | SWI-Prolog version is not supported | Warning | Update to SWI-Prolog 9.0.4+ |
| `MIGRATION_FAILED` | Configuration migration unsuccessful | Warning | Manual configuration required |
| `BACKUP_CREATION_FAILED` | Failed to create configuration backup | Warning | Check disk space and permissions |

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

### Q: The Setup Wizard can't find my SWI-Prolog installation. What should I do?

**A:** Try these steps:
1. Ensure SWI-Prolog is properly installed and accessible from the command line (`swipl --version`)
2. Use "Manual Configuration" in the Setup Wizard to browse to your installation
3. Check common installation paths for your platform (see Installation Path Detection section)
4. Verify file permissions and that the executable is not corrupted

### Q: Why does the extension keep asking me to install SWI-Prolog when it's already installed?

**A:** This usually indicates a path configuration issue:
1. Check that `prolog.executablePath` points to the correct SWI-Prolog executable
2. Verify the executable has proper permissions and is not corrupted
3. Run the Setup Wizard to automatically detect and configure the correct path
4. Check the installation status in the Prolog settings panel for detailed diagnostics

### Q: Can I disable the automatic installation checking?

**A:** The installation checking is essential for the extension to function properly, but you can:
1. Set a valid `prolog.executablePath` to avoid repeated checks
2. Use "Continue Anyway" when prompted if you want to use the extension without SWI-Prolog
3. The extension will remember your choice and won't repeatedly prompt you

### Q: What happens to my settings when the extension migrates my configuration?

**A:** The extension automatically:
1. Creates a backup of your current configuration before making changes
2. Preserves all non-path related settings (linter, formatter, etc.)
3. Only updates the `prolog.executablePath` to point to a valid installation
4. Provides an "Undo Migration" option if you want to revert changes
5. You can view and restore previous configurations through the extension settings

### Q: How do I manually test if my SWI-Prolog installation is working?

**A:** Use these commands:
```bash
# Test basic installation
swipl --version

# Test basic functionality
echo "halt." | swipl -q

# Test if accessible from PATH
which swipl    # macOS/Linux
where swipl    # Windows
```

### Q: The installation guide webview is not loading. How do I fix this?

**A:** Try these solutions:
1. Check VS Code webview security settings
2. Ensure you have an internet connection for external resources
3. Disable other extensions that might interfere with webviews
4. Use "Developer: Reload Window" to refresh the webview
5. Check the browser console in the webview for JavaScript errors