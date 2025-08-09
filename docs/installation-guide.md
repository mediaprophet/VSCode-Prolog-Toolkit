# VSCode Prolog Toolkit Installation Guide

This comprehensive guide covers installation and setup of the VSCode Prolog Toolkit across all supported platforms. The extension provides intelligent platform detection and automated setup assistance.

## Table of Contents

- [Quick Start](#quick-start)
- [Platform-Specific Installation](#platform-specific-installation)
- [SWI-Prolog Installation](#swi-prolog-installation)
- [Extension Configuration](#extension-configuration)
- [Setup Wizard](#setup-wizard)
- [Verification](#verification)
- [Troubleshooting](#troubleshooting)
- [Advanced Setup](#advanced-setup)

## Quick Start

### 1. Install the Extension

#### From VS Code Marketplace
1. Open VS Code
2. Go to Extensions view (`Ctrl+Shift+X` / `Cmd+Shift+X`)
3. Search for "VSCode Prolog Toolkit"
4. Click "Install"

#### From Command Line
```bash
code --install-extension mediaprophet.vscode-prolog-toolkit
```

#### From VSIX File
```bash
code --install-extension vscode-prolog-toolkit-*.vsix
```

### 2. Install SWI-Prolog

The extension will automatically detect if SWI-Prolog is installed. If not found, it will provide platform-specific installation suggestions.

### 3. Run Setup Wizard

1. Open Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`)
2. Run "Prolog: Setup Wizard"
3. Follow the guided setup process

## Platform-Specific Installation

### Windows

#### Prerequisites
- Windows 10 version 1903 or later
- PowerShell 5.1 or PowerShell 7.x
- Administrator privileges (for some installation methods)

#### Recommended: Chocolatey
```powershell
# Install Chocolatey (if not already installed)
Set-ExecutionPolicy Bypass -Scope Process -Force
[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))

# Install SWI-Prolog
choco install swi-prolog
```

#### Alternative: Winget
```powershell
winget install SWI-Prolog.SWI-Prolog
```

#### Alternative: Scoop
```powershell
# Install Scoop (if not already installed)
Set-ExecutionPolicy RemoteSigned -Scope CurrentUser
irm get.scoop.sh | iex

# Install SWI-Prolog
scoop bucket add extras
scoop install swi-prolog
```

#### Manual Installation
1. Download from [SWI-Prolog Windows Downloads](https://www.swi-prolog.org/download/stable)
2. Run installer as Administrator
3. Add to PATH when prompted
4. Verify: `swipl --version`

### macOS

#### Prerequisites
- macOS 10.15 (Catalina) or later
- Command Line Tools: `xcode-select --install`

#### Recommended: Homebrew
```bash
# Install Homebrew (if not already installed)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install SWI-Prolog
brew install swi-prolog
```

#### Alternative: MacPorts
```bash
# Install MacPorts from https://www.macports.org/install.php
sudo port install swi-prolog
```

#### Manual Installation
1. Download from [SWI-Prolog macOS Downloads](https://www.swi-prolog.org/download/stable)
2. Mount DMG and drag to Applications
3. Create symlink: `sudo ln -sf /Applications/SWI-Prolog.app/Contents/MacOS/swipl /usr/local/bin/swipl`
4. Verify: `swipl --version`

### Linux

#### Ubuntu/Debian
```bash
sudo apt update
sudo apt install swi-prolog
```

#### Fedora
```bash
sudo dnf install pl
```

#### CentOS/RHEL
```bash
sudo yum install epel-release
sudo yum install pl
```

#### Arch Linux
```bash
sudo pacman -S swi-prolog
```

#### openSUSE
```bash
sudo zypper install swi-prolog
```

#### Universal: Snap
```bash
sudo snap install swi-prolog
```

#### Universal: Flatpak
```bash
flatpak install flathub org.swi_prolog.SWI-Prolog
```

## SWI-Prolog Installation

### Automatic Detection

The extension automatically searches for SWI-Prolog in common locations:

**Windows:**
- `C:\Program Files\swipl\bin\swipl.exe`
- `C:\Program Files (x86)\swipl\bin\swipl.exe`
- `%LOCALAPPDATA%\Programs\swipl\bin\swipl.exe`

**macOS:**
- `/usr/local/bin/swipl`
- `/opt/homebrew/bin/swipl`
- `/Applications/SWI-Prolog.app/Contents/MacOS/swipl`

**Linux:**
- `/usr/bin/swipl`
- `/usr/local/bin/swipl`
- `/snap/bin/swi-prolog`

### Manual Configuration

If automatic detection fails, configure manually:

```json
{
  "prolog.executablePath": "/path/to/swipl"
}
```

### Version Requirements

- **Minimum**: SWI-Prolog 8.0.0
- **Recommended**: SWI-Prolog 9.0.0 or later
- **Latest**: Always recommended for best compatibility

## Extension Configuration

### Basic Configuration

```json
{
  "prolog.executablePath": "swipl",
  "prolog.dialect": "swi",
  "prolog.linter.run": "onType",
  "prolog.linter.delay": 500,
  "prolog.format.addSpace": true
}
```

### Platform-Specific Configuration

```json
{
  "prolog.platform.autoDetect": true,
  "prolog.platform.pathExpansion": true,
  "prolog.platform.environmentVariables": {
    "SWIPL_HOME": "/usr/lib/swi-prolog"
  }
}
```

### Terminal Configuration

```json
{
  "prolog.terminal.runtimeArgs": ["--quiet"],
  "prolog.terminal.shell": "auto"
}
```


## Setup Wizard (Modern Multi-Step)

The Setup Wizard is a modern, interactive, multi-step webview that guides you through the entire installation and configuration process, including HTTP backend setup and troubleshooting. It is the recommended way to get started and resolve most issues.

### 1. Launch the Setup Wizard
- Open the Command Palette (`Ctrl+Shift+P`) and select **Prolog: Setup Wizard**
- Or click the notification if SWI-Prolog or the backend is not detected

### 2. Platform & Environment Detection
- The wizard auto-detects your operating system and environment
- Platform-specific instructions and recommendations are shown

### 3. SWI-Prolog Detection & Installation
- Searches common installation locations for SWI-Prolog
- If not found, provides direct download links and package manager options
- Offers automated installation via detected package managers (e.g., Chocolatey, Homebrew, apt)

### 4. HTTP Backend Configuration
- Dedicated step for HTTP backend setup
- Configure backend port, test connectivity, and view troubleshooting tips
- Wizard will test backend startup and report actionable diagnostics

### 5. Configuration & Verification
- Sets up optimal default settings and platform-specific options
- Verifies SWI-Prolog and backend installation
- Runs sample queries to confirm end-to-end functionality

### 6. Troubleshooting & Help
- Integrated troubleshooting help for common issues (installation, backend, chat, N3 logic, etc.)
- Actionable recommendations and direct links to documentation
- Diagnostic logs and error codes are referenced for advanced users

### 7. Completion
- On success, the wizard confirms your environment is ready
- If issues remain, the wizard provides next steps and support resources


---

**Tip:** You can re-run the Setup Wizard at any time to reconfigure, test, or troubleshoot your environment.


## Verification & End-to-End Test

After completing the Setup Wizard:

1. **Open a Prolog file** (`.pl`) to verify syntax highlighting and linting
2. **Test the backend**: Use chat commands or the command palette to run a query (e.g., `/query member(X, [1,2,3])`)
3. **Check HTTP backend**: If using HTTP, ensure the backend status is "Running" in the status bar and test connectivity in the wizard
4. **Run sample queries**: Use the following in the chat or terminal:
   ```prolog
   ?- hello_world.
   ?- factorial(5, X).
   ?- trace, factorial(3, X).
   ```
5. **Review diagnostics**: If any step fails, use the wizard's troubleshooting help or refer to the [Troubleshooting Guide](./troubleshooting.md)

### Sample Test File

Create `test.pl`:
```prolog
% Test file for VSCode Prolog Toolkit
hello_world :-
  write('Hello, World!'),
  nl.

factorial(0, 1) :- !.
factorial(N, F) :-
  N > 0,
  N1 is N - 1,
  factorial(N1, F1),
  F is N * F1.
```


## Troubleshooting & Diagnostics

If you encounter issues at any step, use the integrated troubleshooting help in the Setup Wizard or refer to the [Troubleshooting Guide](./troubleshooting.md). Common issues include:

### SWI-Prolog Not Found
- Run the Setup Wizard to auto-detect or manually configure the path
- Ensure SWI-Prolog is in your system PATH
- Use the wizard's "Manual Configuration" to browse to your installation

### HTTP Backend/Connection Issues
- Use the wizard's HTTP backend step to test connectivity and view diagnostics
- If the backend fails to start, check port availability and firewall settings
- Adjust backend port in the wizard or settings if needed

### Permission & Path Issues
- See platform-specific instructions in the wizard and below
- Ensure correct permissions and environment variables

### Diagnostic Commands
Use these commands to verify your setup:
```bash
# Verify SWI-Prolog
swipl --version
swipl --dump-runtime-variables

# Check VS Code
code --version
code --list-extensions | grep prolog
```

### Log Files
Extension logs are available at:
- Windows: `%APPDATA%\Code\logs\*\exthost*\output_logging_*`
- macOS: `~/Library/Application Support/Code/logs/*/exthost*/output_logging_*`
- Linux: `~/.config/Code/logs/*/exthost*/output_logging_*`


## Advanced & Multi-Platform Setup

### Multi-Platform Development

For development across multiple platforms:

```json
{
  "prolog.executablePath": {
    "windows": "C:\\Program Files\\swipl\\bin\\swipl.exe",
    "macos": "/opt/homebrew/bin/swipl",
    "linux": "/usr/bin/swipl"
  }
}
```

### Container Development

#### Docker
```dockerfile
FROM ubuntu:22.04
RUN apt-get update && apt-get install -y swi-prolog nodejs npm
WORKDIR /workspace
```

#### Dev Containers
```json
{
  "name": "Prolog Development",
  "image": "ubuntu:22.04",
  "features": {
    "ghcr.io/devcontainers/features/node:1": {},
    "ghcr.io/devcontainers/features/common-utils:2": {}
  },
  "postCreateCommand": "apt-get update && apt-get install -y swi-prolog",
  "extensions": ["mediaprophet.vscode-prolog-toolkit"]
}
```

### CI/CD Integration

#### GitHub Actions
```yaml
- name: Install SWI-Prolog
  run: |
    if [ "$RUNNER_OS" == "Linux" ]; then
      sudo apt-get update && sudo apt-get install -y swi-prolog
    elif [ "$RUNNER_OS" == "macOS" ]; then
      brew install swi-prolog
    elif [ "$RUNNER_OS" == "Windows" ]; then
      choco install swi-prolog
    fi
```

### Performance Optimization

```json
{
  "prolog.platform.performance": {
    "cacheExecutablePaths": true,
    "cacheDuration": 3600000,
    "parallelDetection": true,
    "maxConcurrentChecks": 4
  }
}
```

### Security Configuration

```json
{
  "prolog.security.validateSignatures": true,
  "prolog.security.trustedPaths": [
    "/usr/bin",
    "/usr/local/bin",
    "C:\\Program Files\\swipl"
  ]
}
```

## Support and Resources

### Documentation
- [Windows Platform Guide](./platform/windows.md)
- [macOS Platform Guide](./platform/macos.md)
- [Linux Platform Guide](./platform/linux.md)
- [SWI-Prolog Documentation](https://www.swi-prolog.org/pldoc/doc_for?object=manual)

### Community
- [GitHub Repository](https://github.com/mediaprophet/VSCode-Prolog-Toolkit)
- [Issue Tracker](https://github.com/mediaprophet/VSCode-Prolog-Toolkit/issues)
- [Discussions](https://github.com/mediaprophet/VSCode-Prolog-Toolkit/discussions)

### Getting Help

1. **Check Documentation**: Review platform-specific guides
2. **Run Diagnostics**: Use built-in diagnostic tools
3. **Search Issues**: Check existing GitHub issues
4. **Create Issue**: Report bugs or request features
5. **Community Support**: Ask questions in discussions

### Contributing

We welcome contributions! See our [Contributing Guide](../CONTRIBUTING.md) for details on:
- Reporting bugs
- Suggesting features
- Submitting pull requests
- Development setup

---

**Need immediate help?** Run the Setup Wizard: `Ctrl+Shift+P` → "Prolog: Setup Wizard"