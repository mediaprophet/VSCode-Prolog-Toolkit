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

## Setup Wizard

The Setup Wizard provides guided configuration:

### 1. Launch Wizard
- Command Palette → "Prolog: Setup Wizard"
- Or click notification when SWI-Prolog is not found

### 2. Platform Detection
- Automatically detects your operating system
- Shows platform-specific recommendations

### 3. SWI-Prolog Detection
- Searches common installation locations
- Provides installation suggestions if not found

### 4. Package Manager Integration
- Detects available package managers
- Offers automated installation options

### 5. Configuration
- Sets up optimal default settings
- Configures platform-specific options

### 6. Verification
- Tests SWI-Prolog installation
- Validates configuration
- Runs sample queries

## Verification

### Test Installation

1. **Open Prolog File**: Create or open a `.pl` file
2. **Check Syntax Highlighting**: Verify Prolog syntax is highlighted
3. **Test Linting**: Introduce a syntax error and check for diagnostics
4. **Run Query**: Use `Ctrl+Shift+Q` to execute a query
5. **Load Document**: Use `Alt+X L` to load current document

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

### Test Commands

```prolog
?- hello_world.
?- factorial(5, X).
?- trace, factorial(3, X).
```

## Troubleshooting

### Common Issues

#### 1. SWI-Prolog Not Found

**Symptoms:**
- "SWI-Prolog executable not found" error
- No syntax highlighting or linting

**Solutions:**
1. Run Setup Wizard: `Ctrl+Shift+P` → "Prolog: Setup Wizard"
2. Check PATH: Ensure SWI-Prolog is in system PATH
3. Manual configuration: Set `prolog.executablePath` in settings
4. Reinstall SWI-Prolog using package manager

#### 2. Permission Errors

**Windows:**
```powershell
# Run as Administrator
# Check execution policy
Get-ExecutionPolicy
Set-ExecutionPolicy RemoteSigned -Scope CurrentUser
```

**macOS:**
```bash
# Remove quarantine
sudo xattr -rd com.apple.quarantine /Applications/SWI-Prolog.app
# Fix permissions
chmod +x /usr/local/bin/swipl
```

**Linux:**
```bash
# Fix permissions
sudo chmod +x /usr/bin/swipl
# Check ownership
ls -la /usr/bin/swipl
```

#### 3. Path Issues

**Environment Variables:**
- Windows: `%PROGRAMFILES%\swipl\bin`
- macOS: `$HOME/.local/bin`
- Linux: `$HOME/.local/bin`

**Configuration:**
```json
{
  "prolog.platform.pathExpansion": true,
  "prolog.executablePath": "${env:PROGRAMFILES}\\swipl\\bin\\swipl.exe"
}
```

#### 4. Terminal Integration

**Shell Detection:**
```json
{
  "prolog.terminal.shell": "powershell",  // Windows
  "prolog.terminal.shell": "zsh",         // macOS
  "prolog.terminal.shell": "bash"         // Linux
}
```

### Diagnostic Commands

#### Check Installation
```bash
# Verify SWI-Prolog
swipl --version
swipl --dump-runtime-variables

# Check VS Code
code --version
code --list-extensions | grep prolog
```

#### Platform Information
```bash
# Windows
systeminfo | findstr /B /C:"OS Name"

# macOS
system_profiler SPSoftwareDataType

# Linux
uname -a && lsb_release -a
```

### Log Files

**Extension Logs:**
- Windows: `%APPDATA%\Code\logs\*\exthost*\output_logging_*`
- macOS: `~/Library/Application Support/Code/logs/*/exthost*/output_logging_*`
- Linux: `~/.config/Code/logs/*/exthost*/output_logging_*`

## Advanced Setup

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