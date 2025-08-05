# Windows Platform Support

The VSCode Prolog Toolkit provides comprehensive support for Windows environments, including Windows 10, Windows 11, and Windows Server editions. This guide covers installation, configuration, and platform-specific features.

## Table of Contents

- [System Requirements](#system-requirements)
- [SWI-Prolog Installation](#swi-prolog-installation)
- [Package Manager Integration](#package-manager-integration)
- [Path Handling](#path-handling)
- [Terminal Integration](#terminal-integration)
- [Permissions and Security](#permissions-and-security)
- [Troubleshooting](#troubleshooting)
- [Advanced Configuration](#advanced-configuration)

## System Requirements

### Minimum Requirements
- **OS**: Windows 10 version 1903 or later
- **Architecture**: x64, x86, ARM64
- **Node.js**: 18.x or 20.x
- **VS Code**: 1.102.0 or later
- **PowerShell**: 5.1 or PowerShell Core 7.x
- **Memory**: 4GB RAM minimum, 8GB recommended

### Recommended Environment
- **OS**: Windows 11 22H2 or later
- **Architecture**: x64
- **Node.js**: 20.x LTS
- **PowerShell**: PowerShell 7.x
- **Terminal**: Windows Terminal
- **Memory**: 16GB RAM

## SWI-Prolog Installation

The extension automatically detects SWI-Prolog installations in common Windows locations:

### Automatic Detection Paths
```
C:\Program Files\swipl\bin\swipl.exe
C:\Program Files (x86)\swipl\bin\swipl.exe
C:\swipl\bin\swipl.exe
%LOCALAPPDATA%\Programs\swipl\bin\swipl.exe
%APPDATA%\swipl\bin\swipl.exe
```

### Package Manager Installation

#### Chocolatey (Recommended)
```powershell
# Install Chocolatey if not already installed
Set-ExecutionPolicy Bypass -Scope Process -Force
[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))

# Install SWI-Prolog
choco install swi-prolog
```

#### Winget
```powershell
# Install SWI-Prolog via Windows Package Manager
winget install SWI-Prolog.SWI-Prolog
```

#### Scoop
```powershell
# Install Scoop if not already installed
Set-ExecutionPolicy RemoteSigned -Scope CurrentUser
irm get.scoop.sh | iex

# Add extras bucket and install SWI-Prolog
scoop bucket add extras
scoop install swi-prolog
```

#### Manual Installation
1. Download from [SWI-Prolog official website](https://www.swi-prolog.org/download/stable)
2. Run the installer as Administrator
3. Choose installation directory (default: `C:\Program Files\swipl`)
4. Add to PATH when prompted
5. Verify installation: `swipl --version`

## Package Manager Integration

The extension integrates with multiple Windows package managers:

### Supported Package Managers
- **Chocolatey**: Community-driven package manager
- **Winget**: Microsoft's official package manager
- **Scoop**: Command-line installer for Windows

### Automatic Detection
The extension automatically detects available package managers and provides installation suggestions:

```typescript
// Example: Package manager detection
const packageManager = PackageManagerIntegration.getInstance();
const suggestions = await packageManager.getInstallationSuggestions();
// Returns: ["choco install swi-prolog", "winget install SWI-Prolog.SWI-Prolog", ...]
```

### Configuration
```json
{
  "prolog.platform.packageManager.preferred": "chocolatey",
  "prolog.platform.packageManager.autoInstall": false,
  "prolog.platform.packageManager.timeout": 300000
}
```

## Path Handling

### Windows Path Conventions
The extension handles various Windows path formats:

#### Supported Path Types
- **Absolute paths**: `C:\Program Files\swipl\bin\swipl.exe`
- **Relative paths**: `.\bin\swipl.exe`
- **UNC paths**: `\\server\share\swipl\bin\swipl.exe`
- **Mixed separators**: `C:/Program Files\swipl/bin\swipl.exe`
- **Environment variables**: `%PROGRAMFILES%\swipl\bin\swipl.exe`
- **User profile**: `%USERPROFILE%\swipl\bin\swipl.exe`

#### Path Normalization
```typescript
// Example: Path normalization
import { PlatformUtils } from './utils/platformUtils';

const path = PlatformUtils.normalizePath('C:/Program Files\\swipl/bin\\swipl.exe');
// Result: 'C:\Program Files\swipl\bin\swipl.exe'
```

#### Environment Variable Expansion
```typescript
// Supported environment variables
const envVars = PlatformUtils.getEnvironmentVariables();
// Windows-specific variables:
// - %USERPROFILE%, %APPDATA%, %LOCALAPPDATA%
// - %PROGRAMFILES%, %PROGRAMFILES(X86)%
// - %TEMP%, %TMP%, %WINDIR%, %SYSTEMROOT%
```

### Configuration Examples
```json
{
  "prolog.executablePath": "%PROGRAMFILES%\\swipl\\bin\\swipl.exe",
  "prolog.platform.pathExpansion": true,
  "prolog.platform.environmentVariables": {
    "SWIPL_HOME": "%PROGRAMFILES%\\swipl",
    "SWIPL_BOOT_FILE": "%PROGRAMFILES%\\swipl\\boot.prc"
  }
}
```

## Terminal Integration

### Supported Shells
- **PowerShell 5.1**: Windows PowerShell (legacy)
- **PowerShell 7.x**: PowerShell Core (recommended)
- **Command Prompt**: Traditional Windows command line
- **WSL**: Windows Subsystem for Linux

### Shell Detection
The extension automatically detects the active shell and configures accordingly:

```typescript
// Example: Shell detection
const terminal = new PrologTerminal();
const shellInfo = await terminal.detectShell();
// Returns: { name: 'powershell', version: '7.3.0', path: 'C:\Program Files\PowerShell\7\pwsh.exe' }
```

### Command Escaping
Platform-specific command escaping for different shells:

#### PowerShell
```powershell
# Automatic escaping for PowerShell
swipl -g "write('Hello World'), nl" -t halt
```

#### Command Prompt
```cmd
# Automatic escaping for Command Prompt
swipl -g "write('Hello World'), nl" -t halt
```

### Terminal Configuration
```json
{
  "prolog.terminal.shell": "powershell",
  "prolog.terminal.runtimeArgs": ["-NoProfile", "-ExecutionPolicy", "Bypass"],
  "prolog.terminal.encoding": "utf8",
  "prolog.terminal.workingDirectory": "${workspaceFolder}"
}
```

## Permissions and Security

### Windows Access Control
The extension handles Windows-specific permission scenarios:

#### File System Permissions
- **Read permissions**: Checking file accessibility
- **Execute permissions**: Validating executable files
- **Administrator rights**: Detecting elevated privileges
- **UAC considerations**: User Account Control compatibility

#### Security Features
- **Code signing verification**: Validating executable signatures
- **Windows Defender integration**: Antivirus compatibility
- **AppLocker support**: Application whitelisting compatibility
- **Windows Security**: SmartScreen integration

### Permission Checking
```typescript
// Example: Permission validation
const finder = new ExecutableFinder();
const result = await finder.validateExecutable('C:\\Program Files\\swipl\\bin\\swipl.exe');

if (result.permissions) {
  console.log('Readable:', result.permissions.readable);
  console.log('Executable:', result.permissions.executable);
  console.log('Signed:', result.permissions.signed);
}
```

### Security Configuration
```json
{
  "prolog.security.validateSignatures": true,
  "prolog.security.allowUnsignedExecutables": false,
  "prolog.security.trustedPaths": [
    "%PROGRAMFILES%\\swipl",
    "%LOCALAPPDATA%\\Programs\\swipl"
  ]
}
```

## Troubleshooting

### Common Issues

#### 1. SWI-Prolog Not Found
**Symptoms**: Extension reports "SWI-Prolog executable not found"

**Solutions**:
```powershell
# Check if SWI-Prolog is in PATH
where swipl

# Add to PATH manually
$env:PATH += ";C:\Program Files\swipl\bin"

# Permanent PATH addition
[Environment]::SetEnvironmentVariable("PATH", $env:PATH + ";C:\Program Files\swipl\bin", "User")
```

#### 2. Permission Denied Errors
**Symptoms**: "Access denied" or "Permission denied" errors

**Solutions**:
- Run VS Code as Administrator
- Check file permissions: `icacls "C:\Program Files\swipl\bin\swipl.exe"`
- Disable antivirus temporarily for testing
- Add exception to Windows Defender

#### 3. PowerShell Execution Policy
**Symptoms**: Scripts cannot be executed

**Solutions**:
```powershell
# Check current execution policy
Get-ExecutionPolicy

# Set execution policy for current user
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser

# Bypass execution policy for VS Code
code --disable-extensions --disable-gpu
```

#### 4. WSL Integration Issues
**Symptoms**: Problems with Windows Subsystem for Linux

**Solutions**:
```bash
# Install SWI-Prolog in WSL
sudo apt update
sudo apt install swi-prolog

# Configure WSL path in VS Code
"prolog.executablePath": "wsl swipl"
```

### Diagnostic Commands
```powershell
# System information
systeminfo | findstr /B /C:"OS Name" /C:"OS Version" /C:"System Type"

# PowerShell version
$PSVersionTable.PSVersion

# Check SWI-Prolog installation
swipl --version
swipl --dump-runtime-variables

# VS Code diagnostics
code --version
code --list-extensions | findstr prolog
```

### Log Files
- **Extension logs**: `%APPDATA%\Code\logs\*\exthost*\output_logging_*`
- **VS Code logs**: `%APPDATA%\Code\logs`
- **Windows Event Logs**: Event Viewer → Windows Logs → Application

## Advanced Configuration

### Registry Integration
The extension can read Windows Registry for SWI-Prolog installations:

```typescript
// Registry paths checked:
// HKEY_LOCAL_MACHINE\SOFTWARE\SWI-Prolog
// HKEY_CURRENT_USER\SOFTWARE\SWI-Prolog
// HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\SWI-Prolog
```

### Windows-Specific Settings
```json
{
  "prolog.platform.windows": {
    "useRegistry": true,
    "checkSignatures": true,
    "preferPowerShell": true,
    "enableUAC": true,
    "pathCaseSensitive": false,
    "maxPathLength": 260,
    "useShortPaths": false
  }
}
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

### Integration with Windows Features
- **Windows Terminal**: Enhanced terminal experience
- **Windows Package Manager**: Automated installation
- **Windows Security**: Antivirus integration
- **Windows Update**: Compatibility with system updates
- **Windows Subsystem for Linux**: Cross-platform development

### Development Environment
```json
{
  "prolog.development.windows": {
    "enableDebugging": true,
    "verboseLogging": false,
    "profilePerformance": false,
    "enableTelemetry": false
  }
}
```

## Best Practices

### Installation
1. Use package managers for easier updates
2. Install as Administrator for system-wide access
3. Verify installation with `swipl --version`
4. Add to PATH for command-line access

### Configuration
1. Use environment variables for portable paths
2. Enable automatic platform detection
3. Configure appropriate shell preferences
4. Set up proper security permissions

### Development
1. Use Windows Terminal for better experience
2. Enable PowerShell 7.x for modern features
3. Configure proper encoding (UTF-8)
4. Use WSL for cross-platform development

### Maintenance
1. Keep SWI-Prolog updated via package manager
2. Monitor Windows Update compatibility
3. Review security settings regularly
4. Clean up temporary files periodically

## Support and Resources

- **Official Documentation**: [SWI-Prolog Windows Guide](https://www.swi-prolog.org/build/windows.html)
- **Package Managers**: [Chocolatey](https://chocolatey.org/), [Winget](https://docs.microsoft.com/en-us/windows/package-manager/), [Scoop](https://scoop.sh/)
- **Windows Terminal**: [Microsoft Terminal](https://github.com/microsoft/terminal)
- **PowerShell**: [PowerShell Documentation](https://docs.microsoft.com/en-us/powershell/)
- **WSL**: [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/)

For additional support, please visit our [GitHub repository](https://github.com/mediaprophet/VSCode-Prolog-Toolkit) or create an issue.