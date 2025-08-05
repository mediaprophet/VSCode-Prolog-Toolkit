# macOS Platform Support

The VSCode Prolog Toolkit provides comprehensive support for macOS environments, including Intel and Apple Silicon Macs. This guide covers installation, configuration, and platform-specific features for macOS.

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
- **OS**: macOS 10.15 (Catalina) or later
- **Architecture**: Intel x64 or Apple Silicon (M1/M2/M3)
- **Node.js**: 18.x or 20.x
- **VS Code**: 1.102.0 or later
- **Shell**: zsh, bash, or fish
- **Memory**: 4GB RAM minimum, 8GB recommended

### Recommended Environment
- **OS**: macOS 13 (Ventura) or later
- **Architecture**: Apple Silicon (M1/M2/M3)
- **Node.js**: 20.x LTS
- **Shell**: zsh (default on macOS 10.15+)
- **Terminal**: Terminal.app or iTerm2
- **Memory**: 16GB RAM

## SWI-Prolog Installation

The extension automatically detects SWI-Prolog installations in common macOS locations:

### Automatic Detection Paths
```
/usr/local/bin/swipl
/opt/homebrew/bin/swipl
/opt/local/bin/swipl
/usr/bin/swipl
~/Applications/SWI-Prolog.app/Contents/MacOS/swipl
/Applications/SWI-Prolog.app/Contents/MacOS/swipl
```

### Package Manager Installation

#### Homebrew (Recommended)
```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install SWI-Prolog
brew install swi-prolog

# For Apple Silicon Macs, Homebrew installs to /opt/homebrew
# For Intel Macs, Homebrew installs to /usr/local
```

#### MacPorts
```bash
# Install MacPorts from https://www.macports.org/install.php

# Install SWI-Prolog
sudo port install swi-prolog

# Update MacPorts
sudo port selfupdate
sudo port upgrade outdated
```

#### Manual Installation
1. Download from [SWI-Prolog official website](https://www.swi-prolog.org/download/stable)
2. Mount the DMG file
3. Drag SWI-Prolog.app to Applications folder
4. Create symlink for command line access:
   ```bash
   sudo ln -sf /Applications/SWI-Prolog.app/Contents/MacOS/swipl /usr/local/bin/swipl
   ```

### Apple Silicon Considerations
For Apple Silicon Macs (M1/M2/M3):
- Homebrew installs to `/opt/homebrew` instead of `/usr/local`
- Native ARM64 binaries provide better performance
- Rosetta 2 compatibility for Intel binaries

```bash
# Check architecture
uname -m
# arm64 = Apple Silicon, x86_64 = Intel

# Homebrew path for Apple Silicon
export PATH="/opt/homebrew/bin:$PATH"
```

## Package Manager Integration

The extension integrates with macOS package managers:

### Supported Package Managers
- **Homebrew**: The most popular package manager for macOS
- **MacPorts**: Ports collection for macOS

### Automatic Detection
The extension automatically detects available package managers:

```typescript
// Example: Package manager detection
const packageManager = PackageManagerIntegration.getInstance();
const suggestions = await packageManager.getInstallationSuggestions();
// Returns: ["brew install swi-prolog", "sudo port install swi-prolog"]
```

### Configuration
```json
{
  "prolog.platform.packageManager.preferred": "homebrew",
  "prolog.platform.packageManager.autoInstall": false,
  "prolog.platform.packageManager.timeout": 300000
}
```

## Path Handling

### macOS Path Conventions
The extension handles various macOS path formats:

#### Supported Path Types
- **Absolute paths**: `/usr/local/bin/swipl`
- **Relative paths**: `./bin/swipl`
- **Home directory**: `~/bin/swipl`
- **Environment variables**: `$HOME/bin/swipl`
- **Application bundles**: `/Applications/SWI-Prolog.app/Contents/MacOS/swipl`

#### Path Normalization
```typescript
// Example: Path normalization
import { PlatformUtils } from './utils/platformUtils';

const path = PlatformUtils.normalizePath('~/Applications/SWI-Prolog.app/Contents/MacOS/swipl');
// Result: '/Users/username/Applications/SWI-Prolog.app/Contents/MacOS/swipl'
```

#### Environment Variable Expansion
```typescript
// Supported environment variables
const envVars = PlatformUtils.getEnvironmentVariables();
// macOS-specific variables:
// - $HOME, $USER, $TMPDIR
// - $PATH, $SHELL, $TERM
// - $XDG_CONFIG_HOME, $XDG_DATA_HOME
```

### Configuration Examples
```json
{
  "prolog.executablePath": "/opt/homebrew/bin/swipl",
  "prolog.platform.pathExpansion": true,
  "prolog.platform.environmentVariables": {
    "SWIPL_HOME": "/opt/homebrew/lib/swipl",
    "DYLD_LIBRARY_PATH": "/opt/homebrew/lib"
  }
}
```

## Terminal Integration

### Supported Shells
- **zsh**: Default shell on macOS 10.15+
- **bash**: Traditional Unix shell
- **fish**: User-friendly shell
- **tcsh**: C shell variant

### Shell Detection
The extension automatically detects the active shell:

```typescript
// Example: Shell detection
const terminal = new PrologTerminal();
const shellInfo = await terminal.detectShell();
// Returns: { name: 'zsh', version: '5.9', path: '/bin/zsh' }
```

### Command Escaping
Platform-specific command escaping for different shells:

#### zsh/bash
```bash
# Automatic escaping for zsh/bash
swipl -g "write('Hello World'), nl" -t halt
```

#### fish
```fish
# Automatic escaping for fish shell
swipl -g "write('Hello World'), nl" -t halt
```

### Terminal Configuration
```json
{
  "prolog.terminal.shell": "zsh",
  "prolog.terminal.runtimeArgs": ["--quiet"],
  "prolog.terminal.encoding": "utf8",
  "prolog.terminal.workingDirectory": "${workspaceFolder}"
}
```

## Permissions and Security

### macOS Security Features
The extension handles macOS-specific security scenarios:

#### File System Permissions
- **Read permissions**: Standard Unix permissions
- **Execute permissions**: Executable bit validation
- **Quarantine attributes**: Gatekeeper integration
- **System Integrity Protection**: SIP compatibility

#### Security Features
- **Gatekeeper**: Code signing verification
- **Notarization**: Apple notarization checking
- **Privacy permissions**: File system access
- **Sandboxing**: App sandbox compatibility

### Permission Checking
```typescript
// Example: Permission validation
const finder = new ExecutableFinder();
const result = await finder.validateExecutable('/opt/homebrew/bin/swipl');

if (result.permissions) {
  console.log('Readable:', result.permissions.readable);
  console.log('Executable:', result.permissions.executable);
  console.log('Quarantined:', result.permissions.quarantined);
}
```

### Security Configuration
```json
{
  "prolog.security.validateSignatures": true,
  "prolog.security.allowUnsignedExecutables": true,
  "prolog.security.trustedPaths": [
    "/usr/local/bin",
    "/opt/homebrew/bin",
    "/opt/local/bin"
  ]
}
```

## Troubleshooting

### Common Issues

#### 1. SWI-Prolog Not Found
**Symptoms**: Extension reports "SWI-Prolog executable not found"

**Solutions**:
```bash
# Check if SWI-Prolog is in PATH
which swipl

# Add Homebrew to PATH (Apple Silicon)
echo 'export PATH="/opt/homebrew/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc

# Add Homebrew to PATH (Intel)
echo 'export PATH="/usr/local/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

#### 2. Permission Denied Errors
**Symptoms**: "Permission denied" errors when executing SWI-Prolog

**Solutions**:
```bash
# Check file permissions
ls -la /opt/homebrew/bin/swipl

# Fix permissions if needed
chmod +x /opt/homebrew/bin/swipl

# Remove quarantine attribute
xattr -d com.apple.quarantine /opt/homebrew/bin/swipl
```

#### 3. Gatekeeper Issues
**Symptoms**: "Cannot be opened because the developer cannot be verified"

**Solutions**:
```bash
# Remove quarantine attribute
sudo xattr -rd com.apple.quarantine /Applications/SWI-Prolog.app

# Allow in System Preferences
# System Preferences → Security & Privacy → General → Allow apps downloaded from: App Store and identified developers
```

#### 4. Apple Silicon Compatibility
**Symptoms**: Performance issues or crashes on Apple Silicon

**Solutions**:
```bash
# Check if running under Rosetta
arch
# arm64 = native, i386 = Rosetta

# Install native ARM64 version
brew uninstall swi-prolog
brew install swi-prolog

# Force ARM64 architecture
arch -arm64 brew install swi-prolog
```

### Diagnostic Commands
```bash
# System information
system_profiler SPSoftwareDataType SPHardwareDataType

# Shell information
echo $SHELL
$SHELL --version

# Check SWI-Prolog installation
swipl --version
swipl --dump-runtime-variables

# Homebrew diagnostics
brew doctor
brew config

# VS Code diagnostics
code --version
code --list-extensions | grep prolog
```

### Log Files
- **Extension logs**: `~/Library/Application Support/Code/logs/*/exthost*/output_logging_*`
- **VS Code logs**: `~/Library/Application Support/Code/logs`
- **System logs**: Console.app → System Reports

## Advanced Configuration

### Homebrew Integration
The extension can detect Homebrew installations:

```typescript
// Homebrew paths checked:
// /opt/homebrew (Apple Silicon)
// /usr/local (Intel)
// ~/.homebrew (custom installation)
```

### macOS-Specific Settings
```json
{
  "prolog.platform.macos": {
    "useHomebrew": true,
    "useMacPorts": true,
    "checkGatekeeper": true,
    "removeQuarantine": false,
    "preferNativeArch": true,
    "enableNotarization": true
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
    "maxConcurrentChecks": 4,
    "useNativeArch": true
  }
}
```

### Integration with macOS Features
- **Spotlight**: Executable indexing
- **Finder**: File association
- **Quick Look**: Prolog file preview
- **Services**: System services integration
- **Automator**: Workflow integration

### Development Environment
```json
{
  "prolog.development.macos": {
    "enableDebugging": true,
    "verboseLogging": false,
    "profilePerformance": false,
    "enableTelemetry": false,
    "useUniversalBinary": true
  }
}
```

## Best Practices

### Installation
1. Use Homebrew for easier package management
2. Install command line tools: `xcode-select --install`
3. Verify installation with `swipl --version`
4. Add to PATH in shell configuration

### Configuration
1. Use environment variables for portable paths
2. Enable automatic platform detection
3. Configure appropriate shell preferences
4. Set up proper security permissions

### Development
1. Use iTerm2 or Terminal.app
2. Configure zsh with oh-my-zsh for enhanced experience
3. Enable proper encoding (UTF-8)
4. Use native ARM64 binaries on Apple Silicon

### Maintenance
1. Keep SWI-Prolog updated via Homebrew
2. Monitor macOS compatibility
3. Review security settings after system updates
4. Clean up Homebrew caches: `brew cleanup`

## Apple Silicon Specific

### Native ARM64 Support
- Better performance and battery life
- Native compilation for optimal speed
- Reduced memory usage

### Rosetta 2 Compatibility
- Automatic translation for Intel binaries
- Slight performance overhead
- Full compatibility with Intel software

### Universal Binaries
- Single binary supporting both architectures
- Automatic architecture selection
- Optimal for distribution

## Support and Resources

- **Official Documentation**: [SWI-Prolog macOS Guide](https://www.swi-prolog.org/build/macos.html)
- **Homebrew**: [Homebrew Documentation](https://docs.brew.sh/)
- **MacPorts**: [MacPorts Guide](https://guide.macports.org/)
- **Apple Developer**: [macOS Development](https://developer.apple.com/macos/)
- **Terminal**: [Terminal User Guide](https://support.apple.com/guide/terminal/)

For additional support, please visit our [GitHub repository](https://github.com/mediaprophet/VSCode-Prolog-Toolkit) or create an issue.