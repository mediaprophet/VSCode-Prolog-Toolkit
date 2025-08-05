# Linux Platform Support

The VSCode Prolog Toolkit provides comprehensive support for Linux distributions, including Ubuntu, Debian, Fedora, CentOS, Arch Linux, openSUSE, and many others. This guide covers installation, configuration, and platform-specific features for Linux environments.

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
- **OS**: Any modern Linux distribution (kernel 3.10+)
- **Architecture**: x64, ARM64, x86, ARM
- **Node.js**: 18.x or 20.x
- **VS Code**: 1.102.0 or later
- **Shell**: bash, zsh, fish, or dash
- **Memory**: 4GB RAM minimum, 8GB recommended

### Recommended Environment
- **OS**: Ubuntu 20.04 LTS or later, Fedora 35+, or equivalent
- **Architecture**: x64 or ARM64
- **Node.js**: 20.x LTS
- **Shell**: bash or zsh
- **Terminal**: GNOME Terminal, Konsole, or Alacritty
- **Memory**: 16GB RAM

### Supported Distributions
- **Debian-based**: Ubuntu, Debian, Linux Mint, Pop!_OS, Elementary OS
- **Red Hat-based**: Fedora, CentOS, RHEL, Rocky Linux, AlmaLinux
- **Arch-based**: Arch Linux, Manjaro, EndeavourOS
- **SUSE-based**: openSUSE Leap, openSUSE Tumbleweed, SLES
- **Others**: Gentoo, Alpine Linux, NixOS, Void Linux

## SWI-Prolog Installation

The extension automatically detects SWI-Prolog installations in common Linux locations:

### Automatic Detection Paths
```
/usr/bin/swipl
/usr/local/bin/swipl
/opt/swipl/bin/swipl
/snap/bin/swi-prolog
/var/lib/flatpak/exports/bin/org.swi_prolog.SWI-Prolog
~/.local/bin/swipl
/usr/lib/swi-prolog/bin/x86_64-linux/swipl
```

### Package Manager Installation

#### APT (Debian/Ubuntu)
```bash
# Update package list
sudo apt update

# Install SWI-Prolog
sudo apt install swi-prolog

# Install development packages (optional)
sudo apt install swi-prolog-nox swi-prolog-doc
```

#### DNF (Fedora)
```bash
# Install SWI-Prolog
sudo dnf install pl

# Install development packages (optional)
sudo dnf install pl-devel
```

#### YUM (CentOS/RHEL)
```bash
# Enable EPEL repository
sudo yum install epel-release

# Install SWI-Prolog
sudo yum install pl
```

#### Pacman (Arch Linux)
```bash
# Install SWI-Prolog
sudo pacman -S swi-prolog

# Install from AUR (alternative)
yay -S swi-prolog-git
```

#### Zypper (openSUSE)
```bash
# Install SWI-Prolog
sudo zypper install swi-prolog
```

#### Snap (Universal)
```bash
# Install SWI-Prolog via Snap
sudo snap install swi-prolog
```

#### Flatpak (Universal)
```bash
# Add Flathub repository
flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

# Install SWI-Prolog
flatpak install flathub org.swi_prolog.SWI-Prolog
```

#### Manual Compilation
```bash
# Install build dependencies (Ubuntu/Debian)
sudo apt install build-essential cmake libreadline-dev libunwind-dev \
  libgmp-dev libssl-dev unixodbc-dev zlib1g-dev libarchive-dev \
  libossp-uuid-dev libxext-dev libice-dev libjpeg-dev libxinerama-dev \
  libxft-dev libxpm-dev libxt-dev pkg-config libdb-dev \
  libpcre3-dev libyaml-dev

# Download and compile
wget https://www.swi-prolog.org/download/stable/src/swipl-9.0.4.tar.gz
tar xzf swipl-9.0.4.tar.gz
cd swipl-9.0.4
mkdir build && cd build
cmake -DCMAKE_INSTALL_PREFIX=/usr/local ..
make -j$(nproc)
sudo make install
```

## Package Manager Integration

The extension integrates with multiple Linux package managers:

### Supported Package Managers
- **APT**: Debian, Ubuntu, and derivatives
- **DNF**: Fedora, CentOS 8+
- **YUM**: CentOS 7, RHEL 7
- **Pacman**: Arch Linux and derivatives
- **Zypper**: openSUSE and SLES
- **Snap**: Universal packages
- **Flatpak**: Universal applications

### Automatic Detection
The extension automatically detects available package managers:

```typescript
// Example: Package manager detection
const packageManager = PackageManagerIntegration.getInstance();
const suggestions = await packageManager.getInstallationSuggestions();
// Returns: ["sudo apt install swi-prolog", "sudo dnf install pl", ...]
```

### Configuration
```json
{
  "prolog.platform.packageManager.preferred": "apt",
  "prolog.platform.packageManager.autoInstall": false,
  "prolog.platform.packageManager.timeout": 300000
}
```

## Path Handling

### Linux Path Conventions
The extension handles various Linux path formats:

#### Supported Path Types
- **Absolute paths**: `/usr/bin/swipl`
- **Relative paths**: `./bin/swipl`
- **Home directory**: `~/bin/swipl`
- **Environment variables**: `$HOME/bin/swipl`, `${HOME}/bin/swipl`
- **XDG directories**: `$XDG_CONFIG_HOME`, `$XDG_DATA_HOME`
- **Snap paths**: `/snap/bin/swi-prolog`
- **Flatpak paths**: `/var/lib/flatpak/exports/bin/org.swi_prolog.SWI-Prolog`

#### Path Normalization
```typescript
// Example: Path normalization
import { PlatformUtils } from './utils/platformUtils';

const path = PlatformUtils.normalizePath('$HOME/.local/bin/swipl');
// Result: '/home/username/.local/bin/swipl'
```

#### Environment Variable Expansion
```typescript
// Supported environment variables
const envVars = PlatformUtils.getEnvironmentVariables();
// Linux-specific variables:
// - $HOME, $USER, $TMPDIR, $TMP
// - $PATH, $SHELL, $TERM
// - $XDG_CONFIG_HOME, $XDG_DATA_HOME, $XDG_CACHE_HOME
// - $DISPLAY, $WAYLAND_DISPLAY
```

### Configuration Examples
```json
{
  "prolog.executablePath": "/usr/bin/swipl",
  "prolog.platform.pathExpansion": true,
  "prolog.platform.environmentVariables": {
    "SWIPL_HOME": "/usr/lib/swi-prolog",
    "LD_LIBRARY_PATH": "/usr/lib/swi-prolog/lib/x86_64-linux"
  }
}
```

## Terminal Integration

### Supported Shells
- **bash**: Bourne Again Shell (most common)
- **zsh**: Z Shell (feature-rich)
- **fish**: Friendly Interactive Shell
- **dash**: Debian Almquist Shell
- **tcsh**: TENEX C Shell
- **ksh**: Korn Shell

### Shell Detection
The extension automatically detects the active shell:

```typescript
// Example: Shell detection
const terminal = new PrologTerminal();
const shellInfo = await terminal.detectShell();
// Returns: { name: 'bash', version: '5.1.16', path: '/bin/bash' }
```

### Command Escaping
Platform-specific command escaping for different shells:

#### bash/zsh
```bash
# Automatic escaping for bash/zsh
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
  "prolog.terminal.shell": "bash",
  "prolog.terminal.runtimeArgs": ["--quiet"],
  "prolog.terminal.encoding": "utf8",
  "prolog.terminal.workingDirectory": "${workspaceFolder}"
}
```

## Permissions and Security

### Linux Security Features
The extension handles Linux-specific security scenarios:

#### File System Permissions
- **Read permissions**: Standard Unix permissions (r--)
- **Write permissions**: File modification rights (-w-)
- **Execute permissions**: Executable bit validation (--x)
- **Special permissions**: setuid, setgid, sticky bit
- **Access Control Lists**: Extended permissions (ACLs)

#### Security Features
- **SELinux**: Security-Enhanced Linux contexts
- **AppArmor**: Application security profiles
- **Capabilities**: Fine-grained privileges
- **Namespaces**: Process isolation
- **Cgroups**: Resource control

### Permission Checking
```typescript
// Example: Permission validation
const finder = new ExecutableFinder();
const result = await finder.validateExecutable('/usr/bin/swipl');

if (result.permissions) {
  console.log('Readable:', result.permissions.readable);
  console.log('Executable:', result.permissions.executable);
  console.log('Owner:', result.permissions.owner);
  console.log('Group:', result.permissions.group);
  console.log('Mode:', result.permissions.mode);
}
```

### Security Configuration
```json
{
  "prolog.security.validatePermissions": true,
  "prolog.security.allowSetuidExecutables": false,
  "prolog.security.trustedPaths": [
    "/usr/bin",
    "/usr/local/bin",
    "/opt/*/bin"
  ]
}
```

## Troubleshooting

### Common Issues

#### 1. SWI-Prolog Not Found
**Symptoms**: Extension reports "SWI-Prolog executable not found"

**Solutions**:
```bash
# Check if SWI-Prolog is installed
which swipl
whereis swipl

# Check package installation
dpkg -l | grep swi-prolog  # Debian/Ubuntu
rpm -qa | grep pl          # Red Hat/Fedora
pacman -Q swi-prolog       # Arch Linux

# Add to PATH if needed
echo 'export PATH="/usr/local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

#### 2. Permission Denied Errors
**Symptoms**: "Permission denied" errors when executing SWI-Prolog

**Solutions**:
```bash
# Check file permissions
ls -la /usr/bin/swipl

# Fix permissions if needed
sudo chmod +x /usr/bin/swipl

# Check if file is executable
test -x /usr/bin/swipl && echo "Executable" || echo "Not executable"
```

#### 3. Library Dependencies
**Symptoms**: "Shared library not found" or similar errors

**Solutions**:
```bash
# Check library dependencies
ldd /usr/bin/swipl

# Install missing libraries (Ubuntu/Debian)
sudo apt install libreadline8 libgmp10 libssl3

# Update library cache
sudo ldconfig
```

#### 4. SELinux/AppArmor Issues
**Symptoms**: Security policy violations

**Solutions**:
```bash
# Check SELinux status
sestatus

# Check AppArmor status
sudo apparmor_status

# Temporarily disable SELinux (for testing)
sudo setenforce 0

# Check audit logs
sudo ausearch -m avc -ts recent
```

### Diagnostic Commands
```bash
# System information
uname -a
lsb_release -a
cat /etc/os-release

# Shell information
echo $SHELL
$SHELL --version

# Check SWI-Prolog installation
swipl --version
swipl --dump-runtime-variables

# Package manager diagnostics
apt list --installed | grep swi-prolog  # Debian/Ubuntu
dnf list installed | grep pl            # Fedora
pacman -Q swi-prolog                     # Arch Linux

# VS Code diagnostics
code --version
code --list-extensions | grep prolog
```

### Log Files
- **Extension logs**: `~/.config/Code/logs/*/exthost*/output_logging_*`
- **VS Code logs**: `~/.config/Code/logs`
- **System logs**: `/var/log/syslog`, `journalctl -u vscode`

## Advanced Configuration

### Distribution-Specific Paths
The extension checks distribution-specific locations:

```typescript
// Distribution detection paths:
// /etc/debian_version (Debian/Ubuntu)
// /etc/redhat-release (Red Hat/CentOS/Fedora)
// /etc/arch-release (Arch Linux)
// /etc/suse-release (openSUSE)
// /etc/os-release (Standard)
```

### Linux-Specific Settings
```json
{
  "prolog.platform.linux": {
    "usePackageManager": true,
    "checkPermissions": true,
    "validateLibraries": true,
    "enableSELinux": true,
    "enableAppArmor": true,
    "useSystemd": true
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
    "maxConcurrentChecks": 8,
    "useNativeLibraries": true
  }
}
```

### Integration with Linux Features
- **Desktop Integration**: .desktop files, MIME types
- **Package Managers**: Native package integration
- **System Services**: systemd integration
- **Container Support**: Docker, Podman compatibility
- **Wayland/X11**: Display server compatibility

### Development Environment
```json
{
  "prolog.development.linux": {
    "enableDebugging": true,
    "verboseLogging": false,
    "profilePerformance": false,
    "enableTelemetry": false,
    "useDebugSymbols": true
  }
}
```

## Container Support

### Docker Integration
```dockerfile
# Example Dockerfile for SWI-Prolog development
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    swi-prolog \
    nodejs \
    npm \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace
COPY . .
RUN npm install
```

### Podman Support
```bash
# Run with Podman
podman run -it --rm -v $(pwd):/workspace ubuntu:22.04 bash
```

### Snap Confinement
```bash
# Connect interfaces for Snap packages
sudo snap connect swi-prolog:home
sudo snap connect swi-prolog:removable-media
```

## Best Practices

### Installation
1. Use distribution package manager when possible
2. Install development packages for full functionality
3. Verify installation with `swipl --version`
4. Check library dependencies with `ldd`

### Configuration
1. Use environment variables for portable paths
2. Enable automatic platform detection
3. Configure appropriate shell preferences
4. Set up proper file permissions

### Development
1. Use modern terminal emulator (GNOME Terminal, Konsole)
2. Configure shell with appropriate plugins
3. Enable proper encoding (UTF-8)
4. Use package manager for dependency management

### Maintenance
1. Keep SWI-Prolog updated via package manager
2. Monitor security updates
3. Review file permissions regularly
4. Clean package caches: `sudo apt autoremove && sudo apt autoclean`

## Distribution-Specific Notes

### Ubuntu/Debian
- Use `apt` package manager
- Enable universe repository for latest packages
- Consider PPA for newer versions

### Fedora/CentOS
- Use `dnf` or `yum` package manager
- Enable EPEL repository on CentOS
- Consider Copr repositories for newer versions

### Arch Linux
- Use `pacman` package manager
- Check AUR for development versions
- Keep system updated regularly

### openSUSE
- Use `zypper` package manager
- Enable additional repositories if needed
- Consider Tumbleweed for latest packages

## Support and Resources

- **Official Documentation**: [SWI-Prolog Linux Guide](https://www.swi-prolog.org/build/unix.html)
- **Package Managers**: Distribution-specific documentation
- **Container Platforms**: [Docker](https://docs.docker.com/), [Podman](https://podman.io/docs/)
- **Security**: [SELinux](https://selinuxproject.org/), [AppArmor](https://apparmor.net/)
- **Desktop Integration**: [FreeDesktop.org](https://www.freedesktop.org/)

For additional support, please visit our [GitHub repository](https://github.com/mediaprophet/VSCode-Prolog-Toolkit) or create an issue.