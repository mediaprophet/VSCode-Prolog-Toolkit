# VSCode Prolog Toolkit v1.3.0 - Validation Guide

This guide provides comprehensive instructions for validating the VSCode Prolog Toolkit extension before release.

## Overview

The validation process consists of several test suites designed to ensure the extension works correctly across different platforms and scenarios:

1. **Smoke Tests** - Basic functionality validation
2. **Activity Bar Tests** - New v1.3.0 activity bar features
3. **Platform Tests** - Cross-platform compatibility
4. **VSIX Installation Tests** - Real-world installation validation

## Prerequisites

### Required Software
- **Node.js** 18.x or 20.x
- **VS Code** 1.102.0 or later
- **Git** (for cloning and version control)
- **SWI-Prolog** (optional, for full functionality testing)

### Platform-Specific Requirements

#### Windows
- Windows 10 or 11
- PowerShell or Command Prompt
- VS Code CLI available in PATH

#### macOS
- macOS 10.15 or later
- Terminal with bash/zsh
- VS Code CLI available in PATH

#### Linux
- Ubuntu 18.04+, Debian 10+, or equivalent
- bash shell
- VS Code CLI available in PATH

## Quick Start

### 15-Minute Quick Validation

For rapid validation during development:

```bash
# 1. Run smoke tests only
npm run test:smoke

# 2. Quick VSIX installation test
node test/validation/vsix-installation-test.js

# 3. Manual verification checklist (5 minutes)
# - Install VSIX in clean VS Code
# - Open a .pl file
# - Check activity bar icon appears
# - Open dashboard
# - Verify no error notifications
```

### 30-Minute Comprehensive Validation

For thorough pre-release validation:

```bash
# 1. Run all automated tests
npm run test:validation

# 2. Platform-specific testing
npm run test:platform

# 3. Activity bar validation
npm run test:activity-bar

# 4. Full VSIX installation test
node test/validation/vsix-installation-test.js

# 5. Manual testing checklist (15 minutes)
# See "Manual Testing Checklist" section below
```

## Test Execution

### Automated Test Suites

#### Running All Tests
```bash
# Run complete validation suite
node test/validation/run-validation.js

# Or using npm script
npm run test:validation
```

#### Running Individual Test Suites
```bash
# Smoke tests - basic functionality
npm run test:smoke

# Activity bar tests - v1.3.0 features
npm run test:activity-bar

# Platform tests - cross-platform compatibility
npm run test:platform
```

#### VSIX Installation Testing
```bash
# Test VSIX package installation
node test/validation/vsix-installation-test.js
```

### Manual Testing Checklist

#### Critical Functionality (Must Pass)
- [ ] **Extension Activation**
  - [ ] Extension loads without errors
  - [ ] No error notifications on startup
  - [ ] Extension appears in Extensions view

- [ ] **Activity Bar Integration**
  - [ ] Prolog icon appears in activity bar
  - [ ] Clicking icon opens Prolog view
  - [ ] Tree view shows workspace Prolog files
  - [ ] Dashboard opens without errors

- [ ] **File Association**
  - [ ] .pl files open with Prolog syntax highlighting
  - [ ] .pro files recognized as Prolog (if applicable)
  - [ ] File icons display correctly

- [ ] **Basic Language Features**
  - [ ] Syntax highlighting works
  - [ ] Hover information appears for predicates
  - [ ] Go to definition works for predicates
  - [ ] Code completion suggestions appear

#### Activity Bar Features (v1.3.0)
- [ ] **Tree View**
  - [ ] Shows all Prolog files in workspace
  - [ ] Files can be opened by clicking
  - [ ] Refresh button works
  - [ ] Handles empty workspace gracefully

- [ ] **Dashboard**
  - [ ] Opens in webview panel
  - [ ] Displays extension information
  - [ ] Query interface works (if SWI-Prolog available)
  - [ ] Styling matches VS Code theme

- [ ] **Commands**
  - [ ] All new commands appear in Command Palette
  - [ ] Commands execute without errors
  - [ ] Appropriate feedback provided

#### Cross-Platform Verification
- [ ] **Windows Testing**
  - [ ] Works on Windows 10/11
  - [ ] File paths handle backslashes correctly
  - [ ] PowerShell integration works

- [ ] **macOS Testing**
  - [ ] Works on macOS 10.15+
  - [ ] Homebrew SWI-Prolog detection works
  - [ ] Terminal integration works

- [ ] **Linux Testing**
  - [ ] Works on Ubuntu/Debian/Fedora
  - [ ] Package manager detection works
  - [ ] File permissions handled correctly

## Test Results Interpretation

### Success Criteria
- **All automated tests pass** (100% success rate preferred, 95%+ acceptable)
- **No critical manual test failures**
- **VSIX installs successfully on clean VS Code**
- **No error notifications during normal usage**
- **Activity bar features work as expected**

### Common Issues and Solutions

#### Extension Fails to Activate
- Check VS Code version compatibility
- Verify all dependencies are included in VSIX
- Check for JavaScript syntax errors in logs

#### Activity Bar Icon Missing
- Verify package.json view contributions
- Check icon file exists in media folder
- Ensure proper view registration in extension.ts

#### Syntax Highlighting Not Working
- Verify language configuration in package.json
- Check TextMate grammar files
- Ensure file associations are correct

#### Performance Issues
- Check for memory leaks in providers
- Verify efficient file system operations
- Test with large workspaces

## Platform-Specific Testing

### Windows Testing
```bash
# Test on Windows Command Prompt
cmd /c "node test/validation/run-validation.js"

# Test on PowerShell
powershell -Command "node test/validation/run-validation.js"

# Test with Windows paths
# Verify backslash handling in file paths
```

### macOS Testing
```bash
# Test on macOS Terminal
node test/validation/run-validation.js

# Test with Homebrew SWI-Prolog
brew install swi-prolog
node test/validation/platform-tests.js

# Test with different shells
zsh -c "node test/validation/run-validation.js"
bash -c "node test/validation/run-validation.js"
```

### Linux Testing
```bash
# Test on Ubuntu/Debian
sudo apt-get install swi-prolog
node test/validation/run-validation.js

# Test on Fedora/CentOS
sudo dnf install pl
node test/validation/run-validation.js

# Test with different distributions
# Verify package manager detection
```

## Continuous Integration

### GitHub Actions Integration
The validation tests can be integrated into GitHub Actions workflow:

```yaml
- name: Run Validation Tests
  run: |
    npm ci
    npm run test:validation
    
- name: Test VSIX Installation
  run: |
    npm run package
    node test/validation/vsix-installation-test.js
```

### Local CI Testing
```bash
# Simulate CI environment
npm ci --production=false
npm run build
npm run package
npm run test:validation
```

## Troubleshooting

### Test Failures
1. **Check logs** - Review detailed error messages
2. **Verify environment** - Ensure all prerequisites are met
3. **Clean install** - Remove node_modules and reinstall
4. **Platform differences** - Some tests may behave differently on different platforms

### VSIX Installation Issues
1. **Uninstall existing** - Remove any previous versions
2. **Check file size** - VSIX should be ~24MB
3. **Verify VS Code version** - Ensure compatibility
4. **Check permissions** - Ensure write access to extensions folder

### Performance Issues
1. **Monitor memory usage** - Check for memory leaks
2. **Profile startup time** - Measure extension activation time
3. **Test with large workspaces** - Verify scalability

## Reporting Issues

When reporting validation issues, include:
- **Platform and version** (Windows 11, macOS 12.6, Ubuntu 22.04, etc.)
- **VS Code version**
- **Node.js version**
- **Complete error logs**
- **Steps to reproduce**
- **Expected vs actual behavior**

## Release Readiness Checklist

Before releasing v1.3.0, ensure:
- [ ] All validation tests pass on all target platforms
- [ ] VSIX installation works on clean VS Code instances
- [ ] Activity bar features work correctly
- [ ] No critical bugs identified
- [ ] Performance is acceptable
- [ ] Documentation is complete
- [ ] GitHub Actions workflow validated

## Additional Resources

- [VS Code Extension Testing Guide](https://code.visualstudio.com/api/working-with-extensions/testing-extension)
- [Mocha Testing Framework](https://mochajs.org/)
- [SWI-Prolog Documentation](https://www.swi-prolog.org/pldoc/doc_for?object=manual)
- [Extension Development Best Practices](https://code.visualstudio.com/api/references/extension-guidelines)

---

**Note**: This validation guide should be updated as new tests are added or requirements change. Regular review and updates ensure the validation process remains effective and comprehensive.