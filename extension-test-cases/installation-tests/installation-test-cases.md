# Installation Test Cases

## 📋 Test Category: Installation and Setup

**Priority**: High  
**Test Count**: 15 test cases  
**Estimated Duration**: 30 minutes per platform  
**Prerequisites**: Clean VS Code installation  

---

## 🎯 Test Case IN-001: Extension Installation from VSIX

**Objective**: Verify extension installs correctly from VSIX package

**Prerequisites**:
- VS Code installed
- VSIX package available
- No previous version installed

**Test Steps**:
1. Open VS Code
2. Go to Extensions view (Ctrl+Shift+X)
3. Click "..." menu → "Install from VSIX..."
4. Select `vscode-prolog-toolkit-1.3.0.vsix`
5. Wait for installation to complete

**Expected Results**:
- ✅ Installation completes without errors
- ✅ Extension appears in installed extensions list
- ✅ Extension is enabled by default
- ✅ No error notifications displayed

**Pass/Fail Criteria**:
- **Pass**: Extension installs and activates successfully
- **Fail**: Installation fails or extension doesn't appear

---

## 🎯 Test Case IN-002: Extension Activation

**Objective**: Verify extension activates when Prolog files are opened

**Test Steps**:
1. Create a new file with `.pl` extension
2. Open the file in VS Code
3. Check extension status in Extensions view
4. Look for Prolog-specific features

**Expected Results**:
- ✅ Extension activates automatically
- ✅ Syntax highlighting appears
- ✅ Activity bar icon becomes visible
- ✅ Extension status shows "Active"

**Pass/Fail Criteria**:
- **Pass**: Extension activates with Prolog file
- **Fail**: Extension remains inactive

---

## 🎯 Test Case IN-003: SWI-Prolog Detection - Installed

**Objective**: Test detection when SWI-Prolog is properly installed

**Prerequisites**: SWI-Prolog installed in standard location

**Test Steps**:
1. Ensure SWI-Prolog is installed
2. Open VS Code with extension
3. Open activity bar panel
4. Check installation status

**Expected Results**:
- ✅ Green checkmark in installation status
- ✅ Correct version number displayed
- ✅ Installation path shown
- ✅ "Test Installation" button works

**Pass/Fail Criteria**:
- **Pass**: SWI-Prolog detected correctly
- **Fail**: Not detected despite being installed

---

## 🎯 Test Case IN-004: SWI-Prolog Detection - Not Installed

**Objective**: Test behavior when SWI-Prolog is not installed

**Prerequisites**: SWI-Prolog not installed or not in PATH

**Test Steps**:
1. Ensure SWI-Prolog is not available
2. Open VS Code with extension
3. Check installation status
4. Try to use Prolog features

**Expected Results**:
- ✅ Red X in installation status
- ✅ "SWI-Prolog Not Found" message
- ✅ Setup wizard button available
- ✅ Helpful error messages for Prolog operations

**Pass/Fail Criteria**:
- **Pass**: Missing installation detected correctly
- **Fail**: False positive or unhelpful messages

---

## 🎯 Test Case IN-005: Auto-Detection Functionality

**Objective**: Test automatic path detection feature

**Prerequisites**: SWI-Prolog installed but not in configured path

**Test Steps**:
1. Install SWI-Prolog in non-standard location
2. Open activity bar panel
3. Click "Auto-Detect Path" button
4. Verify results

**Expected Results**:
- ✅ Auto-detection finds SWI-Prolog
- ✅ Configuration updated automatically
- ✅ Success message displayed
- ✅ Installation status updates to green

**Pass/Fail Criteria**:
- **Pass**: Auto-detection works correctly
- **Fail**: Fails to find or configure SWI-Prolog

---

## 🎯 Test Case IN-006: Setup Wizard Launch

**Objective**: Test setup wizard functionality

**Test Steps**:
1. Click "Setup Wizard" button in activity bar
2. Follow wizard steps
3. Complete setup process

**Expected Results**:
- ✅ Wizard opens in new panel/window
- ✅ Clear step-by-step instructions
- ✅ Platform-specific guidance
- ✅ Links to download SWI-Prolog
- ✅ Configuration options available

**Pass/Fail Criteria**:
- **Pass**: Wizard provides helpful setup guidance
- **Fail**: Wizard doesn't open or is unhelpful

---

## 🎯 Test Case IN-007: Manual Path Configuration

**Objective**: Test manual SWI-Prolog path configuration

**Test Steps**:
1. Open VS Code settings
2. Navigate to Prolog extension settings
3. Set `prolog.executablePath` to SWI-Prolog location
4. Restart VS Code or reload window

**Expected Results**:
- ✅ Setting accepts valid path
- ✅ Installation status updates
- ✅ Prolog features become available
- ✅ Path validation works

**Pass/Fail Criteria**:
- **Pass**: Manual configuration works
- **Fail**: Setting ignored or validation fails

---

## 🎯 Test Case IN-008: Installation Test Feature

**Objective**: Test the "Test Installation" functionality

**Prerequisites**: SWI-Prolog configured

**Test Steps**:
1. Click "Test Installation" button
2. Wait for test to complete
3. Observe results

**Expected Results**:
- ✅ Test executes quickly (< 5 seconds)
- ✅ Success message with version info
- ✅ Or clear error message if test fails
- ✅ No crashes or hangs

**Pass/Fail Criteria**:
- **Pass**: Test provides clear results
- **Fail**: Test fails to run or gives unclear results

---

## 🎯 Test Case IN-009: Configuration Migration

**Objective**: Test migration from previous extension versions

**Prerequisites**: Previous version configuration exists

**Test Steps**:
1. Install extension over previous version
2. Check if old settings are preserved
3. Verify new settings are added
4. Test functionality with migrated config

**Expected Results**:
- ✅ Old settings preserved where applicable
- ✅ New settings added with defaults
- ✅ No configuration conflicts
- ✅ Migration message displayed (if applicable)

**Pass/Fail Criteria**:
- **Pass**: Smooth migration without data loss
- **Fail**: Settings lost or conflicts occur

---

## 🎯 Test Case IN-010: Platform-Specific Paths (Windows)

**Objective**: Test Windows-specific installation detection

**Prerequisites**: Windows system

**Test Steps**:
1. Install SWI-Prolog using Windows installer
2. Test detection in common Windows locations:
   - `C:\Program Files\swipl\bin\swipl.exe`
   - `C:\Program Files (x86)\swipl\bin\swipl.exe`
3. Test with different installation methods

**Expected Results**:
- ✅ Detects standard Windows installation
- ✅ Handles Program Files vs Program Files (x86)
- ✅ Works with Chocolatey installation
- ✅ Works with Winget installation

**Pass/Fail Criteria**:
- **Pass**: Detects Windows installations correctly
- **Fail**: Fails to detect standard Windows installs

---

## 🎯 Test Case IN-011: Platform-Specific Paths (macOS)

**Objective**: Test macOS-specific installation detection

**Prerequisites**: macOS system

**Test Steps**:
1. Test with Homebrew installation
2. Test with MacPorts installation
3. Test with manual installation
4. Check common macOS paths

**Expected Results**:
- ✅ Detects Homebrew installation (`/usr/local/bin/swipl`)
- ✅ Detects Apple Silicon Homebrew (`/opt/homebrew/bin/swipl`)
- ✅ Detects MacPorts installation
- ✅ Handles application bundle installations

**Pass/Fail Criteria**:
- **Pass**: Detects macOS installations correctly
- **Fail**: Fails to detect standard macOS installs

---

## 🎯 Test Case IN-012: Platform-Specific Paths (Linux)

**Objective**: Test Linux-specific installation detection

**Prerequisites**: Linux system

**Test Steps**:
1. Test with package manager installation
2. Test with snap installation
3. Test with flatpak installation
4. Test manual compilation

**Expected Results**:
- ✅ Detects apt/yum/dnf installations
- ✅ Detects snap installations
- ✅ Detects flatpak installations
- ✅ Finds manually compiled versions

**Pass/Fail Criteria**:
- **Pass**: Detects Linux installations correctly
- **Fail**: Fails to detect standard Linux installs

---

## 🎯 Test Case IN-013: Permission Issues

**Objective**: Test handling of permission-related installation issues

**Test Steps**:
1. Install SWI-Prolog with restricted permissions
2. Test detection and execution
3. Observe error handling

**Expected Results**:
- ✅ Detects permission issues
- ✅ Provides helpful error messages
- ✅ Suggests solutions (run as admin, etc.)
- ✅ Graceful degradation of features

**Pass/Fail Criteria**:
- **Pass**: Handles permissions gracefully
- **Fail**: Crashes or provides unhelpful errors

---

## 🎯 Test Case IN-014: Network/Offline Installation

**Objective**: Test installation behavior without network access

**Test Steps**:
1. Disconnect from network
2. Install extension from VSIX
3. Test basic functionality
4. Reconnect and test online features

**Expected Results**:
- ✅ Extension installs offline
- ✅ Basic features work without network
- ✅ Online features gracefully degrade
- ✅ Features restore when network returns

**Pass/Fail Criteria**:
- **Pass**: Works offline with graceful degradation
- **Fail**: Requires network for basic functionality

---

## 🎯 Test Case IN-015: Uninstallation

**Objective**: Test clean extension uninstallation

**Test Steps**:
1. Uninstall extension from Extensions view
2. Restart VS Code
3. Check for leftover files/settings
4. Reinstall and verify clean state

**Expected Results**:
- ✅ Extension uninstalls completely
- ✅ Activity bar icon disappears
- ✅ No leftover UI elements
- ✅ Settings can be optionally preserved
- ✅ Clean reinstallation possible

**Pass/Fail Criteria**:
- **Pass**: Clean uninstallation
- **Fail**: Leftover elements or reinstall issues

---

## 📊 Installation Test Summary

| Test Case | Windows | macOS | Linux | Notes |
|-----------|---------|-------|-------|-------|
| IN-001 | ⏳ | ⏳ | ⏳ | |
| IN-002 | ⏳ | ⏳ | ⏳ | |
| IN-003 | ⏳ | ⏳ | ⏳ | |
| IN-004 | ⏳ | ⏳ | ⏳ | |
| IN-005 | ⏳ | ⏳ | ⏳ | |
| IN-006 | ⏳ | ⏳ | ⏳ | |
| IN-007 | ⏳ | ⏳ | ⏳ | |
| IN-008 | ⏳ | ⏳ | ⏳ | |
| IN-009 | ⏳ | ⏳ | ⏳ | |
| IN-010 | ⏳ | N/A | N/A | Windows only |
| IN-011 | N/A | ⏳ | N/A | macOS only |
| IN-012 | N/A | N/A | ⏳ | Linux only |
| IN-013 | ⏳ | ⏳ | ⏳ | |
| IN-014 | ⏳ | ⏳ | ⏳ | |
| IN-015 | ⏳ | ⏳ | ⏳ | |

**Legend**: ✅ Pass | ❌ Fail | ⏳ Pending | ⚠️ Blocked | N/A Not Applicable

---

## 🔧 Installation Troubleshooting Guide

### Common Installation Issues

**Issue**: Extension doesn't appear after installation
- **Solution**: Restart VS Code, check Extensions view

**Issue**: SWI-Prolog not detected despite being installed
- **Solution**: Check PATH, use auto-detect, or configure manually

**Issue**: Permission denied errors
- **Solution**: Run VS Code as administrator (Windows) or check file permissions

**Issue**: Installation fails on corporate networks
- **Solution**: Check proxy settings, install offline from VSIX

### Platform-Specific Notes

**Windows**:
- May require administrator privileges for some operations
- Check both Program Files locations
- Consider Windows Defender/antivirus interference

**macOS**:
- Different paths for Intel vs Apple Silicon Macs
- May need to allow unsigned extensions
- Check Gatekeeper settings

**Linux**:
- Verify package manager installations
- Check snap/flatpak permissions
- Consider AppArmor/SELinux restrictions

---

**Test Suite Version**: 1.0.0  
**Last Updated**: January 2025  
**Next Review**: Before next release