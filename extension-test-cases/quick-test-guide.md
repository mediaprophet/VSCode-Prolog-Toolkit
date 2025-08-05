# Quick Test Execution Guide

## 🚀 Quick Start Testing

This guide provides a streamlined approach to test the most critical functionality of the VSCode Prolog Toolkit extension.

## ⏱️ 15-Minute Smoke Test

### Prerequisites (2 minutes)
1. Install extension from VSIX: `code --install-extension vscode-prolog-toolkit-1.3.0.vsix`
2. Restart VS Code
3. Open a workspace or create a new folder

### Critical Path Tests (13 minutes)

#### 1. Extension Activation (2 minutes)
- [ ] **AB-001**: Activity bar icon visible
- [ ] **AB-002**: Click icon opens Prolog panel
- [ ] **IN-002**: Extension activates with .pl file

**Quick Test**: Create `test.pl` file, verify activity bar icon appears

#### 2. Installation Detection (3 minutes)
- [ ] **IN-003/IN-004**: Installation status correct
- [ ] **AB-004**: Status displayed in dashboard
- [ ] **IN-008**: Test installation button works

**Quick Test**: Check installation status in activity bar dashboard

#### 3. Activity Bar Functionality (4 minutes)
- [ ] **AB-003**: Dashboard loads completely
- [ ] **AB-005**: Quick query works (if SWI-Prolog installed)
- [ ] **AB-009**: Workspace files detected
- [ ] **AB-010**: Quick actions functional

**Quick Test**: Execute `member(X, [1,2,3])` in quick query interface

#### 4. Core Features (3 minutes)
- [ ] **CF-001**: Syntax highlighting works
- [ ] **CF-005**: Basic completion available
- [ ] **CF-015**: File operations work

**Quick Test**: Type Prolog code, verify syntax coloring and completion

#### 5. Error Handling (1 minute)
- [ ] **AB-015**: Graceful error handling
- [ ] **IN-004**: Helpful messages when SWI-Prolog missing

**Quick Test**: Try operations without SWI-Prolog, verify error messages

### Pass/Fail Criteria
- **PASS**: All 5 areas work without critical errors
- **FAIL**: Any critical functionality broken

---

## 🧪 30-Minute Comprehensive Test

### Extended Testing Areas

#### Installation & Setup (8 minutes)
- Run installation test cases IN-001 through IN-008
- Test auto-detection and manual configuration
- Verify setup wizard functionality

#### Activity Bar Deep Dive (12 minutes)
- Run activity bar test cases AB-001 through AB-015
- Test all dashboard features
- Verify tree view functionality
- Test command integration

#### Core Functionality (10 minutes)
- Test syntax highlighting with complex files
- Verify debugging features (if SWI-Prolog available)
- Test query execution and history
- Verify file operations and workspace integration

---

## 🔧 Platform-Specific Testing

### Windows Testing Checklist
- [ ] Extension installs via VSIX
- [ ] SWI-Prolog detection in Program Files
- [ ] Path handling with backslashes
- [ ] PowerShell integration works

### macOS Testing Checklist  
- [ ] Extension installs via VSIX
- [ ] Homebrew SWI-Prolog detection
- [ ] Unix path handling
- [ ] Terminal integration works

### Linux Testing Checklist
- [ ] Extension installs via VSIX
- [ ] Package manager SWI-Prolog detection
- [ ] Snap/Flatpak detection
- [ ] Shell integration works

---

## 🐛 Common Issues and Quick Fixes

### Issue: Activity bar icon not visible
**Quick Fix**: Restart VS Code, check Extensions view

### Issue: Dashboard not loading
**Quick Fix**: Open Developer Console (F12), check for errors

### Issue: SWI-Prolog not detected
**Quick Fix**: Use auto-detect button or configure path manually

### Issue: Queries not executing
**Quick Fix**: Verify SWI-Prolog installation, check backend status

### Issue: Files not appearing in workspace
**Quick Fix**: Ensure files have .pl extension, refresh panel

---

## 📊 Test Results Quick Log

### Smoke Test Results
| Test Area | Status | Notes |
|-----------|--------|-------|
| Extension Activation | ⏳ | |
| Installation Detection | ⏳ | |
| Activity Bar | ⏳ | |
| Core Features | ⏳ | |
| Error Handling | ⏳ | |

### Environment Tested
- **OS**: ________________
- **VS Code**: ________________
- **SWI-Prolog**: ________________
- **Date**: ________________
- **Tester**: ________________

### Overall Assessment
- [ ] **READY FOR RELEASE** - All critical tests pass
- [ ] **NEEDS WORK** - Issues found that need fixing
- [ ] **BLOCKED** - Cannot complete testing due to environment issues

### Critical Issues Found
1. ________________________________
2. ________________________________
3. ________________________________

---

## 📋 Next Steps

### If Tests Pass
1. Document successful test execution
2. Proceed with release preparation
3. Update test results in main test suite

### If Tests Fail
1. Log bugs using bug report template
2. Prioritize fixes based on severity
3. Retest after fixes are implemented

### For Comprehensive Testing
1. Use full test suite in respective folders
2. Follow detailed test case instructions
3. Complete test execution report template

---

**Quick Test Guide Version**: 1.0  
**Last Updated**: January 2025  
**Compatible with Extension**: 1.3.0