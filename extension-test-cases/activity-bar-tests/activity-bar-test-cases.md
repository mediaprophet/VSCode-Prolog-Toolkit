# Activity Bar Test Cases

## 📋 Test Category: Activity Bar UI and Functionality

**Priority**: High  
**Test Count**: 25 test cases  
**Estimated Duration**: 45 minutes  
**Prerequisites**: Extension installed, VS Code running  

---

## 🎯 Test Case AB-001: Activity Bar Icon Display

**Objective**: Verify the custom Prolog activity bar icon appears correctly

**Prerequisites**:
- Extension installed and activated
- VS Code restarted after installation

**Test Steps**:
1. Open VS Code
2. Look at the activity bar (left sidebar)
3. Locate the Prolog Toolkit icon

**Expected Results**:
- ✅ Custom Prolog SVG icon is visible in activity bar
- ✅ Icon displays clearly at different zoom levels
- ✅ Icon has proper tooltip "Prolog Toolkit"
- ✅ Icon is positioned correctly in activity bar

**Pass/Fail Criteria**:
- **Pass**: Icon visible and functional
- **Fail**: Icon missing, corrupted, or not clickable

---

## 🎯 Test Case AB-002: Activity Bar Panel Opening

**Objective**: Verify clicking the activity bar icon opens the Prolog panel

**Test Steps**:
1. Click the Prolog Toolkit icon in activity bar
2. Observe the side panel that opens

**Expected Results**:
- ✅ Side panel opens immediately
- ✅ Panel title shows "Prolog Toolkit"
- ✅ Panel contains multiple sections
- ✅ No error messages displayed

**Pass/Fail Criteria**:
- **Pass**: Panel opens with expected content
- **Fail**: Panel doesn't open or shows errors

---

## 🎯 Test Case AB-003: Dashboard Section Display

**Objective**: Verify the dashboard section loads and displays correctly

**Test Steps**:
1. Open Prolog Toolkit panel
2. Locate the "Dashboard" section
3. Observe the dashboard content

**Expected Results**:
- ✅ Dashboard section is visible and expanded
- ✅ Installation status card is displayed
- ✅ Quick query interface is present
- ✅ Recent queries section is shown
- ✅ Quick actions grid is visible

**Pass/Fail Criteria**:
- **Pass**: All dashboard elements present and styled correctly
- **Fail**: Missing elements or styling issues

---

## 🎯 Test Case AB-004: Installation Status Display

**Objective**: Verify installation status is correctly detected and displayed

**Test Steps**:
1. Open dashboard section
2. Observe the installation status card
3. Note the status indicator and details

**Expected Results**:
**With SWI-Prolog installed**:
- ✅ Green checkmark icon
- ✅ "SWI-Prolog Installed" message
- ✅ Version number displayed
- ✅ Installation path shown

**Without SWI-Prolog**:
- ✅ Red X icon
- ✅ "SWI-Prolog Not Found" message
- ✅ Setup wizard button available

**Pass/Fail Criteria**:
- **Pass**: Status accurately reflects actual installation
- **Fail**: Incorrect status or missing information

---

## 🎯 Test Case AB-005: Quick Query Interface

**Objective**: Test the quick query functionality in the dashboard

**Prerequisites**: SWI-Prolog installed and working

**Test Steps**:
1. Locate the quick query input field
2. Enter a simple query: `member(X, [1,2,3])`
3. Click the "Execute" button
4. Observe the results

**Expected Results**:
- ✅ Input field accepts text
- ✅ Execute button is clickable
- ✅ Query executes successfully
- ✅ Results are displayed clearly
- ✅ Variable bindings shown correctly

**Pass/Fail Criteria**:
- **Pass**: Query executes and shows correct results
- **Fail**: Query fails or shows incorrect results

---

## 🎯 Test Case AB-006: Example Query Buttons

**Objective**: Test the example query buttons functionality

**Test Steps**:
1. Locate the example query buttons
2. Click on "member(X, [1,2,3])" button
3. Observe the query input field
4. Click execute

**Expected Results**:
- ✅ Query text appears in input field
- ✅ Query can be executed immediately
- ✅ Results match expected output

**Pass/Fail Criteria**:
- **Pass**: Example queries work correctly
- **Fail**: Queries don't populate or execute

---

## 🎯 Test Case AB-007: Prolog Explorer Tree View

**Objective**: Verify the Prolog Explorer tree view displays correctly

**Test Steps**:
1. Open Prolog Toolkit panel
2. Locate "Prolog Explorer" section
3. Expand the tree view
4. Examine the tree structure

**Expected Results**:
- ✅ Tree view is expandable/collapsible
- ✅ Installation node shows correct status
- ✅ Files node lists workspace Prolog files
- ✅ Settings node provides configuration access
- ✅ Icons are displayed correctly for each node

**Pass/Fail Criteria**:
- **Pass**: Tree view functional with correct content
- **Fail**: Tree view broken or missing content

---

## 🎯 Test Case AB-008: Query History Section

**Objective**: Test the query history display and functionality

**Prerequisites**: Some queries executed previously

**Test Steps**:
1. Execute a few test queries using the quick query interface
2. Locate the "Query History" section
3. Examine the history entries
4. Try clicking on a history item

**Expected Results**:
- ✅ Recent queries are listed
- ✅ Success/failure status indicated
- ✅ Timestamps are shown
- ✅ Clicking history item reruns query
- ✅ Clear history button works

**Pass/Fail Criteria**:
- **Pass**: History tracks and displays queries correctly
- **Fail**: History missing or not functional

---

## 🎯 Test Case AB-009: Workspace Files Display

**Objective**: Verify workspace Prolog files are detected and displayed

**Prerequisites**: Workspace with .pl files

**Test Steps**:
1. Open a workspace containing Prolog files
2. Check the "Prolog Files" section
3. Observe the file list
4. Click on a file entry

**Expected Results**:
- ✅ All .pl, .pro, .prolog files are listed
- ✅ File names and paths are correct
- ✅ File icons are displayed
- ✅ Clicking opens the file
- ✅ "New File" button is available

**Pass/Fail Criteria**:
- **Pass**: Files detected and clickable
- **Fail**: Files missing or not openable

---

## 🎯 Test Case AB-010: Quick Actions Grid

**Objective**: Test the quick actions functionality

**Test Steps**:
1. Locate the quick actions grid
2. Test each action button:
   - Settings
   - Setup Wizard
   - New File
   - Documentation

**Expected Results**:
- ✅ All action buttons are visible
- ✅ Settings opens configuration
- ✅ Setup wizard launches
- ✅ New file creates Prolog file
- ✅ Documentation opens in browser

**Pass/Fail Criteria**:
- **Pass**: All actions work as expected
- **Fail**: Any action fails or errors

---

## 🎯 Test Case AB-011: Refresh Functionality

**Objective**: Test the refresh button and auto-refresh behavior

**Test Steps**:
1. Click the refresh button in dashboard header
2. Change SWI-Prolog configuration
3. Observe if status updates automatically

**Expected Results**:
- ✅ Refresh button updates all sections
- ✅ Status changes are reflected
- ✅ File list updates when workspace changes
- ✅ No errors during refresh

**Pass/Fail Criteria**:
- **Pass**: Refresh works correctly
- **Fail**: Refresh fails or shows stale data

---

## 🎯 Test Case AB-012: Theme Compatibility

**Objective**: Verify activity bar works with different VS Code themes

**Test Steps**:
1. Switch to Dark theme
2. Observe activity bar appearance
3. Switch to Light theme
4. Switch to High Contrast theme
5. Test custom themes

**Expected Results**:
- ✅ Icons adapt to theme colors
- ✅ Text remains readable
- ✅ Backgrounds use theme colors
- ✅ No visual artifacts or overlaps

**Pass/Fail Criteria**:
- **Pass**: Works well with all themes
- **Fail**: Visual issues with any theme

---

## 🎯 Test Case AB-013: Responsive Layout

**Objective**: Test activity bar behavior at different panel sizes

**Test Steps**:
1. Resize the activity bar panel to minimum width
2. Resize to maximum width
3. Test with different VS Code window sizes

**Expected Results**:
- ✅ Content adapts to panel width
- ✅ Text wraps appropriately
- ✅ Buttons remain clickable
- ✅ No horizontal scrollbars
- ✅ Layout remains functional

**Pass/Fail Criteria**:
- **Pass**: Responsive design works correctly
- **Fail**: Layout breaks at any size

---

## 🎯 Test Case AB-014: Command Integration

**Objective**: Verify activity bar commands work via Command Palette

**Test Steps**:
1. Open Command Palette (Ctrl+Shift+P)
2. Search for "Prolog:" commands
3. Test each new command:
   - Prolog: Refresh Installation Status
   - Prolog: Test Installation
   - Prolog: Auto-Detect Path
   - Prolog: New File

**Expected Results**:
- ✅ All commands appear in palette
- ✅ Commands execute without errors
- ✅ Results are reflected in activity bar
- ✅ Keyboard shortcuts work (if defined)

**Pass/Fail Criteria**:
- **Pass**: All commands functional
- **Fail**: Any command missing or broken

---

## 🎯 Test Case AB-015: Error Handling

**Objective**: Test error handling in activity bar components

**Test Steps**:
1. Disconnect from network (if applicable)
2. Corrupt SWI-Prolog installation
3. Try operations that might fail
4. Observe error messages

**Expected Results**:
- ✅ Graceful error messages displayed
- ✅ No crashes or freezes
- ✅ Recovery options provided
- ✅ User can continue using extension

**Pass/Fail Criteria**:
- **Pass**: Errors handled gracefully
- **Fail**: Crashes or unhelpful errors

---

## 🎯 Test Case AB-016: Performance

**Objective**: Verify activity bar performance is acceptable

**Test Steps**:
1. Measure time to open activity bar panel
2. Test responsiveness with large workspaces
3. Monitor memory usage
4. Test with many query history items

**Expected Results**:
- ✅ Panel opens in < 1 second
- ✅ Responsive with 100+ files
- ✅ Memory usage reasonable
- ✅ No noticeable lag in interactions

**Pass/Fail Criteria**:
- **Pass**: Performance meets benchmarks
- **Fail**: Slow or unresponsive

---

## 🎯 Test Case AB-017: Accessibility

**Objective**: Test accessibility features

**Test Steps**:
1. Navigate using only keyboard
2. Test with screen reader (if available)
3. Check color contrast
4. Test focus indicators

**Expected Results**:
- ✅ All elements keyboard accessible
- ✅ Proper ARIA labels
- ✅ Good color contrast
- ✅ Clear focus indicators

**Pass/Fail Criteria**:
- **Pass**: Meets accessibility standards
- **Fail**: Accessibility issues found

---

## 🎯 Test Case AB-018: Context Menus

**Objective**: Test right-click context menus in tree views

**Test Steps**:
1. Right-click on different tree view items
2. Test context menu options
3. Verify actions work correctly

**Expected Results**:
- ✅ Context menus appear
- ✅ Relevant options shown
- ✅ Actions execute correctly
- ✅ Menus dismiss properly

**Pass/Fail Criteria**:
- **Pass**: Context menus functional
- **Fail**: Menus missing or broken

---

## 🎯 Test Case AB-019: Multi-Workspace Support

**Objective**: Test activity bar with multiple workspaces

**Test Steps**:
1. Open multiple workspace folders
2. Switch between workspaces
3. Verify file lists update
4. Test workspace-specific settings

**Expected Results**:
- ✅ File lists update per workspace
- ✅ Settings respect workspace scope
- ✅ No cross-workspace contamination
- ✅ Performance remains good

**Pass/Fail Criteria**:
- **Pass**: Multi-workspace support works
- **Fail**: Issues with multiple workspaces

---

## 🎯 Test Case AB-020: Extension Lifecycle

**Objective**: Test activity bar during extension lifecycle events

**Test Steps**:
1. Disable extension
2. Re-enable extension
3. Update extension
4. Uninstall/reinstall extension

**Expected Results**:
- ✅ Activity bar disappears when disabled
- ✅ Reappears when re-enabled
- ✅ Survives extension updates
- ✅ Clean uninstall/reinstall

**Pass/Fail Criteria**:
- **Pass**: Handles lifecycle correctly
- **Fail**: Issues during lifecycle events

---

## 📊 Test Summary Template

| Test Case | Status | Notes | Tester | Date |
|-----------|--------|-------|--------|------|
| AB-001 | ⏳ | | | |
| AB-002 | ⏳ | | | |
| AB-003 | ⏳ | | | |
| AB-004 | ⏳ | | | |
| AB-005 | ⏳ | | | |
| AB-006 | ⏳ | | | |
| AB-007 | ⏳ | | | |
| AB-008 | ⏳ | | | |
| AB-009 | ⏳ | | | |
| AB-010 | ⏳ | | | |
| AB-011 | ⏳ | | | |
| AB-012 | ⏳ | | | |
| AB-013 | ⏳ | | | |
| AB-014 | ⏳ | | | |
| AB-015 | ⏳ | | | |
| AB-016 | ⏳ | | | |
| AB-017 | ⏳ | | | |
| AB-018 | ⏳ | | | |
| AB-019 | ⏳ | | | |
| AB-020 | ⏳ | | | |

**Legend**: ✅ Pass | ❌ Fail | ⏳ Pending | ⚠️ Blocked

---

## 🐛 Common Issues and Troubleshooting

### Issue: Activity bar icon not visible
**Solution**: Check extension is activated, restart VS Code

### Issue: Dashboard not loading
**Solution**: Check developer console for errors, verify file paths

### Issue: Queries not executing
**Solution**: Verify SWI-Prolog installation, check backend status

### Issue: Files not detected
**Solution**: Ensure workspace contains .pl files, refresh panel

---

**Test Suite Version**: 1.0.0  
**Last Updated**: January 2025  
**Next Review**: Before next release