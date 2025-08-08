# Fresh Installation Demo

## 🚀 Complete New User Experience Showcase

**Duration**: 20 minutes  
**Audience**: New users, evaluators, system administrators  
**Goal**: Demonstrate seamless installation and setup experience from zero to productive development  

---

## 📋 Demo Overview

This demo showcases the complete new user experience, from initial extension installation through first productive use. It demonstrates the extension's intelligent installation detection, guided setup wizard, and automatic configuration capabilities that make getting started with Prolog development effortless.

### **Key Features Demonstrated**
- Extension installation and activation
- Automatic SWI-Prolog detection across platforms
- Intelligent setup wizard with guided configuration
- Installation status monitoring and validation
- Error recovery and troubleshooting assistance
- First-time user onboarding experience

---

## 🎬 Demo Script

### **Phase 1: Clean Environment Setup (3 minutes)**

#### **1.1 Prepare Clean Environment**
1. **Uninstall Extension** (if present)
   ```bash
   code --uninstall-extension mediaprophet.vscode-prolog-toolkit
   ```
   - Show clean VS Code without Prolog extension
   - Verify no Prolog activity bar icon
   - **Key Message**: "Starting from completely fresh state"

2. **Verify SWI-Prolog Status**
   ```bash
   swipl --version
   ```
   - Check if SWI-Prolog is installed
   - Note installation status for demo narrative
   - **Key Message**: "Real-world installation scenarios"

#### **1.2 Demonstrate Without Extension**
1. **Open Prolog File Without Extension**
   - Create or open a `.pl` file
   - Show lack of syntax highlighting
   - No language support or IntelliSense
   - **Key Message**: "The problem we're solving"

#### **Expected Results**
- ✅ Clean environment with no Prolog support
- ✅ Clear demonstration of need for extension
- ✅ Baseline established for comparison

### **Phase 2: Extension Installation (4 minutes)**

#### **2.1 Install from VSIX (Recommended for Demos)**
1. **Install Extension**
   ```bash
   code --install-extension vscode-prolog-toolkit-1.3.0.vsix
   ```
   - Show installation progress
   - Point out installation success message
   - **Key Message**: "Simple, reliable installation"

2. **Alternative: Marketplace Installation**
   - Show Extensions view (Ctrl+Shift+X)
   - Search for "VSCode Prolog Toolkit"
   - Click Install button
   - **Key Message**: "Multiple installation options"

#### **2.2 First Activation**
1. **Restart VS Code**
   - Close and reopen VS Code
   - Show extension activation process
   - **Key Message**: "Clean activation process"

2. **Initial Extension Activation**
   - Open a `.pl` file or create new one
   - Show extension activates automatically
   - Point out activity bar icon appears
   - **Key Message**: "Automatic activation on first use"

#### **Expected Results**
- ✅ Extension installs without errors
- ✅ Activity bar icon appears after restart
- ✅ Extension activates on Prolog file interaction
- ✅ No immediate error messages

### **Phase 3: Automatic Detection and Setup (6 minutes)**

#### **3.1 Installation Status Detection**
1. **Open Activity Bar Panel**
   - Click Prolog Toolkit icon in activity bar
   - Show dashboard loading
   - Point out installation status card

2. **Scenario A: SWI-Prolog Already Installed**
   ```
   📊 Installation Status
   ✅ SWI-Prolog Installed
   Version: 9.0.4
   Path: /usr/local/bin/swipl
   Last Checked: Just now
   ```
   - Show green checkmark and version info
   - Point out automatic path detection
   - **Key Message**: "Zero configuration for standard installations"

3. **Scenario B: SWI-Prolog Not Found**
   ```
   📊 Installation Status
   ❌ SWI-Prolog Not Found
   The extension requires SWI-Prolog to function properly.
   [Install SWI-Prolog] [Setup Wizard] [Manual Configuration]
   ```
   - Show red X and helpful message
   - Point out action buttons
   - **Key Message**: "Clear guidance when setup needed"

#### **3.2 Auto-Detection Process**
1. **Demonstrate Auto-Detection**
   - Click "Auto-Detect Path" button
   - Show detection process with progress indicator
   - Display results of detection scan

2. **Detection Results**
   ```
   🔍 Auto-Detection Results
   Found SWI-Prolog installations:
   ✅ /usr/local/bin/swipl (v9.0.4) - Homebrew
   ✅ /opt/homebrew/bin/swipl (v9.0.4) - Homebrew (Apple Silicon)
   ❌ /usr/bin/swipl - Not found
   
   [Use /usr/local/bin/swipl] [Use /opt/homebrew/bin/swipl] [Manual Path]
   ```
   - Show multiple installation detection
   - Point out platform-specific paths
   - **Key Message**: "Intelligent multi-location detection"

#### **Expected Results**
- ✅ Installation status accurately detected
- ✅ Auto-detection finds available installations
- ✅ Clear action buttons for next steps
- ✅ Platform-specific paths recognized

### **Phase 4: Setup Wizard Experience (5 minutes)**

#### **4.1 Launch Setup Wizard**
1. **Start Setup Wizard**
   - Click "Setup Wizard" button in dashboard
   - Or use Command Palette: "Prolog: Setup Wizard"
   - Show wizard welcome screen

2. **Wizard Welcome**
   ```
   🧙 VSCode Prolog Toolkit Setup Wizard
   
   Welcome! This wizard will help you configure the Prolog extension
   for optimal performance on your system.
   
   Platform Detected: macOS (Apple Silicon)
   VS Code Version: 1.102.0
   
   [Continue] [Skip Setup]
   ```
   - Point out platform detection
   - Show professional wizard interface
   - **Key Message**: "Guided, intelligent setup"

#### **4.2 Step-by-Step Configuration**
1. **Step 1: SWI-Prolog Detection**
   ```
   Step 1 of 4: SWI-Prolog Installation
   
   🔍 Scanning for SWI-Prolog installations...
   
   Found: /opt/homebrew/bin/swipl (v9.0.4)
   Status: ✅ Working correctly
   
   Would you like to use this installation?
   [Yes, Use This] [Browse for Different Path] [Install SWI-Prolog]
   ```
   - Show automatic detection in wizard
   - Point out validation process
   - **Key Message**: "Validation ensures working setup"

2. **Step 2: Configuration Validation**
   ```
   Step 2 of 4: Configuration Validation
   
   Testing SWI-Prolog configuration...
   ✅ Executable found and accessible
   ✅ Version compatible (9.0.4 >= 8.0.0)
   ✅ Basic query test successful
   ✅ Path permissions correct
   
   Configuration looks good!
   [Continue] [Advanced Settings]
   ```
   - Show comprehensive validation
   - Point out multiple checks performed
   - **Key Message**: "Thorough validation prevents issues"

3. **Step 3: Feature Configuration**
   ```
   Step 3 of 4: Feature Configuration
   
   Choose which features to enable:
   ✅ Syntax highlighting and completion
   ✅ Linting and error detection
   ✅ Chat assistant with AI support
   ✅ Activity bar dashboard
   ☐ API server (for external tools)
   ☐ Advanced debugging features
   
   [Continue] [Select All] [Minimal Setup]
   ```
   - Show feature selection options
   - Point out recommended defaults
   - **Key Message**: "Customizable feature set"

4. **Step 4: Completion**
   ```
   Step 4 of 4: Setup Complete!
   
   🎉 Your Prolog development environment is ready!
   
   Configuration Summary:
   • SWI-Prolog: /opt/homebrew/bin/swipl (v9.0.4)
   • Features: Full feature set enabled
   • Status: All systems operational
   
   [Finish Setup] [Test Installation] [Open Sample Project]
   ```
   - Show completion summary
   - Offer next steps
   - **Key Message**: "Ready for productive development"

#### **Expected Results**
- ✅ Setup wizard launches and guides through configuration
- ✅ Each step validates configuration properly
- ✅ Feature selection works correctly
- ✅ Final setup is functional and tested

### **Phase 5: First Use Experience (2 minutes)**

#### **5.1 Immediate Productivity**
1. **Create First Prolog File**
   - Click "Open Sample Project" or create new file
   - Show immediate syntax highlighting
   - Demonstrate code completion
   - **Key Message**: "Immediate productivity after setup"

2. **Test Core Features**
   ```prolog
   % First Prolog program
   parent(tom, bob).
   parent(tom, liz).
   
   father(X, Y) :- parent(X, Y), male(X).
   male(tom).
   ```
   - Show syntax highlighting in action
   - Test code completion with `father(`
   - **Key Message**: "Full IDE support immediately available"

#### **5.2 Quick Validation**
1. **Test Query Execution**
   - Use dashboard quick query: `parent(tom, X)`
   - Show results display correctly
   - **Key Message**: "End-to-end functionality working"

2. **Test Chat Assistant**
   ```
   @prolog member(X, [1,2,3])
   ```
   - Show chat assistant responds
   - Point out formatted results
   - **Key Message**: "Advanced features ready to use"

#### **Expected Results**
- ✅ Syntax highlighting works immediately
- ✅ Code completion functional
- ✅ Query execution successful
- ✅ Chat assistant responsive

---

## 🎯 Platform-Specific Demonstrations

### **Windows Installation Demo**

#### **Windows-Specific Features**
1. **Package Manager Integration**
   ```powershell
   # Show Chocolatey installation option
   choco install swi-prolog
   
   # Show Winget installation option  
   winget install SWI-Prolog.SWI-Prolog
   ```
   - Demonstrate package manager detection
   - Show installation guidance for Windows

2. **Path Detection**
   - Show detection of Program Files installations
   - Demonstrate handling of Windows path separators
   - Point out UAC and permission handling

#### **Windows-Specific Validation**
- ✅ PowerShell integration works
- ✅ Command Prompt compatibility
- ✅ Windows path handling correct
- ✅ UAC permissions handled gracefully

### **macOS Installation Demo**

#### **macOS-Specific Features**
1. **Homebrew Integration**
   ```bash
   # Show Homebrew installation detection
   brew install swi-prolog
   ```
   - Demonstrate Intel vs Apple Silicon path detection
   - Show automatic architecture handling

2. **App Bundle Detection**
   - Show detection of SWI-Prolog.app installations
   - Demonstrate handling of macOS app bundles

#### **macOS-Specific Validation**
- ✅ Homebrew paths detected correctly
- ✅ Apple Silicon vs Intel handling
- ✅ App bundle installations recognized
- ✅ Terminal integration functional

### **Linux Installation Demo**

#### **Linux-Specific Features**
1. **Package Manager Detection**
   ```bash
   # Show various package manager support
   sudo apt install swi-prolog      # Debian/Ubuntu
   sudo dnf install pl              # Fedora
   sudo pacman -S swi-prolog        # Arch
   sudo snap install swi-prolog     # Universal
   ```
   - Demonstrate multi-distro support
   - Show package manager detection

2. **Custom Installation Handling**
   - Show detection of compiled installations
   - Demonstrate handling of non-standard paths

#### **Linux-Specific Validation**
- ✅ Multiple package managers supported
- ✅ Distribution-specific paths detected
- ✅ Custom installations recognized
- ✅ Shell integration works across distributions

---

## 🔍 Error Scenarios and Recovery

### **Common Installation Issues**

#### **Scenario 1: SWI-Prolog Not Installed**
1. **Problem Demonstration**
   - Show "SWI-Prolog Not Found" status
   - Point out clear error messaging
   - **Key Message**: "Clear problem identification"

2. **Recovery Process**
   - Click "Install SWI-Prolog" button
   - Show platform-specific installation instructions
   - Demonstrate post-installation detection
   - **Key Message**: "Guided problem resolution"

#### **Scenario 2: Incorrect Path Configuration**
1. **Problem Demonstration**
   - Manually set incorrect SWI-Prolog path
   - Show validation failure
   - Point out specific error messages

2. **Recovery Process**
   - Use "Auto-Detect Path" feature
   - Show path correction
   - Demonstrate validation success
   - **Key Message**: "Automatic error recovery"

#### **Scenario 3: Permission Issues**
1. **Problem Demonstration**
   - Show permission-related errors
   - Point out helpful error messages

2. **Recovery Process**
   - Show permission troubleshooting guidance
   - Demonstrate alternative installation methods
   - **Key Message**: "Comprehensive troubleshooting support"

---

## 📊 Success Criteria

### **Installation Success Indicators**
- [ ] Extension installs without errors
- [ ] Activity bar icon appears and functions
- [ ] SWI-Prolog detection works correctly
- [ ] Setup wizard completes successfully
- [ ] All features activate properly

### **User Experience Indicators**
- [ ] Installation process is intuitive
- [ ] Error messages are helpful and actionable
- [ ] Setup wizard guides effectively
- [ ] Time to productivity is minimal (< 5 minutes)
- [ ] No technical expertise required

### **Technical Indicators**
- [ ] Cross-platform compatibility confirmed
- [ ] Multiple installation methods supported
- [ ] Configuration validation thorough
- [ ] Error recovery mechanisms effective
- [ ] Performance acceptable during setup

---

## 🛠️ Troubleshooting

### **Common Demo Issues**

#### **Extension Installation Fails**
- **Symptoms**: Installation error messages, extension not appearing
- **Causes**: VS Code version incompatibility, corrupted VSIX, permissions
- **Solutions**:
  1. Verify VS Code version (1.102.0+)
  2. Try marketplace installation instead of VSIX
  3. Restart VS Code with admin privileges (Windows)
  4. Clear VS Code extension cache
- **Demo Recovery**: Use marketplace installation, explain troubleshooting

#### **SWI-Prolog Not Detected Despite Installation**
- **Symptoms**: Red X status despite SWI-Prolog being installed
- **Causes**: Non-standard installation path, PATH environment issues
- **Solutions**:
  1. Use manual path configuration
  2. Check PATH environment variable
  3. Try different SWI-Prolog installation method
  4. Use setup wizard for guided configuration
- **Demo Recovery**: Show manual configuration, explain path issues

#### **Setup Wizard Fails**
- **Symptoms**: Wizard crashes or shows errors
- **Causes**: Backend startup issues, configuration conflicts
- **Solutions**:
  1. Restart VS Code and try again
  2. Use manual configuration instead
  3. Check SWI-Prolog installation independently
  4. Review VS Code developer console for errors
- **Demo Recovery**: Use manual configuration, show alternative setup

---

## 📝 Demo Variations

### **Quick Installation Demo (10 minutes)**
- Focus on successful installation scenario
- Show setup wizard briefly
- Demonstrate immediate productivity
- Skip troubleshooting scenarios

### **Comprehensive Setup Demo (30 minutes)**
- Include all platform-specific features
- Show multiple installation scenarios
- Demonstrate error recovery
- Include troubleshooting examples

### **Technical Setup Demo (25 minutes)**
- Explain detection algorithms
- Show configuration file details
- Demonstrate advanced setup options
- Discuss architecture and design decisions

---

## 📚 Follow-up Resources

### **Installation Documentation**
- Platform-specific installation guides
- Troubleshooting documentation
- Configuration reference
- FAQ for common issues

### **Getting Started Resources**
- First steps tutorial
- Sample project templates
- Video walkthroughs
- Community support links

---

**Demo Version**: 1.0.0  
**Last Updated**: January 2025  
**Estimated Success Rate**: 95%  
**Key Differentiator**: Intelligent, guided setup experience