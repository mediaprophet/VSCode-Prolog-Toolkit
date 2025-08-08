# Demo Execution Guide

## 🎯 Complete Guide to Running VSCode Prolog Toolkit Demos

This guide provides step-by-step instructions for executing all demo scenarios, from quick 15-minute showcases to comprehensive 2+ hour evaluations.

---

## 📋 Pre-Demo Setup

### 1. Environment Preparation

#### **System Requirements Check**
```bash
# Check VS Code version (requires 1.102.0+)
code --version

# Check Node.js version (requires 18.x+)
node --version

# Check SWI-Prolog installation
swipl --version
```

#### **Extension Installation**
```bash
# Install from VSIX (recommended for demos)
code --install-extension vscode-prolog-toolkit-1.3.0.vsix

# Or from marketplace
code --install-extension mediaprophet.vscode-prolog-toolkit
```

#### **Demo Files Setup**
```bash
# Clone or download demo files
git clone https://github.com/mediaprophet/VSCode-Prolog-Toolkit.git
cd VSCode-Prolog-Toolkit/demo-use-cases

# Or extract from VSIX
unzip vscode-prolog-toolkit-1.3.0.vsix
```

### 2. Workspace Preparation

#### **Create Demo Workspace**
1. Open VS Code
2. File → Open Folder → Select `demo-use-cases/sample-files`
3. Save workspace as `prolog-demo.code-workspace`

#### **Verify Extension Activation**
- Look for Prolog Toolkit icon in activity bar
- Open Command Palette (`Ctrl+Shift+P`) → Search "Prolog:"
- Check extension is active in Extensions view

---

## 🚀 Quick Demo (15 minutes)

**Perfect for**: Time-constrained evaluations, executive demos, quick validation

### Demo Flow

#### **Phase 1: Installation Verification (3 minutes)**
1. **Activity Bar Check**
   - ✅ Prolog Toolkit icon visible in activity bar
   - ✅ Click icon opens Prolog panel
   - ✅ Dashboard loads without errors

2. **Installation Status**
   - ✅ SWI-Prolog detection status displayed
   - ✅ Version information shown (if installed)
   - ✅ Setup wizard available (if needed)

#### **Phase 2: Core Functionality (7 minutes)**
1. **File Operations**
   ```prolog
   % Create new file: family.pl
   parent(tom, bob).
   parent(tom, liz).
   parent(bob, ann).
   
   father(X, Y) :- parent(X, Y), male(X).
   male(tom).
   male(bob).
   ```
   - ✅ Syntax highlighting works
   - ✅ Code completion available
   - ✅ File appears in workspace explorer

2. **Quick Query Interface**
   - Open activity bar dashboard
   - Enter query: `parent(tom, X)`
   - ✅ Query executes successfully
   - ✅ Results displayed clearly

3. **Chat Assistant**
   - Open Chat panel (`Ctrl+Shift+I`)
   - Type: `@prolog member(X, [1,2,3])`
   - ✅ Query executes with formatted results
   - ✅ Follow-up suggestions provided

#### **Phase 3: Advanced Features Preview (5 minutes)**
1. **Debugging Setup**
   - Set breakpoint in family.pl
   - F5 → Configure debugger
   - ✅ Debug configuration created

2. **N3 Semantic Reasoning**
   ```
   @prolog /n3_load --content "@prefix : <http://example.org/> . :socrates a :Person ."
   @prolog /n3_reason
   ```
   - ✅ N3 data loads successfully
   - ✅ Reasoning produces results

### Quick Demo Checklist
- [ ] Extension installs and activates
- [ ] Activity bar icon and dashboard work
- [ ] Basic Prolog syntax highlighting
- [ ] Quick query execution
- [ ] Chat assistant responds
- [ ] Debugging configuration possible
- [ ] N3 reasoning functional

---

## 🎬 Guided Demo Mode (30-60 minutes)

**Perfect for**: Training sessions, detailed evaluations, feature exploration

### Phase 1: Installation & Setup (10 minutes)

#### **Fresh Installation Demo**
1. **Uninstall Extension** (if present)
   ```bash
   code --uninstall-extension mediaprophet.vscode-prolog-toolkit
   ```

2. **Install from VSIX**
   ```bash
   code --install-extension vscode-prolog-toolkit-1.3.0.vsix
   ```

3. **First Launch Experience**
   - Restart VS Code
   - ✅ Welcome message appears
   - ✅ Activity bar icon visible
   - ✅ Installation check runs automatically

4. **Setup Wizard Demo**
   - Command Palette → "Prolog: Setup Wizard"
   - ✅ Platform detection works
   - ✅ SWI-Prolog auto-detection
   - ✅ Configuration validation

### Phase 2: Development Workflow (20 minutes)

#### **Project Setup**
1. **Create Prolog Project**
   ```
   prolog-demo/
   ├── src/
   │   ├── family.pl
   │   ├── logic.pl
   │   └── utils.pl
   ├── data/
   │   └── facts.pl
   └── tests/
       └── test_family.pl
   ```

2. **Multi-file Development**
   - Create family.pl with facts and rules
   - Create logic.pl with complex predicates
   - ✅ Cross-file references work
   - ✅ Go to definition across files
   - ✅ Find all references

#### **Language Features**
1. **Syntax Highlighting**
   - Comments, strings, operators
   - Built-in predicates
   - User-defined predicates
   - ✅ All elements properly colored

2. **Code Completion**
   - Built-in predicate completion
   - User-defined predicate completion
   - Snippet expansion
   - ✅ IntelliSense works correctly

3. **Code Navigation**
   - Go to definition (F12)
   - Peek definition (Alt+F12)
   - Find all references (Shift+F12)
   - ✅ Navigation works across files

### Phase 3: Interactive Features (15 minutes)

#### **Activity Bar Dashboard**
1. **Dashboard Overview**
   - Installation status card
   - Quick query interface
   - Recent queries history
   - Quick actions grid
   - ✅ All components functional

2. **Workspace Integration**
   - File explorer shows .pl files
   - New file creation
   - File opening from dashboard
   - ✅ Workspace integration seamless

#### **Chat Assistant Deep Dive**
1. **Basic Commands**
   ```
   @prolog /help member/2
   @prolog /consult family.pl
   @prolog /query father(X, Y)
   @prolog /status
   ```

2. **N3 Semantic Web**
   ```
   @prolog /n3_load sample.n3
   @prolog /n3_list --limit 10
   @prolog /n3_reason rdf(X, type, Person)
   @prolog /n3_explain rdf(socrates, type, Mortal)
   ```

### Phase 4: Advanced Capabilities (15 minutes)

#### **Debugging**
1. **Basic Debugging**
   - Set breakpoints
   - Start debugging (F5)
   - Step through code
   - Inspect variables
   - ✅ Debugging works correctly

2. **Advanced Debugging**
   - Conditional breakpoints
   - Hit count breakpoints
   - Spy predicates
   - ✅ Advanced features functional

#### **Enhanced Reasoning**
1. **Constraint Logic Programming**
   ```
   @prolog /clp_solve --domain fd --variables "X,Y" --constraints "X #= Y + 1, X #< 10"
   ```

2. **Probabilistic Logic**
   ```
   @prolog /probabilistic_fact --fact "weather(sunny)" --probability 0.7
   @prolog /probabilistic_query --goal "weather(sunny)" --samples 1000
   ```

---

## 🔬 Comprehensive Evaluation Mode (2+ hours)

**Perfect for**: Quality assurance, certification, complete feature validation

### Detailed Test Matrix

#### **Installation Testing (30 minutes)**
- [ ] Fresh installation on Windows
- [ ] Fresh installation on macOS
- [ ] Fresh installation on Linux
- [ ] SWI-Prolog auto-detection
- [ ] Manual path configuration
- [ ] Setup wizard completion
- [ ] Configuration migration
- [ ] Error handling and recovery

#### **Core Development Features (45 minutes)**
- [ ] Syntax highlighting accuracy
- [ ] Code completion completeness
- [ ] Snippet functionality
- [ ] File operations
- [ ] Multi-file projects
- [ ] Workspace integration
- [ ] Go to definition
- [ ] Find references
- [ ] Code formatting
- [ ] Linting and error detection

#### **Activity Bar & Dashboard (30 minutes)**
- [ ] Activity bar icon display
- [ ] Dashboard loading
- [ ] Installation status accuracy
- [ ] Quick query execution
- [ ] Query history tracking
- [ ] File explorer integration
- [ ] Quick actions functionality
- [ ] Theme compatibility
- [ ] Responsive design
- [ ] Error handling

#### **Chat Assistant (45 minutes)**
- [ ] Basic query execution
- [ ] File consultation
- [ ] Help system
- [ ] Status reporting
- [ ] N3 data loading
- [ ] N3 triple listing
- [ ] N3 reasoning
- [ ] N3 proof explanation
- [ ] Error handling
- [ ] Follow-up suggestions

#### **Debugging Capabilities (30 minutes)**
- [ ] Breakpoint setting
- [ ] Debug session start
- [ ] Step through execution
- [ ] Variable inspection
- [ ] Call stack display
- [ ] Conditional breakpoints
- [ ] Hit count breakpoints
- [ ] Spy predicates
- [ ] Debug console
- [ ] Error recovery

#### **Advanced Features (45 minutes)**
- [ ] Enhanced reasoning setup
- [ ] CLP(FD) constraint solving
- [ ] CLP(R) real constraints
- [ ] CLP(Q) rational constraints
- [ ] Probabilistic fact definition
- [ ] Probabilistic inference
- [ ] Logic module registration
- [ ] Logic module querying
- [ ] Integration testing
- [ ] Performance validation

#### **API Integration (30 minutes)**
- [ ] API server startup
- [ ] HTTP endpoint testing
- [ ] WebSocket connection
- [ ] Authentication methods
- [ ] Rate limiting
- [ ] CORS configuration
- [ ] External tool integration
- [ ] MCP server functionality
- [ ] Error handling
- [ ] Security validation

#### **Cross-Platform Testing (45 minutes)**
- [ ] Windows PowerShell integration
- [ ] Windows CMD integration
- [ ] Windows WSL integration
- [ ] macOS Terminal integration
- [ ] macOS Homebrew paths
- [ ] Linux shell integration
- [ ] Package manager detection
- [ ] Path normalization
- [ ] Environment variables
- [ ] Platform-specific features

---

## 📊 Results Documentation

### Demo Results Template

```markdown
# Demo Execution Results

**Date**: ___________
**Tester**: ___________
**Platform**: ___________
**VS Code Version**: ___________
**Extension Version**: ___________
**SWI-Prolog Version**: ___________

## Quick Demo Results (15 min)
- [ ] Installation verification: ✅/❌
- [ ] Core functionality: ✅/❌
- [ ] Advanced features preview: ✅/❌

**Overall Status**: ✅ PASS / ❌ FAIL
**Notes**: ___________

## Guided Demo Results (30-60 min)
- [ ] Installation & setup: ✅/❌
- [ ] Development workflow: ✅/❌
- [ ] Interactive features: ✅/❌
- [ ] Advanced capabilities: ✅/❌

**Overall Status**: ✅ PASS / ❌ FAIL
**Notes**: ___________

## Comprehensive Evaluation Results (2+ hours)
- [ ] Installation testing: ✅/❌
- [ ] Core development: ✅/❌
- [ ] Activity bar & dashboard: ✅/❌
- [ ] Chat assistant: ✅/❌
- [ ] Debugging: ✅/❌
- [ ] Advanced features: ✅/❌
- [ ] API integration: ✅/❌
- [ ] Cross-platform: ✅/❌

**Overall Status**: ✅ PASS / ❌ FAIL
**Critical Issues**: ___________
**Recommendations**: ___________
```

### Issue Tracking Template

```markdown
# Demo Issue Report

**Issue ID**: DEMO-YYYY-MM-DD-###
**Severity**: Critical/High/Medium/Low
**Category**: Installation/Core/UI/Chat/Debug/Advanced/API/Platform
**Platform**: Windows/macOS/Linux
**Reproducible**: Yes/No

## Description
Brief description of the issue

## Steps to Reproduce
1. Step one
2. Step two
3. Step three

## Expected Behavior
What should happen

## Actual Behavior
What actually happened

## Environment
- OS: ___________
- VS Code: ___________
- Extension: ___________
- SWI-Prolog: ___________

## Workaround
Any temporary solutions

## Priority
Impact on demo success
```

---

## 🛠️ Troubleshooting

### Common Issues and Solutions

#### **Extension Not Loading**
- **Symptom**: No activity bar icon
- **Solution**: Restart VS Code, check Extensions view
- **Alternative**: Reinstall extension

#### **SWI-Prolog Not Found**
- **Symptom**: "SWI-Prolog not installed" message
- **Solution**: Run setup wizard, configure path manually
- **Alternative**: Install SWI-Prolog using package manager

#### **Chat Assistant Not Responding**
- **Symptom**: No response to @prolog commands
- **Solution**: Check backend status, restart extension
- **Alternative**: Use activity bar quick query

#### **Debugging Not Working**
- **Symptom**: Breakpoints not hit
- **Solution**: Check debug configuration, verify SWI-Prolog path
- **Alternative**: Use trace/0 predicate

#### **Performance Issues**
- **Symptom**: Slow response times
- **Solution**: Check system resources, restart backend
- **Alternative**: Reduce query complexity

---

## 📈 Success Metrics

### Demo Success Criteria

#### **Functional Success**
- ✅ 95%+ of features work as expected
- ✅ No critical errors during demo
- ✅ Cross-platform consistency maintained
- ✅ Performance meets benchmarks

#### **User Experience Success**
- ✅ Intuitive interface navigation
- ✅ Clear and helpful error messages
- ✅ Responsive and smooth interactions
- ✅ Comprehensive help and documentation

#### **Technical Success**
- ✅ Stable backend operation
- ✅ Reliable API integration
- ✅ Effective debugging tools
- ✅ Advanced features functional

### Performance Benchmarks

- **Extension Activation**: < 3 seconds
- **Query Execution**: < 2 seconds (simple queries)
- **File Loading**: < 1 second (< 1MB files)
- **Dashboard Loading**: < 1 second
- **Memory Usage**: < 100MB baseline

---

**Execution Guide Version**: 1.0.0  
**Last Updated**: January 2025  
**Next Review**: Before major releases  
**Maintainer**: VSCode Prolog Toolkit Team