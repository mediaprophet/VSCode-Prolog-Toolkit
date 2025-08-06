# Quick Demo Guide (15 minutes)

## 🚀 Rapid VSCode Prolog Toolkit Showcase

**Perfect for**: Executive demos, time-constrained evaluations, quick validation  
**Duration**: 15 minutes  
**Audience**: Decision makers, evaluators, stakeholders  
**Goal**: Demonstrate key capabilities and value proposition  

---

## 📋 Pre-Demo Checklist (2 minutes)

### Environment Setup
- [ ] VS Code 1.102.0+ installed
- [ ] VSCode Prolog Toolkit extension installed
- [ ] SWI-Prolog installed (optional but recommended)
- [ ] Demo workspace ready: `demo-use-cases/sample-files`

### Quick Verification
```bash
# Verify installations
code --version
swipl --version  # Optional
```

---

## 🎬 Demo Script

### **Phase 1: Extension Overview (3 minutes)**

#### **1.1 Extension Activation**
1. **Open VS Code**
   - Launch VS Code
   - Open folder: `demo-use-cases/sample-files`

2. **Show Activity Bar Integration**
   - Point out custom Prolog Toolkit icon in activity bar
   - Click icon to open Prolog panel
   - **Key Message**: "Modern, integrated development experience"

3. **Dashboard Overview**
   - Show installation status (green checkmark if SWI-Prolog installed)
   - Point out quick query interface
   - Highlight workspace file detection
   - **Key Message**: "Everything you need in one place"

#### **Expected Results**
- ✅ Activity bar icon visible and clickable
- ✅ Dashboard loads without errors
- ✅ Installation status correctly displayed
- ✅ Professional, modern interface

---

### **Phase 2: Core Development Features (5 minutes)**

#### **2.1 Syntax Highlighting & Language Support**
1. **Open Sample File**
   - Open `basic/family.pl` from file explorer
   - Show syntax highlighting in action

2. **Create New Content**
   ```prolog
   % Family relationships demo
   parent(tom, bob).
   parent(tom, liz).
   parent(bob, ann).
   parent(bob, pat).
   
   % Gender facts
   male(tom).
   male(bob).
   female(liz).
   female(ann).
   female(pat).
   
   % Derived relationships
   father(X, Y) :- parent(X, Y), male(X).
   mother(X, Y) :- parent(X, Y), female(X).
   grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
   ```

3. **Demonstrate Language Features**
   - Type `father(` and show code completion
   - Use Ctrl+Space to trigger IntelliSense
   - Show hover information on built-in predicates
   - **Key Message**: "Full IDE support for Prolog development"

#### **2.2 Quick Query Execution**
1. **Use Dashboard Quick Query**
   - In activity bar dashboard, find quick query input
   - Enter: `parent(tom, X)`
   - Click Execute button
   - Show results in dashboard

2. **Try More Queries**
   - `father(bob, Y)`
   - `grandparent(tom, Z)`
   - **Key Message**: "Instant query testing without leaving the editor"

#### **Expected Results**
- ✅ Rich syntax highlighting for all Prolog constructs
- ✅ Code completion works for predicates
- ✅ Hover information displays correctly
- ✅ Quick queries execute and show results

---

### **Phase 3: AI-Powered Chat Assistant (4 minutes)**

#### **3.1 Basic Chat Interaction**
1. **Open Chat Panel**
   - Press `Ctrl+Shift+I` or click Chat icon
   - Type: `@prolog` to activate Prolog assistant

2. **Interactive Query Execution**
   ```
   @prolog member(X, [1,2,3])
   ```
   - Show formatted results with variable bindings
   - Point out intelligent result presentation

3. **File Consultation**
   ```
   @prolog /consult family.pl
   ```
   - Show successful file loading
   - **Key Message**: "AI assistant understands Prolog and your code"

#### **3.2 Advanced AI Features**
1. **Help System**
   ```
   @prolog /help member/2
   ```
   - Show comprehensive predicate documentation
   - Point out examples and usage information

2. **Semantic Web Reasoning** (if time permits)
   ```
   @prolog /n3_load --content "@prefix : <http://example.org/> . :socrates a :Person ."
   @prolog /n3_reason
   ```
   - Show N3 semantic reasoning capabilities
   - **Key Message**: "Beyond traditional Prolog - semantic web integration"

#### **Expected Results**
- ✅ Chat assistant responds intelligently
- ✅ Query results are well-formatted
- ✅ File consultation works
- ✅ Help system provides useful information
- ✅ N3 reasoning demonstrates advanced capabilities

---

### **Phase 4: Advanced Capabilities Preview (3 minutes)**

#### **4.1 Debugging Setup**
1. **Show Debugging Configuration**
   - Open family.pl
   - Click in gutter to set breakpoint (red dot appears)
   - Press F5 to start debugging
   - Show debug configuration dialog
   - **Key Message**: "Full debugging support with breakpoints and stepping"

#### **4.2 Enhanced Reasoning Preview**
1. **Constraint Logic Programming**
   ```
   @prolog /clp_solve --domain fd --variables "X,Y" --constraints "X #= Y + 1, X #< 10, Y #> 0"
   ```
   - Show constraint solving results
   - **Key Message**: "Advanced reasoning beyond traditional Prolog"

2. **API Integration Preview**
   - Show API server configuration in settings
   - Mention WebSocket support for real-time integration
   - **Key Message**: "Enterprise-ready with API integration"

#### **Expected Results**
- ✅ Debugging configuration works
- ✅ Breakpoints can be set
- ✅ Advanced reasoning features functional
- ✅ API integration capabilities evident

---

## 🎯 Key Messages Summary

### **Value Propositions**
1. **Modern Development Experience**: Integrated activity bar, dashboard, and AI assistant
2. **Comprehensive Language Support**: Full IDE features for Prolog development
3. **AI-Powered Assistance**: Intelligent chat assistant with semantic reasoning
4. **Advanced Capabilities**: Debugging, constraint solving, API integration
5. **Cross-Platform**: Works seamlessly on Windows, macOS, and Linux

### **Competitive Advantages**
- **Only Prolog extension** with custom activity bar integration
- **AI chat assistant** with N3 semantic web reasoning
- **Advanced reasoning** with CLP and probabilistic logic
- **API server** for external tool integration
- **Comprehensive platform support** with auto-detection

---

## 📊 Success Criteria

### **Demo Success Indicators**
- [ ] Extension loads and activates properly
- [ ] Activity bar and dashboard work smoothly
- [ ] Syntax highlighting and completion functional
- [ ] Quick queries execute successfully
- [ ] Chat assistant responds intelligently
- [ ] Advanced features demonstrate without errors

### **Audience Engagement Signs**
- Questions about specific features
- Interest in installation process
- Requests for deeper technical details
- Comparison questions with other tools
- Discussion of use cases and applications

---

## 🛠️ Troubleshooting

### **Common Issues During Demo**

#### **Extension Not Visible**
- **Quick Fix**: Restart VS Code, check Extensions view
- **Backup Plan**: Show extension in marketplace, highlight features

#### **SWI-Prolog Not Found**
- **Quick Fix**: Show setup wizard, explain auto-detection
- **Backup Plan**: Focus on syntax highlighting and chat features

#### **Chat Assistant Not Responding**
- **Quick Fix**: Check backend status with `/status` command
- **Backup Plan**: Use dashboard quick query interface

#### **Queries Failing**
- **Quick Fix**: Use simpler queries like `member(1, [1,2,3])`
- **Backup Plan**: Show pre-recorded results or screenshots

### **Demo Recovery Strategies**
1. **Have backup screenshots** of key features working
2. **Prepare simple fallback queries** that always work
3. **Know the key talking points** even without live demo
4. **Have installation guide ready** for interested audience

---

## 📝 Post-Demo Actions

### **Immediate Follow-up**
- [ ] Provide installation instructions
- [ ] Share demo workspace files
- [ ] Offer detailed feature walkthrough
- [ ] Schedule technical deep-dive session

### **Resources to Share**
- Installation guide: `docs/installation-guide.md`
- User manual: `README.md`
- Demo files: `demo-use-cases/sample-files/`
- GitHub repository: https://github.com/mediaprophet/VSCode-Prolog-Toolkit

---

## 🎪 Demo Variations

### **Executive Version (10 minutes)**
- Focus on value proposition and competitive advantages
- Emphasize modern UI and AI integration
- Show enterprise features (API, security)
- Minimize technical details

### **Technical Version (20 minutes)**
- Include debugging demonstration
- Show advanced reasoning features
- Demonstrate API integration
- Discuss architecture and extensibility

### **Developer Version (25 minutes)**
- Deep dive into language features
- Show multi-file project development
- Demonstrate testing and debugging workflow
- Explore customization options

---

**Quick Demo Guide Version**: 1.0.0  
**Last Updated**: January 2025  
**Estimated Success Rate**: 95%+ with proper preparation  
**Recommended Practice Runs**: 2-3 before important demos