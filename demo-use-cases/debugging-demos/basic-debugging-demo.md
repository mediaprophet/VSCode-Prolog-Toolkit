# Basic Debugging Demo

## 🐛 Comprehensive Prolog Debugging Showcase

**Duration**: 18 minutes  
**Audience**: Developers, debugging tool evaluators, technical users  
**Goal**: Demonstrate professional debugging capabilities with breakpoints, stepping, and variable inspection  

---

## 📋 Demo Overview

This demo showcases the extension's comprehensive debugging capabilities, demonstrating how developers can effectively debug Prolog programs using modern IDE debugging tools. The extension provides full debugging support with breakpoints, stepping, variable inspection, and call stack visualization.

### **Key Features Demonstrated**
- Breakpoint setting and management
- Step-through debugging (step into, over, out)
- Variable inspection and binding visualization
- Call stack navigation
- Conditional breakpoints and hit counts
- Spy predicates (function breakpoints)
- Debug console interaction
- Error detection and recovery

---

## 🎬 Demo Script

### **Phase 1: Debug Configuration Setup (4 minutes)**

#### **1.1 Create Debug Configuration**
1. **Open Debug File**
   - Open `sample-files/debugging/factorial-debug.pl`
   - Show the factorial implementation with potential issues

2. **Launch Debug Configuration**
   - Press F5 or click Run → Start Debugging
   - Show debug configuration dialog
   - **Key Message**: "One-click debugging setup"

3. **Configure Debug Settings**
   ```json
   {
     "name": "Debug Prolog Files",
     "type": "prolog",
     "request": "launch",
     "program": "${file}",
     "cwd": "${workspaceFolder}",
     "startupQuery": "test_factorial",
     "stopOnEntry": true,
     "runtimeExecutable": "swipl"
   }
   ```
   - Show automatic configuration generation
   - Point out key settings: program, startupQuery, stopOnEntry
   - **Key Message**: "Intelligent default configuration"

#### **1.2 Debug Interface Overview**
1. **Debug Panel Layout**
   - Show Variables panel (empty initially)
   - Show Call Stack panel
   - Show Breakpoints panel
   - Show Debug Console
   - **Key Message**: "Professional debugging interface"

2. **Debug Toolbar**
   - Point out debug controls: Continue, Step Over, Step Into, Step Out, Restart, Stop
   - Show debug status in status bar
   - **Key Message**: "Familiar debugging controls"

#### **Expected Results**
- ✅ Debug configuration created automatically
- ✅ Debug interface opens with all panels
- ✅ Debug toolbar shows standard controls
- ✅ Ready to start debugging session

### **Phase 2: Breakpoint Management (5 minutes)**

#### **2.1 Setting Basic Breakpoints**
1. **Line Breakpoints**
   ```prolog
   factorial(0, 1) :- !.
   factorial(N, F) :-          % <- Set breakpoint here
       N > 0,
       N1 is N - 1,
       factorial(N1, F1),      % <- Set breakpoint here
       F is N * F1.
   ```
   - Click in gutter to set breakpoints (red dots appear)
   - Show breakpoint indicators
   - **Key Message**: "Visual breakpoint management"

2. **Breakpoint Panel Management**
   - Show breakpoints listed in Breakpoints panel
   - Toggle breakpoints on/off
   - Remove breakpoints
   - **Key Message**: "Centralized breakpoint control"

#### **2.2 Advanced Breakpoint Types**
1. **Conditional Breakpoints**
   - Right-click on breakpoint → Edit Breakpoint
   - Set condition: `N > 3`
   - Show conditional breakpoint icon (diamond with dot)
   - **Key Message**: "Smart conditional debugging"

2. **Hit Count Breakpoints**
   - Right-click on breakpoint → Edit Breakpoint
   - Set hit count: `>= 2`
   - Show hit count breakpoint icon
   - **Key Message**: "Precise debugging control"

3. **Spy Predicates (Function Breakpoints)**
   - Click + in Breakpoints panel
   - Enter predicate: `factorial/2`
   - Show spy breakpoint in list
   - **Key Message**: "Predicate-level debugging"

#### **Expected Results**
- ✅ Breakpoints set visually with gutter clicks
- ✅ Conditional breakpoints work with expressions
- ✅ Hit count breakpoints trigger correctly
- ✅ Spy predicates intercept all calls

### **Phase 3: Step-Through Debugging (6 minutes)**

#### **3.1 Start Debug Session**
1. **Initial Execution**
   - Press F5 to start debugging
   - Show execution stops at entry point
   - Point out current execution line (yellow arrow)
   - **Key Message**: "Precise execution control"

2. **Variable Inspection at Start**
   - Show Variables panel with initial bindings
   - Point out local variables and their values
   - **Key Message**: "Real-time variable monitoring"

#### **3.2 Stepping Through Code**
1. **Step Into (F11)**
   ```prolog
   test_factorial :-
       factorial(5, Result),    % <- Step into this call
       write('Result: '), write(Result), nl.
   ```
   - Press F11 to step into factorial/2
   - Show execution moves to factorial predicate
   - Show call stack updates
   - **Key Message**: "Deep execution tracing"

2. **Step Over (F10)**
   ```prolog
   factorial(N, F) :-
       N > 0,                   % <- Step over this
       N1 is N - 1,            % <- And this
       factorial(N1, F1),      % <- But step into this
       F is N * F1.
   ```
   - Use F10 to step over simple operations
   - Use F11 to step into recursive calls
   - Show different stepping behaviors

3. **Step Out (Shift+F11)**
   - When deep in recursion, use Step Out
   - Show execution returns to calling context
   - **Key Message**: "Flexible navigation control"

#### **3.3 Variable Inspection During Execution**
1. **Variables Panel**
   ```
   Variables:
   ├── N: 5
   ├── F: _G123 (unbound)
   ├── N1: 4
   └── F1: _G456 (unbound)
   ```
   - Show variable bindings update in real-time
   - Point out bound vs unbound variables
   - Show variable value changes during execution

2. **Hover Inspection**
   - Hover over variables in code
   - Show tooltip with current values
   - **Key Message**: "Contextual variable inspection"

#### **Expected Results**
- ✅ Stepping controls work correctly
- ✅ Variables update in real-time
- ✅ Call stack shows execution path
- ✅ Hover inspection provides immediate feedback

### **Phase 4: Advanced Debugging Features (3 minutes)**

#### **4.1 Call Stack Navigation**
1. **Call Stack Visualization**
   ```
   Call Stack:
   1. factorial(1, F1) [factorial-debug.pl:8]
   2. factorial(2, F1) [factorial-debug.pl:8]
   3. factorial(3, F1) [factorial-debug.pl:8]
   4. factorial(4, F1) [factorial-debug.pl:8]
   5. factorial(5, Result) [factorial-debug.pl:15]
   6. test_factorial [factorial-debug.pl:14]
   ```
   - Show recursive call stack buildup
   - Click on different stack frames
   - Show variable context changes with frame selection
   - **Key Message**: "Complete execution context"

2. **Stack Frame Navigation**
   - Click on different frames in call stack
   - Show how variable values change per frame
   - Show source code location for each frame

#### **4.2 Debug Console Interaction**
1. **Evaluate Expressions**
   ```
   Debug Console:
   > N
   5
   > N1
   4
   > F1
   _G456
   > N * 2
   10
   ```
   - Type variable names to see values
   - Evaluate expressions in current context
   - **Key Message**: "Interactive debugging exploration"

2. **Execute Queries**
   ```
   Debug Console:
   > factorial(3, X)
   X = 6
   > member(N, [1,2,3,4,5])
   true
   ```
   - Execute Prolog queries in debug context
   - Show results in console

#### **Expected Results**
- ✅ Call stack shows complete execution path
- ✅ Frame navigation works correctly
- ✅ Debug console evaluates expressions
- ✅ Interactive queries work in debug context

---

## 🎯 Interactive Debugging Scenarios

### **Scenario 1: Bug Detection and Fix**

#### **Problem Setup**
```prolog
% Buggy factorial with off-by-one error
factorial_buggy(0, 1) :- !.
factorial_buggy(N, F) :-
    N > 0,
    N1 is N - 2,        % BUG: Should be N - 1
    factorial_buggy(N1, F1),
    F is N * F1.
```

#### **Debugging Process**
1. **Set breakpoints** on recursive calls
2. **Step through execution** and watch variables
3. **Identify the bug** when N1 becomes negative
4. **Fix the bug** and verify with continued debugging
5. **Key Message**: "Systematic bug identification and resolution"

### **Scenario 2: Performance Analysis**
```prolog
% Inefficient list reversal
slow_reverse([], []).
slow_reverse([H|T], Rev) :-
    slow_reverse(T, RevT),
    append(RevT, [H], Rev).  % Inefficient append
```

#### **Debugging Process**
1. **Use hit count breakpoints** to count recursive calls
2. **Monitor call stack depth** for large lists
3. **Identify performance bottleneck** in append operations
4. **Compare with efficient implementation**
5. **Key Message**: "Performance debugging and optimization"

---

## 🔍 Debugging Best Practices Demo

### **Effective Debugging Strategies**
1. **Start with breakpoints** on entry points
2. **Use conditional breakpoints** for specific cases
3. **Monitor variable bindings** throughout execution
4. **Use spy predicates** for predicate-level debugging
5. **Leverage debug console** for interactive exploration

### **Common Debugging Patterns**
1. **Recursive Debugging**: Set breakpoints on base and recursive cases
2. **Unification Debugging**: Watch variable bindings during unification
3. **Cut Debugging**: Understand choice point elimination
4. **Arithmetic Debugging**: Verify mathematical operations
5. **List Processing**: Track list construction and deconstruction

---

## 📊 Success Criteria

### **Functional Indicators**
- [ ] Debug configuration creates successfully
- [ ] Breakpoints can be set and triggered
- [ ] Stepping controls work correctly
- [ ] Variables display current values
- [ ] Call stack shows execution path
- [ ] Debug console accepts input and shows output

### **User Experience Indicators**
- [ ] Interface is intuitive and responsive
- [ ] Visual indicators are clear and helpful
- [ ] Error messages are informative
- [ ] Performance is acceptable for debugging sessions
- [ ] Integration with VS Code debugging feels native

### **Technical Indicators**
- [ ] SWI-Prolog integration works correctly
- [ ] Debug session management is stable
- [ ] Memory usage is reasonable during debugging
- [ ] No crashes or hangs during debug sessions
- [ ] Cleanup happens properly when debugging stops

---

## 🛠️ Troubleshooting

### **Common Issues**

#### **Debug Session Won't Start**
- **Symptoms**: F5 doesn't start debugging, no debug interface
- **Causes**: SWI-Prolog not found, invalid configuration, file not saved
- **Solutions**:
  1. Verify SWI-Prolog installation and path
  2. Check debug configuration in launch.json
  3. Save file before debugging
  4. Check startup query exists in file
- **Demo Recovery**: Show configuration dialog, explain setup

#### **Breakpoints Not Hit**
- **Symptoms**: Execution doesn't stop at breakpoints
- **Causes**: Breakpoints in unreachable code, wrong predicate name
- **Solutions**:
  1. Verify code path reaches breakpoint location
  2. Check predicate names and arity
  3. Use spy predicates as alternative
  4. Add trace/0 calls for debugging
- **Demo Recovery**: Use spy predicates, show trace output

#### **Variables Not Showing Values**
- **Symptoms**: Variables panel empty or shows wrong values
- **Causes**: Variable scope issues, unification problems
- **Solutions**:
  1. Check variable scope in current frame
  2. Use debug console to query variables
  3. Step to different execution points
  4. Verify variable names and bindings
- **Demo Recovery**: Use debug console, explain variable scoping

#### **Performance Issues During Debugging**
- **Symptoms**: Slow stepping, unresponsive interface
- **Causes**: Large data structures, infinite loops, complex queries
- **Solutions**:
  1. Use conditional breakpoints to skip iterations
  2. Set hit count limits
  3. Use smaller test data
  4. Restart debug session if needed
- **Demo Recovery**: Use simpler examples, explain performance considerations

---

## 📝 Demo Variations

### **Quick Version (10 minutes)**
- Focus on basic breakpoints and stepping
- Show variable inspection briefly
- Demonstrate one debugging scenario
- Skip advanced features

### **Comprehensive Version (25 minutes)**
- Include all debugging features
- Show multiple debugging scenarios
- Demonstrate performance debugging
- Explore customization options

### **Technical Version (30 minutes)**
- Explain debugging architecture
- Show integration with SWI-Prolog tracer
- Demonstrate advanced breakpoint types
- Discuss debugging best practices

---

## 📚 Follow-up Resources

### **Documentation**
- Debugging configuration guide
- Breakpoint types reference
- Variable inspection techniques
- Performance debugging strategies

### **Sample Files**
- `debugging/factorial-debug.pl` - Recursive debugging
- `debugging/sorting-debug.pl` - Algorithm debugging
- `debugging/error-examples.pl` - Common error patterns

---

**Demo Version**: 1.0.0  
**Last Updated**: January 2025  
**Estimated Success Rate**: 88%  
**Prerequisites**: SWI-Prolog installed, debug configuration working