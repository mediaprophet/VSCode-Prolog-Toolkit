# VSCode Prolog Toolkit Extension Test Cases

This folder contains comprehensive test cases for the VSCode Prolog Toolkit extension, covering all functionality including the new activity bar features.

## 📁 Test Structure

```
extension-test-cases/
├── README.md                          # This file - test overview
├── test-plan.md                       # Master test plan
├── installation-tests/                # Installation and setup tests
├── activity-bar-tests/               # Activity bar UI tests
├── core-functionality-tests/         # Core Prolog features tests
├── integration-tests/                # Integration and workflow tests
├── performance-tests/                # Performance and scalability tests
├── platform-tests/                   # Cross-platform compatibility tests
├── regression-tests/                 # Regression test suite
├── sample-prolog-files/              # Test Prolog files
└── test-results/                     # Test execution results
```

## 🎯 Test Categories

### 1. **Installation Tests**
- SWI-Prolog detection and validation
- Auto-detection functionality
- Setup wizard workflows
- Configuration migration

### 2. **Activity Bar Tests**
- Custom icon display
- Dashboard functionality
- Tree view providers
- Command integration

### 3. **Core Functionality Tests**
- Syntax highlighting
- Code completion
- Debugging features
- Query execution

### 4. **Integration Tests**
- LSP client integration
- Chat participant functionality
- API server features
- WebSocket communication

### 5. **Performance Tests**
- Large file handling
- Query response times
- Memory usage
- Startup performance

### 6. **Platform Tests**
- Windows compatibility
- macOS compatibility
- Linux compatibility
- Cross-platform features

## 🚀 Quick Start

1. **Install Extension**: Use the VSIX package in the root directory
2. **Run Test Suite**: Follow instructions in each test category
3. **Report Results**: Use the templates in `test-results/`

## 📊 Test Execution

Each test category includes:
- **Test Cases**: Detailed step-by-step instructions
- **Expected Results**: What should happen
- **Pass/Fail Criteria**: Clear success metrics
- **Bug Report Templates**: For issue tracking

## 🔧 Test Environment Setup

### Prerequisites
- VS Code 1.102.0+
- Node.js 18.x or 20.x
- SWI-Prolog (for full functionality tests)
- Test workspace with sample Prolog files

### Optional
- Multiple platforms for cross-platform testing
- Different VS Code themes for UI testing
- Various workspace configurations

## 📝 Contributing Test Cases

When adding new test cases:
1. Follow the existing template format
2. Include clear steps and expected results
3. Add sample files if needed
4. Update this README with new categories

## 🐛 Bug Reporting

Use the templates in `test-results/bug-report-template.md` for consistent issue reporting.

---

**Last Updated**: January 2025  
**Extension Version**: 1.3.0  
**Test Suite Version**: 1.0.0