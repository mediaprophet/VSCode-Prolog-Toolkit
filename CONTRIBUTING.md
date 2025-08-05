# Contributing to New-VSC-Prolog

## Prerequisites
- **Node.js**: Use version specified in `.nvmrc` (18.18.2)
- **SWI-Prolog**: Use version specified in `.swipl-version` (9.0.4)
- **Git**: For version control and pre-commit hooks

## Initial Setup

1. **Clone and Install Dependencies**
   ```bash
   git clone <repository-url>
   cd new-vsc-prolog
   npm install
   ```

2. **Setup Development Environment**
   ```bash
   npm run setup-test-env  # Creates test directories and sample files
   npm run prepare         # Installs Husky pre-commit hooks
   ```

3. **Verify Setup**
   ```bash
   npm run build          # Build the extension
   npm run test           # Run all tests
   npm run lint           # Check code quality
   ```

## Development Scripts

### Build & Compilation
- `npm run build` — Build extension with source maps
- `npm run build:watch` — Build with file watching
- `npm run compile` — TypeScript compilation with watch mode
- `npm run clean` — Remove build artifacts

### Testing
- `npm run test` — Run backend tests
- `npm run test:watch` — Run tests with file watching
- `npm run test:all` — Run all test suites
- `npm run test:coverage` — Run tests with coverage report
- `npm run setup-test-env` — Setup test environment

### Code Quality
- `npm run lint` — ESLint code checking
- `npm run lint:fix` — Auto-fix ESLint issues
- `npm run format` — Format code with Prettier
- `npm run format:check` — Check code formatting

### Extension Development
- `npm run package` — Package extension as .vsix
- `npm run syntax4swi` — Generate SWI-Prolog syntax files
- `npm run syntax4ecl` — Generate ECLiPSe syntax files

## Directory Structure

```
├── src/                     # Extension source code
│   ├── extension.ts         # Main extension entry point
│   ├── prologBackend.ts     # Prolog backend service
│   ├── prolog_json_server.pl # Prolog HTTP server
│   ├── features/            # Language features
│   └── utils/               # Utility modules
├── test/                    # Test files
│   ├── resources/           # Test resources (Prolog/N3 files)
│   ├── logs/                # Test execution logs
│   └── *.test.ts           # Test suites
├── .vscode/                 # VS Code configuration
├── .husky/                  # Git hooks
├── syntaxes/                # Language grammars
└── snippets/                # Code snippets
```

## Testing Guidelines

### Test Organization
- **Unit Tests**: Test individual components in isolation
- **Integration Tests**: Test component interactions
- **Backend Tests**: Test Prolog backend communication
- **Resource Files**: Use `test/resources/` for sample Prolog/N3 files

### Writing Tests
```typescript
import { expect } from 'chai';
import { PrologBackend } from '../src/prologBackend';

describe('Feature Name', function() {
  let backend: PrologBackend;
  
  beforeEach(function() {
    backend = new PrologBackend();
  });
  
  afterEach(function() {
    if (backend) backend.stop();
  });
  
  it('should do something', function(done) {
    // Test implementation
  });
});
```

### Test Resources
- Place sample Prolog files in `test/resources/`
- Use PlDoc comments for documentation testing
- Include N3 files for semantic web testing

## Code Quality Standards

### TypeScript Configuration
- **Strict Mode**: All strict TypeScript checks enabled
- **Type Safety**: No `any` types without justification
- **Documentation**: JSDoc comments for public APIs

### ESLint Rules
- **Prettier Integration**: Automatic code formatting
- **TypeScript Rules**: Enhanced type checking
- **Import Organization**: Automatic import sorting

### Pre-commit Hooks
The pre-commit hook automatically runs:
1. **Format Check**: Ensures consistent code formatting
2. **Lint Check**: Validates code quality and style
3. **Type Check**: Verifies TypeScript compilation

## VS Code Development

### Recommended Extensions
- ESLint
- Prettier
- TypeScript Importer
- Mocha Test Explorer

### Debug Configuration
Use the provided launch configurations:
- **Run Extension**: Debug the extension in a new VS Code window
- **Extension Tests**: Debug extension tests
- **Mocha Tests**: Debug backend tests
- **Debug Prolog Backend**: Debug backend communication

### Tasks
Available VS Code tasks:
- **Build**: Compile extension
- **Test**: Run test suites
- **Lint**: Check code quality
- **Format**: Format all files

## Architecture Guidelines

### Backend Communication
- Use HTTP-based JSON protocol for Prolog communication
- Implement proper error handling and timeouts
- Support batch requests for performance
- Include comprehensive logging

### Extension Features
- Follow VS Code extension best practices
- Implement proper disposal patterns
- Use TypeScript strict mode
- Include comprehensive error handling

### Testing Strategy
- Mock external dependencies
- Test error conditions
- Verify resource cleanup
- Include performance tests

## Git Workflow

### Commit Standards
- Use conventional commit messages
- Reference issue numbers when applicable
- Keep commits focused and atomic

### Pre-commit Checks
- Code formatting (Prettier)
- Linting (ESLint)
- Type checking (TypeScript)

### Branch Strategy
- `main`: Stable release branch
- `develop`: Integration branch
- `feature/*`: Feature development
- `fix/*`: Bug fixes

## Documentation

### Code Documentation
- JSDoc comments for all public APIs
- README updates for new features
- Architecture documentation in `fullimplementation.md`

### User Documentation
- Update README.md for user-facing changes
- Include usage examples
- Document configuration options

## Release Process

1. **Version Bump**: Update version in `package.json`
2. **Changelog**: Update `CHANGELOG.md`
3. **Testing**: Run full test suite
4. **Package**: Create `.vsix` package
5. **Documentation**: Update user documentation

## Troubleshooting

### Common Issues
- **SWI-Prolog Path**: Ensure SWI-Prolog is in PATH or configured
- **Node Version**: Use exact version from `.nvmrc`
- **Test Failures**: Check `test/logs/` for detailed error information
- **Build Issues**: Run `npm run clean` and rebuild

### Debug Logging
- Backend logs: Check console output during development
- Test logs: Available in `test/logs/` directory
- Extension logs: Use VS Code Developer Tools

---

For user-facing features and configuration, see `README.md`.
For implementation details, see `fullimplementation.md` and `implementationsteps.json`.
