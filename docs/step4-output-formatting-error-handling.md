# Step 4: Output Formatting and Error Handling

## Overview

This step implements comprehensive output formatting and error handling for the VS Code Prolog extension. The implementation provides rich, user-friendly formatting for query results, variable bindings, and error messages, along with support for streaming large outputs and internationalization.

## Components

### 1. OutputFormatter (`src/features/outputFormatter.ts`)

Core output formatting functionality for Prolog query results:

#### Key Features:
- **Multi-format Support**: Handles success, failure, error, and multiple result types
- **Variable Binding Formatting**: Displays variable bindings as readable tables or lists
- **Markdown Integration**: Uses VS Code markdown for rich text display
- **Streaming Support**: Handles large result sets with pagination
- **Customizable Options**: Configurable formatting preferences
- **Code Highlighting**: Syntax highlighting for Prolog code blocks

#### Main Methods:
- [`formatQueryResult()`](src/features/outputFormatter.ts:40) - Main entry point for formatting results
- [`formatVariableBindings()`](src/features/outputFormatter.ts:58) - Format variable bindings as tables/lists
- [`formatPrologCode()`](src/features/outputFormatter.ts:305) - Format Prolog code with syntax highlighting
- [`formatHelpText()`](src/features/outputFormatter.ts:329) - Format help documentation
- [`formatStreamingOutput()`](src/features/outputFormatter.ts:365) - Handle streaming results
- [`truncateOutput()`](src/features/outputFormatter.ts:393) - Truncate long outputs

#### Formatting Options:
```typescript
interface FormattingOptions {
  maxResults?: number;        // Maximum results to display (default: 50)
  maxLineLength?: number;     // Maximum line length (default: 120)
  useCodeBlocks?: boolean;    // Use markdown code blocks (default: true)
  showLineNumbers?: boolean;  // Show line numbers (default: false)
  highlightVariables?: boolean; // Highlight variables (default: true)
  compactMode?: boolean;      // Use compact table format (default: false)
  locale?: string;           // Localization locale (default: 'en')
}
```

### 2. ErrorHandler (`src/features/errorHandler.ts`)

Comprehensive error handling with structured error codes and user guidance:

#### Key Features:
- **Structured Error Codes**: Predefined error codes with descriptions and suggestions
- **Error Classification**: Categorizes errors by type (syntax, runtime, system, network, etc.)
- **User-Friendly Messages**: Converts technical errors to actionable user guidance
- **Context Awareness**: Includes context information (file, line, command)
- **Security Validation**: Validates and sanitizes error information
- **Localization Support**: Supports multiple languages for error messages

#### Error Types:
- **Syntax Errors**: Missing periods, unmatched parentheses, invalid operators
- **Runtime Errors**: Existence errors, type errors, instantiation errors
- **System Errors**: Backend crashes, timeouts, connection failures
- **Network Errors**: HTTP errors, connection refused, timeouts
- **Permission Errors**: File access, predicate modification restrictions
- **Resource Errors**: Memory limits, stack overflow
- **User Errors**: Invalid commands, missing arguments, invalid file paths

#### Main Methods:
- [`handleError()`](src/features/errorHandler.ts:46) - Main error processing entry point
- [`formatError()`](src/features/errorHandler.ts:442) - Format errors for display
- [`createUserFriendlyError()`](src/features/errorHandler.ts:477) - Create user-friendly error messages
- [`validatePackSecurity()`](src/features/errorHandler.ts:360) - Validate pack security (from Step 3)

### 3. StreamingHandler (`src/features/streamingHandler.ts`)

Handles streaming of large result sets with progress tracking:

#### Key Features:
- **Chunked Processing**: Processes large datasets in manageable chunks
- **Progress Tracking**: Visual progress indicators for long operations
- **Cancellation Support**: User can cancel long-running operations
- **Memory Efficient**: Processes data without loading everything into memory
- **Event-Driven**: Emits events for chunk processing and completion
- **Flexible Data Sources**: Supports arrays and async iterables

#### Main Methods:
- [`startStreaming()`](src/features/streamingHandler.ts:42) - Start streaming data processing
- [`createPrologResultStreamer()`](src/features/streamingHandler.ts:295) - Create streamer for Prolog results
- [`streamQueryResults()`](src/features/streamingHandler.ts:318) - Utility for streaming query results

### 4. LocalizationManager (`src/features/localization.ts`)

Internationalization support for error messages and UI text:

#### Key Features:
- **Multi-Language Support**: English, Spanish, French, German, Italian, Portuguese, Japanese, Chinese
- **Hierarchical Strings**: Organized string keys with dot notation
- **Fallback Support**: Falls back to English if translation missing
- **Dynamic Loading**: Loads translation files on demand
- **Pluralization**: Supports plural forms for different languages
- **Number/Date Formatting**: Locale-aware formatting

#### Supported Locales:
- `en` - English (default)
- `es` - Spanish (Español)
- `fr` - French (Français)
- `de` - German (Deutsch)
- `it` - Italian (Italiano)
- `pt` - Portuguese (Português)
- `ja` - Japanese (日本語)
- `zh` - Chinese (中文)

#### Main Methods:
- [`getString()`](src/features/localization.ts:165) - Get localized string
- [`getPlural()`](src/features/localization.ts:207) - Get pluralized string
- [`formatNumber()`](src/features/localization.ts:217) - Format numbers
- [`formatDate()`](src/features/localization.ts:226) - Format dates

## Usage Examples

### Basic Output Formatting

```typescript
import { OutputFormatter } from './features/outputFormatter';

const formatter = new OutputFormatter();

// Format successful query with bindings
const result = {
  type: 'success',
  bindings: [
    { Name: 'john', Age: 25 },
    { Name: 'jane', Age: 30 }
  ]
};

const formatted = formatter.formatQueryResult(result);
console.log(formatted);
// Output:
// ✅ **Query succeeded** (2 solutions):
// 
// **Solution 1:**
// ```prolog
// Name = john
// Age = 25
// ```
// 
// **Solution 2:**
// ```prolog
// Name = jane
// Age = 30
// ```
```

### Error Handling

```typescript
import { ErrorHandler } from './features/errorHandler';

const errorHandler = new ErrorHandler();

// Handle syntax error
const error = errorHandler.handleError('syntax error: missing period');
const formatted = errorHandler.formatError(error);

console.log(formatted);
// Output:
// 📝 **Syntax Error**
// 
// **Message:** syntax error: missing period
// 
// 💡 **Suggestion:** Check your Prolog syntax. Common issues include missing periods, unmatched parentheses, or incorrect operators.
// 
// 📚 **Documentation:** https://www.swi-prolog.org/pldoc/man?section=syntax
```

### Streaming Large Results

```typescript
import { createPrologResultStreamer } from './features/streamingHandler';

const streamer = createPrologResultStreamer({
  chunkSize: 25,
  maxTotalResults: 500
});

const largeResults = Array.from({ length: 1000 }, (_, i) => ({ X: i }));

await streamer.startStreaming(
  largeResults,
  async (chunk) => {
    console.log(`Processing chunk ${chunk.index + 1} with ${chunk.data.length} results`);
    // Process chunk data
  }
);
```

### Localization

```typescript
import { initializeLocalization, t, tp } from './features/localization';

// Initialize with extension path
const locManager = initializeLocalization('/path/to/extension');

// Set locale
await locManager.setLocale('es');

// Get localized strings
const errorMsg = t('errors.syntaxError');  // "Error de sintaxis en código Prolog"
const queryMsg = t('query.succeeded');    // "Consulta exitosa"

// Pluralization
const resultCount = tp('query.solution', 3); // "3 soluciones"
```

## Configuration

### Formatting Options

Users can customize output formatting through VS Code settings:

```json
{
  "prolog.output.maxResults": 100,
  "prolog.output.useCodeBlocks": true,
  "prolog.output.compactMode": false,
  "prolog.output.highlightVariables": true
}
```

### Error Handling Options

```json
{
  "prolog.errors.showSuggestions": true,
  "prolog.errors.showDocumentation": true,
  "prolog.errors.userFriendlyMessages": true
}
```

### Localization Options

```json
{
  "prolog.locale": "en",
  "prolog.fallbackLocale": "en"
}
```

## Testing

### Unit Tests

- **OutputFormatter Tests** (`test/outputFormatter.test.ts`):
  - Query result formatting
  - Variable binding display
  - Code highlighting
  - Streaming output
  - Truncation handling
  - Edge cases and error conditions

- **ErrorHandler Tests** (`test/errorHandler.test.ts`):
  - Error parsing and classification
  - User-friendly message generation
  - Context information handling
  - Localization support
  - Security validation

### Integration Tests

- **Step 4 Integration** (`test/step4-integration.test.ts`):
  - Component interaction testing
  - End-to-end formatting workflows
  - Error handling integration
  - Streaming functionality
  - Localization features

## Performance Considerations

### Memory Management
- Streaming handlers prevent memory overflow with large result sets
- Chunked processing keeps memory usage constant
- Automatic cleanup of resources and event listeners

### Output Optimization
- Truncation prevents excessive output in chat
- Lazy loading of translation files
- Efficient string formatting with minimal allocations

### User Experience
- Progress indicators for long operations
- Cancellation support for user control
- Responsive formatting that adapts to content size

## Error Recovery

### Graceful Degradation
- Falls back to basic formatting if advanced features fail
- Uses English text if localization fails
- Provides generic error messages if specific handling fails

### Logging and Diagnostics
- Comprehensive logging for debugging
- Error context preservation
- Performance metrics collection

## Future Enhancements

### Planned Features
1. **Custom Themes**: User-defined color schemes and formatting styles
2. **Export Options**: Export formatted results to various formats (HTML, PDF, etc.)
3. **Interactive Results**: Clickable variable bindings and expandable structures
4. **Syntax Highlighting**: Enhanced Prolog syntax highlighting with semantic tokens
5. **Accessibility**: Screen reader support and high contrast modes

### Extensibility
1. **Plugin System**: Allow custom formatters and error handlers
2. **Template System**: User-defined output templates
3. **Custom Error Codes**: Domain-specific error definitions
4. **Localization Plugins**: Community-contributed translations

## Integration Points

### Chat Handler Integration
The output formatting components integrate seamlessly with the chat handler (Step 5):
- Formatted results display in chat with rich markdown
- Error messages provide actionable guidance
- Streaming results show progress in chat
- Localized messages match user preferences

### Backend Integration
Works with the Prolog backend (Steps 1-2):
- Processes backend response formats
- Handles backend error conditions
- Supports batch request formatting
- Manages backend timeout scenarios

### Package Management Integration
Integrates with package management (Step 3):
- Formats package operation results
- Handles package-specific errors
- Displays package information consistently
- Supports package operation progress tracking

## Conclusion

Step 4 provides a comprehensive foundation for user-friendly output formatting and error handling in the VS Code Prolog extension. The implementation prioritizes user experience, internationalization, and performance while maintaining extensibility for future enhancements.

The components work together to transform raw Prolog results and errors into polished, actionable information that helps users understand and resolve issues quickly. The streaming capabilities ensure the extension remains responsive even with large datasets, while the localization support makes it accessible to a global audience.