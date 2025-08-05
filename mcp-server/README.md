# VSCode Prolog Toolkit MCP Server

This is a Model Context Protocol (MCP) server that provides AI agents like Claude Desktop with access to the VSCode Prolog Toolkit's capabilities. It enables seamless integration between AI assistants and Prolog development environments.

## Features

- **Query Execution**: Execute Prolog queries and get formatted results
- **File Consultation**: Load Prolog files into the knowledge base
- **Session Management**: Create and manage isolated Prolog sessions
- **Syntax Validation**: Validate Prolog code without execution
- **Help System**: Get documentation for Prolog predicates
- **Resource Access**: Browse sessions, predicates, and examples

## Installation

### Prerequisites

- Node.js 18.0.0 or higher
- VSCode Prolog Toolkit API server running

### Install Dependencies

```bash
cd mcp-server
npm install
```

### Build the Server

```bash
npm run build
```

## Configuration

The server can be configured using environment variables:

| Variable | Description | Default |
|----------|-------------|---------|
| `PROLOG_TOOLKIT_API_URL` | Base URL of the Prolog Toolkit API | `http://localhost:3000` |
| `PROLOG_TOOLKIT_WS_URL` | WebSocket URL for real-time updates | (optional) |
| `PROLOG_TOOLKIT_API_KEY` | API key for authentication | (optional) |
| `PROLOG_TOOLKIT_TIMEOUT` | Request timeout in milliseconds | `30000` |

### Example Configuration

```bash
export PROLOG_TOOLKIT_API_URL="http://localhost:3000"
export PROLOG_TOOLKIT_API_KEY="your-api-key-here"
export PROLOG_TOOLKIT_TIMEOUT="60000"
```

## Claude Desktop Integration

To use this MCP server with Claude Desktop, add the following configuration to your Claude Desktop settings:

### macOS/Linux

Edit `~/.config/claude-desktop/claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "vscode-prolog-toolkit": {
      "command": "node",
      "args": ["/path/to/mcp-server/dist/index.js"],
      "env": {
        "PROLOG_TOOLKIT_API_URL": "http://localhost:3000",
        "PROLOG_TOOLKIT_API_KEY": "your-api-key-here"
      }
    }
  }
}
```

### Windows

Edit `%APPDATA%\Claude\claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "vscode-prolog-toolkit": {
      "command": "node",
      "args": ["C:\\path\\to\\mcp-server\\dist\\index.js"],
      "env": {
        "PROLOG_TOOLKIT_API_URL": "http://localhost:3000",
        "PROLOG_TOOLKIT_API_KEY": "your-api-key-here"
      }
    }
  }
}
```

## Available Tools

### execute_prolog_query

Execute a Prolog query and return results.

**Parameters:**
- `query` (string, required): The Prolog query to execute
- `sessionId` (string, optional): Session ID for query execution
- `timeout` (number, optional): Query timeout in milliseconds

**Example:**
```
Execute the query: parent(X, bob).
```

### consult_prolog_file

Load/consult a Prolog file into the knowledge base.

**Parameters:**
- `filePath` (string, required): Path to the Prolog file
- `sessionId` (string, optional): Target session ID

**Example:**
```
Consult the file: /path/to/family.pl
```

### create_prolog_session

Create a new isolated Prolog session.

**Parameters:**
- `name` (string, required): Name for the session
- `description` (string, optional): Session description

**Example:**
```
Create a new session named "family-tree" for genealogy queries
```

### list_prolog_sessions

List all available Prolog sessions.

**Parameters:**
- `includeInactive` (boolean, optional): Include inactive sessions

**Example:**
```
List all active Prolog sessions
```

### get_session_state

Get the current state of a Prolog session.

**Parameters:**
- `sessionId` (string, required): ID of the session to inspect

**Example:**
```
Get the state of session "family-tree"
```

### validate_prolog_syntax

Validate Prolog code syntax without executing it.

**Parameters:**
- `code` (string, required): Prolog code to validate

**Example:**
```
Validate this Prolog code: parent(tom, bob). grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
```

### get_prolog_help

Get help information about Prolog predicates or concepts.

**Parameters:**
- `topic` (string, required): Predicate name or concept

**Example:**
```
Get help for the "append" predicate
```

## Available Resources

### prolog://sessions

JSON resource containing information about all active Prolog sessions.

### prolog://predicates

JSON resource with documentation for available Prolog predicates.

### prolog://examples

Text resource containing Prolog code examples and tutorials.

## Usage Examples

### Basic Query Execution

```
User: "Execute the Prolog query to find all parents: parent(X, Y)."

Claude will use the execute_prolog_query tool to run the query and return formatted results.
```

### File Loading and Querying

```
User: "Load the family.pl file and then find all grandparents."

Claude will:
1. Use consult_prolog_file to load the file
2. Use execute_prolog_query to find grandparents
```

### Session Management

```
User: "Create a new session for working with mathematical predicates, then load math.pl into it."

Claude will:
1. Use create_prolog_session to create a new session
2. Use consult_prolog_file with the session ID to load the file
```

## Development

### Running in Development Mode

```bash
npm run dev
```

This will watch for changes and restart the server automatically.

### Testing

You can test the MCP server using the MCP Inspector:

```bash
npx @modelcontextprotocol/inspector node dist/index.js
```

### Debugging

Enable debug logging by setting the `DEBUG` environment variable:

```bash
DEBUG=mcp:* npm start
```

## API Compatibility

This MCP server is designed to work with the VSCode Prolog Toolkit API. Ensure your API server supports the following endpoints:

- `POST /api/query` - Execute Prolog queries
- `POST /api/consult` - Consult Prolog files
- `GET /api/sessions` - List sessions
- `POST /api/sessions` - Create sessions
- `GET /api/sessions/:id/state` - Get session state
- `POST /api/validate` - Validate syntax
- `GET /api/help` - Get help information
- `GET /api/predicates` - List predicates

## Security Considerations

- **API Key**: Always use API keys in production environments
- **Network Security**: Ensure the API server is properly secured
- **Input Validation**: The server validates all inputs before sending to the API
- **Timeout Protection**: All requests have configurable timeouts
- **Error Handling**: Comprehensive error handling prevents information leakage

## Troubleshooting

### Common Issues

1. **Connection Refused**: Ensure the Prolog Toolkit API server is running
2. **Authentication Failed**: Check your API key configuration
3. **Timeout Errors**: Increase the timeout value for complex queries
4. **Permission Denied**: Verify file paths and permissions for file consultation

### Logs

The server logs important events to stderr. Check Claude Desktop's logs for detailed error information.

### Support

For issues and questions:
1. Check the VSCode Prolog Toolkit documentation
2. Review the API server logs
3. Verify your configuration settings
4. Test the API endpoints directly

## License

This MCP server is part of the VSCode Prolog Toolkit project and follows the same licensing terms.