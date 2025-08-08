# MCP Protocol Documentation

This document describes the Model Context Protocol (MCP) handlers, tools, resources, and schemas implemented in the VSCode Prolog Toolkit MCP server. It complements the REST API OpenAPI spec (`openapi.yaml`) by documenting the protocol-level capabilities, Zod-based schemas, and resource URIs available via the MCP server.

## Overview
- **MCP Server**: Provides advanced Prolog, CLP, N3, and probabilistic reasoning features via a protocol-based interface.
- **Schemas**: All tool and resource handlers use Zod schemas for input validation and documentation.
- **Discoverability**: Tools and resources are discoverable via `ListToolsRequest` and `ListResourcesRequest`.

## Tools
Each tool is registered with a Zod schema describing its input and output. Tools include:

- `clp_constraint_solving`: Constraint Logic Programming (CLP) solver.
- `probabilistic_inference`: Probabilistic logic inference.
- `n3_semantic_reasoning`: N3/semantic web reasoning.
- `batch_query_execution`: Batch execution of Prolog queries.
- `query_history`: Retrieve query history.
- `system_status`: Get backend system status.

(See `src/features/toolSchemas.ts` for full schema definitions.)

### Example Tool Schema (TypeScript/Zod)
```ts
const ClpConstraintSolvingSchema = z.object({
  constraints: z.array(z.string()),
  domain: z.enum(["fd", "r", "q"]).default("fd"),
  variables: z.array(z.string()),
});
```

## Resources
Resources are discoverable and accessible via resource URIs. Examples:
- `prolog://history`: Query history resource.
- `prolog://system-status`: System status resource.

Each resource has a Zod schema for its request/response.

## Handler Registration
All handlers are registered in `src/index.ts` using their Zod schemas. Example:
```ts
registerToolHandler("clp_constraint_solving", ClpConstraintSolvingSchema, handlerFn);
registerResourceHandler("prolog://history", QueryHistorySchema, handlerFn);
```

## Protocol Operations
- **ListToolsRequest**: Returns all available tools and their schemas.
- **ListResourcesRequest**: Returns all available resources and their schemas.
- **Tool/Resource Invocation**: Each request is validated against its Zod schema.

## Extending the Protocol
To add a new tool or resource:
1. Define a Zod schema in `src/features/toolSchemas.ts`.
2. Register the handler in `src/index.ts`.
3. Document the tool/resource here.

## See Also
- [REST API OpenAPI Spec](./openapi.yaml)
- [Tool Schemas](../src/features/toolSchemas.ts)
- [MCP Server Main](../src/index.ts)

---
_Last updated: 2025-08-08_
