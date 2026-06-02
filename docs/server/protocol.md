# MCP Protocol

Chimp implements the HTTP transport of the [MCP protocol](https://modelcontextprotocol.io/specification/2025-03-26) (version **2025-03-26**). Only tools are supported, via the following JSON-RPC commands:

- Initialization and capabilities negotiation (`initialize`)
- Listing available tools (`tools/list`)
- Invoking a tool (`tools/call`)

All requests and responses use JSON-RPC 2.0. Tool input schemas are described using JSON Schema, auto-generated from Scala types.
