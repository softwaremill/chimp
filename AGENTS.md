## Overview

Chimp is an SDK for building [MCP](https://modelcontextprotocol.io/specification) (Model Context Protocol) servers and
clients in Scala 3 using boilerplate-less, type-safe APIs based on [Tapir](https://tapir.softwaremill.com/)
and [sttp](https://github.com/softwaremill/sttp), supporting the variety of the Scala ecosystem.

This is public OSS project.

## Rules

### 1 - Protocol Conformance

Supported protocol version are located in `chimp.protocol.ProtocolVersion`. ALWAYS refer and check your implementation
against the official MCP specification https://modelcontextprotocol.io/specification.

### 2 - Tests organization

Most MCP behaviour is identical across transports and effect backends, such tests are defined once and run everywhere.
When implementing tests ALWAYS place them in shared trait's unless they are somehow specific.

### 3 - Conformance tests

When working with conformance tests ALWAYS refer to the actual
sources <https://github.com/modelcontextprotocol/conformance>.

For instructions on client conformance refer to `/client-conformance/README.md`.
For instructions on server conformance refer to `/server-conformance/README.md`.

### 4 - Documentation

Document a change ONLY when it is:

* non-obvious to the users
* specific to a particular effect backend
* implementing support for a new protocol feature

For specific instructions on working with the docs refer to `/docs/README.md`.

### 5 - Language

ALWAYS use ASD-STE100 Simplified Technical English for documentation, responses and user interactions.

### 6 - Code comments

NEVER add any comments in the code, unless you're asked to do so.
