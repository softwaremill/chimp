# chimp — agent instructions

## Protocol reference

Never guess protocol shapes, field names or error codes; check them against the MCP specification at
https://modelcontextprotocol.io/specification.

`chimp.protocol.ProtocolVersion` enumerates the revisions chimp recognises and is the
source of truth for which those are. Implement behaviour for `ProtocolVersion.Latest` only; the older
revisions are there for negotiation, and `negotiate` answers an unknown or older proposal with
`Latest`. Consult an older revision when a change touches that negotiation surface, not to add
version-specific behaviour for it.

## Tests

### Shared vs. specific tests

Most behaviour is identical across transports and effect backends, so tests are written once and run
everywhere. Two file kinds implement this:

* `*Tests.scala` - a trait holding the tests themselves, abstract over the effect type `F[_]` and
  over how the server or client under test is set up. Contains no wiring.
* `*Spec.scala` - the runnable suite: mixes in the `*Tests` traits it wants and supplies the wiring
  (start a server, open a transport, run a subprocess). Some `client/` specs are themselves abstract
  in `F[_]` and are made concrete per backend under `client-streaming/` or `server-streaming/` modules.

**Put a new test in a shared `*Tests` trait by default.** Write it directly in a `*Spec`, or in a
transport-specific trait, only when the behaviour genuinely is specific. 
A test that passes on one transport and would pass on the others belongs in the common file, not copied into each.

The shared traits must live in the base module (`server/` or `client/`), never in a backend module.

### Where the shared traits live

Server, in `server/src/test/scala/chimp/server/`:

* `McpServerTests` - core server behaviour, expected of every transport
* `McpServerStreamingTests` - streaming-only concerns (notifications emitted during a tool call);
  mixed in only by specs whose transport supports streaming
* `ServerStdioTransportTests` - stdio-specific framing and message handling

Client, in `client/src/test/scala/chimp/client/integration/`:

* `McpClientTests` - core client behaviour, expected of every transport
* `McpClientBidirectionalTests` - sampling, roots and elicitation handlers
* `McpClientBidirectionalHttpTests` - HTTP-only concerns

A spec composes the traits that apply to it: an HTTP streaming server spec mixes in both
`McpServerTests` and `McpServerStreamingTests`, while the stdio spec mixes in
`ServerStdioTransportTests` only.

### Abstracting over the effect type

Shared traits are generic in `F[_]` and self-type onto `ToFuture[F]`, which supplies the
`MonadError[F]` instance plus `toFuture`, `sleep` and `waitUntil` helpers. 

Setup is exposed as abstract methods on the trait (`withServer`, `withStreamingServer`,
`withClient`, `withBidirectionalClient`, `runStdioServer`) which each spec implements.

### Integration tests

Tests that need external processes - `npx @modelcontextprotocol/server-everything`, Docker via
testcontainers, toxiproxy - must mix in `IntegrationSpec`, which tags every test in the suite as
`Integration`. CI runs those separately.

### Conformance tests

When working with conformance tests refer to the actual sources <https://github.com/modelcontextprotocol/conformance>.

For instructions on client conformance refer to `/client-conformance/README.md`.
For instructions on server conformance refer to `/server-conformance/README.md`.

## Documentation

The docs site lives in `docs/`, one page per topic under `docs/server/` and `docs/client/`. A new
page has to be registered in the toctree in `docs/index.md` or it will not be reachable.

The main `README.md` should be updated only by explicit request, as it should be kept short.

Document a change when it is:

* non-obvious to the users
* specific to a particular integration (like ZIO, ox and other Scala frameworks)
* implementing support for a new protocol feature or mechanic

Refactorings and bug fixes that leave the public API unchanged need no documentation.

Keep the documentation short and don't explain the full rationale of the lower level code. Write what
a user has to do, not how the internals work.

Scala snippets in the docs are compiled, so run `sbt compileDocs` after editing them. Never commit
`generated-docs/`; it is mdoc output, regenerated at release.

For specific instructions on working with the docs site, refer to `docs/README.md`.

## Language

User ASD-STE100 Simplified Technical English for documentation, responses and user interactions.

## Code comments

Do not add any comments in the code, unless you're asked to do so.

