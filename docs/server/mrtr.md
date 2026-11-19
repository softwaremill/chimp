# Asking the client for input

In protocol version `2026-07-28`, a server does not send requests to the client while it handles a request. Instead, a handler can end a `tools/call`, `prompts/get` or `resources/read` with an **input-required** outcome. The client gets the input that the handler asks for, and then sends the same request again with the answers. The server keeps no state between the two requests. This is the [multi round-trip requests](https://modelcontextprotocol.io/specification/2026-07-28/basic/patterns/mrtr) (MRTR) pattern.

## Requesting input

A tool, prompt or resource handler returns `InputRequired` instead of its result. `InputRequired` names each input request with a key that the handler chooses:

| Input request | What the client does | Client capability that it needs |
|---|---|---|
| `InputRequired.elicitForm(message, schema)` | asks the user to fill in a form | `elicitation` (form mode) |
| `InputRequired.elicitUrl(message, url)` | asks the user to open a URL | `elicitation.url` |
| `InputRequired.sample(params)` | samples its LLM | `sampling` |
| `InputRequired.listRoots` | sends its roots | `roots` |

On the retry, the handler reads the answers under the same keys from `ctx.request`: `elicitResult(key)`, `samplingResult(key)` and `rootsResult(key)`. A handler that never returns `InputRequired` works as before.

```scala mdoc:compile-only
import chimp.server.*
import io.circe.{Codec, Json}
import sttp.shared.Identity
import sttp.tapir.*

case class NoInput() derives Codec, Schema

val nameSchema = Json.obj(
  "type" -> Json.fromString("object"),
  "properties" -> Json.obj("name" -> Json.obj("type" -> Json.fromString("string")))
)

val greet = tool("greet")
  .input[NoInput]
  .serverLogicWithContext[Identity]: (_, ctx, _) =>
    ctx.request.elicitResult("user_name").flatMap(_.content).flatMap(_.get("name")).flatMap(_.asString) match
      case Some(name) => ToolResult.text(s"Hello, $name!")
      case None       => InputRequired("user_name" -> InputRequired.elicitForm("What is your name?", nameSchema))
```

Prompts and resources use the same `InputRequired` from their `serverLogicWithContext`.

### Several rounds and the request state

A handler can ask again on the retry, as many times as it needs. To remember what happened in the earlier rounds, it attaches a **request state** with `.withRequestState(state)`. The client sends the state back without a change on its retry, and the handler reads it from `ctx.request.requestState`. `InputRequired.withRequestState(state)` makes an outcome that has a state and no input requests: the client then retries at once.

```{warning}
The request state goes through the client, so treat it as input from an attacker. If the state controls authorization, resource access or business logic, protect its integrity with a [request state signer](#protecting-the-request-state).
```

### Missing answers

If the retry does not have an answer that the handler needs, return a new `InputRequired` that asks for it again. Do not fail the request. The client can omit an answer, or send it under a different key. Keys that the handler did not ask for are ignored.

### Client capabilities

A handler can read `ctx.request.clientCapabilities` and ask only for what the client supports. For example, it can ask for sampling when the client declares `sampling`, and for a form if not.

## Rules that chimp enforces

chimp applies these rules, so a handler does not have to:

| Problem | Result |
|---|---|
| an input request that needs a capability which the client did not declare | `-32021` MissingRequiredClientCapability with `data.requiredCapabilities`, HTTP `400` |
| `inputResponses` is not an object, or one of its values is not a sampling, roots or elicitation result | `-32602` InvalidParams, HTTP `400`; the handler does not run |
| `requestState` is not a string | `-32602` InvalidParams, HTTP `400`; the handler does not run |
| `InputRequired` on a legacy (`2025-11-25` or earlier) request | `-32603` InternalError which names protocol version `2026-07-28` |

An `InputRequired` always has at least one input request or a request state. Only `tools/call`, `prompts/get` and `resources/read` can end with it.

On a streaming HTTP server, chimp opens the event stream when the handler sends its first message. If the handler ends with the `-32021` before it sends a message, chimp sends a plain JSON error with HTTP `400`. If the handler already sent a message, for example a progress notification, the `-32021` is the last message on the event stream.

## Protecting the request state

Give the server a `RequestStateSigner` to protect the integrity of all request states:

```scala mdoc:compile-only
import chimp.server.*
import sttp.shared.Identity
import scala.concurrent.duration.*

val secret: Array[Byte] = sys.env("MCP_REQUEST_STATE_SECRET").getBytes("UTF-8")

val server = McpServer[Identity]()
  .withRequestStateSigner(RequestStateSigner(secret, ttl = 5.minutes))
```

The handler code does not change. When a handler returns a request state, chimp signs it with HMAC-SHA256. The signature also covers:

- an expiry, after the `ttl` (10 minutes by default),
- the method, the tool or prompt name or the resource URI, and a digest of the arguments of the request,
- on a secured server, the principal: use `withRequestStateSigner(signer, principalId)`, where `principalId` gives a stable identifier of the principal.

On the retry, chimp verifies the state before the handler runs, and gives the handler the original state. If the state was changed, has expired, or belongs to another request or principal, chimp sends `-32602` InvalidParams with HTTP `400`, and the handler does not run.

Security notes:

- The secret must have at least 32 bytes. Keep it in a secret store, not in the code. All instances of the server must use the same secret, or a retry can fail on a different instance.
- When you change the secret, all request states that are in progress become invalid. The clients must start these requests again.
- The signer protects the integrity of the state, not its confidentiality. The client can read the state, so do not put secrets in it.
- The signer does not make a state single-use. Within its lifetime, the client can send the same state again on the same request. If an action must occur only one time, record it on the server.

