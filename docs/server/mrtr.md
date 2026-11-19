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
The request state goes through the client, so treat it as input from an attacker. If the state controls authorization, resource access or business logic, protect its integrity, and reject a state that you cannot verify.
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
