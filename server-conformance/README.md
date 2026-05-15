# server-conformance

Runs chimp's MCP server against the
official [MCP conformance test suite](https://github.com/modelcontextprotocol/conformance).

## What it does

`Main.scala` starts a small chimp MCP server on a Netty sync backend, registers a handful of tools that the conformance
scenarios expect (`add_numbers`, `test_simple_text`, `test_error_handling`), and prints the bound URL to stdout. The
conformance harness then connects to it as an MCP client and runs the server-side scenarios against it.

The harness is run separately (via the `conformance` sbt task or by hand with `npx`); it acts as the **client**,
our `Main` is the **server under test**.

## How to run

Using sbt task (assembles a server fat jar and runs test suite in one step):

```bash
sbt 'serverConformance/conformance server'
sbt 'serverConformance/conformance server --scenario ping'
sbt 'serverConformance/conformance server --scenario server-initialize'
```

The `@modelcontextprotocol/conformance` will be installed using npm, it must be available on the PATH.

The server binds an ephemeral port by default. To pin it: pass `--port=NNNN` or set `CHIMP_CONFORMANCE_PORT`.

## Adding a scenario

Add server code handling the flow that a given scenario expects. The harness's failure output tells you which name and
what result shape it wants. Remove the corresponding entry from the baseline file once it passes.

## The baseline file

[`conformance-baseline.yml`](../conformance-baseline.yml) lists scenarios that are known to fail today. The harness uses
it like this:

| Scenario result | In baseline? | Exit code | Meaning                               |
|-----------------|--------------|-----------|---------------------------------------|
| Fails           | Yes          | 0         | Expected failure — keep working on it |
| Fails           | No           | 1         | Regression — CI fails                 |
| Passes          | Yes          | 1         | Stale baseline — remove the entry     |
| Passes          | No           | 0         | Normal pass                           |

So the file shrinks as the SDK matures.
