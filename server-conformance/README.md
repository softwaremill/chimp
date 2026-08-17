# server-conformance

Runs chimp's MCP server against the official [MCP conformance test suite](https://github.com/modelcontextprotocol/conformance).

## What it does

`Main.scala` starts a chimp streaming MCP server on a Netty sync backend and prints the bound URL to stdout. 
The server registers the tools, resources, and prompts that the conformance
scenarios expect. The conformance harness then connects to the server as an MCP client and runs the
server-side scenarios against it.

The harness acts as the **client**; our `Main` is the **server under test**.

## Harness version and target protocol

The sbt task pins the harness to the version set in `conformanceHarnessV` in [`build.sbt`](../build.sbt).

## How to run

Run the full requirement set:

```bash
sbt 'serverConformance/conformance server --requirements 2025-11-25'
```

Run one scenario during development:

```bash
sbt 'serverConformance/conformance server --scenario ping --spec-version 2025-11-25'
sbt 'serverConformance/conformance server --scenario server-initialize --spec-version 2025-11-25'
```

The sbt task downloads `@modelcontextprotocol/conformance` with npm; `npx` must be available on the PATH.

The server binds an ephemeral port by default. To pin it: pass `--port=NNNN` or set `CHIMP_CONFORMANCE_PORT`.

## Adding a scenario

Add server code that handles the flow that a given scenario expects. The harness's failure output tells you which name
and what result shape it wants. Remove the corresponding entry from the baseline file after the scenario passes.

## The baseline file

[`conformance-baseline.yml`](../conformance-baseline.yml) lists scenarios that are known to fail today. The harness
uses it like this:

| Scenario result | In baseline? | Exit code | Meaning                               |
|-----------------|--------------|-----------|---------------------------------------|
| Fails           | Yes          | 0         | Expected failure — keep working on it |
| Fails           | No           | 1         | Regression — CI fails                 |
| Passes          | Yes          | 1         | Stale baseline — remove the entry     |
| Passes          | No           | 0         | Normal pass                           |

So the file shrinks as the SDK matures.
