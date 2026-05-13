# client-conformance

Runs chimp's MCP client against the
official [MCP conformance test suite](https://github.com/modelcontextprotocol/conformance).

## What it does

The conformance harness is the inverse of a server: it **starts a test MCP server**, spawns the binary configured
via `--command` (this fat-jar wrapped in [`bin/chimp-conformance-client`](bin/chimp-conformance-client)), and passes the
test-server URL as the last argument. The client process reads the `MCP_CONFORMANCE_SCENARIO` env var to decide what
protocol exchange to drive, talks to the harness's server, and exits 0 on success.

This subproject is the binary the harness invokes. `Main.scala` dispatches on the scenario name and uses `chimp-client`
to drive the protocol.

## How to run

Using sbt task (assembles a client fat jar and runs test suite in one step):

```bash
sbt 'clientConformance/conformance client --suite core'
sbt 'clientConformance/conformance client --scenario initialize'
sbt 'clientConformance/conformance client --scenario tools_call'
```

The `@modelcontextprotocol/conformance` will be installed using npm, it must be available on the PATH.

## Adding a scenario

Add a case in `Main.scala` matching on the scenario name (the value of `MCP_CONFORMANCE_SCENARIO`), drive the protocol
with `McpClient`, return exit code 0 on success. Once it passes, remove the entry from the baseline file (see below).

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
