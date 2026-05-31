# Client

Chimp ships an MCP client that connects to any MCP-compliant server. The client is parameterised over an effect type `F[_]` and is paired with a pluggable transport that carries JSON-RPC messages.

Add the dependency to your `build.sbt`:

```scala
libraryDependencies += "com.softwaremill.chimp" %% "chimp-client" % "0.2.0"
```

For streaming transports (e.g. ZIO), also add:

```scala
libraryDependencies += "com.softwaremill.chimp" %% "chimp-client-zio" % "0.2.0"
```

```{eval-rst}
.. toctree::
   :maxdepth: 2

   transport
   capabilities
   examples
```
