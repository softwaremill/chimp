# Using with ZIO

When using ZIO, you might have to explicitly state the effect type that you are using, as the Tapir-ZIO integration requires a `RIO[R, A]` effect (which is an alias for `ZIO[R, Throwable, A]`), for example:

```scala
val myServerTool = myTool.serverLogic[[X] =>> RIO[Any, X]]: (input, ctx, headers) =>
  ZIO.succeed(ToolResult.text(???))
```
