//> using dep com.softwaremill.chimp::chimp-client:0.6.0
//> using dep com.softwaremill.sttp.client4::core:4.0.27

package examples.client

import chimp.client.*
import chimp.client.transport.ClientHttpTransport
import chimp.protocol.*
import io.circe.Json
import sttp.client4.DefaultSyncBackend
import sttp.model.Header
import sttp.model.Uri.UriContext
import sttp.shared.Identity

import java.util.UUID

@main def parallelSearchClient(query: String, url: String): Unit =
  val backend = DefaultSyncBackend()
  try
    val transport = ClientHttpTransport[Identity](
      backend,
      uri"https://search.parallel.ai/mcp",
      headers = Seq(Header("User-Agent", "chimp-parallel-search-example/0.1.0"))
    )
    val client = McpClient[Identity](transport, Implementation("chimp-parallel-search-example", "0.1.0"))
    try
      val tools = client.listTools().tools.map(_.name)
      require(tools.contains("web_search") && tools.contains("web_fetch"), "Search and fetch tools are required")
      val sessionId = UUID.randomUUID().toString

      val search = client.callTool(
        "web_search",
        Json.obj(
          "objective" -> Json.fromString(query),
          "search_queries" -> Json.arr(Json.fromString(query)),
          "session_id" -> Json.fromString(sessionId)
        )
      )
      search.content.collect { case ToolContent.Text(_, text) => text }.foreach(println)
      require(!search.isError, "Web search failed")

      val fetch = client.callTool(
        "web_fetch",
        Json.obj("urls" -> Json.arr(Json.fromString(url)), "session_id" -> Json.fromString(sessionId))
      )
      fetch.content.collect { case ToolContent.Text(_, text) => text }.foreach(println)
      require(!fetch.isError, "Web fetch failed")
    finally client.close()
  finally backend.close()
