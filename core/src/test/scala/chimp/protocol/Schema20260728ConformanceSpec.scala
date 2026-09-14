package chimp.protocol

/** Schema conformance for the modern 2026-07-28 revision.
  *
  * Seeded with datatypes shared across revisions so the per-version harness runs against the 2026-07-28 schema; it grows with the
  * revision-specific defs (result type, cache hints, discover) as those datatypes are added in later tasks.
  */
class Schema20260728ConformanceSpec extends SchemaConformance:

  override def schemaResourcePath: String = "/schema/2026-07-28/schema.json"

  it should "produce Implementation that matches the spec schema" in:
    validate("Implementation", Implementation(name = "chimp", version = "1.0", title = Some("Chimp")))

  it should "produce TextContent that matches the spec schema" in:
    validate("TextContent", ToolContent.Text(text = "hi"))
