package histos.docs

import mermoid.Mermaid

/** Diagrams the site draws. `Mermaid` parses each one at compile time. */
object Figures:

  /** A person, Metals, and an MCP client reach one server. The native client is optional. */
  val clients: Mermaid =
    Mermaid("""flowchart TB
      |    person[terminal] --> server[JVM server]
      |    person -.-> native["histos-client (optional)"]
      |    native --> server
      |    metals[Metals] --> bsp[histos bsp]
      |    agent[MCP client] --> mcp[histos mcp]
      |    bsp --> server
      |    mcp --> server
      |    server --> graph[project graph]
      |""".stripMargin)

  /** One cell is one module, one Scala version, and one platform. */
  val cell: Mermaid =
    Mermaid("""flowchart TB
      |    module[module] --> cell[cell]
      |    scala[Scala version] --> cell
      |    platform[Jvm, Js, or Native] --> cell
      |    cell --> compile[compile]
      |    cell --> test[test]
      |    cell --> publish[publish]
      |""".stripMargin)

  /** Lookup order for a remote cache hit. */
  val lookup: Mermaid =
    Mermaid("""flowchart LR
      |    ask[action] --> namespace[this namespace]
      |    namespace --> main[main]
      |    main --> local[local store]
      |    local --> run[run the action]
      |""".stripMargin)

  /** What the build writes, and who reads it. */
  val lineage: Mermaid =
    Mermaid("""flowchart TB
      |    program[histos.scala] --> graph[ProjectGraph]
      |    graph --> zinc[compile]
      |    graph --> store[content store]
      |    graph --> workflows[generated workflows]
      |    store --> zinc
      |""".stripMargin)
end Figures
