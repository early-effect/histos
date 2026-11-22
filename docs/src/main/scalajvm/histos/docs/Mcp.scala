package histos.docs

import specular.*

/** Why an agent uses the same server a person uses. */
object Mcp extends DocSpec:

  def doc = page("MCP")(
    md"""
MCP is the Model Context Protocol. An agent (a coding assistant, a script that drives the
build) uses it to call tools and read structured results. Histos speaks MCP so that agent
asks for the same operations you type at the terminal, and reads the same report.

You do not start a second build tool for the agent. The client's configuration names
`histos` as the program and `mcp` as its argument. The process speaks JSON on standard
input and standard output. There is no port to open.

```json
{
  "mcpServers": {
    "histos": {
      "command": "histos",
      "args": ["mcp"]
    }
  }
}
```
""",
    section("The tools are the commands")(
      md"""
Each tool is one operation you could have typed. The set is `graph`, `run`, `cancel`,
`compile`, `test`, `publish`, `version`, `assembly`, `clean`, `resolve`, `workflow`,
`fmt`, `reload`, `reboot`, `cachePath`, `cacheList`, `cacheClean`, `cacheForget`,
`cacheVerify`, `watch`, `preview`, `observe`, and `doctor`. A tool's description, next to
its name in the source, says which arguments it takes. `tools/list` returns this set and
nothing else, so an agent cannot invent a command Histos does not have.
""",
      cite[histos.mcp.HistosMcp.type](_.toolNames),
      cite[histos.mcp.HistosMcp.type](_.tools).elided(24),
    ),
    section("The agent is not sitting at a keyboard")(
      md"""
Some Histos commands ask a question, for example whether to accept a default. A person at
a terminal can answer. An MCP client has no keyboard unless it has negotiated a way to ask.
When it has not, Histos returns the question as a diagnosis instead of waiting forever.
The agent can then decide, or tell you.
""",
      cite[histos.mcp.McpConsent.type](_.unnegotiated),
    ),
    section("A failed compile is still a result")(
      md"""
If the compile or the test actually ran, the tool returns the report, including a report
that says the build failed. That is a result the agent can read. If the request was
refused before it started, because the arguments were not a real command, the tool call
itself fails. The agent can tell those apart: one means "I ran it, and it failed", the
other means "I did not run it".
""",
      cite[histos.cli.Help.type](_.render),
    ),
  )
end Mcp
