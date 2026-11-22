package histos.docs

import specular.*

/** Why an agent uses the same server a person uses. */
object Mcp extends DocSpec:

  def doc = page("MCP")(
    md"""
An agent uses MCP. The client's stdio server is `histos` with the argument `mcp`. The tools
are the same operations the terminal runs. A tool returns the report as data, with each line
as it is printed.

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
    section("The same call")(
      md"""
`HistosMcp.Names` is the tool list: `graph`, `run`, `cancel`, `compile`, `test`, `publish`,
`version`, `assembly`, `clean`, `resolve`, `workflow`, `fmt`, `reload`, `reboot`,
`cachePath`, `cacheList`, `cacheClean`, `cacheForget`, `cacheVerify`, `watch`, `preview`,
`observe`, and `doctor`. Each registration sits next to that name and describes the
arguments the tool takes.
""",
      cite[histos.mcp.HistosMcp.type](_.toolNames),
      cite[histos.mcp.HistosMcp.type](_.tools).elided(24),
    ),
    section("Consent")(
      md"""
The MCP pipe is not a terminal. A consent question the client has not negotiated comes back
as a diagnosis, from `McpConsent`.
""",
      cite[histos.mcp.McpConsent.type](_.unnegotiated),
    ),
    section("A report, or a fault")(
      md"""
A compile or a test that ran returns a report, including one that failed. A request the
grammar refused fails the tool. That split is `Session.CallProblem` on the tool
registrations.
""",
      cite[histos.cli.Help.type](_.render),
    ),
  )
end Mcp
