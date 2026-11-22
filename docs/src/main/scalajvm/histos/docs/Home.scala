package histos.docs

import mermoid.ascent.MermoidAscent
import specular.*

import zio.test.*

/** The front door. The manual starts at Install. */
object Home extends DocSpec:

  def doc = page("Histos")(
    md"""
Histos is a Scala build tool. The build is a Scala program that returns a graph. The work is a
ZIO effect. A person, an editor, and an agent ask for the same operations and read the same
results.

Expensive work runs when the code it depends on changed. A skip that hides a real change is a
failure, even when the command was fast.
""",
    section("Three clients, one graph")(
      md"""
The terminal, Metals, and an MCP client are three doors into one server. Metals speaks BSP.
An agent speaks MCP. Both run beside the commands a person types. The project graph is what
they ask about.
""",
      illustration {
        MermoidAscent.diagram(Figures.clients)
      }.assert { ui =>
        val text = ui.toString
        assertTrue(text.contains("terminal"), text.contains("histos bsp"), text.contains("histos mcp"))
      },
      cite[histos.bsp.Bsp.type](_.BspVersion),
      cite[histos.mcp.HistosMcp.type](_.toolNames),
    ),
    section("A typed build")(
      md"""
`histos.scala` is a program. `object build extends HistosBuild` returns a `ProjectGraph`.
Versions, dependencies, paths, and names are typed literals, so a bad one fails compilation
of the build file.
""",
      cite[histos.HistosBuild](_.graph).elided(12),
      cite[histos.ScalaVersion.type](_.from),
    ),
    section("A skip you can trust")(
      md"""
Compile products are content-addressed. The public signature of a module is its API digest.
Class bytes, with line numbers and the TASTY attribute taken out, are the product digest.
A downstream compile hits when the upstream signature did not change. A comment does not
move that signature.
""",
      cite[histos.zinc.ApiDigest.type](_.of),
      cite[histos.zinc.ProductDigest.type].elided(8),
    ),
    section("Install")(
      md"""
One POSIX script is the installer and the command. It installs itself as `~/.histos/bin/histos`
and does not edit a shell rc.

```sh
curl -fsSL https://github.com/early-effect/histos/releases/download/dist/shim | sh
export PATH="$$HOME/.histos/bin:$$PATH"
```

The next page is the pin, the native client, and the first project.
"""
    ),
  )
end Home
