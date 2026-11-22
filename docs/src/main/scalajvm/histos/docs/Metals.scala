package histos.docs

import mermoid.ascent.MermoidAscent
import specular.*

import zio.test.*

/** What Metals gets from `histos bsp`. */
object Metals extends DocSpec:

  def doc = page("Metals")(
    md"""
Metals speaks BSP to Histos. Open the project and import the build. Metals reads
`.bsp/histos.json` and starts the `argv` in that file. `histos update` rewrites the file
from the pin.
""",
    illustration {
      MermoidAscent.diagram(Figures.clients)
    }.assert { ui =>
      assertTrue(ui.toString.contains("histos bsp"))
    },
    section("The protocol")(
      md"""
The server name is `histos`. The version is BSP 2.2. The methods are the cases of
`Bsp.Method`: initialize, targets, sources, inverse sources, scalac options, dependency
sources, compile, main classes, test classes, and the JVM run and test environments.
""",
      cite[histos.bsp.Bsp.type](_.BspVersion),
      cite[histos.bsp.Bsp.type](_.ServerName),
    ),
    section("One target is one cell")(
      md"""
A target is one cell: a module, a Scala version, a platform, and main or test. `histos.scala`
is its own target. Diagnostics go out as `build/publishDiagnostics`.
""",
      cite[histos.Platform](_.token),
    ),
    section("Where it runs")(
      md"""
`bsp` stays on the JVM. The command is `histos bsp`.
""",
      cite[histos.cli.Help.type](_.entries).elided(20),
    ),
  )
end Metals
