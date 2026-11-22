package histos.docs

import mermoid.ascent.MermoidAscent
import specular.*

import zio.test.*

/** What Metals gets from `histos bsp`. */
object Metals extends DocSpec:

  def doc = page("Metals")(
    md"""
Metals is the Scala language server most editors use for completion, diagnostics, and
go-to-definition. It does not read `histos.scala` itself. It asks a build server, over BSP
(the Build Server Protocol), for the targets, the classpath, and the compiler diagnostics.

Histos is that build server. When you import the build, Metals reads `.bsp/histos.json` in
the project and starts the command listed there. That command is `histos bsp`. `histos
update` rewrites the file from the pin, so after you change Histos versions the editor
starts the Histos you just pinned.
""",
    illustration {
      MermoidAscent.diagram(Figures.clients)
    }.assert { ui =>
      assertTrue(ui.toString.contains("histos bsp"))
    },
    section("What you can do after import")(
      md"""
The server calls itself `histos` and speaks BSP 2.2. After import, Metals can list
targets, find sources, read scalac options, compile, and ask for main classes and test
classes. Run and test from the editor use the JVM run and test environments Histos
reports. Diagnostics are pushed as they are produced, so a compile error shows up in the
editor without a separate command.
""",
      cite[histos.bsp.Bsp.type](_.BspVersion),
      cite[histos.bsp.Bsp.type](_.ServerName),
    ),
    section("One editor target is one cell")(
      md"""
A target is one cell: one module, one Scala version, one platform, and either main sources
or test sources. A module you build for the JVM and for Scala.js shows up as separate
targets. `histos.scala` is a target of its own, so a mistake in the build file is reported
like any other compile error.
""",
      cite[histos.Platform](_.token),
    ),
    section("It stays on the JVM")(
      md"""
`histos bsp` does not go through the native client. The editor needs a long-lived process
that speaks the protocol, and that process is the JVM server. You do not start it by hand
in normal use. Metals starts it from `.bsp/histos.json`. You can run `histos bsp` yourself
when you are debugging the connection.
""",
      cite[histos.cli.Help.type](_.entries).elided(20),
    ),
  )
end Metals
