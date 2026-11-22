package histos.docs

import mermoid.ascent.MermoidAscent
import specular.*

import zio.test.*

/** What Histos keeps from the zipx lesson, without an sbt runtime. */
object Lineage extends DocSpec:

  def doc = page("Lineage")(
    md"""
zipx taught a lesson about expensive work: compile, package, and link should run when their
inputs changed, and a generated workflow should describe the jobs a repository actually has.
Histos keeps that lesson. The build is a Scala program. There is no sbt runtime underneath it.
""",
    illustration {
      MermoidAscent.diagram(Figures.lineage)
    }.assert { ui =>
      val text = ui.toString
      assertTrue(text.contains("ProjectGraph"), text.contains("content store"))
    },
    section("The program")(
      md"""
`HistosBuild.graph` returns a `ProjectGraph`. Modules, Scala versions, and platforms are
values in that program. A bad literal fails compilation of the build file.
""",
      cite[histos.HistosBuild](_.graph),
      cite[histos.ScalaVersion.type](_.from),
    ),
    section("The skip")(
      md"""
The content store names a blob by its digest. The API digest is the public signature, so a
downstream compile hits when that signature is unchanged. A laptop reads a remote cache.
Only CI writes one: `CacheConfig` opens a laptop's `write` as `read`.
""",
      cite[histos.cache.ContentStore](_.read),
      cite[histos.schedule.CacheConfig.type](_.open),
    ),
    section("The workflows")(
      md"""
`histos workflow --update` writes the workflows from the graph, including a job a plugin
contributed. sbt and Mill remain their own tools. Histos does not import either runtime.
A person who already has an sbt or zipx build can draft `histos.scala` with
`histos init --from sbt` or `histos init --from zipx`.
""",
      cite[histos.plugin.CiJob](_.render),
      cite[histos.scaffold.ImportSource.type](_.from),
    ),
    section("Who can ask")(
      md"""
The same graph answers a person at the terminal, Metals over BSP, and an agent over MCP.
The three clients are the point of the server, not three copies of the build.
""",
      cite[histos.bsp.Bsp.type](_.ServerName),
      cite[histos.mcp.HistosMcp.type](_.toolNames),
    ),
  )
end Lineage
