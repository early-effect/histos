package histos.docs

import mermoid.ascent.MermoidAscent
import specular.*

import zio.test.*

/** What Histos keeps from the zipx lesson, without an sbt runtime. */
object Lineage extends DocSpec:

  def doc = page("Lineage")(
    md"""
zipx is a plugin that makes sbt skip work it does not need to do, and that generates the
GitHub workflows a repository actually has. That lesson is worth keeping. The sbt runtime
underneath it is not required for the lesson.

Histos is a build tool of its own. You write the build in Scala. Histos compiles it,
caches the outputs, and writes the workflows. sbt and Mill remain their own tools. Histos
does not shell out to either of them, and it does not rank itself against them with a
benchmark. If you already have one of those builds, you can draft a Histos file from it
and then own the draft.
""",
    illustration {
      MermoidAscent.diagram(Figures.lineage)
    }.assert { ui =>
      val text = ui.toString
      assertTrue(text.contains("ProjectGraph"), text.contains("content store"))
    },
    section("The build is a program")(
      md"""
`histos.scala` is the program. `graph` returns a `ProjectGraph`: the modules, the Scala
versions, and the platforms. A bad version or a bad dependency fails compilation of that
file, so you find out before a command runs.
""",
      cite[histos.HistosBuild](_.graph),
      cite[histos.ScalaVersion.type](_.from),
    ),
    section("A skip has to be true")(
      md"""
Outputs live in a content store, named by the digest of their bytes. The API digest is
the public signature of a module. A downstream compile is reused when that signature is
unchanged. A laptop may read a remote cache. Only CI writes one, so a machine you are
experimenting on cannot publish an output that other machines then trust.
""",
      cite[histos.cache.ContentStore](_.read),
      cite[histos.schedule.CacheConfig.type](_.open),
    ),
    section("The workflows come from the graph")(
      md"""
`histos workflow --update` writes the GitHub workflows from the graph, including a job a
plugin added. `histos workflow --check` fails when the files in the tree have drifted
from that generation. Neither command publishes, and neither changes the Histos pin.

If you are moving a repository, `histos init --from sbt` reads `build.sbt`, and
`histos init --from zipx` reads `project/ZipxVersions.scala`. A setting the draft does
not map stays as a comment for you to decide.
""",
      cite[histos.plugin.CiJob](_.render),
      cite[histos.scaffold.ImportSource.type](_.from),
    ),
    section("One graph, three ways to ask")(
      md"""
The terminal, Metals, and an MCP agent are three clients of one server. They do not each
carry a copy of the build. Metals speaks BSP. An agent speaks MCP. Both call the same
graph the terminal compiles.
""",
      cite[histos.bsp.Bsp.type](_.ServerName),
      cite[histos.mcp.HistosMcp.type](_.toolNames),
    ),
  )
end Lineage
