package histos.docs

import mermoid.ascent.MermoidAscent
import specular.*

import zio.test.*

/** Modules, cells, platforms, and the literals that build them. */
object BuildModel extends DocSpec:

  def doc = page("The build")(
    md"""
`histos.scala` describes what you want built. Histos turns that description into work it
can compile, test, and publish. Two words are enough to read the file.

A module is a named piece of the project, the thing you would have called a subproject.
You give it one or more platforms and one or more Scala versions.

A cell is one combination: one module, one Scala version, and one platform. Compile, test,
and publish run on a cell. If a module is built for the JVM and for Scala.js, those are
two cells, and a change in one does not by itself rebuild the other.
""",
    illustration {
      MermoidAscent.diagram(Figures.cell)
    }.assert { ui =>
      val text = ui.toString
      assertTrue(text.contains("cell"), text.contains("compile"))
    },
    section("Platforms")(
      md"""
There are three platforms. `Jvm` is a JVM program or library. `Js` is Scala.js. `Native`
is Scala Native. On the command line the tokens are `jvm`, `js`, and `native`, so
`histos run app js` is the Scala.js cell of the `app` module.
""",
      cite[histos.Platform](_.token),
    ),
    section("Versions and dependencies are checked when the build file compiles")(
      md"""
You write a Scala version as `scala"x.y.z"` and a dependency as `dep"org::name:revision"`.
Those are macros. A version that is not a full Scala version, or a dependency that is not
a real coordinate, fails compilation of `histos.scala`. Histos never gets as far as
resolving it. A catalog row, the thing you publish, is
`Ship("name").next(major, minor, patch)(the modules)`. The number on `next` is the next
release. You do not write `SNAPSHOT` in the catalog.
""",
      cite[histos.Dep.type](_.from),
    ),
    section("What this repository builds")(
      md"""
This repository is itself a Histos build. `object build extends HistosBuild` in
`histos.scala` returns the graph. The ship lists the modules that are published, and it
names two roles: `app` is the launcher people run, and `client` is the optional native
client. Reading that file is the example. The manual does not keep a second copy of the
version numbers.
""",
      cite[histos.ShipDef].signature.elided(16),
    ),
  )
end BuildModel
