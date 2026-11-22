package histos.docs

import mermoid.ascent.MermoidAscent
import specular.*

import zio.test.*

/** Modules, cells, platforms, and the literals that build them. */
object BuildModel extends DocSpec:

  def doc = page("The build")(
    md"""
A module names its platforms and its Scala versions. A cell is one module, one Scala version,
and one platform. Compile, test, and publish run on a cell.
""",
    illustration {
      MermoidAscent.diagram(Figures.cell)
    }.assert { ui =>
      val text = ui.toString
      assertTrue(text.contains("cell"), text.contains("compile"))
    },
    section("Platforms")(
      md"""
The three platforms are `Jvm`, `Js`, and `Native`. The command tokens are `jvm`, `js`, and
`native`.
""",
      cite[histos.Platform](_.token),
    ),
    section("Literals")(
      md"""
`scala"..."`, `dep"..."`, `ship"..."`, and the rest are macros. A bad literal fails
compilation of the build file. A catalog row is `Ship("name").next(major, minor, patch)(...)`.
""",
      cite[histos.Dep.type](_.from),
    ),
    section("What this repository builds")(
      md"""
This repository's graph is `object build extends HistosBuild` in `histos.scala`. The ship is
`Ship("histos").next(0, 5, 0)` with a toolchain: `app` is the launcher and `client` is the
native client.
""",
      cite[histos.ShipDef].signature.elided(16),
    ),
  )
end BuildModel
