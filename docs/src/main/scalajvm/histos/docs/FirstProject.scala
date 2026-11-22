package histos.docs

import specular.*

/** `new` and `init`, including a draft from sbt or zipx. */
object FirstProject extends DocSpec:

  def doc = page("First project")(
    md"""
`histos new <name>` writes a project in a new directory. `histos init` writes one into the
current directory. Both pin the toolchain that wrote them.
""",
    cite[histos.cli.ScaffoldChoice].signature,
    section("How much to write")(
      md"""
`--bare` writes the build file and the sources. `--yes` takes the defaults of the full
scaffold, including the CI branch. With neither flag, the command asks.
""",
      cite[histos.cli.ScaffoldChoice.type](_.from),
    ),
    section("Draft from an existing build")(
      md"""
`histos init --from sbt` reads `build.sbt`. `histos init --from zipx` reads
`project/ZipxVersions.scala`. A setting the draft does not map stays as a comment.
""",
      cite[histos.scaffold.ImportSource.type](_.from),
    ),
    section("The file `new` writes")(
      md"""
The scaffold writes `object build extends HistosBuild` with one JVM module, a ZIO dependency,
and a main class. That text is `Skeleton`'s build file.
""",
      cite[histos.scaffold.Skeleton.type].elided(20),
    ),
  )
end FirstProject
