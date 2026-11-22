package histos.docs

import specular.*

/** `new` and `init`, including a draft from sbt or zipx. */
object FirstProject extends DocSpec:

  def doc = page("First project")(
    md"""
Once `histos` is on your `PATH`, you can start a project from nothing, or draft one from a
build you already have. Both commands write `histos.scala`, the sources, and a
`build.properties` that pins the Histos that wrote them. The pin is how the next person who
clones the directory runs the same toolchain.
""",
    section("A new directory, or this one")(
      md"""
`histos new <name>` creates a directory and writes the project inside it. `histos init`
writes the project into the directory you are already in. Use `new` when you are starting
fresh. Use `init` when the directory already exists and you want Histos files added to it.

With no extra flags, the command asks you how much to write. You do not have to remember
the flags to get a working project.
""",
      cite[histos.cli.ScaffoldChoice].signature,
    ),
    section("How much to write")(
      md"""
`--bare` writes the build file and the sources, and stops. That is the right choice when
you will add CI yourself.

`--yes` accepts the defaults of the full scaffold, including the branch name CI will use,
and does not ask. Use it from a script.

`--branch <name>` sets that CI branch when you do not want the default.

If you pass neither `--bare` nor `--yes`, Histos asks.
""",
      cite[histos.cli.ScaffoldChoice.type](_.from),
    ),
    section("Start from an sbt or zipx build")(
      md"""
If the directory already has an sbt build, `histos init --from sbt` reads `build.sbt` and
drafts `histos.scala` from it. `histos init --from zipx` reads `project/ZipxVersions.scala`
from a [zipx](https://www.earlyeffect.rocks/zipx/) build.
Anything the draft does not know how to map is left as a comment, so you can see what still
needs a decision. The draft is a starting point. Read it before you trust it.
""",
      cite[histos.scaffold.ImportSource.type](_.from),
    ),
    section("What `new` writes")(
      md"""
The generated build is one JVM module, a ZIO dependency, and a main class. The Scala
version and the ZIO version are the ones the scaffold writes on the day you run it. They
are not a second copy kept in this manual. Open `histos.scala` after `new` and you are
looking at the file the command wrote.
""",
      cite[histos.scaffold.Skeleton.type].elided(20),
    ),
  )
end FirstProject
