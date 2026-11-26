package histos.docs

import mermoid.ascent.MermoidAscent
import specular.*

import zio.ZIO
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
is Scala Native. On the command line the tokens are `jvm`, `js`, and `native`.
`histos run <module> --platform js` selects that module's Scala.js cell, if it
supports Scala.js. Positional platform tokens are not run selectors.

`--scala 2.13` and `--scala 3` select a series. `--scala 2.13.18` selects that full
version. Omission keeps every declared version of the modules you asked for. A named
module refuses a token that matches nothing and a series that matches two of its
versions. `histos run` starts one process, so it needs one Scala version and one
platform.
""",
      cite[histos.Platform](_.token),
      cite[histos.ScalaSelection]((asked, module: histos.ProjectName) => asked.require(module, _)),
    ),
    section("Versions and dependencies are checked when the build file compiles")(
      md"""
You write a Scala version as `scala"x.y.z"` and a dependency as `dep"org::name:revision"`.
Those are macros. A version that is not a full Scala version, or a dependency that is not
a real coordinate, fails compilation of `histos.scala`. Histos never gets as far as
resolving it. Bind the ship once. `Ship("name").parent(Jvm)(scala"3.8.1")` is the
member named like the ship. `ship.next(major, minor, patch)(the modules)` is the
catalog row. The number on `next` is the next release. You do not write `SNAPSHOT`
in the catalog.
""",
      cite[histos.Dep.type](_.from),
    ),
    section("Sources live in more than one directory")(
      md"""
A module compiles the sources under its own directory. That is `<name>/src`, or the
directory you named with `in`.

When the same tree belongs in more than one module, add it with `sources`. Each extra
directory uses the layout you already know: `scala`, `java`, and a filter such as
`jvm`, `js`, `native`, or `scala-2.13`. A file under `jvm` is compiled for the JVM and
left out of Scala.js. The module directory stays the first root. Naming a directory
twice does not compile it twice.

```scala
val shared = path"shared/src"
val core   = module("core", Jvm, Js)(scala"3.8.1").sources(shared)
```
""",
      cite[histos.Module](_.sources),
    ),
    section("Main, Test, and exact roots")(
      md"""
A cell has two compiler stages: `SourceSet.Main` and `SourceSet.Test`. Test includes
its own Main classes. Conventional `<module>/src` and `<module>/test` trees remain
available. Use `roots` for directories that do not follow that layout: each
`ScopedRoot` names an exact directory, its scope, platforms, and whether it contains
sources or resources. Histos does not expand a scoped root into layout subdirectories.
`ScopedRoot.jvmNative` selects the combined JVM/Native tree, not Scala.js.

`dependsOn(core)` supplies Main products. `testDependsOn(core)` additionally supplies
core's Test classes and resources to the depender's Test stage. Use it for shared
fixtures, not to request core's suites. Only selected modules' suites run. Test-only
project edges and `testDependencies` do not enter Main classpaths or published POMs.
Platform-specific fixture edges use `testDependsOn(Jvm)(core)`.
""",
      cite[histos.CompilationStage].signature,
      cite[histos.ScopedRoot].signature,
      exampleZIO {
        import histos.*
        import histos.Platform.*
        import histos.plugin.{RepoPath, SourceSet}

        val fixtures = module("fixtures", Jvm)(scala"3.8.1")
        val core     = module("core", Jvm, Js)(scala"3.8.1")
          .roots(
            ScopedRoot(RepoPath("shared/main"), SourceSet.Main, PlatformSelection.only(Jvm), RootKind.Sources),
            ScopedRoot(RepoPath("shared/test-data"), SourceSet.Test, PlatformSelection.All, RootKind.Resources),
          )
          .testDependsOn(Jvm)(fixtures)
        val graph = ZIO.fromEither(Projects(fixtures, core))
        graph
      }.assert { graph =>
        import histos.*
        import histos.Platform.*
        import histos.plugin.{RepoPath, SourceSet}

        val core = graph.get(ProjectName("core")).collect { case module: Module => module }
        assertTrue(
          core.exists(
            _.roots.exists(root =>
              root.path == RepoPath("shared/main") && root.kind == RootKind.Sources &&
                root.applies(SourceSet.Main, Jvm) && !root.applies(SourceSet.Main, Js) &&
                !root.applies(SourceSet.Test, Jvm)
            )
          ),
          core.exists(
            _.roots.exists(root =>
              root.path == RepoPath("shared/test-data") && root.kind == RootKind.Resources &&
                root.applies(SourceSet.Test, Jvm) && root.applies(SourceSet.Test, Js) &&
                !root.applies(SourceSet.Main, Jvm)
            )
          ),
          core.exists(module =>
            module.upstream(Jvm, SourceSet.Test) == List(ProjectName("fixtures")) &&
              module.upstream(Js, SourceSet.Test).isEmpty && module.upstream(Jvm, SourceSet.Main).isEmpty
          ),
        )
      },
    ),
    section("Native libraries")(
      md"""
A Native module links the libraries you name. `link` takes `Ssl`, `Crypto`, `Gc`,
`Unwind`, or `Stdcxx`. You do not write a linker flag. On macOS, `Ssl` and `Crypto`
use Homebrew OpenSSL when `/opt/homebrew/opt/openssl@3` is installed. On Linux they
are the system libraries, and the generated native job installs them.

A second `link` replaces the set. A module that builds no Native code cannot call it.

```scala
val core = module("core", Jvm, Native)(scala"3.8.1")
  .clang(ClangMajor(21))
  .link(NativeLibrary.Ssl, NativeLibrary.Crypto)
```
""",
      cite[histos.NativeLibrary](_.token),
    ),
    section("How many steps run at once")(
      md"""
Compile, publish, and forked programs each have their own pool. A pool has one permit
per core, so a compile does not wait on a publish, and a small machine does not start
more of one class than it has cores.

`Projects.parallelism` sets a ceiling on every pool. `Projects.parallelism(2)` runs at
most two of each, and one of each on a machine with a single core. Leaving it out keeps
the core count. A second call replaces the ceiling. Zero and a negative number fail
when the graph is built.

```scala
Projects.parallelism(2)
```
""",
      cite[histos.Projects.type](_.parallelism),
    ),
    section("What this repository builds")(
      md"""
This repository is itself a Histos build. `object build extends HistosBuild` in
`histos.scala` returns the graph. The ship lists the modules that are published, and it
names two roles: `histos`, declared with `ship.parent`, is the launcher people run,
with its sources in `app/`, and `client` is the optional native client. Reading that
file is the example. The manual
does not keep a second copy of the
version numbers.
""",
      cite[histos.ShipDef].signature.elided(16),
    ),
  )
end BuildModel
