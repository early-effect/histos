package histos.docs

import mermoid.ascent.MermoidAscent
import specular.*

import zio.test.*

/** Why Histos, next to sbt, Mill, and Bazel. The drive is the one in AGENTS.md. */
object Lineage extends DocSpec:

  def doc = page("Why Histos")(
    md"""
Compile, package, and link are the expensive parts of a Scala build. They should run when
the code they depend on changed, and they should be skipped when it did not. A skip that
hides a real change is a failure, even when the command returned quickly. A result you
cannot trust is the same kind of failure.

That is what Histos is for. You write the build as a Scala program. Evaluating it produces
one immutable graph. Compile, test, publish, and the server are effects over that graph.
You choose whether a cached result lives on this machine or in a remote store, and you
choose how that result is pinned. A quick test run covers what the change touches. The
full run is the one you ask for.

[zipx](https://www.earlyeffect.rocks/zipx/) made the skip tolerable on sbt, and it generated the workflows a repository actually
had. That was the lesson. The goal is a build tool that starts there: functional, the
cache under your control, and the same graph for you, for your editor, and for an agent.
""",
    illustration {
      MermoidAscent.diagram(Figures.lineage)
    }.assert { ui =>
      val text = ui.toString
      assertTrue(text.contains("ProjectGraph"), text.contains("content store"))
    },
    section("sbt")(
      md"""
sbt builds a task graph out of settings. Keys live in scopes. A plugin contributes more
settings. The work you get is whatever that composition evaluates to. [zipx](https://www.earlyeffect.rocks/zipx/) is the plugin
that asked this to be stricter: do not repeat compile, package, and link when their
inputs are unchanged, and let the build describe the CI instead of a workflow someone
maintains by hand.

Histos is that request as the tool. `histos.scala` is the program. `graph` returns the
`ProjectGraph`: the modules, the Scala versions, and the platforms. A bad version or a
bad dependency fails compilation of that file, before a command runs. The skip is a
digest of the bytes, not a guess about what a setting might have invalidated. The
workflow file is a render of the same graph, including a job a plugin added. A quick
test runs the suites the change affects. You run the full suite when you want every one.
""",
      cite[histos.HistosBuild](_.graph),
      cite[histos.ScalaVersion.type](_.from),
      cite[histos.plugin.CiJob](_.render),
    ),
    section("Mill")(
      md"""
Mill is also a Scala program, and the idea it got right is that a module runs its own
task. Histos keeps that. A module's classpath is its own. One module expands to a cell
per Scala version and per platform, and that cell is what compile, test, and publish
run on. Tasks do not share a classpath to look simpler.

What Histos adds is one graph with one compile, used by everyone who asks. Metals does
not export a second build and compile it with a different analysis and a different
classpath. It speaks BSP to this server, and the analysis, the lock, and the options
are the ones `histos compile` uses. An agent speaks MCP and calls the same operations.
The cache is the other half of the purpose: local or remote, pinned the way you name,
a laptop reading, CI writing. `histos cache verify` recompiles a stored hit and names
a module whose classes differ, because a cached result that is wrong is a bug.
""",
      cite[histos.bsp.Bsp.type](_.ServerName),
      cite[histos.mcp.HistosMcp.type](_.toolNames),
      cite[histos.schedule.CacheConfig.type](_.open),
    ),
    section("Bazel")(
      md"""
Bazel names an action by its inputs and can share that result across machines. Histos
wants that trust for the Scala work that dominates the build. The remote store speaks
the Bazel HTTP cache and the Bazel gRPC cache, so the bytes can live in a cache you
already run.

The program you write stays a Scala description of modules and cells. You choose local
or remote. You choose the namespace: `main`, a pull request, or a person. A lookup
tries that namespace, then `main`, then the machine. A laptop that asks to write is
opened for read. CI is who stores. A bad cache URI fails the command rather than
quietly turning the cache off.
""",
      cite[histos.cache.ContentStore](_.read),
      cite[histos.schedule.CacheConfig.type](_.decide),
    ),
    section("Bringing a repository across")(
      md"""
`histos init --from sbt` reads `build.sbt` and drafts `histos.scala`. `histos init
--from zipx` reads `project/ZipxVersions.scala` from a [zipx](https://www.earlyeffect.rocks/zipx/)
build. A setting the draft does not map stays
as a comment, for you to decide. The draft is yours after that.
""",
      cite[histos.scaffold.ImportSource.type](_.from),
    ),
  )
end Lineage
