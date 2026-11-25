package histos.docs

import specular.*

import zio.test.*

/** One help group. The usage line is the `Help.Entry`. The case is the parse. */
object CommandsInfo extends DocSpec:
  def doc = page("Commands: information")(
    md"""
These commands tell you what you have. They do not compile, test, or publish. Start here
when a command surprises you: `histos help` lists every command, and `histos help <command>`
prints one.
""",
    cite[histos.cli.Help.type](_.entries).elided(16),
    section("version")(
      md"""
`histos version` prints the toolchain this command is running. If a background server is
already up, it also prints that server's version, so you can see when the terminal and
the warm process have drifted apart.
"""
    ),
    section("help")(
      md"`histos help` lists the commands in groups. `histos help <command>` prints one command, including its flags."
    ),
    section("graph")(
      md"""
`histos graph` prints every module, the platforms it builds, and the Scala versions. Use
it when you want to see what `histos.scala` became, before you compile.
"""
    ),
    section("observe")(
      md"`histos observe` prints the compile tally Histos has stored: what ran, and what was reused."
    ),
    section("completion")(
      md"""
`histos completion` prints a completion script for zsh or bash. `histos completion install`
writes it under `~/.histos/completions` and prints the line to add to your shell startup
file. Histos does not edit that file for you.
"""
    ),
    section("--explain")(
      md"""
`histos doctor` reports checks by id. `histos --explain <check-id>` prints one check in
full, so you can read why it failed without rerunning the whole doctor.
"""
    ),
    section("--color")(
      md"""
Color follows the terminal unless you say otherwise. `--color` forces it on and
`--no-color` forces it off. `--truecolor` sends 24-bit color. `--no-truecolor` stays in
the 256-color cube. When you pass both of a pair, the last one wins. These flags sit in
front of any other command.
"""
    ),
    section("--verbose")(
      md"""
`--verbose` adds detail to the command after it. On `histos compile` it prints each
compiler phase. On `histos doctor` it lists every check, not only the ones that failed.
"""
    ),
  )

object CommandsBuild extends DocSpec:
  def doc = page("Commands: build")(
    md"""
These are the commands you run while changing code: compile it, run it, format it, pack
it, or throw the outputs away.
""",
    cite[histos.cli.Help.type](_.render),
    section("compile")(
      md"""
`histos compile` compiles every module. Name projects to compile those and their
compilation prerequisites. `--platform jvm,js` or repeated `--platform` flags select
platforms. `--scala 2.13`, `--scala 3`, or a full version such as `--scala 3.8.1`
selects Scala versions the same way. Whole-build selection keeps matching cells;
named modules refuse a platform or a Scala token they do not support, and a series
that matches two versions. `--verbose` prints each compiler phase.
""",
      cite[histos.ScalaSelection]((asked, module: histos.ProjectName) => asked.require(module, _)),
    ),
    section("run")(
      md"""
`histos run <name>` runs a module main. It starts one process: one platform and one
Scala version. `--platform jvm`, `js`, or `native` selects exactly one supported
platform. Without it, the module's `runPlatform` default wins, then its sole
platform. An ambiguous module requires the flag. `--scala` selects one series or one
full version. Omission uses the module's only version, and two declared versions
require the flag. A series that matches two versions, or a comma list, is refused
before anything starts. Positional platform tokens are not accepted. `--main <class>`
chooses among multiple mains.

Everything after `--` is passed to the program, including tokens that look like Histos
flags: `histos run app --platform jvm --main demo.Main -- --port 8080`.
The child inherits the caller's environment and working directory. `inspect`,
`versions`, and `doc` are built-in run names: they describe the graph, print the
catalog, and write scaladoc.

A module's `jvmOptions(JvmOptions(...))` configures child JVM run and test processes,
not the Histos daemon or program arguments. Use `JvmFlag` for VM flags and
`SystemProperty` for properties; provider properties also reach test children.
A second `jvmOptions` replaces the first.

""",
      exampleValue {
        import histos.*

        val options = JvmOptions(
          flags = List(JvmFlag("-Xmx2g")),
          properties = List(SystemProperty("demo.mode=local")),
        )
        val app = module("app", Platform.Jvm)(scala"3.8.1").jvmOptions(options)
        (app.name, options.arguments)
      }.assert(result => assertTrue(result == ("app", List("-Xmx2g", "-Ddemo.mode=local")))),
    ),
    section("clean")(
      md"""
`histos clean` deletes the outputs of the named projects, or of every module. `--full`
also forgets this repository in the content store, so the next compile cannot reuse a
stored result. `--full` does not take a project name. It is the whole repo.
"""
    ),
    section("A production JavaScript file")(
      md"""
A Scala.js link is fast by default: the optimizer stays off, and the script is what
the linker wrote. The production file is the `splice` module, which is sbt-splice
without sbt. `Splice.fast` bundles the linker's own imports. `Splice.full` is that
bundle, minified. `Splice.closure` runs Closure advanced on the Scala.js output and
does not shake mapped libraries. Each one fetches a pinned esbuild when it needs
one. There is no npm and no `package.json`.

`JsOptimize.Full` is `Splice.full` with no mapped libraries: one classic script.
Name libraries when the linker still says `import "preact"`.

The effective optimization is task override, then module `jsOptimize`, then the
locked tool default. An omitted `TaskDep.Link` mode inherits that cascade. Module
run uses the same module/tool choice, so a module declared Full does not silently
run Fast. Optimization and module kind are independent settings. Fast and Full
use distinct link identities and output directories; a cached Fast script cannot
satisfy a Full request. Linking may reuse matching products even when the enclosing
Open task's effects must rerun.
""",
      cite[histos.JsOptimize](_.token),
    ),
    section("fmt")(
      md"""
`histos fmt` formats sources with the scalafmt version the build locked. Name projects,
or pass `--file <path>` for specific files. `--check` reports what would change and
writes nothing, which is what CI runs.
"""
    ),
    section("assembly")(
      md"""
`histos assembly <module>` packs each named assembly module into one file and prints the
path. The module has to be declared as an assembly in the build. The command does not
invent a jar for a library module.
"""
    ),
  )
end CommandsBuild

object CommandsTest extends DocSpec:
  def doc = page("Commands: test")(
    md"""
Tests run the suites on the cells you select. A quick run skips a suite whose inputs
have not changed. A full run does not. Watch reruns a command when you save a file.
""",
    cite[histos.testrun.TestMode](_.token),
    cite[histos.cli.AffectedChoice.type](_.selection),
    section("test")(
      md"""
`histos test` runs the suites. Name projects to limit which modules run. `--quick` skips
a suite whose stamp still matches, which is the run you want while editing. A full
`histos test` runs every selected suite.

`--only` selects suites by pattern. `--test` matches substrings of test labels,
`--tag` keeps tests with any listed exact tag, and `--ignore-tag` drops tests with
any listed tag. A filter does not turn a skipped suite back on under `--quick`.

These filters, `--platform`, and `--scala` accept repeated flags and comma lists,
before or after projects. `--scala` takes a series (`2.13` or `3`) or a full version.
A named module refuses a token that matches nothing and a series that matches two
versions. A whole-build request keeps every matching cell. Omission is every declared
version. When the run covers more than one full version, the tally prints one line
per version before the combined line. Each flag consumes one value. Empty entries
fail. Escape a literal comma as `\\,` and a backslash as `\\\\`, quoting for the shell:
`histos test core --platform jvm --scala 2.13 --only '*Parser*,*Codec*' --tag 'db\\,local'`.

`--affected <json>` intersects the original selection with changed modules. It stays
a JSON list, not a comma list. `--suite <json>` supplies whole-suite CI eligibility;
it cannot combine with named projects and does not turn a singleton into a focused
request. Whole-suite requests exclude Verify modules and unsupported platforms.
Explicitly named unsupported platforms still fail, even if the affected list is empty.
Compilation prerequisites, including upstream Test fixtures, are built as needed,
but their suites do not run unless selected.
""",
      exampleValue {
        histos.TextList.parse(List("db\\,local,remote", "remote"))
      }.assert(result => assertTrue(result == Right(List("db,local", "remote")))),
    ),
    section("coverage")(
      md"""
`histos coverage` instruments Scala on the JVM, runs the suites, and writes a coverage
report. `--scala` selects versions the same way `histos test` does. `--platform` must
be `jvm`; omission stays on the JVM. The `--min-stmt` and `--min-branch` flags set a
floor for the whole run, and the `-file` and `-package` forms set a floor for each
file or each package. The run fails when a floor is missed. `--exclude-class` and
`--exclude-file` take regular expressions for code you do not want measured.
"""
    ),
    section("affected")(
      md"""
`histos affected --base <sha>` prints the modules whose sources differ from that commit.
You use the list to decide what to test, or you pass it to `histos test --affected`.
"""
    ),
    section("One native test binary")(
      md"""
Linking a Native test binary is the slow part of a native suite. When several native
modules should run as one binary, give one module `testAggregate` and name the others.
`histos test nativeTests --platform native` selects its members, compiles their
tests, links once, and runs once. Whole-suite or multi-module requests also use that
aggregate when every member is selected for the same Scala version. A focused
`histos test core --platform native` stays direct, even for a one-member group.
Partial groups and unmatched Scala versions stay direct too. An affected mask does
not pull unselected suites back into a group.

The aggregating module builds Native, it is not published, and it can have no sources
of its own. A member that does not build Native is refused when the build file compiles.

```scala
val nativeTests = module("nativeTests", Native)(scala"3.8.1")
  .clang(ClangMajor(21))
  .testAggregate(core, wire)
```
""",
      cite[histos.Module](_.testAggregates),
    ),
    section("Browser suites")(
      md"""
A suite that opens a browser does not belong in `histos test`. Mark that module
`lane(TestLane.Verify)`. A plain `histos test` skips it. `histos test browsers` runs
it, and a verify job can run that same command.

The JVM child receives the system properties you put on the test provider. Chekhov
reads `chekhov.browsers`. The generated job installs the browsers `RunnerTools`
names. Firefox is the one you get when you name none.

```scala
val browsers = module("browsers", Jvm)(scala"3.8.1")
  .lane(TestLane.Verify)
  .testProvider(
    TestProvider.Zio.properties(SystemProperty("chekhov.browsers=chromium,firefox,webkit"))
  )
```
""",
      cite[histos.SystemProperty.type](_.from),
      cite[histos.Browser](_.token),
    ),
    section("watch")(
      md"""
`histos ~ <command>` runs the command, then runs it again after you save a source. `histos
~ test` is the usual loop. `histos ~ --restart run <name>` restarts the program when the
compile succeeds, instead of leaving the old process up.
"""
    ),
  )

object CommandsProject extends DocSpec:
  def doc = page("Commands: project")(
    md"""
`new` and `init` write a Histos project. The longer explanation, including what the
scaffold puts in the directory, is on the First project page. These are the invocations.
""",
    cite[histos.cli.ScaffoldChoice].signature,
    section("new")(
      md"""
`histos new <name>` creates a directory of that name and writes the project there.
`--bare` writes only the build and the sources. `--yes` accepts the scaffold defaults
without asking. `--branch <name>` sets the CI branch.
"""
    ),
    section("init")(
      md"""
`histos init` writes the project into the current directory. The same `--bare`, `--yes`,
and `--branch` flags apply. `--from sbt` drafts the build from `build.sbt`. `--from zipx`
drafts it from `project/ZipxVersions.scala` in a [zipx](https://www.earlyeffect.rocks/zipx/) build.
"""
    ),
  )

object CommandsPublish extends DocSpec:
  def doc = page("Commands: publish")(
    md"""
Publish turns a module into a coordinate. `publish local` is the one you use while
developing. The release commands are for a ship number, and they refuse to run unless
the session says this is a release. The Publish page explains how a ship differs from
the Histos pin.
""",
    cite[histos.ReleaseMode].signature,
    section("publish local")(
      md"`histos publish local` writes the snapshot coordinate of this commit into the local Ivy repository. It does not upload. Another project on this machine can then depend on it. `histos publish snapshots` publishes that coordinate."
    ),
    section("publish withdraw")(
      md"`histos publish withdraw` removes those local Ivy revisions, and the toolchain directory for this commit. The pin in `build.properties` stays."
    ),
    section("publish snapshots")(
      md"`histos publish snapshots` publishes the snapshot coordinate of this commit to the snapshot repository."
    ),
    section("publish release")(
      md"""
`histos publish release` publishes the number on the ship's `.next`. It runs only when
`HISTOS_SESSION=release`. `--automatic` publishes and waits until the portal reports the
deployment published.
"""
    ),
    section("publish assets")(
      md"`histos publish assets` uploads a packaged file once the deployment is validated and the GitHub release exists."
    ),
    section("publish promote")(
      md"`histos publish promote <deployment>` promotes a deployment that was held for a check."
    ),
    section("publish drop")(md"`histos publish drop <deployment>` drops a held deployment instead of promoting it."),
    section("bump")(
      md"""
`histos bump` moves `.next` forward once a published stable release has caught the number
that is written there. `--major`, `--minor`, and `--patch` choose which part moves. It
does not change `histos.version`.
"""
    ),
    section("portal")(
      md"`histos portal` rehearses the Maven Central portal on loopback, so you can see the upload conversation without publishing."
    ),
  )

object CommandsDependencies extends DocSpec:
  def doc = page("Commands: dependencies")(
    md"""
A hermetic build reads `dependencies.lock` and does not resolve. A solved build resolves
from `histos.scala` and remembers the result. The Resolution page is that choice.
`resolve` writes or checks the lock. `deps` looks at the jars the build is actually using.
""",
    cite[histos.cli.Help.type](_.flags),
    section("resolve")(
      md"""
`histos resolve` checks the lock against a fresh solve. `--update` solves and writes
`dependencies.lock`. `--check` fails when a fresh solve would differ, which is the CI
form. `--refresh` refetches metadata. `--lock=offer`, `update`, `local`, or `frozen`
chooses how far the command is allowed to move the lock.
"""
    ),
    section("deps")(
      md"""
`histos deps check` reports available updates. `deps update` applies them. `--yes` does
not ask, and `--security` limits the set to security updates. `deps audit` reports known
problems, and `--report` writes the report. `deps pr` opens an update as a branch.
`--open` opens the pull request. `deps alert` prints the alerts. `deps snapshot` works
with snapshot dependencies, and `--submit` sends the result.
"""
    ),
  )

object CommandsCi extends DocSpec:
  def doc = page("Commands: CI")(
    md"""
These commands prepare the repository for GitHub Actions and check that the tree matches
what the build would generate.
""",
    cite[histos.cli.DoctorScope.type](_.token),
    section("workflow")(
      md"""
`histos workflow --check` fails when the generated workflows in the tree are stale.
`histos workflow --update` writes them. Commit the result. Neither form publishes, and
neither changes the Histos pin. The CI page says what a generated workflow contains.
"""
    ),
    section("doctor")(
      md"""
`histos doctor` runs the repository checks and prints what failed. Name projects to limit
the run. `--area <area>` runs one group of checks. `--json` prints JSON for a script.
`--preflight <job>` runs the checks that job needs. `--verbose` lists every check.
`--fix` applies the edits Histos knows how to make, and `--yes` applies them without
asking. Job preflight checks configuration and other product-independent prerequisites;
it is not a substitute for the JVM compatibility proof that `publish check`, `publish local`,
`publish snapshots`, and `publish release` run, explained on the Publish page.
"""
    ),
    section("setup")(
      md"""
`histos setup supply-chain` writes the files the supply-chain checks expect. `--from zipx`
starts from a [zipx](https://www.earlyeffect.rocks/zipx/) layout. `--fix` rewrites what it can, and `--yes` does not ask.
"""
    ),
  )

object CommandsCache extends DocSpec:
  def doc = page("Commands: cache")(
    md"""
The cache is the content store described on the Cache page. These commands look at it,
clean it, and check that a stored result is still true.
""",
    cite[histos.cache.ContentStore](_.clean),
    section("path")(md"`histos cache path` prints the directory of the local content store."),
    section("list")(md"`histos cache list` prints each stored digest and its size."),
    section("clean")(
      md"""
`histos cache clean` removes entries older than the retention that no project still
needs. `--dry-run` prints the names and deletes nothing.
"""
    ),
    section("promote")(
      md"""
`histos cache promote --from <namespace> --to <namespace>` copies an HTTP index from one
remote namespace to another. This is how a pull request namespace becomes `main` after
it merges.
"""
    ),
    section("forget")(
      md"""
`histos cache forget <namespace>` drops a remote namespace. `histos cache forget <module>`
drops that module's local compile hit, so the next compile of it cannot reuse the stored
result.
"""
    ),
    section("verify")(
      md"""
`histos cache verify` recompiles stored hits in a scratch checkout and names any module
whose classes differ from what was stored. Name modules to limit the check. A difference
here means the cache would have skipped a compile it should have run.
"""
    ),
  )

object CommandsToolchain extends DocSpec:
  def doc = page("Commands: toolchain")(
    md"""
The toolchain is the Histos you run: the script on your `PATH`, the jars under
`~/.histos/toolchains`, and the optional native client. The Install page walks through
getting the script onto the machine. These commands manage what is already there.
""",
    cite[histos.publish.LauncherPin.type](_.update),
    section("toolchain install")(
      md"""
`histos toolchain install <version>` downloads one toolchain. `--client` also links the
native client. `--no-client` skips it. Passing both fails. If you have no default yet,
this command records the version you installed as the default.
"""
    ),
    section("toolchain client")(
      md"""
`histos toolchain client` links `histos-client` beside the jar you are running. CI
refuses the command with no flags, because CI is expected to use `java -jar` unless the
job opted in with `toolchain install --client`.
"""
    ),
    section("toolchain list")(md"`histos toolchain list` prints the versions installed under `~/.histos/toolchains`."),
    section("update")(
      md"""
`histos update` moves you to a newer Histos. Inside a project it rewrites
`build.properties` and then runs `version` on that pin, so you see the toolchain you
just selected. `--make-default` also writes `~/.histos/default`. A release pin moves to
the newest release. A pin that ends in `-SNAPSHOT`, including a commit coordinate,
defaults to `--follow`, which writes the newest line pointer. `--freeze` writes the
timestamp that pointer's channel file names now. A timestamp pin defaults to `--freeze`.
`--follow` and `--freeze` are opposites.
"""
    ),
    section("self update")(
      md"`histos self update` replaces the installed script when the published sha256 differs. The toolchains stay."
    ),
  )

object CommandsServer extends DocSpec:
  def doc = page("Commands: server")(
    md"""
Histos keeps a JVM process warm so the next command does not pay to start one. Metals and
an MCP client talk to that process. These commands start it, stop it, and ask it to load
the build again.
""",
    cite[histos.bsp.Bsp.type](_.ServerName),
    section("server")(
      md"`histos server` runs the warm process in the foreground. You use it when you want to watch that process. Ordinary commands start it for you."
    ),
    section("bsp")(
      md"""
`histos bsp` speaks the build server protocol on standard input. Metals starts it from
`.bsp/histos.json`. You rarely type it yourself. The Metals page says what an import gets
you.
"""
    ),
    section("mcp")(
      md"`histos mcp` speaks MCP on standard input and output. An agent runs it. The MCP page says how to point a client at it."
    ),
    section("reload")(
      md"`histos reload` asks the running server to read `histos.scala` again. Use it after you change the build file and the server is still on the old graph."
    ),
    section("reboot")(
      md"`histos reboot` stops the server and starts one on this command's toolchain. If no server is running, it starts one."
    ),
    section("stop")(md"`histos stop` asks the running server to exit. It does not start another."),
    section("cancel")(
      md"`histos cancel <id>` cancels a run. The id is the one the command printed when it started."
    ),
  )

object CommandsPreview extends DocSpec:
  def doc = page("Commands: preview")(
    md"""
A preview answers "what would this command do?" and then does not do it. Use it before a
clean, a publish, or a test run when you want the plan without the side effect.
""",
    cite[histos.cli.Help.type](_.grouped),
    section("compile and test")(
      md"""
`histos preview compile` names the sources the last analysis says are out of date.
`histos preview test` names the suites a quick run would skip, and the ones it would run.
"""
    ),
    section("publish and clean")(
      md"""
`histos preview publish local` and `histos preview publish snapshots` describe the
coordinate they would write. `histos preview clean` names the outputs it would delete.
None of them write.
"""
    ),
    section("run")(md"`histos preview run <name>` names the program it would start, and does not start it."),
  )
