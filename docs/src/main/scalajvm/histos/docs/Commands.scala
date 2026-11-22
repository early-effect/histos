package histos.docs

import specular.*

/** One help group. The usage line is the `Help.Entry`. The case is the parse. */
object CommandsInfo extends DocSpec:
  def doc = page("Commands: information")(
    md"""
`histos help` lists the commands. `histos help <command>` prints one. The entries below are
the information group.
""",
    cite[histos.cli.Help.type](_.entries).elided(16),
    section("version")(
      md"`histos version` prints the toolchain this command runs and, with a server up, that server's version."
    ),
    section("help")(md"`histos help [command]` lists commands, or shows one command."),
    section("graph")(md"`histos graph` prints each module, platform, and Scala version."),
    section("observe")(md"`histos observe` prints the stored compile tally."),
    section("completion")(
      md"`histos completion [install] [zsh|bash]` prints the completion script for this shell. `install` writes it under `~/.histos/completions` and prints the rc line."
    ),
    section("--explain")(md"`histos --explain <check-id>` prints one check."),
    section("--color")(
      md"`--color` forces color on. `--no-color` forces it off. `--truecolor` sends 24-bit color. `--no-truecolor` stops at the 256-color cube. The last flag wins."
    ),
    section("--verbose")(
      md"`--verbose` prints each compiler phase on a compile, and lists every check on `histos doctor`."
    ),
  )

object CommandsBuild extends DocSpec:
  def doc = page("Commands: build")(
    md"""
Compile, run, clean, format, and assembly.
""",
    cite[histos.cli.Help.type](_.render),
    section("compile")(
      md"`histos compile [project ...]` compiles the named projects, or every module. `--verbose` prints each compiler phase."
    ),
    section("run")(
      md"`histos run <name> [jvm|js|native] [-- arg ...]` runs a module main. Tokens after `--` are the program's arguments. `inspect`, `versions`, and `doc` are the other names."
    ),
    section("clean")(
      md"`histos clean [project ...] [--full]` deletes outputs. `--full` also forgets this repo in the content store and takes no project."
    ),
    section("fmt")(
      md"`histos fmt [--check] [project ... | --file <path> ...]` formats with the locked scalafmt the build declares. `--check` writes no source."
    ),
    section("assembly")(
      md"`histos assembly <module> ...` writes each assembly module to its asset path and prints the path."
    ),
  )

object CommandsTest extends DocSpec:
  def doc = page("Commands: test")(
    md"""
`test`, `coverage`, `affected`, and watch.
""",
    cite[histos.testrun.TestMode](_.token),
    cite[histos.cli.AffectedChoice.type](_.select),
    section("test")(
      md"""
`histos test [project ...] [--affected <json>] [--quick] [--only <pattern>] [--test <term>] [--tag <name>] [--ignore-tag <name>]` runs the selected suites. `--quick` skips a suite whose stamp still matches. A filter does not change that skip.
"""
    ),
    section("coverage")(
      md"""
`histos coverage` instruments Scala on the JVM, runs the suites, and writes the report. Floors are `--min-stmt`, `--min-branch`, `--min-stmt-file`, `--min-branch-file`, `--min-stmt-package`, and `--min-branch-package`. `--exclude-class` and `--exclude-file` take regexes.
"""
    ),
    section("affected")(md"`histos affected --base <sha>` prints the modules that differ from that commit."),
    section("watch")(
      md"`histos ~ <command>` runs a command again after a source save. `histos ~ --restart run <name>` restarts the program when the compile succeeds."
    ),
  )

object CommandsProject extends DocSpec:
  def doc = page("Commands: project")(
    md"""
`new` and `init`. The flags are `--bare`, `--yes`, `--branch`, and, on `init`, `--from`.
""",
    cite[histos.cli.ScaffoldChoice].signature,
    section("new")(md"`histos new [--bare] [--yes] [--branch <name>] <name>` writes a project in a new directory."),
    section("init")(
      md"`histos init [--bare] [--yes] [--from sbt|zipx] [--branch <name>]` writes a project in the current directory."
    ),
  )

object CommandsPublish extends DocSpec:
  def doc = page("Commands: publish")(
    md"""
Local, snapshots, release, assets, promote, drop, withdraw, and bump.
""",
    cite[histos.ReleaseMode].signature,
    section("publish local")(md"`histos publish local [token ...]` publishes to the local Ivy repository."),
    section("publish withdraw")(
      md"`histos publish withdraw` removes the local ivy revisions and toolchain directory for HEAD. The pin stays."
    ),
    section("publish snapshots")(
      md"`histos publish snapshots [token ...]` publishes the snapshot coordinate at HEAD."
    ),
    section("publish release")(
      md"`histos publish release [token ...] [--automatic]` publishes the ship number. It requires `HISTOS_SESSION=release`."
    ),
    section("publish assets")(
      md"`histos publish assets [token ...]` uploads a packaged file when the deployment is validated and the GitHub release exists."
    ),
    section("publish promote")(md"`histos publish promote <deployment>` promotes a held deployment."),
    section("publish drop")(md"`histos publish drop <deployment>` drops a held deployment."),
    section("bump")(
      md"`histos bump [--major | --minor | --patch] [ship ...]` moves `.next` when a published stable has caught it. It does not change `histos.version`."
    ),
    section("portal")(md"`histos portal` rehearses the Central portal on loopback."),
  )

object CommandsDependencies extends DocSpec:
  def doc = page("Commands: dependencies")(
    md"""
`resolve` and `deps`.
""",
    cite[histos.cli.Help.type](_.flags),
    section("resolve")(
      md"""
`histos resolve [--update | --check | --refresh] [--lock=offer|update|local|frozen]` checks the lock, or writes it. `--update` solves and writes `dependencies.lock`. `--check` fails when a fresh solve differs.
"""
    ),
    section("deps")(
      md"""
`histos deps check`, `deps update [--yes] [--security]`, `deps audit [--report]`, `deps pr [--security] [--open]`, `deps alert`, and `deps snapshot [--submit]`.
"""
    ),
  )

object CommandsCi extends DocSpec:
  def doc = page("Commands: CI")(
    md"""
`workflow`, `doctor`, and `setup`.
""",
    cite[histos.cli.DoctorScope.type](_.token),
    section("workflow")(
      md"`histos workflow [--check | --update]` checks the generated workflows, or writes them. Neither publishes or changes the pin."
    ),
    section("doctor")(
      md"""
`histos doctor [project ...] [--area <area>] [--json] [--preflight <job>] [--verbose] [--fix] [--yes]` runs the repository checks. `--json` prints JSON. `--fix` applies local edits.
"""
    ),
    section("setup")(
      md"`histos setup supply-chain [--from zipx] [--fix] [--yes]` writes the supply-chain files."
    ),
  )

object CommandsCache extends DocSpec:
  def doc = page("Commands: cache")(
    md"""
`cache path`, `cache list`, `cache clean`, `cache promote`, `cache forget`, and `cache verify`.
""",
    cite[histos.cache.ContentStore](_.clean),
    section("path")(md"`histos cache path` prints the content store."),
    section("list")(md"`histos cache list` prints each digest and its size."),
    section("clean")(
      md"`histos cache clean` removes entries older than the retention that no project keeps. `--dry-run` names them and keeps them."
    ),
    section("promote")(md"`histos cache promote --from <namespace> --to <namespace>` copies an HTTP index."),
    section("forget")(
      md"`histos cache forget <namespace|module>` drops a remote namespace or one module's local compile hit."
    ),
    section("verify")(
      md"`histos cache verify [module ...]` recompiles stored hits in a scratch checkout and names a module whose classes differ."
    ),
  )

object CommandsToolchain extends DocSpec:
  def doc = page("Commands: toolchain")(
    md"""
Install, list, the native client, update, and the shim.
""",
    cite[histos.publish.LauncherPin.type](_.update),
    section("toolchain install")(
      md"`histos toolchain install <version>` downloads one toolchain. `--client` and `--no-client` choose the native client. Passing both fails."
    ),
    section("toolchain client")(
      md"`histos toolchain client` links `histos-client` beside the installed jar. CI refuses the bare command."
    ),
    section("toolchain list")(md"`histos toolchain list` prints the installed versions."),
    section("update")(
      md"`histos update` moves the pin. Inside a project it rewrites `build.properties` and runs `version` on that pin. `--make-default` writes the default as well. `--freeze` and `--follow` choose the pin's form."
    ),
    section("self update")(md"`histos self update` replaces the shim when the published hash differs."),
  )

object CommandsServer extends DocSpec:
  def doc = page("Commands: server")(
    md"""
The warm JVM, BSP, MCP, and the commands that talk to a running server.
""",
    cite[histos.bsp.Bsp.type](_.ServerName),
    section("server")(md"`histos server` keeps a warm process in the foreground."),
    section("bsp")(md"`histos bsp` speaks the build server protocol. Metals starts it from `.bsp/histos.json`."),
    section("mcp")(md"`histos mcp` speaks MCP on stdio."),
    section("reload")(md"`histos reload` asks the running server to load the build again."),
    section("reboot")(md"`histos reboot` stops the server and starts one on this command's toolchain, or starts one."),
    section("stop")(md"`histos stop` asks the running server to exit. It does not start another."),
    section("cancel")(md"`histos cancel <id>` cancels the run id a command printed."),
  )

object CommandsPreview extends DocSpec:
  def doc = page("Commands: preview")(
    md"""
Preview writes nothing. It names what the command would do.
""",
    cite[histos.cli.Help.type](_.grouped),
    section("compile and test")(
      md"`histos preview compile` and `histos preview test` name the sources the last analysis says are stale. A test preview names the suites a quick run would skip."
    ),
    section("publish and clean")(
      md"`histos preview publish local`, `histos preview publish snapshots`, and `histos preview clean` describe the write and do not perform it."
    ),
    section("run")(md"`histos preview run <name>` names the program and does not start it."),
  )
