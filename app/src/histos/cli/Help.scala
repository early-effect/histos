package histos.cli

import histos.doctor.Suggest

/** Why `histos help` could not render a topic. */
enum HelpError:
  case Unknown(token: String)

  def message: String = this match
    case Unknown(token) => Help.unknown(token, Help.names)
end HelpError

/** Grouped help, and one command. Shim commands are listed even though the installed script runs them. */
object Help:
  final case class Entry(name: String, group: String, usage: String, about: String)

  val entries: List[Entry] = List(
    Entry(
      "version",
      "info",
      "histos version",
      "Print the toolchain this command runs and, with a server up, that server's version.",
    ),
    Entry("help", "info", "histos help [command]", "List commands, or show one command."),
    Entry("--explain", "info", "histos --explain <check-id>", "Print one check."),
    Entry("graph", "info", "histos graph", "Print each module, platform, and Scala version."),
    Entry("observe", "info", "histos observe", "Print the stored compile tally."),
    Entry("compile", "build", "histos compile [project ...]", "Compile the named projects, or every module."),
    Entry("run", "build", "histos run <name> [jvm|js|native]", "Run a module main, inspect, versions, or doc."),
    Entry(
      "clean",
      "build",
      "histos clean [project ...] [--full]",
      "Delete the named projects' outputs. --full also forgets this repo in the content store and takes no project.",
    ),
    Entry(
      "assembly",
      "build",
      "histos assembly <module> ...",
      "Write each assembly module to its asset path, or to a build output path, and print the path.",
    ),
    Entry(
      "fmt",
      "build",
      "histos fmt [--check] [project ... | --file <path> ...]",
      "Format with the locked scalafmt the build declares. No project is every module and histos.scala. --check writes no source and rewrites .histos/format/ledger.",
    ),
    Entry(
      "test",
      "test",
      "histos test [project ...] [--affected <json>] [--quick] [--only <pattern> ...]",
      "Run the selected suites. --quick skips a suite whose stamp still matches. --only runs suites that match.",
    ),
    Entry(
      "coverage",
      "test",
      "histos coverage [--min-stmt <n>] [--min-branch <n>] [--min-stmt-file <n>] [--min-branch-file <n>] [--min-stmt-package <n>] [--min-branch-package <n>] [--exclude-class <regex>] [--exclude-file <regex>] [project ...]",
      "Instrument Scala 3 and Scala 2.13 on the JVM, run the suites, and write the report.",
    ),
    Entry("affected", "test", "histos affected --base <sha>", "Print modules that differ from the base."),
    Entry("~", "test", "histos ~ <command>", "Re-run a command when a source file is saved."),
    Entry(
      "new",
      "project",
      "histos new [--bare] [--yes] [--branch <name>] <name>",
      "Write a new project in a new directory. --bare writes the build file and the sources only.",
    ),
    Entry(
      "init",
      "project",
      "histos init [--bare] [--yes] [--from sbt|zipx] [--branch <name>]",
      "Write a project in the current directory. --from drafts histos.scala from sbt or zipx.",
    ),
    Entry(
      "publish local",
      "publish",
      "histos publish local [token ...]",
      "Publish to the local Ivy repository. The three publish commands link the client.",
    ),
    Entry(
      "publish",
      "publish",
      "histos publish snapshots [token ...]",
      "Publish the snapshot coordinate at HEAD. A snapshot or pull-request session already set is kept. The pointer moves from any checkout.",
    ),
    Entry(
      "publish release",
      "publish",
      "histos publish release [token ...] [--automatic]",
      "Publish the ship number. Requires HISTOS_SESSION=release. --automatic closes the portal deployment.",
    ),
    Entry(
      "publish assets",
      "publish",
      "histos publish assets [token ...]",
      "Upload a packaged file when the deployment is validated and the GitHub release exists.",
    ),
    Entry("publish promote", "publish", "histos publish promote <deployment>", "Promote a held deployment."),
    Entry("publish drop", "publish", "histos publish drop <deployment>", "Drop a held deployment."),
    Entry(
      "bump",
      "publish",
      "histos bump [--major | --minor | --patch] [ship ...]",
      "Move .next past the newest stable coursier.Versions lists. The default is the next patch. A maven repository wins over ivy2 local. Does not change histos.version.",
    ),
    Entry("portal", "publish", "histos portal", "Rehearse the Central portal on loopback."),
    Entry("resolve", "dependencies", "histos resolve [--update | --check]", "Check the lock, or write it."),
    Entry("deps", "dependencies", "histos deps check", "Check, update, audit, or snapshot dependencies."),
    Entry("workflow", "ci", "histos workflow [--check | --update]", "Check generated workflows, or write them."),
    Entry(
      "doctor",
      "ci",
      "histos doctor [project ...] [--area <area>] [--json] [--preflight <job>] [--verbose] [--fix] [--yes]",
      "Run the repository checks. The text lists errors and warnings. --verbose lists every check. Named projects are the modules that launch. With no --json and no --preflight, the whole doctor and --area toolchain also print the JDK, Node, and clang lines.",
    ),
    Entry("setup", "ci", "histos setup supply-chain [--from zipx] [--fix] [--yes]", "Write the supply-chain files."),
    Entry("cache", "cache", "histos cache path", "Print, list, clean, promote, forget, or verify the cache."),
    Entry(
      "toolchain install",
      "toolchain",
      "histos toolchain install <version>",
      "Download one toolchain. The installed shim runs this.",
    ),
    Entry(
      "toolchain list",
      "toolchain",
      "histos toolchain list",
      "Print installed toolchains. The installed shim runs this.",
    ),
    Entry(
      "update",
      "toolchain",
      "histos update",
      "Move the selected Histos version. The installed shim runs this. Inside a project it then runs version, which rewrites the BSP connection and reboots a server on another driver.",
    ),
    Entry("self", "toolchain", "histos self update", "Replace the shim when the published hash differs."),
    Entry("bsp", "server", "histos bsp", "Speak the build server protocol."),
    Entry("mcp", "server", "histos mcp", "Speak the MCP server."),
    Entry("reload", "server", "histos reload", "Reload the build file."),
    Entry(
      "reboot",
      "server",
      "histos reboot",
      "Replace the running server, or start one, and return when it is listening.",
    ),
    Entry("cancel", "server", "histos cancel <id>", "Cancel one run."),
    Entry("preview", "preview", "histos preview compile [project ...]", "Show the plan. Nothing is compiled."),
  )

  /** Tokens a typo is compared with. Shim commands are included. */
  val names: List[String] = List(
    "version",
    "help",
    "--explain",
    "graph",
    "observe",
    "compile",
    "run",
    "clean",
    "assembly",
    "fmt",
    "test",
    "coverage",
    "affected",
    "new",
    "init",
    "publish",
    "bump",
    "portal",
    "resolve",
    "deps",
    "workflow",
    "doctor",
    "setup",
    "cache",
    "toolchain",
    "update",
    "self",
    "bsp",
    "mcp",
    "reload",
    "reboot",
    "cancel",
    "preview",
  )

  val flags: List[String] = List(
    "affected",
    "area",
    "automatic",
    "bare",
    "base",
    "branch",
    "check",
    "dry-run",
    "fix",
    "from",
    "explain",
    "full",
    "json",
    "major",
    "make-default",
    "minor",
    "only",
    "open",
    "patch",
    "preflight",
    "report",
    "quick",
    "security",
    "submit",
    "to",
    "update",
    "verbose",
    "yes",
  )

  private val groups: List[String] = List(
    "info",
    "build",
    "test",
    "project",
    "publish",
    "dependencies",
    "ci",
    "cache",
    "toolchain",
    "server",
    "preview",
  )

  def grouped: String =
    val blocks = groups.map: group =>
      val lines = entries.filter(_.group == group).map(entry => s"  ${entry.usage}")
      (group :: lines).mkString("\n")
    ("histos help" :: "histos help <command>" :: "" :: blocks).mkString("\n")

  def render(words: List[String]): Either[HelpError, String] =
    val key = words.map(_.trim).filter(_.nonEmpty).mkString(" ")
    if key.isEmpty then Right(grouped)
    else
      family(key) match
        case Some(text) => Right(text)
        case None       => Left(HelpError.Unknown(key))

  def unknown(token: String, known: List[String]): String =
    val typed = if token.isEmpty then "command" else token
    val near  = if token.isEmpty then Nil else Suggest.near(token, known)
    val guess = near match
      case Nil        => s"unknown command '$typed'"
      case one :: Nil => s"unknown command '$typed'\ndid you mean $one?"
      case many       => s"unknown command '$typed'\ndid you mean ${many.mkString(" or ")}?"
    val pointer = near.headOption match
      case Some(name) => s"histos help\nhistos help $name"
      case None       => "histos help\nhistos help <command>"
    s"$guess\n$pointer"

  def flag(command: String, raw: String): String =
    val name = raw.dropWhile(_ == '-')
    Suggest.closest(name, flags) match
      case Some(near) => s"$command does not take $raw\ndid you mean --$near?"
      case None       => s"$command does not take $raw\nhistos help $command"

  private def family(key: String): Option[String] =
    val hits = entries.filter(entry => entry.name == key || entry.name.startsWith(s"$key "))
    hits match
      case Nil => None
      case one => Some(one.map(entry => s"${entry.usage}\n${entry.about}").mkString("\n\n"))
end Help
