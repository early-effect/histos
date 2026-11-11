package histos.cli

import histos.cache.CacheCleanMode
import histos.plugin.{PluginArgs, PluginIdError, PluginTaskError, PluginTaskId}
import histos.scaffold.ImportSource
import histos.schedule.{CacheNamespace, NamespaceError}
import histos.server.{LauncherAction, ResolveAction}
import histos.task.{RunId, TaskError, TaskId}
import histos.testrun.{SuitePattern, SuitePatternError}
import histos.workflow.WorkflowAction
import histos.{Platform, PlatformError, ProjectName, ProjectNameError}

import zio.*
import zio.test.*

object GrammarLawSpec extends ZIOSpecDefault:
  def spec = suite("grammar laws")(
    test("a bare watch is refused and a nested watch is refused"):
      held(
        List("~")                 -> Left(CliParseError.WatchCommand("watch needs a command")),
        List("~", "~", "compile") -> Left(CliParseError.WatchCommand("watch does not nest")),
      )
    ,
    test("watch wraps the inner command and keeps the original tokens"):
      withReady: ready =>
        held(
          List("~", "compile", "api") -> Right(
            Command.Watch(Command.Compile(List(ProjectName("api"))), List("compile", "api"))
          ),
          List("~", "run", "inspect") -> Right(
            Command.Watch(Command.Run(ready.inspect, None), List("run", "inspect"))
          ),
          List("~", "--restart", "run", "api") -> Right(
            Command.Watch(Command.Run(ready.api, None), List("--restart", "run", "api"))
          ),
          List("~", "--restart", "compile")        -> Left(CliParseError.WatchCommand("--restart watches a program")),
          List("~", "--restart", "run", "inspect") -> Left(CliParseError.WatchCommand("--restart watches a program")),
        )
    ,
    test("explain takes one check id"):
      held(
        List("--explain")               -> Left(CliParseError.Pattern("--explain needs a check id")),
        List("--explain", "a", "b")     -> Left(CliParseError.Pattern("--explain takes one check id")),
        List("--explain", "supply.pin") -> Right(Command.Explain("supply.pin")),
      )
    ,
    test("publish without a known subcommand is the publish form"):
      held(
        List("publish")                    -> Left(CliParseError.PublishForm),
        List("publish", "nope")            -> Left(CliParseError.PublishForm),
        List("preview", "publish")         -> Left(CliParseError.PublishForm),
        List("preview", "publish", "nope") -> Left(CliParseError.PublishForm),
      )
    ,
    test("publish release keeps automatic when the flag follows a token"):
      held(
        List("publish", "release")                        -> Right(Command.PublishRelease(false, Nil)),
        List("publish", "release", "--automatic")         -> Right(Command.PublishRelease(true, Nil)),
        List("publish", "release", "ship", "--automatic") -> Right(
          Command.PublishRelease(true, List("ship"))
        ),
        List("publish", "snapshots", "api") -> Right(Command.PublishSnapshots(List("api"))),
        List("publish", "assets", "a")      -> Right(Command.PublishAssets(List("a"))),
        List("publish", "promote", "dep")   -> Right(Command.PublishPromote("dep")),
        List("publish", "drop", "dep")      -> Right(Command.PublishDrop("dep")),
        List("publishLocal", "tok")         -> Right(Command.PublishLocal(List("tok"))),
      )
    ,
    test("cleanFull rejects a project and preview cleanFull does the same"):
      held(
        List("cleanFull")                   -> Right(Command.CleanFull),
        List("cleanFull", "api")            -> Left(CliParseError.Pattern("cleanFull does not take a project")),
        List("preview", "cleanFull")        -> Right(Command.PreviewCleanFull),
        List("preview", "cleanFull", "api") -> Left(CliParseError.Pattern("cleanFull does not take a project")),
        List("clean", "api")                -> Right(Command.Clean(List(ProjectName("api")))),
      )
    ,
    test("init from is sbt or zipx and new keeps bare, yes, and the branch"):
      held(
        List("init", "--from", "mill") -> Left(CliParseError.Pattern("init --from is sbt or zipx")),
        List("init")                   -> Right(Command.Init(false, false, None, None)),
        List("init", "--from", "sbt")  -> Right(Command.Init(false, false, Some(ImportSource.Sbt), None)),
        List("init", "--bare", "--yes", "--from", "zipx", "--branch", "main") -> Right(
          Command.Init(true, true, Some(ImportSource.Zipx), Some("main"))
        ),
        List("new", "demo")                                        -> Right(Command.New("demo", false, false, None)),
        List("new", "--bare", "--yes", "--branch", "main", "demo") -> Right(
          Command.New("demo", true, true, Some("main"))
        ),
      )
    ,
    test("cancel rejects an empty id and keeps a token that is only a space"):
      withReady: ready =>
        held(
          List("cancel")          -> Left(CliParseError.MissingArgument),
          List("cancel", "")      -> Left(CliParseError.RunId(TaskError.EmptyId)),
          List("cancel", "run-1") -> Right(Command.Cancel(RunId.trusted("run-1"))),
          List("cancel", " ")     -> Right(Command.Cancel(ready.spaced)),
        )
    ,
    test("cache promote needs both namespaces and rejects a bad shape"):
      withReady: ready =>
        held(
          List("cache", "promote") -> Left(CliParseError.Pattern("cache promote needs --from and --to")),
          List("cache", "promote", "--from", "nope", "--to", "main") -> Left(
            CliParseError.Namespace(NamespaceError.Shape("nope"))
          ),
          List("cache", "promote", "--from", "main", "--to", "pr/2") -> Right(
            Command.CachePromote(ready.from, ready.to)
          ),
          List("cache", "path")               -> Right(Command.CachePath),
          List("cache", "list")               -> Right(Command.CacheList),
          List("cache", "clean")              -> Right(Command.CacheClean(CacheCleanMode.Remove)),
          List("cache", "clean", "--dry-run") -> Right(Command.CacheClean(CacheCleanMode.DryRun)),
        )
    ,
    test("resolve, workflow, and launcher refuse both flags and accept one"):
      held(
        List("resolve")                          -> Right(Command.Resolve(ResolveAction.Verify)),
        List("resolve", "--update")              -> Right(Command.Resolve(ResolveAction.Update)),
        List("resolve", "--check")               -> Right(Command.Resolve(ResolveAction.Check)),
        List("resolve", "--update", "--check")   -> Left(CliParseError.Rejected),
        List("workflow")                         -> Right(Command.Workflow(WorkflowAction.Check)),
        List("workflow", "--update")             -> Right(Command.Workflow(WorkflowAction.Update)),
        List("workflow", "--check")              -> Right(Command.Workflow(WorkflowAction.Check)),
        List("workflow", "--check", "--update")  -> Left(CliParseError.Rejected),
        List("launcher")                         -> Left(CliParseError.Rejected),
        List("launcher", "--update", "--latest") -> Left(CliParseError.Rejected),
        List("launcher", "--update")             -> Right(Command.Launcher(LauncherAction.Update)),
        List("launcher", "--latest")             -> Right(Command.Launcher(LauncherAction.Latest)),
      )
    ,
    test("affected json selects nothing, everything, or the named modules"):
      held(
        List("test")                     -> Right(Command.Test(Nil, AffectedChoice.Unset)),
        List("test", "api")              -> Right(Command.Test(List(ProjectName("api")), AffectedChoice.Unset)),
        List("test", "--affected", "[]") -> Right(Command.Test(Nil, AffectedChoice.NoneSelected)),
        List("test", "--affected", """["all"]""")       -> Right(Command.Test(Nil, AffectedChoice.Everything)),
        List("test", "--affected", """["all","api"]""") -> Right(Command.Test(Nil, AffectedChoice.Everything)),
        List("test", "--affected", """["api"]""")       -> Right(
          Command.Test(Nil, AffectedChoice.Only(List(ProjectName("api"))))
        ),
        List("test", "--affected", "nope") -> Left(
          CliParseError.Pattern("affected is a JSON list of module names")
        ),
        List("test", "--affected", """["1api"]""") -> Left(
          CliParseError.ProjectName(ProjectNameError.Bad("1api"))
        ),
      )
    ,
    test("testOnly needs a pattern and a project after module"):
      withReady: ready =>
        held(
          List("testOnly")                         -> Left(CliParseError.MissingPattern),
          List("testOnly", "--module")             -> Left(CliParseError.Pattern("testOnly --module needs a project")),
          List("testOnly", "--module", "api", "*") -> Right(
            Command.TestOnly(List(ProjectName("api")), List(ready.star))
          ),
          List("testQuick")           -> Right(Command.TestQuick(Nil, Nil)),
          List("testQuick", "*")      -> Right(Command.TestQuick(Nil, List(ready.star))),
          List("testOnly", " ")       -> Left(CliParseError.SuitePattern(SuitePatternError.Blank(" "))),
          List("testOnly", "--bogus") -> Left(CliParseError.Pattern(Help.flag("testOnly", "--bogus"))),
          List("test", "--bogus")     -> Left(CliParseError.Pattern(Help.flag("test", "--bogus"))),
        )
    ,
    test("a module run takes a platform and a built-in or a plugin task does not"):
      withReady: ready =>
        held(
          List("run")                     -> Left(CliParseError.MissingArgument),
          List("run", "")                 -> Left(CliParseError.BadTaskId),
          List("run", "inspect")          -> Right(Command.Run(ready.inspect, None)),
          List("run", "api", "jvm")       -> Right(Command.Run(ready.api, Some(Platform.Jvm))),
          List("run", "api", "bogus")     -> Left(CliParseError.BadPlatform(PlatformError.Unknown("bogus"))),
          List("run", "inspect", "jvm")   -> Left(CliParseError.TaskPlatform("inspect")),
          List("run", "demo/pack")        -> Right(Command.Plugin(ready.plugin, PluginArgs.empty)),
          List("run", "demo/pack", "jvm") -> Left(CliParseError.TaskPlatform("demo/pack")),
          List("run", "/pack")            -> Left(
            CliParseError.BadPluginTask(PluginTaskError.Plugin(PluginIdError.Empty))
          ),
          List("preview", "run", "api", "js")     -> Right(Command.PreviewRun(ready.api, Some(Platform.Js))),
          List("preview", "run", "demo/pack")     -> Right(Command.PreviewPlugin(ready.plugin)),
          List("preview", "compile", "api")       -> Right(Command.PreviewCompile(List(ProjectName("api")))),
          List("preview", "test")                 -> Right(Command.PreviewTest(Nil)),
          List("preview", "publishLocal", "tok")  -> Right(Command.PreviewPublishLocal(List("tok"))),
          List("preview", "publish", "snapshots") -> Right(Command.PreviewPublishSnapshots(Nil)),
          List("preview", "clean", "api")         -> Right(Command.PreviewClean(List(ProjectName("api")))),
        )
    ,
    test("doctor, deps, and setup keep the flags they were given"):
      held(
        List("doctor", "--area", "supply", "--json", "--preflight", "job", "--fix", "--yes") -> Right(
          Command.Doctor(Some("supply"), true, Some("job"), true, true, false, Nil)
        ),
        List("doctor", "--verbose") -> Right(
          Command.Doctor(None, false, None, false, false, true, Nil)
        ),
        List("doctor", "web") -> Right(
          Command.Doctor(None, false, None, false, false, false, List(ProjectName("web")))
        ),
        List("doctor", "--area", "toolchain", "web") -> Right(
          Command.Doctor(Some("toolchain"), false, None, false, false, false, List(ProjectName("web")))
        ),
        List("deps", "check")                         -> Right(Command.Deps(DepsAction.Check)),
        List("deps", "update", "--yes", "--security") -> Right(Command.Deps(DepsAction.Update(true, true))),
        List("deps", "audit", "--report")             -> Right(Command.Deps(DepsAction.Audit(true))),
        List("deps", "pr", "--security", "--open")    -> Right(Command.Deps(DepsAction.Pr(true, true))),
        List("deps", "alert")                         -> Right(Command.Deps(DepsAction.Alert)),
        List("deps", "snapshot", "--submit")          -> Right(Command.Deps(DepsAction.Snapshot(true))),
        List("setup", "supply-chain", "--from", "zipx", "--fix", "--yes") -> Right(
          Command.Setup(Some("zipx"), true, true)
        ),
        List("affected", "--base", "abc") -> Right(Command.Affected("abc")),
      )
    ,
    test("shim commands name the installed script"):
      held(
        List("update") -> Left(
          CliParseError.Pattern("histos update is run by the installed shim\nhistos help update")
        ),
        List("self", "update") -> Left(
          CliParseError.Pattern("histos self update is run by the installed shim\nhistos help self")
        ),
        List("toolchain", "install", "9") -> Left(
          CliParseError.Pattern(
            "histos toolchain install <version> is run by the installed shim\nhistos help toolchain"
          )
        ),
        List("toolchain", "list") -> Left(
          CliParseError.Pattern("histos toolchain list is run by the installed shim\nhistos help toolchain")
        ),
        List("toolchain") -> Right(Command.Toolchain),
      )
    ,
    test("an unknown family token is that family and an unknown root is not a command"):
      held(
        List("help", "nope")         -> Right(Command.Help(List("nope"))),
        List("help", "compil")       -> Right(Command.Help(List("compil"))),
        List("setup", "suply-chain") -> Left(
          CliParseError.UnknownCommand("suply-chain", histos.doctor.Suggest.near("suply-chain", List("supply-chain")))
        ),
        List("deps", "chek") -> Left(
          CliParseError.UnknownCommand(
            "chek",
            histos.doctor.Suggest.near("chek", List("check", "update", "audit", "pr", "alert", "snapshot")),
          )
        ),
        List("cache", "pah") -> Left(
          CliParseError
            .UnknownCommand("pah", histos.doctor.Suggest.near("pah", List("path", "list", "clean", "promote")))
        ),
        List("preview", "nope") -> Left(
          CliParseError.UnknownCommand(
            "nope",
            histos.doctor.Suggest.near(
              "nope",
              List("compile", "test", "publish", "publishLocal", "clean", "cleanFull", "run"),
            ),
          )
        ),
        List("nope") -> Left(CliParseError.UnknownRoot("nope")),
        Nil          -> Left(CliParseError.Help),
      )
    ,
    test("help keeps the topic, and render refuses a topic that is not a command"):
      val nope   = Help.render(List("nope"))
      val compil = Help.render(List("compil"))
      assertTrue(
        nope == Left("unknown command 'nope'\nhistos help\nhistos help <command>"),
        compil == Left("unknown command 'compil'\ndid you mean compile?\nhistos help\nhistos help compile"),
      )
    ,
    test("compile rejects a coverage flag"):
      held(
        List("compile", "--exclude-class", "Foo") -> Left(
          CliParseError.Pattern(Help.flag("compile", "--exclude-class"))
        ),
        List("compile", "--min-stmt", "1") -> Left(CliParseError.Pattern(Help.flag("compile", "--min-stmt"))),
        List("compile", "api")             -> Right(Command.Compile(List(ProjectName("api")))),
      )
    ,
    test("a bare command is that command"):
      held(
        List("version")                  -> Right(Command.Version),
        List("graph")                    -> Right(Command.Graph),
        List("mcp")                      -> Right(Command.Mcp),
        List("bsp")                      -> Right(Command.Bsp),
        List("server")                   -> Right(Command.Server),
        List("portal")                   -> Right(Command.Portal),
        List("observe")                  -> Right(Command.Observe),
        List("reload")                   -> Right(Command.Reload),
        List("reboot")                   -> Right(Command.Reboot),
        List("client")                   -> Right(Command.Client),
        List("assembly")                 -> Right(Command.Assembly(None)),
        List("assembly", "dist/app.jar") -> Right(Command.Assembly(Some("dist/app.jar"))),
        List("help")                     -> Right(Command.Help(Nil)),
        List("help", "compile")          -> Right(Command.Help(List("compile"))),
      ),
  )

  private def held(rows: (List[String], Either[CliParseError, Command])*): UIO[TestResult] =
    mismatches(rows.toList).map(bad => assertTrue(bad == Nil))

  private def withReady(use: Ready => UIO[TestResult]): UIO[TestResult] =
    ready match
      case Some(found) => use(found)
      case None        => ZIO.succeed(assertTrue(false))

  private def mismatches(rows: List[(List[String], Either[CliParseError, Command])]): UIO[List[String]] =
    ZIO
      .foreach(rows): (args, expected) =>
        Grammar
          .parse(args)
          .either
          .map: got =>
            if got == expected then Nil
            else List(s"${args.mkString(" ")} expected $expected got $got")
      .map(_.flatten)

  private def ready: Option[Ready] =
    for
      inspect <- TaskId.from("inspect").toOption
      api     <- TaskId.from("api").toOption
      plugin  <- PluginTaskId.parse("demo/pack").toOption
      from    <- CacheNamespace.from("main").toOption
      to      <- CacheNamespace.from("pr/2").toOption
      star    <- SuitePattern.from("*").toOption
      spaced  <- RunId.from(" ").toOption
    yield Ready(inspect, api, plugin, from, to, star, spaced)

  private final case class Ready(
      inspect: TaskId,
      api: TaskId,
      plugin: PluginTaskId,
      from: CacheNamespace,
      to: CacheNamespace,
      star: SuitePattern,
      spaced: RunId,
  )
end GrammarLawSpec
