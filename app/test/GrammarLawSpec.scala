package histos.cli

import histos.cache.CacheCleanMode
import histos.central.DeploymentId
import histos.doctor.{Area, FixMode}
import histos.plugin.{CommandName, JobName, PluginArgs, PluginId, PluginIdError, PluginTaskError, PluginTaskId}
import histos.publish.PublishToken
import histos.scaffold.ImportSource
import histos.schedule.{CacheNamespace, NamespaceError}
import histos.server.{CleanExtent, ForgetTarget, ResolveAction}
import histos.supply.{PolicyWrite, SetupSource, ZipxConfirm}
import histos.task.{RunId, TaskError, TaskId}
import histos.testrun.{SuitePattern, SuitePatternError, TestMode}
import histos.workflow.WorkflowAction
import histos.{Platform, PlatformError, ProjectName, ProjectNameError}

import zio.*
import zio.cli.Command as CliCommand
import zio.test.*

object GrammarLawSpec extends ZIOSpecDefault:
  def spec = suite("grammar laws")(
    test("every built-in root word is reserved, and only acp and the shim words are reserved beyond them"):
      val words    = rootWords(Grammar.builtIns)
      val reserved = CommandName.reserved.map(_.value)
      assertTrue(
        words.nonEmpty,
        words -- reserved == Set.empty[String],
        reserved -- words == Set(
          "acp",
          "update",
          "self",
          "testOnly",
          "testQuick",
          "cleanFull",
          "publishLocal",
          "client",
          "launcher",
          "toolchain",
        ),
      )
    ,
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
        List("publish", "promote", "dep")   -> Right(Command.PublishPromote(DeploymentId("dep"))),
        List("publish", "drop", "dep")      -> Right(Command.PublishDrop(DeploymentId("dep"))),
        List("publish", "local", "tok")     -> Right(Command.PublishLocal(List(PublishToken("tok")))),
        List("publishLocal", "tok")         -> Left(CliParseError.Retired("publishLocal", "publish local")),
      )
    ,
    test("clean --full takes no project and the old word names the flag"):
      held(
        List("clean", "--full")            -> Right(Command.Clean(Nil, CleanExtent.Full)),
        List("clean", "--full", "api")     -> Left(CliParseError.Pattern("clean --full does not take a project")),
        List("preview", "clean", "--full") -> Right(Command.PreviewClean(Nil, CleanExtent.Full)),
        List("preview", "clean", "--full", "api") -> Left(
          CliParseError.Pattern("preview clean --full does not take a project")
        ),
        List("clean", "api")         -> Right(Command.Clean(List(ProjectName("api")), CleanExtent.Products)),
        List("cleanFull")            -> Left(CliParseError.Retired("cleanFull", "clean --full")),
        List("cleanFull", "api")     -> Left(CliParseError.Retired("cleanFull", "clean --full")),
        List("preview", "cleanFull") -> Left(CliParseError.Retired("preview cleanFull", "preview clean --full")),
      )
    ,
    test("init from is sbt or zipx and new keeps bare, yes, and the branch"):
      held(
        List("init", "--from", "mill") -> Left(CliParseError.Pattern("init --from is sbt or zipx")),
        List("init")                   -> Right(Command.Init(ScaffoldChoice.Prompt, None, None)),
        List("init", "--from", "sbt")  -> Right(Command.Init(ScaffoldChoice.Prompt, Some(ImportSource.Sbt), None)),
        List("init", "--bare", "--yes", "--from", "zipx", "--branch", "main") -> Right(
          Command.Init(ScaffoldChoice.Bare, Some(ImportSource.Zipx), Some("main"))
        ),
        List("new", "demo") -> Right(Command.New("demo", ScaffoldChoice.Prompt, None)),
        List("new", "--bare", "--yes", "--branch", "main", "demo") -> Right(
          Command.New("demo", ScaffoldChoice.Bare, Some("main"))
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
    test("cache forget is a namespace or a module, and verify names modules"):
      val pr = CacheNamespace.PullRequest(histos.version.PullRequestNumber("7"))
      held(
        List("cache", "forget")           -> Left(CliParseError.MissingArgument),
        List("cache", "forget", "main")   -> Right(Command.CacheForget(ForgetTarget.Namespace(CacheNamespace.Main))),
        List("cache", "forget", "pr/7")   -> Right(Command.CacheForget(ForgetTarget.Namespace(pr))),
        List("cache", "forget", "core")   -> Right(Command.CacheForget(ForgetTarget.Module(ProjectName("core")))),
        List("cache", "forget", "nope/x") -> Left(CliParseError.Namespace(NamespaceError.Shape("nope/x"))),
        List("cache", "forget", "")       -> Left(CliParseError.ProjectName(ProjectNameError.Empty)),
        List("cache", "verify")           -> Right(Command.CacheVerify(Nil)),
        List("cache", "verify", "core", "api") -> Right(
          Command.CacheVerify(List(ProjectName("core"), ProjectName("api")))
        ),
        List("cache", "verify", "1api") -> Left(CliParseError.ProjectName(ProjectNameError.Bad("1api"))),
      )
    ,
    test("resolve and workflow refuse both flags and accept one, and launcher names update"):
      held(
        List("resolve")                         -> Right(Command.Resolve(ResolveAction.Verify)),
        List("resolve", "--update")             -> Right(Command.Resolve(ResolveAction.Update)),
        List("resolve", "--check")              -> Right(Command.Resolve(ResolveAction.Check)),
        List("resolve", "--update", "--check")  -> Left(CliParseError.Rejected),
        List("workflow")                        -> Right(Command.Workflow(WorkflowAction.Check)),
        List("workflow", "--update")            -> Right(Command.Workflow(WorkflowAction.Update)),
        List("workflow", "--check")             -> Right(Command.Workflow(WorkflowAction.Check)),
        List("workflow", "--check", "--update") -> Left(CliParseError.Rejected),
        List("launcher")                        -> Left(CliParseError.Retired("launcher", "update")),
        List("launcher", "--update")            -> Left(CliParseError.Retired("launcher", "update")),
        List("launcher", "--latest")            -> Left(CliParseError.Retired("launcher", "update")),
      )
    ,
    test("affected json selects nothing, everything, or the named modules"):
      val run = TestMode.Run
      held(
        List("test")        -> Right(Command.Test(Nil, AffectedChoice.Unset, run, Nil)),
        List("test", "api") -> Right(Command.Test(List(ProjectName("api")), AffectedChoice.Unset, run, Nil)),
        List("test", "--affected", "[]")          -> Right(Command.Test(Nil, AffectedChoice.NoneSelected, run, Nil)),
        List("test", "--affected", """["all"]""") -> Right(Command.Test(Nil, AffectedChoice.Everything, run, Nil)),
        List("test", "--affected", """["all","api"]""") -> Right(
          Command.Test(Nil, AffectedChoice.Everything, run, Nil)
        ),
        List("test", "--affected", """["api"]""") -> Right(
          Command.Test(Nil, AffectedChoice.Only(List(ProjectName("api"))), run, Nil)
        ),
        List("test", "--affected", "nope") -> Left(
          CliParseError.Pattern("affected is a JSON list of module names")
        ),
        List("test", "--affected", """["1api"]""") -> Left(
          CliParseError.ProjectName(ProjectNameError.Bad("1api"))
        ),
      )
    ,
    test("test --only needs a pattern and the old words name the flags"):
      val run   = TestMode.Run
      val quick = TestMode.Quick
      val star  = List(SuitePattern("*"))
      held(
        List("test", "--only")             -> Left(CliParseError.Pattern("test --only needs a pattern")),
        List("test", "api", "--only", "*") -> Right(
          Command.Test(List(ProjectName("api")), AffectedChoice.Unset, run, star)
        ),
        List("test", "--quick")                -> Right(Command.Test(Nil, AffectedChoice.Unset, quick, Nil)),
        List("test", "--quick", "--only", "*") -> Right(Command.Test(Nil, AffectedChoice.Unset, quick, star)),
        List("test", "--only", " ")            -> Left(CliParseError.SuitePattern(SuitePatternError.Blank(" "))),
        List("test", "--bogus")                -> Left(CliParseError.Pattern(Help.flag("test", "--bogus"))),
        List("testOnly")                       -> Left(CliParseError.Retired("testOnly", "test --only")),
        List("testOnly", "*")                  -> Left(CliParseError.Retired("testOnly", "test --only")),
        List("testQuick")                      -> Left(CliParseError.Retired("testQuick", "test --quick")),
        List("preview", "testOnly", "*") -> Left(CliParseError.Retired("preview testOnly", "preview test --only")),
        List("preview", "testQuick")     -> Left(CliParseError.Retired("preview testQuick", "preview test --quick")),
        List("preview", "test", "--only", "*") -> Right(
          Command.PreviewTest(Nil, AffectedChoice.Unset, run, star)
        ),
        List("preview", "test", "--quick") -> Right(Command.PreviewTest(Nil, AffectedChoice.Unset, quick, Nil)),
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
          List("preview", "run", "api", "js") -> Right(Command.PreviewRun(ready.api, Some(Platform.Js))),
          List("preview", "run", "demo/pack") -> Right(Command.PreviewPlugin(ready.plugin)),
          List("preview", "compile", "api")   -> Right(Command.PreviewCompile(List(ProjectName("api")))),
          List("preview", "test") -> Right(Command.PreviewTest(Nil, AffectedChoice.Unset, TestMode.Run, Nil)),
          List("preview", "publish", "local", "tok") -> Right(
            Command.PreviewPublishLocal(List(PublishToken("tok")))
          ),
          List("preview", "publishLocal", "tok") -> Left(
            CliParseError.Retired("preview publishLocal", "preview publish local")
          ),
          List("preview", "publish", "snapshots") -> Right(Command.PreviewPublishSnapshots(Nil)),
          List("preview", "clean", "api")         -> Right(
            Command.PreviewClean(List(ProjectName("api")), CleanExtent.Products)
          ),
        )
    ,
    test("doctor, deps, and setup keep the flags they were given"):
      held(
        List("doctor", "--area", "supply", "--json", "--preflight", "job", "--fix", "--yes") -> Right(
          Command.Doctor(
            Some(DoctorScope.Plugin(PluginId("supply"))),
            ReportForm.Json,
            Some(JobName("job")),
            FixMode.from(true, true),
            DoctorDetail.Brief,
            Nil,
          )
        ),
        List("doctor", "--verbose") -> Right(
          Command.Doctor(None, ReportForm.Text, None, FixMode.Print, DoctorDetail.Verbose, Nil)
        ),
        List("doctor", "web") -> Right(
          Command.Doctor(None, ReportForm.Text, None, FixMode.Print, DoctorDetail.Brief, List(ProjectName("web")))
        ),
        List("doctor", "--area", "toolchain", "web") -> Right(
          Command.Doctor(
            Some(DoctorScope.BuiltIn(Area.Toolchain)),
            ReportForm.Text,
            None,
            FixMode.Print,
            DoctorDetail.Brief,
            List(ProjectName("web")),
          )
        ),
        List("deps", "check")                         -> Right(Command.Deps(DepsAction.Check)),
        List("deps", "update", "--yes", "--security") -> Right(Command.Deps(DepsAction.Update(true, true))),
        List("deps", "audit", "--report")             -> Right(Command.Deps(DepsAction.Audit(true))),
        List("deps", "pr", "--security", "--open")    -> Right(Command.Deps(DepsAction.Pr(true, true))),
        List("deps", "alert")                         -> Right(Command.Deps(DepsAction.Alert)),
        List("deps", "snapshot", "--submit")          -> Right(Command.Deps(DepsAction.Snapshot(true))),
        List("setup", "supply-chain", "--from", "zipx", "--fix", "--yes") -> Right(
          Command.Setup(Some(SetupSource.Zipx), PolicyWrite.Write, ZipxConfirm.Delete)
        ),
        List("setup", "supply-chain", "--from", "vendor/zipx") -> Right(
          Command.Setup(Some(SetupSource.Directory("vendor/zipx")), PolicyWrite.Preview, ZipxConfirm.Ask)
        ),
        List("affected", "--base", "abc") -> Right(Command.Affected("abc")),
        List("bump")                      -> Right(Command.Bump(histos.version.BumpPart.Patch, Nil)),
        List("bump", "--minor", "histos") -> Right(
          Command.Bump(histos.version.BumpPart.Minor, List(histos.version.ShipName("histos")))
        ),
        List("bump", "--major", "--patch") -> Left(CliParseError.Rejected),
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
              List("compile", "test", "publish", "clean", "run"),
            ),
          )
        ),
        List("nope") -> Left(CliParseError.UnknownRoot("nope")),
        Nil          -> Left(CliParseError.Help),
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
        List("version")              -> Right(Command.Version),
        List("graph")                -> Right(Command.Graph),
        List("mcp")                  -> Right(Command.Mcp),
        List("bsp")                  -> Right(Command.Bsp),
        List("server")               -> Right(Command.Server),
        List("portal")               -> Right(Command.Portal),
        List("observe")              -> Right(Command.Observe),
        List("toolchain")            -> Left(CliParseError.Retired("toolchain", "doctor")),
        List("toolchain", "install") -> Left(
          CliParseError.Pattern(
            "histos toolchain install <version> is run by the installed shim\nhistos help toolchain"
          )
        ),
        List("reload")          -> Right(Command.Reload),
        List("reboot")          -> Right(Command.Reboot),
        List("client")          -> Left(CliParseError.Retired("client", "publish")),
        List("assembly")        -> Right(Command.Assembly(Nil)),
        List("assembly", "app") -> Right(Command.Assembly(List("app"))),
        List("help")            -> Right(Command.Help(Nil)),
        List("help", "compile") -> Right(Command.Help(List("compile"))),
      ),
  )

  /** The word each alternative answers to. A command with subcommands answers to its own word. */
  private def rootWords(command: CliCommand[?]): Set[String] =
    command match
      case CliCommand.OrElse(left, right)    => rootWords(left) ++ rootWords(right)
      case CliCommand.Map(inner, _)          => rootWords(inner)
      case CliCommand.Subcommands(parent, _) => rootWords(parent)
      case single: CliCommand.Single[?, ?]   => Set(single.name)

  private def held(rows: (List[String], Either[CliParseError, Command])*): UIO[TestResult] =
    mismatches(rows.toList).map(bad => assertTrue(bad == Nil))

  /** A parser regression fails the law with its own error. */
  private def withReady(use: Ready => UIO[TestResult]): IO[TaskError | PluginTaskError | NamespaceError, TestResult] =
    ready.flatMap(use)

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

  /** The values a parse builds that have no literal are parsed here. */
  private def ready: IO[TaskError | PluginTaskError | NamespaceError, Ready] =
    for
      api    <- ZIO.fromEither(TaskId.from("api"))
      plugin <- ZIO.fromEither(PluginTaskId.parse("demo/pack"))
      from   <- ZIO.fromEither(CacheNamespace.from("main"))
      to     <- ZIO.fromEither(CacheNamespace.from("pr/2"))
    yield Ready(TaskId.Inspect, api, plugin, from, to, SuitePattern("*"), RunId(" "))

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
