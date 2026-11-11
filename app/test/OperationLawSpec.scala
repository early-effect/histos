package histos.cli

import histos.coverage.Excludes
import histos.plugin.{PluginArgs, PluginId, PluginTaskId, TaskName}
import histos.scaffold.ImportSource
import histos.server.{Call, WatchPolicy}
import histos.task.{RunId, TaskId}
import histos.testrun.{SuitePattern, TestMode}
import histos.{CoverageFloors, Platform, ProjectName}

import zio.*
import zio.test.*

object OperationLawSpec extends ZIOSpecDefault:
  def spec = suite("operation laws")(
    test("mcp, bsp, server, portal, observe, and the local commands are not operations"):
      val rejected = List(
        Command.Mcp,
        Command.Bsp,
        Command.Server,
        Command.Portal,
        Command.Observe,
        Command.New("demo", false, false, None),
        Command.Init(false, false, None, None),
        Command.Init(false, false, Some(ImportSource.Sbt), None),
        Command.Doctor(None, false, None, false, false, false, Nil),
        Command.Explain("supply.pin"),
        Command.Deps(DepsAction.Check),
        Command.Setup(None, false, false),
        Command.Help(Nil),
        Command.Coverage(Nil, CoverageFloors(), Excludes(Nil, Nil)),
        Command.Affected("abc"),
        Command.Watch(Command.Compile(Nil), List("compile")),
      )
      val bad = rejected.filter(command =>
        Operation.operation(command) != Left(CliParseError.Rejected) &&
          Operation.operation(command) != Left(CliParseError.WatchCommand("watch does not nest"))
      )
      val watch = Operation.operation(Command.Watch(Command.Compile(Nil), Nil))
      assertTrue(
        bad == Nil,
        watch == Left(CliParseError.WatchCommand("watch does not nest")),
        Operation.operation(Command.Coverage(Nil, CoverageFloors(), Excludes(Nil, Nil))) == Left(
          CliParseError.Rejected
        ),
      )
    ,
    test("a nested watch and bsp are not a watch payload"):
      assertTrue(
        Operation.underWatch(Call.Watch(Call.Version, WatchPolicy.After), WatchPolicy.After) == Left(
          CliParseError.WatchCommand("watch does not nest")
        ),
        Operation.underWatch(Call.Compile(Nil), WatchPolicy.Restart) == Left(
          CliParseError.WatchCommand("--restart watches a program")
        ),
        Operation.underWatch(Call.Bsp, WatchPolicy.After) == Left(CliParseError.Rejected),
        Operation.underWatch(Call.Version, WatchPolicy.After) == Right(Call.Version),
        Operation.call(Command.Watch(Command.Version, Nil)) == Right(Call.Watch(Call.Version, WatchPolicy.After)),
      )
    ,
    test("nothing affected is a pattern and every other test choice is a run"):
      val none  = Command.Test(List(ProjectName("api")), AffectedChoice.NoneSelected)
      val unset = Command.Test(Nil, AffectedChoice.Unset)
      val all   = Command.Test(List(ProjectName("api")), AffectedChoice.Everything)
      assertTrue(
        Operation.operation(none) == Left(CliParseError.Pattern("nothing is affected")),
        Operation.operation(unset) == Right(Call.Test(Nil, Nil, TestMode.Run)),
        Operation.operation(all) == Right(Call.Test(List(ProjectName("api")), Nil, TestMode.Run)),
      )
    ,
    test("a built-in run is a task, a plugin is a task, and release flips its arguments"):
      val plugin = PluginTaskId(PluginId("demo"), TaskName("pack"))
      TaskId.from("inspect") match
        case Left(_)        => assertTrue(false)
        case Right(inspect) =>
          TaskId.from("api") match
            case Left(_)    => assertTrue(false)
            case Right(api) =>
              SuitePattern.from("*") match
                case Left(_)     => assertTrue(false)
                case Right(star) =>
                  assertTrue(
                    Operation.operation(Command.Run(inspect, Some(Platform.Jvm))) == Right(
                      Call.Task("inspect", PluginArgs.empty)
                    ),
                    Operation.operation(Command.Run(api, Some(Platform.Jvm))) == Right(
                      Call.Run("api", Some(Platform.Jvm))
                    ),
                    Operation.operation(Command.Plugin(plugin, PluginArgs.empty)) == Right(
                      Call.Task(plugin.render, PluginArgs.empty)
                    ),
                    Operation.operation(Command.PublishRelease(true, List("ship"))) == Right(
                      Call.PublishRelease(List("ship"), true)
                    ),
                    Operation.operation(Command.PreviewPlugin(plugin)) == Right(
                      Call.PreviewRun(plugin.render, None)
                    ),
                    Operation.operation(Command.TestOnly(List(ProjectName("api")), List(star))) == Right(
                      Call.Test(List(ProjectName("api")), List(star), TestMode.Run)
                    ),
                    Operation.operation(Command.TestQuick(Nil, Nil)) == Right(Call.Test(Nil, Nil, TestMode.Quick)),
                    Operation.operation(Command.Compile(List(ProjectName("api")))) == Right(
                      Call.Compile(List(ProjectName("api")))
                    ),
                    Operation.operation(Command.Version) == Right(Call.Version),
                    Operation.operation(Command.CleanFull) == Right(Call.CleanFull),
                  )
    ,
    test("callOf sends compile, test, run, and clean, and leaves every other command"):
      val plugin = PluginTaskId(PluginId("demo"), TaskName("pack"))
      TaskId.from("inspect") match
        case Left(_)        => assertTrue(false)
        case Right(inspect) =>
          TaskId.from("api") match
            case Left(_)    => assertTrue(false)
            case Right(api) =>
              RunId.from("run-1") match
                case Left(_)   => assertTrue(false)
                case Right(id) =>
                  val rows = List(
                    Command.Compile(List(ProjectName("api"))) -> Some(Call.Compile(List(ProjectName("api")))),
                    Command.Test(Nil, AffectedChoice.Unset)   -> Some(Call.Test(Nil, Nil, TestMode.Run)),
                    Command.Test(List(ProjectName("api")), AffectedChoice.NoneSelected) -> None,
                    Command.Run(inspect, None)               -> Some(Call.Task("inspect", PluginArgs.empty)),
                    Command.Run(api, Some(Platform.Native))  -> Some(Call.Run("api", Some(Platform.Native))),
                    Command.Clean(Nil)                       -> Some(Call.Clean(Nil)),
                    Command.CleanFull                        -> Some(Call.CleanFull),
                    Command.Plugin(plugin, PluginArgs.empty) -> None,
                    Command.Version                          -> None,
                    Command.TestOnly(Nil, Nil)               -> None,
                    Command.Cancel(id)                       -> None,
                    Command.Graph                            -> None,
                  )
                  val bad = rows.filter { (command, expected) => Cli.callOf(command) != expected }
                  assertTrue(bad == Nil),
  )
