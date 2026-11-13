package histos.cli

import histos.coverage.Excludes
import histos.doctor.FixMode
import histos.plugin.{PluginArgs, PluginId, PluginTaskId, TaskName}
import histos.scaffold.ImportSource
import histos.server.{Call, CleanExtent, WatchPolicy}
import histos.supply.{PolicyWrite, ZipxConfirm}
import histos.task.{RunId, TaskId}
import histos.testrun.{SuitePattern, TestMode}
import histos.{CoverageFloors, Platform, ProjectName}

import zio.*
import zio.test.*

object OperationLawSpec extends ZIOSpecDefault:
  private val plugin = PluginTaskId(PluginId("demo"), TaskName("pack"))

  def spec = suite("operation laws")(
    test("mcp, bsp, server, portal, observe, and the local commands are not operations"):
      val nested = Left(CliParseError.WatchCommand("watch does not nest"))
      val rows   = List(
        Command.Mcp                                                       -> Left(CliParseError.Rejected),
        Command.Bsp                                                       -> Left(CliParseError.Rejected),
        Command.Server                                                    -> Left(CliParseError.Rejected),
        Command.Portal                                                    -> Left(CliParseError.Rejected),
        Command.Observe                                                   -> Left(CliParseError.Rejected),
        Command.New("demo", ScaffoldChoice.Prompt, None)                  -> Left(CliParseError.Rejected),
        Command.Init(ScaffoldChoice.Prompt, None, None)                   -> Left(CliParseError.Rejected),
        Command.Init(ScaffoldChoice.Prompt, Some(ImportSource.Sbt), None) -> Left(CliParseError.Rejected),
        Command.Doctor(None, ReportForm.Text, None, FixMode.Print, DoctorDetail.Brief, Nil) -> Left(
          CliParseError.Rejected
        ),
        Command.Explain("supply.pin")                               -> Left(CliParseError.Rejected),
        Command.Deps(DepsAction.Check)                              -> Left(CliParseError.Rejected),
        Command.Setup(None, PolicyWrite.Preview, ZipxConfirm.Ask)   -> Left(CliParseError.Rejected),
        Command.Help(Nil)                                           -> Left(CliParseError.Rejected),
        Command.Coverage(Nil, CoverageFloors(), Excludes(Nil, Nil)) -> Left(CliParseError.Rejected),
        Command.Affected("abc")                                     -> Left(CliParseError.Rejected),
        Command.Watch(Command.Compile(Nil), List("compile"))        -> nested,
        Command.Watch(Command.Compile(Nil), Nil)                    -> nested,
      )
      val misses = rows.collect {
        case (command, wanted) if Operation.operation(command) != wanted => (command, Operation.operation(command))
      }
      assertTrue(misses == Nil)
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
      val none  = Command.Test(List(ProjectName("api")), AffectedChoice.NoneSelected, TestMode.Run, Nil)
      val unset = Command.Test(Nil, AffectedChoice.Unset, TestMode.Run, Nil)
      val all   = Command.Test(List(ProjectName("api")), AffectedChoice.Everything, TestMode.Run, Nil)
      assertTrue(
        Operation.operation(none) == Left(CliParseError.Pattern("nothing is affected")),
        Operation.operation(unset) == Right(Call.Test(Nil, Nil, TestMode.Run)),
        Operation.operation(all) == Right(Call.Test(List(ProjectName("api")), Nil, TestMode.Run)),
      )
    ,
    test("a built-in run is a task, a plugin is a task, and release flips its arguments"):
      for api <- ZIO.fromEither(TaskId.from("api"))
      yield assertTrue(
        Operation.operation(Command.Run(TaskId.Inspect, Some(Platform.Jvm))) == Right(
          Call.Task("inspect", PluginArgs.empty)
        ),
        Operation.operation(Command.Run(api, Some(Platform.Jvm))) == Right(Call.Run("api", Some(Platform.Jvm))),
        Operation.operation(Command.Plugin(plugin, PluginArgs.empty)) == Right(
          Call.Task("demo/pack", PluginArgs.empty)
        ),
        Operation.operation(Command.PublishRelease(true, List("ship"))) == Right(
          Call.PublishRelease(List("ship"), true)
        ),
        Operation.operation(Command.PreviewPlugin(plugin)) == Right(Call.PreviewRun("demo/pack", None)),
        Operation.operation(
          Command.Test(List(ProjectName("api")), AffectedChoice.Unset, TestMode.Run, List(SuitePattern("*")))
        ) == Right(Call.Test(List(ProjectName("api")), List(SuitePattern("*")), TestMode.Run)),
        Operation.operation(Command.Test(Nil, AffectedChoice.Unset, TestMode.Quick, Nil)) == Right(
          Call.Test(Nil, Nil, TestMode.Quick)
        ),
        Operation.operation(Command.Compile(List(ProjectName("api")))) == Right(
          Call.Compile(List(ProjectName("api")))
        ),
        Operation.operation(Command.Version) == Right(Call.Version),
        Operation.operation(Command.Clean(Nil, CleanExtent.Full)) == Right(Call.Clean(Nil, CleanExtent.Full)),
        Operation.operation(Command.Clean(List(ProjectName("api")), CleanExtent.Full)) == Left(
          CliParseError.Pattern("clean --full does not take a project")
        ),
      )
    ,
    test("callOf sends compile, test, run, and clean, and leaves every other command"):
      for api <- ZIO.fromEither(TaskId.from("api"))
      yield
        val rows = List(
          Command.Compile(List(ProjectName("api")))                  -> Some(Call.Compile(List(ProjectName("api")))),
          Command.Test(Nil, AffectedChoice.Unset, TestMode.Run, Nil) -> Some(Call.Test(Nil, Nil, TestMode.Run)),
          Command.Test(List(ProjectName("api")), AffectedChoice.NoneSelected, TestMode.Run, Nil) -> None,
          Command.Run(TaskId.Inspect, None)        -> Some(Call.Task("inspect", PluginArgs.empty)),
          Command.Run(api, Some(Platform.Native))  -> Some(Call.Run("api", Some(Platform.Native))),
          Command.Clean(Nil, CleanExtent.Products) -> Some(Call.Clean(Nil, CleanExtent.Products)),
          Command.Clean(Nil, CleanExtent.Full)     -> Some(Call.Clean(Nil, CleanExtent.Full)),
          Command.Plugin(plugin, PluginArgs.empty) -> None,
          Command.Version                          -> None,
          Command.Assembly(None)                   -> None,
          Command.Cancel(RunId("run-1"))           -> None,
          Command.Graph                            -> None,
        )
        val bad = rows.filter { (command, expected) => Cli.callOf(command) != expected }
        assertTrue(bad == Nil),
  )
end OperationLawSpec
