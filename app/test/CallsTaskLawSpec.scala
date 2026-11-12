package histos.server

import histos.build.BuildEvent
import histos.cache.ContentStore
import histos.load.{Loader, PluginRuntime}
import histos.plugin.{ApiDigest, FlagName, PluginArgs, PluginTaskError, PluginTaskId}
import histos.report.{BuildFault, CallResult, Rejection}
import histos.resolve.{ResolutionService, Resolve}
import histos.task.TaskError
import histos.version.Catalog
import histos.workspace.Workspace
import histos.{
  LawMachine,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  Resolution,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
}
import slayer.stubbed

import java.nio.file.Path

import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** Built-in tasks name the projects and refuse arguments. Toolchain names the JDK and skips tools the graph does not
  * need.
  */
object CallsTaskLawSpec extends ZIOSpecDefault:
  private val lawName = ProjectName.trusted("tasklaw")

  def spec = suite("calls task laws")(
    test("inspect names the project, versions has no catalog, and toolchain skips node and clang"):
      Workspace
        .work("histos-calls-task")
        .flatMap: dir =>
          val program =
            for
              machine               <- LawMachine.at(dir)
              (blank, blankHeard)   <- speak(say => Calls.task(dir, graph, idle, machine, "", PluginArgs.empty, say))
              (shaped, shapedHeard) <- speak(say =>
                Calls.task(dir, graph, idle, machine, "nope", PluginArgs.empty, say)
              )
              (extra, extraHeard) <- speak(say =>
                Calls.task(dir, graph, idle, machine, "inspect", PluginArgs(Nil, Set(FlagName("extra"))), say)
              )
              (inspected, inspectedHeard) <- speak(say =>
                Calls.task(dir, graph, idle, machine, "inspect", PluginArgs.empty, say)
              )
              (versions, versionsHeard) <- speak(say =>
                Calls.task(dir, graph, idle, machine, "versions", PluginArgs.empty, say)
              )
              (tools, toolsHeard) <- speak(say => Calls.toolchain(machine, graph, say))
            yield
              val toolText = outs(toolsHeard)
              assertTrue(
                blank == Right(()),
                done(blankHeard) == Some(CallResult.Fault(BuildFault.Task(TaskError.EmptyId))),
                shaped == Right(()),
                done(shapedHeard) == Some(CallResult.Fault(BuildFault.PluginTask(PluginTaskError.Shape("nope")))),
                extra == Right(()),
                done(extraHeard) == Some(
                  CallResult.Fault(BuildFault.Rejected(Rejection.TaskArguments("inspect")))
                ),
                inspected == Right(()),
                done(inspectedHeard).contains(CallResult.reported()),
                outs(inspectedHeard) == lawName.value,
                versions == Right(()),
                done(versionsHeard).contains(CallResult.reported()),
                outs(versionsHeard) == "no catalog",
                tools == Right(()),
                done(toolsHeard).contains(CallResult.reported()),
                toolText.contains("node not required"),
                toolText.contains("clang not required"),
                toolText.contains(" from java.home"),
              )
          program.provideSomeLayer[Scope](session(dir))
  )

  private val idle: PluginRuntime = new PluginRuntime:
    def launcher: ApiDigest = ApiDigest.trusted("a" * 64)

    def run(id: PluginTaskId, args: PluginArgs) =
      ZIO.dieMessage("plugin was not asked")
    def bind =
      ZIO.dieMessage("plugin was not asked")
    def links = Nil

  private def done(heard: Chunk[Message]): Option[CallResult] =
    heard.collect { case Message.Done(result) => result }.lastOption

  private def outs(heard: Chunk[Message]): String =
    heard.collect { case Message.Event(BuildEvent.Out(text)) => text }.mkString("\n")

  private def speak[R, E](use: Say => ZIO[R, E, Unit]): ZIO[R, Nothing, (Either[E, Unit], Chunk[Message])] =
    for
      messages <- Ref.make(Chunk.empty[Message])
      say = new Say(message => messages.update(_ :+ message))
      result <- use(say).either
      heard  <- messages.get
    yield (result, heard)

  private def graph: ProjectGraph =
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(ScalaVersion("3.10.0"), Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )
    ProjectGraph.trusted(
      List(law),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  /** Inspect, versions, and toolchain read no jar and load no build, so every service but the store dies if asked. */
  private def session(dir: Path) =
    stubbed[Resolve] ++ stubbed[Loader] ++ stubbed[ResolutionService] ++
      ZLayer.succeed(ContentStore.in(dir.resolve("store")))
end CallsTaskLawSpec
