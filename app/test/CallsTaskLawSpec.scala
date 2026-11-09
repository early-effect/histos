package histos.server

import histos.build.BuildEvent
import histos.cache.ContentStore
import histos.jdk.Machine
import histos.load.{Loader, PluginRuntime}
import histos.plugin.{FlagName, PluginArgs, PluginTaskId}
import histos.report.CallResult
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, ResolutionService, Resolve}
import histos.version.Catalog
import histos.workspace.Workspace
import histos.{
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

import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** Built-in tasks name the projects and refuse arguments. Toolchain names the JDK and skips tools the graph does not
  * need.
  */
object CallsTaskLawSpec extends ZIOSpecDefault:
  private val scala3  = ScalaVersion("3.10.0")
  private val lawName = ProjectName.trusted("tasklaw")

  def spec = suite("calls task laws")(
    test("inspect names the project, versions has no catalog, and toolchain skips node and clang"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
        Workspace
          .work("histos-calls-task")
          .flatMap: dir =>
            val program =
              for
                loaded  <- Machine.load(dir).either
                machine <- loaded match
                  case Left(error)  => ZIO.dieMessage(error.message)
                  case Right(value) => ZIO.succeed(value)
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
                  done(blankHeard).exists(faultHas("a task id is not empty")),
                  shaped == Right(()),
                  done(shapedHeard).exists(faultHas("'nope' is not a plugin task id")),
                  extra == Right(()),
                  done(extraHeard).exists(faultHas("run inspect does not take arguments")),
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
                  toolText.contains("from "),
                )
            program.provideSomeLayer[Resolve & Scope](session(dir, lock))
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private val idle: PluginRuntime = new PluginRuntime:
    def run(id: PluginTaskId, args: PluginArgs) =
      ZIO.dieMessage("plugin was not asked")
    def bind =
      ZIO.dieMessage("plugin was not asked")
    def links = Nil

  private def faultHas(text: String)(result: CallResult): Boolean =
    result match
      case CallResult.Fault(fault) => fault.message.contains(text)
      case _                       => false

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
      ScalaVersions(scala3, Nil),
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

  private def session(dir: Path, lock: DependencyLock) =
    val store   = ZLayer.succeed(ContentStore.in(dir.resolve("store")))
    val resolve = Resolve.offline(_root_.coursier.cache.CacheDefaults.location.toPath)
    (resolve ++ store) >+> Loader.live ++ store ++ ResolutionService.pinned(pinned(lock))

  private def pinned(lock: DependencyLock): DependencyLock =
    lock.copy(cells = LockedCell(lawName, scala3, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile) :: lock.cells)

  private def opened =
    Resolve.offline(_root_.coursier.cache.CacheDefaults.location.toPath) ++
      ZLayer.fromZIO(Resolve.readLock(repository))

  private def repository: Path =
    def walk(path: Path, left: Int): Option[Path] =
      if left == 0 then None
      else if Files.isRegularFile(path.resolve("dependencies.lock")) && Files.isRegularFile(
          path.resolve("histos.scala")
        )
      then Some(path)
      else
        Option(path.getParent) match
          case Some(parent) => walk(parent, left - 1)
          case None         => None
    walk(Path.of("").toAbsolutePath.normalize, 8).getOrElse(Path.of("").toAbsolutePath.normalize)
end CallsTaskLawSpec
