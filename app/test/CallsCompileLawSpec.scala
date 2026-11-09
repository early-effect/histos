package histos.server

import histos.build.BuildEvent
import histos.cache.ContentStore
import histos.report.ModuleTally
import histos.jdk.Machine
import histos.report.CallResult
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, ResolutionService, Resolve}
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompileMemo, Compilers, GraphCompile}
import histos.{
  Module,
  Platform,
  Platforms,
  ProjectDep,
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

/** Compile through the call reports the cells, and an unknown name is a fault. */
object CallsCompileLawSpec extends ZIOSpecDefault:
  private val scala3   = ScalaVersion("3.10.0")
  private val coreName = ProjectName.trusted("callcore")
  private val appName  = ProjectName.trusted("callapp")

  def spec = suite("calls compile laws")(
    test("the downstream call compiles its upstream, and a missing name is a fault"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
        Workspace
          .work("histos-calls-compile")
          .flatMap: dir =>
            val coreSrc = dir.resolve("callcore").resolve("src").resolve("main").resolve("scala").resolve("Core.scala")
            val appSrc  = dir.resolve("callapp").resolve("src").resolve("main").resolve("scala").resolve("App.scala")
            val program =
              for
                loaded  <- Machine.load(dir).either
                machine <- loaded match
                  case Left(error)  => ZIO.dieMessage(error.message)
                  case Right(value) => ZIO.succeed(value)
                found <- GraphCompile.jdk(machine).either
                jdk   <- found match
                  case Left(error)  => ZIO.dieMessage(error.message)
                  case Right(value) => ZIO.succeed(value)
                (missed, missedHeard) <- speak(say =>
                  Calls.compile(dir, graph, List(ProjectName.trusted("missing")), jdk, machine, say)
                )
                _                 <- Workspace.createDirectories(parentOf(coreSrc))
                _                 <- Workspace.createDirectories(parentOf(appSrc))
                _                 <- Workspace.writeString(coreSrc, "object Core:\n  def ready: Int = 1\n")
                _                 <- Workspace.writeString(appSrc, "object App:\n  def n: Int = Core.ready\n")
                (compiled, heard) <- speak(say => Calls.compile(dir, graph, List(appName), jdk, machine, say))
                coreClass = GraphCompile.classesDir(dir, coreName, scala3, Platform.Jvm).resolve("Core.class")
                appClass  = GraphCompile.classesDir(dir, appName, scala3, Platform.Jvm).resolve("App.class")
                coreThere <- ZIO.attempt(Files.isRegularFile(coreClass)).orDie
                appThere  <- ZIO.attempt(Files.isRegularFile(appClass)).orDie
              yield
                val text    = outs(heard)
                val tallies = modulesOf(done(heard))
                assertTrue(
                  missed == Right(()),
                  done(missedHeard).exists(faultNamesMissing),
                  compiled == Right(()),
                  tallies.map(_.module).sorted == List(appName.value, coreName.value),
                  tallies
                    .forall(row => row.compiled.contains(1) && row.classes.contains(2) && row.version == scala3.value),
                  text == "files compiled: 2 skipped: 0; modules: 2; classes: 4",
                  coreThere,
                  appThere,
                )
            program.provideSomeLayer[Resolve & Scope](session(dir, lock))
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private def faultNamesMissing(result: CallResult): Boolean =
    result match
      case CallResult.Fault(fault) => fault.message.contains("missing")
      case _                       => false

  private def modulesOf(result: Option[CallResult]): List[ModuleTally] =
    result match
      case Some(CallResult.Reported(report)) => report.observation.fold(List.empty[ModuleTally])(_.modules.toList)
      case _                                 => Nil

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
    val core = Module(
      coreName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )
    val app = Module(
      appName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      List(ProjectDep(coreName, None)),
      SourcePos("law.scala", 2, 1),
    )
    ProjectGraph.trusted(
      List(core, app),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def session(dir: Path, lock: DependencyLock) =
    ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++
      Compilers.live(1) ++
      CompileMemo.live ++
      ResolutionService.pinned(pinned(lock))

  private def pinned(lock: DependencyLock): DependencyLock =
    val cells = List(coreName, appName).map: name =>
      LockedCell(name, scala3, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile)
    lock.copy(cells = cells ++ lock.cells)

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

  private def parentOf(path: Path): Path =
    Option(path.getParent).getOrElse(path)
end CallsCompileLawSpec
