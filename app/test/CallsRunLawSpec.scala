package histos.server

import histos.build.BuildEvent
import histos.cache.ContentStore
import histos.jdk.Machine
import histos.report.CallResult
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, ResolutionService, Resolve}
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{
  MainClass,
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

/** A named main prints and exits, and signals once its compile is done. A missing name and an unbuilt platform never
  * start and never signal.
  */
object CallsRunLawSpec extends ZIOSpecDefault:
  private val scala3  = ScalaVersion("3.10.0")
  private val lawName = ProjectName.trusted("runlaw")

  def spec = suite("calls run laws")(
    test("a main prints and its exit is the report, and a missing platform is a fault"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
        Workspace
          .work("histos-calls-run")
          .flatMap: dir =>
            val probe =
              dir.resolve(lawName.value).resolve("src").resolve("main").resolve("scala").resolve("Probe.scala")
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
                signals <- Ref.make(0)
                compiled = signals.update(_ + 1)
                (missed, missedHeard) <- speak(say =>
                  Calls.run(dir, graph, jdk, machine, "missing", None, say, compiled)
                )
                (unbuilt, unbuiltHeard) <- speak(say =>
                  Calls.run(dir, graph, jdk, machine, lawName.value, Some(Platform.Js), say, compiled)
                )
                unstarted <- signals.get
                _         <- Workspace.createDirectories(parentOf(probe))
                _         <- Workspace.writeString(
                  probe,
                  "object Probe:\n  def main(args: Array[String]): Unit = println(\"ran-law\")\n",
                )
                (ran, heard) <- speak(say => Calls.run(dir, graph, jdk, machine, lawName.value, None, say, compiled))
                started      <- signals.get
                clazz = GraphCompile.classesDir(dir, lawName, scala3, Platform.Jvm).resolve("Probe.class")
                wrote <- ZIO.attempt(Files.isRegularFile(clazz)).orDie
                _     <- Workspace.writeString(
                  probe,
                  "object Probe:\n  def main(args: Array[String]): Unit =\n    println(\"ran-law\")\n    System.exit(4)\n",
                )
                (stopped, stoppedHeard) <- speak(say =>
                  Calls.run(dir, graph, jdk, machine, lawName.value, None, say, compiled)
                )
              yield
                val text        = outs(heard)
                val stoppedText = outs(stoppedHeard)
                assertTrue(
                  missed == Right(()),
                  done(missedHeard).exists(faultHas("'missing' is not a module")),
                  unbuilt == Right(()),
                  done(unbuiltHeard).exists(faultHas("does not build js")),
                  unstarted == 0,
                  started == 1,
                  ran == Right(()),
                  programOf(done(heard)) == Some(0),
                  text.contains("runlaw 3.10.0 jvm"),
                  text.contains("ran-law"),
                  wrote,
                  stopped == Right(()),
                  programOf(done(stoppedHeard)) == Some(4),
                  stoppedText.contains("ran-law"),
                )
            program.provideSomeLayer[Resolve & Scope](session(dir, lock))
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private def faultHas(text: String)(result: CallResult): Boolean =
    result match
      case CallResult.Fault(fault) => fault.message.contains(text)
      case _                       => false

  private def programOf(result: Option[CallResult]): Option[Int] =
    result match
      case Some(CallResult.Reported(report)) => report.program
      case _                                 => None

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
      application = Some(MainClass("Probe")),
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
    ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++
      ResolutionService.pinned(pinned(lock))

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

  private def parentOf(path: Path): Path =
    Option(path.getParent).getOrElse(path)
end CallsRunLawSpec
