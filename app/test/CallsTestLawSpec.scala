package histos.server

import histos.build.BuildEvent
import histos.cli.TestLogs
import histos.cache.ContentStore
import histos.jdk.Machine
import histos.report.CallResult
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, ResolutionService, Resolve}
import histos.testrun.{SuitePattern, TestCompile, TestMode, TestStatus, TestedCell}
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompileMemo, Compilers, GraphCompile}
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
  TestMain,
}

import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** A named test runs its main. A name that is not a project is a fault, and a pattern that matches nothing says so. */
object CallsTestLawSpec extends ZIOSpecDefault:
  private val scala3  = ScalaVersion("3.10.0")
  private val lawName = ProjectName.trusted("testlaw")

  def spec = suite("calls test laws")(
    test("a test main passes, a missing name is a fault, and a pattern that misses says so"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
        Workspace
          .work("histos-calls-test")
          .flatMap: dir =>
            val main    = dir.resolve("testlaw").resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val probe   = dir.resolve("testlaw").resolve("src").resolve("test").resolve("scala").resolve("Probe.scala")
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
                pattern <- SuitePattern.from("Nope") match
                  case Right(value) => ZIO.succeed(value)
                  case Left(error)  => ZIO.dieMessage(error.message)
                (missed, missedHeard) <- speak(say =>
                  Calls.test(dir, graph, List(ProjectName.trusted("missing")), Nil, TestMode.Run, jdk, machine, say)
                )
                _ <- Workspace.createDirectories(parentOf(main))
                _ <- Workspace.createDirectories(parentOf(probe))
                _ <- Workspace.writeString(main, "object Hello:\n  def ready: Int = 1\n")
                _ <- Workspace.writeString(probe, "object Probe:\n  def main(args: Array[String]): Unit = ()\n")
                (ran, heard) <- speak(say =>
                  Calls.test(dir, graph, List(lawName), Nil, TestMode.Run, jdk, machine, say)
                )
                (skipped, skippedHeard) <- speak(say =>
                  Calls.test(dir, graph, List(lawName), List(pattern), TestMode.Run, jdk, machine, say)
                )
                clazz = TestCompile.testClasses(dir, lawName, scala3, Platform.Jvm).resolve("Probe.class")
                there <- ZIO.attempt(Files.isRegularFile(clazz)).orDie
                log = TestLogs.file(dir, lawName, scala3, Platform.Jvm)
                logged <- ZIO.attempt(if Files.isRegularFile(log) then Files.readString(log) else "").orDie
              yield
                val cells = testsOf(done(heard))
                assertTrue(
                  missed == Right(()),
                  done(missedHeard).exists(faultNamesMissing),
                  ran == Right(()),
                  cells == List(TestedCell(lawName, scala3, Platform.Jvm, TestStatus.Passed)),
                  outs(heard).contains("tests passed: 1 failed: 0 cached: 0 modules: 1"),
                  there,
                  logged.contains("testlaw 3.10.0 jvm passed"),
                  skipped == Right(()),
                  done(skippedHeard).exists(reportsUnmatched),
                )
            program.provideSomeLayer[Resolve & Scope](session(dir, lock))
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private def faultNamesMissing(result: CallResult): Boolean =
    result match
      case CallResult.Fault(fault) => fault.message.contains("missing")
      case _                       => false

  private def reportsUnmatched(result: CallResult): Boolean =
    result.message.contains("no tests matched Nope")

  private def testsOf(result: Option[CallResult]): List[TestedCell] =
    result match
      case Some(CallResult.Reported(report)) => report.tests.toList
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
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
      test = Some(TestMain("Probe")),
    )
    ProjectGraph.trusted(
      List(law),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      testLog = true,
      resolution = Resolution.Lock,
    )

  private def session(dir: Path, lock: DependencyLock) =
    ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++
      Compilers.live(1) ++
      CompileMemo.live ++
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
end CallsTestLawSpec
