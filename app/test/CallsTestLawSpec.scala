package histos.server

import histos.build.BuildEvent
import histos.cli.TestLogs
import histos.cache.ContentStore
import histos.report.{BuildFault, CallResult}
import histos.resolve.{ResolutionService, Resolve}
import histos.select.SelectError
import histos.testrun.{SuitePattern, TestCompile, TestMode, TestStatus, TestedCell}
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompileMemo, Compilers}
import histos.{
  CompilerClosure,
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
  TestMain,
}

import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** A named test runs its main. A name that is not a project is a fault, and a pattern that matches nothing says so. */
object CallsTestLawSpec extends ZIOSpecDefault:
  private val lawName = ProjectName.trusted("testlaw")
  private val missing = ProjectName.trusted("missing")

  def spec = suite("calls test laws")(
    test("a test main passes, a missing name is a fault, and a pattern that misses says so"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val scala3 = closure.scalaVersion
        Workspace
          .work("histos-calls-test")
          .flatMap: dir =>
            val main    = dir.resolve("testlaw").resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val probe   = dir.resolve("testlaw").resolve("src").resolve("test").resolve("scala").resolve("Probe.scala")
            val program =
              for
                machine               <- LawMachine.at(dir)
                jdk                   <- LawMachine.jdk(machine)
                (missed, missedHeard) <- speak(say =>
                  Calls.test(dir, graph(scala3), List(missing), Nil, TestMode.Run, jdk, machine, say)
                )
                _ <- Workspace.writeString(main, "object Hello:\n  def ready: Int = 1\n")
                _ <- Workspace.writeString(probe, "object Probe:\n  def main(args: Array[String]): Unit = ()\n")
                (ran, heard) <- speak(say =>
                  Calls.test(dir, graph(scala3), List(lawName), Nil, TestMode.Run, jdk, machine, say)
                )
                (skipped, skippedHeard) <- speak(say =>
                  Calls.test(
                    dir,
                    graph(scala3),
                    List(lawName),
                    List(SuitePattern("Nope")),
                    TestMode.Run,
                    jdk,
                    machine,
                    say,
                  )
                )
                clazz = TestCompile.testClasses(dir, lawName, scala3, Platform.Jvm).resolve("Probe.class")
                there <- ZIO.attempt(Files.isRegularFile(clazz)).orDie
                log = TestLogs.file(dir, lawName, scala3, Platform.Jvm)
                logged <- ZIO.attempt(if Files.isRegularFile(log) then Files.readString(log) else "").orDie
              yield assertTrue(
                missed == Right(()),
                done(missedHeard) == Some(CallResult.Fault(BuildFault.Select(SelectError.Unknown(missing)))),
                ran == Right(()),
                testsOf(done(heard)) == List(TestedCell(lawName, scala3, Platform.Jvm, TestStatus.Passed)),
                outs(heard).contains("tests passed: 1 failed: 0 cached: 0 modules: 1"),
                there,
                logged.contains(s"testlaw ${scala3.value} jvm passed"),
                skipped == Right(()),
                done(skippedHeard).exists(_.message.contains("no tests matched Nope")),
              )
            program.provideSomeLayer[Resolve & Scope](session(dir, closure))
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer)

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

  private def graph(scala3: ScalaVersion): ProjectGraph =
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

  private def session(dir: Path, closure: CompilerClosure) =
    ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++
      Compilers.live(1) ++
      CompileMemo.live ++
      ResolutionService.pinned(closure.pinning(lawName))
end CallsTestLawSpec
