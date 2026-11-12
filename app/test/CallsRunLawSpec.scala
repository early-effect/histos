package histos.server

import histos.build.BuildEvent
import histos.cache.ContentStore
import histos.program.ProgramError
import histos.report.{BuildFault, CallResult}
import histos.resolve.{ResolutionService, Resolve}
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{
  CompilerClosure,
  LawMachine,
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
import slayer.stubbed

import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A named main prints and exits, and signals once its compile is done. A missing name and an unbuilt platform never
  * start and never signal.
  */
object CallsRunLawSpec extends ZIOSpecDefault:
  private val lawName = ProjectName.trusted("runlaw")

  def spec = suite("calls run laws")(
    test("a missing name and an unbuilt platform are faults that never start and never signal"):
      Workspace
        .work("histos-calls-refuse")
        .flatMap: dir =>
          val program =
            for
              machine <- LawMachine.at(dir)
              signals <- Ref.make(0)
              compiled = signals.update(_ + 1)
              law      = graph(uncompiled)
              (missed, missedHeard) <- speak(say =>
                Calls.run(dir, law, LawMachine.fixtureJdk, machine, "missing", None, say, compiled)
              )
              (unbuilt, unbuiltHeard) <- speak(say =>
                Calls.run(dir, law, LawMachine.fixtureJdk, machine, lawName.value, Some(Platform.Js), say, compiled)
              )
              unstarted <- signals.get
            yield assertTrue(
              missed == Right(()),
              done(missedHeard) == Some(CallResult.Fault(BuildFault.Program(ProgramError.Absent("missing")))),
              unbuilt == Right(()),
              done(unbuiltHeard) == Some(
                CallResult.Fault(
                  BuildFault.Program(ProgramError.Unbuilt(lawName, Platform.Js, List(Platform.Jvm)))
                )
              ),
              unstarted == 0,
            )
          program.provideSomeLayer[Scope](stubbed[Resolve] ++ stubbed[ContentStore] ++ stubbed[ResolutionService])
    ,
    test("a main prints and its exit is the report, and it signals once its compile is done"):
      ran.provideSomeLayer[Scope](CompilerClosure.layer),
  )

  private def ran =
    ZIO.serviceWithZIO[CompilerClosure]: closure =>
      val scala3 = closure.scalaVersion
      Workspace
        .work("histos-calls-run")
        .flatMap: dir =>
          val probe =
            dir.resolve(lawName.value).resolve("src").resolve("main").resolve("scala").resolve("Probe.scala")
          val program =
            for
              machine <- LawMachine.at(dir)
              jdk     <- LawMachine.jdk(machine)
              signals <- Ref.make(0)
              compiled = signals.update(_ + 1)
              _ <- Workspace.writeString(
                probe,
                "object Probe:\n  def main(args: Array[String]): Unit = println(\"ran-law\")\n",
              )
              (ran, heard) <- speak(say =>
                Calls.run(dir, graph(scala3), jdk, machine, lawName.value, None, say, compiled)
              )
              started <- signals.get
              clazz = GraphCompile.classesDir(dir, lawName, scala3, Platform.Jvm).resolve("Probe.class")
              wrote <- ZIO.attempt(Files.isRegularFile(clazz)).orDie
              _     <- Workspace.writeString(
                probe,
                "object Probe:\n  def main(args: Array[String]): Unit =\n    println(\"ran-law\")\n    System.exit(4)\n",
              )
              (stopped, stoppedHeard) <- speak(say =>
                Calls.run(dir, graph(scala3), jdk, machine, lawName.value, None, say, compiled)
              )
            yield assertTrue(
              ran == Right(()),
              programOf(done(heard)) == Some(0),
              outs(heard).contains(s"runlaw ${scala3.value} jvm"),
              outs(heard).contains("ran-law"),
              started == 1,
              wrote,
              stopped == Right(()),
              programOf(done(stoppedHeard)) == Some(4),
              outs(stoppedHeard).contains("ran-law"),
            )
          program.provideSomeLayer[Resolve & Scope](session(dir, closure))

  /** The refusals never compile, so the cell's Scala version is never read from a lock. */
  private val uncompiled = ScalaVersion("3.10.0")

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

  private def graph(scala3: ScalaVersion): ProjectGraph =
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

  private def session(dir: Path, closure: CompilerClosure) =
    ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++
      ResolutionService.pinned(closure.pinning(lawName))
end CallsRunLawSpec
