package histos.server

import histos.build.BuildEvent
import histos.cache.ContentStore
import histos.jdk.Sha256Digest
import histos.report.{BuildFault, CallResult, Rejection}
import histos.resolve.{ResolutionService, Resolve}
import histos.version.Catalog
import histos.workspace.Workspace
import histos.{
  CompilerClosure,
  LawMachine,
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
import slayer.stubbed

import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** Assembly writes `dist/histos.jar`, its checksum, and the compile tally. A destination that is not a path is refused.
  * What the jar holds is `AssembleLawSpec`'s.
  */
object CallsAssemblyLawSpec extends ZIOSpecDefault:
  private val api = ProjectName.trusted("api")
  private val app = ProjectName.trusted("app")

  def spec = suite("calls assembly laws")(
    test("a destination that is not a path is a fault before anything compiles"):
      Workspace
        .work("histos-calls-refuse")
        .flatMap: dir =>
          val program =
            for
              machine           <- LawMachine.at(dir)
              (refused, report) <- speak(say =>
                Calls.assembly(dir, graph(ScalaVersion("3.10.0")), LawMachine.fixtureJdk, machine, Some("\u0000"), say)
              )
            yield assertTrue(
              refused == Right(()),
              done(report) == Some(CallResult.Fault(BuildFault.Rejected(Rejection.AssemblyDestination("\u0000")))),
            )
          program.provideSomeLayer[Scope](stubbed[Resolve] ++ stubbed[ContentStore] ++ stubbed[ResolutionService])
    ,
    test("the call writes the jar's checksum next to the build and reports both modules"):
      assembled.provideSomeLayer[Scope](CompilerClosure.layer),
  )

  private def assembled =
    ZIO.serviceWithZIO[CompilerClosure]: closure =>
      Workspace
        .work("histos-calls-assembly")
        .flatMap: dir =>
          val jar     = dir.resolve("dist").resolve("histos.jar")
          val pin     = dir.resolve(".histos").resolve("assembly.sha256")
          val program =
            for
              machine <- LawMachine.at(dir)
              jdk     <- LawMachine.jdk(machine)
              _       <- Workspace.writeString(
                dir.resolve("api").resolve("src").resolve("main").resolve("scala").resolve("Api.scala"),
                "object ApiLib:\n  def ready: Int = 1\n",
              )
              _ <- Workspace.writeString(
                dir.resolve("app").resolve("src").resolve("main").resolve("scala").resolve("Main.scala"),
                "package histos.cli\nobject Main:\n  def main(args: Array[String]): Unit = ()\n",
              )
              (written, heard) <- speak(say =>
                Calls.assembly(dir, graph(closure.scalaVersion), jdk, machine, None, say)
              )
              digest <- ZIO.attempt(Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(jar))).value).orDie
              pinned <- ZIO.attempt(Files.readString(pin)).orDie
            yield assertTrue(
              written == Right(()),
              modulesOf(done(heard)) == List("api", "app"),
              outs(heard) == "files compiled: 2 skipped: 0; modules: 2; classes: 4\ndist/histos.jar",
              pinned == s"$digest\n",
            )
          program.provideSomeLayer[Resolve & Scope](session(dir, closure))

  private def modulesOf(result: Option[CallResult]): List[String] =
    result match
      case Some(CallResult.Reported(report)) =>
        report.observation.toList.flatMap(_.modules.map(_.module).toList).sorted
      case _ => Nil

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
    val apiModule = Module(
      api,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("api.scala", 1, 1),
    )
    val appModule = Module(
      app,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      List(ProjectDep(api, None)),
      SourcePos("app.scala", 1, 1),
    )
    ProjectGraph.trusted(
      List(apiModule, appModule),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def session(dir: Path, closure: CompilerClosure) =
    ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++
      ResolutionService.pinned(closure.pinning(api, app))
end CallsAssemblyLawSpec
