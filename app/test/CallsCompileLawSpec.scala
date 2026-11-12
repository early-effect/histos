package histos.server

import histos.build.BuildEvent
import histos.cache.ContentStore
import histos.report.{BuildFault, CallResult, ModuleTally}
import histos.resolve.{ResolutionService, Resolve}
import histos.select.SelectError
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompileMemo, Compilers, GraphCompile, GraphCompileError}
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

import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** Compile through the call reports the cells, and an unknown name is a fault. */
object CallsCompileLawSpec extends ZIOSpecDefault:
  private val coreName = ProjectName.trusted("callcore")
  private val appName  = ProjectName.trusted("callapp")
  private val missing  = ProjectName.trusted("missing")

  def spec = suite("calls compile laws")(
    test("the downstream call compiles its upstream, and a missing name is a fault"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val scala3 = closure.scalaVersion
        Workspace
          .work("histos-calls-compile")
          .flatMap: dir =>
            val coreSrc = dir.resolve("callcore").resolve("src").resolve("main").resolve("scala").resolve("Core.scala")
            val appSrc  = dir.resolve("callapp").resolve("src").resolve("main").resolve("scala").resolve("App.scala")
            val program =
              for
                machine               <- LawMachine.at(dir)
                jdk                   <- LawMachine.jdk(machine)
                (missed, missedHeard) <- speak(say =>
                  Calls.compile(dir, graph(scala3), List(missing), jdk, machine, say)
                )
                _                 <- Workspace.writeString(coreSrc, "object Core:\n  def ready: Int = 1\n")
                _                 <- Workspace.writeString(appSrc, "object App:\n  def n: Int = Core.ready\n")
                (compiled, heard) <- speak(say => Calls.compile(dir, graph(scala3), List(appName), jdk, machine, say))
                coreClass = GraphCompile.classesDir(dir, coreName, scala3, Platform.Jvm).resolve("Core.class")
                appClass  = GraphCompile.classesDir(dir, appName, scala3, Platform.Jvm).resolve("App.class")
                coreThere <- ZIO.attempt(Files.isRegularFile(coreClass)).orDie
                appThere  <- ZIO.attempt(Files.isRegularFile(appClass)).orDie
              yield
                val tallies = modulesOf(done(heard))
                assertTrue(
                  missed == Right(()),
                  done(missedHeard) == Some(
                    CallResult.Fault(BuildFault.Compile(GraphCompileError.Target(SelectError.Unknown(missing, Nil))))
                  ),
                  compiled == Right(()),
                  tallies.map(_.module).sorted == List(appName.value, coreName.value),
                  tallies
                    .forall(row => row.compiled.contains(1) && row.classes.contains(2) && row.version == scala3.value),
                  outs(heard) == "files compiled: 2 skipped: 0; modules: 2; classes: 4",
                  coreThere,
                  appThere,
                )
            program.provideSomeLayer[Resolve & Scope](session(dir, closure))
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer)

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

  private def graph(scala3: ScalaVersion): ProjectGraph =
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

  private def session(dir: Path, closure: CompilerClosure) =
    ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++
      Compilers.live(1) ++
      CompileMemo.live ++
      ResolutionService.pinned(closure.pinning(coreName, appName))
end CallsCompileLawSpec
