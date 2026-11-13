package histos.program

import histos.cache.ContentStore
import histos.doctor.Suggest
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Machine, Os, Sha256}
import histos.native.NativeOrigin
import histos.pack.MainClassError
import histos.resolve.{DependencyLock, ResolutionService, Resolve}
import histos.toolchain.{Architecture, RuntimeIdentity, Runtimes}
import histos.{
  Aggregate,
  MainClass,
  Module,
  Platform,
  Platforms,
  Project,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
}

import java.nio.file.Path

import slayer.{stub, stubbed}

import zio.{Ref, ZIO}
import zio.test.*

object ProgramConsoleLawSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion.trusted("3.10.0")
  private val repo   = Path.of("/repo")
  private val jdk    = JdkRuntime(
    Path.of("/jdk"),
    JdkVersion.trusted("21", JdkMajor.trusted(21)),
    RuntimeIdentity.Local(Path.of("/jdk"), Sha256.trusted("ab")),
  )
  private val machine = Machine(
    Map.empty,
    "/home",
    Os.Mac,
    Architecture.Aarch64,
    Path.of("/cache"),
    repo,
    None,
    NativeOrigin.Central,
  )

  private val core  = module("core", Platforms(Platform.Jvm, Nil), Some(MainClass.trusted("demo.Main")))
  private val both  = module("both", Platforms(Platform.Jvm, List(Platform.Js)), Some(MainClass.trusted("demo.Main")))
  private val bad   = module("bad", Platforms(Platform.Jvm, Nil), Some(MainClass.trusted("1bad")))
  private val plain = module("plain", Platforms(Platform.Jvm, Nil), None)
  private val all   = Aggregate(ProjectName.trusted("all"), List(core.name), SourcePos("histos.scala", 1, 1))
  private val projects: List[Project] = List(core, both, bad, plain, all)
  private val graph                   = ProjectGraph.trusted(projects)
  private val names                   = projects.map(_.name.value)

  def spec = suite("program console laws")(
    test("a missing name, an aggregate, no main, a bad main, and the wrong platform never ask the lock"):
      val program =
        for
          asked    <- Ref.make(0)
          _        <- stub[ResolutionService](_.current)(asked.update(_ + 1) *> ZIO.succeed(DependencyLock(Nil)))
          runtimes <- Runtimes.read(machine, None)
          missing  <- console(graph, "nope", None, runtimes)
          grouped  <- console(graph, "all", None, runtimes)
          noMain   <- console(graph, "plain", None, runtimes)
          badMain  <- console(graph, "bad", None, runtimes)
          choose   <- console(graph, "both", None, runtimes)
          unbuilt  <- console(graph, "core", Some(Platform.Js), runtimes)
          hits     <- asked.get
        yield assertTrue(
          missing == Left(ProgramError.Absent("nope", Suggest.runs("nope", names))),
          grouped == Left(ProgramError.Aggregate(all.name)),
          noMain == Left(ProgramError.NoMain(plain.name)),
          badMain == Left(ProgramError.Main(MainClassError.Bad("1bad"))),
          choose == Left(ProgramError.Choose(both.name, both.platforms.all)),
          unbuilt == Left(ProgramError.Unbuilt(core.name, Platform.Js, core.platforms.all)),
          hits == 0,
        )
      program.provideSomeLayer[TestEnvironment](services)
    ,
    test("installing a missing client or a client that is not native never asks the lock"):
      val client  = module("client", Platforms(Platform.Jvm, Nil), Some(MainClass.trusted("demo.Main")))
      val held    = ProjectGraph.trusted(projects :+ client)
      val program =
        for
          asked    <- Ref.make(0)
          _        <- stub[ResolutionService](_.current)(asked.update(_ + 1) *> ZIO.succeed(DependencyLock(Nil)))
          runtimes <- Runtimes.read(machine, None)
          absent   <- Program.install(repo, graph, jdk, runtimes, _ => ZIO.unit).either
          native   <- Program.install(repo, held, jdk, runtimes, _ => ZIO.unit).either
          hits     <- asked.get
        yield assertTrue(
          absent == Left(ProgramError.Absent(ClientBinary.moduleName, Suggest.runs(ClientBinary.moduleName, names))),
          native == Left(ProgramError.Unbuilt(client.name, Platform.Native, client.platforms.all)),
          hits == 0,
        )
      program.provideSomeLayer[TestEnvironment](services),
  )

  private val services =
    stubbed[ResolutionService] ++ stubbed[Resolve] ++ stubbed[ContentStore] ++ histos.schedule.RemoteCache.off

  private def console(
      graph: ProjectGraph,
      name: String,
      choice: Option[Platform],
      runtimes: Runtimes,
  ) =
    Program
      .console(repo, graph, name, jdk, runtimes, choice, _ => ZIO.unit, ZIO.unit)
      .runCollect
      .either

  private def module(name: String, platforms: Platforms, main: Option[MainClass]): Module =
    Module(
      ProjectName.trusted(name),
      platforms,
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("histos.scala", 1, 1),
      application = main,
    )
end ProgramConsoleLawSpec
