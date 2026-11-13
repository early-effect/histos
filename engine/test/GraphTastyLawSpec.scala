package histos.zinc

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.native.NativeOrigin
import histos.resolve.{LockedTools, Repositories, ResolutionService, Resolve}
import histos.version.Catalog
import histos.workspace.Workspace
import histos.{
  CompilerClosure,
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
  ToolCatalog,
}

import java.nio.file.{Files, Path}

import zio.{Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** A module on an older Scala 3 series cannot take a newer upstream. The upstream still compiles. Before any upstream
  * cell holds TASTy, a cell reads the newest declared cell no newer than itself.
  */
object GraphTastyLawSpec extends ZIOSpecDefault:
  private val coreName = ProjectName.trusted("tastycore")
  private val appName  = ProjectName.trusted("tastyapp")

  def spec = suite("graph tasty laws")(
    test("an older series refuses a newer upstream after that upstream has classes"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        Workspace
          .work("histos-graph-tasty")
          .flatMap: dir =>
            val newer   = closure.scalaVersion
            val coreSrc =
              dir.resolve(coreName.value).resolve("src").resolve("main").resolve("scala").resolve("Core.scala")
            val appSrc = dir.resolve(appName.value).resolve("src").resolve("main").resolve("scala").resolve("App.scala")
            val coreClass = GraphCompile.classesDir(dir, coreName, newer, Platform.Jvm).resolve("Core.class")
            for
              older   <- closure.older
              loaded  <- Machine.load(dir).either
              machine <- loaded match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value)
              found <- GraphCompile.jdk(machine).either
              jdk   <- found match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value)
              _ <- Workspace.createDirectories(parentOf(coreSrc))
              _ <- Workspace.createDirectories(parentOf(appSrc))
              _ <- Workspace.writeString(coreSrc, "object Core:\n  def ready: Int = 1\n")
              _ <- Workspace.writeString(appSrc, "object App:\n  def n: Int = Core.ready\n")
              pinned = closure.pinningAt(coreName -> newer, appName -> older)
              compiled <- GraphCompile
                .run(
                  CompileSession(dir, graph(newer, older), jdk, NativeOrigin.read(machine.env), lock = pinned),
                  List(appName),
                  Some(Platform.Jvm),
                  _ => ZIO.unit,
                )
                .either
                .provideSomeLayer[Resolve & Scope](
                  ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++ ResolutionService.pinned(pinned)
                )
              wrote <- ZIO.attempt(Files.isRegularFile(coreClass)).orDie
            yield
              val detail = compiled.fold(_.message, _ => "")
              assertTrue(
                wrote,
                detail.contains(s"'tastyapp' ${older.value} cannot read 'tastycore' ${newer.value} jvm"),
              )
    ,
    test("before an upstream holds TASTy, a cell reads the newest declared cell no newer than itself"):
      for
        closure  <- ZIO.service[CompilerClosure]
        resolve  <- ZIO.service[Resolve]
        dir      <- Workspace.work("histos-graph-tasty-none")
        previous <- closure.previous
        newest = closure.scalaVersion
        tools  = ZLayer.succeed(LockedTools(closure.lock, ToolCatalog.default, Repositories.central, resolve))
        below  = module(coreName, ScalaVersions(previous, Nil), Nil)
        above  = module(coreName, ScalaVersions(newest, Nil), Nil)
        read <- UpstreamCells
          .read(
            module(appName, ScalaVersions(newest, Nil), List(coreName)),
            List(below),
            newest,
            Platform.Jvm,
            declared(dir, below),
          )
          .provide(tools)
        none <- UpstreamCells
          .read(
            module(appName, ScalaVersions(previous, Nil), List(coreName)),
            List(above),
            previous,
            Platform.Jvm,
            declared(dir, above),
          )
          .provide(tools)
          .either
      yield assertTrue(
        read == List(UpstreamCell(below, previous, GraphCompile.classesDir(dir, coreName, previous, Platform.Jvm))),
        none == Left(GraphCompileError.MissingUpstream(appName, coreName, previous, Platform.Jvm)),
      ),
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer) @@ TestAspect.withLiveSystem

  private def declared(dir: Path, upstream: Module): Map[UpstreamCells.Cell, Path] =
    UpstreamCells.declared(dir, List(upstream), Platform.Jvm)

  private def module(name: ProjectName, versions: ScalaVersions, upstream: List[ProjectName]): Module =
    Module(
      name,
      Platforms(Platform.Jvm, Nil),
      versions,
      Nil,
      upstream.map(ProjectDep(_, None)),
      SourcePos("law.scala", 1, 1),
    )

  private def graph(newer: ScalaVersion, older: ScalaVersion): ProjectGraph =
    val core = Module(
      coreName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(newer, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )
    val app = Module(
      appName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(older, Nil),
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

  private def parentOf(path: Path): Path =
    Option(path.getParent).getOrElse(path)
end GraphTastyLawSpec
