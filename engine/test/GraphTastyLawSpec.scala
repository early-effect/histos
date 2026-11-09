package histos.zinc

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.native.NativeOrigin
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, ResolutionService, Resolve}
import histos.version.Catalog
import histos.workspace.Workspace
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

import zio.{Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** A Scala 3.9 module cannot take a 3.10 upstream. The upstream still compiles. */
object GraphTastyLawSpec extends ZIOSpecDefault:
  private val scala310 = ScalaVersion("3.10.0")
  private val scala39  = ScalaVersion("3.9.0")
  private val coreName = ProjectName.trusted("tastycore")
  private val appName  = ProjectName.trusted("tastyapp")

  def spec = suite("graph tasty laws")(
    test("3.9 refuses a 3.10 upstream after that upstream has classes"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
        Workspace
          .work("histos-graph-tasty")
          .flatMap: dir =>
            val coreSrc =
              dir.resolve(coreName.value).resolve("src").resolve("main").resolve("scala").resolve("Core.scala")
            val appSrc = dir.resolve(appName.value).resolve("src").resolve("main").resolve("scala").resolve("App.scala")
            val coreClass = GraphCompile.classesDir(dir, coreName, scala310, Platform.Jvm).resolve("Core.class")
            val program   =
              for
                loaded  <- Machine.load(dir).either
                machine <- loaded match
                  case Left(error)  => ZIO.dieMessage(error.message)
                  case Right(value) => ZIO.succeed(value)
                found <- GraphCompile.jdk(machine).either
                jdk   <- found match
                  case Left(error)  => ZIO.dieMessage(error.message)
                  case Right(value) => ZIO.succeed(value)
                _        <- Workspace.createDirectories(parentOf(coreSrc))
                _        <- Workspace.createDirectories(parentOf(appSrc))
                _        <- Workspace.writeString(coreSrc, "object Core:\n  def ready: Int = 1\n")
                _        <- Workspace.writeString(appSrc, "object App:\n  def n: Int = Core.ready\n")
                compiled <- GraphCompile
                  .run(
                    CompileSession(dir, graph, jdk, NativeOrigin.read(machine.env), lock = pinned(lock)),
                    List(appName),
                    Some(Platform.Jvm),
                    _ => ZIO.unit,
                  )
                  .either
                wrote <- ZIO.attempt(Files.isRegularFile(coreClass)).orDie
              yield
                val detail = compiled.fold(_.message, _ => "")
                assertTrue(
                  wrote,
                  detail.contains("'tastyapp' 3.9.0 cannot read 'tastycore' 3.10.0 jvm"),
                )
            program.provideSomeLayer[Resolve & Scope](session(dir, lock))
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private def graph: ProjectGraph =
    val core = Module(
      coreName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala310, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )
    val app = Module(
      appName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala39, Nil),
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
      ResolutionService.pinned(pinned(lock))

  private def pinned(lock: DependencyLock): DependencyLock =
    val added = List(
      LockedCell(coreName, scala310, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile),
      LockedCell(appName, scala39, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile),
    )
    lock.copy(cells = added ++ lock.cells)

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
end GraphTastyLawSpec
