package histos.zinc

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, Resolve}
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

/** Asking for the downstream cell compiles its upstream first. An unknown cell never starts. */
object GraphCellLawSpec extends ZIOSpecDefault:
  private val scala3   = ScalaVersion("3.10.0")
  private val coreName = ProjectName.trusted("cellcore")
  private val appName  = ProjectName.trusted("cellapp")

  def spec = suite("graph cell laws")(
    test("the downstream cell sees the upstream classes compiled in the same feed"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
        Workspace
          .work("histos-graph-cell")
          .flatMap: dir =>
            val coreSrc = dir.resolve("cellcore").resolve("src").resolve("main").resolve("scala").resolve("Core.scala")
            val appSrc  = dir.resolve("cellapp").resolve("src").resolve("main").resolve("scala").resolve("App.scala")
            val missing = ProjectName.trusted("missing")
            for
              loaded  <- Machine.load(dir).either
              machine <- loaded match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value)
              found <- GraphCompile.jdk(machine).either
              jdk   <- found match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value)
              _       <- Workspace.writeString(dir.resolve("dependencies.lock"), DependencyLock.render(pinned(lock)))
              unknown <- GraphCompile
                .cellFeed(dir, graph, List(missing -> scala3), Platform.Jvm, jdk, machine.native)
                .runCollect
                .provideSomeLayer[Resolve](ZLayer.succeed(ContentStore.in(dir.resolve("store"))))
                .either
              _     <- Workspace.createDirectories(parentOf(coreSrc))
              _     <- Workspace.createDirectories(parentOf(appSrc))
              _     <- Workspace.writeString(coreSrc, "object Core:\n  def ready: Int = 1\n")
              _     <- Workspace.writeString(appSrc, "object App:\n  def n: Int = Core.ready\n")
              feeds <- GraphCompile
                .strict(
                  GraphCompile.cellFeed(dir, graph, List(appName -> scala3), Platform.Jvm, jdk, machine.native)
                )
                .runCollect
                .provideSomeLayer[Resolve](ZLayer.succeed(ContentStore.in(dir.resolve("store"))))
                .either
              coreClass = GraphCompile.classesDir(dir, coreName, scala3, Platform.Jvm).resolve("Core.class")
              appClass  = GraphCompile.classesDir(dir, appName, scala3, Platform.Jvm).resolve("App.class")
              coreThere <- ZIO.attempt(Files.isRegularFile(coreClass)).orDie
              appThere  <- ZIO.attempt(Files.isRegularFile(appClass)).orDie
            yield
              val began = feeds.toOption.fold(List.empty[String])(
                _.collect { case CompileFeed.Began(module, _, _) => module.value }.toList
              )
              val done = feeds.toOption.fold(List.empty[String])(
                _.collect { case CompileFeed.Done(cell) => cell.module.value }.toList.sorted
              )
              val coreAt = began.indexOf(coreName.value)
              val appAt  = began.indexOf(appName.value)
              assertTrue(
                unknown == Left(GraphCompileError.UnknownCell(missing, scala3)),
                feeds.isRight,
                done == List(appName.value, coreName.value),
                coreAt >= 0 && appAt >= 0 && coreAt < appAt,
                coreThere,
                appThere,
              )
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

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
end GraphCellLawSpec
