package histos.resolve

import histos.jdk.Sha256
import histos.native.{NativeClasspath, NativeRequests, NativeToolchain}
import histos.{Dep, DepKind, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, Ref, Scope, ZIO, ZLayer}
import zio.test.*

object ResolutionServiceSpec extends ZIOSpecDefault:
  private val version = ScalaVersion("3.9.0")
  private val digest  = Sha256.of(Chunk.fromArray(Array[Byte](1, 2, 3)))

  def spec = suite("resolution service")(
    test("a missing module is solved once, and a reload fetches the report"):
      ZIO.scoped:
        for
          root <- temp
          jar = root.resolve("lib.jar")
          _       <- ZIO.attempt(Files.write(jar, Array[Byte](1, 2, 3)))
          solves  <- Ref.make(0)
          fetches <- Ref.make(0)
          name    <- ZIO.fromEither(ProjectName.from("core"))
          cell = ResolveCell(
            name,
            Nil,
            version,
            Platform.Jvm,
            Repositories.central,
          )
          report <- ResolutionService
            .open(root, histos.Resolution.Solve)
            .provide(ZLayer.succeed(new Counting(solves, fetches, jar)))
          _      <- report.module(cell)
          firstS <- solves.get
          firstF <- fetches.get
          _      <- report.module(cell)
          heldS  <- solves.get
          heldF  <- fetches.get
          _      <- report.adopt(ProjectGraph.trusted(Nil))
          _      <- report.module(cell)
          againS <- solves.get
          againF <- fetches.get
        yield assertTrue(
          firstS == 1 && firstF == 0,
          heldS == 1 && heldF == 0,
          againS == 1 && againF == 1,
        )
    ,
    test("a locked miss does not fetch"):
      val request = ToolRequest(
        ToolId(ToolRole.Compiler, version, ToolVariant.Jvm),
        Nil,
        Repositories.central,
      )
      for
        solves  <- Ref.make(0)
        fetches <- Ref.make(0)
        out     <- ZIO
          .scoped(
            ResolutionService
              .pinned(DependencyLock(Nil))
              .build
              .flatMap(_.get[ResolutionService].tool(request))
          )
          .provide(ZLayer.succeed(new Counting(solves, fetches, Path.of("absent.jar"))))
          .either
        solved  <- solves.get
        fetched <- fetches.get
      yield assertTrue(
        out == Left(ResolveError.NoTool(request.id)),
        solved == 0,
        fetched == 0,
      )
    ,
    test("a pin and a solve of that pin hand a native module the same libraries"):
      ZIO.scoped:
        for
          root   <- temp
          solves <- Ref.make(0)
          moduleVersion = ScalaVersion("3.10.0")
          toolchain     = NativeToolchain.Release
          request       = NativeRequests.libraries(moduleVersion, toolchain, Repositories.central)
          name <- ZIO.fromEither(ProjectName.from("api"))
          dep   = Dep.trusted("dev.zio", "zio", "2.1.27", DepKind.Binary)
          asked = ResolveCell(
            name,
            List(dep),
            moduleVersion,
            Platform.Native,
            Repositories.central,
            ClasspathScope.Test,
          )
          library = LockedArtifact(
            "dev.zio",
            "zio_native0.5_3",
            "2.1.27",
            digest,
            Repository.Central,
          )
          lock = DependencyLock(
            List(
              LockedCell(
                name,
                moduleVersion,
                Platform.Native,
                List(LockedDirect("dev.zio", "zio", "2.1.27", DepKind.Binary)),
                List(library),
                Nil,
                ClasspathScope.Test,
              )
            ),
            Nil,
            List(
              LockedTool(
                request.id,
                request.roots,
                Repositories.ordered(request.repositories),
                request.roots.map(item =>
                  LockedArtifact(
                    item.organization,
                    item.name,
                    item.revision,
                    digest,
                    Repository.Central,
                  )
                ),
                Nil,
              )
            ),
          )
          fetching = new Fetching(root, solves)
          _ <- ZIO.attempt {
            val text = DependencyLock.render(lock)
            Files.createDirectories(root.resolve(".histos"))
            Files.writeString(Resolve.lockPath(root), text)
            Files.writeString(ResolutionService.reportPath(root), text)
          }
          graph = ProjectGraph.trusted(Nil)
          pinned <- ResolutionService
            .open(root, histos.Resolution.Lock)
            .provide(ZLayer.succeed(fetching))
          solved <- ResolutionService
            .open(root, histos.Resolution.Solve)
            .provide(ZLayer.succeed(fetching))
          pinTools <- LockedTools
            .make(lock, graph, pinned)
            .provide(ZLayer.succeed(fetching))
          solveTools <- LockedTools
            .make(lock, graph, solved)
            .provide(ZLayer.succeed(fetching))
          pinLibs <- NativeClasspath
            .fetch(toolchain, Some(moduleVersion))
            .provide(ZLayer.succeed(pinTools))
          solveLibs <- NativeClasspath
            .fetch(toolchain, Some(moduleVersion))
            .provide(ZLayer.succeed(solveTools))
          pinCell     <- pinTools.module(asked)
          solveCell   <- solveTools.module(asked)
          solvedCount <- solves.get
        yield assertTrue(
          pinLibs.plugin == solveLibs.plugin,
          pinLibs.libraries == solveLibs.libraries,
          pinLibs.libraries.map(path => String.valueOf(path.getFileName)).exists(_.contains("3.10.0")),
          coordinates(pinCell) == coordinates(solveCell),
          solvedCount == 0,
        ),
  )

  private def coordinates(classpath: Classpath): List[(String, String, String, String)] =
    classpath.artifacts.map(artifact =>
      (artifact.organization, artifact.name, artifact.revision, artifact.path.toString)
    )

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(
      ZIO.attempt(Files.createTempDirectory("histos-resolution"))
    )(dir =>
      ZIO.attempt {
        val walk  = Files.walk(dir)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists(_))
      }.ignore
    )

  private final class Counting(solves: Ref[Int], fetches: Ref[Int], jar: Path) extends Resolve:
    def resolveTool(request: ToolRequest): IO[ResolveError, Classpath] =
      solves.update(_ + 1) *> ZIO.fail(ResolveError.NoTool(request.id))

    def resolve(cell: ResolveCell): IO[ResolveError, Classpath] =
      solves.update(_ + 1).as(Classpath(List(artifact), Nil))

    def fetch(ignored: LockedArtifact): IO[ResolveError, ResolvedArtifact] =
      fetches.update(_ + 1).as(artifact)

    def resolvePlugin(
        dependency: histos.Dep,
        repositories: Repositories,
    ): IO[ResolveError, Classpath] =
      ZIO.fail(ResolveError.NoLock(Path.of("plugin")))

    def sources(
        organization: String,
        name: String,
        revision: String,
        repository: Repository,
    ): IO[ResolveError, ResolvedArtifact] =
      ZIO.fail(ResolveError.NotFound(organization, name, revision))

    private def artifact: ResolvedArtifact =
      ResolvedArtifact("dev.fixture", "lib", "1", jar, digest, Repository.Central)
  end Counting

  /** Fetch returns the locked coordinate. A solve is a failure, so a mode that re-resolves does not look equivalent. */
  private final class Fetching(root: Path, solves: Ref[Int]) extends Resolve:
    def resolveTool(request: ToolRequest): IO[ResolveError, Classpath] =
      solves.update(_ + 1) *> ZIO.fail(ResolveError.NoTool(request.id))

    def resolve(cell: ResolveCell): IO[ResolveError, Classpath] =
      solves.update(_ + 1) *> ZIO.fail(
        ResolveError.LockDrift(cell.module, cell.scalaVersion, cell.platform, cell.scope)
      )

    def fetch(held: LockedArtifact): IO[ResolveError, ResolvedArtifact] =
      val path = root.resolve(s"${held.name}-${held.revision}.jar")
      ZIO
        .attempt(Files.write(path, Array[Byte](1)))
        .orDie
        .as(
          ResolvedArtifact(
            held.organization,
            held.name,
            held.revision,
            path,
            held.sha256,
            held.repository,
            held.classifier,
          )
        )

    def resolvePlugin(
        dependency: histos.Dep,
        repositories: Repositories,
    ): IO[ResolveError, Classpath] =
      ZIO.fail(ResolveError.NoLock(root))

    def sources(
        organization: String,
        name: String,
        revision: String,
        repository: Repository,
    ): IO[ResolveError, ResolvedArtifact] =
      ZIO.fail(ResolveError.NotFound(organization, name, revision))
  end Fetching
end ResolutionServiceSpec
