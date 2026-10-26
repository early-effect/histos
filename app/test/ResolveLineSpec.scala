package histos.server

import histos.jdk.Sha256
import histos.load.{BuildNotes, LoadedBuild, Loader, ScannedBuild}
import histos.native.NativeOrigin
import histos.resolve.{
  Classpath,
  ClasspathScope,
  DependencyLock,
  Eviction,
  LockedArtifact,
  LockedCell,
  Resolve,
  ResolveCell,
  ResolveError,
  ResolvedArtifact,
  ToolRequest,
}
import histos.workspace.Workspace
import histos.{Dep, Jvm, Platform, ProjectName, Projects, ScalaVersion, module}

import java.nio.file.Path

import zio.{Chunk, ZIO, ZLayer}
import zio.test.*

object ResolveLineSpec extends ZIOSpecDefault:
  private val digest      = Sha256.of(Chunk.fromArray(Array[Byte](1, 2, 3)))
  private val kept        = Eviction("dev.histos", "base", "1.0.0", "1.1.0")
  private val support     = Eviction("dev.histos", "support", "1.0.0", "1.1.0")
  private val hidden      = Eviction("dev.histos", "hidden", "", "9.9.9")
  private val keptLine    = "evict dev.histos base 1.0.0 1.1.0"
  private val supportLine = "evict dev.histos support 1.0.0 1.1.0"

  def spec = suite("resolve lines")(
    test("a resolve check names the cell and the eviction"):
      ZIO.scoped:
        for
          root  <- Workspace.work("histos-resolve-line")
          graph <- ZIO.fromEither(opened)
          resolver = Solving(digest)
          loader   = Fixed(graph)
          layer    = ZLayer.succeed(loader) ++ ZLayer.succeed[Resolve](resolver)
          updated <- Calls
            .resolveWork(
              root,
              Path.of("api.jar"),
              ResolveAction.Update,
              NativeOrigin.Central,
            )
            .provide(layer)
          checked <- Calls
            .resolveWork(
              root,
              Path.of("api.jar"),
              ResolveAction.Check,
              NativeOrigin.Central,
            )
            .provide(layer)
        yield assertTrue(
          updated.contains("dependencies.lock"),
          updated.contains(keptLine),
          updated.contains(supportLine),
          checked.contains("core 3.10.0 jvm"),
          checked.contains(keptLine),
          checked.contains(supportLine),
          !checked.contains("hidden"),
          !updated.contains("hidden"),
        )
    ,
    test("an unprintable eviction is left out of the success text"):
      ZIO
        .fromEither(ProjectName.from("core"))
        .map: name =>
          val lock = DependencyLock(
            List(
              LockedCell(
                name,
                ScalaVersion("3.10.0"),
                Platform.Jvm,
                Nil,
                Nil,
                List(kept, hidden),
                ClasspathScope.Compile,
              )
            )
          )
          val text = DependencyLock.withEvictions("core 3.10.0 jvm", lock)
          assertTrue(
            text == s"core 3.10.0 jvm\n$keptLine",
            !text.contains("hidden"),
          ),
  )

  private def opened =
    Projects(module("core", Jvm)(ScalaVersion("3.10.0")))

  private final class Fixed(graph: histos.ProjectGraph) extends Loader:
    def scan(repo: Path) = ZIO.succeed(ScannedBuild(Nil, ""))
    def loadLocked(
        repo: Path,
        apiJar: Path,
        lock: DependencyLock,
    )                                  = ZIO.succeed(LoadedBuild(graph, Calls.quiet))
    def load(repo: Path, apiJar: Path) =
      ZIO.succeed(LoadedBuild(graph, Calls.quiet))
    def buildClasspath(apiJar: Path)          = ZIO.succeed(Nil)
    def reportBuild(repo: Path, apiJar: Path) =
      ZIO.succeed(BuildNotes(false, Nil))

  private final class Solving(sha: Sha256) extends Resolve:
    def resolveTool(request: ToolRequest) =
      val artifacts = request.roots.map: root =>
        ResolvedArtifact(
          root.organization,
          root.name,
          root.revision,
          Path.of("tool.jar"),
          sha,
          request.repositories.first,
          None,
        )
      ZIO.succeed(Classpath(artifacts, List(support)))
    def resolve(cell: ResolveCell) =
      ZIO.succeed(Classpath(Nil, List(kept)))
    def fetch(artifact: LockedArtifact) =
      ZIO.succeed(
        ResolvedArtifact(
          artifact.organization,
          artifact.name,
          artifact.revision,
          Path.of("tool.jar"),
          artifact.sha256,
          artifact.repository,
          artifact.classifier,
        )
      )
    def resolvePlugin(
        dependency: Dep,
        repositories: histos.resolve.Repositories,
    ) =
      ZIO.succeed(Classpath(Nil, Nil))
    def sources(
        organization: String,
        name: String,
        revision: String,
        repository: histos.resolve.Repository,
    ) = ZIO.fail(ResolveError.NotFound(organization, name, revision))
end ResolveLineSpec
