package histos.publish

import histos.cache.ContentStore
import histos.central.{DeploymentId, DeploymentState}
import histos.jdk.{JdkRuntime, JdkVersion, Sha256, Sha256Digest}
import histos.plugin.ApiDigest
import histos.resolve.Resolve
import histos.toolchain.RuntimeIdentity
import histos.version.{CiVersion, ShipName}
import histos.workspace.Workspace
import histos.{
  BuildError,
  HistosBuild,
  Jvm,
  MainClass,
  ProjectGraph,
  Projects,
  Publishing,
  Ship,
  SonaPublish,
  asset,
  module,
  organization,
  ship,
  ships,
}
import histos.`scala`

import java.nio.file.{Files, Path}

import slayer.stubbed

import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.*

object PublishRunLawSpec extends ZIOSpecDefault:
  private val hostApi = ApiDigest.trusted("ab" * 32)
  private val scala3  = scala"3.10.0"
  private val number  = ship"1.2.3"
  private val group   = organization"example.com"
  private val app     = module("app", Jvm)(scala3)
  private val packed  =
    module("app", Jvm)(scala3).assembly(MainClass("demo.Main")).asset(asset"dist/app.jar")

  private val stock = stubbed[Resolve] ++ stubbed[ContentStore]

  def spec = suite("publish run laws")(
    test("an unknown local token is noted and refused before anything is built"):
      ZIO
        .scoped {
          Workspace.work("histos-run-local").flatMap { repo =>
            for
              graph   <- load(Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(app)), app))
              runtime <- jdk
              notes   <- Ref.make(List.empty[String])
              found   <- PublishRun
                .local(
                  repo,
                  graph,
                  HistosBuild.fixed(graph),
                  List("missing"),
                  runtime,
                  Map.empty,
                  hostApi,
                  _ => ZIO.unit,
                  note(notes),
                )
                .provideSomeLayer[Resolve & ContentStore](ZLayer.succeed(IvyRoot.at(repo.resolve("ivy"))))
                .either
              said  <- notes.get
              wrote <- touched(repo)
            yield assertTrue(
              found == Left(LocalPublishError.Unknown("missing")),
              said == List(LocalPublishError.Unknown("missing").message),
              !wrote,
            )
          }
        }
        .provideSomeLayer[TestEnvironment & Scope](stock)
    ,
    test("sona snapshots and a release refuse missing credentials before a development session is staged"):
      ZIO
        .scoped {
          Workspace.work("histos-run-gate").flatMap { repo =>
            for
              graph   <- load(Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(app)), app))
              runtime <- jdk
              build = HistosBuild.fixed(graph)
              notes <- Ref.make(List.empty[String])
              snaps <- PublishRun
                .snapshots(
                  repo,
                  graph,
                  build,
                  Nil,
                  runtime,
                  Map.empty,
                  hostApi,
                  _ => ZIO.unit,
                  note(notes),
                )
                .either
              snapNotes <- notes.getAndUpdate(_ => Nil)
              released  <- PublishRun
                .release(
                  repo,
                  graph,
                  build,
                  Nil,
                  runtime,
                  Map.empty,
                  false,
                  hostApi,
                  _ => ZIO.unit,
                  note(notes),
                )
                .either
              releaseNotes <- notes.get
              wrote        <- touched(repo)
            yield assertTrue(
              snaps == Left(LocalPublishError.Credentials),
              snapNotes == List(LocalPublishError.Credentials.message),
              released == Left(ReleaseError.Credentials),
              releaseNotes == List(ReleaseError.Credentials.message),
              !wrote,
            )
          }
        }
        .provideSomeLayer[TestEnvironment & Scope](stock)
    ,
    test("a publisher that is not sona does not ask for credentials, and development is still not published"):
      ZIO
        .scoped {
          Workspace.work("histos-run-quiet").flatMap { repo =>
            val expected = LocalPublishError.Development(ShipName("demo"), CiVersion(number))
            val noCommit = LocalPublishError.NoCommit
            for
              graph   <- load(Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(app)), app))
              runtime <- jdk
              build = quiet(graph)
              notes <- Ref.make(List.empty[String])
              snaps <- PublishRun
                .snapshots(
                  repo,
                  graph,
                  build,
                  Nil,
                  runtime,
                  Map.empty,
                  hostApi,
                  _ => ZIO.unit,
                  note(notes),
                )
                .either
              snapNotes <- notes.getAndUpdate(_ => Nil)
              released  <- PublishRun
                .release(
                  repo,
                  graph,
                  build,
                  Nil,
                  runtime,
                  Map.empty,
                  false,
                  hostApi,
                  _ => ZIO.unit,
                  note(notes),
                )
                .either
              releaseNotes <- notes.get
              wrote        <- touched(repo)
            yield assertTrue(
              snaps == Left(noCommit),
              snapNotes == List(noCommit.message),
              released == Left(ReleaseError.Stage(expected)),
              releaseNotes.nonEmpty,
              releaseNotes.forall(_ == expected.message),
              !releaseNotes.exists(_.contains("SONATYPE")),
              !wrote,
            )
          }
        }
        .provideSomeLayer[TestEnvironment & Scope](stock)
    ,
    test("an asset token is refused only once a file is ready to upload"):
      ZIO.scoped {
        Workspace.work("histos-run-assets").flatMap { repo =>
          val assetPath = repo.resolve("dist").resolve("app.jar")
          for
            graph <- load(
              Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(packed)), packed)
            )
            id = DeploymentId("dep-1")
            quiet   <- PublishRun.assets(repo, graph, Nil, Map.empty)
            named   <- PublishRun.assets(repo, graph, List("demo"), Map.empty)
            _       <- DeploymentFile.write(repo, ShipName("demo"), id, DeploymentState.Validated, "9.9.9")
            version <- PublishRun.assets(repo, graph, List("demo"), Map.empty)
            _       <- DeploymentFile.write(repo, ShipName("demo"), id, DeploymentState.Validated, number.render)
            missing <- PublishRun.assets(repo, graph, List("demo"), Map.empty)
            _       <- Workspace.writeString(assetPath, "jar")
            present <- PublishRun.assets(repo, graph, List("demo"), Map.empty).either
            onDisk  <- ZIO.attemptBlocking(Files.isRegularFile(assetPath)).orDie
          yield assertTrue(
            quiet == AssetReport(Nil, Nil),
            named == AssetReport(List("demo has no recorded deployment"), Nil),
            version == AssetReport(List(s"demo deployment records 9.9.9, not ${number.render}"), Nil),
            missing == AssetReport(List("demo dist/app.jar is not a file"), Nil),
            present == Left(AssetError.Credentials),
            onDisk,
          )
        }
      }
    ,
    test("an unknown asset, a repeated ship, and a ship with no asset never ask for a token"):
      ZIO.scoped {
        Workspace.work("histos-run-asset-names").flatMap { repo =>
          for
            plain <- load(Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(app)), app))
            named <- load(
              Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(packed)), packed)
            )
            unknown   <- PublishRun.assets(repo, named, List("missing"), Map.empty).either
            duplicate <- PublishRun.assets(repo, named, List("demo", "demo"), Map.empty).either
            bare      <- PublishRun.assets(repo, plain, List("demo"), Map.empty)
            wrote     <- touched(repo)
          yield assertTrue(
            unknown == Left(AssetError.UnknownShip("missing")),
            unknown.fold(_.message == "ship 'missing' is not in the catalog", _ => false),
            duplicate == Left(AssetError.Duplicate("demo")),
            duplicate.fold(_.message == "ship 'demo' is named twice", _ => false),
            bare == AssetReport(List("demo has no asset"), Nil),
            !wrote,
          )
        }
      },
  )

  private def quiet(found: ProjectGraph): HistosBuild =
    val publishing =
      Publishing.fromEffect(_ => ZIO.die(new IllegalStateException("published")), secrets = Nil)
    new HistosBuild:
      def graph: Either[BuildError, ProjectGraph]                 = Right(found)
      override def sona: ZLayer[SonaPublish, Nothing, Publishing] =
        ZLayer.succeed(publishing)

  private def load(built: Either[BuildError, ProjectGraph]): ZIO[Any, Nothing, ProjectGraph] =
    ZIO.fromEither(built).orDieWith(blow)

  private def jdk: ZIO[Any, Nothing, JdkRuntime] =
    ZIO.fromEither(JdkVersion.from("21")).orDieWith(blow).map { version =>
      JdkRuntime(Path.of("jdk"), version, RuntimeIdentity.Local(Path.of("jdk"), Sha256Digest.of(Chunk.empty)))
    }

  private def note(notes: Ref[List[String]])(line: String): ZIO[Any, Nothing, Unit] =
    notes.update(_ :+ line)

  private def touched(repo: Path): ZIO[Any, Nothing, Boolean] =
    ZIO.attemptBlocking(Files.exists(repo.resolve(".histos"))).orDie

  private def blow(error: Any): IllegalStateException =
    new IllegalStateException(error.toString)
end PublishRunLawSpec
