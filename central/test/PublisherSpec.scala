package histos.central

import histos.version.{CommitSha, ManualVersion, ReleaseVersion, ShipNumber, SnapshotCoordinate, SnapshotPointer}
import histos.Organization

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Scope, ZIO}
import zio.test.*

object PublisherSpec extends ZIOSpecDefault:
  private val bytes = Chunk[Byte](0x50, 0x4b, 0x03, 0x04) ++ Chunk.fromArray("histos".getBytes)

  private final case class Fixture(
      number: ShipNumber,
      group: Organization,
      widget: ArtifactId,
      app: ArtifactId,
      commit: CommitSha,
      manual: ManualVersion,
  )

  private val opened: Either[PublishError, Fixture] =
    for
      widget <- ArtifactId.from("widget")
      app    <- ArtifactId.from("app")
    yield Fixture(
      ShipNumber("1.4.2"),
      Organization("com.example"),
      widget,
      app,
      CommitSha("1234567890abcdef1234567890abcdef12345678"),
      ManualVersion("1.2.3-SNAPSHOT"),
    )

  private def use[E](f: Fixture => ZIO[Any, E, TestResult]): ZIO[Any, E | PublishError, TestResult] =
    opened match
      case Right(fixture) => f(fixture)
      case Left(error)    => ZIO.fail(error)

  private val publish = suite("local repository")(
    test("a release jar and its pom land under the group"):
      use: fixture =>
        ZIO.scoped:
          for
            root <- directory
            published <- send(root, fixture, Revision.Release(ReleaseVersion(fixture.number)), fixture.widget)
            pom <- ZIO.attempt(Files.readString(published.pom))
            stored <- ZIO.attempt(Files.readAllBytes(published.jar))
            sidecar <- ZIO.attempt(Files.readString(published.jar.resolveSibling("widget-1.4.2.jar.sha256")))
          yield assertTrue(
            published.jar.endsWith(Path.of("com", "example", "widget", "1.4.2", "widget-1.4.2.jar")),
            published.jarSha256 == Digest.sha256(Chunk.fromArray(stored)),
            sidecar.stripLineEnd == published.jarSha256,
            pom.contains("<version>1.4.2</version>"),
            pom.contains("<groupId>com.example</groupId>"),
            !pom.contains("-ci"),
          )
    ,
    test("a stored snapshot and a manual version keep the text they were given"):
      use: fixture =>
        ZIO.scoped:
          for
            root <- directory
            snapshot <- send(root, fixture, Revision.Snapshot(SnapshotCoordinate(fixture.number, fixture.commit.abbreviate)), fixture.widget)
            stated <- send(root, fixture, Revision.Manual(fixture.manual), fixture.app)
          yield assertTrue(
            snapshot.jar.toString.contains("widget-1.4.2-1234567890ab-SNAPSHOT.jar"),
            stated.jar.toString.contains("app-1.2.3-SNAPSHOT.jar"),
          )
    ,
    test("a pointer is its own coordinate, not a commit"):
      use: fixture =>
        ZIO.scoped:
          for
            root <- directory
            published <- send(root, fixture, Revision.Pointer(SnapshotPointer(fixture.number)), fixture.widget)
          yield assertTrue(
            published.jar.toString.contains("widget-1.4.2-SNAPSHOT.jar"),
            !published.jar.toString.contains("1234567890ab"),
          )
    ,
    test("a dependency is written into the pom"):
      use: fixture =>
        ZIO.scoped:
          for
            root <- directory
            name <- ZIO.fromEither(ArtifactId.from("zio_3"))
            version <- ZIO.fromEither(DependencyVersion.from("2.1.27"))
            dependency = MavenDependency(Organization("dev.zio"), name, version)
            published <- ZIO.serviceWithZIO[Publisher](
                           _.publish(Artifact(fixture.group, fixture.widget, Revision.Release(ReleaseVersion(fixture.number)), bytes, List(dependency)))
                         ).provide(Publisher.local(root))
            pom <- ZIO.attempt(Files.readString(published.pom))
          yield assertTrue(
            pom.contains("<artifactId>zio_3</artifactId>"),
            pom.contains("<groupId>dev.zio</groupId>"),
            pom.contains("<version>2.1.27</version>"),
          )
    ,
    test("an empty file, plain text, and an escaping name are refused before a write"):
      use: fixture =>
        ZIO.scoped:
          for
            root <- directory
            empty <- sendBytes(root, fixture, Chunk.empty).either
            text <- sendBytes(root, fixture, Chunk.fromArray("not a jar".getBytes)).either
            listed <- ZIO.attempt(Option(root.toFile.list()).map(_.toList).getOrElse(Nil))
          yield assertTrue(
            empty == Left(PublishError.EmptyJar),
            text == Left(PublishError.NotAJar),
            ArtifactId.from("..") == Left(PublishError.BadArtifact("..")),
            DependencyVersion.from("../1") == Left(PublishError.BadVersion("../1")),
            listed.isEmpty,
          )
    ,
  )

  private def directory: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-central"))): path =>
      ZIO.attempt {
        if Files.exists(path) then
          val walk = Files.walk(path)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(item => Files.deleteIfExists(item))
      }.ignore

  private def send(root: Path, fixture: Fixture, revision: Revision, name: ArtifactId) =
    ZIO.serviceWithZIO[Publisher](_.publish(Artifact(fixture.group, name, revision, bytes)))
      .provide(Publisher.local(root))

  private def sendBytes(root: Path, fixture: Fixture, body: Chunk[Byte]) =
    ZIO.serviceWithZIO[Publisher](
      _.publish(Artifact(fixture.group, fixture.widget, Revision.Release(ReleaseVersion(fixture.number)), body))
    ).provide(Publisher.local(root))

  def spec = publish
