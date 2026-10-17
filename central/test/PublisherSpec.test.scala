package histos.central

import histos.version.{CommitSha, CommitShaError, ManualVersion, ManualVersionError, ReleaseVersion, ShipNumber, ShipNumberError, SnapshotCoordinate, SnapshotPointer}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Scope, ZIO}
import zio.test.*

object PublisherSpec extends ZIOSpecDefault:
  private val bytes = Chunk[Byte](0x50, 0x4b, 0x03, 0x04) ++ Chunk.fromArray("histos".getBytes)
  private val full = "1234567890abcdef1234567890abcdef12345678"

  private enum FixtureError:
    case Ship(error: ShipNumberError)
    case Group(error: PublishError)
    case Name(error: PublishError)
    case Commit(error: CommitShaError)
    case Version(error: ManualVersionError)

    def message: String = this match
      case Ship(error)    => error.message
      case Group(error)   => error.message
      case Name(error)    => error.message
      case Commit(error)  => error.message
      case Version(error) => error.message

  private final case class Fixture(
      number: ShipNumber,
      group: GroupId,
      widget: ArtifactId,
      app: ArtifactId,
      commit: CommitSha,
      manual: ManualVersion,
  )

  private val opened: Either[FixtureError, Fixture] =
    for
      number  <- ShipNumber.from("1.4.2").left.map(FixtureError.Ship(_))
      group   <- GroupId.from("com.example").left.map(FixtureError.Group(_))
      widget  <- ArtifactId.from("widget").left.map(FixtureError.Name(_))
      app     <- ArtifactId.from("app").left.map(FixtureError.Name(_))
      commit  <- CommitSha.from(full).left.map(FixtureError.Commit(_))
      manual  <- ManualVersion.from("1.2.3-SNAPSHOT").left.map(FixtureError.Version(_))
    yield Fixture(number, group, widget, app, commit, manual)

  private def use(f: Fixture => ZIO[Any, Throwable, TestResult]): ZIO[Any, Throwable, TestResult] =
    opened match
      case Right(fixture) => f(fixture)
      case Left(error)    => ZIO.succeed(assertTrue(error.message == "fixture"))

  private val publish = suite("local repository")(
    test("a release jar and its pom land under the group"):
      use: fixture =>
        ZIO.scoped:
          for
            root <- directory
            published <- required(send(root, fixture, Revision.Release(ReleaseVersion(fixture.number)), fixture.widget))
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
            snapshot <- required(send(root, fixture, Revision.Snapshot(SnapshotCoordinate(fixture.number, fixture.commit.abbreviate)), fixture.widget))
            stated <- required(send(root, fixture, Revision.Manual(fixture.manual), fixture.app))
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
            published <- required(send(root, fixture, Revision.Pointer(SnapshotPointer(fixture.number)), fixture.widget))
          yield assertTrue(
            published.jar.toString.contains("widget-1.4.2-SNAPSHOT.jar"),
            !published.jar.toString.contains("1234567890ab"),
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
            GroupId.from("com/../example") == Left(PublishError.BadGroup("com/../example")),
            GroupId.from("com..example") == Left(PublishError.BadGroup("com..example")),
            ArtifactId.from("..") == Left(PublishError.BadArtifact("..")),
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

  private def required(effect: ZIO[Any, PublishError, Published]): ZIO[Any, Throwable, Published] =
    effect.mapError(error => new IllegalStateException(error.message))

  private def send(root: Path, fixture: Fixture, revision: Revision, name: ArtifactId) =
    ZIO.serviceWithZIO[Publisher](_.publish(Artifact(fixture.group, name, revision, bytes)))
      .provide(Publisher.local(root))

  private def sendBytes(root: Path, fixture: Fixture, body: Chunk[Byte]) =
    ZIO.serviceWithZIO[Publisher](
      _.publish(Artifact(fixture.group, fixture.widget, Revision.Release(ReleaseVersion(fixture.number)), body))
    ).provide(Publisher.local(root))

  def spec = publish
