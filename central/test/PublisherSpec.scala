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

  private val opened: Either[ArtifactIdError, Fixture] =
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

  private def use[E](f: Fixture => ZIO[Any, E, TestResult]): ZIO[Any, E | ArtifactIdError, TestResult] =
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
            checked <- readLocal(published)
            result = checked match
              case None => assertTrue(s"$published" == "Published.Local")
              case Some((pom, stored, sidecar, jar, sha256)) =>
                assertTrue(
                  jar.endsWith(Path.of("com", "example", "widget", "1.4.2", "widget-1.4.2.jar")),
                  sha256 == Checksum.sha256(Chunk.fromArray(stored)),
                  sidecar.stripLineEnd == sha256.value,
                  pom.contains("<version>1.4.2</version>"),
                  pom.contains("<groupId>com.example</groupId>"),
                  !pom.contains("-ci"),
                )
          yield result
    ,
    test("a stored snapshot and a manual version keep the text they were given"):
      use: fixture =>
        ZIO.scoped:
          for
            root <- directory
            snapshot <- send(root, fixture, Revision.Snapshot(SnapshotCoordinate(fixture.number, fixture.commit.abbreviate)), fixture.widget)
            stated <- send(root, fixture, Revision.Manual(fixture.manual), fixture.app)
          yield assertTrue(
            jarOf(snapshot).exists(_.toString.contains("widget-1.4.2-1234567890ab-SNAPSHOT.jar")),
            jarOf(stated).exists(_.toString.contains("app-1.2.3-SNAPSHOT.jar")),
          )
    ,
    test("a pointer is its own coordinate, not a commit"):
      use: fixture =>
        ZIO.scoped:
          for
            root <- directory
            published <- send(root, fixture, Revision.Pointer(SnapshotPointer(fixture.number)), fixture.widget)
          yield assertTrue(
            jarOf(published).exists(path => path.toString.contains("widget-1.4.2-SNAPSHOT.jar") && !path.toString.contains("1234567890ab")),
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
            pom <- pomOf(published)
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
            ArtifactId.from("..") == Left(ArtifactIdError.Bad("..")),
            DependencyVersion.from("../1") == Left(DependencyVersionError.Bad("../1")),
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

  private def jarOf(published: Published): Option[Path] =
    published match
      case Published.Local(jar, _, _) => Some(jar)
      case Published.Snapshot(_, _)   => None

  private def pomOf(published: Published): zio.Task[String] =
    published match
      case Published.Local(_, pom, _) => ZIO.attempt(Files.readString(pom))
      case Published.Snapshot(_, _)   => ZIO.succeed("")

  private def readLocal(published: Published): zio.Task[Option[(String, Array[Byte], String, Path, Checksum)]] =
    published match
      case Published.Local(jar, pom, sha256) =>
        for
          pomText <- ZIO.attempt(Files.readString(pom))
          stored  <- ZIO.attempt(Files.readAllBytes(jar))
          sidecar <- ZIO.attempt(Files.readString(jar.resolveSibling("widget-1.4.2.jar.sha256")))
        yield Some((pomText, stored, sidecar, jar, sha256))
      case Published.Snapshot(_, _) => ZIO.succeed(None)

  private def send(root: Path, fixture: Fixture, revision: Revision, name: ArtifactId) =
    ZIO.serviceWithZIO[Publisher](_.publish(Artifact(fixture.group, name, revision, bytes)))
      .provide(Publisher.local(root))

  private def sendBytes(root: Path, fixture: Fixture, body: Chunk[Byte]) =
    ZIO.serviceWithZIO[Publisher](
      _.publish(Artifact(fixture.group, fixture.widget, Revision.Release(ReleaseVersion(fixture.number)), body))
    ).provide(Publisher.local(root))

  def spec = publish
