package histos.central

import histos.version.{CommitSha, ManualVersion, ReleaseVersion, ShipNumber, SnapshotCoordinate, SnapshotPointer}
import histos.Organization

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Scope, ZIO}
import zio.test.*

object PublisherSpec extends histos.SuiteClock:
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
            root      <- directory
            published <- send(root, fixture, Revision.Release(ReleaseVersion(fixture.number)), fixture.widget)
            checked   <- readLocal(published)
            result = checked match
              case None                                      => assertTrue(s"$published" == "Published.Local")
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
            root     <- directory
            snapshot <- send(
              root,
              fixture,
              Revision.Snapshot(SnapshotCoordinate(fixture.number, fixture.commit.abbreviate)),
              fixture.widget,
            )
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
            root      <- directory
            published <- send(root, fixture, Revision.Pointer(SnapshotPointer(fixture.number)), fixture.widget)
          yield assertTrue(
            jarOf(published).exists(path =>
              path.toString.contains("widget-1.4.2-SNAPSHOT.jar") && !path.toString.contains("1234567890ab")
            )
          )
    ,
    test("a dependency is written into the pom"):
      use: fixture =>
        ZIO.scoped:
          for
            root    <- directory
            name    <- ZIO.fromEither(ArtifactId.from("zio_3"))
            version <- ZIO.fromEither(DependencyVersion.from("2.1.27"))
            dependency = MavenDependency(Organization("dev.zio"), name, version)
            published <- ZIO
              .serviceWithZIO[Publisher](
                _.publish(
                  Artifact(
                    fixture.group,
                    fixture.widget,
                    Revision.Release(ReleaseVersion(fixture.number)),
                    bytes,
                    List(dependency),
                  )
                )
              )
              .provide(Publisher.local(root))
            pom <- pomOf(published)
          yield assertTrue(
            pom.contains("<artifactId>zio_3</artifactId>"),
            pom.contains("<groupId>dev.zio</groupId>"),
            pom.contains("<version>2.1.27</version>"),
          )
    ,
    test("a bare jar gets three checksums and no sources, javadoc, or signature"):
      use: fixture =>
        ZIO.scoped:
          for
            root      <- directory
            published <- send(root, fixture, Revision.Release(ReleaseVersion(fixture.number)), fixture.widget)
            jar       <- localJar(published)
            names     <- listed(jar)
            jarBytes  <- ZIO.attempt(Files.readAllBytes(jar))
            pom       <- ZIO.attempt(Files.readAllBytes(jar.resolveSibling("widget-1.4.2.pom")))
            jarSums   <- summed(jar, jarBytes)
            pomSums   <- summed(jar.resolveSibling("widget-1.4.2.pom"), pom)
            (jarMd5, jarMd5Expected, jarSha1, jarSha1Expected, jarSha256, jarSha256Expected) = jarSums
            (pomMd5, pomMd5Expected, pomSha1, pomSha1Expected, pomSha256, pomSha256Expected) = pomSums
          yield assertTrue(
            names == List(
              "widget-1.4.2.jar",
              "widget-1.4.2.jar.md5",
              "widget-1.4.2.jar.sha1",
              "widget-1.4.2.jar.sha256",
              "widget-1.4.2.pom",
              "widget-1.4.2.pom.md5",
              "widget-1.4.2.pom.sha1",
              "widget-1.4.2.pom.sha256",
            ),
            !names.exists(_.endsWith(".asc")),
            jarMd5 == jarMd5Expected,
            jarSha1 == jarSha1Expected,
            jarSha256 == jarSha256Expected,
            pomMd5 == pomMd5Expected,
            pomSha1 == pomSha1Expected,
            pomSha256 == pomSha256Expected,
          )
    ,
    test("sources and javadoc land beside the jar with the same checksums"):
      use: fixture =>
        ZIO.scoped:
          for
            root      <- directory
            published <- ZIO
              .serviceWithZIO[Publisher](
                _.publish(
                  Artifact(
                    fixture.group,
                    fixture.widget,
                    Revision.Release(ReleaseVersion(fixture.number)),
                    bytes,
                    sources = Some(bytes),
                    javadoc = Some(bytes),
                  )
                )
              )
              .provide(Publisher.local(root))
            jar     <- localJar(published)
            names   <- listed(jar)
            stored  <- ZIO.attempt(Files.readAllBytes(jar.resolveSibling("widget-1.4.2-sources.jar")))
            javadoc <- ZIO.attempt(Files.readAllBytes(jar.resolveSibling("widget-1.4.2-javadoc.jar")))
            sums    <- summed(jar.resolveSibling("widget-1.4.2-sources.jar"), stored)
            docSums <- summed(jar.resolveSibling("widget-1.4.2-javadoc.jar"), javadoc)
            (srcMd5, srcMd5Expected, srcSha1, srcSha1Expected, srcSha256, srcSha256Expected) = sums
            (docMd5, docMd5Expected, docSha1, docSha1Expected, docSha256, docSha256Expected) = docSums
          yield assertTrue(
            names == List(
              "widget-1.4.2-javadoc.jar",
              "widget-1.4.2-javadoc.jar.md5",
              "widget-1.4.2-javadoc.jar.sha1",
              "widget-1.4.2-javadoc.jar.sha256",
              "widget-1.4.2-sources.jar",
              "widget-1.4.2-sources.jar.md5",
              "widget-1.4.2-sources.jar.sha1",
              "widget-1.4.2-sources.jar.sha256",
              "widget-1.4.2.jar",
              "widget-1.4.2.jar.md5",
              "widget-1.4.2.jar.sha1",
              "widget-1.4.2.jar.sha256",
              "widget-1.4.2.pom",
              "widget-1.4.2.pom.md5",
              "widget-1.4.2.pom.sha1",
              "widget-1.4.2.pom.sha256",
            ),
            !names.exists(_.endsWith(".asc")),
            stored.sameElements(bytes.toArray),
            javadoc.sameElements(bytes.toArray),
            srcMd5 == srcMd5Expected,
            srcSha1 == srcSha1Expected,
            srcSha256 == srcSha256Expected,
            docMd5 == docMd5Expected,
            docSha1 == docSha1Expected,
            docSha256 == docSha256Expected,
          )
    ,
    test("sources that are not a jar are refused before a write"):
      use: fixture =>
        ZIO.scoped:
          for
            root    <- directory
            refused <- ZIO
              .serviceWithZIO[Publisher](
                _.publish(
                  Artifact(
                    fixture.group,
                    fixture.widget,
                    Revision.Release(ReleaseVersion(fixture.number)),
                    bytes,
                    sources = Some(Chunk.fromArray("not a jar".getBytes)),
                  )
                )
              )
              .provide(Publisher.local(root))
              .either
            listedNames <- ZIO.attempt(Option(root.toFile.list()).map(_.toList).getOrElse(Nil))
          yield assertTrue(
            refused == Left(PublishError.NotAJar),
            listedNames.isEmpty,
          )
    ,
    test("an empty file, plain text, and an escaping name are refused before a write"):
      use: fixture =>
        ZIO.scoped:
          for
            root   <- directory
            empty  <- sendBytes(root, fixture, Chunk.empty).either
            text   <- sendBytes(root, fixture, Chunk.fromArray("not a jar".getBytes)).either
            listed <- ZIO.attempt(Option(root.toFile.list()).map(_.toList).getOrElse(Nil))
          yield assertTrue(
            empty == Left(PublishError.EmptyJar),
            text == Left(PublishError.NotAJar),
            ArtifactId.from("..") == Left(ArtifactIdError.Bad("..")),
            DependencyVersion.from("../1") == Left(DependencyVersionError.Bad("../1")),
            listed.isEmpty,
          ),
  )

  private def directory: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-central"))): path =>
      ZIO.attempt {
        if Files.exists(path) then
          val walk  = Files.walk(path)
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

  private def localJar(published: Published): zio.UIO[Path] =
    published match
      case Published.Local(jar, _, _) => ZIO.succeed(jar)
      case Published.Snapshot(_, _)   => ZIO.dieMessage("local publish returned a snapshot")

  private def listed(jar: Path): zio.Task[List[String]] =
    ZIO.attempt:
      val parent = Option(jar.getParent).getOrElse(jar)
      Option(parent.toFile.list()).map(_.toList.sorted).getOrElse(Nil)

  private def summed(path: Path, body: Array[Byte]): zio.Task[(String, String, String, String, String, String)] =
    ZIO.attempt:
      val bytes                        = Chunk.fromArray(body)
      def side(suffix: String): String =
        val name   = Option(path.getFileName).map(_.toString).getOrElse("artifact")
        val parent = Option(path.getParent).getOrElse(path)
        Files.readString(parent.resolve(s"$name.$suffix")).stripLineEnd
      (side("md5"), Digest.md5(bytes), side("sha1"), Digest.sha1(bytes), side("sha256"), Digest.sha256(bytes))

  private def send(root: Path, fixture: Fixture, revision: Revision, name: ArtifactId) =
    ZIO
      .serviceWithZIO[Publisher](_.publish(Artifact(fixture.group, name, revision, bytes)))
      .provide(Publisher.local(root))

  private def sendBytes(root: Path, fixture: Fixture, body: Chunk[Byte]) =
    ZIO
      .serviceWithZIO[Publisher](
        _.publish(Artifact(fixture.group, fixture.widget, Revision.Release(ReleaseVersion(fixture.number)), body))
      )
      .provide(Publisher.local(root))

  def spec = publish
end PublisherSpec
