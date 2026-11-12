package histos.central

import histos.version.{CiVersion, ReleaseVersion, ShipNumber, SnapshotPointer}
import histos.{
  ArtifactBase,
  DepExclusion,
  Developer,
  DeveloperId,
  HttpUrl,
  License,
  LicenseName,
  MavenScope,
  Organization,
  PomText,
  Scm,
  ScmConnection,
}

import zio.Chunk
import zio.test.*

object LayoutLawSpec extends ZIOSpecDefault:
  private val ship = ShipNumber("1.0.0")
  private val pk   = Chunk[Byte](0x50.toByte, 0x4b.toByte, 3.toByte, 4.toByte)

  def spec = suite("layout")(
    test("a jar is empty, not a zip, or a zip"):
      assertTrue(
        JarCheck.accept(Chunk.empty) == Left(PublishError.EmptyJar),
        JarCheck.accept(Chunk.single(0x50.toByte)) == Left(PublishError.NotAJar),
        JarCheck.accept(Chunk[Byte](1, 2, 3)) == Left(PublishError.NotAJar),
        JarCheck.accept(pk) == Right(pk),
      )
    ,
    test("paths follow the group, the artifact, and the rendered revision"):
      val root = java.nio.file.Path.of("repo")
      built(Revision.Release(ReleaseVersion(ship)), Organization("demo.group")) match
        case Left(_)         => assertTrue(false)
        case Right(artifact) =>
          Layout.paths(root, artifact) match
            case Left(_)      => assertTrue(false)
            case Right(paths) =>
              val base     = root.toAbsolutePath.normalize
              val expected = List("demo", "group", "widget", "1.0.0").foldLeft(base)(_.resolve(_))
              assertTrue(
                paths.directory.normalize == expected,
                paths.jar == expected.resolve("widget-1.0.0.jar"),
                paths.pom == expected.resolve("widget-1.0.0.pom"),
                paths.sources.isEmpty,
                paths.javadoc.isEmpty,
              )
    ,
    test("sources and javadoc paths appear only when the artifact carries them"):
      val root = java.nio.file.Path.of("repo")
      withExtras match
        case Left(_)         => assertTrue(false)
        case Right(artifact) =>
          Layout.paths(root, artifact) match
            case Left(_)      => assertTrue(false)
            case Right(paths) =>
              val directory = paths.directory
              assertTrue(
                paths.sources.contains(directory.resolve("widget-1.0.0-SNAPSHOT-sources.jar")),
                paths.javadoc.contains(directory.resolve("widget-1.0.0-SNAPSHOT-javadoc.jar")),
              )
    ,
    test("a pom escapes xml, writes distribution repo, and copies the scm connection"):
      describedArtifact match
        case None           => assertTrue(false)
        case Some(artifact) =>
          val pom = Layout.pom(artifact)
          assertTrue(
            pom.contains("<groupId>a&quot;&amp;b</groupId>"),
            !pom.contains("<groupId>a\"&b</groupId>"),
            pom.contains("<description>rocks &amp; rolls</description>"),
            pom.contains("<distribution>repo</distribution>"),
            pom.contains("<connection>scm:git:https://example.com/demo.git</connection>"),
            pom.contains("<developerConnection>scm:git:https://example.com/demo.git</developerConnection>"),
            pom.contains("<id>ada</id>"),
            pom.contains("<dependencies>"),
            pom.contains("<version>1.2.3</version>"),
          )
    ,
    test("an exclusion is the published artifact id, and an empty list writes no exclusions block"):
      val rendered =
        for
          name    <- ArtifactId.from("widget")
          kept    <- ArtifactId.from("widget_3")
          plain   <- ArtifactId.from("lib")
          version <- DependencyVersion.from("1.0.0")
        yield
          val exclusion =
            DepExclusion(Organization.trusted("org.codehaus.plexus"), ArtifactBase.trusted("plexus-archiver"))
          val revision = Revision.Release(ReleaseVersion(ship))
          val marked   = Artifact(
            Organization("demo"),
            name,
            revision,
            pk,
            dependencies = List(MavenDependency(Organization("org.example"), kept, version, List(exclusion))),
          )
          val open = Artifact(
            Organization("demo"),
            name,
            revision,
            pk,
            dependencies = List(MavenDependency(Organization("org.example"), plain, version)),
          )
          (Layout.pom(marked), Layout.pom(open))
      rendered match
        case Left(_)               => assertTrue(false)
        case Right((marked, open)) =>
          assertTrue(
            marked.contains("<artifactId>widget_3</artifactId>"),
            marked.contains("<groupId>org.codehaus.plexus</groupId>"),
            marked.contains("<artifactId>plexus-archiver</artifactId>"),
            marked.contains("<exclusions>"),
            !marked.contains("plexus-archiver_3"),
            open.contains("<artifactId>lib</artifactId>"),
            !open.contains("<exclusions>"),
          )
    ,
    test("a provided dependency writes scope provided, and a compile dependency writes no scope"):
      val rendered =
        for
          name    <- ArtifactId.from("servlet")
          version <- DependencyVersion.from("4.0.1")
        yield
          val revision = Revision.Release(ReleaseVersion(ship))
          val supplied = Artifact(
            Organization("demo"),
            name,
            revision,
            pk,
            dependencies = List(
              MavenDependency(Organization("javax.servlet"), name, version, scope = MavenScope.Provided)
            ),
          )
          val ordinary = Artifact(
            Organization("demo"),
            name,
            revision,
            pk,
            dependencies = List(MavenDependency(Organization("javax.servlet"), name, version)),
          )
          (Layout.pom(supplied), Layout.pom(ordinary))
      rendered match
        case Left(_)                     => assertTrue(false)
        case Right((supplied, ordinary)) =>
          assertTrue(
            supplied.contains("<scope>provided</scope>"),
            !ordinary.contains("<scope>"),
          )
    ,
    test("an empty dependency list omits the dependencies wrapper"):
      built(Revision.Ci(CiVersion(ship)), Organization("demo")) match
        case Left(_)         => assertTrue(false)
        case Right(artifact) =>
          val pom = Layout.pom(artifact)
          assertTrue(
            pom.contains("<version>1.0.0-ci</version>"),
            !pom.contains("<dependencies>"),
            pom.contains("<name>widget</name>"),
          )
    ,
    test("snapshot metadata keeps a quote and escapes an ampersand"):
      val rendered = ArtifactId
        .from("widget")
        .map: name =>
          SnapshotMetadata.render(
            SnapshotDocument(
              Organization.trusted("a&b"),
              name,
              SnapshotRevision.trusted("1.0.0-SNAPSHOT"),
              SnapshotStamp.from(java.time.Instant.EPOCH),
              2,
              SnapshotVersion(SnapshotBase.trusted("1.0.0"), SnapshotStamp.from(java.time.Instant.EPOCH), 2),
              SnapshotUpdated.from(java.time.Instant.EPOCH),
            )
          )
      rendered match
        case Left(_)    => assertTrue(false)
        case Right(xml) =>
          assertTrue(
            xml.contains("<groupId>a&amp;b</groupId>"),
            !xml.contains("<groupId>a&b</groupId>"),
            xml.contains("<buildNumber>2</buildNumber>"),
            xml.contains("<timestamp>19700101.000000</timestamp>"),
            xml.contains("<lastUpdated>19700101000000</lastUpdated>"),
          )
    ,
    test("a snapshot name is the text before -SNAPSHOT, and the first build number wins"):
      assertTrue(
        SnapshotMetadata.parse("1.0.0-SNAPSHOT").map(_.base.value) == Right("1.0.0"),
        SnapshotMetadata.parse("1.0.0-abcdefabcdef-SNAPSHOT").map(_.revision.value) == Right(
          "1.0.0-abcdefabcdef-SNAPSHOT"
        ),
        SnapshotMetadata.parse("1.0.0") == Left(SnapshotRevisionError.NotSnapshot("1.0.0")),
        SnapshotMetadata.parse("1.0.0-ci") == Left(SnapshotRevisionError.NotSnapshot("1.0.0-ci")),
        SnapshotMetadata.parse("-SNAPSHOT") == Left(SnapshotRevisionError.NotSnapshot("-SNAPSHOT")),
        SnapshotMetadata.parse("1..0-SNAPSHOT") == Left(SnapshotRevisionError.NotSnapshot("1..0-SNAPSHOT")),
        SnapshotMetadata.buildNumber("<buildNumber>2</buildNumber><buildNumber>9</buildNumber>") == Right(2),
        SnapshotMetadata.buildNumber("<metadata></metadata>") == Left(MetadataFault.MissingBuild),
        SnapshotMetadata.buildNumber("<buildNumber>0</buildNumber>") == Left(MetadataFault.BadBuild),
        SnapshotMetadata.buildNumber("<buildNumber>nope</buildNumber>") == Left(MetadataFault.BadBuild),
      ),
  )

  private def built(revision: Revision, group: Organization): Either[ArtifactIdError, Artifact] =
    ArtifactId.from("widget").map(name => Artifact(group, name, revision, pk))

  private def withExtras: Either[ArtifactIdError, Artifact] =
    ArtifactId
      .from("widget")
      .map: name =>
        Artifact(
          Organization("demo"),
          name,
          Revision.Pointer(SnapshotPointer(ship)),
          pk,
          sources = Some(pk),
          javadoc = Some(pk),
        )

  private def describedArtifact: Option[Artifact] =
    ArtifactId.from("widget") match
      case Left(_)     => None
      case Right(name) =>
        ArtifactId.from("core") match
          case Left(_)        => None
          case Right(depends) =>
            DependencyVersion.from("1.2.3") match
              case Left(_)        => None
              case Right(version) =>
                Some(
                  Artifact(
                    Organization.trusted("a\"&b"),
                    name,
                    Revision.Release(ReleaseVersion(ship)),
                    pk,
                    dependencies = List(MavenDependency(Organization("demo"), depends, version)),
                    document = PomDocument(
                      description = Some(PomText("rocks & rolls")),
                      license = Some(
                        License(LicenseName("Apache-2.0"), HttpUrl("https://www.apache.org/licenses/LICENSE-2.0.txt"))
                      ),
                      developer = Some(Developer(DeveloperId("ada"), PomText("Ada"))),
                      scm = Some(
                        Scm(HttpUrl("https://example.com/demo"), ScmConnection("scm:git:https://example.com/demo.git"))
                      ),
                    ),
                  )
                )
end LayoutLawSpec
