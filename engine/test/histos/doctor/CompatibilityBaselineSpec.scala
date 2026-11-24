package histos.doctor

import histos.Organization
import histos.central.{ArtifactId, MavenModule}
import histos.version.ShipNumber
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.jar.{JarEntry, JarOutputStream}

import zio.{IO, ZIO}
import zio.test.*

object CompatibilityBaselineSpec extends ZIOSpecDefault:
  private val revision = ShipNumber.trusted(0, 15, 0)
  private val group    = Organization("example.test")

  private val mainArtifactCases = List("resources_3", "javadoc-tool_3", "ordinary_3").map: artifact =>
    test(s"$artifact selects its exact main jar, not source or Javadoc classifiers") {
      ZIO.scoped:
        for
          root <- Workspace.work("baseline-resolver")
          id   <- ZIO.fromEither(ArtifactId.from(artifact))
          coordinate = BaselineCoordinate(MavenModule(group, id), revision)
          directory <- publication(root, coordinate)
          main      <- jar(directory.resolve(s"$artifact-${revision.render}.jar"), "main")
          _         <- jar(directory.resolve(s"$artifact-${revision.render}-sources.jar"), "sources")
          _         <- jar(directory.resolve(s"$artifact-${revision.render}-javadoc.jar"), "javadoc")
          repository = coursier.MavenRepository(root.toUri.toString)
          all           <- publications(repository, coordinate)
          result        <- CoursierCompatibilityBaseline(Some(Seq(repository))).fetch(coordinate)
          expectedBytes <- ZIO.attemptBlocking(Files.readAllBytes(main)).mapError(_ => IoFailure(main, IoFault.Read))
          actualBytes   <- ZIO
            .attemptBlocking(Files.readAllBytes(result))
            .mapError(_ => IoFailure(result, IoFault.Read))
        yield assertTrue(
          Option(result.getFileName).map(_.toString).contains(s"$artifact-${revision.render}.jar"),
          actualBytes.sameElements(expectedBytes),
          all.flatMap(path => Option(path.getFileName).map(_.toString)).toSet == Set(
            s"$artifact-${revision.render}.jar",
            s"$artifact-${revision.render}-sources.jar",
            s"$artifact-${revision.render}-javadoc.jar",
          ),
        )
    }

  def spec = suite("local compatibility baseline resolver")(
    suite("main artifact identity")(mainArtifactCases*),
    test("classifier-only publications cannot substitute for a missing main jar") {
      ZIO.scoped:
        for
          root <- Workspace.work("classifier-only")
          id   <- ZIO.fromEither(ArtifactId.from("ordinary_3"))
          coordinate = BaselineCoordinate(MavenModule(group, id), revision)
          directory <- publication(root, coordinate)
          _         <- jar(directory.resolve(s"ordinary_3-${revision.render}-sources.jar"), "sources")
          _         <- jar(directory.resolve(s"ordinary_3-${revision.render}-javadoc.jar"), "javadoc")
          result    <- CoursierCompatibilityBaseline(Some(Seq(coursier.MavenRepository(root.toUri.toString))))
            .fetch(coordinate)
            .either
        yield result match
          case Left(BaselineError.Unavailable(found, _)) => assertTrue(found == coordinate)
          case Left(BaselineError.MissingJar(found))     => assertTrue(found == coordinate)
          case Right(_)                                  => assertTrue(false)
    },
  )

  // A file-backed Maven layout keeps metadata and artifact resolution offline.
  private def publication(root: Path, coordinate: BaselineCoordinate): IO[IoFailure, Path] =
    val directory =
      root.resolve("example/test").resolve(coordinate.module.artifact.value).resolve(coordinate.revision.render)
    val artifact = coordinate.module.artifact.value
    val pom      = s"""<project>
      |<modelVersion>4.0.0</modelVersion>
      |<groupId>${coordinate.module.group.value}</groupId>
      |<artifactId>$artifact</artifactId>
      |<version>${coordinate.revision.render}</version>
      |<packaging>jar</packaging>
      |</project>
      |""".stripMargin
    Workspace.createDirectories(directory) *> Workspace
      .writeString(directory.resolve(s"$artifact-${coordinate.revision.render}.pom"), pom)
      .as(directory)

  // Distinct jar payloads make accidental classifier substitution observable.
  private def jar(path: Path, payload: String): IO[IoFailure, Path] =
    ZIO.scoped:
      ZIO
        .acquireRelease(
          ZIO
            .attemptBlocking(new JarOutputStream(Files.newOutputStream(path)))
            .mapError(_ => IoFailure(path, IoFault.Write))
        )(stream => ZIO.attemptBlocking(stream.close()).orDie)
        .flatMap: stream =>
          ZIO
            .attemptBlocking:
              stream.putNextEntry(new JarEntry("publication.txt"))
              stream.write(payload.getBytes(UTF_8))
              stream.closeEntry()
              path
            .mapError(_ => IoFailure(path, IoFault.Write))

  // Fetch all publications so classifier fixtures are proven resolvable, not merely present on disk.
  private def publications(
      repository: coursier.Repository,
      coordinate: BaselineCoordinate,
  ): IO[BaselineError, List[Path]] =
    ZIO
      .attemptBlockingInterrupt:
        val dependency = coursier
          .Dependency(
            coursier.Module(
              coursier.Organization(coordinate.module.group.value),
              coursier.ModuleName(coordinate.module.artifact.value),
            ),
            coursier.version.VersionConstraint(coordinate.revision.render),
          )
          .copy(transitive = false)
        coursier
          .Fetch()
          .withRepositories(Seq(repository))
          .addDependencies(dependency)
          .withMainArtifacts()
          .withClassifiers(Set(coursier.Classifier("sources"), coursier.Classifier("javadoc")))
          .run()
          .map(_.toPath)
          .toList
      .mapError(BaselineError.Unavailable(coordinate, _))
end CompatibilityBaselineSpec
