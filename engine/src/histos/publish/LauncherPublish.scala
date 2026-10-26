package histos.publish

import histos.central.{Artifact, ArtifactId, Published, Publisher, Revision}
import histos.version.{Catalog, ManualVersion}
import histos.{Organization, ProjectGraph}

import java.io.IOException
import java.nio.file.{Files, Path}

import zio.{Chunk, IO, ZIO}

/** The assembly jar, published beside the ship with an empty pom and no Scala suffix. */
object LauncherPublish:
  def ready(repo: Path, graph: ProjectGraph): IO[LocalPublishError, Unit] =
    graph.assembly match
      case None    => ZIO.unit
      case Some(_) =>
        for
          _ <- ZIO.fromEither(organization(graph))
          _ <- ZIO.fromEither(manual(graph))
          _ <- present(jar(repo))
        yield ()

  def upload(
      repo: Path,
      graph: ProjectGraph,
      written: PublishOutput,
  ): ZIO[Publisher, LocalPublishError, PublishOutput] =
    graph.assembly match
      case None       => ZIO.succeed(written)
      case Some(name) =>
        val path = jar(repo)
        for
          version  <- ZIO.fromEither(manual(graph))
          _        <- ZIO.fromEither(revisions(written, version))
          group    <- ZIO.fromEither(organization(graph))
          id       <- ZIO.fromEither(artifact(group, name.value, version))
          revision <- ZIO.fromEither(manualRevision(version))
          bytes    <- ZIO
            .attemptBlocking(Chunk.fromArray(Files.readAllBytes(path)))
            .mapError(_ => LocalPublishError.Read(path))
          published <- ZIO
            .serviceWithZIO[Publisher](_.publish(Artifact(group, id, revision, bytes)))
            .mapError(LocalPublishError.Publish(_))
          line = published match
            case Published.Local(jar, _, _)    => PublishedModule(group, name.value, version, Some(jar))
            case Published.Snapshot(unique, _) => PublishedModule(group, name.value, version, None, Some(unique.value))
        yield written.copy(modules =
          (written.modules :+ line).sortBy(row => (row.group.value, row.artifact, row.revision))
        )

  private def organization(graph: ProjectGraph): Either[LocalPublishError, Organization] =
    graph.organization match
      case Some(group) => Right(group)
      case None        => Left(LocalPublishError.NoOrganization)

  private def manual(graph: ProjectGraph): Either[LocalPublishError, String] =
    graph.catalog match
      case Catalog.Manual(rows) =>
        rows.toList.map(_.version.value).distinct match
          case List(version) => Right(version)
          case _             => Left(LocalPublishError.LauncherRevision)
      case _ => Left(LocalPublishError.LauncherRevision)

  private def present(path: Path): IO[LocalPublishError, Unit] =
    ZIO
      .attemptBlocking(Files.isRegularFile(path))
      .refineOrDie:
        case _: IOException | _: SecurityException => LocalPublishError.Read(path)
      .flatMap: regular =>
        if regular then ZIO.unit else ZIO.fail(LocalPublishError.NoAssembly(path))

  private def revisions(written: PublishOutput, version: String): Either[LocalPublishError, Unit] =
    written.modules.map(_.revision).distinct match
      case List(found) if found == version => Right(())
      case List(found)                     => Left(LocalPublishError.VersionMismatch(version, found))
      case Nil                             => Left(LocalPublishError.NothingToPublish)
      case _                               => Left(LocalPublishError.LauncherRevision)

  private def artifact(group: Organization, name: String, version: String): Either[LocalPublishError, ArtifactId] =
    ArtifactId.from(name).left.map(_ => LocalPublishError.BadDependency(group.value, name, version))

  private def manualRevision(version: String): Either[LocalPublishError, Revision] =
    ManualVersion.from(version) match
      case Right(parsed) => Right(Revision.Manual(parsed))
      case Left(_)       => Left(LocalPublishError.VersionMismatch(version, version))

  private def jar(repo: Path): Path =
    repo.resolve("dist").resolve("histos.jar")
end LauncherPublish
