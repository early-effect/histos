package histos.publish

import histos.central.{Artifact, ArtifactId, Publisher, Revision}
import histos.version.{Catalog, ManualVersion}
import histos.{Organization, ProjectGraph}

import java.nio.file.{Files, Path}

import zio.{Chunk, ZIO}

/** The assembly jar, published beside the ship with an empty pom and no Scala suffix. */
object LauncherPublish:
  def ready(repo: Path, graph: ProjectGraph): Either[LocalPublishError, Unit] =
    graph.assembly match
      case None => Right(())
      case Some(_) =>
        for
          _ <- organization(graph)
          version <- manual(graph)
          file <- readVersion(repo)
          _ <- matched(file, version)
          _ <- present(jar(repo))
        yield ()

  def upload(
      repo: Path,
      graph: ProjectGraph,
      written: PublishOutput,
  ): ZIO[Publisher, LocalPublishError, PublishOutput] =
    graph.assembly match
      case None => ZIO.succeed(written)
      case Some(name) =>
        val path = jar(repo)
        for
          version <- ZIO.fromEither(readVersion(repo))
          _ <- ZIO.fromEither(revisions(written, version))
          group <- ZIO.fromEither(organization(graph))
          id <- ZIO.fromEither(artifact(group, name.value, version))
          revision <- ZIO.fromEither(manualRevision(version))
          bytes <- ZIO.attemptBlocking(Chunk.fromArray(Files.readAllBytes(path))).mapError(_ => LocalPublishError.Read(path))
          published <- ZIO.serviceWithZIO[Publisher](_.publish(Artifact(group, id, revision, bytes))).mapError(LocalPublishError.Publish(_))
        yield written.copy(modules =
          (written.modules :+ PublishedModule(group, name.value, version, published.jar, published.unique))
            .sortBy(line => (line.group.value, line.artifact, line.revision))
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

  private def readVersion(repo: Path): Either[LocalPublishError, String] =
    val path = repo.resolve(".histos-version")
    try
      val text = Files.readString(path).trim
      if text.isEmpty then Left(LocalPublishError.VersionFile(path)) else Right(text)
    catch case _: java.io.IOException => Left(LocalPublishError.VersionFile(path))

  private def matched(file: String, version: String): Either[LocalPublishError, Unit] =
    if file == version then Right(()) else Left(LocalPublishError.VersionMismatch(file, version))

  private def present(path: Path): Either[LocalPublishError, Unit] =
    if Files.isRegularFile(path) then Right(()) else Left(LocalPublishError.NoAssembly(path))

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
