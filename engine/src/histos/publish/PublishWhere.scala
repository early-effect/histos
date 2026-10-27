package histos.publish

import histos.central.{Publisher, RepositorySecret, RepositoryUser, SnapshotRepository}

import java.nio.file.{InvalidPathException, Path}

import zio.{Chunk, Config}

/** Where a publish writes. A directory stays on this machine. Snapshots are Central. */
enum PublishWhere:
  case Directory(path: Path)
  case Snapshots(user: RepositoryUser, password: Config.Secret)

  def layer: zio.ZLayer[Any, histos.central.PublishError, Publisher] = this match
    case Directory(path)           => Publisher.local(path)
    case Snapshots(user, password) =>
      Publisher.snapshots(SnapshotRepository.central, RepositorySecret(user, password))

  override def toString: String = this match
    case Directory(path) => s"Directory($path)"
    case Snapshots(_, _) => "Snapshots"
end PublishWhere

/** Local writes the coordinate this session compiles, including `<ship>-ci`. Snapshots and a release refuse that
  * coordinate. A release also drops a snapshot coordinate after selection, so a dependent ship is still refused.
  */
enum PublishWrite:
  case Local
  case Snapshots
  case Release

object PublishWhere:
  def directory(
      repo: Path,
      raw: Option[String],
      home: Path,
  ): Either[LocalPublishError, PublishWhere] =
    localRoot(repo, raw, home).map(PublishWhere.Directory(_))

  def snapshots(env: Map[String, String]): Either[LocalPublishError, PublishWhere] =
    credentials(env)

  /** `None` is `home/.m2/repository`. A relative token resolves under `repo`. An absolute token stays absolute. */
  def localRoot(repo: Path, raw: Option[String], home: Path): Either[LocalPublishError, Path] =
    raw match
      case None        => Right(maven(home))
      case Some(token) => located(repo, token)

  def maven(home: Path): Path =
    home.resolve(".m2").resolve("repository")

  private def credentials(
      env: Map[String, String]
  ): Either[LocalPublishError, PublishWhere] =
    val user     = env.get("SONATYPE_USERNAME").map(_.trim).filter(_.nonEmpty)
    val password = env.get("SONATYPE_PASSWORD").filter(_.nonEmpty)
    (user, password) match
      case (Some(name), Some(secret)) =>
        RepositoryUser.from(name) match
          case Right(parsed) =>
            Right(PublishWhere.Snapshots(parsed, Config.Secret(Chunk.fromArray(secret.toCharArray))))
          case Left(_) => Left(LocalPublishError.Credentials)
      case _ => Left(LocalPublishError.Credentials)

  private def located(repo: Path, repository: String): Either[LocalPublishError, Path] =
    try
      val raw  = Path.of(repository)
      val path = if raw.isAbsolute then raw else repo.resolve(raw)
      Right(path)
    catch
      case _: InvalidPathException =>
        Left(LocalPublishError.NotAPath(repository))
end PublishWhere
