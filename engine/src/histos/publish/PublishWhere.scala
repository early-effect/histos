package histos.publish

import histos.central.{Publisher, RepositorySecret, RepositoryUser, SnapshotRepository}

import java.nio.file.{InvalidPathException, Path}

import zio.{Chunk, Config}

/** Where `histos publish` writes. `snapshots` is Central. Any other token is a directory. */
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

object PublishWhere:
  val Token: String = "snapshots"

  def resolve(
      repo: Path,
      repository: String,
      env: Map[String, String],
  ): Either[LocalPublishError, PublishWhere] =
    if repository == Token then credentials(env)
    else directory(repo, repository)

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

  private def directory(
      repo: Path,
      repository: String,
  ): Either[LocalPublishError, PublishWhere] =
    try
      val raw  = Path.of(repository)
      val path = if raw.isAbsolute then raw else repo.resolve(raw)
      Right(PublishWhere.Directory(path))
    catch
      case _: InvalidPathException =>
        Left(LocalPublishError.NotAPath(repository))
end PublishWhere
