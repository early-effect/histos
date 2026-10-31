package histos.publish

import histos.central.RepositoryUser

import zio.{Chunk, Config}

/** Central snapshot credentials. A local publish does not choose a directory. */
enum PublishWhere:
  case Snapshots(user: RepositoryUser, password: Config.Secret)

  override def toString: String = this match
    case Snapshots(_, _) => "Snapshots"
end PublishWhere

/** Local writes the coordinate this session compiles, including `<ship>-ci`. Snapshots and a release refuse that
  * coordinate. A release with no release coordinate stops before selection. When another ship is a release, the
  * snapshot coordinate stays through selection so that ship is refused, and then it is not staged.
  */
enum PublishWrite:
  case Local
  case Snapshots
  case Release

object PublishWhere:
  def snapshots(env: Map[String, String]): Either[LocalPublishError, PublishWhere] =
    credentials(env)

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
end PublishWhere
