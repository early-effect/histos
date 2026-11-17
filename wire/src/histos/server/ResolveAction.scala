package histos.server

import histos.ProjectName

/** What `histos resolve` does. Refresh names the mutable snapshots to re-read. An empty list is every one. */
enum ResolveAction:
  case Verify
  case Update
  case Check
  case Refresh(selectors: List[ProjectName])

  def token: String = this match
    case Verify         => "verify"
    case Update         => "update"
    case Check          => "check"
    case Refresh(Nil)   => "refresh"
    case Refresh(names) => s"refresh:${names.map(_.value).mkString(",")}"
end ResolveAction

object ResolveAction:
  import zio.json.*

  /** The closed actions an MCP schema can name. Selectors ride on the refresh token, not this list. */
  val tokens: List[String] = List("verify", "update", "check", "refresh")

  given JsonCodec[ResolveAction] =
    JsonCodec.string.transformOrFail(
      raw => from(raw).left.map(_.message),
      _.token,
    )

  def from(raw: String): Either[ResolveActionError, ResolveAction] =
    raw match
      case "verify"                              => Right(ResolveAction.Verify)
      case "update"                              => Right(ResolveAction.Update)
      case "check"                               => Right(ResolveAction.Check)
      case "refresh"                             => Right(ResolveAction.Refresh(Nil))
      case other if other.startsWith("refresh:") =>
        selectors(other.drop("refresh:".length)).map(ResolveAction.Refresh(_))
      case other => Left(ResolveActionError.Unknown(other))

  private def selectors(raw: String): Either[ResolveActionError, List[ProjectName]] =
    if raw.isEmpty then Left(ResolveActionError.Selector)
    else
      raw
        .split(",", -1)
        .toList
        .foldLeft[Either[ResolveActionError, List[ProjectName]]](Right(Nil)): (acc, token) =>
          acc.flatMap: have =>
            if token.isEmpty then Left(ResolveActionError.Selector)
            else ProjectName.from(token).left.map(_ => ResolveActionError.Selector).map(have :+ _)
end ResolveAction

enum ResolveActionError:
  case Unknown(raw: String)
  case Selector

  def message: String = this match
    case Unknown(raw) => s"'$raw' is not a resolve action"
    case Selector     => "resolve --refresh names modules separated by commas"
end ResolveActionError
