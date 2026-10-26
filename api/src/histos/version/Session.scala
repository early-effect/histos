package histos.version

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

import zio.Config

/** Where this build is publishing from. A pull request has no branch: it cannot move the pointer. */
enum Session:
  case Development
  case Snapshot(commit: CommitSha, branch: Branch)
  case PullRequest(number: PullRequestNumber, commit: CommitSha)
  case Release

/** The default branch is the only place the `<ship>-SNAPSHOT` pointer moves. */
enum Branch:
  case Default
  case Other

object Branch:
  def from(raw: String): Either[BranchError, Branch] =
    raw match
      case "default" => Right(Branch.Default)
      case "other"   => Right(Branch.Other)
      case other     => Left(BranchError.Unknown(other))

enum BranchError:
  case Unknown(raw: String)

  def message: String = this match
    case Unknown(raw) => s"'$raw' is not a branch. A branch is default or other."
end BranchError

/** A pull request number. Zero and negative values are not pull requests. */
opaque type PullRequestNumber = Int

object PullRequestNumber:
  given CanEqual[PullRequestNumber, PullRequestNumber] = CanEqual.derived

  inline def apply(inline raw: String): PullRequestNumber = ${
    PullRequestNumberMacro.literal('raw)
  }

  def from(raw: Int): Either[PullRequestError, PullRequestNumber] =
    if raw > 0 then Right(trusted(raw)) else Left(PullRequestError.NotPositive(raw))

  def parse(raw: String): Either[PullRequestError, PullRequestNumber] =
    if raw.nonEmpty && raw.forall(_.isDigit) && !(raw.length > 1 && raw.charAt(0) == '0') then
      raw.toIntOption match
        case Some(value) => from(value)
        case None        => Left(PullRequestError.NotANumber(raw))
    else Left(PullRequestError.NotANumber(raw))

  extension (number: PullRequestNumber) def value: Int = number

  @publicInBinary private[histos] def trusted(raw: Int): PullRequestNumber = raw
end PullRequestNumber

enum PullRequestError:
  case NotANumber(raw: String)
  case NotPositive(value: Int)

  def message: String = this match
    case NotANumber(raw)    => s"'$raw' is not a pull request number"
    case NotPositive(value) => s"$value is not a positive pull request number"
end PullRequestError

private object PullRequestNumberMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[PullRequestNumber] =
    raw.value match
      case None =>
        quotes.reflect.report.errorAndAbort(
          "PullRequestNumber(...) takes a literal. Use PullRequestNumber.parse at runtime.",
          raw,
        )
      case Some(text) =>
        PullRequestNumber.parse(text) match
          case Left(error) =>
            quotes.reflect.report.errorAndAbort(error.message, raw)
          case Right(number) => '{ PullRequestNumber.trusted(${ Expr(number.value) }) }

/** The four session fields. `Session.from` parses them. A provider loads `config`. */
final case class SessionSource(
    session: Option[String],
    sha: Option[String],
    branch: Option[String],
    pullRequest: Option[String],
)

object SessionSource:
  /** One segment per key. `ConfigProvider.fromMap` splits on `.`, and these keys have no dot. */
  val config: Config[SessionSource] =
    (
      Config.string(Session.Key).optional ++
        Config.string(Session.ShaKey).optional ++
        Config.string(Session.BranchKey).optional ++
        Config.string(Session.PullRequestKey).optional
    ).map:
      case (session, sha, branch, pullRequest) =>
        SessionSource(session, sha, branch, pullRequest)

object Session:
  val Key: String            = "HISTOS_SESSION"
  val ShaKey: String         = "HISTOS_SHA"
  val BranchKey: String      = "HISTOS_BRANCH"
  val PullRequestKey: String = "HISTOS_PR"

  /** Absent or `development` is development. A snapshot or pull request missing a field is refused. */
  def from(source: SessionSource): Either[SessionError, Session] =
    source.session match
      case None | Some("development") => Right(Session.Development)
      case Some("snapshot")           => snapshot(source)
      case Some("pr")                 => pullRequest(source)
      case Some("release")            => Right(Session.Release)
      case Some(other)                => Left(SessionError.Unknown(other))

  private def snapshot(source: SessionSource): Either[SessionError, Session] =
    for
      sha    <- source.sha.toRight(SessionError.MissingSha).flatMap(parseSha)
      branch <- source.branch.toRight(SessionError.MissingBranch).flatMap(parseBranchName)
    yield Session.Snapshot(sha, branch)

  private def pullRequest(source: SessionSource): Either[SessionError, Session] =
    for
      number <- source.pullRequest.toRight(SessionError.MissingPullRequest).flatMap(parsePullRequest)
      sha    <- source.sha.toRight(SessionError.MissingSha).flatMap(parseSha)
    yield Session.PullRequest(number, sha)

  private def parseSha(raw: String): Either[SessionError, CommitSha] =
    CommitSha.from(raw).left.map(SessionError.Sha(_))

  private def parseBranchName(raw: String): Either[SessionError, Branch] =
    Branch.from(raw).left.map(SessionError.BadBranch(_))

  private def parsePullRequest(raw: String): Either[SessionError, PullRequestNumber] =
    PullRequestNumber.parse(raw).left.map(SessionError.PullRequest(_))
end Session

enum SessionError:
  case Unknown(raw: String)
  case MissingSha
  case MissingBranch
  case MissingPullRequest
  case Sha(error: CommitShaError)
  case BadBranch(error: BranchError)
  case PullRequest(error: PullRequestError)

  def message: String = this match
    case Unknown(raw) =>
      s"'$raw' is not a session. Sessions are development, snapshot, pr, and release."
    case MissingSha         => s"a snapshot session needs ${Session.ShaKey}, a 40-hex commit"
    case MissingBranch      => s"a snapshot session needs ${Session.BranchKey}, default or other"
    case MissingPullRequest => s"a pull request session needs ${Session.PullRequestKey}, a positive number"
    case Sha(error)         => error.message
    case BadBranch(error)   => error.message
    case PullRequest(error) => error.message
end SessionError

/** What git can prove about a ship that is already on the release registry. */
enum TagRelation:
  case Matches
  case Moved
  case Unknown

object TagRelation:
  def from(raw: String): Either[TagRelationError, TagRelation] =
    raw match
      case "matches" => Right(TagRelation.Matches)
      case "moved"   => Right(TagRelation.Moved)
      case "unknown" => Right(TagRelation.Unknown)
      case other     => Left(TagRelationError.Unknown(other))

enum TagRelationError:
  case Unknown(raw: String)

  def message: String = this match
    case Unknown(raw) => s"'$raw' is not a tag relation. A relation is matches, moved, or unknown."
end TagRelationError

/** A publish never treats "we did not look" as "nothing is released". */
enum Evidence:
  case Unchecked
  case Checked(registry: Registry)

/** Ships already on the release registry, and whether the tree still matches the tag. */
final case class Registry private (entries: List[(ShipName, TagRelation)]):
  def relation(name: ShipName): Option[TagRelation] =
    entries.collectFirst:
      case (found, relation) if found == name => relation

object Registry:
  val empty: Registry = new Registry(Nil)

  def parse(lines: List[String]): Either[RegistryError, Registry] =
    val kept = lines.map(_.trim).filter(line => line.nonEmpty && !line.startsWith("#"))
    kept
      .foldLeft[Either[RegistryError, List[(ShipName, TagRelation)]]](Right(Nil)): (acc, line) =>
        acc.flatMap: seen =>
          entry(line).flatMap: parsed =>
            val (name, _) = parsed
            if seen.exists((found, _) => found == name) then Left(RegistryError.Duplicate(name))
            else Right(parsed :: seen)
      .map(parsed => new Registry(parsed.reverse))

  private def entry(line: String): Either[RegistryError, (ShipName, TagRelation)] =
    line.split("\\s+").toList match
      case name :: relation :: Nil =>
        for
          ship <- ShipName.from(name).left.map(RegistryError.Name(_))
          tag  <- TagRelation.from(relation).left.map(RegistryError.Relation(_))
        yield (ship, tag)
      case _ => Left(RegistryError.BadLine(line))
end Registry

enum RegistryError:
  case BadLine(line: String)
  case Name(error: ShipNameError)
  case Relation(error: TagRelationError)
  case Duplicate(name: ShipName)

  def message: String = this match
    case BadLine(line) =>
      s"'$line' is not a registry line. A line is '<ship> matches', '<ship> moved', or '<ship> unknown'."
    case Name(error)     => error.message
    case Relation(error) => error.message
    case Duplicate(name) => s"ship '${name.value}' is listed twice in .histos/releases"
end RegistryError
