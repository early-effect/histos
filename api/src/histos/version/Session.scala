package histos.version

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

/** A pull request number. Zero and negative values are not pull requests. */
opaque type PullRequestNumber = Int

object PullRequestNumber:
  given CanEqual[PullRequestNumber, PullRequestNumber] = CanEqual.derived

  def from(raw: Int): Either[PullRequestError, PullRequestNumber] =
    if raw > 0 then Right(raw) else Left(PullRequestError.NotPositive(raw))

  def parse(raw: String): Either[PullRequestError, PullRequestNumber] =
    if raw.nonEmpty && raw.forall(_.isDigit) && !(raw.length > 1 && raw.charAt(0) == '0') then
      raw.toIntOption match
        case Some(value) => from(value)
        case None        => Left(PullRequestError.NotANumber(raw))
    else Left(PullRequestError.NotANumber(raw))

  extension (number: PullRequestNumber) def value: Int = number

enum PullRequestError:
  case NotANumber(raw: String)
  case NotPositive(value: Int)

  def message: String = this match
    case NotANumber(raw)  => s"'$raw' is not a pull request number"
    case NotPositive(value) => s"$value is not a positive pull request number"

object Session:
  val Key: String = "HISTOS_SESSION"
  val ShaKey: String = "HISTOS_SHA"
  val BranchKey: String = "HISTOS_BRANCH"
  val PullRequestKey: String = "HISTOS_PR"

  /** Absent means development. A snapshot or pull request without its commit is refused. */
  def from(env: Map[String, String]): Either[SessionError, Session] =
    env.get(Key) match
      case None | Some("development") => Right(Session.Development)
      case Some("snapshot")           => snapshot(env)
      case Some("pr")                 => pullRequest(env)
      case Some("release")            => Right(Session.Release)
      case Some(other)                => Left(SessionError.Unknown(other))

  private def snapshot(env: Map[String, String]): Either[SessionError, Session] =
    for
      sha    <- field(env, ShaKey, SessionError.MissingSha).flatMap(parseSha)
      branch <- field(env, BranchKey, SessionError.MissingBranch).flatMap(parseBranchName)
    yield Session.Snapshot(sha, branch)

  private def pullRequest(env: Map[String, String]): Either[SessionError, Session] =
    for
      number <- field(env, PullRequestKey, SessionError.MissingPullRequest).flatMap(parsePullRequest)
      sha    <- field(env, ShaKey, SessionError.MissingSha).flatMap(parseSha)
    yield Session.PullRequest(number, sha)

  private def field(env: Map[String, String], key: String, missing: SessionError): Either[SessionError, String] =
    env.get(key) match
      case Some(raw) => Right(raw)
      case None      => Left(missing)

  private def parseSha(raw: String): Either[SessionError, CommitSha] =
    CommitSha.from(raw).left.map(SessionError.Sha(_))

  private def parseBranchName(raw: String): Either[SessionError, Branch] =
    Branch.from(raw).left.map(SessionError.BadBranch(_))

  private def parsePullRequest(raw: String): Either[SessionError, PullRequestNumber] =
    PullRequestNumber.parse(raw).left.map(SessionError.PullRequest(_))

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
    case MissingSha           => s"a snapshot session needs ${Session.ShaKey}, a 40-hex commit"
    case MissingBranch        => s"a snapshot session needs ${Session.BranchKey}, default or other"
    case MissingPullRequest   => s"a pull request session needs ${Session.PullRequestKey}, a positive number"
    case Sha(error)           => error.message
    case BadBranch(error)     => error.message
    case PullRequest(error)   => error.message

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
    kept.foldLeft[Either[RegistryError, List[(ShipName, TagRelation)]]](Right(Nil)): (acc, line) =>
      acc.flatMap: seen =>
        entry(line).flatMap: parsed =>
          val (name, _) = parsed
          if seen.exists((found, _) => found == name) then Left(RegistryError.Duplicate(name))
          else Right(seen :+ parsed)
    .map(parsed => new Registry(parsed))

  private def entry(line: String): Either[RegistryError, (ShipName, TagRelation)] =
    line.split("\\s+").toList match
      case name :: relation :: Nil =>
        for
          ship <- ShipName.from(name).left.map(_ => RegistryError.BadName(name))
          tag  <- TagRelation.from(relation).left.map(_ => RegistryError.BadRelation(relation))
        yield (ship, tag)
      case _ => Left(RegistryError.BadLine(line))

enum RegistryError:
  case BadLine(line: String)
  case BadName(raw: String)
  case BadRelation(raw: String)
  case Duplicate(name: ShipName)

  def message: String = this match
    case BadLine(line) =>
      s"'$line' is not a registry line. A line is '<ship> matches', '<ship> moved', or '<ship> unknown'."
    case BadName(raw)      => s"'$raw' is not a ship name"
    case BadRelation(raw)  => s"'$raw' is not a tag relation. A relation is matches, moved, or unknown."
    case Duplicate(name)   => s"ship '${name.value}' is listed twice in .histos/releases"
