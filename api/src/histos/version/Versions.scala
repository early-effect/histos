package histos.version

/** `<ship>-ci` for one ship. Development never publishes. */
final case class DevelopLine(ship: ShipName, compile: CiVersion)

/** What a publish session does with one ship. A hold publishes nothing. */
enum PublishAction:
  case Snapshot(compile: CiVersion, stored: SnapshotCoordinate, pointer: Option[SnapshotPointer])
  case Release(compile: ReleaseVersion, bump: CatalogBump)
  case Hold(compile: HoldCompile, reason: HoldReason)

final case class PublishLine(ship: ShipName, action: PublishAction)

/** The compile coordinate of a ship that publishes nothing. */
enum HoldCompile:
  case Everyday(version: CiVersion)
  case Release(version: ReleaseVersion)

  def render: String = this match
    case Everyday(version) => version.render
    case Release(version)  => version.render

enum HoldReason:
  case AlreadyReleased

/** The release job opens a pull request for the next line. The tagged commit does not contain it, and the next number is not guessed here. */
final case class CatalogBump(ship: ShipName, released: ShipNumber)

enum Drift:
  case Clear
  case Unchecked

enum Refusal:
  /** Snapshot, pull request, and release do not run until the registry has been read. */
  case Unchecked
  case Shadowed(ship: ShipName, number: ShipNumber)
  case Unproven(ship: ShipName, number: ShipNumber)
  case NothingToRelease

/** A session that can publish. Development is not one: it only compiles. */
enum PublishKind:
  case Snapshot(commit: CommitSha, branch: Branch)
  case PullRequest(number: PullRequestNumber, commit: CommitSha)
  case Release

/** The version decision. A refusal carries no coordinate, so a caller cannot publish or compile past it. */
enum Decision:
  case NoCatalog
  case Develop(drift: Drift, lines: ::[DevelopLine])
  case Publish(kind: PublishKind, lines: ::[PublishLine])
  case Refused(refusals: ::[Refusal])

object Decision:
  def text(decision: Decision): String = decision match
    case Decision.NoCatalog => "no catalog"
    case Decision.Develop(drift, lines) =>
      (sessionText(Session.Development) :: driftText(drift) :: lines.map(developText).toList).mkString("\n")
    case Decision.Publish(kind, lines) =>
      (kindText(kind) :: lines.map(publishText).toList).mkString("\n")
    case Decision.Refused(refusals) =>
      refusalText(refusals)

  def refusalText(refusals: ::[Refusal]): String =
    refusals.map(oneRefusal).mkString("\n")

  private def sessionText(session: Session): String = session match
    case Session.Development => "session development"
    case Session.Snapshot(_, branch) =>
      branch match
        case Branch.Default => "session snapshot default"
        case Branch.Other   => "session snapshot other"
    case Session.PullRequest(number, _) => s"session pr ${number.value}"
    case Session.Release                 => "session release"

  private def kindText(kind: PublishKind): String = kind match
    case PublishKind.Snapshot(_, branch) =>
      branch match
        case Branch.Default => "session snapshot default"
        case Branch.Other   => "session snapshot other"
    case PublishKind.PullRequest(number, _) => s"session pr ${number.value}"
    case PublishKind.Release                => "session release"

  private def driftText(drift: Drift): String = drift match
    case Drift.Clear     => "drift clear"
    case Drift.Unchecked => "drift unchecked"

  private def developText(line: DevelopLine): String =
    s"${line.ship.value} compile ${line.compile.render}"

  private def publishText(line: PublishLine): String =
    val body = line.action match
      case PublishAction.Snapshot(compile, stored, pointer) =>
        val published = s"compile ${compile.render} publish ${stored.render}"
        pointer match
          case Some(value) => s"$published pointer ${value.render}"
          case None        => published
      case PublishAction.Release(compile, _) =>
        s"compile ${compile.render} publish ${compile.render} bump pull-request"
      case PublishAction.Hold(compile, HoldReason.AlreadyReleased) =>
        s"compile ${compile.render} hold already-released"
    s"${line.ship.value} $body"

  private def oneRefusal(refusal: Refusal): String = refusal match
    case Refusal.Unchecked =>
      "the release registry was not checked, so nothing publishes"
    case Refusal.Shadowed(ship, number) =>
      s"${ship.value} ${number.render} is released and has changes since its tag, so nothing publishes"
    case Refusal.Unproven(ship, number) =>
      s"${ship.value} ${number.render} is released and its tag was not proven, so nothing publishes"
    case Refusal.NothingToRelease =>
      "every ship number is already released"

enum VersionError:
  case Session(error: SessionError)
  case Registry(error: RegistryError)
  case UnknownShip(name: ShipName)
  case Refused(refusals: ::[Refusal])
  case Unreadable

  def message: String = this match
    case Session(error)   => error.message
    case Registry(error)  => error.message
    case UnknownShip(name) =>
      s"ship '${name.value}' is in .histos/releases and not in the catalog"
    case Refused(refusals) => Decision.refusalText(refusals)
    case Unreadable       => "could not read .histos/releases"

object Versions:
  def assess(catalog: Catalog, env: Map[String, String], evidence: Evidence): Either[VersionError, Decision] =
    Session.from(env).left.map(VersionError.Session(_)).flatMap: session =>
      evidence match
        case Evidence.Unchecked => Right(decide(catalog, session, evidence))
        case Evidence.Checked(registry) =>
          unknown(catalog, registry) match
            case Some(name) => Left(VersionError.UnknownShip(name))
            case None       => Right(decide(catalog, session, evidence))

  def decide(catalog: Catalog, session: Session, evidence: Evidence): Decision =
    catalog.ships match
      case Nil => Decision.NoCatalog
      case first :: rest =>
        val ships = ::(first, rest)
        evidence match
          case Evidence.Unchecked => unchecked(session, ships)
          case Evidence.Checked(registry) => checked(session, ships, registry)

  private def unchecked(session: Session, ships: ::[Ship]): Decision =
    session match
      case Session.Development => Decision.Develop(Drift.Unchecked, developLines(ships))
      case _                   => Decision.Refused(::(Refusal.Unchecked, Nil))

  private def checked(session: Session, ships: ::[Ship], registry: Registry): Decision =
    val judged = ships.map(ship => ship -> place(ship, registry.relation(ship.name)))
    val refusals = judged.collect { case (_, Left(refusal)) => refusal }
    refusals match
      case head :: tail => Decision.Refused(::(head, tail))
      case Nil =>
        judged.collect { case (ship, Right(state)) => ship -> state } match
          case (ship, state) :: rest => ready(session, ::((ship, state), rest))
          case Nil                   => Decision.NoCatalog

  private def ready(session: Session, placed: ::[(Ship, Placement)]): Decision =
    session match
      case Session.Development =>
        Decision.Develop(Drift.Clear, developLines(names(placed)))
      case Session.Snapshot(commit, branch) =>
        val kind = PublishKind.Snapshot(commit, branch)
        Decision.Publish(kind, publishLines(kind, placed))
      case Session.PullRequest(number, commit) =>
        val kind = PublishKind.PullRequest(number, commit)
        Decision.Publish(kind, publishLines(kind, placed))
      case Session.Release =>
        val lines = publishLines(PublishKind.Release, placed)
        if lines.forall(isHold) then Decision.Refused(::(Refusal.NothingToRelease, Nil))
        else Decision.Publish(PublishKind.Release, lines)

  private def developLines(ships: ::[Ship]): ::[DevelopLine] =
    ships match
      case ship :: rest =>
        ::(DevelopLine(ship.name, CiVersion(ship.number)), rest.map(item => DevelopLine(item.name, CiVersion(item.number))))

  private def names(placed: ::[(Ship, Placement)]): ::[Ship] =
    placed match
      case (ship, _) :: rest => ::(ship, rest.map(_._1))

  private def publishLines(kind: PublishKind, placed: ::[(Ship, Placement)]): ::[PublishLine] =
    placed match
      case (ship, state) :: rest =>
        ::(publishLine(kind, ship, state), rest.map(pair => publishLine(kind, pair._1, pair._2)))

  private def publishLine(kind: PublishKind, ship: Ship, state: Placement): PublishLine =
    val action = state match
      case Placement.Current =>
        kind match
          case PublishKind.Release =>
            PublishAction.Hold(HoldCompile.Release(ReleaseVersion(ship.number)), HoldReason.AlreadyReleased)
          case _ =>
            PublishAction.Hold(HoldCompile.Everyday(CiVersion(ship.number)), HoldReason.AlreadyReleased)
      case Placement.Unreleased =>
        kind match
          case PublishKind.Snapshot(commit, branch) =>
            val pointer = branch match
              case Branch.Default => Some(SnapshotPointer(ship.number))
              case Branch.Other   => None
            PublishAction.Snapshot(CiVersion(ship.number), SnapshotCoordinate(ship.number, commit.abbreviate), pointer)
          case PublishKind.PullRequest(_, commit) =>
            PublishAction.Snapshot(CiVersion(ship.number), SnapshotCoordinate(ship.number, commit.abbreviate), None)
          case PublishKind.Release =>
            PublishAction.Release(ReleaseVersion(ship.number), CatalogBump(ship.name, ship.number))
    PublishLine(ship.name, action)

  private def isHold(line: PublishLine): Boolean =
    line.action match
      case PublishAction.Hold(_, _) => true
      case _                        => false

  private def place(ship: Ship, relation: Option[TagRelation]): Either[Refusal, Placement] =
    relation match
      case None                      => Right(Placement.Unreleased)
      case Some(TagRelation.Matches) => Right(Placement.Current)
      case Some(TagRelation.Moved)   => Left(Refusal.Shadowed(ship.name, ship.number))
      case Some(TagRelation.Unknown) => Left(Refusal.Unproven(ship.name, ship.number))

  private def unknown(catalog: Catalog, registry: Registry): Option[ShipName] =
    registry.entries.collectFirst:
      case (name, _) if catalog.get(name).isEmpty => name

  private enum Placement:
    case Unreleased
    case Current
