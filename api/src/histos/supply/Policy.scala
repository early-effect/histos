package histos.supply

import histos.doctor.DoctorPlacement
import histos.plugin.{ActionName, ActionPin, Cron, JobName, SecretName}
import histos.Developer

import zio.IO

/** Every policy has a named default. Pins are strict: a generated `uses` is a full sha. */
final case class SupplyChain(
    checkName: JobName = JobName("supply-chain"),
    extraActions: Map[ActionName, ActionPin] = Map.empty,
    advisories: AdvisoryPolicy = AdvisoryPolicy(),
    updates: UpdatePolicy = UpdatePolicy(),
    security: SecurityFixes = SecurityFixes.default,
    transitive: TransitiveFix = TransitiveFix.default,
    snapshot: Snapshot = Snapshot.default,
    placement: DoctorPlacement = DoctorPlacement.default,
)

enum BumpKind derives CanEqual:
  case Libraries, Actions, Tools, Launcher

enum SeverityGate:
  /** At or above `fail`, the check fails. At or above `annotate`, and below `fail`, it warns. */
  case Threshold(fail: Severity, annotate: Severity)
  case AnnotateOnly

object SeverityGate:
  val default: SeverityGate = Threshold(fail = Severity.High, annotate = Severity.Low)

enum GateScope:
  case All
  case Introduced

enum OutageStrategy:
  case FailClosed
  case FailOpen
  case Cached(maxAge: Hours)
  case ByEvent(pullRequest: OutageStrategy, push: OutageStrategy, scheduled: OutageStrategy)

object OutageStrategy:
  val default: OutageStrategy =
    ByEvent(pullRequest = Cached(Hours(24)), push = FailClosed, scheduled = FailClosed)

  def forEvent(strategy: OutageStrategy, event: Event): OutageStrategy =
    strategy match
      case ByEvent(pullRequest, push, scheduled) =>
        event match
          case Event.PullRequest => forEvent(pullRequest, event)
          case Event.Push        => forEvent(push, event)
          case Event.Schedule    => forEvent(scheduled, event)
      case other => other
end OutageStrategy

enum IgnoreRule:
  case ReasonAndExpiry(maxSpan: Days)
  case ReasonOnly
  case Disallowed

object IgnoreRule:
  val default: IgnoreRule = ReasonAndExpiry(maxSpan = Days(90))

final case class Ignore(id: AdvisoryId, reason: Reason, until: Option[Date])

object Ignore:
  def until(id: AdvisoryId, date: Date, reason: Reason): Ignore = Ignore(id, reason, Some(date))

  def open(id: AdvisoryId, reason: Reason): Ignore = Ignore(id, reason, None)
end Ignore

enum DailyAudit:
  case Off
  case On(at: Schedule)

object DailyAudit:
  val default: DailyAudit = Off

  val sixCentral: DailyAudit = On(Schedule(Cron("0 6 * * *"), TimeZone("America/Chicago")))
end DailyAudit

enum MainAlert:
  case Issue(label: LabelName)
  case CheckOnly

object MainAlert:
  val default: MainAlert = Issue(LabelName("histos-advisory"))

/** Open, so a build can add a source. `Osv` and `Ghsa` are tokens. The engine binds the transport. */
trait AdvisorySource:
  def name: SourceName
  def query(packages: List[Purl]): IO[SupplyFault, List[Finding]]

object AdvisorySource:
  case object Osv extends AdvisorySource:
    val name: SourceName = SourceName.trusted("osv")

    def query(packages: List[Purl]): IO[SupplyFault, List[Finding]] =
      zio.ZIO.fail(SupplyFault.ServiceDown(name, OutageStrategy.FailClosed))
  end Osv

  case object Ghsa extends AdvisorySource:
    val name: SourceName = SourceName.trusted("ghsa")

    def query(packages: List[Purl]): IO[SupplyFault, List[Finding]] =
      zio.ZIO.fail(SupplyFault.ServiceDown(name, OutageStrategy.FailClosed))
  end Ghsa
end AdvisorySource

final case class AdvisoryPolicy(
    gate: SeverityGate = SeverityGate.default,
    scope: GateScope = GateScope.All,
    outage: OutageStrategy = OutageStrategy.default,
    ignoreRule: IgnoreRule = IgnoreRule.default,
    ignores: List[Ignore] = Nil,
    daily: DailyAudit = DailyAudit.default,
    onMain: MainAlert = MainAlert.default,
    sources: ::[AdvisorySource] = ::(AdvisorySource.Osv, Nil),
)

final case class UpdatePolicy(
    schedule: Schedule = Schedule(Cron("0 6 * * 1"), TimeZone("America/Chicago")),
    branch: BranchName = BranchName("histos/deps"),
    grouping: Grouping = Grouping.default,
    author: PrAuthor = PrAuthor.default,
    signing: CommitSigning = CommitSigning.default,
    versions: VersionPolicy = VersionPolicy(),
    kinds: Set[BumpKind] = BumpKind.values.toSet,
    humanEdits: HumanEdits = HumanEdits.CommentOnly,
)

enum Grouping:
  case OnePr(commits: CommitSplit)
  case PrPerKind
  case PrPerDependency

object Grouping:
  val default: Grouping = OnePr(CommitSplit.PerKind)

enum CommitSplit:
  case PerKind, PerDependency, Single

enum PrAuthor:
  case App(appId: SecretName, privateKey: SecretName)
  case OwnerOpens(appId: SecretName, privateKey: SecretName)
  case Pat(token: SecretName)

object PrAuthor:
  val default: PrAuthor = App(SecretName("HISTOS_APP_ID"), SecretName("HISTOS_APP_PRIVATE_KEY"))

enum CommitSigning:
  case ApiSigned
  case GitPush(identity: Identity)
  case GitPushSshSigned(identity: Identity, signingKey: SecretName)

object CommitSigning:
  val default: CommitSigning = ApiSigned

enum Identity:
  case FirstDeveloper
  case Named(name: PersonName, email: Email)

enum HumanEdits:
  case CommentOnly
  case Rebase

final case class VersionPolicy(
    cooldown: Cooldown = Cooldown.After(Days(3)),
    preRelease: PreRelease = PreRelease.Skip,
    majors: Majors = Majors.ListOnly,
    zeroX: ZeroX = ZeroX.MinorIsMinor,
)

enum Cooldown:
  case Off
  case After(days: Days)

enum PreRelease:
  case Skip, FollowCurrentLine, Allow

enum Majors:
  case ListOnly, Apply

enum ZeroX:
  case MinorIsMinor
  case MinorIsMajor

enum SecurityFixes:
  case OutOfBand(branch: BranchName, pick: FixPick, cooldown: Cooldown)
  case InWeekly
  case AlertOnly

object SecurityFixes:
  val default: SecurityFixes =
    OutOfBand(BranchName("histos/security"), FixPick.Smallest, Cooldown.Off)

enum FixPick:
  case Smallest, Latest

enum TransitiveFix:
  case ParentThenForce(cleanup: ForceCleanup)
  case ForceOnly(cleanup: ForceCleanup)
  case ParentOnly
  case ReportOnly

object TransitiveFix:
  val default: TransitiveFix = ParentThenForce(ForceCleanup.WhenParentCatchesUp)

enum ForceCleanup:
  case WhenParentCatchesUp, Never

enum Snapshot:
  case Off
  case OnPushToDefault(job: JobName)

object Snapshot:
  val default: Snapshot = OnPushToDefault(JobName("dependency-snapshot"))

/** The one developer on the organization, when `Identity.FirstDeveloper` needs an address. */
def developerEmail(developer: Developer): Email =
  Email.trusted(s"${developer.id.value}@users.noreply.github.com")
