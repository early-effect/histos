package histos.supply

import histos.plugin.{ActionName, RepoPath}
import histos.version.CommitSha
import histos.Developer

import java.time.LocalDate
import java.time.temporal.ChronoUnit

import zio.{IO, UIO, ZIO}

final case class AuditResult(failures: List[SupplyFault], annotations: List[Finding])

object Audit:
  def classify(word: Option[String], vector: Option[Cvss]): Severity =
    word.flatMap(named).orElse(vector.map(_.band)).getOrElse(Severity.Low)

  def evaluate(
      findings: List[Finding],
      policy: AdvisoryPolicy,
      event: Event,
      introduced: Set[Purl],
      today: Date,
      down: Option[(SourceName, Hours)],
  ): Either[SupplyFault, AuditResult] =
    down match
      case Some((source, age)) =>
        OutageStrategy.forEvent(policy.outage, event) match
          case OutageStrategy.FailOpen    => Right(AuditResult(Nil, Nil))
          case OutageStrategy.FailClosed  => Left(SupplyFault.ServiceDown(source, policy.outage))
          case OutageStrategy.Cached(max) =>
            if age.value <= max.value then gated(findings, policy, introduced, today)
            else Left(SupplyFault.ServiceDown(source, policy.outage))
          case OutageStrategy.ByEvent(_, _, _) => Left(SupplyFault.ServiceDown(source, policy.outage))
      case None => gated(findings, policy, introduced, today)

  /** The batch query always runs. Hydration runs only for an id whose `modified` is new. */
  def fetch[R](
      purls: List[Purl],
      cache: Map[AdvisoryId, (String, Finding)],
      query: List[Purl] => IO[SupplyFault, List[Finding]],
      hydrate: Finding => IO[SupplyFault, Finding],
  ): ZIO[R, SupplyFault, (List[Finding], Map[AdvisoryId, (String, Finding)])] =
    query(purls).flatMap: listed =>
      ZIO.foldLeft(listed)((List.empty[Finding], cache)):
        case ((kept, seen), finding) =>
          seen.get(finding.id) match
            case Some((modified, stored)) if modified == finding.modified =>
              ZIO.succeed((kept :+ stored, seen))
            case _ =>
              hydrate(finding).map: full =>
                (kept :+ full, seen.updated(finding.id, (finding.modified, full)))

  private def named(word: String): Option[Severity] =
    word.toLowerCase match
      case "low"      => Some(Severity.Low)
      case "moderate" => Some(Severity.Moderate)
      case "medium"   => Some(Severity.Moderate)
      case "high"     => Some(Severity.High)
      case "critical" => Some(Severity.Critical)
      case _          => None

  private def gated(
      findings: List[Finding],
      policy: AdvisoryPolicy,
      introduced: Set[Purl],
      today: Date,
  ): Either[SupplyFault, AuditResult] =
    ignores(policy, today).map: dropped =>
      val live   = findings.filterNot(finding => dropped.contains(finding.id))
      val scoped = policy.scope match
        case GateScope.All        => live
        case GateScope.Introduced =>
          live.filter(finding => finding.affected.exists(introduced.contains))
      val (fail, note) = scoped.partition(finding => fails(policy.gate, finding.severity))
      val annotated    = note.filter(finding => annotates(policy.gate, finding.severity))
      AuditResult(fail.map(SupplyFault.Advisory(_)), annotated)

  private def ignores(policy: AdvisoryPolicy, today: Date): Either[SupplyFault, Set[AdvisoryId]] =
    policy.ignoreRule match
      case IgnoreRule.Disallowed if policy.ignores.nonEmpty =>
        Left(SupplyFault.IgnoresDisallowed(policy.ignores.map(_.id)))
      case IgnoreRule.Disallowed           => Right(Set.empty)
      case IgnoreRule.ReasonOnly           => Right(policy.ignores.map(_.id).toSet)
      case IgnoreRule.ReasonAndExpiry(max) =>
        policy.ignores.foldLeft[Either[SupplyFault, Set[AdvisoryId]]](Right(Set.empty)):
          case (Left(error), _)      => Left(error)
          case (Right(seen), ignore) =>
            ignore.until match
              case None        => Left(SupplyFault.IgnoreNeedsExpiry(ignore.id))
              case Some(until) =>
                val span = ChronoUnit.DAYS.between(LocalDate.parse(today.value), LocalDate.parse(until.value))
                if LocalDate.parse(until.value).isBefore(LocalDate.parse(today.value)) then
                  Left(SupplyFault.IgnoreExpired(ignore.id, until))
                else if span > max.value then Left(SupplyFault.IgnoreTooLong(ignore.id, max))
                else Right(seen + ignore.id)

  private def rank(severity: Severity): Int =
    severity match
      case Severity.Unknown  => 0
      case Severity.Low      => 1
      case Severity.Moderate => 2
      case Severity.High     => 3
      case Severity.Critical => 4

  private def fails(gate: SeverityGate, severity: Severity): Boolean =
    gate match
      case SeverityGate.AnnotateOnly       => false
      case SeverityGate.Threshold(fail, _) => rank(severity) >= rank(fail)

  private def annotates(gate: SeverityGate, severity: Severity): Boolean =
    gate match
      case SeverityGate.AnnotateOnly              => true
      case SeverityGate.Threshold(fail, annotate) =>
        rank(severity) >= rank(annotate) && rank(severity) < rank(fail)
end Audit

final case class BumpPlan(apply: List[ChosenBump], listed: List[ChosenBump])

object Bump:
  def choose(
      locked: List[LockedDependency],
      releases: List[Release],
      policy: VersionPolicy,
      today: Date,
      security: Boolean,
      transitive: TransitiveFix,
  ): Either[SupplyFault, BumpPlan] =
    split(locked) match
      case Some(fault) => Left(fault)
      case None        =>
        val chosen          = locked.flatMap(row => pick(row, releases, policy, today, security, transitive))
        val (listed, apply) = chosen.partition(_.listed)
        Right(BumpPlan(apply.map(_.bump), listed.map(_.bump)))

  /** The first coordinate, in lock order, that two cells lock at different versions, with the cells that lock it. */
  private def split(locked: List[LockedDependency]): Option[SupplyFault] =
    locked
      .map(_.coordinate)
      .distinct
      .map(coordinate => coordinate -> locked.filter(_.coordinate == coordinate))
      .collectFirst:
        case (coordinate, rows) if rows.map(_.version).distinct.length > 1 =>
          SupplyFault.CrossVersionSplit(coordinate, rows.map(row => CellName.trusted(row.cell)).distinct)

  def verify(
      action: ActionName,
      tag: String,
      sha: CommitSha,
      reachable: Boolean,
      was: RepoId,
      now: RepoId,
  ): Either[SupplyFault, Unit] =
    if was != now then Left(SupplyFault.ActionRepoMoved(action, was, now))
    else if !reachable then Left(SupplyFault.TagNotReachable(action, tag, sha))
    else Right(())

  private final case class Row(bump: ChosenBump, listed: Boolean)

  private def pick(
      locked: LockedDependency,
      releases: List[Release],
      policy: VersionPolicy,
      today: Date,
      security: Boolean,
      transitive: TransitiveFix,
  ): Option[Row] =
    val candidates =
      releases.filter(_.coordinate == locked.coordinate).filter(release => newer(locked.version, release.version))
    val allowed  = candidates.filter(release => admits(locked.version, release, policy, today, security))
    val selected =
      if security then allowed.filter(_.security.isDefined).sortBy(release => numbers(release.version)).headOption
      else allowed.sortBy(release => numbers(release.version)).reverse.headOption
    selected.map: release =>
      val major  = isMajor(locked.version, release.version, policy.zeroX)
      val listed = !security && major && policy.majors == Majors.ListOnly
      val forced = !locked.direct && force(transitive, locked, release, releases)
      val kind   = if security then BumpKind.Libraries else BumpKind.Libraries
      Row(ChosenBump(kind, locked, release, forced), listed || reportOnly(transitive, locked))

  private def force(
      transitive: TransitiveFix,
      locked: LockedDependency,
      release: Release,
      releases: List[Release],
  ): Boolean =
    if locked.direct then false
    else
      transitive match
        case TransitiveFix.ReportOnly | TransitiveFix.ParentOnly => false
        case TransitiveFix.ForceOnly(_)                          => true
        case TransitiveFix.ParentThenForce(cleanup)              =>
          val parentHasIt =
            releases.exists(item => item.version == release.version && item.coordinate != locked.coordinate)
          cleanup match
            case ForceCleanup.WhenParentCatchesUp if parentHasIt => false
            case _                                               => true

  private def reportOnly(transitive: TransitiveFix, locked: LockedDependency): Boolean =
    !locked.direct && transitive == TransitiveFix.ReportOnly

  private def admits(
      current: String,
      release: Release,
      policy: VersionPolicy,
      today: Date,
      security: Boolean,
  ): Boolean =
    val cooled = security || policy.cooldown == Cooldown.Off || cooledEnough(release.published, policy.cooldown, today)
    val pre    = policy.preRelease match
      case PreRelease.Allow             => true
      case PreRelease.Skip              => !release.preRelease
      case PreRelease.FollowCurrentLine => !release.preRelease || current.exists(char => char == '-' || char == '+')
    cooled && pre

  private def cooledEnough(published: Date, cooldown: Cooldown, today: Date): Boolean =
    cooldown match
      case Cooldown.Off         => true
      case Cooldown.After(days) =>
        val span = ChronoUnit.DAYS.between(LocalDate.parse(published.value), LocalDate.parse(today.value))
        span >= days.value

  private def isMajor(from: String, to: String, zero: ZeroX): Boolean =
    val (f, t) = (numbers(from), numbers(to))
    if f._1 == 0 && t._1 == 0 then
      zero match
        case ZeroX.MinorIsMinor => false
        case ZeroX.MinorIsMajor => t._2 != f._2
    else t._1 != f._1

  private def newer(from: String, to: String): Boolean =
    val left  = numbers(from)
    val right = numbers(to)
    right._1 > left._1 || (right._1 == left._1 && (right._2 > left._2 || (right._2 == left._2 && right._3 > left._3)))

  private def numbers(version: String): (Int, Int, Int) =
    val core                = version.takeWhile(char => char.isDigit || char == '.')
    val parts               = core.split("\\.", -1).toList
    def at(index: Int): Int = parts.lift(index).flatMap(_.toIntOption).getOrElse(0)
    (at(0), at(1), at(2))
end Bump

enum GhCall:
  case UpdateRef(branch: BranchName, sha: CommitSha, create: Boolean)
  case CommitOnBranch(branch: BranchName, expected: CommitSha, headline: String, files: List[(String, String)])
  case OpenOrUpdate(title: String, body: String)
  case Comment(body: String)
  case Close(branch: BranchName)
  case Push(branch: BranchName, signed: Boolean)

final case class KindChange(kind: BumpKind, files: List[(String, String)])

final case class PrRequest(
    policy: UpdatePolicy,
    branch: BranchName,
    main: CommitSha,
    head: Option[CommitSha],
    changes: List[KindChange],
    human: List[CommitSha],
    openPr: Boolean,
    developer: Option[Developer],
)

object PrFlow:
  val title: String = "deps: weekly updates"

  def marker(oid: CommitSha): String = s"<!-- histos-deps head=${oid.value} -->"

  def workflowPath(path: String): Boolean =
    path == ".github/workflows" || path.startsWith(".github/workflows/")

  /** Actions commits the catalog and the lock. Workflow files stay for a person to regenerate and push. */
  def keep(author: PrAuthor, files: List[(String, String)]): List[(String, String)] =
    author match
      case PrAuthor.Actions => files.filterNot((path, _) => workflowPath(path))
      case PrAuthor.App(_, _) | PrAuthor.OwnerOpens(_, _) | PrAuthor.Pat(_) => files

  def plan(request: PrRequest): Either[SupplyFault, List[GhCall]] =
    val kept = dropWorkflows(request)
    if kept.changes.isEmpty then Right(if kept.openPr then List(GhCall.Close(kept.branch)) else Nil)
    else if kept.human.nonEmpty && kept.policy.humanEdits == HumanEdits.CommentOnly then
      Right(
        List(
          GhCall.Comment(
            s"${kept.branch.value} has human commits ${kept.human.map(_.abbreviate.value).mkString(", ")}"
          )
        )
      )
    else
      identity(kept).map: _ =>
        val create = kept.head.isEmpty
        val ref    =
          if kept.human.nonEmpty then Nil
          else List(GhCall.UpdateRef(kept.branch, kept.main, create))
        val commits = kept.policy.signing match
          case CommitSigning.ApiSigned =>
            // UpdateRef points the branch at main. GhRun replaces every later
            // expectedHeadOid with the oid createCommitOnBranch just returned.
            kept.changes.map: change =>
              GhCall.CommitOnBranch(kept.branch, kept.main, s"deps: ${change.kind}", change.files)
          case CommitSigning.GitPush(_) =>
            List(GhCall.Push(kept.branch, false))
          case CommitSigning.GitPushSshSigned(_, _) =>
            List(GhCall.Push(kept.branch, true))
        val body = kept.changes.map(_.kind.toString).mkString(", ") + "\n" + regenerate(kept.policy.author) + marker(
          kept.main
        )
        ref ++ commits ++ List(GhCall.OpenOrUpdate(title, body))

  private def dropWorkflows(request: PrRequest): PrRequest =
    val changes = request.changes.flatMap: change =>
      val files = keep(request.policy.author, change.files)
      if files.isEmpty then Nil else List(change.copy(files = files))
    request.copy(changes = changes)

  private def regenerate(author: PrAuthor): String =
    author match
      case PrAuthor.Actions =>
        "Run ./histos workflow --update and push the workflow files.\n"
      case PrAuthor.App(_, _) | PrAuthor.OwnerOpens(_, _) | PrAuthor.Pat(_) => ""

  def committed(branch: BranchName, expected: CommitSha, actual: CommitSha): Either[SupplyFault, CommitSha] =
    if expected == actual then Right(actual) else Left(SupplyFault.HeadMoved(branch, expected))

  private def identity(request: PrRequest): Either[SupplyFault, Unit] =
    request.policy.signing match
      case CommitSigning.ApiSigned    => Right(())
      case CommitSigning.GitPush(who) =>
        who match
          case Identity.Named(_, _)    => Right(())
          case Identity.FirstDeveloper =>
            request.developer.fold(Left(SupplyFault.IdentityMissing))(_ => Right(()))
      case CommitSigning.GitPushSshSigned(who, _) =>
        who match
          case Identity.Named(_, _)    => Right(())
          case Identity.FirstDeveloper =>
            request.developer.fold(Left(SupplyFault.IdentityMissing))(_ => Right(()))
end PrFlow

object Alert:
  enum Move:
    case Open(id: AdvisoryId)
    case Update(id: AdvisoryId)
    case Close(id: AdvisoryId)

  def issues(open: Set[AdvisoryId], found: Set[AdvisoryId]): List[Move] =
    val created = found.diff(open).toList.map(Move.Open(_))
    val updated = found.intersect(open).toList.map(Move.Update(_))
    val closed  = open.diff(found).toList.map(Move.Close(_))
    created ++ updated ++ closed
end Alert

final case class SnapshotPackage(purl: String, relationship: String, scope: String)

final case class SnapshotManifest(file: String, packages: List[SnapshotPackage])

object SnapshotSubmit:
  def fromLock(text: String): SnapshotManifest =
    val lines = text.split('\n').toList.map(_.trim).filter(_.nonEmpty)
    val rows  = lines.foldLeft((false, "runtime", List.empty[SnapshotPackage])):
      case ((_, scope, seen), line) if line.startsWith("test ")          => (false, "development", seen)
      case ((_, scope, seen), line) if line.startsWith("cell ")          => (false, "runtime", seen)
      case ((direct, scope, seen), line) if line.startsWith("direct ")   => (true, scope, seen)
      case ((direct, scope, seen), line) if line.startsWith("artifact ") =>
        val parts = line.split(" ").toList
        parts match
          case _ :: org :: name :: version :: _ =>
            val purl         = s"pkg:maven/$org/$name@$version"
            val relationship = if direct then "direct" else "indirect"
            (false, scope, seen :+ SnapshotPackage(purl, relationship, scope))
          case _ => (direct, scope, seen)
      case (state, _) => state
    SnapshotManifest("dependencies.lock", rows._3)

  def json(manifest: SnapshotManifest, sha: String, ref: String): String =
    val resolved = manifest.packages.map: item =>
      s""""${item.purl}":{"package_url":"${item.purl}","relationship":"${item.relationship}","scope":"${item.scope}"}"""
    s"""{"version":0,"sha":"$sha","ref":"$ref","job":{"correlator":"dependency-snapshot","id":"dependency-snapshot"},"detector":{"name":"histos","version":"0.2.0","url":"https://github.com/early-effect/histos"},"manifests":{"${manifest.file}":{"name":"${manifest.file}","file":{"source_location":"${manifest.file}"},"resolved":{${resolved
        .mkString(",")}}}}}"""
end SnapshotSubmit
