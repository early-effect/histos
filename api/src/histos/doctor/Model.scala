package histos.doctor

import _root_.scala.annotation.publicInBinary

import histos.plugin.{CiJob, PluginId, RepoPath}
import histos.HttpUrl

import zio.UIO

/** A checked literal such as `CheckId("supply.secret-missing")`. */
opaque type CheckId = String

object CheckId:
  given CanEqual[CheckId, CheckId] = CanEqual.derived

  inline def apply(inline raw: String): CheckId = ${ DoctorMacros.checkId('raw) }

  def from(raw: String): Either[CheckIdError, CheckId] =
    if raw.isEmpty then Left(CheckIdError.Empty)
    else if valid(raw) then Right(trusted(raw))
    else Left(CheckIdError.Bad(raw))

  extension (id: CheckId) def value: String = id

  @publicInBinary private[histos] def trusted(raw: String): CheckId = raw

  private def valid(raw: String): Boolean =
    val parts = raw.split("\\.", -1).toList
    parts.length >= 2 && parts.forall(segment)

  private def segment(raw: String): Boolean =
    raw.headOption match
      case Some(head) if head.isLetter && head.isLower =>
        raw.forall(char => char.isLower || char.isDigit || char == '-')
      case _ => false
end CheckId

enum CheckIdError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a check id is not empty"
    case Bad(raw) => s"'$raw' is not a check id"
end CheckIdError

/** Where a check belongs. A plugin uses [[Area.Plugin]]. */
enum Area:
  case Toolchain, BuildDefinition, Workflows, RepoSettings, SupplyChain, SitePublish, Release
  case Plugin(id: PluginId)

/** What a check reads. A missing probe is [[Outcome.Skipped]], never a crash. */
enum Probe:
  case Local, GitHubRead, GitHubAdmin, AppToken, JobEnv

enum Level derives CanEqual:
  case Error, Warning, Info

/** One way to repair a finding. The first of a diagnostic's fixes is preferred. There is no secret case. */
enum Fix:
  case Settings(url: HttpUrl, click: String)
  case Command(line: ShellLine)
  case Config(file: RepoPath, edit: Edit)
  case Steps(items: ::[String])

/** A local file edit. `before` is the text that is replaced. `after` is the replacement. */
final case class Edit(before: String, after: String)

/** One non-empty shell command. It is not a secret value. */
opaque type ShellLine = String

object ShellLine:
  given CanEqual[ShellLine, ShellLine] = CanEqual.derived

  def from(raw: String): Either[ShellLineError, ShellLine] =
    if raw.isEmpty || raw.exists(_.isControl) then Left(ShellLineError.Bad(raw))
    else Right(trusted(raw))

  extension (line: ShellLine) def value: String = line

  @publicInBinary private[histos] def trusted(raw: String): ShellLine = raw
end ShellLine

enum ShellLineError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a shell command"
end ShellLineError

/** Which fixes `--fix` may apply. Secrets are not a kind. */
enum FixKind:
  case LocalEdit, HistosCommand, GitHubSetting

/** Print, or apply the named kinds. `--yes` never includes [[FixKind.GitHubSetting]]. */
enum FixMode:
  case Print
  case Apply(kinds: Set[FixKind])

object FixMode:
  def from(fix: Boolean, yes: Boolean): FixMode =
    if !fix then Print
    else if yes then Apply(Set(FixKind.LocalEdit, FixKind.HistosCommand))
    else Apply(Set(FixKind.LocalEdit, FixKind.HistosCommand, FixKind.GitHubSetting))

/** Where doctor runs in generated workflows. Neither value adds a job. */
enum DoctorPlacement:
  /** Every generated job starts with `./histos doctor --preflight`, and the `workflow` job also runs the full doctor.
    */
  case PreflightPlusWorkflowJob

  /** The `workflow` job runs the full doctor. No per-job preflight step is rendered. */
  case WorkflowJobOnly
end DoctorPlacement

object DoctorPlacement:
  val default: DoctorPlacement = PreflightPlusWorkflowJob

/** A file and an optional line. The line is 1-based. */
final case class Location(file: RepoPath, line: Option[Int])

/** Shared by every area. `summary` can carry an existing fault message verbatim. */
trait Diagnostic:
  def check: CheckId
  def level: Level
  def title: String
  def summary: String
  def explain: String
  def fixes: ::[Fix]
  def location: Option[Location] = None

enum Outcome:
  case Ok(check: CheckId, note: String)
  case Found(diagnostic: Diagnostic)
  case Skipped(check: CheckId, missing: Probe, howToRun: Fix)

/** Open: core, modules, [[histos.plugin.CiJob]] implementations, and plugins contribute checks. */
trait DoctorCheck:
  def id: CheckId
  def area: Area
  def needs: Set[Probe]
  def run(ctx: DoctorContext): UIO[List[Outcome]]

/** What a check may read. Probes that are absent stay out of `present`. */
final case class DoctorContext(
    jobs: List[CiJob],
    present: Set[Probe],
)
