package histos.plugin

import _root_.scala.annotation.publicInBinary
import _root_.scala.collection.immutable.::

import histos.doctor.DoctorCheck
import histos.version.CommitSha
import histos.ProjectName

/** One plugin task in CI. Open: a build extends this trait. Histos ships [[TaskJob]] and [[SiteJob]]. The renderer
  * matches [[JobRender]], not this trait.
  */
trait CiJob:
  def name: JobName
  def task: TaskName
  def permissions: JobPermissions
  def needs: List[JobName]
  def render(plugin: PluginId): List[JobRender]
  def preflight: List[DoctorCheck] = Nil

/** A task runs in `ci.yml`, or on a cron in its own file. A cron is not a site. */
final case class TaskJob(
    name: JobName,
    task: TaskName,
    trigger: Trigger = Trigger.Ci,
    cron: Option[Cron] = None,
    permissions: JobPermissions = JobPermissions.read,
    needs: List[JobName] = Nil,
    runner: Runner = Runner.Default,
    env: List[(EnvName, EnvValue)] = Nil,
    environment: Option[EnvironmentName] = None,
    pre: List[PinnedAction] = Nil,
    needsPolicy: NeedsPolicy = NeedsPolicy.NoneFailed,
    graphSecrets: GraphSecrets = GraphSecrets.WhenNoThirdParty,
    guard: Option[Guard] = None,
    concurrency: Option[Concurrency] = None,
) extends CiJob:
  def render(plugin: PluginId): List[JobRender] =
    val arranged = arrange
    cron match
      case Some(expr) =>
        val (cond, events) = Trigger.cron(trigger)
        List(JobRender.Schedule(expr, arranged.copy(cond = cond, events = events)))
      case None =>
        List(JobRender.Task(arranged))

  private def arrange: TaskArrange =
    val base     = trigger.cond
    val narrowed = guard match
      case None        => base
      case Some(found) => Cond.All(List(base, Guard.cond(found)))
    TaskArrange(
      name,
      task,
      permissions,
      needs,
      narrowed,
      trigger.on,
      runner,
      env,
      environment,
      pre,
      needsPolicy,
      graphSecrets,
      concurrency,
    )
end TaskJob

object TaskJob:
  /** Cron plus `workflow_dispatch`. The cron is not part of [[Trigger]], so a site cannot take this. */
  def nightly(name: JobName, task: TaskName, cron: Cron): TaskJob =
    TaskJob(name, task, trigger = Trigger.Dispatch, cron = Some(cron))

/** Publishes one directory. There is no cron parameter: a scheduled site cannot be constructed. The default trigger is
  * a push to the repository's default branch, not a tag and not a pull request.
  */
final case class SiteJob(
    name: JobName,
    task: TaskName,
    site: Site,
    trigger: Trigger = Trigger.DefaultPush,
    permissions: JobPermissions = JobPermissions.read,
    needs: List[JobName] = Nil,
    runner: Runner = Runner.Default,
    env: List[(EnvName, EnvValue)] = Nil,
    environment: Option[EnvironmentName] = None,
    pre: List[PinnedAction] = Nil,
    needsPolicy: NeedsPolicy = NeedsPolicy.NoneFailed,
    graphSecrets: GraphSecrets = GraphSecrets.WhenNoThirdParty,
    guard: Option[Guard] = None,
    concurrency: Option[Concurrency] = None,
) extends CiJob:
  def render(plugin: PluginId): List[JobRender] =
    val arranged = arrange
    val deploy   = JobRender.Site(arranged, SiteShape.DeployOnPush)
    val build    = JobRender.Site(arranged, SiteShape.Build)
    val dispatch = JobRender.Site(arranged, SiteShape.Dispatch)
    site.publish match
      case SitePublish.Automatic => List(deploy)
      case SitePublish.Manual    => List(build, dispatch)
      case SitePublish.Both      => List(deploy, dispatch)

  private def arrange: SiteArrange =
    val base     = trigger.cond
    val narrowed = guard match
      case None        => base
      case Some(found) => Cond.All(List(base, Guard.cond(found)))
    SiteArrange(
      name,
      task,
      permissions,
      needs,
      site,
      narrowed,
      trigger.on,
      runner,
      env,
      environment,
      pre,
      needsPolicy,
      graphSecrets,
      concurrency,
    )
end SiteJob

enum SitePublish:
  case Automatic
  case Manual
  case Both

final case class Site(dir: RepoPath, publish: SitePublish = SitePublish.Both)

/** What the engine knows how to print. A plugin chooses cases. It does not hand over YAML. */
enum JobRender:
  case Task(job: TaskArrange)
  case Schedule(cron: Cron, job: TaskArrange)
  case Site(job: SiteArrange, shape: SiteShape)
  case Fan(job: TaskArrange, targets: ::[Target])

enum SiteShape:
  case Build
  case DeployOnPush
  case Dispatch

final case class TaskArrange(
    name: JobName,
    task: TaskName,
    permissions: JobPermissions,
    needs: List[JobName],
    cond: Cond,
    events: List[On] = Nil,
    runner: Runner = Runner.Default,
    env: List[(EnvName, EnvValue)] = Nil,
    environment: Option[EnvironmentName] = None,
    pre: List[PinnedAction] = Nil,
    needsPolicy: NeedsPolicy = NeedsPolicy.NoneFailed,
    graphSecrets: GraphSecrets = GraphSecrets.WhenNoThirdParty,
    concurrency: Option[Concurrency] = None,
)

final case class SiteArrange(
    name: JobName,
    task: TaskName,
    permissions: JobPermissions,
    needs: List[JobName],
    site: Site,
    cond: Cond,
    events: List[On] = Nil,
    runner: Runner = Runner.Default,
    env: List[(EnvName, EnvValue)] = Nil,
    environment: Option[EnvironmentName] = None,
    pre: List[PinnedAction] = Nil,
    needsPolicy: NeedsPolicy = NeedsPolicy.NoneFailed,
    graphSecrets: GraphSecrets = GraphSecrets.WhenNoThirdParty,
    concurrency: Option[Concurrency] = None,
)

final case class Target(
    name: TargetName,
    environment: Option[EnvironmentName] = None,
    env: List[(EnvName, EnvValue)] = Nil,
)

/** Job-level skip. [[Guard.Affected]] is the whole module, so an unaffected deploy does not start a matrix row. */
enum Guard:
  case OnCi
  case OnDefaultPush
  case Unguarded
  case Affected(modules: ::[ProjectName], within: Guard)

object Guard:
  def cond(guard: Guard): Cond =
    guard match
      case OnCi                   => Trigger.Ci.cond
      case OnDefaultPush          => Cond.DefaultBranchPush
      case Unguarded              => Cond.True
      case Affected(mods, within) =>
        val mine = mods.map(name => Cond.Affected(name): Cond)
        val any  = Cond.AnyOf(Cond.AffectedAll :: mine)
        Cond.All(List(cond(within), any))

enum NeedsPolicy:
  case AllSucceeded
  case NoneFailed

/** Graph-wide secrets reach a third-party action only when the job opts in. */
enum GraphSecrets:
  case WhenNoThirdParty
  case Omit
  case Include

final case class Concurrency(group: GroupName, cancel: Cancel)

enum Cancel:
  case InProgress
  case Finish

opaque type GroupName = String

object GroupName:
  given CanEqual[GroupName, GroupName] = CanEqual.derived

  inline def apply(inline raw: String): GroupName = ${ PluginMacros.groupName('raw) }

  def from(raw: String): Either[GroupNameError, GroupName] =
    Tokens.parseSimple(raw, GroupNameError.Empty, GroupNameError.Bad(_)).map(trusted)

  extension (name: GroupName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): GroupName = raw
end GroupName

enum GroupNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a concurrency group is not empty"
    case Bad(raw) => s"'$raw' is not a concurrency group"
end GroupNameError

enum Grant:
  case ContentsRead
  case ContentsWrite
  case PackagesRead
  case PackagesWrite
  case PullRequestsRead
  case PullRequestsWrite
  case ActionsRead
  case ActionsWrite
  case IdTokenWrite
  case PagesRead
  case PagesWrite
  case IssuesRead
  case IssuesWrite
  case SecurityEventsWrite

final case class JobPermissions(grants: Set[Grant])

object JobPermissions:
  val read: JobPermissions  = JobPermissions(Set(Grant.ContentsRead))
  val write: JobPermissions = JobPermissions(Set(Grant.ContentsWrite))

enum Runner:
  case Default
  case Named(name: RunnerName)

opaque type RunnerName = String

object RunnerName:
  given CanEqual[RunnerName, RunnerName] = CanEqual.derived

  inline def apply(inline raw: String): RunnerName = ${ PluginMacros.runnerName('raw) }

  def from(raw: String): Either[RunnerNameError, RunnerName] =
    Tokens.parseSimple(raw, RunnerNameError.Empty, RunnerNameError.Bad(_)).map(trusted)

  extension (name: RunnerName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): RunnerName = raw
end RunnerName

enum RunnerNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a runner name is not empty"
    case Bad(raw) => s"'$raw' is not a runner name"
end RunnerNameError

opaque type EnvironmentName = String

object EnvironmentName:
  given CanEqual[EnvironmentName, EnvironmentName] = CanEqual.derived

  inline def apply(inline raw: String): EnvironmentName = ${ PluginMacros.environmentName('raw) }

  def from(raw: String): Either[EnvironmentNameError, EnvironmentName] =
    Tokens.parseSimple(raw, EnvironmentNameError.Empty, EnvironmentNameError.Bad(_)).map(trusted)

  extension (name: EnvironmentName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): EnvironmentName = raw
end EnvironmentName

enum EnvironmentNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "an environment name is not empty"
    case Bad(raw) => s"'$raw' is not an environment name"
end EnvironmentNameError

opaque type TargetName = String

object TargetName:
  given CanEqual[TargetName, TargetName] = CanEqual.derived

  inline def apply(inline raw: String): TargetName = ${ PluginMacros.targetName('raw) }

  def from(raw: String): Either[TargetNameError, TargetName] =
    Tokens.parseSimple(raw, TargetNameError.Empty, TargetNameError.Bad(_)).map(trusted)

  extension (name: TargetName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): TargetName = raw
end TargetName

enum TargetNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a target name is not empty"
    case Bad(raw) => s"'$raw' is not a target name"
end TargetNameError

opaque type ActionName = String

object ActionName:
  given CanEqual[ActionName, ActionName] = CanEqual.derived

  inline def apply(inline raw: String): ActionName = ${ PluginMacros.actionName('raw) }

  def from(raw: String): Either[ActionNameError, ActionName] =
    if action(raw) then Right(trusted(raw))
    else if raw.isEmpty then Left(ActionNameError.Empty)
    else Left(ActionNameError.Bad(raw))

  extension (name: ActionName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): ActionName = raw

  private def action(raw: String): Boolean =
    raw.nonEmpty &&
      !raw.startsWith("/") &&
      !raw.endsWith("/") &&
      !raw.contains("//") &&
      raw.split("/", -1).forall(part => Tokens.repoSegment(part) || Tokens.simple(part))
end ActionName

enum ActionNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "an action name is not empty"
    case Bad(raw) => s"'$raw' is not an action name"
end ActionNameError

opaque type SecretName = String

object SecretName:
  given CanEqual[SecretName, SecretName] = CanEqual.derived

  inline def apply(inline raw: String): SecretName = ${ PluginMacros.secretName('raw) }

  def from(raw: String): Either[SecretNameError, SecretName] =
    if Tokens.env(raw) then Right(trusted(raw))
    else if raw.isEmpty then Left(SecretNameError.Empty)
    else Left(SecretNameError.Bad(raw))

  extension (name: SecretName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): SecretName = raw
end SecretName

enum SecretNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a secret name is not empty"
    case Bad(raw) => s"'$raw' is not a secret name"
end SecretNameError

final case class ActionPin(sha: CommitSha, tag: String)

/** A third-party action. The pin is the catalog entry for [[name]], checked when the job is loaded. */
final case class PinnedAction(
    name: ActionName,
    inputs: List[(InputKey, EnvValue)],
    grants: Set[Grant],
)

/** OIDC into AWS. The pin lives in the catalog under [[action]]. LaunchPad stays in the adopter's build. */
object AwsOidc:
  val action: ActionName = ActionName.trusted("aws-actions/configure-aws-credentials")

  def apply(role: EnvValue, region: EnvValue, ecr: Option[EnvValue] = None): PinnedAction =
    val base = List(
      InputKey("role-to-assume") -> role,
      InputKey("aws-region")     -> region,
    )
    val extra = ecr.toList.map(value => InputKey("ecr-registry") -> value)
    PinnedAction(action, base ++ extra, Set(Grant.IdTokenWrite))
end AwsOidc

enum EnvValue:
  case Plain(value: String)
  case Secret(name: SecretName)
  case Github(field: GithubField)
  case Env(name: EnvName)

enum GithubField:
  case Sha
  case RefName
  case Token

enum AffectedBase:
  case PullRequestBase
  case LastGreenPush
  case All
