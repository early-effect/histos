package histos.plugin

import histos.ProjectName

/** An open trigger. It renders only into [[On]] and [[Cond]]. The printer is the only place an expression becomes text.
  */
trait Trigger:
  def on: List[On]
  def cond: Cond
  def ++(other: Trigger): Trigger = Trigger.Combine(this, other)

object Trigger:
  /** Pull request, or a push to [[On.defaultBranch]]. */
  case object Ci extends Trigger:
    def on: List[On] = List(On.pullRequest, On.pushDefault)
    def cond: Cond   = Cond.AnyOf(List(Cond.Event(EventKind.PullRequest), Cond.DefaultBranchPush))

  /** A push to [[On.defaultBranch]]. Not a tag and not a pull request. */
  case object DefaultPush extends Trigger:
    def on: List[On] = List(On.pushDefault)
    def cond: Cond   = Cond.DefaultBranchPush

  /** `workflow_dispatch` with no inputs. */
  case object Dispatch extends Trigger:
    def on: List[On] = List(On.Dispatch(Nil))
    def cond: Cond   = Cond.Event(EventKind.Dispatch)

  /** A tag push. The glob is a git ref glob, such as `v*`. */
  final case class Tag(glob: String) extends Trigger:
    def on: List[On] = List(On.Push(Nil, List(glob), Nil))
    def cond: Cond   = Cond.TagMatches(glob)

  /** A pull request that gains this label. The type is not a default pull-request type, so the job is its own file. */
  final case class Label(label: String) extends Trigger:
    def on: List[On] = List(On.PullRequest(List(PullType.Labeled), Nil))
    def cond: Cond   = Cond.HasLabel(label)

  /** Paths on a push to [[On.defaultBranch]]. Non-empty paths are their own file: they would wake `ci.yml`. */
  final case class Paths(paths: ::[String]) extends Trigger:
    def on: List[On] = List(On.Push(List(On.defaultBranch), Nil, paths))
    def cond: Cond   = Cond.DefaultBranchPush

  final case class Combine(left: Trigger, right: Trigger) extends Trigger:
    def on: List[On] = left.on ++ right.on
    def cond: Cond   = Cond.AnyOf(List(left.cond, right.cond))

  /** Events that stay on a cron file. Safe `ci.yml` events are dropped, because the job is not in that file. */
  def cron(trigger: Trigger): (Cond, List[On]) =
    val extra = trigger.on.filter(On.withCron)
    if extra.isEmpty then (Cond.True, Nil)
    else (Cond.AnyOf(List(trigger.cond, Cond.Event(EventKind.Schedule))), extra)
end Trigger

/** A closed event. The engine prints these and does not accept a workflow string. */
enum On:
  case Push(branches: List[String], tags: List[String], paths: List[String])
  case PullRequest(types: List[PullType], paths: List[String])
  case Dispatch(inputs: List[DispatchInput])
  case MergeGroup
  case WorkflowRun(workflows: List[String])
  case Release

object On:
  val defaultPull: List[PullType] = List(PullType.Opened, PullType.Synchronize, PullType.Reopened)

  def pullRequest: On = PullRequest(defaultPull, Nil)

  /** `on.push.branches` entry. The short name GitHub reports as the repository default branch, the same fact as
    * [[Cond.DefaultBranch]]. Not a literal branch name.
    */
  val defaultBranch: String = "${{ github.event.repository.default_branch }}"

  /** A push to [[defaultBranch]]. Not every branch, and not a tag. */
  def pushDefault: On = Push(List(defaultBranch), Nil, Nil)

  /** Would this event wake or cancel `ci.yml` if it were merged there. */
  def splits(on: On): Boolean = on match
    case Push(_, _, paths)         => paths.nonEmpty
    case PullRequest(types, paths) =>
      paths.nonEmpty || types.exists(kind => !defaultPull.contains(kind))
    case WorkflowRun(_)                     => true
    case Dispatch(_) | MergeGroup | Release => false

  /** Kept beside a cron. A pull request or a branch push belongs to `ci.yml`, and this job is not in that file. */
  def withCron(on: On): Boolean = on match
    case Dispatch(_) | WorkflowRun(_) => true
    case other                        => splits(other)
end On

enum PullType:
  case Opened
  case Synchronize
  case Reopened
  case Labeled
  case Unlabeled

final case class DispatchInput(name: InputKey, kind: InputKind, default: Option[String])

enum InputKind:
  case Text
  case Boolean
  case Choice

/** A closed expression. It is not a YAML string. */
enum Cond:
  case True
  case Event(kind: EventKind)
  case DefaultBranch
  case DefaultBranchPush
  case TagMatches(glob: String)
  case HasLabel(label: String)
  case InputIs(name: InputKey, value: String)
  case Affected(module: ProjectName)
  case AffectedAll
  case NoUpstreamFailed
  case All(parts: List[Cond])
  case AnyOf(parts: List[Cond])
  case Not(inner: Cond)

enum EventKind:
  case PullRequest
  case Push
  case Dispatch
  case Schedule
  case MergeGroup
  case Release
  case WorkflowRun
  case Tag
