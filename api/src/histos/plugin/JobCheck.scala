package histos.plugin

import histos.version.Catalog
import histos.{
  BuildError,
  CiFault,
  CoverageJob,
  DocCheck,
  Formatting,
  Module,
  Platform,
  PluginGraphFault,
  ProjectGraph,
  ProjectName,
}

/** One rendered job, the plugin that contributed it, and the workflow file it belongs in. */
final case class Placed(file: WorkflowFile, plugin: PluginId, render: JobRender)

enum WorkflowFile:
  case Ci
  case Pages
  case Named(name: String)

/** Events merged into `ci.yml` from jobs that stay in that file. */
final case class CiHeader(
    tags: List[String],
    extras: Set[CiExtra],
    inputs: List[DispatchInput],
)

enum CiExtra:
  case MergeGroup
  case Release

/** Load and render checks for [[CiJob]]. The engine prints [[Placed]] and does not match a job class. */
object JobCheck:
  def arrange(graph: ProjectGraph): Either[BuildError, List[Placed]] =
    val items = renders(graph)
    for
      _      <- reserved(items)
      _      <- sites(items)
      _      <- needs(graph, items)
      _      <- fold(items)(item => viewChecks(graph, view(item.render)))
      placed <- Right(items.map(place))
      _      <- ids(placed)
      _      <- fold(grouped(placed)) { case (file, jobs) => fileChecks(graph, placed, file, jobs) }
    yield placed

  def header(placed: List[Placed]): CiHeader =
    val events = placed.collect { case Placed(WorkflowFile.Ci, _, render) => eventsOf(render) }.flatten
    val tags   = events.collect { case On.Push(_, found, _) => found }.flatten.distinct
    val extras = events.flatMap {
      case On.MergeGroup => List(CiExtra.MergeGroup)
      case On.Release    => List(CiExtra.Release)
      case _             => Nil
    }.toSet
    CiHeader(tags, extras, mergeInputs(events))

  def generated(graph: ProjectGraph): Set[JobName] =
    val modules = graph.projects.collect { case module: Module => module }
    val base    = List(JobName.trusted("workflow"), JobName.trusted("affected"))
    val suite   =
      if modules.nonEmpty then List(JobName.trusted("test"), JobName.trusted("cache-seed")) else Nil
    val doc      = if graph.docCheck == DocCheck.On then List(JobName.trusted("scaladoc")) else Nil
    val coverage = graph.coverage match
      case CoverageJob.Off    => Nil
      case CoverageJob.Ask(_) => List(JobName.trusted("coverage"))
    val format = graph.formatting match
      case Formatting.Off         => Nil
      case Formatting.Scalafmt(_) => List(JobName.trusted("format"))
    val versions = if graph.catalog != Catalog.Empty then List(JobName.trusted("versions")) else Nil
    val launch   = if launcher(graph) then List(JobName.trusted("launcher")) else Nil
    (base ++ suite ++ doc ++ coverage ++ format ++ versions ++ launch).toSet

  private def launcher(graph: ProjectGraph): Boolean =
    graph.projects.exists:
      case module: Module =>
        module.name.value == "client" &&
        module.platforms.all.contains(Platform.Native) &&
        module.application.nonEmpty
      case _ => false

  private final case class Item(plugin: PluginId, job: CiJob, render: JobRender)

  private def renders(graph: ProjectGraph): List[Item] =
    graph.plugins.flatMap: plugin =>
      plugin.contribution.jobs.flatMap: job =>
        job.render.map(render => Item(plugin.id, job, render))

  private def reserved(items: List[Item]): Either[BuildError, Unit] =
    items.map(item => nameOf(item.render)).find(_.isReserved) match
      case Some(name) => Left(BuildError.PluginGraph(PluginGraphFault.ReservedJob(name)))
      case None       => Right(())

  private def sites(items: List[Item]): Either[BuildError, Unit] =
    val names = items
      .map(item => nameOf(item.render))
      .distinct
      .filter: name =>
        items.exists(item => nameOf(item.render) == name && isSite(item.render))
    names match
      case first :: second :: _ =>
        Left(BuildError.PluginGraph(PluginGraphFault.DuplicateSite(first, second)))
      case name :: Nil => shapes(name, items.filter(item => nameOf(item.render) == name))
      case Nil         => Right(())

  private def shapes(name: JobName, items: List[Item]): Either[BuildError, Unit] =
    val found = items.collect { case Item(_, _, JobRender.Site(_, shape)) => shape }
    val legal = found.toSet match
      case set if set == Set(SiteShape.Build)                            => true
      case set if set == Set(SiteShape.DeployOnPush)                     => true
      case set if set == Set(SiteShape.Dispatch)                         => true
      case set if set == Set(SiteShape.Build, SiteShape.Dispatch)        => true
      case set if set == Set(SiteShape.DeployOnPush, SiteShape.Dispatch) => true
      case _                                                             => false
    if found.distinct.size != found.size || !legal then Left(BuildError.Ci(CiFault.IllegalSite(name)))
    else Right(())

  private def needs(graph: ProjectGraph, items: List[Item]): Either[BuildError, Unit] =
    val known     = items.map(item => nameOf(item.render)).toSet
    val available = known ++ generated(graph)
    fold(items): item =>
      val name   = nameOf(item.render)
      val listed = needsOf(item.render)
      duplicate(listed) match
        case Some(need) => Left(BuildError.Ci(CiFault.DuplicateNeed(name, need)))
        case None       =>
          listed.find(need => !available.contains(need) || need == name) match
            case Some(need) => Left(BuildError.Ci(CiFault.UnknownNeed(name, need)))
            case None       => Right(())

  private def viewChecks(graph: ProjectGraph, seen: View): Either[BuildError, Unit] =
    for
      _ <- runner(graph, seen)
      _ <- pins(graph, seen)
      _ <- grants(seen)
      _ <- secrets(graph, seen)
      _ <- modules(graph, seen)
      _ <- targets(seen)
    yield ()

  private def runner(graph: ProjectGraph, seen: View): Either[BuildError, Unit] =
    seen.runner match
      case Runner.Default     => Right(())
      case Runner.Named(name) =>
        if graph.ci.runners.contains(name) then Right(())
        else Left(BuildError.Ci(CiFault.UnknownRunner(seen.name, name)))

  private def pins(graph: ProjectGraph, seen: View): Either[BuildError, Unit] =
    seen.pre.find(action => !graph.ci.actions.contains(action.name)) match
      case Some(action) => Left(BuildError.Ci(CiFault.UnpinnedAction(seen.name, action.name)))
      case None         => Right(())

  private def grants(seen: View): Either[BuildError, Unit] =
    seen.pre.find(action => !action.grants.subsetOf(seen.permissions.grants)) match
      case Some(action) => Left(BuildError.Ci(CiFault.GrantMissing(seen.name, action.name)))
      case None         => Right(())

  private def secrets(graph: ProjectGraph, seen: View): Either[BuildError, Unit] =
    val used =
      pairSecrets(graph.ci.env) ++ pairSecrets(seen.env) ++ seen.pre.flatMap(action =>
        secretValues(action.inputs.map(_._2))
      )
    used.find(secret => !graph.ci.secrets.contains(secret)) match
      case Some(secret) => Left(BuildError.Ci(CiFault.UnknownSecret(secret)))
      case None         => Right(())

  private def modules(graph: ProjectGraph, seen: View): Either[BuildError, Unit] =
    val known = graph.projects.collect { case module: Module => module.name }.toSet
    affected(seen.cond).find(name => !known.contains(name)) match
      case Some(name) => Left(BuildError.Ci(CiFault.UnknownModule(seen.name, name)))
      case None       => Right(())

  private def targets(seen: View): Either[BuildError, Unit] =
    duplicate(seen.targets.map(_.name)) match
      case Some(name) => Left(BuildError.Ci(CiFault.DuplicateTarget(seen.name, name)))
      case None       => Right(())

  private def ids(placed: List[Placed]): Either[BuildError, Unit] =
    val pairs = placed.map(item => (item.file, nameOf(item.render)))
    duplicate(pairs) match
      case Some((_, name)) => Left(BuildError.PluginGraph(PluginGraphFault.DuplicateJob(name)))
      case None            => Right(())

  private def fileChecks(
      graph: ProjectGraph,
      placed: List[Placed],
      file: WorkflowFile,
      jobs: List[Placed],
  ): Either[BuildError, Unit] =
    val picture = eventsFor(file, jobs)
    for
      _ <- fold(jobs)(job => reachable(nameOf(job.render), printed(job.render), picture))
      _ <- inputs(picture)
      _ <- concurrency(file, jobs)
      _ <- workflows(graph, placed, picture)
    yield ()

  private def reachable(name: JobName, cond: Cond, picture: Picture): Either[BuildError, Unit] =
    if covers(cond, picture) then Right(()) else Left(BuildError.Ci(CiFault.UnreachableCond(name)))

  private def covers(cond: Cond, picture: Picture): Boolean =
    cond match
      case Cond.True | Cond.Affected(_) | Cond.AffectedAll | Cond.NoUpstreamFailed => true
      case Cond.Event(kind)                                                        => heard(picture, kind)
      case Cond.DefaultBranch | Cond.DefaultBranchPush                             => pushes(picture)
      case Cond.TagMatches(_)                                                      => tagged(picture)
      case Cond.HasLabel(_)                                                        => labeled(picture)
      case Cond.InputIs(name, _) => picture.inputs.exists(_.name == name)
      case Cond.All(parts)       => parts.forall(covers(_, picture))
      case Cond.AnyOf(parts)     => parts.exists(covers(_, picture))
      case Cond.Not(inner)       => covers(inner, picture)

  private def inputs(picture: Picture): Either[BuildError, Unit] =
    picture.inputs
      .foldLeft[Either[BuildError, List[DispatchInput]]](Right(Nil)): (acc, input) =>
        acc.flatMap: seen =>
          seen.find(_.name == input.name) match
            case Some(prior) if prior.kind != input.kind || prior.default != input.default =>
              Left(BuildError.Ci(CiFault.DispatchInputConflict(input.name.value)))
            case Some(_) => Right(seen)
            case None    => Right(seen :+ input)
      .map(_ => ())

  private def concurrency(file: WorkflowFile, jobs: List[Placed]): Either[BuildError, Unit] =
    val chosen = jobs.flatMap(job => view(job.render).concurrency)
    if chosen.distinct.sizeIs > 1 then Left(BuildError.Ci(CiFault.ConcurrencyConflict(label(file))))
    else Right(())

  private def workflows(
      graph: ProjectGraph,
      placed: List[Placed],
      picture: Picture,
  ): Either[BuildError, Unit] =
    val known = knownWorkflows(graph, placed)
    picture.workflows.find(name => !known.contains(name)) match
      case Some(name) => Left(BuildError.Ci(CiFault.UnpinnedWorkflowRun(name)))
      case None       => Right(())

  def knownWorkflows(graph: ProjectGraph, placed: List[Placed]): Set[String] =
    val emitted = placed
      .map(_.file)
      .collect {
        case WorkflowFile.Pages       => "histos-pages.yml"
        case WorkflowFile.Named(name) => name
      }
      .toSet + "ci.yml"
    val release =
      if graph.catalog == Catalog.Empty then Set.empty[String]
      else Set("histos-release.yml", "histos-promote.yml")
    emitted ++ release ++ graph.ci.workflows

  private def eventsFor(file: WorkflowFile, jobs: List[Placed]): Picture =
    file match
      case WorkflowFile.Ci =>
        val extra = jobs.flatMap(job => eventsOf(job.render)).filterNot(On.splits)
        Picture(base ++ extra, cron = false)
      case WorkflowFile.Pages =>
        Picture(List(On.Dispatch(Nil)), cron = false)
      case WorkflowFile.Named(_) =>
        val events = jobs.flatMap(job => eventsOf(job.render))
        val cron   = jobs.exists(job => job.render.isInstanceOf[JobRender.Schedule])
        Picture(events, cron)

  private val base: List[On] =
    List(On.pullRequest, On.pushDefault, On.Dispatch(Nil))

  private def heard(picture: Picture, kind: EventKind): Boolean =
    kind match
      case EventKind.PullRequest => picture.events.exists(_.isInstanceOf[On.PullRequest])
      case EventKind.Push        => pushes(picture)
      case EventKind.Dispatch    => picture.events.exists(_.isInstanceOf[On.Dispatch])
      case EventKind.Schedule    => picture.cron
      case EventKind.MergeGroup  => picture.events.contains(On.MergeGroup)
      case EventKind.Release     => picture.events.contains(On.Release)
      case EventKind.WorkflowRun => picture.workflows.nonEmpty
      case EventKind.Tag         => tagged(picture)

  private def pushes(picture: Picture): Boolean =
    picture.events.exists(_.isInstanceOf[On.Push])

  private def tagged(picture: Picture): Boolean =
    picture.events.exists:
      case On.Push(_, tags, _) => tags.nonEmpty
      case _                   => false

  private def labeled(picture: Picture): Boolean =
    picture.events.exists:
      case On.PullRequest(types, _) =>
        types.contains(PullType.Labeled) || types.contains(PullType.Unlabeled)
      case _ => false

  private def mergeInputs(events: List[On]): List[DispatchInput] =
    events.collect { case On.Dispatch(inputs) => inputs }.flatten.distinctBy(_.name)

  private def place(item: Item): Placed =
    val file = item.render match
      case JobRender.Schedule(_, job)                             => WorkflowFile.Named(fileName(job.name))
      case JobRender.Site(_, SiteShape.Dispatch)                  => WorkflowFile.Pages
      case JobRender.Site(job, _) if job.events.exists(On.splits) => WorkflowFile.Named(fileName(job.name))
      case JobRender.Task(job) if job.events.exists(On.splits)    => WorkflowFile.Named(fileName(job.name))
      case JobRender.Fan(job, _) if job.events.exists(On.splits)  => WorkflowFile.Named(fileName(job.name))
      case _                                                      => WorkflowFile.Ci
    Placed(file, item.plugin, item.render)

  private def fileName(name: JobName): String = s"histos-${name.value}.yml"

  private def printed(render: JobRender): Cond =
    render match
      case JobRender.Site(_, SiteShape.Dispatch) => Cond.True
      case other                                 => view(other).cond

  private def eventsOf(render: JobRender): List[On] =
    view(render).events

  private def needsOf(render: JobRender): List[JobName] =
    view(render).needs

  private def nameOf(render: JobRender): JobName =
    view(render).name

  private def isSite(render: JobRender): Boolean =
    render match
      case JobRender.Site(_, _) => true
      case _                    => false

  private def view(render: JobRender): View =
    render match
      case JobRender.Task(job)         => seen(job, Nil)
      case JobRender.Schedule(_, job)  => seen(job, Nil)
      case JobRender.Fan(job, targets) => seen(job, targets)
      case JobRender.Site(job, _)      =>
        View(
          job.name,
          job.permissions,
          job.runner,
          job.env,
          job.pre,
          job.cond,
          job.concurrency,
          job.events,
          Nil,
          job.needs,
        )

  private def seen(job: TaskArrange, targets: List[Target]): View =
    View(
      job.name,
      job.permissions,
      job.runner,
      job.env,
      job.pre,
      job.cond,
      job.concurrency,
      job.events,
      targets,
      job.needs,
    )

  private def secretValues(values: List[EnvValue]): List[SecretName] =
    values.collect { case EnvValue.Secret(name) => name }

  private def pairSecrets(pairs: List[(EnvName, EnvValue)]): List[SecretName] =
    pairs.collect { case (_, EnvValue.Secret(name)) => name }

  private def affected(cond: Cond): List[ProjectName] =
    cond match
      case Cond.Affected(name) => List(name)
      case Cond.All(parts)     => parts.flatMap(affected)
      case Cond.AnyOf(parts)   => parts.flatMap(affected)
      case Cond.Not(inner)     => affected(inner)
      case _                   => Nil

  private def grouped(placed: List[Placed]): List[(WorkflowFile, List[Placed])] =
    placed.foldLeft(List.empty[(WorkflowFile, List[Placed])]): (acc, item) =>
      acc.find(_._1 == item.file) match
        case None               => acc :+ (item.file, List(item))
        case Some((file, jobs)) =>
          acc.map: pair =>
            if pair._1 == file then (file, jobs :+ item) else pair

  private def label(file: WorkflowFile): String =
    file match
      case WorkflowFile.Ci          => "ci.yml"
      case WorkflowFile.Pages       => "histos-pages.yml"
      case WorkflowFile.Named(name) => name

  private def duplicate[A](values: List[A]): Option[A] =
    values
      .foldLeft[(Set[A], Option[A])]((Set.empty, None)):
        case ((_, found @ Some(_)), _) => (Set.empty, found)
        case ((seen, None), value)     =>
          if seen.contains(value) then (seen, Some(value)) else (seen + value, None)
      ._2

  private def fold[A](items: List[A])(check: A => Either[BuildError, Unit]): Either[BuildError, Unit] =
    items.foldLeft[Either[BuildError, Unit]](Right(())): (acc, item) =>
      acc.flatMap(_ => check(item))

  private final case class View(
      name: JobName,
      permissions: JobPermissions,
      runner: Runner,
      env: List[(EnvName, EnvValue)],
      pre: List[PinnedAction],
      cond: Cond,
      concurrency: Option[Concurrency],
      events: List[On],
      targets: List[Target],
      needs: List[JobName],
  )

  private final case class Picture(events: List[On], cron: Boolean):
    def inputs: List[DispatchInput] =
      events.collect { case On.Dispatch(found) => found }.flatten

    def workflows: List[String] =
      events.collect { case On.WorkflowRun(names) => names }.flatten
  end Picture
end JobCheck
