package histos

import histos.plugin.{
  DeclaredInput,
  EntryName,
  HistosPlugin,
  ModuleNote,
  Need,
  PluginId,
  RegisteredPlugin,
  RepoPath,
  TaskDep,
  TaskName,
}
import histos.version.Catalog

import _root_.scala.util.control.NonFatal

import zio.ZLayer

sealed trait Project:
  def name: ProjectName
  def position: SourcePos

/** An upstream module. No platforms means every platform the depender builds.
  */
final case class ProjectDep(
    name: ProjectName,
    platforms: Option[List[Platform]],
):
  def applies(platform: Platform): Boolean =
    platforms match
      case None         => true
      case Some(chosen) => chosen.contains(platform)

final case class Module(
    name: ProjectName,
    platforms: Platforms,
    versions: ScalaVersions,
    dependencies: List[Dep],
    dependsOn: List[ProjectDep],
    position: SourcePos,
    clang: ClangPin = ClangPin.Unused,
    test: Option[TestMain] = None,
    testDependencies: List[Dep] = Nil,
    provider: Option[TestProvider] = None,
    nativeVersion: Option[NativeVersion] = None,
    jsVersion: Option[JsVersion] = None,
    directory: Option[ProjectPath] = None,
    jsEnv: JsEnv = JsEnv.Node,
    jsModule: JsModuleKind = JsModuleKind.NoModule,
    application: Option[MainClass] = None,
    notes: List[ModuleNote] = Nil,
    flags: ScalacFlags = ScalacFlags.empty,
    published: Option[ArtifactBase] = None,
    publication: Publication = Publication.Library,
    asset: Option[RepoPath] = None,
    testStore: TestStore = TestStore.Off,
    needs: Set[Need] = Set.empty,
) extends Project:
  /** `<directory>/src`, or `<name>/src` when the module does not name a directory.
    */
  def baseDirectory: String =
    directory match
      case Some(path) => path.value
      case None       => name.value

  /** Upstream names on `platform`. Empty when this module does not build that platform.
    */
  def upstream(platform: Platform): List[ProjectName] =
    if platforms.all.contains(platform) then dependsOn.filter(_.applies(platform)).map(_.name)
    else Nil

  /** Every upstream name, whichever platform the edge names. */
  def upstreamNames: List[ProjectName] =
    dependsOn.map(_.name)
end Module

object Module:
  /** Upstream modules on `platform`, each dependency before the module that uses it.
    */
  def chain(
      module: Module,
      modules: List[Module],
      platform: Platform,
  ): List[Module] =
    val byName = modules.map(item => item.name -> item).toMap
    def walk(
        names: List[ProjectName],
        seen: Set[ProjectName],
        acc: List[Module],
    ): (Set[ProjectName], List[Module]) =
      names.foldLeft((seen, acc)):
        case ((have, built), name) if have.contains(name) => (have, built)
        case ((have, built), name)                        =>
          byName.get(name) match
            case None             => (have + name, built)
            case Some(dependency) =>
              val (next, below) =
                walk(dependency.upstream(platform), have + name, built)
              (next, below :+ dependency)
    walk(module.upstream(platform), Set(module.name), Nil)._2

  /** Named modules plus every `upstreamNames` dependency, in `modules` order. Not the platform-filtered chain.
    */
  def closure(modules: List[Module], roots: Set[ProjectName]): List[Module] =
    val byName                                         = modules.map(module => module.name -> module).toMap
    def grow(have: Set[ProjectName]): Set[ProjectName] =
      val extra =
        have.flatMap(name => byName.get(name).toList.flatMap(_.upstreamNames))
      if extra.subsetOf(have) then have else grow(have ++ extra)
    val needed = grow(roots)
    modules.filter(module => needed.contains(module.name))
end Module

final case class Aggregate(
    name: ProjectName,
    children: List[ProjectName],
    position: SourcePos,
) extends Project

/** The project list is fixed once this value exists. Lookup is total: a missing name is `None`.
  */
final case class ProjectGraph private (
    projects: List[Project],
    splits: Splits,
    catalog: Catalog,
    organization: Option[Organization],
    resolvers: List[Resolver],
    plugins: List[RegisteredPlugin] = Nil,
    tools: ToolCatalog = ToolCatalog.default,
    runnerTools: RunnerTools = RunnerTools.empty,
    facts: OrganizationFacts = OrganizationFacts.empty,
    promote: Promote = Promote.Rebuild,
):
  def get(name: ProjectName): Option[Project] =
    projects.find(_.name == name)

object ProjectGraph:
  private[histos] def trusted(projects: List[Project]): ProjectGraph =
    trusted(projects, Splits.aggregate)

  private[histos] def trusted(
      projects: List[Project],
      splits: Splits,
  ): ProjectGraph =
    trusted(projects, splits, Catalog.empty)

  private[histos] def trusted(
      projects: List[Project],
      splits: Splits,
      catalog: Catalog,
  ): ProjectGraph =
    trusted(projects, splits, catalog, None)

  private[histos] def trusted(
      projects: List[Project],
      splits: Splits,
      catalog: Catalog,
      organization: Option[Organization],
  ): ProjectGraph =
    trusted(projects, splits, catalog, organization, Nil)

  private[histos] def trusted(
      projects: List[Project],
      splits: Splits,
      catalog: Catalog,
      organization: Option[Organization],
      resolvers: List[Resolver],
      plugins: List[RegisteredPlugin] = Nil,
      tools: ToolCatalog = ToolCatalog.default,
      runnerTools: RunnerTools = RunnerTools.empty,
      facts: OrganizationFacts = OrganizationFacts.empty,
      promote: Promote = Promote.Rebuild,
  ): ProjectGraph =
    new ProjectGraph(
      projects,
      splits,
      catalog,
      organization,
      resolvers,
      plugins,
      tools,
      runnerTools,
      facts,
      promote,
    )
end ProjectGraph

trait HistosBuild:
  def graph: Either[BuildError, ProjectGraph]
  def local: ZLayer[LocalPublish, Nothing, Publishing] = Local.layer
  def sona: ZLayer[SonaPublish, Nothing, Publishing]   = Sona.layer(ReleaseMode.Automatic)

object HistosBuild:
  /** A build whose graph is already known. `local` and `sona` stay the trait defaults. */
  def fixed(graph: ProjectGraph): HistosBuild =
    val found = graph
    new HistosBuild:
      def graph: Either[BuildError, ProjectGraph] = Right(found)

object Projects:
  def tools(catalog: ToolCatalog): Organized =
    Organized(None, Nil, tools = catalog)

  /** Empty installs nothing. A build that never calls this asks the runner for no tools. */
  def runnerTools(tools: RunnerTools): Organized =
    Organized(None, Nil, runnerTools = tools)

  /** Every published module uses this organization. A build that does not publish omits it.
    */
  def organization(organization: Organization): Organized =
    Organized(Some(organization), Nil)

  /** Repositories this build resolves from. The list is not Central unless `central` is in it. An empty list, from
    * [[apply]], is Central.
    */
  def resolvers(first: Resolver, rest: Resolver*): Organized =
    Organized(None, first :: rest.toList)

  /** How a push to main fills the main index. Omission, from [[apply]], is rebuild. */
  def promote(value: Promote): Organized =
    Organized(None, Nil, promote = value)

  def apply(defs: ProjectDef*): Either[BuildError, ProjectGraph] =
    build(None, Nil, Splits.aggregate, CatalogSpec.empty, defs.toList)

  def apply(
      splits: Splits,
      defs: ProjectDef*
  ): Either[BuildError, ProjectGraph] =
    build(None, Nil, splits, CatalogSpec.empty, defs.toList)

  def apply(
      catalog: CatalogSpec,
      defs: ProjectDef*
  ): Either[BuildError, ProjectGraph] =
    build(None, Nil, Splits.aggregate, catalog, defs.toList)

  def apply(
      splits: Splits,
      catalog: CatalogSpec,
      defs: ProjectDef*
  ): Either[BuildError, ProjectGraph] =
    build(None, Nil, splits, catalog, defs.toList)

  def apply(uses: Uses, defs: ProjectDef*): Either[BuildError, ProjectGraph] =
    build(None, Nil, Splits.aggregate, CatalogSpec.empty, defs.toList, uses)

  def apply(
      uses: Uses,
      splits: Splits,
      defs: ProjectDef*
  ): Either[BuildError, ProjectGraph] =
    build(None, Nil, splits, CatalogSpec.empty, defs.toList, uses)

  def apply(
      uses: Uses,
      catalog: CatalogSpec,
      defs: ProjectDef*
  ): Either[BuildError, ProjectGraph] =
    build(None, Nil, Splits.aggregate, catalog, defs.toList, uses)

  def apply(
      uses: Uses,
      splits: Splits,
      catalog: CatalogSpec,
      defs: ProjectDef*
  ): Either[BuildError, ProjectGraph] =
    build(None, Nil, splits, catalog, defs.toList, uses)

  private[histos] def assemble(
      organization: Option[Organization],
      resolvers: List[Resolver],
      splits: Splits,
      catalog: CatalogSpec,
      listed: List[ProjectDef],
      uses: Uses = Uses(Nil),
      tools: ToolCatalog = ToolCatalog.default,
      runnerTools: RunnerTools = RunnerTools.empty,
      facts: OrganizationFacts = OrganizationFacts.empty,
      promote: Promote = Promote.Rebuild,
  ): Either[BuildError, ProjectGraph] =
    build(
      organization,
      resolvers,
      splits,
      catalog,
      listed,
      uses,
      tools,
      runnerTools,
      facts,
      promote,
    )

  private def build(
      organization: Option[Organization],
      resolvers: List[Resolver],
      splits: Splits,
      catalog: CatalogSpec,
      listed: List[ProjectDef],
      uses: Uses = Uses(Nil),
      tools: ToolCatalog = ToolCatalog.default,
      runnerTools: RunnerTools = RunnerTools.empty,
      facts: OrganizationFacts = OrganizationFacts.empty,
      promote: Promote = Promote.Rebuild,
  ): Either[BuildError, ProjectGraph] =
    for
      registered <- registerAll(uses)
      _          <- checkPlugins(registered)
      named      <- nameAll(listed)
      _          <- duplicates(named)
      _          <- shape(named)
      built      <- links(named)
      _          <- directories(built)
      _          <- cycle(built)
      _          <- bindPlugins(registered, built)
      ships      <- ShipLink.catalog(catalog, built)
      _          <- uniqueResolvers(resolvers)
    yield ProjectGraph.trusted(
      built,
      splits,
      ships,
      organization,
      resolvers,
      registered,
      tools,
      runnerTools,
      facts,
      promote,
    )

  private def uniqueResolvers(
      resolvers: List[Resolver]
  ): Either[BuildError, Unit] =
    resolvers
      .foldLeft[Either[BuildError, List[Resolver]]](Right(Nil)): (acc, resolver) =>
        acc.flatMap: seen =>
          if seen.contains(resolver) then Left(BuildError.DuplicateResolver(token(resolver)))
          else Right(seen :+ resolver)
      .map(_ => ())

  private def token(resolver: Resolver): String = resolver match
    case Resolver.Central    => "central"
    case Resolver.Maven(url) => url.value

  private def nameAll(defs: List[ProjectDef]): Either[BuildError, List[Named]] =
    defs.foldLeft[Either[BuildError, List[Named]]](Right(Nil)): (acc, d) =>
      acc.flatMap: soFar =>
        ProjectName.from(d.name) match
          case Left(ProjectNameError.Empty) =>
            Left(BuildError.EmptyName(d.position))
          case Left(ProjectNameError.Bad(_)) =>
            Left(BuildError.BadName(d.name, d.position))
          case Right(name) => Right(soFar :+ Named(name, d))

  private def duplicates(named: List[Named]): Either[BuildError, Unit] =
    named
      .foldLeft[Either[BuildError, List[Named]]](Right(Nil)): (acc, n) =>
        acc.flatMap: seen =>
          seen.find(_.name == n.name) match
            case Some(first) =>
              Left(
                BuildError
                  .DuplicateName(n.name, first.defn.position, n.defn.position)
              )
            case None => Right(seen :+ n)
      .map(_ => ())

  private def shape(named: List[Named]): Either[BuildError, Unit] =
    named.flatMap(shapeOf) match
      case err :: _ => Left(err)
      case Nil      => Right(())

  private def shapeOf(n: Named): Option[BuildError] =
    n.defn match
      case m: ModuleBody[?] =>
        duplicate(m.platforms.all)
          .map(BuildError.DuplicatePlatform(n.name, _, m.position))
          .orElse(
            duplicate(m.versions.all).map(
              BuildError.DuplicateScalaVersion(n.name, _, m.position)
            )
          )
          .orElse(clangOf(n.name, m))
          .orElse(duplicateNote(n.name, m))
      case a: AggregateDef =>
        val children = a.children.map(_.name)
        if children.contains(a.name) then Some(BuildError.AggregateIncludesItself(n.name, a.position))
        else
          duplicate(children).map: raw =>
            ProjectName.from(raw) match
              case Right(child) =>
                BuildError.DuplicateChild(n.name, child, a.position)
              case Left(_) => BuildError.BadName(raw, a.position)

  private def links(named: List[Named]): Either[BuildError, List[Project]] =
    val byName = named.map(n => n.name.value -> n).toMap
    first(named.map(n => link(n, byName)))

  private def link(
      n: Named,
      byName: Map[String, Named],
  ): Either[BuildError, Project] =
    n.defn match
      case m: ModuleBody[?] => linkModule(n, m, byName)
      case a: AggregateDef  => linkAggregate(n, a, byName)

  private def linkModule(
      n: Named,
      m: ModuleBody[?],
      byName: Map[String, Named],
  ): Either[BuildError, Project] =
    m.dependsOn
      .foldLeft[Either[BuildError, List[ProjectDep]]](Right(Nil)): (acc, edge) =>
        acc.flatMap: deps =>
          lookup(edge.project, byName) match
            case Left(err)                              => Left(err)
            case Right(target) if target.name == n.name =>
              Left(BuildError.SelfDependency(n.name, m.position))
            case Right(target) =>
              target.defn match
                case _: AggregateDef =>
                  Left(
                    BuildError
                      .DependsOnAggregate(n.name, target.name, m.position)
                  )
                case upstream: ModuleBody[?] =>
                  if deps.exists(_.name == target.name) then
                    Left(
                      BuildError
                        .DuplicateDependency(n.name, target.name, m.position)
                    )
                  else
                    accept(n.name, m, target.name, upstream, edge).map(
                      deps :+ _
                    )
      .flatMap: deps =>
        if m.testStore == TestStore.Remote && m.needs.nonEmpty then
          Left(BuildError.RemoteNeeds(n.name, m.needs, m.position))
        else
          (m.test, m.provider) match
            case (Some(_), Some(_)) =>
              Left(BuildError.MixedTest(n.name, m.position))
            case _ =>
              publicationOf(n.name, m).map: (publication, asset) =>
                Module(
                  n.name,
                  m.platforms,
                  m.versions,
                  m.dependencies,
                  deps,
                  m.position,
                  m.clang,
                  m.test,
                  m.testDependencies,
                  m.provider,
                  m.nativeVersion,
                  m.jsVersion,
                  m.directory,
                  m.jsEnv,
                  m.jsModule,
                  m.mainClass,
                  m.notes,
                  m.flags,
                  m.published,
                  publication,
                  asset,
                  m.testStore,
                  m.needs,
                )

  private def publicationOf(
      name: ProjectName,
      module: ModuleBody[?],
  ): Either[BuildError, (Publication, Option[RepoPath])] =
    val chosen = module.publications match
      case Nil       => Right(Publication.Library)
      case List(one) =>
        one match
          case Publication.Assembly(_)
              if module.platforms.all != List(Platform.Jvm) || module.versions.all.lengthIs != 1 =>
            Left(BuildError.AssemblyShape(name, module.position))
          case found => Right(found)
      case _ => Left(BuildError.TwoPublications(name, module.position))
    chosen.flatMap: publication =>
      (publication, module.assets) match
        case (Publication.Assembly(_), Nil)        => Right((publication, None))
        case (Publication.Assembly(_), List(file)) => Right((publication, Some(file)))
        case (Publication.Assembly(_), _)          => Left(BuildError.TwoAssets(name, module.position))
        case (_, Nil)                              => Right((publication, None))
        case _                                     => Left(BuildError.AssetWithoutAssembly(name, module.position))

  private def linkAggregate(
      n: Named,
      a: AggregateDef,
      byName: Map[String, Named],
  ): Either[BuildError, Project] =
    a.children
      .foldLeft[Either[BuildError, List[ProjectName]]](Right(Nil)): (acc, child) =>
        acc.flatMap: names =>
          lookup(child, byName).flatMap: target =>
            target.defn match
              case _: ModuleDef[?] => Right(names :+ target.name)
              case _               =>
                Left(
                  BuildError.AggregateChild(n.name, target.name, a.position)
                )
      .map(children => Aggregate(n.name, children, a.position))

  private def clangOf(
      name: ProjectName,
      module: ModuleBody[?],
  ): Option[BuildError] =
    module.clang match
      case ClangPin.Unused if module.platforms.all.contains(Platform.Native) =>
        Some(BuildError.MissingClang(name, module.position))
      case _ => None

  private def directories(projects: List[Project]): Either[BuildError, Unit] =
    val modules = projects.collect { case module: Module => module }
    modules
      .foldLeft[Either[BuildError, List[Module]]](Right(Nil)): (acc, module) =>
        acc.flatMap: seen =>
          seen.find(_.baseDirectory == module.baseDirectory) match
            case Some(first) =>
              Left(
                BuildError.DuplicateDirectory(
                  module.name,
                  first.name,
                  module.baseDirectory,
                )
              )
            case None => Right(seen :+ module)
      .map(_ => ())

  /** The platforms an edge actually builds. An unscoped edge is every platform of the depender.
    */
  private def accept(
      module: ProjectName,
      depender: ModuleBody[?],
      target: ProjectName,
      upstream: ModuleBody[?],
      edge: ProjectEdge,
  ): Either[BuildError, ProjectDep] =
    val wanted = edge.platforms match
      case Some(chosen) => chosen
      case None         => depender.platforms.all
    wanted.find(platform => !depender.platforms.all.contains(platform)) match
      case Some(platform) =>
        Left(
          BuildError.EdgeWithoutPlatform(module, platform, depender.position)
        )
      case None =>
        wanted.find(platform => !upstream.platforms.all.contains(platform)) match
          case Some(platform) =>
            Left(
              BuildError.UpstreamMissingPlatform(
                module,
                target,
                platform,
                depender.position,
              )
            )
          case None => Right(ProjectDep(target, edge.platforms))

  private def cycle(projects: List[Project]): Either[BuildError, Unit] =
    val modules = projects.collect { case module: Module => module }
    List(Platform.Jvm, Platform.Js, Platform.Native)
      .foldLeft[Either[BuildError, Unit]](Right(())): (acc, platform) =>
        acc.flatMap: _ =>
          val edges =
            modules.map(module => module.name -> module.upstream(platform))
          findCycle(edges) match
            case Some(path) => Left(BuildError.Cycle(platform, path))
            case None       => Right(())

  /** `path` is newest-first. A name already on it is the cycle. */
  private def findCycle(
      edges: List[(ProjectName, List[ProjectName])]
  ): Option[List[ProjectName]] =
    val deps = edges.toMap
    def walk(
        name: ProjectName,
        path: List[ProjectName],
        done: Set[ProjectName],
    ): (Option[List[ProjectName]], Set[ProjectName]) =
      if done.contains(name) then (None, done)
      else if path.contains(name) then
        val chain = (name :: path).reverse.dropWhile(_ != name)
        (Some(chain), done)
      else
        val (found, next) = deps
          .getOrElse(name, Nil)
          .foldLeft((Option.empty[List[ProjectName]], done)):
            case ((Some(c), d), _)  => (Some(c), d)
            case ((None, d), child) => walk(child, name :: path, d)
        found match
          case Some(c) => (Some(c), next)
          case None    => (None, next + name)

    edges
      .foldLeft((Option.empty[List[ProjectName]], Set.empty[ProjectName])):
        case ((Some(c), done), _)      => (Some(c), done)
        case ((None, done), (name, _)) => walk(name, Nil, done)
      ._1
  end findCycle

  private def lookup(
      ref: ProjectDef,
      byName: Map[String, Named],
  ): Either[BuildError, Named] =
    byName.get(ref.name) match
      case Some(found) => Right(found)
      case None        => Left(BuildError.MissingProject(ref.name, ref.position))

  private def first[A](
      items: List[Either[BuildError, A]]
  ): Either[BuildError, List[A]] =
    items.foldLeft[Either[BuildError, List[A]]](Right(Nil)): (acc, item) =>
      acc.flatMap(soFar => item.map(soFar :+ _))

  private def duplicate[A](items: List[A]): Option[A] =
    items
      .foldLeft[(List[A], Option[A])]((Nil, None)):
        case ((_, Some(found)), _) => (Nil, Some(found))
        case ((seen, None), item)  =>
          if seen.contains(item) then (seen, Some(item))
          else (item :: seen, None)
      ._2

  private def duplicateNote(
      name: ProjectName,
      module: ModuleBody[?],
  ): Option[BuildError] =
    duplicate(module.notes.map(note => (note.plugin, note.key))) match
      case Some((plugin, key)) =>
        Some(BuildError.DuplicateNote(name, plugin, key))
      case None => None

  private def registerAll(
      uses: Uses
  ): Either[BuildError, List[RegisteredPlugin]] =
    uses.plugins.foldLeft[Either[BuildError, List[RegisteredPlugin]]](
      Right(Nil)
    ): (acc, plugin) =>
      acc.flatMap(soFar => register(plugin).map(soFar :+ _))

  private def register(
      plugin: HistosPlugin
  ): Either[BuildError, RegisteredPlugin] =
    val contributed =
      try Right(plugin.contribute)
      catch
        case NonFatal(err) =>
          Left(BuildError.PluginDefect(plugin.id, err.getClass.getName))
    contributed.flatMap: contribution =>
      EntryName.fromClass(plugin.getClass) match
        case Right(entry) =>
          Right(RegisteredPlugin(plugin.id, entry, contribution))
        case Left(_) =>
          Left(BuildError.PluginDefect(plugin.id, plugin.getClass.getName))

  private def checkPlugins(
      registered: List[RegisteredPlugin]
  ): Either[BuildError, Unit] =
    for
      _ <- uniquePlugins(registered)
      _ <- each(registered)(checkOne)
      _ <- uniqueNames(
        registered.flatMap(_.contribution.commands.map(_.name)),
        BuildError.DuplicateCommand(_),
      )
      _ <- uniqueNames(
        registered.flatMap(_.contribution.jobs.map(_.name)),
        BuildError.DuplicateJob(_),
      )
    yield ()

  private def uniquePlugins(
      registered: List[RegisteredPlugin]
  ): Either[BuildError, Unit] =
    registered
      .foldLeft[Either[BuildError, List[PluginId]]](Right(Nil)): (acc, plugin) =>
        acc.flatMap: seen =>
          if seen.contains(plugin.id) then Left(BuildError.DuplicatePlugin(plugin.id))
          else Right(seen :+ plugin.id)
      .map(_ => ())

  private def each[A](
      items: List[A]
  )(check: A => Either[BuildError, Unit]): Either[BuildError, Unit] =
    items.foldLeft[Either[BuildError, Unit]](Right(())): (acc, item) =>
      acc.flatMap(_ => check(item))

  private def uniqueNames[A](
      names: List[A],
      error: A => BuildError,
  ): Either[BuildError, Unit] =
    duplicate(names) match
      case Some(name) => Left(error(name))
      case None       => Right(())

  private def checkOne(plugin: RegisteredPlugin): Either[BuildError, Unit] =
    val tasks   = plugin.contribution.tasks.map(_.name)
    val missing = plugin.contribution.tasks
      .flatMap(_.dependsOn)
      .collect:
        case TaskDep.Same(name) if !tasks.contains(name) => name
    duplicate(tasks)
      .map(BuildError.DuplicatePluginTask(plugin.id, _))
      .orElse(
        missing.headOption.map(BuildError.UnknownPluginTask(plugin.id, _))
      )
      .orElse(commandProblem(plugin, tasks))
      .orElse(jobProblem(plugin, tasks)) match
      case Some(err) => Left(err)
      case None      => Right(())

  private def commandProblem(
      plugin: RegisteredPlugin,
      tasks: List[TaskName],
  ): Option[BuildError] =
    plugin.contribution.commands.collectFirst:
      case spec if spec.name.isReserved       => BuildError.CommandShadow(spec.name)
      case spec if !tasks.contains(spec.task) =>
        BuildError.UnknownPluginTask(plugin.id, spec.task)

  private def jobProblem(
      plugin: RegisteredPlugin,
      tasks: List[TaskName],
  ): Option[BuildError] =
    plugin.contribution.jobs.collectFirst:
      case job if job.name.isReserved       => BuildError.ReservedJob(job.name)
      case job if !tasks.contains(job.task) =>
        BuildError.UnknownPluginTask(plugin.id, job.task)

  private def bindPlugins(
      registered: List[RegisteredPlugin],
      projects: List[Project],
  ): Either[BuildError, Unit] =
    val modules = projects.collect { case module: Module =>
      module.name -> module
    }.toMap
    val problem = registered.flatMap: plugin =>
      plugin.contribution.tasks.flatMap: task =>
        task.inputs.flatMap(inputOf(plugin, modules, _)) ++ task.dependsOn
          .flatMap(depOf(plugin, modules, _))
    problem.headOption match
      case Some(err) => Left(err)
      case None      => Right(())

  private def inputOf(
      plugin: RegisteredPlugin,
      modules: Map[ProjectName, Module],
      input: DeclaredInput,
  ): Option[BuildError] =
    input match
      case DeclaredInput.File(_) | DeclaredInput.Directory(_) | DeclaredInput.Text(_, _) | DeclaredInput.Env(_) =>
        None
      case DeclaredInput.Module(name, version, platform) =>
        cellOf(plugin, modules, name, version, platform)
      case DeclaredInput.Note(name, key) =>
        modules.get(name) match
          case None         => Some(BuildError.PluginMissingModule(plugin.id, name))
          case Some(module) =>
            val present = module.notes.exists(note => note.plugin == plugin.id && note.key == key)
            if present then None
            else Some(BuildError.MissingNote(name, plugin.id, key))

  private def depOf(
      plugin: RegisteredPlugin,
      modules: Map[ProjectName, Module],
      dep: TaskDep,
  ): Option[BuildError] =
    dep match
      case TaskDep.Same(_)                          => None
      case TaskDep.Compile(name, version, platform) =>
        cellOf(plugin, modules, name, version, platform)

  private def cellOf(
      plugin: RegisteredPlugin,
      modules: Map[ProjectName, Module],
      name: ProjectName,
      version: Option[ScalaVersion],
      platform: Option[Platform],
  ): Option[BuildError] =
    modules.get(name) match
      case None         => Some(BuildError.PluginMissingModule(plugin.id, name))
      case Some(module) =>
        val versionOk  = version.forall(module.versions.all.contains)
        val platformOk = platform.forall(module.platforms.all.contains)
        if versionOk && platformOk then None
        else
          Some(
            BuildError.PluginMissingCell(
              plugin.id,
              module.name,
              version,
              platform,
            )
          )

  private final case class Named(name: ProjectName, defn: ProjectDef)
end Projects

/** [[Projects]] with an organization, repositories, or both already chosen. */
final class Organized(
    organization: Option[Organization],
    resolvers: List[Resolver],
    tools: ToolCatalog = ToolCatalog.default,
    runnerTools: RunnerTools = RunnerTools.empty,
    facts: OrganizationFacts = OrganizationFacts.empty,
    promote: Promote = Promote.Rebuild,
):
  def tools(catalog: ToolCatalog): Organized =
    Organized(organization, resolvers, catalog, runnerTools, facts, promote)

  def organization(value: Organization): Organized =
    Organized(Some(value), resolvers, tools, runnerTools, facts, promote)

  def resolvers(first: Resolver, rest: Resolver*): Organized =
    Organized(
      organization,
      resolvers ++ (first :: rest.toList),
      tools,
      runnerTools,
      facts,
      promote,
    )

  def runnerTools(value: RunnerTools): Organized =
    Organized(organization, resolvers, tools, value, facts, promote)

  def promote(value: Promote): Organized =
    Organized(organization, resolvers, tools, runnerTools, facts, value)

  /** Written once. A second call replaces it. */
  def license(value: License): Organized =
    withFacts(facts.copy(license = Some(value)))

  /** Written once. A second call replaces it. */
  def developer(value: Developer): Organized =
    withFacts(facts.copy(developer = Some(value)))

  /** Written once. A second call replaces it. */
  def scm(value: Scm): Organized =
    withFacts(facts.copy(scm = Some(value)))

  /** The description a ship may replace. */
  def description(text: PomText): Organized =
    withFacts(facts.copy(description = Some(text)))

  /** The url a ship may replace. */
  def url(value: HttpUrl): Organized =
    withFacts(facts.copy(url = Some(value)))

  private def withFacts(value: OrganizationFacts): Organized =
    Organized(organization, resolvers, tools, runnerTools, value, promote)

  def apply(defs: ProjectDef*): Either[BuildError, ProjectGraph] =
    Projects.assemble(
      organization,
      resolvers,
      Splits.aggregate,
      CatalogSpec.empty,
      defs.toList,
      tools = tools,
      runnerTools = runnerTools,
      facts = facts,
      promote = promote,
    )

  def apply(
      splits: Splits,
      defs: ProjectDef*
  ): Either[BuildError, ProjectGraph] =
    Projects.assemble(
      organization,
      resolvers,
      splits,
      CatalogSpec.empty,
      defs.toList,
      tools = tools,
      runnerTools = runnerTools,
      facts = facts,
      promote = promote,
    )

  def apply(
      catalog: CatalogSpec,
      defs: ProjectDef*
  ): Either[BuildError, ProjectGraph] =
    Projects.assemble(
      organization,
      resolvers,
      Splits.aggregate,
      catalog,
      defs.toList,
      tools = tools,
      runnerTools = runnerTools,
      facts = facts,
      promote = promote,
    )

  def apply(
      splits: Splits,
      catalog: CatalogSpec,
      defs: ProjectDef*
  ): Either[BuildError, ProjectGraph] =
    Projects.assemble(
      organization,
      resolvers,
      splits,
      catalog,
      defs.toList,
      tools = tools,
      runnerTools = runnerTools,
      facts = facts,
      promote = promote,
    )

  def apply(uses: Uses, defs: ProjectDef*): Either[BuildError, ProjectGraph] =
    Projects.assemble(
      organization,
      resolvers,
      Splits.aggregate,
      CatalogSpec.empty,
      defs.toList,
      uses,
      tools,
      runnerTools,
      facts,
      promote,
    )

  def apply(
      uses: Uses,
      splits: Splits,
      defs: ProjectDef*
  ): Either[BuildError, ProjectGraph] =
    Projects.assemble(
      organization,
      resolvers,
      splits,
      CatalogSpec.empty,
      defs.toList,
      uses,
      tools,
      runnerTools,
      facts,
      promote,
    )

  def apply(
      uses: Uses,
      catalog: CatalogSpec,
      defs: ProjectDef*
  ): Either[BuildError, ProjectGraph] =
    Projects.assemble(
      organization,
      resolvers,
      Splits.aggregate,
      catalog,
      defs.toList,
      uses,
      tools,
      runnerTools,
      facts,
      promote,
    )

  def apply(
      uses: Uses,
      splits: Splits,
      catalog: CatalogSpec,
      defs: ProjectDef*
  ): Either[BuildError, ProjectGraph] =
    Projects.assemble(
      organization,
      resolvers,
      splits,
      catalog,
      defs.toList,
      uses,
      tools,
      runnerTools,
      facts,
      promote,
    )
end Organized
