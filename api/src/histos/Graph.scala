package histos

import histos.version.Catalog

sealed trait Project:
  def name: ProjectName
  def position: SourcePos

final case class Module(
    name: ProjectName,
    platforms: Platforms,
    versions: ScalaVersions,
    dependencies: List[Dep],
    dependsOn: List[ProjectName],
    position: SourcePos,
    clang: ClangPin = ClangPin.Unused,
    test: Option[TestMain] = None,
    testDependencies: List[Dep] = Nil,
    provider: Option[TestProvider] = None,
    nativeVersion: Option[NativeVersion] = None,
) extends Project

final case class Aggregate(
    name: ProjectName,
    children: List[ProjectName],
    position: SourcePos,
) extends Project

/** The project list is fixed once this value exists. Lookup is total: a missing name is `None`. */
final case class ProjectGraph private (
    projects: List[Project],
    splits: Splits,
    catalog: Catalog,
    organization: Option[Organization],
    resolvers: List[Resolver],
):
  def get(name: ProjectName): Option[Project] =
    projects.find(_.name == name)

object ProjectGraph:
  private[histos] def trusted(projects: List[Project]): ProjectGraph =
    trusted(projects, Splits.aggregate)

  private[histos] def trusted(projects: List[Project], splits: Splits): ProjectGraph =
    trusted(projects, splits, Catalog.empty)

  private[histos] def trusted(projects: List[Project], splits: Splits, catalog: Catalog): ProjectGraph =
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
  ): ProjectGraph =
    new ProjectGraph(projects, splits, catalog, organization, resolvers)

trait HistosBuild:
  def graph: Either[BuildError, ProjectGraph]

object Projects:
  /** Every published module uses this organization. A build that does not publish omits it. */
  def organization(organization: Organization): Organized =
    Organized(Some(organization), Nil)

  /** Repositories this build resolves from. The list is not Central unless `central` is in it. An empty list, from [[apply]], is Central. */
  def resolvers(first: Resolver, rest: Resolver*): Organized =
    Organized(None, first :: rest.toList)

  def apply(defs: ProjectDef*): Either[BuildError, ProjectGraph] =
    build(None, Nil, Splits.aggregate, CatalogSpec.empty, defs.toList)

  def apply(splits: Splits, defs: ProjectDef*): Either[BuildError, ProjectGraph] =
    build(None, Nil, splits, CatalogSpec.empty, defs.toList)

  def apply(catalog: CatalogSpec, defs: ProjectDef*): Either[BuildError, ProjectGraph] =
    build(None, Nil, Splits.aggregate, catalog, defs.toList)

  def apply(splits: Splits, catalog: CatalogSpec, defs: ProjectDef*): Either[BuildError, ProjectGraph] =
    build(None, Nil, splits, catalog, defs.toList)

  private[histos] def assemble(
      organization: Option[Organization],
      resolvers: List[Resolver],
      splits: Splits,
      catalog: CatalogSpec,
      listed: List[ProjectDef],
  ): Either[BuildError, ProjectGraph] =
    build(organization, resolvers, splits, catalog, listed)

  private def build(
      organization: Option[Organization],
      resolvers: List[Resolver],
      splits: Splits,
      catalog: CatalogSpec,
      listed: List[ProjectDef],
  ): Either[BuildError, ProjectGraph] =
    for
      named <- nameAll(listed)
      _     <- duplicates(named)
      _     <- shape(named)
      built <- links(named)
      _     <- cycle(built)
      ships <- ShipLink.catalog(catalog, built)
      _     <- uniqueResolvers(resolvers)
    yield ProjectGraph.trusted(built, splits, ships, organization, resolvers)

  private def uniqueResolvers(resolvers: List[Resolver]): Either[BuildError, Unit] =
    resolvers.foldLeft[Either[BuildError, List[Resolver]]](Right(Nil)): (acc, resolver) =>
      acc.flatMap: seen =>
        if seen.contains(resolver) then Left(BuildError.DuplicateResolver(token(resolver)))
        else Right(seen :+ resolver)
    .map(_ => ())

  private def token(resolver: Resolver): String = resolver match
    case Resolver.Central      => "central"
    case Resolver.Maven(url)   => url.value

  private def nameAll(defs: List[ProjectDef]): Either[BuildError, List[Named]] =
    defs.foldLeft[Either[BuildError, List[Named]]](Right(Nil)): (acc, d) =>
      acc.flatMap: soFar =>
        ProjectName.from(d.name) match
          case Left(ProjectNameError.Empty)  => Left(BuildError.EmptyName(d.position))
          case Left(ProjectNameError.Bad(_)) => Left(BuildError.BadName(d.name, d.position))
          case Right(name)                   => Right(soFar :+ Named(name, d))

  private def duplicates(named: List[Named]): Either[BuildError, Unit] =
    named.foldLeft[Either[BuildError, List[Named]]](Right(Nil)): (acc, n) =>
      acc.flatMap: seen =>
        seen.find(_.name == n.name) match
          case Some(first) => Left(BuildError.DuplicateName(n.name, first.defn.position, n.defn.position))
          case None        => Right(seen :+ n)
    .map(_ => ())

  private def shape(named: List[Named]): Either[BuildError, Unit] =
    named.flatMap(shapeOf) match
      case err :: _ => Left(err)
      case Nil      => Right(())

  private def shapeOf(n: Named): Option[BuildError] =
    n.defn match
      case m: ModuleDef    =>
        duplicate(m.platforms.all).map(BuildError.DuplicatePlatform(n.name, _, m.position)).orElse(
          duplicate(m.versions.all).map(BuildError.DuplicateScalaVersion(n.name, _, m.position))
        ).orElse(clangOf(n.name, m)).orElse(nativeVersionOf(n.name, m))
      case a: AggregateDef =>
        val children = a.children.map(_.name)
        if children.contains(a.name) then Some(BuildError.AggregateIncludesItself(n.name, a.position))
        else
          duplicate(children).map: raw =>
            ProjectName.from(raw) match
              case Right(child) => BuildError.DuplicateChild(n.name, child, a.position)
              case Left(_)      => BuildError.BadName(raw, a.position)

  private def links(named: List[Named]): Either[BuildError, List[Project]] =
    val byName = named.map(n => n.name.value -> n).toMap
    first(named.map(n => link(n, byName)))

  private def link(n: Named, byName: Map[String, Named]): Either[BuildError, Project] =
    n.defn match
      case m: ModuleDef    => linkModule(n, m, byName)
      case a: AggregateDef => linkAggregate(n, a, byName)

  private def linkModule(n: Named, m: ModuleDef, byName: Map[String, Named]): Either[BuildError, Project] =
    m.dependsOn.foldLeft[Either[BuildError, List[ProjectName]]](Right(Nil)): (acc, dep) =>
      acc.flatMap: names =>
        lookup(dep, byName) match
          case Left(err)                               => Left(err)
          case Right(target) if target.name == n.name => Left(BuildError.SelfDependency(n.name, m.position))
          case Right(target)                          =>
            target.defn match
              case _: AggregateDef => Left(BuildError.DependsOnAggregate(n.name, target.name, m.position))
              case _: ModuleDef    => Right(names :+ target.name)
    .flatMap: deps =>
      (m.test, m.provider) match
        case (Some(_), Some(_)) => Left(BuildError.MixedTest(n.name, m.position))
        case _                  =>
          Right(Module(
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
          ))

  private def linkAggregate(n: Named, a: AggregateDef, byName: Map[String, Named]): Either[BuildError, Project] =
    a.children.foldLeft[Either[BuildError, List[ProjectName]]](Right(Nil)): (acc, child) =>
      acc.flatMap: names =>
        lookup(child, byName).map(target => names :+ target.name)
    .map(children => Aggregate(n.name, children, a.position))

  private def clangOf(name: ProjectName, module: ModuleDef): Option[BuildError] =
    val native = module.platforms.all.contains(Platform.Native)
    module.clang match
      case ClangPin.Major(_) if !native => Some(BuildError.ClangWithoutNative(name, module.position))
      case ClangPin.Unused if native    => Some(BuildError.MissingClang(name, module.position))
      case _                            => None

  private def nativeVersionOf(name: ProjectName, module: ModuleDef): Option[BuildError] =
    val native = module.platforms.all.contains(Platform.Native)
    module.nativeVersion match
      case Some(_) if !native => Some(BuildError.NativeVersionWithoutNative(name, module.position))
      case _                  => None

  private def cycle(projects: List[Project]): Either[BuildError, Unit] =
    val edges = projects.collect:
      case m: Module => m.name -> m.dependsOn
    findCycle(edges) match
      case Some(path) => Left(BuildError.Cycle(path))
      case None       => Right(())

  /** `path` is newest-first. A name already on it is the cycle. */
  private def findCycle(edges: List[(ProjectName, List[ProjectName])]): Option[List[ProjectName]] =
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
        val (found, next) = deps.getOrElse(name, Nil).foldLeft((Option.empty[List[ProjectName]], done)):
          case ((Some(c), d), _)   => (Some(c), d)
          case ((None, d), child)  => walk(child, name :: path, d)
        found match
          case Some(c) => (Some(c), next)
          case None    => (None, next + name)

    edges.foldLeft((Option.empty[List[ProjectName]], Set.empty[ProjectName])):
      case ((Some(c), done), _)       => (Some(c), done)
      case ((None, done), (name, _))  => walk(name, Nil, done)
    ._1

  private def lookup(ref: ProjectDef, byName: Map[String, Named]): Either[BuildError, Named] =
    byName.get(ref.name) match
      case Some(found) => Right(found)
      case None        => Left(BuildError.MissingProject(ref.name, ref.position))

  private def first[A](items: List[Either[BuildError, A]]): Either[BuildError, List[A]] =
    items.foldLeft[Either[BuildError, List[A]]](Right(Nil)): (acc, item) =>
      acc.flatMap(soFar => item.map(soFar :+ _))

  private def duplicate[A](items: List[A]): Option[A] =
    items.foldLeft[(List[A], Option[A])]((Nil, None)):
      case ((_, Some(found)), _) => (Nil, Some(found))
      case ((seen, None), item)  =>
        if seen.contains(item) then (seen, Some(item)) else (item :: seen, None)
    ._2

  private final case class Named(name: ProjectName, defn: ProjectDef)
end Projects

/** [[Projects]] with an organization, repositories, or both already chosen. */
final class Organized(organization: Option[Organization], resolvers: List[Resolver]):
  def organization(value: Organization): Organized =
    Organized(Some(value), resolvers)

  def resolvers(first: Resolver, rest: Resolver*): Organized =
    Organized(organization, resolvers ++ (first :: rest.toList))

  def apply(defs: ProjectDef*): Either[BuildError, ProjectGraph] =
    Projects.assemble(organization, resolvers, Splits.aggregate, CatalogSpec.empty, defs.toList)

  def apply(splits: Splits, defs: ProjectDef*): Either[BuildError, ProjectGraph] =
    Projects.assemble(organization, resolvers, splits, CatalogSpec.empty, defs.toList)

  def apply(catalog: CatalogSpec, defs: ProjectDef*): Either[BuildError, ProjectGraph] =
    Projects.assemble(organization, resolvers, Splits.aggregate, catalog, defs.toList)

  def apply(splits: Splits, catalog: CatalogSpec, defs: ProjectDef*): Either[BuildError, ProjectGraph] =
    Projects.assemble(organization, resolvers, splits, catalog, defs.toList)
