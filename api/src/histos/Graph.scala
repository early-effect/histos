package histos

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
) extends Project

final case class Aggregate(
    name: ProjectName,
    children: List[ProjectName],
    position: SourcePos,
) extends Project

/** The project list is fixed once this value exists. Lookup is total: a missing name is `None`. */
final case class ProjectGraph private (projects: List[Project], splits: Splits):
  def get(name: ProjectName): Option[Project] =
    projects.find(_.name == name)

object ProjectGraph:
  private[histos] def trusted(projects: List[Project]): ProjectGraph =
    trusted(projects, Splits.aggregate)

  private[histos] def trusted(projects: List[Project], splits: Splits): ProjectGraph =
    new ProjectGraph(projects, splits)

trait HistosBuild:
  def graph: Either[BuildError, ProjectGraph]

object Projects:
  def apply(defs: ProjectDef*): Either[BuildError, ProjectGraph] =
    build(Splits.aggregate, defs.toList)

  def apply(splits: Splits, defs: ProjectDef*): Either[BuildError, ProjectGraph] =
    build(splits, defs.toList)

  private def build(splits: Splits, listed: List[ProjectDef]): Either[BuildError, ProjectGraph] =
    for
      named <- nameAll(listed)
      _     <- duplicates(named)
      _     <- shape(named)
      built <- links(named)
      _     <- cycle(built)
    yield ProjectGraph.trusted(built, splits)

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
        ).orElse(clangOf(n.name, m))
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
    .map(deps => Module(n.name, m.platforms, m.versions, m.dependencies, deps, m.position, m.clang))

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
