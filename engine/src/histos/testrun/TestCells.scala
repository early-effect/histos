package histos.testrun

import histos.plugin.SourceSet
import histos.{
  Aggregate,
  Cell,
  CompilationStage,
  Module,
  Platform,
  ProjectGraph,
  ProjectName,
  TestLane,
  TestSelection,
  TestTargets,
}

import zio.NonEmptyChunk

/** Requested suites are separate from compilation dependencies and aggregate link ownership. */
final case class TestCellPlan(
    requestedCells: List[Cell],
    compileStageRoots: List[CompilationStage],
    executionGroups: List[TestExecutionGroup],
)

enum TestExecutionGroup:
  case Direct(cell: Cell)
  case NativeAggregate(owner: Cell, members: NonEmptyChunk[Cell])

  def requestedCells: List[Cell] = this match
    case Direct(cell)                => List(cell)
    case NativeAggregate(_, members) => members.toList
end TestExecutionGroup

enum TestCellPolicy:
  case Declared, MeasuredJvm

object TestCells:
  /** DAG roots are not a dependency closure. The DAG expands scoped Main/Test edges and picks upstream versions. A
    * Native owner is link configuration, not an extra requested suite. Only exact, fully selected member cells
    * aggregate; focused individual requests, unmatched Scala versions and partial groups remain direct.
    */
  def plan(graph: ProjectGraph, selection: TestSelection): Either[TestSelectionError, TestCellPlan] =
    plan(graph, selection, TestCellPolicy.Declared)

  def plan(
      graph: ProjectGraph,
      selection: TestSelection,
      policy: TestCellPolicy,
  ): Either[TestSelectionError, TestCellPlan] =
    val modules = graph.projects.collect { case module: Module => module }
    val owners  = modules.filter(_.testAggregates.nonEmpty)
    for
      _        <- validate(graph, owners).left.map(TestSelectionError.Aggregate(_))
      selected <- select(graph, selection, policy)
    yield
      val requested = selected.filter: cell =>
        val eligible = policy match
          case TestCellPolicy.Declared    => true
          case TestCellPolicy.MeasuredJvm => histos.coverage.Cell.measures(cell.version, cell.platform)
        eligible && selection.affected.forall(_.contains(cell.module))
      val groups     = execution(owners, requested, selection.targets)
      val ownerRoots = groups.collect:
        case TestExecutionGroup.NativeAggregate(owner, _) if !requested.contains(owner) =>
          CompilationStage(owner, SourceSet.Main)
      TestCellPlan(
        requested,
        (requested.map(CompilationStage(_, SourceSet.Test)) ++ ownerRoots).distinct,
        groups,
      )

  private def validate(graph: ProjectGraph, owners: List[Module]): Either[TestAggregateError, Unit] =
    val members = owners.foldLeft[Either[TestAggregateError, Unit]](Right(())): (acc, owner) =>
      acc.flatMap: _ =>
        if !owner.platforms.all.contains(Platform.Native) then Left(TestAggregateError.UnsupportedNative(owner.name))
        else
          owner.testAggregates
            .foldLeft[Either[TestAggregateError, Set[ProjectName]]](Right(Set.empty)): (checked, name) =>
              checked.flatMap: seen =>
                if name == owner.name then Left(TestAggregateError.SelfMember(owner.name))
                else if seen.contains(name) then Left(TestAggregateError.DuplicateMember(owner.name, name))
                else
                  graph.get(name) match
                    case None                 => Left(TestAggregateError.MissingMember(owner.name, name))
                    case Some(_: Aggregate)   => Left(TestAggregateError.NonModuleMember(owner.name, name))
                    case Some(member: Module) =>
                      if member.platforms.all.contains(Platform.Native) then Right(seen + name)
                      else Left(TestAggregateError.UnsupportedNative(member.name))
            .map(_ => ())
    members.flatMap: _ =>
      owners.zipWithIndex.foldLeft[Either[TestAggregateError, Unit]](Right(())):
        case (acc, (owner, index)) =>
          acc.flatMap: _ =>
            owners
              .drop(index + 1)
              .foldLeft[Either[TestAggregateError, Unit]](Right(())): (checked, other) =>
                checked.flatMap: _ =>
                  (owner.name :: owner.testAggregates).find((other.name :: other.testAggregates).contains) match
                    case Some(member) => Left(TestAggregateError.Overlap(owner.name, other.name, member))
                    case None         => Right(())

  private def select(
      graph: ProjectGraph,
      selection: TestSelection,
      policy: TestCellPolicy,
  ): Either[TestSelectionError, List[Cell]] =
    selection.targets match
      case TestTargets.All               => Right(suites(graph, selection.platforms))
      case TestTargets.Filtered(modules) =>
        val eligible = policy match
          case TestCellPolicy.Declared    => suites(graph, selection.platforms)
          case TestCellPolicy.MeasuredJvm =>
            graph.projects
              .collect { case module: Module =>
                module
              }
              .flatMap(module => cells(module, selection.platforms.selected(module.platforms)))
        Right(eligible.filter(cell => modules.contains(cell.module)))
      case TestTargets.Named(first, rest) =>
        (first :: rest).distinct.foldLeft[Either[TestSelectionError, List[Cell]]](Right(Nil)): (acc, name) =>
          acc.flatMap: found =>
            named(graph, name, selection).map(picked => (found ++ picked).distinct)

  private def suites(graph: ProjectGraph, platforms: histos.PlatformSelection): List[Cell] =
    graph.projects
      .collect { case module: Module if module.lane == TestLane.Suite => module }
      .flatMap(module => cells(module, platforms.selected(module.platforms)))
      .distinct

  private def named(
      graph: ProjectGraph,
      name: ProjectName,
      selection: TestSelection,
  ): Either[TestSelectionError, List[Cell]] =
    graph.get(name) match
      case None                       => Left(TestSelectionError.UnknownProject(name))
      case Some(module: Module)       => namedModule(graph, module, selection)
      case Some(aggregate: Aggregate) =>
        aggregate.children.foldLeft[Either[TestSelectionError, List[Cell]]](Right(Nil)): (acc, child) =>
          acc.flatMap: found =>
            graph.get(child) match
              case Some(module: Module) =>
                namedModule(graph, module, selection).map(picked => (found ++ picked).distinct)
              case Some(_: Aggregate) | None =>
                Left(TestSelectionError.InvalidProjectAggregateMember(aggregate.name, child))

  private def namedModule(
      graph: ProjectGraph,
      module: Module,
      selection: TestSelection,
  ): Either[TestSelectionError, List[Cell]] =
    selection.platforms
      .validate(module.name, module.platforms)
      .left
      .map(TestSelectionError.Selection(_))
      .flatMap: platforms =>
        val members = if platforms.contains(Platform.Native) then module.testAggregates else Nil
        members.foldLeft[Either[TestSelectionError, List[Cell]]](Right(cells(module, platforms))): (acc, name) =>
          acc.flatMap: found =>
            graph.get(name) match
              case Some(member: Module) => Right((found ++ cells(member, List(Platform.Native))).distinct)
              case Some(_: Aggregate)   =>
                Left(TestSelectionError.Aggregate(TestAggregateError.NonModuleMember(module.name, name)))
              case None => Left(TestSelectionError.Aggregate(TestAggregateError.MissingMember(module.name, name)))

  private def cells(module: Module, platforms: List[Platform]): List[Cell] =
    module.versions.all.distinct.flatMap: version =>
      platforms.distinct.map(Cell(module.name, version, _))

  private def execution(
      owners: List[Module],
      requested: List[Cell],
      targets: TestTargets,
  ): List[TestExecutionGroup] =
    val focused = targets match
      case TestTargets.Named(first, rest) if rest.forall(_ == first)           => Some(first)
      case TestTargets.Named(_, _) | TestTargets.All | TestTargets.Filtered(_) => None
    val selected   = requested.toSet
    val aggregates = owners
      .filterNot(owner => focused.exists(owner.testAggregates.contains))
      .flatMap: owner =>
        owner.versions.all.distinct.flatMap: version =>
          val ownerCell = Cell(owner.name, version, Platform.Native)
          val members   = owner.testAggregates.map(Cell(_, version, Platform.Native))
          if members.forall(selected.contains) then
            val suites = (if selected.contains(ownerCell) then List(ownerCell) else Nil) ++ members
            NonEmptyChunk.fromIterableOption(suites).toList.map(TestExecutionGroup.NativeAggregate(ownerCell, _))
          else Nil
    requested
      .foldLeft((Set.empty[Cell], List.empty[TestExecutionGroup])):
        case ((covered, groups), cell) =>
          if covered.contains(cell) then (covered, groups)
          else
            val group = aggregates.find(_.requestedCells.contains(cell)).getOrElse(TestExecutionGroup.Direct(cell))
            (covered ++ group.requestedCells, groups :+ group)
      ._2
end TestCells
