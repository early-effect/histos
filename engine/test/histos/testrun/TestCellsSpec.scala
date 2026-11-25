package histos.testrun

import histos.plugin.SourceSet
import histos.suite.HistosSpec
import histos.{
  Aggregate,
  Cell,
  CompilationStage,
  Module,
  Platform,
  Platforms,
  PlatformSelection,
  ProjectDep,
  ProjectGraph,
  ProjectName,
  ScalaSelection,
  ScalaToken,
  ScalaVersion,
  ScalaVersions,
  SelectionError,
  SourcePos,
  TestLane,
  TestSelection,
  TestTargets,
  scala,
}

import _root_.scala.compiletime.testing.typeChecks

import zio.NonEmptyChunk
import zio.test.*

object TestCellsSpec extends HistosSpec:
  private val v1          = scala"3.9.2"
  private val v2          = scala"3.10.1"
  private val position    = SourcePos("TestCellsSpec.scala", 1, 1)
  private val coreName    = ProjectName("core")
  private val wireName    = ProjectName("wire")
  private val ownerName   = ProjectName("nativeTests")
  private val verifyName  = ProjectName("browser")
  private val missingName = ProjectName("missing")

  private def module(
      name: ProjectName,
      platforms: Platforms,
      versions: ScalaVersions = ScalaVersions(v2, Nil),
  ): Module =
    Module(name, platforms, versions, Nil, Nil, position)

  private val native = Platforms(Platform.Native, Nil)
  private val cross  = Platforms(Platform.Jvm, List(Platform.Js, Platform.Native))
  private val core   = module(coreName, cross, ScalaVersions(v1, List(v2)))
  private val wire   = module(wireName, cross, ScalaVersions(v1, List(v2)))
  private val owner  = module(ownerName, native, ScalaVersions(v1, List(v2)))
    .copy(testAggregates = List(coreName, wireName))
  private val verify = module(verifyName, Platforms(Platform.Jvm, Nil)).copy(lane = TestLane.Verify)
  private val graph  = ProjectGraph.trusted(List(core, wire, owner, verify))

  private def cell(module: Module, version: ScalaVersion = v2, platform: Platform = Platform.Native): Cell =
    Cell(module.name, version, platform)

  private def named(first: ProjectName, rest: ProjectName*): TestSelection =
    TestSelection(TestTargets.Named(first, rest.toList), PlatformSelection.only(Platform.Native))

  private def aggregate(version: ScalaVersion, includeOwner: Boolean): TestExecutionGroup =
    val members = NonEmptyChunk(cell(core, version), cell(wire, version))
    TestExecutionGroup.NativeAggregate(
      cell(owner, version),
      if includeOwner then NonEmptyChunk(cell(owner, version), cell(core, version), cell(wire, version)) else members,
    )

  def spec = suite("test cell selection")(
    test("the callable contract is fixed and empty named targets or aggregate members do not compile") {
      val planner: (ProjectGraph, TestSelection) => Either[TestSelectionError, TestCellPlan] = TestCells.plan
      assertTrue(
        planner(ProjectGraph.trusted(Nil), TestSelection()) == Right(TestCellPlan(Nil, Nil, Nil)),
        !typeChecks("histos.TestTargets.Named()"),
        !typeChecks("""
          histos.testrun.TestExecutionGroup.NativeAggregate(
            histos.Cell(histos.ProjectName("owner"), histos.ScalaVersion("3.10.1"), histos.Platform.Native),
            zio.Chunk.empty[histos.Cell],
          )
        """),
      )
    },
    test("empty graphs and unmatched whole-build platforms have empty plans") {
      val empty = TestCellPlan(Nil, Nil, Nil)
      assertTrue(
        TestCells.plan(ProjectGraph.trusted(Nil), TestSelection()) == Right(empty),
        TestCells.plan(
          ProjectGraph.trusted(List(verify.copy(lane = TestLane.Suite))),
          TestSelection(platforms = PlatformSelection.only(Platform.Native)),
        ) == Right(empty),
      )
    },
    test("all selects each supported Scala version and platform, excluding Verify") {
      val planned  = TestCells.plan(graph, TestSelection())
      val expected = List(core, wire, owner).flatMap: module =>
        module.versions.all.flatMap(version => module.platforms.all.map(Cell(module.name, version, _)))
      assertTrue(
        planned.map(_.requestedCells) == Right(expected),
        planned.map(_.compileStageRoots) == Right(expected.map(CompilationStage(_, SourceSet.Test))),
        planned.map(_.executionGroups.flatMap(_.requestedCells).toSet) == Right(expected.toSet),
        planned.map(_.executionGroups.count {
          case TestExecutionGroup.Direct(_)             => false
          case TestExecutionGroup.NativeAggregate(_, _) => true
        }) == Right(2),
      )
    },
    test("the legacy suite selector exposes duplicate Native work, the plan consolidates it") {
      val legacy     = histos.select.Selection.suite(graph).modules
      val legacyRuns = legacy
        .filter(_.platforms.all.contains(Platform.Native))
        .flatMap: module =>
          module.versions.all.flatMap: version =>
            Cell(module.name, version, Platform.Native) :: module.testAggregates.map(Cell(_, version, Platform.Native))
      val planned = TestCells.plan(graph, TestSelection(platforms = PlatformSelection.only(Platform.Native)))
      assertTrue(
        legacyRuns.count(_ == cell(core)) == 2,
        legacyRuns.count(_ == cell(wire)) == 2,
        planned.map(_.executionGroups) == Right(List(aggregate(v1, true), aggregate(v2, true))),
        planned.map(_.executionGroups.flatMap(_.requestedCells).count(_ == cell(core))) == Right(1),
        planned.map(_.executionGroups.flatMap(_.requestedCells).count(_ == cell(wire))) == Right(1),
      )
    },
    test("a Verify owner aggregates all selected members without becoming a requested suite") {
      val planned = TestCells.plan(
        ProjectGraph.trusted(List(core, wire, owner.copy(lane = TestLane.Verify), verify)),
        TestSelection(platforms = PlatformSelection.only(Platform.Native)),
      )
      val expected = List(cell(core, v1), cell(core), cell(wire, v1), cell(wire))
      assertTrue(
        planned.map(_.requestedCells) == Right(expected),
        planned.map(_.executionGroups) == Right(List(aggregate(v1, false), aggregate(v2, false))),
        planned.map(_.compileStageRoots) == Right(
          expected.map(CompilationStage(_, SourceSet.Test)) ++
            List(CompilationStage(cell(owner, v1), SourceSet.Main), CompilationStage(cell(owner), SourceSet.Main))
        ),
      )
    },
    test("naming an owner expands its Native members and deduplicates explicit members and repeats") {
      val planned  = TestCells.plan(graph, named(ownerName, coreName, wireName, ownerName, coreName))
      val expected = List(cell(owner, v1), cell(owner), cell(core, v1), cell(core), cell(wire, v1), cell(wire))
      assertTrue(
        planned.map(_.requestedCells) == Right(expected),
        planned.map(_.executionGroups) == Right(List(aggregate(v1, true), aggregate(v2, true))),
      )
    },
    test("a named individual and a partial affected member list stay direct") {
      val planned = TestCells.plan(graph, named(coreName))
      assertTrue(
        planned.map(_.requestedCells) == Right(List(cell(core, v1), cell(core))),
        planned.map(_.executionGroups) == Right(
          List(TestExecutionGroup.Direct(cell(core, v1)), TestExecutionGroup.Direct(cell(core)))
        ),
        planned.map(_.compileStageRoots) == Right(
          List(CompilationStage(cell(core, v1), SourceSet.Test), CompilationStage(cell(core), SourceSet.Test))
        ),
      )
    },
    test("a focused individual stays direct even when it is the owner's only declared member") {
      val single    = ProjectGraph.trusted(List(core, owner.copy(testAggregates = List(coreName))))
      val focused   = TestCells.plan(single, named(coreName, coreName))
      val automatic = TestCells.plan(single, TestSelection(platforms = PlatformSelection.only(Platform.Native)))
      assertTrue(
        focused.map(_.executionGroups) == Right(
          List(TestExecutionGroup.Direct(cell(core, v1)), TestExecutionGroup.Direct(cell(core)))
        ),
        automatic.map(_.executionGroups) == Right(
          List(
            TestExecutionGroup.NativeAggregate(cell(owner, v1), NonEmptyChunk(cell(owner, v1), cell(core, v1))),
            TestExecutionGroup.NativeAggregate(cell(owner), NonEmptyChunk(cell(owner), cell(core))),
          )
        ),
      )
    },
    test("a complete named member list aggregates without explicitly naming the owner") {
      assertTrue(
        TestCells.plan(graph, named(coreName, wireName)).map(_.executionGroups) ==
          Right(List(aggregate(v1, false), aggregate(v2, false)))
      )
    },
    test("whole-build selection never expands a Suite owner into excluded Verify members") {
      val planned = TestCells.plan(
        ProjectGraph.trusted(List(core, wire.copy(lane = TestLane.Verify), owner)),
        TestSelection(platforms = PlatformSelection.only(Platform.Native)),
      )
      val expected = List(cell(core, v1), cell(core), cell(owner, v1), cell(owner))
      assertTrue(
        planned.map(_.requestedCells) == Right(expected),
        planned.map(_.executionGroups) == Right(expected.map(TestExecutionGroup.Direct(_))),
      )
    },
    test("explicit Verify selection and named owners can request Verify members") {
      val verifyGraph = ProjectGraph.trusted(List(core, wire.copy(lane = TestLane.Verify), owner, verify))
      assertTrue(
        TestCells.plan(verifyGraph, named(ownerName)).map(_.executionGroups) ==
          Right(List(aggregate(v1, true), aggregate(v2, true))),
        TestCells.plan(graph, TestSelection(TestTargets.Named(verifyName, Nil))).map(_.requestedCells) ==
          Right(List(cell(verify, platform = Platform.Jvm))),
      )
    },
    test("named platforms refuse unsupported requests while all is only a matching filter") {
      val explicit =
        TestSelection(TestTargets.Named(verifyName, Nil), PlatformSelection.only(Platform.Jvm, Platform.Js))
      assertTrue(
        TestCells.plan(graph, explicit) ==
          Left(TestSelectionError.Selection(SelectionError.Unsupported(verifyName, Platform.Js))),
        TestCells.plan(graph, named(missingName)) == Left(TestSelectionError.UnknownProject(missingName)),
      )
    },
    test("nonnative owner cells remain direct and do not expand Native members") {
      val crossOwner = owner.copy(platforms = cross)
      val planned    = TestCells.plan(
        ProjectGraph.trusted(List(core, wire, crossOwner)),
        TestSelection(TestTargets.Named(ownerName, Nil), PlatformSelection.only(Platform.Jvm, Platform.Js)),
      )
      val expected = List(v1, v2).flatMap(version => List(Platform.Jvm, Platform.Js).map(cell(owner, version, _)))
      assertTrue(
        planned.map(_.requestedCells) == Right(expected),
        planned.map(_.executionGroups) == Right(expected.map(TestExecutionGroup.Direct(_))),
      )
    },
    test("only exact full version groups aggregate, other member versions stay their own cells") {
      val mixed   = ProjectGraph.trusted(List(core, wire.copy(versions = ScalaVersions(v2, Nil)), owner))
      val planned = TestCells.plan(mixed, named(ownerName))
      assertTrue(
        planned.map(_.requestedCells) == Right(
          List(cell(owner, v1), cell(owner), cell(core, v1), cell(core), cell(wire))
        ),
        planned.map(_.executionGroups) == Right(
          List(
            TestExecutionGroup.Direct(cell(owner, v1)),
            aggregate(v2, true),
            TestExecutionGroup.Direct(cell(core, v1)),
          )
        ),
      )
    },
    test("inherited Main and Test dependencies never become requested upstream suites or root stages") {
      val consumerName = ProjectName("consumer")
      val consumer     = module(consumerName, Platforms(Platform.Jvm, Nil)).copy(
        dependsOn = List(
          ProjectDep(coreName, Some(List(Platform.Jvm)), SourceSet.Main),
          ProjectDep(wireName, Some(List(Platform.Jvm)), SourceSet.Test),
        )
      )
      val dependencyGraph = ProjectGraph.trusted(List(core, wire, consumer))
      val requested       = Cell(consumerName, v2, Platform.Jvm)
      val planned         = TestCells.plan(dependencyGraph, TestSelection(TestTargets.Named(consumerName, Nil)))
      assertTrue(
        Module.closure(List(core, wire, consumer), Set(consumerName)).map(_.name) == List(
          coreName,
          wireName,
          consumerName,
        ),
        planned == Right(
          TestCellPlan(
            List(requested),
            List(CompilationStage(requested, SourceSet.Test)),
            List(TestExecutionGroup.Direct(requested)),
          )
        ),
      )
    },
    test("ordinary project aggregates select their children, not dependencies, and still group Native cells") {
      val aggregateName    = ProjectName("libraries")
      val projectAggregate = Aggregate(aggregateName, List(coreName, wireName), position)
      val aggregateGraph   = ProjectGraph.trusted(graph.projects :+ projectAggregate)
      assertTrue(
        TestCells.plan(aggregateGraph, named(aggregateName, coreName)).map(_.executionGroups) ==
          Right(List(aggregate(v1, false), aggregate(v2, false))),
        TestCells.plan(
          ProjectGraph.trusted(graph.projects :+ projectAggregate.copy(children = List(missingName))),
          named(aggregateName),
        ) == Left(TestSelectionError.InvalidProjectAggregateMember(aggregateName, missingName)),
      )
    },
    test("invalid trusted Native graphs return typed errors instead of dropping or duplicating suites") {
      val secondName = ProjectName("secondOwner")
      val second     = owner.copy(name = secondName, testAggregates = List(coreName))
      val ordinary   = Aggregate(ProjectName("ordinary"), List(coreName), position)
      val cases      = List(
        (
          List(core, wire, owner.copy(platforms = Platforms(Platform.Jvm, Nil))),
          TestAggregateError.UnsupportedNative(ownerName),
        ),
        (List(core, wire, owner.copy(testAggregates = List(ownerName))), TestAggregateError.SelfMember(ownerName)),
        (
          List(core, wire, owner.copy(testAggregates = List(coreName, coreName))),
          TestAggregateError.DuplicateMember(ownerName, coreName),
        ),
        (
          List(core, wire, owner.copy(testAggregates = List(missingName))),
          TestAggregateError.MissingMember(ownerName, missingName),
        ),
        (
          List(core, wire, ordinary, owner.copy(testAggregates = List(ordinary.name))),
          TestAggregateError.NonModuleMember(ownerName, ordinary.name),
        ),
        (
          List(core, wire.copy(platforms = Platforms(Platform.Jvm, Nil)), owner),
          TestAggregateError.UnsupportedNative(wireName),
        ),
        (List(core, wire, owner, second), TestAggregateError.Overlap(ownerName, secondName, coreName)),
        (
          List(core, wire, owner, second.copy(testAggregates = List(ownerName))),
          TestAggregateError.Overlap(ownerName, secondName, ownerName),
        ),
      )
      assertTrue(cases.forall { (projects, error) =>
        TestCells.plan(ProjectGraph.trusted(projects), TestSelection()) == Left(TestSelectionError.Aggregate(error))
      })
    },
    test("every named selection executes each requested cell once, with distinct roots and exact Native groups") {
      check(Gen.listOfBounded(1, 12)(Gen.elements(coreName, wireName, ownerName))) { names =>
        val selection = names match
          case first :: rest => TestSelection(TestTargets.Named(first, rest), PlatformSelection.only(Platform.Native))
          case Nil           => TestSelection(platforms = PlatformSelection.only(Platform.Native))
        val expectedNames =
          if names.contains(ownerName) then Set(coreName, wireName, ownerName) else names.toSet
        val expected =
          expectedNames.flatMap(name => Set(Cell(name, v1, Platform.Native), Cell(name, v2, Platform.Native)))
        val planned = TestCells.plan(graph, selection)
        assertTrue(
          planned.map(_.requestedCells.toSet) == Right(expected),
          planned.map(plan => plan.requestedCells.distinct == plan.requestedCells) == Right(true),
          planned.map(plan => plan.compileStageRoots.distinct == plan.compileStageRoots) == Right(true),
          planned.map(plan => plan.executionGroups.flatMap(_.requestedCells).toSet) == Right(expected),
          planned.map(plan => plan.executionGroups.flatMap(_.requestedCells).size) == Right(expected.size),
          planned.map(_.executionGroups.forall {
            case TestExecutionGroup.Direct(_)                       => true
            case TestExecutionGroup.NativeAggregate(owner, members) =>
              owner.platform == Platform.Native && members.forall(member =>
                member.platform == Platform.Native && member.version == owner.version
              )
          }) == Right(true),
        )
      }
    },
    test("a named exact version keeps that version and one native aggregate") {
      val asked = TestSelection(
        TestTargets.Named(ownerName, Nil),
        PlatformSelection.only(Platform.Native),
        scala = ScalaSelection.Only(ScalaToken.Exact(v2), Nil),
      )
      val planned = TestCells.plan(graph, asked)
      assertTrue(
        planned.map(_.requestedCells) == Right(List(cell(owner, v2), cell(core, v2), cell(wire, v2))),
        planned.map(_.executionGroups) == Right(List(aggregate(v2, true))),
      )
    },
    test("a named series that matches two versions is ambiguous") {
      val asked = TestSelection(
        TestTargets.Named(coreName, Nil),
        PlatformSelection.only(Platform.Jvm),
        scala = ScalaSelection.Only(ScalaToken.Series3, Nil),
      )
      assertTrue(
        TestCells.plan(graph, asked) == Left(
          TestSelectionError.Selection(SelectionError.AmbiguousScala(coreName, ScalaToken.Series3, List(v1, v2)))
        )
      )
    },
    test("a whole-build version filter keeps matches and a missing series drops members") {
      val kept     = TestCells.plan(graph, TestSelection(scala = ScalaSelection.Only(ScalaToken.Exact(v2), Nil)))
      val v213     = scala"2.13.18"
      val owner213 =
        module(ownerName, native, ScalaVersions(v213, List(v2))).copy(testAggregates = List(coreName, wireName))
      val core213 = module(coreName, cross, ScalaVersions(v213, List(v2)))
      val wire3   = module(wireName, cross, ScalaVersions(v2, Nil))
      val mixed   = ProjectGraph.trusted(List(core213, wire3, owner213))
      val asked   = TestSelection(
        TestTargets.Named(ownerName, Nil),
        PlatformSelection.only(Platform.Native),
        scala = ScalaSelection.Only(ScalaToken.Series2(13), Nil),
      )
      val planned   = TestCells.plan(mixed, asked)
      val ownerCell = Cell(ownerName, v213, Platform.Native)
      val coreCell  = Cell(coreName, v213, Platform.Native)
      assertTrue(
        kept.map(_.requestedCells.forall(_.version == v2)) == Right(true),
        kept.map(_.executionGroups.collect { case group: TestExecutionGroup.NativeAggregate => group }) == Right(
          List(aggregate(v2, true))
        ),
        TestCells.plan(graph, TestSelection(scala = ScalaSelection.Only(ScalaToken.Series2(13), Nil))) == Right(
          TestCellPlan(Nil, Nil, Nil)
        ),
        planned.map(_.requestedCells) == Right(List(ownerCell, coreCell)),
        planned.map(_.executionGroups) == Right(
          List(TestExecutionGroup.Direct(ownerCell), TestExecutionGroup.Direct(coreCell))
        ),
      )
    },
  )
end TestCellsSpec
