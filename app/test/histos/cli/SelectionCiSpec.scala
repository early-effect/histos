package histos.cli

import histos.*
import histos.testrun.{TestCells, TestExecutionGroup, TestSelectionError}
import histos.workflow.{CiBranch, CiEvent, NpmLock, RenderingShip, Workflow}
import histos.suite.HistosSpec

import zio.ZIO
import zio.test.*
import zio.json.*

object SelectionCiSpec extends HistosSpec:
  private val version  = ScalaVersion("3.10.1")
  private val upstream = module("fixture", Jvm, Js, Native)(version).clang(ClangMajor(21)).lane(TestLane.Verify)
  private val first    = module("first", Jvm, Js, Native)(version).clang(ClangMajor(21)).dependsOn(upstream)
  private val second   = module("second", Jvm, Js, Native)(version).clang(ClangMajor(21))
  private val owner    =
    module("owner", Native)(version).clang(ClangMajor(21)).lane(TestLane.Verify).testAggregate(first, second)
  private val graph = Projects(Splits.of(Axis.Module, Axis.Platform), upstream, first, second, owner)

  private def selection(command: Command): Either[CliParseError, TestSelection] = command match
    case Command.Test(names, affected, _, _, _, platforms, versions) =>
      Right(AffectedChoice.selection(names, affected, platforms, versions))
    case _ => Left(CliParseError.Rejected)

  private def renderedRequests(
      build: ProjectGraph,
      changed: List[String],
  ): ZIO[Any, BuildError | histos.workflow.CiError | TestSelectionError | CliParseError, List[TestSelection]] =
    for
      pin  <- ZIO.fromEither(RenderingShip.from("0123456789abcdef0123456789abcdef01234567"))
      body <- ZIO.fromEither:
        given RenderingShip = pin
        given CiBranch      = CiBranch("main")
        Workflow.render(build, NpmLock.Absent)
      lines = body.linesIterator.map(_.trim).toList
      rawStep <- ZIO
        .fromOption(lines.collectFirst {
          case line if line.startsWith("- run: ") && line.contains("needs.affected.outputs.modules") =>
            line.stripPrefix("- run: ")
        })
        .orElseFail(CliParseError.Pattern("rendered workflow has no complete test step"))
      step <- ZIO.fromEither(
        rawStep.fromJson[String].left.map(_ => CliParseError.Pattern("test step is not quoted text"))
      )
      rawRows = lines.collect { case line if line.startsWith("command: ") => line.stripPrefix("command: ") }
      rows <- ZIO.foreach(rawRows)(raw =>
        ZIO.fromEither(raw.fromJson[String].left.map(_ => CliParseError.Pattern("row is not quoted text")))
      )
      complete = if rows.isEmpty then List(step) else rows.map(row => step.replace("${{ matrix.command }}", row))
      requests <- ZIO.foreach(complete): shell =>
        val resolved = shell.replace("${{ needs.affected.outputs.modules }}", changed.toJson)
        Grammar
          .parse(resolved.split(" ").toList.map(_.stripPrefix("'").stripSuffix("'")).drop(1))
          .flatMap(command => ZIO.fromEither(selection(command)))
    yield requests

  def spec = suite("CLI and CI test cells")(
    test("complete rendered affected steps preserve whole-suite filtering on a heterogeneous graph") {
      val api    = module("api", Jvm)(version)
      val web    = module("web", Js)(version)
      val verify = module("verify", Jvm)(version).lane(TestLane.Verify)
      for
        build   <- ZIO.fromEither(Projects(Splits.of(Axis.Platform), api, web, verify))
        results <- ZIO.foreach(List(List("web"), List("verify"), Nil, List("api", "web"))): names =>
          for
            requests <- renderedRequests(build, names)
            plans    <- ZIO.foreach(requests)(request => ZIO.fromEither(TestCells.plan(build, request)))
            changed  <- ZIO.foreach(names)(name => ZIO.fromEither(ProjectName.from(name)))
            local    <- ZIO.fromEither(TestCells.plan(build, TestSelection(affected = Some(changed))))
          yield plans.flatMap(_.requestedCells).toSet == local.requestedCells.toSet && plans
            .flatMap(_.executionGroups)
            .toSet == local.executionGroups.toSet && plans
            .flatMap(_.compileStageRoots)
            .toSet == local.compileStageRoots.toSet && plans
            .flatMap(_.requestedCells)
            .size == plans.flatMap(_.requestedCells).distinct.size && plans
            .filter(_.requestedCells.isEmpty)
            .forall(plan => plan.executionGroups.isEmpty && plan.compileStageRoots.isEmpty)
      yield assertTrue(results.forall(identity))
    },
    test("complete singleton aggregate steps and every supported axis preserve exact groups and roots") {
      val member = module("member", Jvm, Native)(version).clang(ClangMajor(21))
      val owner = module("singleton", Native)(version).clang(ClangMajor(21)).lane(TestLane.Verify).testAggregate(member)
      val other = module("other", Jvm)(version)
      val layouts =
        List(Splits.aggregate, Splits.of(Axis.Module), Splits.of(Axis.Platform), Splits.of(Axis.Module, Axis.Platform))
      val checked = ZIO.foreach(layouts): layout =>
        for
          build    <- ZIO.fromEither(Projects(layout, member, owner, other))
          requests <- renderedRequests(build, List("member", "other"))
          plans    <- ZIO.foreach(requests)(request => ZIO.fromEither(TestCells.plan(build, request)))
          local    <- ZIO.fromEither(
            TestCells.plan(build, TestSelection(affected = Some(List(ProjectName("member"), ProjectName("other")))))
          )
        yield plans.flatMap(_.requestedCells).toSet == local.requestedCells.toSet && plans
          .flatMap(_.executionGroups)
          .toSet == local.executionGroups.toSet && plans
          .flatMap(_.compileStageRoots)
          .toSet == local.compileStageRoots.toSet && plans.flatMap(_.requestedCells).size == local.requestedCells.size
      checked.map(results => assertTrue(results.forall(identity)))
    },
    test("Module-only splits independent Native direct groups without inventing a platform axis") {
      val first  = module("left", Native)(version).clang(ClangMajor(21))
      val second = module("right", Native)(version).clang(ClangMajor(21))
      for
        build <- ZIO.fromEither(Projects(Splits.of(Axis.Module), first, second))
        rows  <- ZIO.fromEither(Workflow.plan(build, CiEvent.Other))
      yield assertTrue(
        rows.size == 2,
        rows.forall(_.selection.platforms == PlatformSelection.All),
        rows
          .flatMap(_.cells.executionGroups)
          .forall:
            case TestExecutionGroup.Direct(_)             => true
            case TestExecutionGroup.NativeAggregate(_, _) => false,
      )
    },
    test("generated affected filters preserve cells, groups, roots, and exactly-once coverage in complete steps") {
      check(Gen.listOfBounded(0, 8)(Gen.elements("first", "second", "fixture"))): names =>
        for
          build    <- ZIO.fromEither(graph)
          requests <- renderedRequests(build, names)
          plans    <- ZIO.foreach(requests)(request => ZIO.fromEither(TestCells.plan(build, request)))
          changed  <- ZIO.foreach(names)(name => ZIO.fromEither(ProjectName.from(name)))
          local    <- ZIO.fromEither(TestCells.plan(build, TestSelection(affected = Some(changed))))
        yield assertTrue(
          plans.flatMap(_.requestedCells).toSet == local.requestedCells.toSet,
          plans.flatMap(_.executionGroups).toSet == local.executionGroups.toSet,
          plans.flatMap(_.compileStageRoots).toSet == local.compileStageRoots.toSet,
          plans.flatMap(_.requestedCells).size == local.requestedCells.size,
        )
    },
    test("explicit named unsupported platforms fail even when the affected mask is empty") {
      for
        build   <- ZIO.fromEither(Projects(module("web", Js)(version)))
        command <- Grammar.parse(List("test", "web", "--platform", "jvm", "--affected", "[]"))
        request <- ZIO.fromEither(selection(command))
      yield assertTrue(
        TestCells.plan(build, request) == Left(
          TestSelectionError.Selection(SelectionError.Unsupported(ProjectName("web"), Jvm))
        )
      )
    },
    test("a complete affected JVM row filters a JS-only or Verify-only change") {
      val api    = module("api", Jvm)(version)
      val web    = module("web", Js)(version)
      val verify = module("verify", Jvm)(version).lane(TestLane.Verify)
      for
        build   <- ZIO.fromEither(Projects(Splits.of(Axis.Platform), api, web, verify))
        results <- ZIO.foreach(List("web", "verify")): name =>
          for
            command <- Grammar.parse(List("test", "--platform", "jvm", "--affected", s"[\"$name\"]"))
            request <- ZIO.fromEither(selection(command))
          yield TestCells.plan(build, request).map(_.requestedCells) == Right(Nil)
      yield assertTrue(results.forall(identity))
    },
    test("a singleton Native group and its owner roots survive platform partition") {
      val member = module("member", Native)(version).clang(ClangMajor(21))
      val owner = module("singleton", Native)(version).clang(ClangMajor(21)).lane(TestLane.Verify).testAggregate(member)
      val other = module("other", Jvm)(version)
      for
        build   <- ZIO.fromEither(Projects(Splits.of(Axis.Platform), member, owner, other))
        initial <- ZIO.fromEither(
          TestCells.plan(build, TestSelection(TestTargets.Named(ProjectName("member"), List(ProjectName("other")))))
        )
        rows <- ZIO.fromEither(
          Workflow.plan(build, CiEvent.PullRequest(Set(ProjectName("member"), ProjectName("other"))))
        )
      yield assertTrue(
        rows.flatMap(_.cells.executionGroups).toSet == initial.executionGroups.toSet,
        rows.flatMap(_.cells.compileStageRoots).toSet == initial.compileStageRoots.toSet,
      )
    },
    test("a Module-only split does not invent platform rows") {
      val cross = module("cross", Jvm, Js)(version)
      for
        build <- ZIO.fromEither(Projects(Splits.of(Axis.Module), cross))
        rows  <- ZIO.fromEither(Workflow.plan(build, CiEvent.Other))
      yield assertTrue(rows.size == 1, rows.forall(_.selection.platforms == PlatformSelection.All))
    },
    test("supported axes preserve every module Scala version without pretending to version-split") {
      val multi   = module("multi", Jvm, Js)(version, ScalaVersion("3.9.0"))
      val layouts =
        List(Splits.aggregate, Splits.of(Axis.Module), Splits.of(Axis.Platform), Splits.of(Axis.Module, Axis.Platform))
      val checked = ZIO.foreach(layouts): layout =>
        for
          build    <- ZIO.fromEither(Projects(layout, multi))
          rows     <- ZIO.fromEither(Workflow.plan(build, CiEvent.Other))
          requests <- renderedRequests(build, List("multi"))
          plans    <- ZIO.foreach(requests)(request => ZIO.fromEither(TestCells.plan(build, request)))
          local    <- ZIO.fromEither(TestCells.plan(build, TestSelection()))
        yield rows.size == (if layout.axes.contains(Axis.Platform) then 2 else 1) && plans
          .flatMap(_.requestedCells)
          .size == 4 && plans.flatMap(_.executionGroups).toSet == local.executionGroups.toSet && plans
          .flatMap(_.compileStageRoots)
          .toSet == local.compileStageRoots.toSet
      checked.map(results => assertTrue(results.forall(identity)))
    },
    test("a multi-Scala fan-out refuses an unsupported configured axis") {
      val multi = module("multi", Jvm)(version, ScalaVersion("3.9.0"))
      for
        build <- ZIO.fromEither(Projects(Splits.of(Axis.ScalaVersion), multi))
        pin   <- ZIO.fromEither(RenderingShip.from("0123456789abcdef0123456789abcdef01234567"))
      yield
        given RenderingShip = pin
        given CiBranch      = CiBranch("main")
        assertTrue(
          Workflow.plan(build, CiEvent.Other) == Left(histos.workflow.CiError.UnsupportedAxis(Axis.ScalaVersion)),
          Workflow.render(build, NpmLock.Absent) == Left(histos.workflow.CiError.UnsupportedAxis(Axis.ScalaVersion)),
        )
    },
    test("every rendered row reparses to its exact requested cells and Native groups") {
      for
        build    <- ZIO.fromEither(graph)
        rows     <- ZIO.fromEither(Workflow.plan(build, CiEvent.Other))
        local    <- ZIO.fromEither(TestCells.plan(build, TestSelection()))
        reparsed <- ZIO.foreach(rows): row =>
          for
            command <- Grammar.parse(Workflow.argv(row).drop(1))
            request <- ZIO.fromEither(selection(command))
            planned <- ZIO.fromEither(TestCells.plan(build, request))
          yield planned == row.cells
      yield assertTrue(
        reparsed.forall(identity),
        rows.flatMap(_.cells.requestedCells).toSet == local.requestedCells.toSet,
        rows.flatMap(_.cells.requestedCells).distinct.size == rows.flatMap(_.cells.requestedCells).size,
        rows.flatMap(_.cells.executionGroups).toSet == local.executionGroups.toSet,
        !rows.flatMap(_.cells.requestedCells).exists(_.module == ProjectName("fixture")),
        rows
          .flatMap(_.cells.executionGroups)
          .collect { case group: TestExecutionGroup.NativeAggregate => group }
          .size == 1,
      )
    },
    test("commands extracted from rendered YAML reparse to the actual planned groups") {
      for
        build <- ZIO.fromEither(graph)
        pin   <- ZIO.fromEither(RenderingShip.from("0123456789abcdef0123456789abcdef01234567"))
        rows  <- ZIO.fromEither(Workflow.plan(build, CiEvent.Other))
        body  <- ZIO.fromEither:
          given RenderingShip = pin
          given CiBranch      = CiBranch("main")
          Workflow.render(build, NpmLock.Absent)
        commands = body.linesIterator
          .map(_.trim)
          .collect { case line if line.startsWith("command: ") => line.stripPrefix("command: ") }
          .toList
        plans <- ZIO.foreach(commands): raw =>
          for
            shell <- ZIO.fromEither(
              raw.fromJson[String].left.map(_ => CliParseError.Pattern("rendered command is not quoted text"))
            )
            command <- Grammar.parse(shell.split(" ").toList.map(_.stripPrefix("'").stripSuffix("'")).drop(1))
            request <- ZIO.fromEither(selection(command))
            planned <- ZIO.fromEither(TestCells.plan(build, request))
          yield planned
      yield assertTrue(plans == rows.map(_.cells), commands.size == rows.size)
    },
    test("full, focused, partial, owner, and explicit Verify selections share the planner") {
      val targets = List(
        TestTargets.All,
        TestTargets.Named(ProjectName("first"), Nil),
        TestTargets.Named(ProjectName("first"), List(ProjectName("second"))),
        TestTargets.Named(ProjectName("owner"), Nil),
        TestTargets.Named(ProjectName("fixture"), Nil),
      )
      for
        build   <- ZIO.fromEither(graph)
        results <- ZIO.foreach(targets): targets =>
          val request = TestSelection(targets, PlatformSelection.only(Native))
          for
            command <- Grammar.parse(Workflow.selectionArgv(request).drop(1))
            parsed  <- ZIO.fromEither(selection(command))
          yield TestCells.plan(build, parsed) == Workflow.cells(build, request)
      yield assertTrue(results.forall(identity))
    },
    test("a suite dependency is a compile obligation, not a requested upstream test") {
      for
        build   <- ZIO.fromEither(Projects(upstream.lane(TestLane.Suite), first))
        command <- Grammar.parse(List("test", "first", "--platform", "jvm"))
        request <- ZIO.fromEither(selection(command))
        planned <- ZIO.fromEither(TestCells.plan(build, request))
      yield assertTrue(
        planned.requestedCells == List(Cell(ProjectName("first"), version, Jvm)),
        planned.executionGroups == List(TestExecutionGroup.Direct(Cell(ProjectName("first"), version, Jvm))),
        planned.compileStageRoots == List(
          CompilationStage(Cell(ProjectName("first"), version, Jvm), histos.plugin.SourceSet.Test)
        ),
      )
    },
    test("unsupported named platforms return the same typed planner failure") {
      for
        build   <- ZIO.fromEither(graph)
        command <- Grammar.parse(List("test", "owner", "--platform", "js"))
        request <- ZIO.fromEither(selection(command))
      yield assertTrue(
        Workflow.cells(build, request) == Left(
          TestSelectionError.Selection(SelectionError.Unsupported(ProjectName("owner"), Js))
        )
      )
    },
    test("affected JSON keeps partial Native members direct") {
      for
        build   <- ZIO.fromEither(graph)
        command <- Grammar.parse(List("test", "--platform", "native", "--affected", "[\"first\"]"))
        request <- ZIO.fromEither(selection(command))
        planned <- ZIO.fromEither(Workflow.cells(build, request))
      yield assertTrue(
        planned.executionGroups == List(TestExecutionGroup.Direct(Cell(ProjectName("first"), version, Native))),
        planned.compileStageRoots == List(
          CompilationStage(Cell(ProjectName("first"), version, Native), histos.plugin.SourceSet.Test)
        ),
      )
    },
    test("all platform unions remain equivalent after CLI round trip") {
      check(Gen.listOfBounded(1, 8)(Gen.elements(Jvm, Js, Native))): platforms =>
        platforms match
          case first :: rest =>
            val request = TestSelection(TestTargets.All, PlatformSelection.only(first, rest*))
            for
              build   <- ZIO.fromEither(graph)
              command <- Grammar.parse(Workflow.selectionArgv(request).drop(1))
              parsed  <- ZIO.fromEither(selection(command))
            yield assertTrue(TestCells.plan(build, parsed) == TestCells.plan(build, request))
          case Nil => ZIO.succeed(assertTrue(false))
    },
  )
end SelectionCiSpec
