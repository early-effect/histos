package histos.cli

import histos.*
import histos.testrun.{TestCells, TestExecutionGroup, TestSelectionError}
import histos.workflow.{CiBranch, CiEvent, NpmLock, RenderingShip, Workflow}

import zio.ZIO
import zio.test.*
import zio.json.*

object SelectionCiSpec extends ZIOSpecDefault:
  private val version  = ScalaVersion("3.10.1")
  private val upstream = module("fixture", Jvm, Js, Native)(version).clang(ClangMajor(21)).lane(TestLane.Verify)
  private val first    = module("first", Jvm, Js, Native)(version).clang(ClangMajor(21)).dependsOn(upstream)
  private val second   = module("second", Jvm, Js, Native)(version).clang(ClangMajor(21))
  private val owner    =
    module("owner", Native)(version).clang(ClangMajor(21)).lane(TestLane.Verify).testAggregate(first, second)
  private val graph = Projects(Splits.of(Axis.Module, Axis.Platform), upstream, first, second, owner)

  private def selection(command: Command): Either[CliParseError, TestSelection] = command match
    case Command.Test(names, affected, _, _, _, platforms) =>
      AffectedChoice.selection(names, affected, platforms).toRight(CliParseError.Pattern("nothing is affected"))
    case _ => Left(CliParseError.Rejected)

  def spec = suite("CLI and CI test cells")(
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
            command <- Grammar.parse(shell.split(" ").toList.drop(1))
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
