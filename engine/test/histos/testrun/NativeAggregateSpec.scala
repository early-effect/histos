package histos.testrun

import histos.*
import histos.build.BuildNode
import histos.cache.LinkerStarts
import histos.plugin.{RepoPath, SourceSet}
import histos.workspace.Workspace
import histos.zinc.ApiDigest

import zio.{ZIO, durationInt}
import zio.test.*

object NativeAggregateSpec extends ZIOSpecDefault:
  private val version   = TestExecutionFixtures.version
  private val leftName  = ProjectName("left")
  private val rightName = ProjectName("right")
  private val ownerName = ProjectName("nativeTests")
  private val provider  = List(dep"dev.zio::zio-test:2.1.27", dep"dev.zio::zio-test-sbt:2.1.27")

  private val tests = suite("actual Native test groups")(
    test(
      "a source-free owner links two real member Test products once; quick/resources/focus/filter keep group identity"
    ) {
      val left = module("left", Native)(version)
        .clang(ClangMajor(21))
        .nativeVersion(TestExecutionFixtures.nightly)
        .testDependencies((provider :+ dep"dev.zio::zio-json:1.1.1")*)
        .roots(ScopedRoot(RepoPath("assets/left"), SourceSet.Test, PlatformSelection.only(Native), RootKind.Resources))
      val right = module("right", Native)(version)
        .clang(ClangMajor(21))
        .nativeVersion(TestExecutionFixtures.nightly)
        .testDependencies((provider :+ dep"dev.zio::zio-streams:2.1.27")*)
        .roots(ScopedRoot(RepoPath("assets/right"), SourceSet.Test, PlatformSelection.only(Native), RootKind.Resources))
      val owner = module("nativeTests", Native)(version)
        .clang(ClangMajor(21))
        .nativeVersion(TestExecutionFixtures.nightly)
        .testAggregate(left, right)
      ZIO
        .fromEither(Projects(left, right, owner))
        .flatMap: graph =>
          TestExecutionFixtures.withGraph(graph): fixture =>
            val asked = TestRun(
              fixture.repo,
              graph,
              Nil,
              fixture.session.jdk,
              fixture.runtimes,
              TestMode.Run,
              platforms = PlatformSelection.only(Native),
            )
            for
              _        <- Workspace.writeString(fixture.repo.resolve("left/test/demo/LeftSpec.scala"), leftSpec)
              _        <- Workspace.writeString(fixture.repo.resolve("right/test/demo/RightSpec.scala"), rightSpec)
              _        <- Workspace.writeString(fixture.repo.resolve("assets/left/left/value.txt"), "left-one")
              _        <- Workspace.writeString(fixture.repo.resolve("assets/right/right/value.txt"), "right")
              plan     <- ZIO.fromEither(TestCells.plan(graph, asked.selection))
              products <- fixture.ensure(plan.compileStageRoots.map(BuildNode.Compile(_)))
              main     <- ZIO
                .fromEither(products.require(CompilationStage(Cell(ownerName, version, Native), SourceSet.Main)))
              leftProduct <- ZIO
                .fromEither(products.require(CompilationStage(Cell(leftName, version, Native), SourceSet.Test)))
              rightProduct <- ZIO
                .fromEither(products.require(CompilationStage(Cell(rightName, version, Native), SourceSet.Test)))
              first      <- ModuleTest.execute(asked, plan, products).runCollect
              leftCount  <- count(fixture, "left")
              rightCount <- count(fixture, "right")
              starts     <- ZIO.serviceWithZIO[LinkerStarts](_.get)
              quick      <- ModuleTest.execute(asked.copy(mode = TestMode.Quick), plan, products).runCollect
              unchanged  <- ZIO.serviceWithZIO[LinkerStarts](_.get)
              _          <- Workspace.writeString(fixture.repo.resolve("assets/left/left/value.txt"), "left-two")
              updated    <- fixture.ensure(plan.compileStageRoots.map(BuildNode.Compile(_)))
              changed    <- ModuleTest.execute(asked.copy(mode = TestMode.Quick), plan, updated).runCollect
              leftAfter  <- count(fixture, "left")
              rightAfter <- count(fixture, "right")
              relinks    <- ZIO.serviceWithZIO[LinkerStarts](_.get)
              focused = asked.copy(names = List(leftName), patterns = List(SuitePattern("*LeftSpec*")))
              focusedPlan     <- ZIO.fromEither(TestCells.plan(graph, focused.selection))
              focusedProducts <- fixture.ensure(focusedPlan.compileStageRoots.map(BuildNode.Compile(_)))
              direct          <- ModuleTest.execute(focused, focusedPlan, focusedProducts).runCollect
              focusedLeft     <- count(fixture, "left")
              focusedRight    <- count(fixture, "right")
              filtered        <- ModuleTest
                .execute(asked.copy(filter = TestFilter(List(TestTerm("left")), Nil, Nil)), plan, updated)
                .runCollect
              filteredLeft  <- count(fixture, "left")
              filteredRight <- count(fixture, "right")
            yield assertTrue(
              main.compiled.classFiles == 0,
              main.api == ApiDigest.empty,
              leftProduct.compiled.module == leftName,
              rightProduct.compiled.module == rightName,
              leftProduct.classes != rightProduct.classes,
              leftProduct.classpath.exists(_.toString.contains("zio-json")),
              rightProduct.classpath.exists(_.toString.contains("zio-streams")),
              completed(first.toList) == Map(
                leftName  -> TestStatus.Passed,
                rightName -> TestStatus.Passed,
                ownerName -> TestStatus.NoTests,
              ),
              leftCount == 1,
              rightCount == 1,
              starts == 1,
              quick.count { case TestFeed.Cached(_, _, _, _) => true; case _ => false } == 2,
              unchanged == starts,
              completed(changed.toList).get(leftName).contains(TestStatus.Passed),
              leftAfter == 2,
              rightAfter == 2,
              relinks == 2,
              focusedPlan.executionGroups == List(TestExecutionGroup.Direct(Cell(leftName, version, Native))),
              completed(direct.toList) == Map(leftName -> TestStatus.Passed),
              focusedLeft == 3,
              focusedRight == 2,
              completed(filtered.toList).get(leftName).contains(TestStatus.Passed),
              filteredLeft == 4,
              filteredRight == 2,
            )
    }
  )

  def spec = tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(15.minutes) @@ TestAspect.sequential

  private def completed(feeds: List[TestFeed]): Map[ProjectName, TestStatus] =
    feeds.collect { case TestFeed.Done(cell) => cell.module -> cell.status }.toMap

  private def count(fixture: TestExecutionFixtures.Fixture, name: String) =
    Workspace.readIfPresent(fixture.repo.resolve(s"$name-counter")).map(_.toList.flatMap(_.linesIterator).size)

  private def support(name: String): String =
    s"""|  enum Failure:
        |    case Read, Write
        |  private def hit = ZIO.attemptBlocking:
        |    val path = java.nio.file.Path.of(sys.env.getOrElse("HISTOS_REPO", "."), "$name-counter")
        |    java.nio.file.Files.writeString(path, "$name\\n", java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND)
        |  .mapError(_ => Failure.Write)
        |  private def resource = ZIO.scoped:
        |    ZIO.acquireRelease(ZIO.fromOption(Option(getClass.getResourceAsStream("/$name/value.txt"))).orElseFail(Failure.Read))(
        |      input => ZIO.attemptBlocking(input.close()).ignore
        |    ).flatMap(input => ZIO.attemptBlocking(new String(input.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)).mapError(_ => Failure.Read))
        |""".stripMargin

  private val leftSpec =
    s"""|package demo
        |import zio.*
        |import zio.test.*
        |import zio.json.*
        |object LeftSpec extends ZIOSpecDefault:
        |${support("left")}
        |  def spec = test("left member dependency and resource") {
        |    for
        |      _ <- hit
        |      text <- resource
        |    yield assertTrue("42".fromJson[Int] == Right(42), text.startsWith("left-"))
        |  }
        |""".stripMargin

  private val rightSpec =
    s"""|package demo
        |import zio.*
        |import zio.test.*
        |import zio.stream.*
        |object RightSpec extends ZIOSpecDefault:
        |${support("right")}
        |  def spec = test("right member dependency and resource") {
        |    for
        |      _ <- hit
        |      text <- resource
        |      values <- ZStream(1, 2).runCollect
        |    yield assertTrue(text == "right", values.toList == List(1, 2))
        |  }
        |""".stripMargin
end NativeAggregateSpec
