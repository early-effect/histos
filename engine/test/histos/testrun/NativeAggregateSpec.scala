package histos.testrun

import histos.*
import histos.build.BuildNode
import histos.cache.LinkerStarts
import histos.plugin.{RepoPath, SourceSet}
import histos.suite.HistosSpec
import histos.workspace.Workspace
import histos.zinc.ApiDigest

import zio.{ZIO, durationInt}
import zio.test.*

object NativeAggregateSpec extends HistosSpec:
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
              leftSource <- TestExecutionFixtures.checkout(
                "engine/src/test/resources/fixtures/native-aggregate/LeftSpec.scala"
              )
              rightSource <- TestExecutionFixtures.checkout(
                "engine/src/test/resources/fixtures/native-aggregate/RightSpec.scala"
              )
              _        <- Workspace.writeString(fixture.repo.resolve("left/test/demo/LeftSpec.scala"), leftSource)
              _        <- Workspace.writeString(fixture.repo.resolve("right/test/demo/RightSpec.scala"), rightSource)
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

  def spec = (tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(15.minutes) @@ TestAspect.sequential)
    .provideShared(histos.zinc.Compilers.owned)

  private def completed(feeds: List[TestFeed]): Map[ProjectName, TestStatus] =
    feeds.collect { case TestFeed.Done(cell) => cell.module -> cell.status }.toMap

  private def count(fixture: TestExecutionFixtures.Fixture, name: String) =
    Workspace.readIfPresent(fixture.repo.resolve(s"$name-counter")).map(_.toList.flatMap(_.linesIterator).size)
end NativeAggregateSpec
