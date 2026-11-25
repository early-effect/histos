package histos.testrun

import histos.*
import histos.build.BuildNode
import histos.suite.HistosSpec
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.lang.ProcessHandle
import _root_.scala.jdk.OptionConverters.*

import zio.{ZIO, durationInt}
import zio.test.*

object NativeInterruptionSpec extends HistosSpec:
  private val tests = suite("Native test interruption")(
    test("interrupting an actual aggregate child ends the Native process and records no success") {
      val version = TestExecutionFixtures.version
      val member  = module("held", Native)(version)
        .clang(ClangMajor(21))
        .nativeVersion(TestExecutionFixtures.nightly)
        .testDependencies(dep"dev.zio::zio-test:2.1.27", dep"dev.zio::zio-test-sbt:2.1.27")
      val owner = module("owner", Native)(version)
        .clang(ClangMajor(21))
        .nativeVersion(TestExecutionFixtures.nightly)
        .testAggregate(member)
      ZIO
        .fromEither(Projects(member, owner))
        .flatMap: graph =>
          TestExecutionFixtures.withGraph(graph): fixture =>
            val asked = TestRun(
              fixture.repo,
              graph,
              List(ProjectName("owner")),
              fixture.session.jdk,
              fixture.runtimes,
              TestMode.Run,
              platforms = PlatformSelection.only(Native),
            )
            val marker = fixture.repo.resolve("native-pid")
            for
              source <- TestExecutionFixtures.checkout(
                "engine/src/test/resources/fixtures/native-interruption/HeldSpec.scala"
              )
              _        <- Workspace.writeString(fixture.repo.resolve("held/test/demo/HeldSpec.scala"), source)
              plan     <- ZIO.fromEither(TestCells.plan(graph, asked.selection))
              products <- fixture.ensure(plan.compileStageRoots.map(BuildNode.Compile(_)))
              fiber    <- ModuleTest.execute(asked, plan, products).runCollect.forkScoped
              observed <- Workspace
                .readIfPresent(marker)
                .map(_.flatMap(_.trim.toLongOption))
                .repeatUntil(_.isDefined)
              pid   <- ZIO.fromOption(observed).orElseFail(IoFailure(marker, IoFault.Read))
              alive <- ZIO
                .attemptBlocking(ProcessHandle.of(pid).toScala.exists(_.isAlive))
                .mapError(_ => IoFailure(marker, IoFault.Stat))
              exit <- fiber.interrupt
              dead <- ZIO
                .attemptBlocking(!ProcessHandle.of(pid).toScala.exists(_.isAlive))
                .mapError(_ => IoFailure(marker, IoFault.Stat))
              success <- Workspace.readIfPresent(
                QuickRecord.file(fixture.repo, ProjectName("held"), version, Native, SuiteName.trusted("demo.HeldSpec"))
              )
            yield assertTrue(alive, exit.isInterrupted, dead, success.isEmpty)
    }
  )

  def spec =
    (tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.minutes)).provideShared(histos.zinc.Compilers.owned)
end NativeInterruptionSpec
