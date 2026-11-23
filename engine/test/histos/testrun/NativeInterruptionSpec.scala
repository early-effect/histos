package histos.testrun

import histos.*
import histos.build.BuildNode
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.lang.ProcessHandle
import _root_.scala.jdk.OptionConverters.*

import zio.{ZIO, durationInt}
import zio.test.*

object NativeInterruptionSpec extends ZIOSpecDefault:
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

  def spec = tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.minutes)

  private val source =
    """package demo
      |import zio.*
      |import zio.test.*
      |object HeldSpec extends ZIOSpecDefault:
      |  enum Failure:
      |    case Write
      |  def spec = test("held native process") {
      |    val marker = java.nio.file.Path.of(sys.env.getOrElse("HISTOS_REPO", "."), "native-pid")
      |    ZIO.attemptBlocking(java.nio.file.Files.writeString(marker, scala.scalanative.posix.unistd.getpid().toString))
      |      .mapError(_ => Failure.Write) *> ZIO.never
      |  }
      |""".stripMargin
end NativeInterruptionSpec
