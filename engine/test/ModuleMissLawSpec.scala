package histos.testrun

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.resolve.{DependencyLock, Resolve}
import histos.toolchain.Runtimes
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{
  CompilerClosure,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  Resolution,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
  TestMain,
}

import java.nio.file.Files

import zio.{Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** A test main is its module's one suite, so a pattern that does not name it is a miss before the tests compile. */
object ModuleMissLawSpec extends ZIOSpecDefault:
  private val lawName = ProjectName.trusted("misslaw")
  private val pattern = SuitePattern("Nope")

  def spec = suite("module miss laws")(
    test("a pattern that does not name the test main fails, and the main compiles while the tests do not"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val scala3 = closure.scalaVersion
        Workspace
          .work("histos-module-miss")
          .flatMap: dir =>
            val main = dir.resolve(lawName.value).resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val probe =
              dir.resolve(lawName.value).resolve("src").resolve("test").resolve("scala").resolve("Probe.scala")
            val program =
              for
                machine  <- Machine.load(dir)
                jdk      <- GraphCompile.jdk(machine)
                runtimes <- Runtimes.read(machine, None)
                _        <- Workspace
                  .writeString(dir.resolve("dependencies.lock"), DependencyLock.render(closure.pinning(lawName)))
                _   <- Workspace.writeString(main, "object Hello:\n  def ready: Int = 1\n")
                _   <- Workspace.writeString(probe, "object Probe:\n  def main(args: Array[String]): Unit = ()\n")
                ran <- ModuleTest
                  .run(TestRun(dir, graph(scala3), List(lawName), jdk, runtimes, TestMode.Run, List(pattern)))
                  .either
                built <- ZIO
                  .attemptBlocking(
                    (
                      Files.isRegularFile(GraphCompile.classesDir(dir, lawName, scala3).resolve("Hello.class")),
                      Files.exists(TestCompile.testClasses(dir, lawName, scala3, Platform.Jvm)),
                    )
                  )
                  .orDie
                named <- ModuleTest
                  .run(
                    TestRun(dir, graph(scala3), List(lawName), jdk, runtimes, TestMode.Run, List(SuitePattern("Probe")))
                  )
                  .map(_.map(_.cell.status))
                probed <- ZIO
                  .attemptBlocking(
                    Files
                      .isRegularFile(TestCompile.testClasses(dir, lawName, scala3, Platform.Jvm).resolve("Probe.class"))
                  )
                  .orDie
              yield assertTrue(
                ran == Left(ModuleTestError.NoneMatched(List(pattern))),
                ran.fold(_.message, _ => "matched") == "no tests matched Nope",
                built == (true, false),
                named == List(TestStatus.Passed),
                probed,
              )
            program.provideSomeLayer[Resolve & Scope](ZLayer.succeed(ContentStore.in(dir.resolve("store"))))
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer) @@ TestAspect.withLiveSystem

  private def graph(scala3: ScalaVersion): ProjectGraph =
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
      test = Some(TestMain("Probe")),
    )
    ProjectGraph.trusted(
      List(law),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )
end ModuleMissLawSpec
