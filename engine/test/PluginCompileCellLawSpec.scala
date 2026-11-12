package histos.load

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.native.NativeOrigin
import histos.resolve.{ResolutionService, Resolve}
import histos.testrun.TestCompile
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompileSession, GraphCompile}
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
}

import java.nio.file.Files

import zio.{Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** A plugin cell compiles its main and its tests. A type error is a compile fault. */
object PluginCompileCellLawSpec extends ZIOSpecDefault:
  private val lawName = ProjectName.trusted("pluglaw")

  def spec = suite("plugin compile cell laws")(
    test("the cell writes main and test classes, and a type error is a compile fault"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val scala3 = closure.scalaVersion
        Workspace
          .work("histos-plugin-cell")
          .flatMap: dir =>
            val main  = dir.resolve(lawName.value).resolve("src").resolve("main").resolve("scala").resolve("Plug.scala")
            val probe =
              dir.resolve(lawName.value).resolve("src").resolve("test").resolve("scala").resolve("Probe.scala")
            val mainClass = GraphCompile.classesDir(dir, lawName, scala3, Platform.Jvm).resolve("Plug.class")
            val testClass = TestCompile.testClasses(dir, lawName, scala3, Platform.Jvm).resolve("Probe.class")
            val cell      = PluginCells.Cell(module(scala3), scala3, Platform.Jvm)
            val program   =
              for
                machine <- Machine.load(dir)
                jdk     <- GraphCompile.jdk(machine)
                _       <- Workspace.writeString(main, "object Plug:\n  def ready: Int = 1\n")
                _       <- Workspace.writeString(probe, "object Probe:\n  def n: Int = Plug.ready\n")
                session = CompileSession(
                  dir,
                  graph(scala3),
                  jdk,
                  NativeOrigin.read(machine.env),
                  closure.pinning(lawName),
                )
                compiled  <- PluginCells.compile(session, cell).either
                wroteMain <- ZIO.attemptBlocking(Files.isRegularFile(mainClass)).orDie
                wroteTest <- ZIO.attemptBlocking(Files.isRegularFile(testClass)).orDie
                _         <- Workspace.writeString(main, "object Plug:\n  def ready: Int = \"no\"\n")
                broken    <- PluginCells.compile(session, cell).either
              yield
                val detail = broken.fold(_.message, _ => "")
                assertTrue(
                  compiled == Right(()),
                  wroteMain,
                  wroteTest,
                  detail.contains(s"'pluglaw' ${scala3.value} jvm failed to compile"),
                )
            program.provideSomeLayer[Resolve & Scope](
              ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++
                ResolutionService.pinned(closure.pinning(lawName))
            )
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer) @@ TestAspect.withLiveSystem

  private def module(scala3: ScalaVersion): Module =
    Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )

  private def graph(scala3: ScalaVersion): ProjectGraph =
    ProjectGraph.trusted(
      List(module(scala3)),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )
end PluginCompileCellLawSpec
