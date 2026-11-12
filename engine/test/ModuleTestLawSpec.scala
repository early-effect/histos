package histos.testrun

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.resolve.{ResolutionService, Resolve}
import histos.toolchain.Runtimes
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompileMemo, Compilers, GraphCompile}
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

import zio.*
import zio.test.*

/** Compiles a module and runs its main. The compiler jars are the locked ones, read offline. */
object ModuleTestLawSpec extends ZIOSpecDefault:
  private val lawName = ProjectName.trusted("law")

  def spec = suite("module test laws")(
    test("a test main that reads its own resource and a main resource is a pass"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val scala3 = closure.scalaVersion
        Workspace
          .work("histos-module-resource")
          .flatMap: dir =>
            val main  = dir.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val probe = dir.resolve("src").resolve("test").resolve("scala").resolve("Probe.scala")
            val note  = dir.resolve("src").resolve("main").resolve("resources").resolve("note.txt")
            val own   = dir.resolve("src").resolve("test").resolve("resources").resolve("probe.txt")
            val ran   =
              for
                machine   <- Machine.load(dir)
                jdk       <- GraphCompile.jdk(machine)
                runtimes  <- Runtimes.read(machine, None)
                _         <- Workspace.writeString(main, "object Hello:\n  def ready: Int = 1\n")
                _         <- Workspace.writeString(probe, resourceProbe)
                _         <- Workspace.writeString(note, "main")
                _         <- Workspace.writeString(own, "test")
                collected <- ModuleTest
                  .sessionOn(TestRun(dir, graph(scala3), List(lawName), jdk, runtimes, TestMode.Run))
                  .runCollect
                mainText <- Workspace.readIfPresent(GraphCompile.classesDir(dir, lawName, scala3).resolve("note.txt"))
                testText <- Workspace.readIfPresent(
                  TestCompile.testClasses(dir, lawName, scala3, Platform.Jvm).resolve("probe.txt")
                )
              yield
                val statuses = collected.collect { case SessionFeed.Test(TestFeed.Done(cell)) => cell.status }.toList
                val err      = collected.collect { case SessionFeed.Test(TestFeed.Err(_, _, _, text)) => text }.toList
                // A failed run shows its stderr; a JVM warning beside a pass is not a failure.
                val noise = if statuses == List(TestStatus.Passed) then Nil else err
                assertTrue(
                  statuses == List(TestStatus.Passed),
                  noise == Nil,
                  mainText.contains("main"),
                  testText.contains("test"),
                )
            ran.provideSomeLayer[TestEnvironment & Scope & Resolve](
              ResolutionService.pinned(closure.pinning(lawName)) ++
                CompileMemo.live ++
                Compilers.live(1) ++
                ZLayer.succeed(ContentStore.in(dir.resolve("store")))
            )
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer) @@ TestAspect.withLiveSystem

  private val resourceProbe =
    """object Probe:
      |  def main(args: Array[String]): Unit =
      |    def text(name: String): String =
      |      Option(Probe.getClass.getResourceAsStream(name)) match
      |        case Some(stream) =>
      |          try new String(stream.readAllBytes, java.nio.charset.StandardCharsets.UTF_8)
      |          finally stream.close()
      |        case None =>
      |          sys.error("missing " + name)
      |    val note  = text("/note.txt")
      |    val probe = text("/probe.txt")
      |    if note != "main" || probe != "test" then sys.error(note + " " + probe)
      |""".stripMargin

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
end ModuleTestLawSpec
