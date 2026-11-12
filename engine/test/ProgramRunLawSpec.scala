package histos.program

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.resolve.{ResolutionService, Resolve}
import histos.toolchain.Runtimes
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{
  CompilerClosure,
  GeneratedAsset,
  MainClass,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  ResourceName,
  Resolution,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
}

import zio.*
import zio.test.*

/** Runs the main a one-platform module names. The compiler jars are the locked ones, read offline. */
object ProgramRunLawSpec extends ZIOSpecDefault:
  private val lawName = ProjectName.trusted("law")

  def spec = suite("program run laws")(
    test("generated text replaces the unmanaged resource the main reads, and the main exits 0"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val scala3 = closure.scalaVersion
        Workspace
          .work("histos-program-resource")
          .flatMap: dir =>
            val probe = dir.resolve("src").resolve("main").resolve("scala").resolve("Probe.scala")
            val note  = dir.resolve("src").resolve("main").resolve("resources").resolve("note.txt")
            val ran   =
              for
                machine   <- Machine.load(dir)
                jdk       <- GraphCompile.jdk(machine)
                runtimes  <- Runtimes.read(machine, None)
                _         <- Workspace.writeString(probe, generatedProbe)
                _         <- Workspace.writeString(note, "plain")
                collected <- Program
                  .console(
                    dir,
                    graph(scala3, List(GeneratedAsset.Text(ResourceName("note.txt"), "generated"))),
                    lawName.value,
                    jdk,
                    runtimes,
                    None,
                    _ => ZIO.unit,
                    ZIO.unit,
                  )
                  .runCollect
                landed <- Workspace.readIfPresent(GraphCompile.classesDir(dir, lawName, scala3).resolve("note.txt"))
              yield
                val codes = collected.collect { case ProgramFeed.Exited(_, _, _, code) => code }.toList
                val err   = collected.collect { case ProgramFeed.Err(text) => text }.toList
                // A failed run shows its stderr; a JVM warning beside a clean exit is not a failure.
                val noise = if codes == List(0) then Nil else err
                assertTrue(
                  codes == List(0),
                  noise == Nil,
                  landed.contains("generated"),
                )
            ran.provideSomeLayer[TestEnvironment & Scope & Resolve](
              ResolutionService.pinned(closure.pinning(lawName)) ++
                ZLayer.succeed(ContentStore.in(dir.resolve("store")))
            )
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer) @@ TestAspect.withLiveSystem

  private val generatedProbe =
    """object Probe:
      |  def main(args: Array[String]): Unit =
      |    val text = Option(Probe.getClass.getResourceAsStream("/note.txt")) match
      |      case Some(stream) =>
      |        try new String(stream.readAllBytes, java.nio.charset.StandardCharsets.UTF_8)
      |        finally stream.close()
      |      case None =>
      |        sys.error("missing note")
      |    if text != "generated" then sys.error(text)
      |""".stripMargin

  private def graph(scala3: ScalaVersion, resources: List[GeneratedAsset]): ProjectGraph =
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
      application = Some(MainClass("Probe")),
      resources = resources,
    )
    ProjectGraph.trusted(
      List(law),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )
end ProgramRunLawSpec
