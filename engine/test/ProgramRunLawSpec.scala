package histos.program

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, ResolutionService, Resolve}
import histos.toolchain.Runtimes
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{
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

import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

/** Runs the main a one-platform module names. The compiler jars are the locked ones, read offline. */
object ProgramRunLawSpec extends ZIOSpecDefault:
  private val scala3  = ScalaVersion("3.10.0")
  private val lawName = ProjectName.trusted("law")

  def spec = suite("program run laws")(
    test("a main that returns exits 0"):
      Workspace
        .work("histos-program-run")
        .flatMap: dir =>
          Machine
            .load(dir)
            .either
            .flatMap:
              case Left(error)    => ZIO.succeed(assertTrue(error.message == ""))
              case Right(machine) =>
                GraphCompile
                  .jdk(machine)
                  .either
                  .flatMap:
                    case Left(error) => ZIO.succeed(assertTrue(error.message == ""))
                    case Right(jdk)  =>
                      ZIO.serviceWithZIO[DependencyLock]: lock =>
                        val probe = dir.resolve("src").resolve("main").resolve("scala").resolve("Probe.scala")
                        val ran   =
                          for
                            runtimes <- Runtimes.read(machine, None)
                            _        <- Workspace.writeString(
                              probe,
                              "object Probe:\n  def main(args: Array[String]): Unit = ()\n",
                            )
                            collected <- Program
                              .console(
                                dir,
                                graph,
                                lawName.value,
                                jdk,
                                runtimes,
                                None,
                                _ => ZIO.unit,
                                ZIO.unit,
                              )
                              .runCollect
                              .either
                            detail = collected.fold(_.message, _ => "")
                            codes  = collected.toOption.toList
                              .flatMap(chunk => chunk.collect { case ProgramFeed.Exited(_, _, _, code) => code }.toList)
                            err = collected.toOption.toList
                              .flatMap(chunk => chunk.collect { case ProgramFeed.Err(text) => text }.toList)
                            noise = if codes == List(0) then "" else err.mkString("\n")
                          yield assertTrue(
                            detail == "",
                            codes == List(0),
                            noise == "",
                          )
                        ran.provideSomeLayer[TestEnvironment & Scope & Resolve](
                          ResolutionService.pinned(pinned(lock)) ++
                            ZLayer.succeed(ContentStore.in(dir.resolve("store")))
                        )
    ,
    test("generated text replaces the unmanaged resource the main reads"):
      Workspace
        .work("histos-program-resource")
        .flatMap: dir =>
          Machine
            .load(dir)
            .either
            .flatMap:
              case Left(error)    => ZIO.succeed(assertTrue(error.message == ""))
              case Right(machine) =>
                GraphCompile
                  .jdk(machine)
                  .either
                  .flatMap:
                    case Left(error) => ZIO.succeed(assertTrue(error.message == ""))
                    case Right(jdk)  =>
                      ZIO.serviceWithZIO[DependencyLock]: lock =>
                        val probe = dir.resolve("src").resolve("main").resolve("scala").resolve("Probe.scala")
                        val note  = dir.resolve("src").resolve("main").resolve("resources").resolve("note.txt")
                        val ran   =
                          for
                            runtimes  <- Runtimes.read(machine, None)
                            _         <- Workspace.writeString(probe, generatedProbe)
                            _         <- Workspace.writeString(note, "plain")
                            collected <- Program
                              .console(
                                dir,
                                graphWith(List(GeneratedAsset.Text(ResourceName("note.txt"), "generated"))),
                                lawName.value,
                                jdk,
                                runtimes,
                                None,
                                _ => ZIO.unit,
                                ZIO.unit,
                              )
                              .runCollect
                              .either
                            landed <- Workspace.readIfPresent(
                              GraphCompile.classesDir(dir, lawName, scala3).resolve("note.txt")
                            )
                            detail = collected.fold(_.message, _ => "")
                            codes  = collected.toOption.toList
                              .flatMap(chunk => chunk.collect { case ProgramFeed.Exited(_, _, _, code) => code }.toList)
                            err = collected.toOption.toList
                              .flatMap(chunk => chunk.collect { case ProgramFeed.Err(text) => text }.toList)
                            noise = if codes == List(0) then "" else err.mkString("\n")
                            found = landed.getOrElse("")
                          yield assertTrue(
                            detail == "",
                            codes == List(0),
                            noise == "",
                            found == "generated",
                          )
                        ran.provideSomeLayer[TestEnvironment & Scope & Resolve](
                          ResolutionService.pinned(pinned(lock)) ++
                            ZLayer.succeed(ContentStore.in(dir.resolve("store")))
                        ),
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

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

  private def graph: ProjectGraph =
    graphWith(Nil)

  private def graphWith(resources: List[GeneratedAsset]): ProjectGraph =
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

  private def pinned(lock: DependencyLock): DependencyLock =
    lock.copy(cells = LockedCell(lawName, scala3, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile) :: lock.cells)

  private def opened =
    Resolve.offline(_root_.coursier.cache.CacheDefaults.location.toPath) ++
      ZLayer.fromZIO(Resolve.readLock(repository))

  private def repository: Path =
    def walk(path: Path, left: Int): Option[Path] =
      if left == 0 then None
      else if Files.isRegularFile(path.resolve("dependencies.lock")) && Files.isRegularFile(
          path.resolve("histos.scala")
        )
      then Some(path)
      else
        Option(path.getParent) match
          case Some(parent) => walk(parent, left - 1)
          case None         => None
    walk(Path.of("").toAbsolutePath.normalize, 8).getOrElse(Path.of("").toAbsolutePath.normalize)
end ProgramRunLawSpec
