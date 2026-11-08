package histos.testrun

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, Resolve}
import histos.toolchain.Runtimes
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{
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

import java.nio.file.{Files, Path}

import zio.{Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** A pattern that names nothing is a miss after the test class has compiled. */
object ModuleMissLawSpec extends ZIOSpecDefault:
  private val scala3  = ScalaVersion("3.10.0")
  private val lawName = ProjectName.trusted("misslaw")

  def spec = suite("module miss laws")(
    test("a pattern that names nothing fails after the test class exists"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
        Workspace.work("histos-module-miss").flatMap: dir =>
          val main  = dir.resolve(lawName.value).resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
          val probe = dir.resolve(lawName.value).resolve("test").resolve("scala").resolve("Probe.scala")
          val program =
            for
              pattern <- SuitePattern.from("Nope") match
                case Right(found) => ZIO.succeed(found)
                case Left(error)  => ZIO.dieMessage(error.message)
              loaded <- Machine.load(dir).either
              machine <- loaded match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value)
              found <- GraphCompile.jdk(machine).either
              jdk <- found match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value)
              runtimes <- Runtimes.read(machine, None)
              _ <- Workspace.createDirectories(parentOf(main))
              _ <- Workspace.createDirectories(parentOf(probe))
              _ <- Workspace.writeString(dir.resolve("dependencies.lock"), DependencyLock.render(pinned(lock)))
              _ <- Workspace.writeString(main, "object Hello:\n  def ready: Int = 1\n")
              _ <- Workspace.writeString(probe, "object Probe:\n  def main(args: Array[String]): Unit = ()\n")
              ran <- ModuleTest
                .run(TestRun(dir, graph, List(lawName), jdk, runtimes, TestMode.Run, List(pattern)))
                .either
            yield
              val detail = ran.fold(_.message, _ => "matched")
              assertTrue(
                detail == "no tests matched Nope",
                ran == Left(ModuleTestError.NoneMatched(List(pattern))),
              )
          program.provideSomeLayer[Resolve & Scope](ZLayer.succeed(ContentStore.in(dir.resolve("store"))))
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private def graph: ProjectGraph =
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

  private def parentOf(path: Path): Path =
    Option(path.getParent).getOrElse(path)
end ModuleMissLawSpec
