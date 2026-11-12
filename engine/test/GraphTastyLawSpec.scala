package histos.zinc

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.native.NativeOrigin
import histos.resolve.{ResolutionService, Resolve}
import histos.version.Catalog
import histos.workspace.Workspace
import histos.{
  CompilerClosure,
  Module,
  Platform,
  Platforms,
  ProjectDep,
  ProjectGraph,
  ProjectName,
  Resolution,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
}

import java.nio.file.{Files, Path}

import zio.{Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** A module on an older Scala 3 series cannot take a newer upstream. The upstream still compiles. */
object GraphTastyLawSpec extends ZIOSpecDefault:
  private val coreName = ProjectName.trusted("tastycore")
  private val appName  = ProjectName.trusted("tastyapp")

  def spec = suite("graph tasty laws")(
    test("an older series refuses a newer upstream after that upstream has classes"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        Workspace
          .work("histos-graph-tasty")
          .flatMap: dir =>
            val newer   = closure.scalaVersion
            val coreSrc =
              dir.resolve(coreName.value).resolve("src").resolve("main").resolve("scala").resolve("Core.scala")
            val appSrc = dir.resolve(appName.value).resolve("src").resolve("main").resolve("scala").resolve("App.scala")
            val coreClass = GraphCompile.classesDir(dir, coreName, newer, Platform.Jvm).resolve("Core.class")
            for
              older   <- closure.older
              loaded  <- Machine.load(dir).either
              machine <- loaded match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value)
              found <- GraphCompile.jdk(machine).either
              jdk   <- found match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value)
              _ <- Workspace.createDirectories(parentOf(coreSrc))
              _ <- Workspace.createDirectories(parentOf(appSrc))
              _ <- Workspace.writeString(coreSrc, "object Core:\n  def ready: Int = 1\n")
              _ <- Workspace.writeString(appSrc, "object App:\n  def n: Int = Core.ready\n")
              pinned = closure.pinningAt(coreName -> newer, appName -> older)
              compiled <- GraphCompile
                .run(
                  CompileSession(dir, graph(newer, older), jdk, NativeOrigin.read(machine.env), lock = pinned),
                  List(appName),
                  Some(Platform.Jvm),
                  _ => ZIO.unit,
                )
                .either
                .provideSomeLayer[Resolve & Scope](
                  ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++ ResolutionService.pinned(pinned)
                )
              wrote <- ZIO.attempt(Files.isRegularFile(coreClass)).orDie
            yield
              val detail = compiled.fold(_.message, _ => "")
              assertTrue(
                wrote,
                detail.contains(s"'tastyapp' ${older.value} cannot read 'tastycore' ${newer.value} jvm"),
              )
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer) @@ TestAspect.withLiveSystem

  private def graph(newer: ScalaVersion, older: ScalaVersion): ProjectGraph =
    val core = Module(
      coreName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(newer, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )
    val app = Module(
      appName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(older, Nil),
      Nil,
      List(ProjectDep(coreName, None)),
      SourcePos("law.scala", 2, 1),
    )
    ProjectGraph.trusted(
      List(core, app),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def parentOf(path: Path): Path =
    Option(path.getParent).getOrElse(path)
end GraphTastyLawSpec
