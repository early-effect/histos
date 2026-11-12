package histos.doc

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.resolve.{DependencyLock, Resolve}
import histos.task.{TaskError, TaskResult}
import histos.workspace.Workspace
import histos.zinc.{AnalysisKind, FileStaleness, GraphCompile, SourceStale}
import histos.{CompilerClosure, Jvm, ProjectName, Projects, ScalaVersion, Ship, module, organization, ships}

import java.nio.file.{Files, Path}

import zio.{Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** Scaladoc follows the catalog. A ship member is documented from the classes just compiled. Anything else is not. */
object DocLawSpec extends ZIOSpecDefault:
  private val group = organization"example.com"

  def spec = suite("documentation laws")(
    test("a catalog with no ships does not compile"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        Workspace
          .work("histos-doc-idle")
          .flatMap: dir =>
            val program =
              for
                graph   <- ZIO.fromEither(idle(closure.scalaVersion)).catchAll(error => ZIO.dieMessage(error.message))
                machine <- machineOf(dir)
                ran     <- DocTask.fromRepo(graph, dir, machine).either
                documented <- ZIO.attempt(Files.exists(dir.resolve(".histos").resolve("doc"))).orDie
              yield assertTrue(
                ran == Right(TaskResult.Doc),
                !documented,
              )
            program.provideSomeLayer[Resolve & Scope](ZLayer.succeed(ContentStore.in(dir.resolve("store"))))
    ,
    test("a ship member is documented and its dependency is not"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val scala3 = closure.scalaVersion
        Workspace
          .work("histos-doc-ship")
          .flatMap: dir =>
            val coreSrc   = dir.resolve("doccore").resolve("src").resolve("main").resolve("scala").resolve("Core.scala")
            val appSrc    = dir.resolve("doclaw").resolve("src").resolve("main").resolve("scala").resolve("App.scala")
            val index     = docOf(dir, "doclaw", scala3).resolve("index.html")
            val coreDoc   = docOf(dir, "doccore", scala3)
            val coreClass =
              GraphCompile.classesDir(dir, ProjectName.trusted("doccore"), scala3).resolve("Core.class")
            val analysis =
              SourceStale.directory(dir, ProjectName.trusted("doclaw"), scala3, PlatformJvm, AnalysisKind.Main)
            val source = appSrc.toAbsolutePath.normalize
            val pinned =
              closure.pinning(ProjectName.trusted("doccore"), ProjectName.trusted("doclaw"))
            val program =
              for
                graph   <- ZIO.fromEither(shipped(scala3)).catchAll(error => ZIO.dieMessage(error.message))
                machine <- machineOf(dir)
                _ <- Workspace.createDirectories(parentOf(coreSrc)).catchAll(error => ZIO.dieMessage(error.message))
                _ <- Workspace.createDirectories(parentOf(appSrc)).catchAll(error => ZIO.dieMessage(error.message))
                _ <- Workspace.writeString(coreSrc, coreText).catchAll(error => ZIO.dieMessage(error.message))
                _ <- Workspace.writeString(appSrc, appText).catchAll(error => ZIO.dieMessage(error.message))
                _ <- Workspace
                  .writeString(dir.resolve("dependencies.lock"), DependencyLock.render(pinned))
                  .catchAll(error => ZIO.dieMessage(error.message))
                ran        <- DocTask.fromRepo(graph, dir, machine).either
                wroteIndex <- ZIO.attempt(Files.isRegularFile(index)).orDie
                wroteCore  <- ZIO.attempt(Files.exists(coreDoc)).orDie
                wroteClass <- ZIO.attempt(Files.isRegularFile(coreClass)).orDie
                fresh      <- SourceStale.read(analysis, List(source))
                _          <- Workspace.writeString(appSrc, appChanged).catchAll(error => ZIO.dieMessage(error.message))
                stale      <- SourceStale.read(analysis, List(source))
              yield
                val detail = ran.fold(_.message, _ => "")
                assertTrue(
                  ran == Right(TaskResult.Doc),
                  detail.isEmpty,
                  wroteIndex,
                  !wroteCore,
                  wroteClass,
                  fresh == FileStaleness.Known(Nil),
                  stale == FileStaleness.Known(List(source)),
                )
            program.provideSomeLayer[Resolve & Scope](ZLayer.succeed(ContentStore.in(dir.resolve("store"))))
    ,
    test("an unknown name is a documentation failure"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val scala3 = closure.scalaVersion
        Workspace
          .work("histos-doc-bad")
          .flatMap: dir =>
            val source  = dir.resolve("docbad").resolve("src").resolve("main").resolve("scala").resolve("Bad.scala")
            val program =
              for
                graph   <- ZIO.fromEither(broken(scala3)).catchAll(error => ZIO.dieMessage(error.message))
                machine <- machineOf(dir)
                _ <- Workspace.createDirectories(parentOf(source)).catchAll(error => ZIO.dieMessage(error.message))
                _ <- Workspace.writeString(source, badText).catchAll(error => ZIO.dieMessage(error.message))
                _ <- Workspace
                  .writeString(
                    dir.resolve("dependencies.lock"),
                    DependencyLock.render(closure.pinning(ProjectName.trusted("docbad"))),
                  )
                  .catchAll(error => ZIO.dieMessage(error.message))
                ran   <- DocTask.fromRepo(graph, dir, machine).either
                index <- ZIO.attempt(Files.exists(docOf(dir, "docbad", scala3))).orDie
              yield assertTrue(
                ran match
                  case Left(error: TaskError.Doc) => error.message.contains("nope")
                  case _                          => false
                ,
                !index,
              )
            program.provideSomeLayer[Resolve & Scope](ZLayer.succeed(ContentStore.in(dir.resolve("store")))),
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer) @@ TestAspect.withLiveSystem

  private val PlatformJvm = histos.Platform.Jvm

  private def idle(scala3: ScalaVersion) =
    Projects(module("docidle", Jvm)(scala3))

  private def shipped(scala3: ScalaVersion) =
    val core = module("doccore", Jvm)(scala3)
    val app  = module("doclaw", Jvm)(scala3).dependsOn(core)
    Projects.organization(group).locked(ships(Ship("demo").next(1, 2, 3)(app)), core, app)

  private def broken(scala3: ScalaVersion) =
    val bad = module("docbad", Jvm)(scala3)
    Projects.organization(group).locked(ships(Ship("demo").next(1, 2, 3)(bad)), bad)

  private val coreText =
    """object Core:
      |  def ready: Int = 1
      |""".stripMargin

  private val appText =
    """object App:
      |  def n: Int = Core.ready
      |""".stripMargin

  private val appChanged =
    """object App:
      |  def n: Int = Core.ready + 1
      |""".stripMargin

  private val badText =
    """object Bad:
      |  def n: Int = nope
      |""".stripMargin

  private def docOf(dir: Path, name: String, scala3: ScalaVersion): Path =
    dir.resolve(".histos").resolve("doc").resolve(name).resolve(scala3.value).resolve("jvm")

  private def machineOf(dir: Path) =
    Machine.load(dir).catchAll(error => ZIO.dieMessage(error.message))

  private def parentOf(path: Path): Path =
    Option(path.getParent).getOrElse(path)
end DocLawSpec
