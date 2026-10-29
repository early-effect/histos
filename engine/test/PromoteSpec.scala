package histos.load

import histos.Promote
import histos.cache.ContentStore
import histos.resolve.IdleResolve
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, Path}

import zio.{Scope, UIO, ZIO, ZLayer}
import zio.test.*

/** One module. Omission is rebuild. `promote"copy-index"` is the other value. Any other token fails while the build
  * file is loading.
  */
object PromoteSpec extends histos.SuiteClock:
  private val omitted =
    """import histos.*
      |object build extends HistosBuild:
      |  def graph =
      |    val core = module("core", Jvm)(scala"3.9.0")
      |    Projects(core)
      |""".stripMargin

  private val copyIndex =
    """import histos.*
      |object build extends HistosBuild:
      |  def graph =
      |    val core = module("core", Jvm)(scala"3.9.0")
      |    Projects.promote(promote"copy-index")(core)
      |""".stripMargin

  private val unknown =
    """import histos.*
      |object build extends HistosBuild:
      |  def graph =
      |    val core = module("core", Jvm)(scala"3.9.0")
      |    Projects.promote(promote"nope")(core)
      |""".stripMargin

  private val reads =
    test("a one-module graph defaults to rebuild and reads copy-index"):
      ZIO.scoped:
        for
          quiet <- load(omitted)
          copy  <- load(copyIndex)
        yield assertTrue(
          quiet.graph.projects.map(_.name.value) == List("core"),
          quiet.graph.promote == Promote.Rebuild,
          copy.graph.projects.map(_.name.value) == List("core"),
          copy.graph.promote == Promote.CopyIndex,
        )

  private val refused =
    test("an unknown promote token fails at load"):
      ZIO.scoped:
        for loaded <- load(unknown).either
        yield assertTrue:
          loaded match
            case Left(LoadError.CompileFailed(diagnostics)) =>
              val text = diagnostics.map(_.text).mkString("\n")
              text.contains("nope") && text.contains("not a promote strategy")
            case _ => false

  private def load(source: String): ZIO[Loader & Scope, LoadError, LoadedBuild] =
    for
      jar <- ApiJar.locate
      dir <- withRepo(source)
      out <- ZIO.serviceWithZIO[Loader](_.load(dir, jar))
    yield out

  private def withRepo(source: String): ZIO[Scope, LoadError, Path] =
    for
      dir <- ZIO
        .attempt(Files.createTempDirectory("histos-promote"))
        .mapError(_ => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
      _ <- ZIO.addFinalizer(deleteTree(dir))
      _ <- ZIO
        .attempt(Files.writeString(dir.resolve("histos.scala"), source))
        .mapError(_ => LoadError.Io(IoFailure(Path.of("histos.scala"), IoFault.Write)))
    yield dir

  private def deleteTree(root: Path): UIO[Unit] =
    Workspace.delete(root).ignore

  private val store: ZLayer[Any, Nothing, ContentStore] =
    ZLayer.scoped:
      ZIO
        .acquireRelease(
          ZIO.attempt(Files.createTempDirectory("histos-promote-cache")).orDie
        )(deleteTree)
        .map(ContentStore.in)

  def spec =
    suite("promote")(reads, refused).provide(
      Loader.live,
      IdleResolve.layer,
      store,
    ) @@ TestAspect.withLiveSystem
end PromoteSpec
