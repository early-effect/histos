package histos.load

import histos.cache.ContentStore
import histos.resolve.Resolve
import histos.workspace.Workspace
import histos.zinc.CompileSeverity

import java.nio.file.Path

import slayer.stubbed

import zio.{ZIO, ZLayer}
import zio.test.*

/** Diagnostics are the compiler's. A clean file is not a failure, and an unknown name is. */
object LoaderReportLawSpec extends ZIOSpecDefault:
  def spec = suite("build report")(
    test("a clean build reports no problems, and an unknown name is an error"):
      Workspace
        .work("histos-report")
        .flatMap: root =>
          val layer =
            (stubbed[Resolve] ++ ZLayer.succeed(ContentStore.in(root.resolve("store")))) >>> Loader.live
          val run =
            for
              api <- BuildClasspath(codeSource, root.resolve("cache")).catchAll(error => ZIO.dieMessage(error.message))
              _   <- Workspace
                .writeString(root.resolve("histos.scala"), good)
                .catchAll(error => ZIO.dieMessage(error.message))
              clean <- ZIO.serviceWithZIO[Loader](_.reportBuild(root, api))
              _     <- Workspace
                .writeString(root.resolve("histos.scala"), bad)
                .catchAll(error => ZIO.dieMessage(error.message))
              broken <- ZIO.serviceWithZIO[Loader](_.reportBuild(root, api))
            yield assertTrue(
              clean.failed == false,
              clean.problems.isEmpty,
              broken.failed,
              broken.problems
                .exists(problem => problem.severity == CompileSeverity.Error && problem.message.contains("nope")),
            )
          run.provideSomeLayer[zio.Scope](layer)
  )

  private val good =
    """import histos.*
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val reportlaw = module("reportlaw", Jvm)(scala"3.10.0")
      |    Projects(reportlaw)
      |""".stripMargin

  private val bad =
    """import histos.*
      |
      |object build extends HistosBuild:
      |  def graph = nope
      |""".stripMargin

  private def codeSource: Path =
    val location = classOf[histos.HistosBuild].getProtectionDomain.getCodeSource.getLocation
    Path.of(location.toURI)
end LoaderReportLawSpec
