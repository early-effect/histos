package histos.zinc

import histos.*
import histos.jdk.{JdkVersion, VersionError}
import histos.plugin.SourceSet
import histos.report.WireJson.given
import histos.suite.HistosSpec
import histos.workspace.{IoFailure, Workspace}

import java.nio.file.Path

import zio.{ZIO, durationInt}
import zio.json.*
import zio.test.*

object CellCapturePathSpec extends HistosSpec:
  private val stage = CompilationStage(Cell(ProjectName("capture"), ScalaVersion("3.10.1"), Jvm), SourceSet.Test)
  private val tests = suite("capture path safety")(
    test("readable malformed filenames remain path failures, including literal Unix backslashes") {
      withRequest: request =>
        ZIO
          .foreach(List("input:bad.txt", "input\\bad.txt", "input\nbad.txt")): name =>
            val file     = request.repo.resolve(name)
            val expected = failure(CompileError.InvalidPath(file))
            for
              _      <- Workspace.writeString(file, "readable")
              result <- CellCapture
                .record(request.copy(files = StageFiles(Nil, List(ResourceName("resource.txt") -> file), Nil)))
                .either
            yield assertTrue(
              result == Left(expected),
              expected.toJson.fromJson[GraphCompileError] == Right(expected),
              !expected.message.contains("could not read"),
            )
          .map(_.foldLeft(assertTrue(true))(_ && _))
    },
    test("a checkout prefix sibling is outside, not an unreadable input") {
      withRequest: request =>
        val outside = Path.of(request.repo.toString + "-sibling").resolve("Input.scala")
        val files   = StageFiles(List(PackagedSource("Input.scala", outside)), Nil, Nil)
        CellCapture
          .record(request.copy(files = files))
          .either
          .map: result =>
            assertTrue(result == Left(failure(CompileError.Outside(outside, request.repo))))
    },
    test("malformed sealed entries retain a path failure instead of an I/O failure") {
      withRequest: request =>
        val file = request.product.classes.resolve("output\\bad.txt")
        for
          _      <- Workspace.writeString(file, "readable")
          result <- CellCapture.record(request).either
        yield assertTrue(result == Left(failure(CompileError.InvalidPath(file))))
    },
  )

  def spec = tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(1.minute)

  private def failure(error: CompileError): GraphCompileError =
    GraphCompileError.Failed(stage.cell.module, stage.cell.version, stage.cell.platform, error)

  private def withRequest[E, A](use: CaptureRequest => ZIO[Any, E, A]): ZIO[Any, E | IoFailure | VersionError, A] =
    ZIO.scoped:
      for
        repo    <- Workspace.work("histos-capture-path")
        classes <- Workspace.createDirectories(repo.resolve("classes"))
        tool = repo.resolve("tool.jar")
        _     <- Workspace.writeString(tool, "capture tool bytes")
        jdk   <- ZIO.fromEither(JdkVersion.from("25"))
        value <- use(
          CaptureRequest(
            repo,
            stage,
            StageFiles(Nil, Nil, Nil),
            CaptureProduct(classes, Nil),
            CaptureTools(jdk, List(tool), List(tool), tool),
          )
        )
      yield value
end CellCapturePathSpec
