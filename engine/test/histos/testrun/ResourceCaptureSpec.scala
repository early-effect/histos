package histos.testrun

import histos.*
import histos.build.BuildNode
import histos.plugin.{Digests, SourceKind, SourceProvenance, SourceSet}
import histos.suite.HistosSpec
import histos.workspace.Workspace
import histos.zinc.CompilationCapture

import java.nio.charset.StandardCharsets.UTF_8

import zio.{ZIO, durationInt}
import zio.test.*

object ResourceCaptureSpec extends HistosSpec:
  private val names = List("vendor/escape-string-regexp@5.0.0.js", "notes/front page.txt", "données/日本語.txt")
  private val body  = "vendored resource\n"
  private val tests = suite("resource file capture")(
    test("Test compilation captures and seals vendored, space and Unicode resource names") {
      val version = TestExecutionFixtures.version
      val stage   = CompilationStage(Cell(ProjectName("resources"), version, Jvm), SourceSet.Test)
      ZIO
        .fromEither(Projects(module("resources", Jvm)(version)))
        .flatMap: graph =>
          TestExecutionFixtures.withGraph(graph): fixture =>
            for
              _ <- Workspace
                .writeString(fixture.repo.resolve("resources/test/demo/Check.scala"), "package demo\nobject Check\n")
              _ <- ZIO.foreachDiscard(names)(name =>
                Workspace.writeString(fixture.repo.resolve(s"resources/src/test/resources/$name"), body)
              )
              products <- fixture.ensure(List(BuildNode.Compile(stage)))
              product  <- ZIO.fromEither(products.require(stage))
              copied   <- ZIO.foreach(names)(name => Workspace.readString(product.classes.resolve(name)))
            yield
              val captured = product.compiled.capture match
                case CompilationCapture.Unrecorded         => None
                case CompilationCapture.Recorded(snapshot) => Some(snapshot)
              val hash = Digests.content(body.getBytes(UTF_8))
              assertTrue(
                product.compiled.classFiles > 0,
                copied == names.map(_ => body),
                captured.exists(
                  _.resources.map(_.path.value).toSet == names.map(name => s"resources/src/test/resources/$name").toSet
                ),
                captured.exists(
                  _.resources.forall(input =>
                    input.kind == SourceKind.Resource && input.set == SourceSet.Test &&
                      input.provenance == SourceProvenance.Unmanaged && input.digest == hash && input.length == body
                        .getBytes(UTF_8)
                        .length
                        .toLong
                  )
                ),
                captured.exists(snapshot =>
                  names.forall(name =>
                    snapshot.outputs.entries.exists(entry => entry.path.value == name && entry.digest == hash)
                  )
                ),
              )
    }
  )

  def spec =
    (tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.minutes)).provideShared(histos.zinc.Compilers.owned)
end ResourceCaptureSpec
