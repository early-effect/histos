package histos.archive

import heddle.client.{Client, ClientError}
import heddle.http.{Request, Response}
import histos.jdk.Sha256
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, Path}
import zio.{IO, Scope, ZIO}
import zio.test.*

object ArchiveLinkPolicySpec extends ZIOSpecDefault:
  private val client = new Client:
    def batched(request: Request): IO[ClientError, Response] =
      ZIO.succeed(Response())
    def streaming(request: Request): ZIO[Scope, ClientError, Response] =
      batched(request)

  def spec = suite("composed archive link policy")(
    test(
      "targets traversing another link are rejected before parent-component cancellation"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-composed-links")
          bytes <- ArchiveFixture.tarGz(
            List(
              ArchiveFixture
                .Entry("d/a/", kind = ArchiveFixture.Kind.Directory),
              ArchiveFixture.Entry(
                "d/a/up",
                kind = ArchiveFixture.Kind.Symbolic,
                link = ".."
              ),
              ArchiveFixture.Entry(
                "escape",
                kind = ArchiveFixture.Kind.Symbolic,
                link = "d/a/up/../.."
              )
            )
          )
          source = root.resolve("source")
          _ <- ZIO
            .attemptBlocking(Files.write(source, bytes.toArray))
            .mapError(_ => IoFailure(source, IoFault.Write))
          actual <- ArchiveStage(client, CommonsArchiveDecoder())
            .stage(
              ArchiveRequest(
                ArchiveSource.Local(source),
                root.resolve("cache"),
                Sha256.of(bytes)
              )
            )(ZIO.succeed(_))
            .either
        yield assertTrue(
          actual == Left(ArchiveStageError.UnsafeLink("escape", "d/a/up/../.."))
        )
  )
