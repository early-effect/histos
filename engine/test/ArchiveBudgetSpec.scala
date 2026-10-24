package histos.archive

import histos.jdk.Sha256
import histos.archive.*
import heddle.client.{Client, ClientError}
import heddle.http.{Body, Request, Response}
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

object ArchiveBudgetSpec extends ZIOSpecDefault:
  private def budget(
      limit: ArchiveLimit
  ): IO[ArchiveBudgetError, ArchiveBudget] =
    val defaults = ArchiveBudget.default
    def maximum(field: ArchiveLimit): Long =
      if field == limit then 1L else defaults.maximum(field)
    ZIO.fromEither(
      ArchiveBudget.from(
        ArchiveBudget.Settings(
          maximum(ArchiveLimit.DownloadBytes),
          maximum(ArchiveLimit.DecompressedBytes),
          maximum(ArchiveLimit.ExtractedBytes),
          maximum(ArchiveLimit.EntryBytes),
          if limit == ArchiveLimit.Entries then 1 else defaults.entries,
          maximum(ArchiveLimit.MetadataBytes),
          maximum(ArchiveLimit.DecoderMemoryKiB).toInt
        )
      )
    )

  private def client(bytes: Chunk[Byte]): Client = new Client:
    def batched(request: Request): IO[ClientError, Response] =
      ZIO.succeed(Response(body = Body.fromBytes(bytes)))
    def streaming(request: Request): ZIO[Scope, ClientError, Response] =
      batched(request)

  def spec = suite("archive budgets")(
    test(
      "metadata bytes and XZ decoder working memory are independent typed limits"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-memory-budget")
          gzip <- ArchiveFixture.tarGz(
            List(ArchiveFixture.Entry("a" * 200, Chunk(1.toByte)))
          )
          xz <- ArchiveFixture.tarXz(
            List(ArchiveFixture.Entry("tool", Chunk(1.toByte)))
          )
          metadata <- budget(ArchiveLimit.MetadataBytes)
          memory <- budget(ArchiveLimit.DecoderMemoryKiB)
          source = root.resolve("source")
          _ <- ZIO.attemptBlocking(Files.write(source, gzip.toArray))
          metadataResult <- CommonsArchiveDecoder()
            .fold(source, metadata, ())((_, entry) => entry.body.runDrain)
            .either
          _ <- ZIO.attemptBlocking(Files.write(source, xz.toArray))
          memoryResult <- CommonsArchiveDecoder()
            .fold(source, memory, ())((_, entry) => entry.body.runDrain)
            .either
        yield assertTrue(
          metadataResult == Left(
            ExtractFailure.Budget(
              ArchiveBudgetError.Exceeded(ArchiveLimit.MetadataBytes, 1L)
            )
          ),
          memoryResult == Left(
            ExtractFailure.Budget(
              ArchiveBudgetError.Exceeded(ArchiveLimit.DecoderMemoryKiB, 1L)
            )
          )
        )
    ,
    test("invalid numeric budgets retain their field"):
      assertTrue(
        ArchiveBudget.from(
          ArchiveBudget.default.settings.copy(downloadBytes = 0L)
        ) == Left(
          ArchiveBudgetError.Invalid(ArchiveLimit.DownloadBytes, 0L)
        )
      )
    ,
    test("download size is enforced independently of the Client body framing"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-budget")
          bytes <- ArchiveFixture.zip("tool" -> Chunk(1.toByte))
          limits <- budget(ArchiveLimit.DownloadBytes)
          url <- ZIO.fromEither(
            HttpsUrl.from("https://example.test/archive.zip")
          )
          result <- ArchiveStage(client(bytes), CommonsArchiveDecoder(), limits)
            .stage(
              ArchiveRequest(
                ArchiveSource.Remote(url),
                root.resolve("cache"),
                Sha256.of(bytes)
              )
            )(ZIO.succeed(_))
            .either
        yield assertTrue(
          result == Left(
            ArchiveStageError.Budget(
              ArchiveBudgetError.Exceeded(ArchiveLimit.DownloadBytes, 1L)
            )
          )
        )
    ,
    test("per-entry and aggregate extracted bytes have independent limits"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-budget")
          bytes <- ArchiveFixture.tarGz(
            List(
              ArchiveFixture.Entry("first", Chunk(1.toByte, 2.toByte)),
              ArchiveFixture.Entry("second", Chunk(3.toByte))
            )
          )
          source = root.resolve("source")
          _ <- ZIO
            .attemptBlocking(Files.write(source, bytes.toArray))
            .mapError(_ => IoFailure(source, IoFault.Write))
          actual <- ZIO.foreach(
            List(ArchiveLimit.EntryBytes, ArchiveLimit.ExtractedBytes)
          ): limit =>
            for
              limits <- budget(limit)
              result <- ArchiveStage(
                client(bytes),
                CommonsArchiveDecoder(),
                limits
              ).stage(
                ArchiveRequest(
                  ArchiveSource.Local(source),
                  root.resolve(limit.toString),
                  Sha256.of(bytes)
                )
              )(ZIO.succeed(_))
                .either
            yield result
        yield assertTrue(
          actual == List(
            Left(
              ArchiveStageError.Budget(
                ArchiveBudgetError.Exceeded(ArchiveLimit.EntryBytes, 1L)
              )
            ),
            Left(
              ArchiveStageError.Budget(
                ArchiveBudgetError.Exceeded(ArchiveLimit.ExtractedBytes, 1L)
              )
            )
          )
        )
    ,
    test("entry count is bounded even when every body is empty"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-budget")
          bytes <- ArchiveFixture.zip(
            "first" -> Chunk.empty,
            "second" -> Chunk.empty
          )
          source = root.resolve("source")
          _ <- ZIO.attemptBlocking(Files.write(source, bytes.toArray))
          limits <- budget(ArchiveLimit.Entries)
          result <- ArchiveStage(client(bytes), CommonsArchiveDecoder(), limits)
            .stage(
              ArchiveRequest(
                ArchiveSource.Local(source),
                root.resolve("cache"),
                Sha256.of(bytes)
              )
            )(ZIO.succeed(_))
            .either
        yield assertTrue(
          result == Left(
            ArchiveStageError.Extract(
              root.resolve("cache/archive"),
              ExtractFailure.Budget(
                ArchiveBudgetError.Exceeded(ArchiveLimit.Entries, 1L)
              )
            )
          )
        )
    ,
    test(
      "decompressed tar metadata and headers are bounded before entry delivery"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-budget")
          bytes <- ArchiveFixture.tarGz(
            List(ArchiveFixture.Entry("tool", Chunk(1.toByte)))
          )
          source = root.resolve("source")
          _ <- ZIO.attemptBlocking(Files.write(source, bytes.toArray))
          limits <- budget(ArchiveLimit.DecompressedBytes)
          cache = root.resolve("cache")
          result <- ArchiveStage(client(bytes), CommonsArchiveDecoder(), limits)
            .stage(
              ArchiveRequest(
                ArchiveSource.Local(source),
                cache,
                Sha256.of(bytes)
              )
            )(ZIO.succeed(_))
            .either
        yield assertTrue(
          result == Left(
            ArchiveStageError.Extract(
              cache.resolve("archive"),
              ExtractFailure.Budget(
                ArchiveBudgetError.Exceeded(ArchiveLimit.DecompressedBytes, 1L)
              )
            )
          )
        )
  ) @@ TestAspect.timeout(1.minute)
