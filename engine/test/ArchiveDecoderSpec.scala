package histos.archive

import histos.archive.*
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, Path}

import zio.{Chunk, IO, Ref, ZIO, durationInt}
import zio.stream.ZStream
import zio.test.*

object ArchiveDecoderSpec extends ZIOSpecDefault:
  private def stored(path: Path, bytes: Chunk[Byte]): IO[IoFailure, Unit] =
    ZIO
      .attemptBlocking(Files.write(path, bytes.toArray))
      .mapError(_ => IoFailure(path, IoFault.Write))
      .unit

  def spec = suite("Commons archive decoder")(
    test("unread bodies fail before the next entry can be visited"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-decoder")
          bytes <- ArchiveFixture.tarGz(
            List(
              ArchiveFixture.Entry("first", Chunk(1.toByte)),
              ArchiveFixture.Entry("second", Chunk(2.toByte))
            )
          )
          file = root.resolve("archive")
          _ <- stored(file, bytes)
          visited <- Ref.make(List.empty[String])
          result <- CommonsArchiveDecoder()
            .fold(file, ArchiveBudget.default, ())((_, entry) =>
              visited.update(_ :+ entry.name)
            )
            .either
          names <- visited.get
        yield assertTrue(
          result == Left(ExtractFailure.UnconsumedEntry("first")),
          names == List("first")
        )
    ,
    test(
      "escaped bodies fail with a closed-entry error rather than reading another entry"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-decoder")
          bytes <- ArchiveFixture.tarGz(
            List(ArchiveFixture.Entry("first", Chunk(1.toByte)))
          )
          file = root.resolve("archive")
          _ <- stored(file, bytes)
          escaped <- Ref.make(Option.empty[ZStream[Any, ExtractFailure, Byte]])
          _ <- CommonsArchiveDecoder().fold(file, ArchiveBudget.default, ())(
            (_, entry) => escaped.set(Some(entry.body)) *> entry.body.runDrain
          )
          body <- escaped.get.someOrFail(ExtractFailure.MissingMetadata)
          result <- body.runCollect.either
        yield assertTrue(result == Left(ExtractFailure.EntryClosed("first")))
    ,
    test("entry bodies cannot be read twice or partially skipped"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-decoder")
          bytes <- ArchiveFixture.zip("tool" -> Chunk(1.toByte, 2.toByte))
          file = root.resolve("archive")
          _ <- stored(file, bytes)
          twice <- CommonsArchiveDecoder()
            .fold(file, ArchiveBudget.default, ())((_, entry) =>
              entry.body.runDrain *> entry.body.runDrain
            )
            .either
          partial <- CommonsArchiveDecoder()
            .fold(file, ArchiveBudget.default, ())((_, entry) =>
              entry.body.take(1).runDrain
            )
            .either
        yield assertTrue(
          twice == Left(ExtractFailure.EntryAlreadyRead("tool")),
          partial == Left(ExtractFailure.UnconsumedEntry("tool"))
        )
    ,
    test(
      "zip, tar.gz, and tar.xz expose entries and stream their exact content"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-decoder")
          payload = Chunk.fromArray(
            "runtime".getBytes(java.nio.charset.StandardCharsets.UTF_8)
          )
          zip <- ArchiveFixture.zip("runtime/bin/tool" -> payload)
          gzip <- ArchiveFixture.tarGz(
            List(ArchiveFixture.Entry("runtime/bin/tool", payload, mode = 493))
          )
          xz <- ArchiveFixture.tarXz(
            List(ArchiveFixture.Entry("runtime/bin/tool", payload, mode = 493))
          )
          actual <- ZIO.foreach(List(zip, gzip, xz).zipWithIndex):
            (bytes, index) =>
              val file = root.resolve(index.toString)
              stored(file, bytes) *> CommonsArchiveDecoder().fold(
                file,
                ArchiveBudget.default,
                Chunk
                  .empty[(String, ArchiveEntryKind, Option[Int], Chunk[Byte])]
              )((values, entry) =>
                entry.body.runCollect.map(body =>
                  values :+ (entry.name, entry.kind, entry.mode, body)
                )
              )
        yield assertTrue(
          actual == List(
            Chunk(("runtime/bin/tool", ArchiveEntryKind.File, None, payload)),
            Chunk(
              ("runtime/bin/tool", ArchiveEntryKind.File, Some(493), payload)
            ),
            Chunk(
              ("runtime/bin/tool", ArchiveEntryKind.File, Some(493), payload)
            )
          )
        )
    ,
    test(
      "truncated zip, gzip, and xz inputs fail with the format, not a string"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-decoder")
          zip <- ArchiveFixture.zip("tool" -> Chunk(1.toByte))
          gzip <- ArchiveFixture.tarGz(
            List(ArchiveFixture.Entry("tool", Chunk(1.toByte)))
          )
          xz <- ArchiveFixture.tarXz(
            List(ArchiveFixture.Entry("tool", Chunk(1.toByte)))
          )
          actual <- ZIO.foreach(List(zip, gzip, xz).zipWithIndex):
            (bytes, index) =>
              val file = root.resolve(index.toString)
              stored(file, bytes.take(12)) *> CommonsArchiveDecoder()
                .fold(file, ArchiveBudget.default, ())((_, entry) =>
                  entry.body.runDrain
                )
                .either
        yield assertTrue(
          actual == List(
            Left(ExtractFailure.Malformed(ArchiveKind.Zip)),
            Left(ExtractFailure.Malformed(ArchiveKind.TarGz)),
            Left(ExtractFailure.Malformed(ArchiveKind.TarXz))
          )
        )
    ,
    test("unknown content and missing files are different errors"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-decoder")
          file = root.resolve("unknown")
          _ <- stored(file, Chunk(1.toByte))
          unknown <- CommonsArchiveDecoder()
            .fold(file, ArchiveBudget.default, ())((_, entry) =>
              entry.body.runDrain
            )
            .either
          absent <- CommonsArchiveDecoder()
            .fold(root.resolve("absent"), ArchiveBudget.default, ())(
              (_, entry) => entry.body.runDrain
            )
            .either
        yield assertTrue(
          unknown == Left(ExtractFailure.UnknownArchive),
          absent == Left(
            ExtractFailure.Io(IoFailure(root.resolve("absent"), IoFault.Read))
          )
        )
    ,
    test("tar FIFO entries are rejected instead of masquerading as files"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-decoder")
          bytes <- ArchiveFixture.tarGz(
            List(ArchiveFixture.Entry("pipe", kind = ArchiveFixture.Kind.Fifo))
          )
          file = root.resolve("archive")
          _ <- stored(file, bytes)
          actual <- CommonsArchiveDecoder()
            .fold(file, ArchiveBudget.default, ())((_, entry) =>
              entry.body.runDrain
            )
            .either
        yield assertTrue(
          actual == Left(ExtractFailure.UnsupportedEntry("pipe"))
        )
  ) @@ TestAspect.timeout(1.minute)
