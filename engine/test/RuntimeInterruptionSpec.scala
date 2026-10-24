package histos.jdk

import histos.archive.*
import heddle.client.{Client, ClientError}
import heddle.http.{Request, Response}
import histos.cache.Scratch
import histos.toolchain.RuntimeFiles
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, Path}

import zio.*
import zio.stream.ZStream
import zio.test.*

object RuntimeInterruptionSpec extends ZIOSpecDefault:
  private val unusedClient = new Client:
    def batched(request: Request): IO[ClientError, Response] =
      ZIO.succeed(Response())
    def streaming(request: Request): ZIO[Scope, ClientError, Response] =
      batched(request)

  private def present(path: Path): IO[IoFailure, Boolean] = ZIO
    .attemptBlocking(Files.exists(path))
    .mapError(_ => IoFailure(path, IoFault.Stat))

  private def names(root: Path): IO[IoFailure, List[String]] = RuntimeFiles
    .paths(root, 1)
    .map(
      _.filterNot(_ == root)
        .flatMap(path => Option(path.getFileName).map(_.toString))
        .sorted
    )

  def spec = suite("runtime interruption boundaries")(
    test(
      "a paused local-file copy closes and removes its unique scratch before termination"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-copy-interrupt")
          source = root.resolve("source")
          _ <- ZIO.attemptBlocking(Files.write(source, Array[Byte](1, 2, 3)))
          started <- Promise.make[Nothing, Unit]
          release <- Promise.make[Nothing, Unit]
          created <- Promise.make[Nothing, Path]
          count <- Ref.make(0)
          closed <- Ref.make(false)
          stream = RuntimeFiles
            .bytes(source)
            .rechunk(1)
            .mapZIO(byte =>
              count
                .updateAndGet(_ + 1)
                .flatMap(index =>
                  if index == 2 then
                    started.succeed(()) *> release.await.as(byte)
                  else ZIO.succeed(byte)
                )
            )
          target = root.resolve("archive")
          fiber <- ZIO
            .scoped(
              for
                scratch <- Scratch.file(target, IoFailure(target, IoFault.Temp))
                _ <- created.succeed(scratch)
                _ <- RuntimeFiles.writeHashed(stream, scratch)
              yield ()
            )
            .ensuring(closed.set(true))
            .forkScoped
          _ <- started.await.raceFirst(fiber.join.unit)
          scratch <- created.await
          written <- ZIO.attemptBlocking(Files.size(scratch))
          exit <- fiber.interrupt
          released <- closed.get
          scratchExists <- present(scratch)
          targetExists <- present(target)
        yield assertTrue(
          written == 1L,
          exit.isInterrupted,
          released,
          !scratchExists,
          !targetExists
        )
    ,
    test(
      "ZIP, gzip and XZ decoder cancellation closes entry lifetimes and removes extraction scratch"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-decode-interrupt")
          payload = Chunk.fromArray(Array.fill[Byte](64)(7))
          zip <- ArchiveFixture.zip("tool" -> payload)
          gzip <- ArchiveFixture.tarGz(
            List(ArchiveFixture.Entry("tool", payload))
          )
          xz <- ArchiveFixture.tarXz(
            List(ArchiveFixture.Entry("tool", payload))
          )
          actual <- ZIO.foreach(List(zip, gzip, xz).zipWithIndex):
            (bytes, index) =>
              for
                source = root.resolve(index.toString + "-source")
                _ <- ZIO.attemptBlocking(Files.write(source, bytes.toArray))
                started <- Promise.make[Nothing, Unit]
                release <- Promise.make[Nothing, Unit]
                closed <- Ref.make(false)
                escaped <- Ref.make(
                  Option.empty[ZStream[Any, ExtractFailure, Byte]]
                )
                count <- Ref.make(0)
                delegate = CommonsArchiveDecoder()
                decoder = new ArchiveDecoder:
                  def fold[A, E](
                      archive: Path,
                      budget: ArchiveBudget,
                      initial: A
                  )(
                      consume: (A, DecodedEntry) => IO[E, A]
                  ): IO[ExtractFailure | E, A] =
                    delegate
                      .fold(archive, budget, initial)((state, entry) =>
                        escaped.set(Some(entry.body)) *> consume(
                          state,
                          entry.copy(body =
                            entry.body
                              .rechunk(1)
                              .mapZIO(byte =>
                                count
                                  .updateAndGet(_ + 1)
                                  .flatMap(n =>
                                    if n == 2 then
                                      started.succeed(()) *> release.await
                                        .as(byte)
                                    else ZIO.succeed(byte)
                                  )
                              )
                          )
                        )
                      )
                      .ensuring(closed.set(true))
                request = ArchiveRequest(
                  ArchiveSource.Local(source),
                  root.resolve(index.toString),
                  Sha256.of(bytes)
                )
                fiber <- ArchiveStage(unusedClient, decoder)
                  .stage(request)(ZIO.succeed(_))
                  .forkScoped
                _ <- started.await.raceFirst(fiber.join.unit)
                exit <- fiber.interrupt
                released <- closed.get
                body <- escaped.get.someOrFail(ExtractFailure.MissingMetadata)
                late <- body.runDrain.either
                left <- names(request.root)
              yield (exit.isInterrupted, released, late, left)
        yield assertTrue(
          actual == List.fill(3)(
            (
              true,
              true,
              Left(ExtractFailure.EntryClosed("tool")),
              List("archive")
            )
          )
        )
    ,
    test(
      "cancellation after atomic publication leaves a complete verified installation and no scratch"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-published-interrupt")
          payload = Chunk(7.toByte, 8.toByte)
          bytes <- ArchiveFixture.zip("tool" -> payload)
          source = root.resolve("source")
          _ <- ZIO.attemptBlocking(Files.write(source, bytes.toArray))
          ready <- Promise.make[Nothing, Unit]
          count <- Ref.make(0)
          request = ArchiveRequest(
            ArchiveSource.Local(source),
            root.resolve("cache"),
            Sha256.of(bytes)
          )
          fiber <- ArchiveStage(unusedClient, CommonsArchiveDecoder())
            .stage(request)(path =>
              count
                .updateAndGet(_ + 1)
                .flatMap(n =>
                  if n == 2 then ready.succeed(()) *> ZIO.never
                  else ZIO.succeed(path)
                )
            )
            .forkScoped
          _ <- ready.await.raceFirst(fiber.join.unit)
          installed = request.root.resolve("unpack")
          content <- RuntimeFiles.bytes(installed.resolve("tool")).runCollect
          expected <- Workspace
            .readString(installed.resolve(ArchiveStage.ManifestName))
            .flatMap(text => ZIO.fromEither(Sha256.from(text)))
          actual <- RuntimeFiles.content(
            installed,
            Set(installed.resolve(ArchiveStage.ManifestName))
          )
          exit <- fiber.interrupt
          left <- names(request.root)
        yield assertTrue(
          exit.isInterrupted,
          content == payload,
          expected == actual,
          left == List("archive", "unpack")
        )
    ,
    test(
      "cancellation after decoded validation but before publication never exposes an unpack tree"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-stage-interrupt")
          payload = Chunk(7.toByte, 8.toByte)
          bytes <- ArchiveFixture.zip("tool" -> payload)
          source = root.resolve("source")
          _ <- ZIO.attemptBlocking(Files.write(source, bytes.toArray))
          ready <- Promise.make[Nothing, Unit]
          release <- Promise.make[Nothing, Unit]
          observed <- Ref.make(Chunk.empty[Byte])
          request = ArchiveRequest(
            ArchiveSource.Local(source),
            root.resolve("cache"),
            Sha256.of(bytes)
          )
          fiber <- ArchiveStage(unusedClient, CommonsArchiveDecoder())
            .stage(request)(path =>
              RuntimeFiles
                .bytes(path.resolve("tool"))
                .runCollect
                .flatMap(content =>
                  observed.set(content) *> ready.succeed(()) *> release.await
                    .as(path)
                )
            )
            .forkScoped
          _ <- ready.await.raceFirst(fiber.join.unit)
          before <- present(request.root.resolve("unpack"))
          exit <- fiber.interrupt
          content <- observed.get
          after <- present(request.root.resolve("unpack"))
          left <- names(request.root)
          stored <- RuntimeFiles.sha256(request.root.resolve("archive"))
        yield assertTrue(
          content == payload,
          exit.isInterrupted,
          !before,
          !after,
          left == List("archive"),
          stored == request.sha256
        )
  ) @@ TestAspect.timeout(1.minute)
