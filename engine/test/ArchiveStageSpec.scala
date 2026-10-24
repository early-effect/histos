package histos.archive

import histos.jdk.Sha256
import histos.archive.*
import heddle.client.{Authority, Client, ClientError}
import heddle.http.{Body, Request, Response, Status}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.toolchain.RuntimeFiles

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermission

import scala.jdk.CollectionConverters.*

import zio.*
import zio.stream.ZStream
import zio.test.*

object ArchiveStageSpec extends ZIOSpecDefault:
  private def client(response: ZIO[Scope, ClientError, Response]): Client =
    new Client:
      def batched(request: Request): IO[ClientError, Response] =
        ZIO.scoped(response)
      def streaming(request: Request): ZIO[Scope, ClientError, Response] =
        response

  private def request(
      root: Path,
      source: ArchiveSource,
      bytes: Chunk[Byte]
  ): ArchiveRequest =
    ArchiveRequest(
      source,
      root.resolve("stage"),
      Sha256.of(bytes)
    )

  private def writeBytes(path: Path, bytes: Chunk[Byte]): IO[IoFailure, Unit] =
    ZIO
      .attemptBlocking(Files.write(path, bytes.toArray))
      .mapError(_ => IoFailure(path, IoFault.Write))
      .unit

  private def names(root: Path): IO[IoFailure, List[String]] =
    ZIO.scoped:
      ZIO
        .acquireRelease(
          ZIO
            .attemptBlocking(Files.list(root))
            .mapError(_ => IoFailure(root, IoFault.Walk))
        )(stream => ZIO.attemptBlocking(stream.close()).ignore)
        .flatMap(stream =>
          ZIO
            .attemptBlocking(
              stream.iterator.asScala.toList
                .flatMap(path => Option(path.getFileName).map(_.toString))
                .sorted
            )
            .mapError(_ => IoFailure(root, IoFault.Walk))
        )

  def spec = suite("shared archive stage")(
    test(
      "an injected decoder determines entries while Histos retains extraction policy"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          bytes = Chunk(42.toByte)
          source = root.resolve("source")
          _ <- writeBytes(source, bytes)
          closed <- Ref.make(false)
          decoder = new ArchiveDecoder:
            def fold[A, E](archive: Path, budget: ArchiveBudget, initial: A)(
                consume: (A, DecodedEntry) => IO[E, A]
            ) = ZIO.scoped(
              ZIO
                .acquireRelease(
                  ZIO.succeed(
                    DecodedEntry(
                      "tool/bin/tool",
                      ArchiveEntryKind.File,
                      Some(493),
                      ZStream.fromChunk(bytes)
                    )
                  )
                )(_ => closed.set(true))
                .flatMap(entry => consume(initial, entry))
            )
          req = request(root, ArchiveSource.Local(source), bytes)
          home <- ArchiveStage(
            client(
              ZIO.fail(
                ClientError.ConnectTimeout(Authority("unused.test", 443))
              )
            ),
            decoder
          ).stage(req)(ZIO.succeed(_))
          content <- RuntimeFiles
            .bytes(home.resolve("tool/bin/tool"))
            .runCollect
          released <- closed.get
        yield assertTrue(
          content == bytes,
          released,
          home == req.root.resolve("unpack")
        )
    ,
    test("decoder failure closes its scope and never installs a partial tree"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          source = root.resolve("source")
          bytes = Chunk(42.toByte)
          _ <- writeBytes(source, bytes)
          closed <- Ref.make(false)
          failure = ExtractFailure.UnsupportedData("tool")
          decoder = new ArchiveDecoder:
            def fold[A, E](archive: Path, budget: ArchiveBudget, initial: A)(
                consume: (A, DecodedEntry) => IO[E, A]
            ) = ZIO.scoped(
              ZIO
                .acquireRelease(
                  ZIO.succeed(
                    DecodedEntry(
                      "tool",
                      ArchiveEntryKind.File,
                      None,
                      ZStream.fromChunk(bytes) ++ ZStream.fail(failure)
                    )
                  )
                )(_ => closed.set(true))
                .flatMap(entry => consume(initial, entry))
            )
          req = request(root, ArchiveSource.Local(source), bytes)
          result <- ArchiveStage(client(ZIO.succeed(Response())), decoder)
            .stage(req)(ZIO.succeed(_))
            .either
          released <- closed.get
          left <- names(req.root)
        yield assertTrue(
          result == Left(
            ArchiveStageError.Extract(req.root.resolve("archive"), failure)
          ),
          released,
          left == List("archive")
        )
    ,
    test(
      "decoder cancellation closes its scope and removes the unique extraction scratch"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          bytes = Chunk(42.toByte)
          source = root.resolve("source")
          _ <- writeBytes(source, bytes)
          started <- Promise.make[Nothing, Unit]
          closed <- Ref.make(false)
          decoder = new ArchiveDecoder:
            def fold[A, E](archive: Path, budget: ArchiveBudget, initial: A)(
                consume: (A, DecodedEntry) => IO[E, A]
            ) = ZIO.scoped(
              ZIO
                .acquireRelease(
                  ZIO.succeed(
                    DecodedEntry(
                      "tool",
                      ArchiveEntryKind.File,
                      None,
                      ZStream
                        .fromZIO(started.succeed(()))
                        .drain ++ ZStream.never
                    )
                  )
                )(_ => closed.set(true))
                .flatMap(entry => consume(initial, entry))
            )
          req = request(root, ArchiveSource.Local(source), bytes)
          fiber <- ArchiveStage(client(ZIO.succeed(Response())), decoder)
            .stage(req)(ZIO.succeed(_))
            .forkScoped
          _ <- started.await.raceFirst(fiber.join.unit)
          visible <- ZIO.attemptBlocking(
            Files.exists(req.root.resolve("unpack"))
          )
          exit <- fiber.interrupt
          released <- closed.get
          left <- names(req.root)
        yield assertTrue(
          !visible,
          exit.isInterrupted,
          released,
          left == List("archive")
        )
    ,
    test(
      "validation is completed before the atomic installation becomes visible"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          bytes <- ArchiveFixture.zip("tool" -> Chunk(1.toByte))
          source = root.resolve("source")
          _ <- writeBytes(source, bytes)
          started <- Promise.make[Nothing, Unit]
          release <- Promise.make[Nothing, Unit]
          req = request(root, ArchiveSource.Local(source), bytes)
          stage = ArchiveStage(
            client(ZIO.succeed(Response())),
            CommonsArchiveDecoder()
          )
          fiber <- stage
            .stage(req)(path => started.succeed(()) *> release.await.as(path))
            .forkScoped
          _ <- started.await.raceFirst(fiber.join.unit)
          before <- ZIO.attemptBlocking(
            Files.exists(req.root.resolve("unpack"))
          )
          _ <- release.succeed(())
          result <- fiber.join
          installed <- RuntimeFiles.bytes(result.resolve("tool")).runCollect
        yield assertTrue(
          !before,
          result == req.root.resolve("unpack"),
          installed == Chunk(1.toByte)
        )
    ,
    test(
      "warm installed content corruption is detected even with an intact archive"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          bytes <- ArchiveFixture.zip("tool" -> Chunk(1.toByte))
          source = root.resolve("source")
          _ <- writeBytes(source, bytes)
          req = request(root, ArchiveSource.Local(source), bytes)
          stage = ArchiveStage(
            client(ZIO.succeed(Response())),
            CommonsArchiveDecoder()
          )
          home <- stage.stage(req)(ZIO.succeed(_))
          manifest = home.resolve(ArchiveStage.ManifestName)
          expected <- RuntimeFiles.content(home, Set(manifest))
          _ <- writeBytes(home.resolve("tool"), Chunk(9.toByte))
          actual <- RuntimeFiles.content(home, Set(manifest))
          result <- stage.stage(req)(ZIO.succeed(_)).either
          left <- names(req.root)
        yield assertTrue(
          result == Left(
            ArchiveStageError.CorruptInstallation(expected, actual)
          ),
          expected != actual,
          left == List("archive", "unpack")
        )
    ,
    test("duplicate entries and file-directory conflicts are rejected"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          fixtures = List(
            List(ArchiveFixture.Entry("tool"), ArchiveFixture.Entry("tool")),
            List(
              ArchiveFixture.Entry("tool"),
              ArchiveFixture.Entry("tool/child")
            ),
            List(
              ArchiveFixture.Entry("tool/child"),
              ArchiveFixture.Entry("tool")
            ),
            List(ArchiveFixture.Entry(ArchiveStage.ManifestName))
          )
          actual <- ZIO.foreach(fixtures.zipWithIndex): (entries, index) =>
            for
              bytes <- ArchiveFixture.tarGz(entries)
              source = root.resolve(index.toString + "-source")
              _ <- writeBytes(source, bytes)
              req = request(
                root.resolve(index.toString),
                ArchiveSource.Local(source),
                bytes
              )
              result <- ArchiveStage(
                client(ZIO.succeed(Response())),
                CommonsArchiveDecoder()
              ).stage(req)(ZIO.succeed(_)).either
            yield result
        yield assertTrue(
          actual == List(
            Left(ArchiveStageError.UnsafePath("tool")),
            Left(ArchiveStageError.UnsafePath("tool/child")),
            Left(ArchiveStageError.UnsafePath("tool")),
            Left(ArchiveStageError.UnsafePath(ArchiveStage.ManifestName))
          )
        )
    ,
    test(
      "zip Unix links, dangling tar links, and link chains cannot bypass containment"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          zip <- ArchiveFixture.zipEntries(
            List(
              ArchiveFixture.Entry(
                "tool/link",
                kind = ArchiveFixture.Kind.Symbolic,
                link = "../../outside"
              )
            )
          )
          dangling <- ArchiveFixture.tarGz(
            List(
              ArchiveFixture.Entry(
                "tool/link",
                kind = ArchiveFixture.Kind.Symbolic,
                link = "missing"
              )
            )
          )
          chain <- ArchiveFixture.tarGz(
            List(
              ArchiveFixture.Entry("tool/data"),
              ArchiveFixture.Entry(
                "tool/first",
                kind = ArchiveFixture.Kind.Symbolic,
                link = "data"
              ),
              ArchiveFixture.Entry(
                "tool/second",
                kind = ArchiveFixture.Kind.Symbolic,
                link = "first"
              )
            )
          )
          actual <- ZIO.foreach(List(zip, dangling, chain).zipWithIndex):
            (bytes, index) =>
              for
                source = root.resolve(index.toString + "-source")
                _ <- writeBytes(source, bytes)
                req = request(
                  root.resolve(index.toString),
                  ArchiveSource.Local(source),
                  bytes
                )
                result <- ArchiveStage(
                  client(ZIO.succeed(Response())),
                  CommonsArchiveDecoder()
                ).stage(req)(ZIO.succeed(_)).either
              yield result
        yield assertTrue(
          actual == List(
            Left(ArchiveStageError.UnsafeLink("tool/link", "../../outside")),
            Left(ArchiveStageError.UnsafeLink("tool/link", "missing")),
            Left(ArchiveStageError.UnsafeLink("tool/second", "first"))
          )
        )
    ,
    test(
      "HTTPS uses the injected streaming Client with the exact locked target"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          bytes <- ArchiveFixture.zip("tool" -> Chunk(1.toByte))
          url <- ZIO.fromEither(
            HttpsUrl.from("https://example.test/tool.zip?revision=locked")
          )
          seen <- Ref.make(List.empty[String])
          closed <- Ref.make(false)
          transport = new Client:
            def batched(request: Request): IO[ClientError, Response] = ZIO.fail(
              ClientError.ConnectTimeout(Authority("batched.invalid", 443))
            )
            def streaming(request: Request): ZIO[Scope, ClientError, Response] =
              seen.update(_ :+ request.url.render) *> ZIO.acquireRelease(
                ZIO.succeed(
                  Response(body =
                    Body.stream(ZStream.fromChunk(bytes).rechunk(3))
                  )
                )
              )(_ => closed.set(true))
          req = request(root, ArchiveSource.Remote(url), bytes)
          home <- ArchiveStage(transport, CommonsArchiveDecoder()).stage(req)(
            ZIO.succeed(_)
          )
          requests <- seen.get
          released <- closed.get
          body <- RuntimeFiles.bytes(home.resolve("tool")).runCollect
        yield assertTrue(
          requests == List(url.value),
          released,
          body == Chunk(1.toByte)
        )
    ,
    test(
      "HTTPS streams a checksum-verified zip and a warm archive is verified again"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          bytes <- ArchiveFixture.zip(
            "runtime/bin/tool" -> Chunk.fromArray("tool".getBytes)
          )
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/tool.zip"))
          calls <- Ref.make(0)
          transport = client(
            calls
              .update(_ + 1)
              .as(
                Response(body =
                  Body.stream(ZStream.fromChunk(bytes).rechunk(7))
                )
              )
          )
          stage = ArchiveStage(transport, CommonsArchiveDecoder())
          req = request(root, ArchiveSource.Remote(url), bytes)
          first <- stage.stage(req)(ZIO.succeed(_))
          second <- stage.stage(req)(ZIO.succeed(_))
          count <- calls.get
          stored <- ZIO.attemptBlocking(
            Files.readAllBytes(first.resolve("runtime/bin/tool"))
          )
          _ <- ZIO.attemptBlocking(
            Files.write(req.root.resolve("archive"), Array[Byte](1))
          )
          tampered <- stage.stage(req)(ZIO.succeed(_)).either
        yield assertTrue(
          first == second,
          count == 1,
          Chunk.fromArray(stored) == Chunk.fromArray("tool".getBytes),
          tampered == Left(
            ArchiveStageError.Checksum(req.sha256, Sha256.of(Chunk(1.toByte)))
          )
        )
    ,
    test("HTTPS checksum mismatch keeps neither archive nor scratch"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/tool.zip"))
          req = request(root, ArchiveSource.Remote(url), Chunk(9.toByte))
          actual = Chunk(1.toByte, 2.toByte)
          result <- ArchiveStage(
            client(ZIO.succeed(Response(body = Body.fromBytes(actual)))),
            CommonsArchiveDecoder()
          ).stage(req)(ZIO.succeed(_)).either
          left <- names(req.root)
        yield assertTrue(
          result == Left(
            ArchiveStageError.Checksum(req.sha256, Sha256.of(actual))
          ),
          left.isEmpty
        )
    ,
    test("HTTP status is a typed download failure"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/tool.zip"))
          req = request(root, ArchiveSource.Remote(url), Chunk.empty)
          result <- ArchiveStage(
            client(ZIO.succeed(Response(Status.NotFound))),
            CommonsArchiveDecoder()
          )
            .stage(req)(ZIO.succeed(_))
            .either
        yield assertTrue(
          result == Left(
            ArchiveStageError.Download(
              url,
              DownloadFailure.Status(Status.NotFound)
            )
          )
        )
    ,
    test("HTTPS transport errors stay typed"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/tool.zip"))
          failure = ClientError.ConnectTimeout(Authority("example.test", 443))
          req = request(root, ArchiveSource.Remote(url), Chunk.empty)
          result <- ArchiveStage(
            client(ZIO.fail(failure)),
            CommonsArchiveDecoder()
          ).stage(req)(ZIO.succeed(_)).either
        yield assertTrue(
          result == Left(
            ArchiveStageError.Download(url, DownloadFailure.Transport(failure))
          )
        )
    ,
    test("HTTPS redirects retain the pin and reject an HTTP downgrade"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          bytes <- ArchiveFixture.zip("tool" -> Chunk(1.toByte))
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/start"))
          transport = new Client:
            def batched(req: Request): IO[ClientError, Response] =
              ZIO.succeed(if req.url.render.endsWith("/start") then
                Response.redirect("/archive")
              else Response(body = Body.fromBytes(bytes)))
            def streaming(req: Request): ZIO[Scope, ClientError, Response] =
              batched(req)
          req = request(root, ArchiveSource.Remote(url), bytes)
          home <- ArchiveStage(transport, CommonsArchiveDecoder()).stage(req)(
            ZIO.succeed(_)
          )
          downgrade <- ArchiveStage(
            client(
              ZIO.succeed(Response.redirect("http://example.test/archive"))
            ),
            CommonsArchiveDecoder()
          ).stage(
            request(root.resolve("downgrade"), ArchiveSource.Remote(url), bytes)
          )(ZIO.succeed(_))
            .either
        yield assertTrue(
          home == req.root.resolve("unpack"),
          downgrade == Left(
            ArchiveStageError.Download(
              url,
              DownloadFailure.RedirectInvalid(
                "http://example.test/archive",
                HttpsUrlError.Invalid("http://example.test/archive")
              )
            )
          )
        )
    ,
    test("interrupting an HTTPS body closes its scope and removes scratch"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/tool.zip"))
          started <- Promise.make[Nothing, Unit]
          closed <- Ref.make(false)
          response = ZIO.acquireRelease(
            ZIO.succeed(
              Response(body =
                Body.stream(
                  ZStream.fromZIO(started.succeed(())).drain ++ ZStream.never
                )
              )
            )
          )(_ => closed.set(true))
          req = request(root, ArchiveSource.Remote(url), Chunk.empty)
          fiber <- ArchiveStage(client(response), CommonsArchiveDecoder())
            .stage(req)(ZIO.succeed(_))
            .forkScoped
          _ <- started.await.raceFirst(fiber.join.unit)
          exit <- fiber.interrupt
          released <- closed.get
          left <- names(req.root)
        yield assertTrue(exit.isInterrupted, released, left.isEmpty)
    ,
    test("concurrent attempts install one complete tree without scratch"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          bytes <- ArchiveFixture.zip("runtime/bin/tool" -> Chunk(1.toByte))
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/tool.zip"))
          req = request(root, ArchiveSource.Remote(url), bytes)
          stage = ArchiveStage(
            client(ZIO.succeed(Response(body = Body.fromBytes(bytes)))),
            CommonsArchiveDecoder()
          )
          results <- ZIO.foreachPar(1 to 8)(_ =>
            stage.stage(req)(ZIO.succeed(_))
          )
          left <- names(req.root)
        yield assertTrue(
          results.distinct == List(req.root.resolve("unpack")),
          left == List("archive", "unpack")
        )
    ,
    test("zip directory traversal is rejected, not ignored"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          bytes <- ArchiveFixture.zip("../escape/" -> Chunk.empty)
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/tool.zip"))
          req = request(root, ArchiveSource.Remote(url), bytes)
          result <- ArchiveStage(
            client(ZIO.succeed(Response(body = Body.fromBytes(bytes)))),
            CommonsArchiveDecoder()
          ).stage(req)(ZIO.succeed(_)).either
          left <- names(req.root)
        yield assertTrue(
          result == Left(ArchiveStageError.UnsafePath("../escape/")),
          left == List("archive")
        )
    ,
    test("tar rejects traversal, escaping links, and writes through a link"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          url <- ZIO.fromEither(
            HttpsUrl.from("https://example.test/tool.tar.gz")
          )
          fixtures = List(
            List(ArchiveFixture.Entry("../outside")),
            List(
              ArchiveFixture
                .Entry(
                  "tool/link",
                  kind = ArchiveFixture.Kind.Symbolic,
                  link = "../../outside"
                )
            ),
            List(
              ArchiveFixture.Entry(
                "tool/link",
                kind = ArchiveFixture.Kind.Hard,
                link = "../outside"
              )
            ),
            List(
              ArchiveFixture.Entry(
                "tool/link",
                kind = ArchiveFixture.Kind.Symbolic,
                link = "bin"
              ),
              ArchiveFixture.Entry("tool/link/written")
            )
          )
          results <- ZIO.foreach(fixtures.zipWithIndex): (entries, index) =>
            for
              bytes <- ArchiveFixture.tarGz(entries)
              req = request(
                root.resolve(index.toString),
                ArchiveSource.Remote(url),
                bytes
              )
              result <- ArchiveStage(
                client(ZIO.succeed(Response(body = Body.fromBytes(bytes)))),
                CommonsArchiveDecoder()
              ).stage(req)(ZIO.succeed(_)).either
            yield result
        yield assertTrue(
          results == List(
            Left(ArchiveStageError.UnsafePath("../outside")),
            Left(ArchiveStageError.UnsafeLink("tool/link", "../../outside")),
            Left(ArchiveStageError.UnsafeLink("tool/link", "../outside")),
            Left(ArchiveStageError.UnsafePath("tool/link/written"))
          )
        )
    ,
    test("PAX and GNU long names cannot bypass path validation"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          url <- ZIO.fromEither(
            HttpsUrl.from("https://example.test/tool.tar.gz")
          )
          name = "../" + "a" * 120
          formats = List(ArchiveFixture.Names.Pax, ArchiveFixture.Names.Gnu)
          results <- ZIO.foreach(formats.zipWithIndex): (format, index) =>
            for
              bytes <- ArchiveFixture.tarGz(
                List(ArchiveFixture.Entry(name)),
                format
              )
              req = request(
                root.resolve(index.toString),
                ArchiveSource.Remote(url),
                bytes
              )
              result <- ArchiveStage(
                client(ZIO.succeed(Response(body = Body.fromBytes(bytes)))),
                CommonsArchiveDecoder()
              ).stage(req)(ZIO.succeed(_)).either
            yield result
        yield assertTrue(
          results == List.fill(2)(
            Left(ArchiveStageError.UnsafePath(name))
          )
        )
    ,
    test(
      "tar preserves executable modes and contained symbolic and hard links"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-archive")
          url <- ZIO.fromEither(
            HttpsUrl.from("https://example.test/tool.tar.gz")
          )
          bytes <- ArchiveFixture.tarGz(
            List(
              ArchiveFixture
                .Entry("tool/bin/tool", Chunk(7.toByte), mode = 493),
              ArchiveFixture.Entry(
                "tool/tool",
                kind = ArchiveFixture.Kind.Symbolic,
                link = "bin/tool"
              ),
              ArchiveFixture.Entry(
                "tool/copy",
                kind = ArchiveFixture.Kind.Hard,
                link = "tool/bin/tool"
              )
            )
          )
          req = request(root, ArchiveSource.Remote(url), bytes)
          home <- ArchiveStage(
            client(ZIO.succeed(Response(body = Body.fromBytes(bytes)))),
            CommonsArchiveDecoder()
          ).stage(req)(ZIO.succeed(_))
          permissions <- ZIO.attemptBlocking(
            Files
              .getPosixFilePermissions(home.resolve("tool/bin/tool"))
              .asScala
              .toSet
          )
          linked <- ZIO.attemptBlocking(
            Files.readAllBytes(home.resolve("tool/tool"))
          )
          hard <- ZIO.attemptBlocking(
            Files.isSameFile(
              home.resolve("tool/copy"),
              home.resolve("tool/bin/tool")
            )
          )
        yield assertTrue(
          permissions.contains(PosixFilePermission.OWNER_EXECUTE),
          Chunk.fromArray(linked) == Chunk(7.toByte),
          hard
        )
  ) @@ TestAspect.timeout(1.minute)
