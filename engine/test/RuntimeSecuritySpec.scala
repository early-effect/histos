package histos.jdk

import histos.toolchain.{RuntimeLockFixture, ToolLockError}
import histos.archive.*
import heddle.client.{Client, ClientError}
import heddle.http.{Body, Request, Response}
import histos.native.NativeOrigin
import histos.node.*
import histos.toolchain.{RuntimeFiles, RuntimeIdentity}
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, Path}
import java.nio.file.attribute.{PosixFilePermission, PosixFilePermissions}
import scala.jdk.CollectionConverters.*

import zio.*
import zio.test.*

object RuntimeSecuritySpec extends ZIOSpecDefault:
  private def machine(root: Path): Machine = Machine(
    Map.empty,
    root.toString,
    Os.Linux,
    histos.toolchain.Architecture.X86_64,
    root.resolve("cache"),
    root,
    None,
    Settled.No,
    NativeOrigin.Central
  )

  private def client(bytes: Chunk[Byte]): Client = new Client:
    def batched(request: Request): IO[ClientError, Response] =
      ZIO.succeed(Response(body = Body.fromBytes(bytes)))
    def streaming(request: Request): ZIO[Scope, ClientError, Response] =
      batched(request)

  private def script(home: Path, body: String): IO[IoFailure, Path] =
    for
      _ <- Workspace.writeString(home.resolve("bin/node"), "#!/bin/sh\n" + body)
      _ <- ZIO
        .attemptBlocking(
          Files.setPosixFilePermissions(
            home.resolve("bin/node"),
            PosixFilePermissions.fromString("rwx------")
          )
        )
        .mapError(_ => IoFailure(home.resolve("bin/node"), IoFault.Write))
    yield home

  private def lock(root: Path): IO[
    IoFailure | NodeVersionError | HttpsUrlError | ToolLockError[Nothing],
    (NodeLock, Chunk[Byte])
  ] =
    for
      bytes <- ArchiveFixture.zip(
        "node/bin/node" -> Chunk.fromArray(
          "#!/bin/sh\nprintf '%s\\n' 'v22.14.0'\n".getBytes(
            java.nio.charset.StandardCharsets.UTF_8
          )
        )
      )
      version <- ZIO.fromEither(NodeVersion.from("22.14.0"))
      url <- ZIO.fromEither(HttpsUrl.from("https://example.test/node.zip"))
      pin <- ZIO.fromEither(
        RuntimeLockFixture.node(
          version,
          ArchiveSource.Remote(url),
          Sha256.of(bytes)
        )
      )
      _ <- Workspace.writeString(
        root.resolve(".histos/node.lock"),
        NodeLock.render(pin)
      )
    yield (pin, bytes)

  private def exists(path: Path): IO[IoFailure, Boolean] = ZIO
    .attemptBlocking(Files.exists(path))
    .mapError(_ => IoFailure(path, IoFault.Stat))

  def spec = suite("runtime security regressions")(
    test(
      "B1 locked resolve and explain never execute unrelated PATH, manager, or cache candidates"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-security")
          (pin, bytes) <- lock(root)
          markers = List("path-marker", "manager-marker", "cache-marker").map(
            root.resolve(_)
          )
          path <- script(
            root.resolve("unrelated"),
            s"printf marker > '${root.resolve("path-marker")}'\nprintf bad\n"
          )
          _ <- script(
            root.resolve(".nvm/versions/node/unrelated"),
            s"printf marker > '${root.resolve("manager-marker")}'\nprintf bad\n"
          )
          _ <- script(
            root.resolve("cache/nodes/unrelated"),
            s"printf marker > '${root.resolve("cache-marker")}'\nprintf bad\n"
          )
          current = machine(root).copy(env =
            Map("PATH" -> path.resolve("bin").toString)
          )
          survey <- NodeToolchain.explain(current)
          runtime <- NodeToolchain(
            NodeStage(ArchiveStage(client(bytes), CommonsArchiveDecoder()))
          ).resolve(current)
          executed <- ZIO.foreach(markers)(exists)
        yield assertTrue(
          executed == List(false, false, false),
          survey.resolution
            .is(_.subtype[NodeResolution.Provision])
            .major == pin.major,
          runtime.identity == RuntimeIdentity.Locked(Sha256.of(bytes))
        )
    ,
    test("B1 explain cannot be blocked by an unrelated hung Node"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-security")
          (pin, _) <- lock(root)
          path <- script(
            root.resolve("hung"),
            s"printf marker > '${root.resolve("executed")}'\nexec /bin/sleep 30\n"
          )
          result <- NodeToolchain
            .explain(
              machine(root)
                .copy(env = Map("PATH" -> path.resolve("bin").toString))
            )
            .timeout(500.millis)
          executed <- exists(root.resolve("executed"))
        yield assertTrue(
          !executed,
          result.map(_.resolution) == Some(
            NodeResolution.Provision(pin.major, Nil)
          )
        )
    ,
    test(
      "B1 corrupt cached Node is rejected before its version child executes"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-security")
          (_, bytes) <- lock(root)
          tool = NodeToolchain(
            NodeStage(ArchiveStage(client(bytes), CommonsArchiveDecoder()))
          )
          runtime <- tool.resolve(machine(root))
          home <- ZIO
            .fromOption(RuntimeFiles.homeOf(runtime.binary.path))
            .orElseFail(NodeError.NoRuntime)
          unpack <- ZIO
            .fromOption(Option(home.getParent))
            .orElseFail(NodeError.NoRuntime)
          expected <- RuntimeFiles.content(
            unpack,
            Set(unpack.resolve(ArchiveStage.ManifestName))
          )
          _ <- script(
            home,
            s"printf marker > '${root.resolve("corrupt-executed")}'\nprintf bad\n"
          )
          actual <- RuntimeFiles.content(
            unpack,
            Set(unpack.resolve(ArchiveStage.ManifestName))
          )
          result <- tool.resolve(machine(root)).either
          executed <- exists(root.resolve("corrupt-executed"))
        yield assertTrue(
          !executed,
          result == Left(
            NodeMiss.Unresolved(
              NodeError.Stage(
                NodeStageError.Archive(
                  ArchiveStageError.CorruptInstallation(expected, actual)
                )
              )
            )
          ),
          expected != actual,
          runtime.identity == RuntimeIdentity.Locked(Sha256.of(bytes))
        )
    ,
    test(
      "B2 cold and warm stages work through /tmp and a directory-symlink cache"
    ):
      ZIO.scoped:
        for
          root <- ZIO.acquireRelease(
            ZIO.attemptBlocking(
              Files.createTempDirectory(Path.of("/tmp"), "histos-runtime-alias")
            )
          )(path => Workspace.delete(path).ignore)
          target <- Workspace.createDirectories(root.resolve("target"))
          alias = root.resolve("alias")
          _ <- ZIO.attemptBlocking(Files.createSymbolicLink(alias, target))
          bytes <- ArchiveFixture.zip("tool" -> Chunk(7.toByte))
          _ <- ZIO.attemptBlocking(
            Files.write(root.resolve("unused"), bytes.toArray)
          )
          stage = ArchiveStage(client(bytes), CommonsArchiveDecoder())
          request = ArchiveRequest(
            ArchiveSource.Local(root.resolve("unused")),
            alias.resolve("cache"),
            Sha256.of(bytes)
          )
          first <- stage.stage(request)(ZIO.succeed(_))
          firstHash <- RuntimeFiles.content(
            first,
            Set(first.resolve(ArchiveStage.ManifestName))
          )
          second <- stage.stage(request)(ZIO.succeed(_))
          secondHash <- RuntimeFiles.content(
            second,
            Set(second.resolve(ArchiveStage.ManifestName))
          )
          content <- RuntimeFiles.bytes(second.resolve("tool")).runCollect
        yield assertTrue(
          firstHash == secondHash,
          first == second,
          content == Chunk(7.toByte)
        )
    ,
    test("B2 cold and warm stages work with a relative request root"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-relative")
          relative <- ZIO.attemptBlocking(
            Path.of("").toAbsolutePath.relativize(root.resolve("cache"))
          )
          bytes <- ArchiveFixture.zip("tool" -> Chunk(7.toByte))
          _ <- ZIO.attemptBlocking(
            Files.write(root.resolve("unused"), bytes.toArray)
          )
          stage = ArchiveStage(client(bytes), CommonsArchiveDecoder())
          request = ArchiveRequest(
            ArchiveSource.Local(root.resolve("unused")),
            relative,
            Sha256.of(bytes)
          )
          first <- stage.stage(request)(ZIO.succeed(_))
          second <- stage.stage(request)(ZIO.succeed(_))
          content <- RuntimeFiles.bytes(second.resolve("tool")).runCollect
        yield assertTrue(first == second, content == Chunk(7.toByte))
    ,
    test(
      "bootstrap Node symlink resolves to its installation, not the shared prefix"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-security")
          home <- script(
            root.resolve("node-install"),
            "printf '%s\\n' 'v22.14.0'\n"
          )
          prefix <- Workspace.createDirectories(
            root.resolve("shared-prefix/bin")
          )
          _ <- Workspace.writeString(
            root.resolve("shared-prefix/unrelated"),
            "must not be hashed"
          )
          _ <- ZIO.attemptBlocking(
            Files.createSymbolicLink(
              prefix.resolve("node"),
              home.resolve("bin/node")
            )
          )
          runtime <- NodeToolchain.resolve(
            machine(root).copy(env = Map("PATH" -> prefix.toString))
          )
          canonical <- ZIO.attemptBlocking(home.toRealPath())
        yield assertTrue(
          runtime.identity
            .is(_.subtype[RuntimeIdentity.Local])
            .home == canonical,
          runtime.binary.path == canonical.resolve("bin/node")
        )
    ,
    test(
      "buffering decoder entries cannot substitute the next tar body's bytes"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-security")
          bytes <- ArchiveFixture.tarGz(
            List(
              ArchiveFixture.Entry("first", Chunk(1.toByte)),
              ArchiveFixture.Entry("second", Chunk(2.toByte))
            )
          )
          _ <- ZIO.attemptBlocking(
            Files.write(root.resolve("archive"), bytes.toArray)
          )
          actual <- CommonsArchiveDecoder().fold(
            root.resolve("archive"),
            ArchiveBudget.default,
            Chunk.empty[(String, Chunk[Byte])]
          )((values, entry) =>
            entry.body.runCollect.map(body => values :+ (entry.name -> body))
          )
        yield assertTrue(
          actual == Chunk(
            "first" -> Chunk(1.toByte),
            "second" -> Chunk(2.toByte)
          )
        )
    ,
    test(
      "S2 existing shared roots and writable ancestors are rejected before download"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-security")
          cache <- Workspace.createDirectories(root.resolve("shared-cache"))
          parent <- Workspace.createDirectories(root.resolve("shared-parent"))
          _ <- ZIO.attemptBlocking(
            Files.setPosixFilePermissions(
              cache,
              PosixFilePermissions.fromString("rwxr-xr-x")
            )
          )
          _ <- ZIO.attemptBlocking(
            Files.setPosixFilePermissions(
              parent,
              PosixFilePermissions.fromString("rwxrwxrwx")
            )
          )
          bytes <- ArchiveFixture.zip("tool" -> Chunk(1.toByte))
          calls <- Ref.make(0)
          transport = new Client:
            def batched(request: Request): IO[ClientError, Response] =
              calls.update(_ + 1).as(Response(body = Body.fromBytes(bytes)))
            def streaming(request: Request): ZIO[Scope, ClientError, Response] =
              batched(request)
          url <- ZIO.fromEither(
            HttpsUrl.from("https://example.test/archive.zip")
          )
          stage = ArchiveStage(transport, CommonsArchiveDecoder())
          shared <- stage
            .stage(
              ArchiveRequest(ArchiveSource.Remote(url), cache, Sha256.of(bytes))
            )(ZIO.succeed(_))
            .either
          ancestor <- stage
            .stage(
              ArchiveRequest(
                ArchiveSource.Remote(url),
                parent.resolve("cache"),
                Sha256.of(bytes)
              )
            )(ZIO.succeed(_))
            .either
          count <- calls.get
        yield assertTrue(
          shared == Left(
            ArchiveStageError.Cache(RuntimeCacheError.Shared(cache))
          ),
          ancestor == Left(
            ArchiveStageError.Cache(RuntimeCacheError.Shared(parent))
          ),
          count == 0
        )
    ,
    test(
      "N1 owned aliases cannot hide writable physical ancestors before creation"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-trust-runtime")
          shared <- Workspace.createDirectories(root.resolve("shared"))
          target <- Workspace.createDirectories(shared.resolve("private"))
          alias = root.resolve("alias")
          _ <- ZIO.attemptBlocking:
            Files.setPosixFilePermissions(
              shared,
              PosixFilePermissions.fromString("rwxrwxrwx")
            )
            Files.setPosixFilePermissions(
              target,
              PosixFilePermissions.fromString("rwx------")
            )
            Files.createSymbolicLink(alias, target)
          bytes <- ArchiveFixture.zip("tool" -> Chunk(1.toByte))
          downloads <- Ref.make(0)
          decodes <- Ref.make(0)
          validations <- Ref.make(0)
          transport = new Client:
            def batched(request: Request): IO[ClientError, Response] =
              downloads.update(_ + 1).as(Response(body = Body.fromBytes(bytes)))
            def streaming(request: Request): ZIO[Scope, ClientError, Response] =
              batched(request)
          decoder = new ArchiveDecoder:
            def fold[A, E](archive: Path, budget: ArchiveBudget, initial: A)(
                consume: (A, DecodedEntry) => IO[E, A]
            ): IO[ExtractFailure | E, A] =
              decodes.update(_ + 1) *> CommonsArchiveDecoder()
                .fold(archive, budget, initial)(consume)
          url <- ZIO.fromEither(
            HttpsUrl.from("https://example.test/archive.zip")
          )
          results <- ZIO.foreach(
            List(alias.resolve("new/cache"), alias.resolve("../cache"))
          )(cache =>
            ArchiveStage(transport, decoder)
              .stage(
                ArchiveRequest(
                  ArchiveSource.Remote(url),
                  cache,
                  Sha256.of(bytes)
                )
              )(path => validations.update(_ + 1).as(path))
              .either
          )
          fetched <- downloads.get
          decoded <- decodes.get
          validated <- validations.get
          created <- exists(target.resolve("new"))
          cancelled <- exists(root.resolve("cache"))
          physical <- ZIO.attemptBlocking(shared.toRealPath())
        yield assertTrue(
          results == List.fill(2)(
            Left(
              ArchiveStageError.Cache(RuntimeCacheError.Shared(physical))
            )
          ),
          fetched == 0,
          decoded == 0,
          validated == 0,
          !created,
          !cancelled
        )
    ,
    test(
      "S2 archive root and write modes do not weaken private installation permissions"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-security")
          bytes <- ArchiveFixture.tarGz(
            List(
              ArchiveFixture
                .Entry("./", mode = 511, kind = ArchiveFixture.Kind.Directory),
              ArchiveFixture.Entry("tool", Chunk(1.toByte), mode = 4095)
            )
          )
          _ <- ZIO.attemptBlocking(
            Files.write(root.resolve("unused"), bytes.toArray)
          )
          request = ArchiveRequest(
            ArchiveSource.Local(root.resolve("unused")),
            root.resolve("cache"),
            Sha256.of(bytes)
          )
          installed <- ArchiveStage(client(bytes), CommonsArchiveDecoder())
            .stage(request)(ZIO.succeed(_))
          cacheMode <- ZIO.attemptBlocking(
            Files.getPosixFilePermissions(request.root).asScala.toSet
          )
          rootMode <- ZIO.attemptBlocking(
            Files.getPosixFilePermissions(installed).asScala.toSet
          )
          fileMode <- ZIO.attemptBlocking(
            Files
              .getPosixFilePermissions(installed.resolve("tool"))
              .asScala
              .toSet
          )
        yield assertTrue(
          cacheMode == PosixFilePermissions
            .fromString("rwx------")
            .asScala
            .toSet,
          rootMode == PosixFilePermissions
            .fromString("rwx------")
            .asScala
            .toSet,
          !fileMode.contains(PosixFilePermission.GROUP_WRITE),
          !fileMode.contains(PosixFilePermission.OTHERS_WRITE),
          fileMode.contains(PosixFilePermission.OWNER_EXECUTE)
        )
  ) @@ TestAspect.withLiveSystem @@ TestAspect.withLiveClock @@ TestAspect.sequential @@ TestAspect
    .timeout(2.minutes)
