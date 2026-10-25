package histos.publish

import histos.jdk.Sha256
import histos.workspace.Workspace
import histos.process.{ChildProcess, ChildEvent, ProcessExit}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import heddle.{Body, Handler, Request, Response, Routes, Server, Status}
import heddle.server.Port
import heddle.client.Client
import zio.*
import zio.test.*
import zio.stream.ZStream
import java.security.MessageDigest

object LauncherPinSpec extends ZIOSpecDefault:
  private val version  = "0.0.1-SNAPSHOT"
  private val revision = "0.0.1-20261023.224241-14"
  private val bytes    =
    Chunk.fromArray("pinned launcher".getBytes(StandardCharsets.UTF_8))
  private val sha        = Sha256.of(bytes).value
  private val repository =
    "https://central.sonatype.com/repository/maven-snapshots"
  private val lock =
    s"repository $repository\nversion $version\nrevision $revision\nsha256 $sha\n"
  private val metadata =
    s"""<metadata><groupId>rocks.earlyeffect</groupId><artifactId>histos</artifactId><version>$version</version>
       |<versioning><snapshot><timestamp>20261023.224241</timestamp><buildNumber>14</buildNumber></snapshot>
       |<snapshotVersions><snapshotVersion><extension>jar</extension><value>$revision</value></snapshotVersion>
       |</snapshotVersions></versioning></metadata>""".stripMargin

  def spec = suite("immutable launcher pin")(
    test(
      "the complete schema round trips and rejects duplicate and unknown fields"
    ):
      val pin = LauncherPin.parse(lock)
      assertTrue(
        pin.map(LauncherPin.render).contains(lock),
        LauncherPin.parse(lock + "version 0.0.1-SNAPSHOT\n") == Left(
          LauncherError.Duplicate(LauncherField.Version)
        ),
        LauncherPin.parse(lock + "latest true\n") == Left(
          LauncherError.Unknown("latest")
        ),
        LauncherPin.parse(lock.replace(s"sha256 $sha\n", "")) == Left(
          LauncherError.Missing(LauncherField.Sha256)
        ),
        LauncherPin.parse("version\n") == Left(LauncherError.Line("version")),
      )
    ,
    test(
      "coordinates cannot resolve latest, escape a directory, or accept an invalid timestamp"
    ):
      assertTrue(
        LauncherPin.from(repository, version, "0.0.1-SNAPSHOT", sha) == Left(
          LauncherError.Revision("0.0.1-SNAPSHOT")
        ),
        LauncherPin.from(repository, "../0.0.1-SNAPSHOT", revision, sha).isLeft,
        LauncherPin
          .from(repository, version, "0.0.1-20260230.224241-14", sha)
          .isLeft,
        LauncherPin
          .from(repository, version, "0.0.1-20261023.224241-2147483648", sha)
          .isLeft,
        LauncherPin
          .from(repository, version, "0.0.1-20261023.224241-0", sha)
          .isLeft,
        LauncherPin.from(repository, version, revision, "g" * 64).isLeft,
      )
    ,
    test("PR snapshot qualifiers keep their exact immutable coordinate"):
      val pin = LauncherPin.from(
        repository,
        "0.0.1-pr31-SNAPSHOT",
        "0.0.1-pr31-20261023.224241-14",
        sha,
      )
      assertTrue(
        pin.map(_.version).contains("0.0.1-pr31-SNAPSHOT"),
        pin.map(_.revision).contains("0.0.1-pr31-20261023.224241-14"),
      )
    ,
    test("only https or loopback http repositories are accepted"):
      assertTrue(
        LauncherPin
          .from("http://127.0.0.1:1234/repo", version, revision, sha)
          .isRight,
        LauncherPin
          .from("http://localhost:1234/repo", version, revision, sha)
          .isRight,
        LauncherPin
          .from("http://example.com/repo", version, revision, sha)
          .isLeft,
        LauncherPin
          .from("https://user@example.com/repo", version, revision, sha)
          .isLeft,
        LauncherPin
          .from(repository + "?latest=1", version, revision, sha)
          .isLeft,
        LauncherPin
          .from(repository + "/../other", version, revision, sha)
          .isLeft,
      )
    ,
    test("canonical URL outcomes match the shared bootstrap corpus"):
      for
        root <- zio.System
          .env("HISTOS_REPO")
          .mapError(_ => LauncherError.Repository("fixture root"))
        path <- ZIO
          .fromOption(root.map(Path.of(_)))
          .orElseFail(LauncherError.Repository("fixture root"))
        text <- Workspace.readString(
          path.resolve("engine/test/fixtures/launcher-urls.tsv")
        )
      yield
        val cases = text.linesIterator
          .filterNot(_.startsWith("#"))
          .toList
          .map: line =>
            line.split("\\t", 3).toList match
              case kind :: expected :: encoded :: Nil =>
                val raw = new String(
                  java.util.Base64.getDecoder.decode(encoded),
                  StandardCharsets.UTF_8,
                )
                val found =
                  if kind == "repository" then LauncherUrl.repository(raw)
                  else LauncherUrl.archive(raw)
                found.isRight == (expected == "1")
              case _ => false
        assertTrue(cases.nonEmpty, cases.forall(identity))
    ,
    test(
      "snapshot metadata validates coordinates and its unclassified jar entry"
    ):
      for
        exact <- LauncherMetadata.revision(metadata, version)
        wrong <- LauncherMetadata
          .revision(
            metadata.replace("<artifactId>histos", "<artifactId>other"),
            version,
          )
          .either
        duplicate <- LauncherMetadata
          .revision(
            metadata.replace(
              "</snapshotVersions>",
              s"<snapshotVersion><extension>jar</extension><value>$revision</value></snapshotVersion></snapshotVersions>",
            ),
            version,
          )
          .either
        jar <- LauncherMetadata
          .revision(
            metadata.replace(s"<value>$revision", "<value>0.0.1-SNAPSHOT"),
            version,
          )
          .either
        missing   <- LauncherMetadata.revision("<metadata/>", version).either
        malformed <- LauncherMetadata.revision("<metadata>", version).either
        entity    <- LauncherMetadata
          .revision(
            "<!DOCTYPE metadata [<!ENTITY leak SYSTEM 'file:///etc/passwd'>]><metadata>&leak;</metadata>",
            version,
          )
          .either
      yield assertTrue(
        exact == revision,
        wrong == Left(
          LauncherError.Metadata(LauncherMetadataError.Coordinates)
        ),
        duplicate == Left(LauncherError.Metadata(LauncherMetadataError.Jar)),
        jar == Left(LauncherError.Metadata(LauncherMetadataError.Jar)),
        missing == Left(
          LauncherError.Metadata(LauncherMetadataError.Missing("groupId"))
        ),
        malformed == Left(LauncherError.Metadata(LauncherMetadataError.Xml)),
        entity == Left(LauncherError.Metadata(LauncherMetadataError.Xml)),
      )
    ,
    test(
      "a scoped heddle updater stages checked bytes before replacing the pin"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("launcher-pin")
          seen <- Ref.make(List.empty[String])
          routes = fixture(metadata, bytes, sha, seen)
          server <- Server.install(
            routes,
            Server.Config.default.copy(host = "127.0.0.1", port = Port(0)),
          )
          port <- server.port
          old  <- ZIO.fromEither(
            LauncherPin.from(
              s"http://127.0.0.1:$port/repo",
              version,
              "0.0.1-20261022.224241-13",
              sha,
            )
          )
          _ <- Workspace.writeString(
            root.resolve(LauncherPin.file),
            LauncherPin.render(old),
          )
          pin    <- LauncherPin.update(root)
          read   <- LauncherPin.read(root)
          staged <- ZIO
            .attemptBlocking(
              Files.readAllBytes(
                root
                  .resolve(".histos/launcher-cache")
                  .resolve(sha)
                  .resolve("histos.jar")
              )
            )
            .mapError(_ =>
              LauncherError.Io(
                histos.workspace.IoFailure(root, histos.workspace.IoFault.Read)
              )
            )
          requests <- seen.get
          temps    <- temporary(root)
        yield assertTrue(
          pin.revision == revision,
          LauncherPin.render(read) == LauncherPin.render(pin),
          Chunk.fromArray(staged) == bytes,
          requests.exists(_.endsWith("maven-metadata.xml")),
          requests.exists(_.endsWith(s"histos-$revision.jar")),
          temps.isEmpty,
        )
    ,
    test(
      "invalid metadata, sidecar and artifact leave the committed pin intact"
    ):
      ZIO.scoped:
        ZIO
          .foreach(
            List(
              (metadata.replace("rocks.earlyeffect", "other"), bytes, sha),
              (metadata, bytes, "invalid"),
              (
                metadata,
                Chunk.fromArray("corrupt".getBytes(StandardCharsets.UTF_8)),
                sha,
              ),
            )
          ): (xml, artifact, checksum) =>
            for
              root   <- Workspace.work("launcher-refuse")
              _      <- Workspace.writeString(root.resolve(LauncherPin.file), lock)
              seen   <- Ref.make(List.empty[String])
              client <- ZIO
                .service[Client]
                .provide(
                  Client.inMemory(fixture(xml, artifact, checksum, seen))
                )
              refused <- LauncherUpdater(client).update(root).either
              after   <- Workspace.readString(root.resolve(LauncherPin.file))
              temps   <- temporary(root)
            yield assertTrue(refused.isLeft, after == lock, temps.isEmpty)
          .map(results => results.foldLeft(assertTrue(true))(_ && _))
    ,
    test(
      "staging failure preserves the old pin and concurrent updates publish complete files"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("launcher-atomic")
          _    <- Workspace.writeString(root.resolve(LauncherPin.file), lock)
          _    <- Workspace.writeString(
            root.resolve(".histos/launcher-cache"),
            "blocked destination",
          )
          seen   <- Ref.make(List.empty[String])
          client <- ZIO
            .service[Client]
            .provide(Client.inMemory(fixture(metadata, bytes, sha, seen)))
          updater = LauncherUpdater(client)
          refused   <- updater.update(root).either
          old       <- Workspace.readString(root.resolve(LauncherPin.file))
          _         <- Workspace.delete(root.resolve(".histos/launcher-cache"))
          published <- ZIO.foreachPar(List(1, 2, 3))(_ => updater.update(root).either)
          pin       <- LauncherPin.read(root)
          temps     <- temporary(root)
        yield assertTrue(
          refused.isLeft,
          old == lock,
          published.forall {
            case Right(pin)                               => pin.revision == revision
            case Left(LauncherError.Conflict(_, current)) => current == revision
            case _                                        => false
          },
          pin.revision == revision,
          temps.isEmpty,
        )
    ,
    suite("80 MiB HTTP framing")(
      List("fixed", "chunked", "close").map { framing =>
        test(s"streamed launcher accepts $framing framing"):
          ZIO.scoped:
            val block = Chunk.fromArray(Array.fill[Byte](64 * 1024)(42))
            val size  = 80L * 1024 * 1024
            for
              expected <- ZIO
                .attemptBlocking:
                  val digest = MessageDigest.getInstance("SHA-256")
                  (0 until 1280).foreach(_ => digest.update(block.toArray))
                  Sha256.fromDigest(digest.digest()).value
                .mapError(_ =>
                  LauncherError.Checksum(
                    histos.jdk.Sha256Error.Bad("fixture digest")
                  )
                )
              stream = ZStream.repeatZIOChunk(ZIO.succeed(block)).take(size)
              body   = Body.stream(
                stream,
                length = if framing == "chunked" then None else Some(size),
              )
              artifact =
                if framing == "close" then
                  Response(Status.Ok, body)
                    .withHeader("Transfer-Encoding", "identity")
                    .withHeader("Connection", "close")
                else Response(Status.Ok, body)
              routes = Routes.fromHandler(Handler { (request: Request) =>
                val path = request.path.render
                ZIO.succeed(if path.endsWith("maven-metadata.xml") then Response.text(metadata)
                else if path.endsWith(".sha256") then Response.text(expected)
                else artifact)
              })
              root   <- Workspace.work("launcher-large")
              server <- Server.install(
                routes,
                Server.Config.default.copy(host = "127.0.0.1", port = Port(0)),
              )
              port <- server.port
              old  <- ZIO.fromEither(
                LauncherPin.from(
                  s"http://127.0.0.1:$port/repo",
                  version,
                  revision,
                  sha,
                )
              )
              _ <- Workspace.writeString(
                root.resolve(LauncherPin.file),
                LauncherPin.render(old),
              )
              pin    <- LauncherPin.update(root)
              staged <- ZIO
                .attemptBlocking(
                  Files.size(
                    root.resolve(s".histos/launcher-cache/$expected/histos.jar")
                  )
                )
                .mapError(_ =>
                  LauncherError.Io(
                    histos.workspace
                      .IoFailure(root, histos.workspace.IoFault.Stat)
                  )
                )
              temps <- temporary(root)
            yield assertTrue(
              pin.sha256.value == expected,
              staged == size,
              temps.isEmpty,
            )
      }*
    ),
    test("every payload bound rejects oversize bytes before replacing the pin"):
      ZIO.scoped:
        ZIO
          .foreach(
            List(
              LauncherPayload.Metadata,
              LauncherPayload.Checksum,
              LauncherPayload.Artifact,
            )
          ): kind =>
            val block    = Chunk.fromArray(Array.fill[Byte](64 * 1024)(42))
            val tooLarge = Response(
              Status.Ok,
              Body.stream(
                ZStream.repeatZIOChunk(ZIO.succeed(block)).take(kind.limit + 1)
              ),
            )
            val routes = Routes.fromHandler(Handler { (request: Request) =>
              val path   = request.path.render
              val chosen = if path.endsWith("maven-metadata.xml") then LauncherPayload.Metadata
              else if path.endsWith(".sha256") then LauncherPayload.Checksum
              else LauncherPayload.Artifact
              ZIO.succeed(if chosen == kind then tooLarge
              else if chosen == LauncherPayload.Metadata then Response.text(metadata)
              else if chosen == LauncherPayload.Checksum then Response.text(sha)
              else Response(Status.Ok, Body.fromBytes(bytes)))
            })
            for
              root   <- Workspace.work("launcher-size")
              _      <- Workspace.writeString(root.resolve(LauncherPin.file), lock)
              client <- ZIO.service[Client].provide(Client.inMemory(routes))
              result <- LauncherUpdater(client).update(root).either
              after  <- Workspace.readString(root.resolve(LauncherPin.file))
              temps  <- temporary(root)
            yield assertTrue(
              result == Left(LauncherError.Size(kind, kind.limit)),
              after == lock,
              temps.isEmpty,
            )
          .map(_.foldLeft(assertTrue(true))(_ && _))
    ,
    test(
      "N2 unknown-length oversize checksum fails without pulling a blocked continuation"
    ):
      ZIO.scoped:
        for
          root      <- Workspace.work("histos-trust-checksum")
          _         <- Workspace.writeString(root.resolve(LauncherPin.file), lock)
          requested <- Ref.make(false)
          closed    <- Ref.make(false)
          sidecar = Response(
            Status.Ok,
            Body.stream(
              ZStream.fromChunk(Chunk.fromArray(Array.fill[Byte](257)(42))) ++
                ZStream.fromZIO(requested.set(true) *> ZIO.never).drain
            ),
          )
          client = new Client:
            def batched(request: Request) = ZIO.succeed(
              if request.path.render.endsWith("maven-metadata.xml") then Response.text(metadata)
              else sidecar
            )
            def streaming(request: Request) =
              ZIO.acquireRelease(batched(request))(_ =>
                closed
                  .set(true)
                  .when(request.path.render.endsWith(".sha256"))
                  .unit
              )
          result <- LauncherUpdater(client)
            .update(root)
            .either
            .timeout(1.second)
          continuation <- requested.get
          released     <- closed.get
          after        <- Workspace.readString(root.resolve(LauncherPin.file))
          temps        <- temporary(root)
        yield assertTrue(
          result.contains(
            Left(LauncherError.Size(LauncherPayload.Checksum, 256))
          ),
          !continuation,
          released,
          after == lock,
          temps.isEmpty,
        )
    ,
    test(
      "N1 updater refuses an alias behind a writable physical parent before staging"
    ):
      ZIO.scoped:
        for
          root   <- Workspace.work("histos-trust-updater")
          shared <- Workspace.createDirectories(root.resolve("shared"))
          target <- Workspace.createDirectories(shared.resolve("private"))
          alias = root.resolve("alias")
          _ <- ZIO.attemptBlocking:
            Files.setPosixFilePermissions(
              shared,
              java.nio.file.attribute.PosixFilePermissions
                .fromString("rwxrwxrwx"),
            )
            Files.setPosixFilePermissions(
              target,
              java.nio.file.attribute.PosixFilePermissions
                .fromString("rwx------"),
            )
            Files.createSymbolicLink(alias, target)
          _      <- Workspace.writeString(target.resolve(LauncherPin.file), lock)
          seen   <- Ref.make(List.empty[String])
          client <- ZIO
            .service[Client]
            .provide(Client.inMemory(fixture(metadata, bytes, sha, seen)))
          result  <- LauncherUpdater(client).update(alias).either
          after   <- Workspace.readString(target.resolve(LauncherPin.file))
          created <- ZIO.attemptBlocking(
            Files.exists(target.resolve(".histos/launcher-cache"))
          )
          temps    <- temporary(target)
          requests <- seen.get
        yield assertTrue(
          result == Left(
            LauncherError.Cache(alias.resolve(".histos/launcher-cache"))
          ),
          after == lock,
          !created,
          temps.isEmpty,
          !requests.exists(_.endsWith(".jar")),
        )
    ,
    test(
      "N1 updater retains trusted /tmp aliases and relative repository roots"
    ):
      ZIO.scoped:
        for
          root <- ZIO.acquireRelease(
            ZIO
              .attemptBlocking(
                Files.createTempDirectory(
                  Path.of("/tmp"),
                  "histos-trust-updater-positive",
                )
              )
              .mapError(_ =>
                LauncherError.Io(
                  histos.workspace
                    .IoFailure(Path.of("/tmp"), histos.workspace.IoFault.Temp)
                )
              )
          )(path => Workspace.delete(path).ignore)
          target <- Workspace.createDirectories(root.resolve("private"))
          alias = root.resolve("alias")
          _ <- ZIO
            .attemptBlocking(Files.createSymbolicLink(alias, target))
            .mapError(_ =>
              LauncherError.Io(
                histos.workspace
                  .IoFailure(alias, histos.workspace.IoFault.Create)
              )
            )
          _        <- Workspace.writeString(target.resolve(LauncherPin.file), lock)
          relative <- ZIO
            .attemptBlocking(Path.of("").toAbsolutePath.relativize(alias))
            .mapError(_ =>
              LauncherError.Io(
                histos.workspace.IoFailure(alias, histos.workspace.IoFault.Stat)
              )
            )
          seen   <- Ref.make(List.empty[String])
          client <- ZIO
            .service[Client]
            .provide(Client.inMemory(fixture(metadata, bytes, sha, seen)))
          pin    <- LauncherUpdater(client).update(relative)
          cached <- Workspace.readString(
            target.resolve(s".histos/launcher-cache/$sha/histos.jar")
          )
          temps <- temporary(target)
        yield assertTrue(
          pin.sha256.value == sha,
          cached == "pinned launcher",
          temps.isEmpty,
        )
    ,
    test("a delayed R1 updater cannot overwrite a newer R2 pin"):
      ZIO.scoped:
        for
          root    <- Workspace.work("launcher-conflict")
          _       <- Workspace.writeString(root.resolve(LauncherPin.file), lock)
          entered <- Promise.make[Nothing, Unit]
          resume  <- Promise.make[Nothing, Unit]
          newer     = revision.stripSuffix("14") + "15"
          firstBody = Body.stream(
            ZStream
              .fromZIO(entered.succeed(()) *> resume.await)
              .drain ++ ZStream.fromChunk(bytes)
          )
          first <- ZIO
            .service[Client]
            .provide(
              Client.inMemory(
                updaterRoutes(metadata, sha, Response(Status.Ok, firstBody))
              )
            )
          second <- ZIO
            .service[Client]
            .provide(
              Client.inMemory(
                updaterRoutes(
                  metadata
                    .replace("<buildNumber>14", "<buildNumber>15")
                    .replace(revision, newer),
                  sha,
                  Response(Status.Ok, Body.fromBytes(bytes)),
                )
              )
            )
          delayed  <- LauncherUpdater(first).update(root).either.forkScoped
          _        <- entered.await
          advanced <- LauncherUpdater(second).update(root)
          _        <- resume.succeed(())
          refused  <- delayed.join
          current  <- LauncherPin.read(root)
          temps    <- temporary(root)
        yield assertTrue(
          advanced.revision == newer,
          current.revision == newer,
          refused == Left(LauncherError.Conflict(revision, newer)),
          temps.isEmpty,
        )
    ,
    test(
      "the repository commit lock rejects a stale updater in a separate JVM"
    ):
      ZIO.scoped:
        for
          root          <- Workspace.work("launcher-process-conflict")
          entered       <- Promise.make[Nothing, Unit]
          resume        <- Promise.make[Nothing, Unit]
          metadataCalls <- Ref.make(0)
          newer       = revision.stripSuffix("14") + "15"
          delayedBody = Body.stream(
            ZStream
              .fromZIO(entered.succeed(()) *> resume.await)
              .drain ++ ZStream.fromChunk(bytes),
            length = Some(bytes.length.toLong),
          )
          routes = Routes.fromHandler(Handler { (request: Request) =>
            val path = request.path.render
            if path.endsWith("maven-metadata.xml") then
              metadataCalls
                .updateAndGet(_ + 1)
                .map: count =>
                  Response.text(if count == 1 then metadata
                  else
                    metadata
                      .replace("<buildNumber>14", "<buildNumber>15")
                      .replace(revision, newer))
            else
              ZIO.succeed(if path.endsWith(".sha256") then Response.text(sha)
              else if path.endsWith(s"histos-$revision.jar") then Response(Status.Ok, delayedBody)
              else Response(Status.Ok, Body.fromBytes(bytes)))
          })
          server <- Server.install(
            routes,
            Server.Config.default.copy(host = "127.0.0.1", port = Port(0)),
          )
          port <- server.port
          old  <- ZIO.fromEither(
            LauncherPin.from(
              s"http://127.0.0.1:$port/repo",
              version,
              revision,
              sha,
            )
          )
          _ <- Workspace.writeString(
            root.resolve(LauncherPin.file),
            LauncherPin.render(old),
          )
          javaHome <- zio.System
            .property("java.home")
            .mapError(_ => LauncherProcessError.Property("java.home"))
          home <- ZIO
            .fromOption(javaHome)
            .orElseFail(LauncherProcessError.Property("java.home"))
          cpValue <- zio.System
            .property("java.class.path")
            .mapError(_ => LauncherProcessError.Property("java.class.path"))
          cp <- ZIO
            .fromOption(cpValue)
            .orElseFail(LauncherProcessError.Property("java.class.path"))
          classpath = cp
            .split(java.io.File.pathSeparator)
            .map(raw => Path.of(raw).toAbsolutePath.normalize.toString)
            .mkString(java.io.File.pathSeparator)
          env     <- zio.System.envs.mapError(_ => LauncherProcessError.Environment)
          work    <- Workspace.createDirectories(root.resolve("process"))
          delayed <- ChildProcess
            .command(
              List(
                Path.of(home, "bin", "java").toString,
                "--enable-native-access=ALL-UNNAMED",
                "-cp",
                classpath,
                "histos.publish.LauncherPinProcess",
                root.toString,
              ),
              work,
              env,
            )
            .runCollect
            .forkScoped
          _        <- ZIO.addFinalizer(resume.succeed(()).unit)
          _        <- entered.await
          advanced <- LauncherPin.update(root)
          _        <- resume.succeed(())
          refused  <- delayed.join
          current  <- LauncherPin.read(root)
          temps    <- temporary(root)
        yield assertTrue(
          advanced.revision == newer,
          current.revision == newer,
          refused.lastOption.contains(ChildEvent.Exited(ProcessExit(1))),
          refused.exists {
            case ChildEvent.Err(text) =>
              text.contains("Conflict") && text.contains(newer)
            case ChildEvent.Out(text) =>
              text.contains("Conflict") && text.contains(newer)
            case ChildEvent.Exited(_) => false
          },
          temps.isEmpty,
        )
    ,
    test(
      "interrupted artifact consumption closes its response and removes scratch"
    ):
      ZIO.scoped:
        for
          root    <- Workspace.work("launcher-interrupt")
          _       <- Workspace.writeString(root.resolve(LauncherPin.file), lock)
          entered <- Promise.make[Nothing, Unit]
          closed  <- Ref.make(false)
          response = Response(
            Status.Ok,
            Body.stream(
              ZStream.fromChunk(
                Chunk.fromArray(Array.fill[Byte](64 * 1024)(42))
              ) ++
                ZStream.fromZIO(entered.succeed(()) *> ZIO.never).drain
            ),
          )
          base <- ZIO
            .service[Client]
            .provide(Client.inMemory(updaterRoutes(metadata, sha, response)))
          client = new Client:
            def batched(request: Request)   = base.batched(request)
            def streaming(request: Request) =
              ZIO.acquireRelease(base.streaming(request))(_ => closed.set(true))
          fiber    <- LauncherUpdater(client).update(root).forkScoped
          _        <- entered.await
          exit     <- fiber.interrupt
          after    <- Workspace.readString(root.resolve(LauncherPin.file))
          temps    <- temporary(root)
          released <- closed.get
        yield assertTrue(
          exit.isInterrupted,
          after == lock,
          temps.isEmpty,
          released,
        )
    ,
    test(
      "interrupting a process-lock waiter preserves the old complete pin and cleans scratch"
    ):
      ZIO.scoped:
        for
          root    <- Workspace.work("launcher-commit-interrupt")
          _       <- Workspace.writeString(root.resolve(LauncherPin.file), lock)
          locked  <- Promise.make[Nothing, Unit]
          release <- Promise.make[Nothing, Unit]
          holder  <- LauncherFiles
            .locked(root)(locked.succeed(()) *> release.await)
            .forkScoped
          _      <- locked.await
          client <- ZIO
            .service[Client]
            .provide(
              Client.inMemory(
                updaterRoutes(
                  metadata,
                  sha,
                  Response(Status.Ok, Body.fromBytes(bytes)),
                )
              )
            )
          updater <- LauncherUpdater(client).update(root).forkScoped
          _       <- temporary(root).repeatUntil(_.size >= 2)
          exit    <- updater.interrupt
          after   <- Workspace.readString(root.resolve(LauncherPin.file))
          temps   <- temporary(root)
          _       <- release.succeed(())
          _       <- holder.join
        yield assertTrue(exit.isInterrupted, after == lock, temps.isEmpty)
    ,
    test(
      "interrupted metadata and checksum reads release the body without creating staged bytes"
    ):
      ZIO.scoped:
        ZIO
          .foreach(List(LauncherPayload.Metadata, LauncherPayload.Checksum)): kind =>
            for
              root    <- Workspace.work("launcher-small-interrupt")
              _       <- Workspace.writeString(root.resolve(LauncherPin.file), lock)
              entered <- Promise.make[Nothing, Unit]
              closed  <- Ref.make(false)
              waiting = Response(
                Status.Ok,
                Body.stream(
                  ZStream.fromZIO(entered.succeed(()) *> ZIO.never).drain
                ),
              )
              routes = Routes.fromHandler(Handler { (request: Request) =>
                val selected =
                  if request.path.render.endsWith("maven-metadata.xml") then LauncherPayload.Metadata
                  else LauncherPayload.Checksum
                ZIO.succeed(if kind == selected then waiting
                else Response.text(metadata))
              })
              base <- ZIO.service[Client].provide(Client.inMemory(routes))
              client = new Client:
                def batched(request: Request)   = base.batched(request)
                def streaming(request: Request) = ZIO.acquireRelease(
                  base.streaming(request)
                )(_ => closed.set(true))
              fiber    <- LauncherUpdater(client).update(root).forkScoped
              _        <- entered.await
              exit     <- fiber.interrupt
              after    <- Workspace.readString(root.resolve(LauncherPin.file))
              temps    <- temporary(root)
              released <- closed.get
            yield assertTrue(
              exit.isInterrupted,
              after == lock,
              temps.isEmpty,
              released,
            )
          .map(_.foldLeft(assertTrue(true))(_ && _))
    ,
    test(
      "bounded write and each atomic rename finish before interruption cleanup"
    ):
      ZIO.scoped:
        ZIO
          .foreach(List("write", "artifact-move", "pin-move")): phase =>
            for
              root <- Workspace.work("launcher-io-boundary")
              old  <- ZIO.fromEither(
                LauncherPin
                  .from(repository, version, "0.0.1-20261022.224241-13", sha)
              )
              _ <- Workspace.writeString(
                root.resolve(LauncherPin.file),
                LauncherPin.render(old),
              )
              entered <- ZIO.succeed(new java.util.concurrent.Semaphore(0))
              resume  <- ZIO.succeed(new java.util.concurrent.Semaphore(0))
              client  <- ZIO
                .service[Client]
                .provide(
                  Client.inMemory(
                    updaterRoutes(
                      metadata,
                      sha,
                      Response(Status.Ok, Body.fromBytes(bytes)),
                    )
                  )
                )
              barrier = ZIO
                .attemptBlocking { entered.release(); resume.acquire() }
                .mapError(_ =>
                  LauncherError.Io(
                    histos.workspace
                      .IoFailure(root, histos.workspace.IoFault.Write)
                  )
                )
              storage = new LauncherCommit:
                def write(path: Path, bytes: Chunk[Byte]) =
                  (if phase == "write" then barrier
                   else ZIO.unit) *> LauncherCommit.live.write(path, bytes)
                def move(from: Path, to: Path) =
                  (if (phase == "artifact-move" && to.getFileName.toString == "histos.jar") ||
                     (phase == "pin-move" && to == root
                       .resolve(LauncherPin.file))
                   then barrier
                   else ZIO.unit) *> LauncherCommit.live.move(from, to)
              fiber <- LauncherUpdater(client, storage).update(root).forkScoped
              _     <- ZIO.addFinalizer(ZIO.succeed(resume.release()))
              _     <- ZIO
                .attemptBlockingInterrupt(entered.acquire())
                .mapError(_ => LauncherError.InvalidPath(root))
              interrupted <- fiber.interrupt.fork
              _           <- ZIO.yieldNow
              before      <- LauncherPin.read(root)
              pending     <- interrupted.poll
              _           <- ZIO.succeed(resume.release())
              exit        <- interrupted.join
              after       <- LauncherPin.read(root)
              temps       <- temporary(root)
              finalArtifact = root.resolve(
                s".histos/launcher-cache/$sha/histos.jar"
              )
              verified <- ZIO
                .attemptBlocking(if Files.isRegularFile(finalArtifact) then
                  Some(
                    Sha256
                      .of(Chunk.fromArray(Files.readAllBytes(finalArtifact)))
                      .value
                  )
                else None)
                .mapError(_ => LauncherError.InvalidPath(finalArtifact))
            yield assertTrue(
              before.revision == old.revision,
              pending.isEmpty,
              exit.isInterrupted,
              temps.isEmpty,
              after.revision == (if phase == "write" then old.revision
                                 else revision),
              if phase == "write" then verified.isEmpty
              else verified.contains(sha),
            )
          .map(_.foldLeft(assertTrue(true))(_ && _))
    ,
    test(
      "a directory or dangling committed pin is a typed invalid path, not absence"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("launcher-presence")
          path = root.resolve(LauncherPin.file)
          _         <- Workspace.createDirectories(path)
          directory <- LauncherPin.read(root).either
          _         <- Workspace.delete(path)
          _         <- ZIO
            .attemptBlocking(
              Files.createSymbolicLink(path, root.resolve("missing"))
            )
            .mapError(_ =>
              LauncherError.Io(
                histos.workspace
                  .IoFailure(path, histos.workspace.IoFault.Create)
              )
            )
          dangling <- LauncherPin.read(root).either
          _        <- ZIO
            .attemptBlocking(Files.delete(path))
            .mapError(_ => LauncherError.InvalidPath(path))
          absent <- LauncherPin.read(root).either
        yield assertTrue(
          directory == Left(LauncherError.InvalidPath(path)),
          dangling == Left(LauncherError.InvalidPath(path)),
          absent == Left(LauncherError.Absent(path)),
        )
    ,
    test(
      "workflow generation checks the local pin without reaching a repository"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("launcher-offline")
          _    <- Workspace.writeString(root.resolve(LauncherPin.file), lock)
          read <- LauncherPin.read(root)
        yield assertTrue(read.revision == revision, read.sha256.value == sha)
    ,
    test("the Windows script finds java.exe and runs the pinned jar"):
      val script = LauncherScript.windows(version)
      assertTrue(
        script.contains("java.exe"),
        script.contains("launcher.lock"),
        script.contains("Get-FileHash -LiteralPath"),
        script.contains("--enable-native-access=ALL-UNNAMED"),
        !script.contains("certutil"),
        !script.contains("maven-metadata.xml"),
        !script.contains(".jar.sha256"),
      ),
  ) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.sequential @@ TestAspect
    .timeout(90.seconds)

  private def fixture(
      xml: String,
      bytes: Chunk[Byte],
      checksum: String,
      seen: Ref[List[String]],
  ) =
    Routes.fromHandler(Handler { (request: Request) =>
      val path = request.path.render
      val body =
        if path.endsWith("maven-metadata.xml") then Some(Chunk.fromArray(xml.getBytes(StandardCharsets.UTF_8)))
        else if path.endsWith(".jar.sha256") then Some(Chunk.fromArray(checksum.getBytes(StandardCharsets.UTF_8)))
        else if path.endsWith(".jar") then Some(bytes)
        else None
      seen
        .update(_ :+ path)
        .as(
          body.fold(Response.notFound())(bytes => Response(Status.Ok, Body.fromBytes(bytes)))
        )
    })

  private def updaterRoutes(xml: String, checksum: String, artifact: Response) =
    Routes.fromHandler(Handler { (request: Request) =>
      val path = request.path.render
      ZIO.succeed(if path.endsWith("maven-metadata.xml") then Response.text(xml)
      else if path.endsWith(".sha256") then Response.text(checksum)
      else artifact)
    })

  private def temporary(root: Path): IO[LauncherError, List[Path]] =
    ZIO
      .attemptBlocking:
        val files = Files.walk(root)
        try
          import scala.jdk.CollectionConverters.*
          files.iterator.asScala
            .filter(_.getFileName.toString.startsWith(".launcher-"))
            .toList
        finally files.close()
      .mapError(_ =>
        LauncherError.Io(
          histos.workspace.IoFailure(root, histos.workspace.IoFault.Walk)
        )
      )
end LauncherPinSpec

private enum LauncherProcessError:
  case Arguments
  case Property(name: String)
  case Environment

object LauncherPinProcess extends ZIOAppDefault:
  def run =
    for
      args <- getArgs
      root <- args.toList match
        case raw :: Nil =>
          ZIO
            .attempt(Path.of(raw))
            .mapError(_ => LauncherProcessError.Arguments)
        case _ => ZIO.fail(LauncherProcessError.Arguments)
      _ <- LauncherPin.update(root)
    yield ()
