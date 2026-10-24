package histos.jdk

import histos.toolchain.{RuntimeLockFixture, ToolLockError, HostPlatform}
import histos.archive.*
import histos.cache.ContentStore
import histos.toolchain.RuntimeIdentity
import heddle.client.{Client, ClientError}
import heddle.http.{Body, Request, Response}
import histos.native.{NativeOrigin, NativeSource, NativeToolchainError}
import histos.process.{ChildEvent, ChildProcess, ProcessExit}

import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.toolchain.RuntimeFiles

import java.nio.file.{Files, Path}
import java.nio.file.attribute.{PosixFilePermission, PosixFilePermissions}

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

import zio.*
import zio.test.*

object JdkSpec extends ZIOSpecDefault:
  private val major21 = JdkMajor.trusted(21)
  private val major17 = JdkMajor.trusted(17)
  private val major8 = JdkMajor.trusted(8)
  private val v21 = JdkVersion.trusted("21.0.2", major21)
  private val v17 = JdkVersion.trusted("17.0.9", major17)

  private def home(path: String, version: JdkVersion): JdkHome =
    JdkHome(Path.of(path), hasJavac = true, version = Right(version))

  private val bare =
    ProbeView(
      Located.Unset,
      Located.Unset,
      Located.Unset,
      ManagerSlot.Absent,
      ManagerSlot.Absent,
      Nil
    )

  private val policy = suite("resolution")(
    test("JAVA_HOME and PATH are diagnostics, not authority"):
      val java = home("/jdks/21", v21)
      val path = home("/jdks/path", v21)
      val found = Jdk.resolve(
        major21,
        bare.copy(javaHome = Located.Found(java), onPath = Located.Found(path))
      )
      assertTrue(
        found.is(_.subtype[Resolution.Provision]).major == major21,
        found
          .is(_.subtype[Resolution.Provision])
          .trace
          .contains(Candidate.Match(Origin.JavaHome, java.path, v21)),
        found
          .is(_.subtype[Resolution.Provision])
          .trace
          .contains(Candidate.Match(Origin.Path, path.path, v21))
      )
    ,
    test(
      "installed managers and an arbitrary cache do not replace the archive pin"
    ):
      val found = Jdk.resolve(
        major21,
        bare.copy(
          coursier = ManagerSlot.Present(List(home("/cs/21", v21))),
          sdkman = ManagerSlot.Present(Nil),
          cached = List(home("/cache/21", v21))
        )
      )
      assertTrue(found.is(_.subtype[Resolution.Provision]).major == major21)
    ,
    test("a matching explicit override is selected"):
      val chosen = Candidate.Match(Origin.Override, Path.of("/local/21"), v21)
      assertTrue(
        Jdk
          .resolve(
            major21,
            bare.copy(explicit = Located.Found(home("/local/21", v21)))
          )
          .is(_.subtype[Resolution.Selected])
          .chosen == chosen
      )
    ,
    test("a wrong explicit override stops, even with a matching JAVA_HOME"):
      val found = Jdk.resolve(
        major21,
        bare.copy(
          explicit = Located.Found(home("/local/17", v17)),
          javaHome = Located.Found(home("/jdks/21", v21))
        )
      )
      assertTrue(
        found.is(_.subtype[Resolution.OverrideRejected]).reason == Reject
          .WrongMajor(major17)
      )
    ,
    test("a missing explicit override stops"):
      assertTrue(
        Jdk
          .resolve(
            major21,
            bare.copy(explicit = Located.Missing(Path.of("/missing")))
          )
          .is(_.subtype[Resolution.OverrideRejected])
          .reason == Reject.Absent
      )
    ,
    test("a bare machine provisions"):
      assertTrue(
        Jdk
          .resolve(major21, bare)
          .is(_.subtype[Resolution.Provision])
          .major == major21
      )
  )

  private val versions = suite("versions")(
    test("a modern release file yields its major"):
      assertTrue(
        JdkVersion.fromRelease("JAVA_VERSION=\"21.0.2\"\n") == Right(v21)
      )
    ,
    test("a 1.8 release file yields major 8"):
      assertTrue(JdkVersion.from("1.8.0_402").is(_.right).major == major8)
    ,
    test("a release file without the key is a typed miss"):
      assertTrue(
        JdkVersion.fromRelease("IMPLEMENTOR=\"Eclipse\"") == Left(
          ReleaseError.MissingKey
        )
      )
    ,
    test("an unclosed quote is not a missing key"):
      assertTrue(
        JdkVersion.fromRelease("JAVA_VERSION=\"21") == Left(
          ReleaseError.Unclosed
        )
      )
    ,
    test("words are not a version"):
      assertTrue(JdkVersion.from("nope") == Left(VersionError.Bad("nope")))
  )

  private val locks = suite("lock file")(
    test("a file URL host pin round-trips"):
      for lock <- ZIO.fromEither(
          RuntimeLockFixture.jdk(
            v21,
            ArchiveSource.Local(Path.of("/opt/jdk.zip")),
            Sha256.of(Chunk(1.toByte))
          )
        )
      yield assertTrue(
        JdkLock.parse(JdkLock.render(lock)).map(JdkLock.render) == Right(
          JdkLock.render(lock)
        )
      )
    ,
    test("the declared major must agree with the full version"):
      val text =
        s"major 21\nversion 17.0.9\narchive linux-x86_64 file:///opt/a.zip ${"ab" * 32}\n"
      assertTrue(
        JdkLock.parse(text) == Left(
          LockError.Schema(
            ToolLockError.Pin(JdkPinError.MajorDisagrees(major21, major17))
          )
        )
      )
    ,
    test("HTTPS rows retain their typed source and HTTP rows fail"):
      for
        url <- ZIO.fromEither(HttpsUrl.from("https://example.test/jdk.zip"))
        lock <- ZIO.fromEither(
          RuntimeLockFixture.jdk(
            v21,
            ArchiveSource.Remote(url),
            Sha256.of(Chunk.empty)
          )
        )
        parsed <- ZIO.fromEither(JdkLock.parse(JdkLock.render(lock)))
        pin <- ZIO.fromEither(parsed.archive(HostPlatform.LinuxX86_64))
      yield assertTrue(
        pin.source == ArchiveSource.Remote(url),
        JdkLock.parse(
          JdkLock.render(lock).replace("https://", "http://")
        ) == Left(
          LockError.Schema(
            ToolLockError.Source(
              ArchiveSourceError.Invalid("http://example.test/jdk.zip")
            )
          )
        )
      )
    ,
    test("unknown legacy fields are not silently accepted"):
      assertTrue(
        JdkLock.parse(
          "major 21\nversion 21.0.2\nurl file:///opt/a.zip"
        ) == Left(LockError.Schema(ToolLockError.UnknownField("url")))
      )
  )

  private val stage = suite("stage")(
    test("a local zip is verified and unpacked"):
      ZIO.scoped:
        for
          root <- temp
          zip <- jdkZip("21.0.2")
          file = root.resolve("jdk.zip")
          _ <- ZIO.attempt(Files.write(file, zip))
          lock <- ZIO.fromEither(
            RuntimeLockFixture.jdk(
              v21,
              ArchiveSource.Local(file),
              Sha256.of(zio.Chunk.fromArray(zip))
            )
          )
          home <- JdkStage.stage(lock, machine(root))
          java <- ZIO.attempt(
            Files.isRegularFile(home.path.resolve("bin").resolve("java"))
          )
          versionOk = home.version.exists(version =>
            version.value == v21.value && version.major == v21.major
          )
        yield assertTrue(home.hasJavac, versionOk, java)
    ,
    test("a checksum mismatch does not keep the archive"):
      ZIO.scoped:
        for
          root <- temp
          zip <- jdkZip("21.0.2")
          file = root.resolve("jdk.zip")
          _ <- ZIO.attempt(Files.write(file, zip))
          other = Sha256.of(zio.Chunk.fromArray(Array[Byte](9)))
          lock <- ZIO.fromEither(
            RuntimeLockFixture.jdk(v21, ArchiveSource.Local(file), other)
          )
          out <- JdkStage.stage(lock, machine(root)).either
        yield assertTrue(
          out == Left(
            StageError.Checksum(other, Sha256.of(zio.Chunk.fromArray(zip)))
          ),
          !Files.exists(
            root
              .resolve("cache")
              .resolve("jdks")
              .resolve(other.value)
              .resolve("archive")
          )
        )
    ,
    test("the locked full version is validated before installation"):
      ZIO.scoped:
        for
          root <- temp
          zip <- jdkZip("21.0.3")
          file = root.resolve("different.zip")
          _ <- ZIO.attempt(Files.write(file, zip))
          sha = Sha256.of(Chunk.fromArray(zip))
          result <- ZIO
            .fromEither(
              RuntimeLockFixture.jdk(v21, ArchiveSource.Local(file), sha)
            )
            .flatMap(lock => JdkStage.stage(lock, machine(root)))
            .either
          left <- names(root.resolve("cache/jdks").resolve(sha.value))
        yield assertTrue(
          result == Left(StageError.NoHome),
          left == List("archive")
        )
    ,
    test("a malformed release file retains its typed version failure"):
      ZIO.scoped:
        for
          root <- temp
          zip <- zipOf(
            "jdk/bin/javac" -> Array.emptyByteArray,
            "jdk/release" -> "IMPLEMENTOR=fixture\n".getBytes(
              java.nio.charset.StandardCharsets.UTF_8
            )
          )
          file = root.resolve("bad-release.zip")
          _ <- ZIO.attemptBlocking(Files.write(file, zip))
          lock <- ZIO.fromEither(
            RuntimeLockFixture.jdk(
              v21,
              ArchiveSource.Local(file),
              Sha256.of(Chunk.fromArray(zip))
            )
          )
          result <- JdkStage.stage(lock, machine(root)).either
          left <- names(
            root
              .resolve("cache/jdks")
              .resolve(Sha256.of(Chunk.fromArray(zip)).value)
          )
        yield assertTrue(
          result
            .is(_.left)
            .is(_.subtype[StageError.Version])
            .error == ReleaseError.MissingKey,
          left == List("archive")
        )
    ,
    test("a malformed zip reports an extraction failure"):
      ZIO.scoped:
        for
          root <- temp
          raw <- zipOf("jdk/bin/javac" -> Array[Byte](1, 2, 3))
          zip = raw.take(24)
          file = root.resolve("malformed.zip")
          _ <- ZIO.attempt(Files.write(file, zip))
          sha = Sha256.of(zio.Chunk.fromArray(zip))
          out <- ZIO
            .fromEither(
              RuntimeLockFixture.jdk(v21, ArchiveSource.Local(file), sha)
            )
            .flatMap(lock => JdkStage.stage(lock, machine(root)))
            .either
          archive = root
            .resolve("cache")
            .resolve("jdks")
            .resolve(sha.value)
            .resolve("archive")
        yield assertTrue(
          out == Left(
            StageError.Extract(
              archive,
              ExtractFailure.Malformed(ArchiveKind.Zip)
            )
          )
        )
    ,
    test("a zip entry that escapes the cache is rejected"):
      ZIO.scoped:
        for
          root <- temp
          zip <- zipOf("../evil" -> Array[Byte](1))
          file = root.resolve("slip.zip")
          _ <- ZIO.attempt(Files.write(file, zip))
          sha = Sha256.of(zio.Chunk.fromArray(zip))
          lock <- ZIO.fromEither(
            RuntimeLockFixture.jdk(v21, ArchiveSource.Local(file), sha)
          )
          out <- JdkStage.stage(lock, machine(root)).either
        yield assertTrue(
          out == Left(StageError.UnsafePath("../evil")),
          !Files.exists(
            root
              .resolve("cache")
              .resolve("jdks")
              .resolve(sha.value)
              .resolve("evil")
          )
        )
    ,
    test("an HTTPS archive stages through the injected streaming client"):
      ZIO.scoped:
        for
          root <- temp
          bytes <- jdkZip("21.0.2")
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/jdk.zip"))
          lock <- ZIO.fromEither(
            RuntimeLockFixture.jdk(
              v21,
              ArchiveSource.Remote(url),
              Sha256.of(Chunk.fromArray(bytes))
            )
          )
          home <- JdkStage(
            ArchiveStage(archiveClient(bytes), CommonsArchiveDecoder())
          )
            .stage(lock, machine(root))
        yield assertTrue(home.version == Right(v21), home.hasJavac)
    ,
    test("a local tar.gz keeps the exec bit, and its java runs"):
      ZIO.scoped:
        for
          root <- temp
          env <- System.envs
          tree = root.resolve("tree")
          _ <- ZIO.attempt {
            val home = tree.resolve("jdk-21.0.2+13")
            Files.createDirectories(home.resolve("bin"))
            Files.writeString(
              home.resolve("bin").resolve("java"),
              "#!/bin/sh\necho staged java\n"
            )
            Files.setPosixFilePermissions(
              home.resolve("bin").resolve("java"),
              PosixFilePermissions.fromString("rwxr-xr-x")
            )
            Files.writeString(home.resolve("bin").resolve("javac"), "")
            Files.writeString(
              home.resolve("release"),
              "JAVA_VERSION=\"21.0.2\"\n"
            )
            Files.createDirectories(root.resolve("pack"))
            Files.createDirectories(root.resolve("run"))
          }
          file = root.resolve("jdk.tar.gz")
          packed <- ChildProcess
            .command(
              List(
                "tar",
                "-czf",
                file.toString,
                "-C",
                tree.toString,
                "jdk-21.0.2+13"
              ),
              root.resolve("pack"),
              env
            )
            .runCollect
          bytes <- ZIO.attempt(Files.readAllBytes(file))
          lock <- ZIO.fromEither(
            RuntimeLockFixture.jdk(
              v21,
              ArchiveSource.Local(file),
              Sha256.of(zio.Chunk.fromArray(bytes))
            )
          )
          home <- JdkStage.stage(lock, machine(root).copy(env = env))
          java = home.path.resolve("bin").resolve("java")
          mode <- ZIO.attempt(Files.getPosixFilePermissions(java).asScala.toSet)
          ran <- ChildProcess
            .command(List(java.toString), root.resolve("run"), Map.empty)
            .runCollect
        yield assertTrue(
          packed.lastOption.contains(ChildEvent.Exited(ProcessExit(0))),
          home.version.map(_.value) == Right("21.0.2"),
          mode.contains(PosixFilePermission.OWNER_EXECUTE),
          ran.toList == List(
            ChildEvent.Out("staged java"),
            ChildEvent.Exited(ProcessExit(0))
          )
        )
    @@ TestAspect.withLiveSystem,
    test("a failed unpack leaves no partial directory and no temp file"):
      ZIO.scoped:
        for
          root <- temp
          zip <- zipOf(
            "jdk/bin/javac" -> Array.emptyByteArray,
            "../evil" -> Array[Byte](1)
          )
          file = root.resolve("slip.zip")
          _ <- ZIO.attempt(Files.write(file, zip))
          sha = Sha256.of(zio.Chunk.fromArray(zip))
          out <- ZIO
            .fromEither(
              RuntimeLockFixture.jdk(v21, ArchiveSource.Local(file), sha)
            )
            .flatMap(lock => JdkStage.stage(lock, machine(root)))
            .either
          left <- names(
            root.resolve("cache").resolve("jdks").resolve(sha.value)
          )
        yield assertTrue(
          out == Left(StageError.UnsafePath("../evil")),
          left == List("archive")
        )
    ,
    test("a verified warm unpack is reused without decoding again"):
      ZIO.scoped:
        for
          root <- temp
          bytes <- jdkZip("21.0.2")
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/jdk.zip"))
          calls <- Ref.make(0)
          delegate = CommonsArchiveDecoder()
          decoder = new ArchiveDecoder:
            def fold[A, E](archive: Path, budget: ArchiveBudget, initial: A)(
                consume: (A, DecodedEntry) => IO[E, A]
            ) =
              calls.update(_ + 1) *> delegate.fold(archive, budget, initial)(
                consume
              )
          stager = JdkStage(ArchiveStage(archiveClient(bytes), decoder))
          lock <- ZIO.fromEither(
            RuntimeLockFixture.jdk(
              v21,
              ArchiveSource.Remote(url),
              Sha256.of(Chunk.fromArray(bytes))
            )
          )
          first <- stager.stage(lock, machine(root))
          second <- stager.stage(lock, machine(root))
          count <- calls.get
        yield assertTrue(
          first == second,
          first.version == Right(v21),
          count == 1
        )
  )

  private val startup = suite("startup")(
    test("locations follow the os and the env"):
      assertTrue(
        JdkLocations.coursierCache("/Users/a", Os.Mac, Map.empty) == Path.of(
          "/Users/a/Library/Caches/Coursier"
        ),
        JdkLocations.coursierCache("/Users/a", Os.Linux, Map.empty) == Path.of(
          "/Users/a/.cache/coursier"
        ),
        JdkLocations.coursierCache(
          "/Users/a",
          Os.Mac,
          Map("COURSIER_CACHE" -> "/opt/cs")
        ) == Path.of("/opt/cs"),
        JdkLocations.sdkmanJava(
          "/Users/a",
          Map("SDKMAN_DIR" -> "/opt/sdk")
        ) == Path.of("/opt/sdk/candidates/java")
      )
    ,
    test("an empty home does not see this machine's JDKs"):
      ZIO.scoped:
        for
          root <- temp
          view <- JdkLocations.probe(machine(root), None).flatMap(Jdk.scan)
        yield assertTrue(
          Jdk
            .resolve(major21, view)
            .is(_.subtype[Resolution.Provision])
            .major == major21
        )
    ,
    test(
      "a coursier cache without a JDK, and no cs on PATH, is not an installed manager"
    ):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt(
            Files.createDirectories(
              root
                .resolve(".cache")
                .resolve("coursier")
                .resolve("arc")
                .resolve("https")
            )
          )
          view <- JdkLocations.probe(machine(root), None).flatMap(Jdk.scan)
        yield assertTrue(
          view.coursier == ManagerSlot.Absent,
          Jdk
            .resolve(major21, view)
            .is(_.subtype[Resolution.Provision])
            .major == major21
        )
    ,
    test("cs on PATH, or a JDK in coursier's cache, is an installed manager"):
      ZIO.scoped:
        for
          root <- temp
          bin = root.resolve("bin")
          jdk = root
            .resolve(".cache")
            .resolve("coursier")
            .resolve("arc")
            .resolve("https")
            .resolve("jdk-17")
          _ <- ZIO.attempt {
            Files.createDirectories(bin)
            Files.writeString(bin.resolve("cs"), "")
            Files.createDirectories(jdk.resolve("bin"))
            Files.writeString(jdk.resolve("bin").resolve("javac"), "")
            Files.writeString(
              jdk.resolve("release"),
              "JAVA_VERSION=\"17.0.9\"\n"
            )
          }
          onPath <- JdkLocations
            .probe(
              machine(root.resolve("elsewhere"))
                .copy(env = Map("PATH" -> bin.toString)),
              None
            )
            .flatMap(Jdk.scan)
          cached <- JdkLocations.probe(machine(root), None).flatMap(Jdk.scan)
        yield assertTrue(
          onPath.coursier == ManagerSlot.Present(Nil),
          cached.coursier == ManagerSlot.Present(
            List(JdkHome(jdk, hasJavac = true, version = Right(v17)))
          )
        )
    ,
    test("scan reads javac and the release file"):
      ZIO.scoped:
        for
          root <- temp
          jdk = root.resolve("java-home")
          _ <- ZIO.attempt {
            Files.createDirectories(jdk.resolve("bin"))
            Files.writeString(jdk.resolve("bin").resolve("javac"), "")
            Files.writeString(
              jdk.resolve("release"),
              "JAVA_VERSION=\"21.0.2\"\n"
            )
          }
          view <- Jdk.scan(
            ScanInput(
              None,
              Some(jdk),
              None,
              ManagerProbe.Absent,
              ManagerProbe.Absent,
              root.resolve("jdks")
            )
          )
        yield assertTrue(
          Jdk
            .resolve(major21, view)
            .is(_.subtype[Resolution.Provision])
            .trace
            .contains(Candidate.Match(Origin.JavaHome, jdk, v21))
        )
    ,
    test("no lock continues on the current JVM"):
      ZIO.scoped:
        for
          root <- temp
          out <- Toolchain.prepare(machine(root))
        yield assertTrue(
          out == Startup.Continue && !Files.exists(
            root.resolve("cache").resolve("jdks")
          )
        )
    ,
    test("a bare machine stages a file url and plans a re-exec"):
      ZIO.scoped:
        for
          root <- temp
          zip <- jdkZip("21.0.2")
          file = root.resolve("jdk.zip")
          _ <- ZIO.attempt(Files.write(file, zip))
          _ <- writeLock(root, file, zip)
          out <- Toolchain.prepare(machine(root))
          shown = out match
            case Startup.Replace(javaBin) =>
              val file = Files.isRegularFile(javaBin)
              val ends = javaBin.endsWith(Path.of("bin", "java"))
              if file && ends then "ok"
              else s"bad file=$file ends=$ends path=$javaBin"
            case other => other.toString
        yield assertTrue(shown == "ok")
    ,
    test("prepare provisions HTTPS instead of refusing"):
      ZIO.scoped:
        for
          root <- temp
          bytes <- jdkZip("21.0.2")
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/jdk.zip"))
          lock <- ZIO.fromEither(
            RuntimeLockFixture.jdk(
              v21,
              ArchiveSource.Remote(url),
              Sha256.of(Chunk.fromArray(bytes))
            )
          )
          _ <- ZIO.attempt {
            Files.createDirectories(root.resolve(".histos"))
            Files.writeString(
              root.resolve(".histos/jdk.lock"),
              JdkLock.render(lock)
            )
          }
          out <- Toolchain(
            JdkStage(
              ArchiveStage(archiveClient(bytes), CommonsArchiveDecoder())
            )
          )
            .prepare(machine(root))
        yield assertTrue(
          out
            .is(_.subtype[Startup.Replace])
            .javaBin
            .endsWith(Path.of("bin/java"))
        )
    ,
    test("a malformed toolchain.local fails before a download"):
      ZIO.scoped:
        for
          root <- temp
          bytes <- jdkZip("21.0.2")
          file = root.resolve("jdk.zip")
          pinForFile <- ZIO.fromEither(
            RuntimeLockFixture.jdk(
              v21,
              ArchiveSource.Local(file),
              Sha256.of(Chunk.fromArray(bytes))
            )
          )
          _ <- ZIO.attempt {
            Files.write(file, bytes)
            Files.createDirectories(root.resolve(".histos"))
            Files.writeString(
              root.resolve(".histos/jdk.lock"),
              JdkLock.render(pinForFile)
            )
            Files.writeString(
              root.resolve(".histos/toolchain.local"),
              "relative"
            )
          }
          out <- Toolchain.prepare(machine(root)).either
        yield assertTrue(
          out == Left(
            ToolchainError.Override(OverrideError.Relative("relative"))
          )
        )
    ,
    test(
      "a local override reports a content identity that changes with its files"
    ):
      ZIO.scoped:
        for
          root <- temp
          local = root.resolve("local")
          pinForFile <- ZIO.fromEither(
            RuntimeLockFixture.jdk(
              v21,
              ArchiveSource.Local(root.resolve("unused.zip")),
              Sha256.of(Chunk.empty)
            )
          )
          _ <- ZIO.attempt {
            Files.createDirectories(local.resolve("bin"))
            Files.writeString(local.resolve("bin/javac"), "one")
            Files.writeString(
              local.resolve("release"),
              "JAVA_VERSION=\"21.0.2\"\n"
            )
            Files.createDirectories(root.resolve(".histos"))
            Files.writeString(
              root.resolve(".histos/toolchain.local"),
              local.toString
            )
            Files.writeString(
              root.resolve(".histos/jdk.lock"),
              JdkLock.render(pinForFile)
            )
          }
          first <- Toolchain.resolve(machine(root))
          _ <- ZIO.attempt(Files.writeString(local.resolve("bin/javac"), "two"))
          second <- Toolchain.resolve(machine(root))
        yield assertTrue(
          first.version == v21,
          first.identity
            .is(_.subtype[RuntimeIdentity.Local])
            .home
            .endsWith(Path.of("local")),
          first.identity != second.identity
        )
    ,
    test("a settled process does not re-exec"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
            Files.createDirectories(root.resolve(".histos"))
            Files.writeString(
              root.resolve(".histos").resolve("jdk.lock"),
              "not a lock"
            )
          }
          out <- Toolchain.prepare(machine(root).copy(settled = Settled.Yes))
        yield assertTrue(out == Startup.Continue)
    ,
    test("re-exec keeps the jar and the arguments"):
      val command = Toolchain.command(
        Path.of("/jdks/21/bin/java"),
        Path.of("/app.jar"),
        List("run", "inspect")
      )
      assertTrue(
        command == List(
          "/jdks/21/bin/java",
          Toolchain.NativeAccess,
          "-jar",
          "/app.jar",
          "run",
          "inspect"
        )
      )
    ,
    test("the re-exec guard is the digit 1"):
      assertTrue(
        Settled.from(Map("HISTOS_JDK_SETTLED" -> "1")) == Settled.Yes && Settled
          .from(Map("HISTOS_JDK_SETTLED" -> "true")) == Settled.No
      )
  )

  private val handOff = suite("hand-off")(
    test("the hand-off returns the child's status and marks the child settled"):
      ZIO.scoped:
        for
          root <- temp
          stub <- script(
            root,
            s"echo \"$$HISTOS_JDK_SETTLED\" > '$root/settled'\nexit 7\n"
          )
          code <- Toolchain.replace(stub, root.resolve("app.jar"), Nil)
          settled <- ZIO.attempt(Files.readString(root.resolve("settled")).trim)
        yield assertTrue(code == 7, settled == "1")
    ,
    test("interrupting the hand-off kills the child and what it started"):
      ZIO.scoped:
        for
          root <- temp
          stub <- script(
            root,
            s"echo $$$$ > '$root/leader.pid'\nsleep 300 &\necho $$! > '$root/child.pid'\nwait\n"
          )
          fiber <- Toolchain.replace(stub, root.resolve("app.jar"), Nil).fork
          leader <- awaitPid(root.resolve("leader.pid"))
          child <- awaitPid(root.resolve("child.pid"))
          exit <- fiber.interrupt
          dead <- ZIO.foreach(leader.toList ++ child.toList)(awaitDead)
        yield assertTrue(
          exit.isInterrupted,
          leader.nonEmpty,
          child.nonEmpty,
          dead == List(true, true)
        )
    ,
    test(
      "an interrupted hand-off lets the child handle SIGTERM before anything is killed"
    ):
      ZIO.scoped:
        for
          root <- temp
          stub <- script(root, trapsTerm(root))
          fiber <- Toolchain.replace(stub, root.resolve("app.jar"), Nil).fork
          leader <- awaitPid(root.resolve("leader.pid"))
          child <- awaitPid(root.resolve("child.pid"))
          _ <- fiber.interrupt
          status <- ZIO.attempt(
            Option.when(Files.isRegularFile(root.resolve("status")))(
              Files.readString(root.resolve("status")).trim
            )
          )
        yield assertTrue(
          leader.nonEmpty,
          child.nonEmpty,
          status.contains("130")
        )
  ) @@ TestAspect.withLiveClock

  /** The child traps TERM and exits 130. The leader ignores TERM, which the
    * child must not inherit, so it starts the child first, then records the
    * child's real exit status.
    */
  private def trapsTerm(root: Path): String =
    s"""sh -c 'trap "exit 130" TERM; echo $$$$ > "$$0"; while :; do sleep 0.2; done' '$root/child.pid' 2>/dev/null &
       |trap '' TERM
       |echo $$$$ > '$root/leader.pid'
       |wait $$!
       |echo $$? > '$root/status'
       |""".stripMargin

  /** A stand-in for `java`: the hand-off runs whatever executable it is given.
    */
  private def script(root: Path, body: String): IO[IoFailure, Path] =
    ZIO
      .attemptBlocking:
        val path = root.resolve("java")
        Files.writeString(path, s"#!/bin/sh\n$body")
        Files.setPosixFilePermissions(
          path,
          java.nio.file.attribute.PosixFilePermissions.fromString("rwx------")
        )
      .mapError(_ => IoFailure(root.resolve("java"), IoFault.Write))

  private def awaitPid(path: Path): IO[IoFailure, Option[Long]] =
    readPid(path)
      .flatMap:
        case Some(pid) => ZIO.succeed(Some(pid))
        case None      => ZIO.sleep(20.millis) *> awaitPid(path)
      .timeout(2.seconds)
      .map(_.flatten)

  private def readPid(path: Path): IO[IoFailure, Option[Long]] =
    RuntimeFiles
      .readIfPresent(path)
      .map(_.flatMap(_.trim.toLongOption.filter(_ > 0)))

  private def awaitDead(pid: Long): UIO[Boolean] =
    ZIO
      .suspendSucceed {
        if ProcessHandle.of(pid).toScala.exists(_.isAlive) then
          ZIO.sleep(20.millis) *> awaitDead(pid)
        else ZIO.succeed(true)
      }
      .timeout(1.second)
      .map(_.getOrElse(false))

  private def archiveClient(bytes: Array[Byte]): Client =
    new Client:
      def batched(request: Request): IO[ClientError, Response] =
        ZIO.succeed(Response(body = Body.fromBytes(Chunk.fromArray(bytes))))
      def streaming(request: Request): ZIO[Scope, ClientError, Response] =
        batched(request)

  private def machine(root: Path): Machine =
    Machine(
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

  private def writeLock(
      root: Path,
      archive: Path,
      bytes: Array[Byte]
  ): IO[IoFailure | ToolLockError[Nothing], Unit] =
    for
      lock <- ZIO.fromEither(
        RuntimeLockFixture.jdk(
          v21,
          ArchiveSource.Local(archive),
          Sha256.of(Chunk.fromArray(bytes))
        )
      )
      _ <- Workspace.writeString(
        root.resolve(".histos/jdk.lock"),
        JdkLock.render(lock)
      )
    yield ()

  private def jdkZip(version: String): IO[IoFailure, Array[Byte]] =
    val release = s"JAVA_VERSION=\"$version\"\n".getBytes(
      java.nio.charset.StandardCharsets.UTF_8
    )
    zipOf(
      "jdk/bin/javac" -> Array.emptyByteArray,
      "jdk/bin/java" -> Array.emptyByteArray,
      "jdk/release" -> release
    )

  private def zipOf(
      entries: (String, Array[Byte])*
  ): IO[IoFailure, Array[Byte]] =
    ArchiveFixture
      .zip(entries.map((name, bytes) => name -> Chunk.fromArray(bytes))*)
      .map(_.toArray)

  private def names(dir: Path): IO[IoFailure, List[String]] =
    RuntimeFiles
      .paths(dir, 1)
      .map(
        _.filterNot(_ == dir)
          .flatMap(path => Option(path.getFileName).map(_.toString))
          .sorted
      )

  private def temp: ZIO[Scope, IoFailure, Path] = Workspace.work("histos-jdk")

  private val config = suite("machine")(
    test(
      "HISTOS_NATIVE and HISTOS_JDK_SETTLED are config over the one environment read"
    ):
      TestSystem.save.flatMap: restore =>
        val body =
          for
            _ <- TestSystem.putProperty("user.home", "/home/dev")
            _ <- TestSystem.putProperty("os.name", "Linux")
            _ <- TestSystem.putProperty("os.arch", "amd64")
            _ <- TestSystem.putProperty("java.home", "/jdks/21")
            _ <- TestSystem.putEnv("HISTOS_NATIVE", "ivy-local")
            _ <- TestSystem.putEnv("HISTOS_JDK_SETTLED", "1")
            _ <- TestSystem.putEnv("XDG_CACHE_HOME", "/xdg")
            loaded <- Machine.load(Path.of("/repo"))
            _ <- TestSystem.clearProperty("user.home")
            missing <- Machine.load(Path.of("/repo")).either
            _ <- TestSystem.putProperty("user.home", "")
            blank <- Machine.load(Path.of("/repo")).either
            _ <- TestSystem.putProperty("user.home", "/home/dev")
            _ <- TestSystem.clearProperty("java.home")
            _ <- TestSystem.putEnv("HISTOS_NATIVE", "nope")
            _ <- TestSystem.putEnv("HISTOS_JDK_SETTLED", "true")
            _ <- TestSystem.clearEnv("XDG_CACHE_HOME")
            refused <- Machine.load(Path.of("/repo"))
            _ <- TestSystem.clearEnv("HISTOS_NATIVE")
            central <- Machine.load(Path.of("/repo"))
          yield assertTrue(
            loaded.native == NativeOrigin.Chosen(NativeSource.IvyLocal),
            loaded.settled == Settled.Yes,
            loaded.os == Os.Linux,
            loaded.architecture == histos.toolchain.Architecture.X86_64,
            loaded.host == HostPlatform.LinuxX86_64,
            loaded.userHome == "/home/dev",
            loaded.running.contains(Path.of("/jdks/21")),
            loaded.cache == ContentStore.home(Some("/xdg"), "/home/dev"),
            loaded.repo == Path.of("/repo"),
            loaded.env.get("HISTOS_NATIVE").contains("ivy-local"),
            missing == Left(ToolchainError.NoUserHome),
            blank == Left(ToolchainError.NoUserHome),
            refused.native == NativeOrigin.Refused(
              NativeToolchainError.Origin("nope")
            ),
            refused.settled == Settled.No,
            refused.running.isEmpty,
            central.native == NativeOrigin.Central,
            central.cache == ContentStore.home(None, "/home/dev")
          )
        body.ensuring(restore)
  )

  def spec =
    suite("jdk")(policy, versions, locks, stage, startup, handOff, config)
