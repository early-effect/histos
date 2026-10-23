package histos.jdk

import histos.testrun.{ProcessExit, TestEvent, TestProcess}

import java.nio.file.{Files, Path}
import java.nio.file.attribute.{PosixFilePermission, PosixFilePermissions}

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

import zio.*
import zio.test.*

object JdkSpec extends ZIOSpecDefault:
  private val major21 = JdkMajor.trusted(21)
  private val major17 = JdkMajor.trusted(17)
  private val major8  = JdkMajor.trusted(8)
  private val v21     = JdkVersion.trusted("21.0.2", major21)
  private val v17     = JdkVersion.trusted("17.0.9", major17)

  private def home(path: String, version: JdkVersion): JdkHome =
    JdkHome(Path.of(path), hasJavac = true, version = Right(version))

  private val bare =
    ProbeView(Located.Unset, Located.Unset, Located.Unset, ManagerSlot.Absent, ManagerSlot.Absent, Nil)

  private val policy = suite("resolution")(
    test("JAVA_HOME of the pinned major is chosen before PATH"):
      val view = bare.copy(
        javaHome = Located.Found(home("/jdks/21", v21)),
        onPath = Located.Found(home("/jdks/path", v21)),
      )
      assertTrue(
        Jdk.resolve(major21, view).is(_.subtype[Resolution.Selected]).chosen ==
          Candidate.Match(Origin.JavaHome, Path.of("/jdks/21"), v21)
      )
    ,
    test("a wrong JAVA_HOME falls through to PATH"):
      val view = bare.copy(
        javaHome = Located.Found(home("/jdks/17", v17)),
        onPath = Located.Found(home("/jdks/21", v21)),
      )
      assertTrue(
        Jdk.resolve(major21, view).is(_.subtype[Resolution.Selected]).chosen ==
          Candidate.Match(Origin.Path, Path.of("/jdks/21"), v21)
      )
    ,
    test("an unreadable JAVA_HOME falls through to PATH"):
      val view = bare.copy(
        javaHome = Located.Found(JdkHome(Path.of("/bad"), hasJavac = true, version = Left(ReleaseError.NoFile))),
        onPath = Located.Found(home("/jdks/21", v21)),
      )
      assertTrue(
        Jdk.resolve(major21, view).is(_.subtype[Resolution.Selected]).chosen ==
          Candidate.Match(Origin.Path, Path.of("/jdks/21"), v21)
      )
    ,
    test("a present manager without the pin does not use the cache"):
      val view = bare.copy(
        coursier = ManagerSlot.Present(List(home("/cs/17", v17))),
        cached = List(home("/cache/21", v21)),
      )
      assertTrue(
        Jdk.resolve(major21, view).is(_.subtype[Resolution.Install]).hint == InstallHint.Coursier(major21)
      )
    ,
    test("sdkman is used when coursier has no match"):
      val view = bare.copy(
        coursier = ManagerSlot.Present(Nil),
        sdkman = ManagerSlot.Present(List(home("/sdk/21", v21))),
        cached = List(home("/cache/17", v17)),
      )
      assertTrue(
        Jdk.resolve(major21, view).is(_.subtype[Resolution.Selected]).chosen ==
          Candidate.Match(Origin.Sdkman, Path.of("/sdk/21"), v21)
      )
    ,
    test("both managers without the pin ask for coursier, not the cache"):
      val view = bare.copy(
        coursier = ManagerSlot.Present(Nil),
        sdkman = ManagerSlot.Present(Nil),
        cached = List(home("/cache/21", v21)),
      )
      assertTrue(
        Jdk.resolve(major21, view) == Resolution.Install(
          InstallHint.Coursier(major21),
          List(
            Candidate.NotPresent(Origin.Override),
            Candidate.NotPresent(Origin.JavaHome),
            Candidate.NotPresent(Origin.Path),
            Candidate.Empty(Origin.Coursier),
            Candidate.Empty(Origin.Sdkman),
          ),
        )
      )
    ,
    test("sdkman alone is told to list java"):
      val view = bare.copy(sdkman = ManagerSlot.Present(Nil))
      assertTrue(Jdk.resolve(major21, view).is(_.subtype[Resolution.Install]).hint.command == "sdk list java")
    ,
    test("the cache is used only when no manager is installed"):
      val view = bare.copy(cached = List(home("/cache/21", v21)))
      assertTrue(
        Jdk.resolve(major21, view).is(_.subtype[Resolution.Selected]).chosen ==
          Candidate.Match(Origin.Cache, Path.of("/cache/21"), v21)
      )
    ,
    test("a bare machine provisions"):
      assertTrue(Jdk.resolve(major21, bare).is(_.subtype[Resolution.Provision]).major == major21)
    ,
    test("a wrong explicit override stops"):
      val view = bare.copy(
        explicit = Located.Found(home("/jdks/17", v17)),
        javaHome = Located.Found(home("/jdks/21", v21)),
      )
      assertTrue(
        Jdk.resolve(major21, view).is(_.subtype[Resolution.OverrideRejected]).reason == Reject.WrongMajor(major17)
      )
    ,
    test("a missing explicit override stops"):
      val view = bare.copy(
        explicit = Located.Missing(Path.of("/missing")),
        javaHome = Located.Found(home("/jdks/21", v21)),
      )
      assertTrue(Jdk.resolve(major21, view).is(_.subtype[Resolution.OverrideRejected]).reason == Reject.Absent)
    ,
  )

  private val versions = suite("versions")(
    test("a modern release file yields its major"):
      assertTrue(JdkVersion.fromRelease("JAVA_VERSION=\"21.0.2\"\n") == Right(v21))
    ,
    test("a 1.8 release file yields major 8"):
      assertTrue(JdkVersion.from("1.8.0_402").is(_.right).major == major8)
    ,
    test("a release file without the key is a typed miss"):
      assertTrue(JdkVersion.fromRelease("IMPLEMENTOR=\"Eclipse\"") == Left(ReleaseError.MissingKey))
    ,
    test("an unclosed quote is not a missing key"):
      assertTrue(JdkVersion.fromRelease("JAVA_VERSION=\"21") == Left(ReleaseError.Unclosed))
    ,
    test("words are not a version"):
      assertTrue(JdkVersion.from("nope") == Left(VersionError.Bad("nope")))
    ,
  )

  private val locks = suite("lock file")(
    test("a file url round-trips"):
      val path = Path.of("/opt/jdk.zip")
      val sha  = Sha256.of(zio.Chunk.fromArray(Array[Byte](1, 2, 3)))
      val lock = JdkLock(major21, v21, ArchiveSource.Local(path), sha)
      assertTrue(JdkLock.parse(JdkLock.render(lock)) == Right(lock))
    ,
    test("the major has to agree with the version"):
      val text = s"major 21\nversion 17.0.9\nurl file:///opt/a.zip\nsha256 ${"ab" * 32}\n"
      assertTrue(JdkLock.parse(text) == Left(LockError.MajorDisagrees(major21, major17)))
    ,
    test("https is remote and http is rejected"):
      val sha  = "ab" * 32
      val http = s"major 21\nversion 21.0.2\nurl http://example.test/jdk.zip\nsha256 $sha\n"
      val https = s"major 21\nversion 21.0.2\nurl https://example.test/jdk.zip\nsha256 $sha\n"
      ZIO.fromEither(HttpsUrl.from("https://example.test/jdk.zip")).map: url =>
        assertTrue(
          JdkLock.parse(http) == Left(LockError.BadUrl("http://example.test/jdk.zip")),
          JdkLock.parse(https).is(_.right).source == ArchiveSource.Remote(url),
        )
    ,
    test("an unknown field is not dropped"):
      val text = s"major 21\nversion 21.0.2\nurl file:///opt/a.zip\nsha256 ${"ab" * 32}\nextra 1\n"
      assertTrue(JdkLock.parse(text) == Left(LockError.UnknownField("extra")))
    ,
  )

  private val stage = suite("stage")(
    test("a local zip is verified and unpacked"):
      ZIO.scoped:
        for
          root <- temp
          zip  = jdkZip("21.0.2")
          file = root.resolve("jdk.zip")
          _    <- ZIO.attempt(Files.write(file, zip))
          lock = JdkLock(major21, v21, ArchiveSource.Local(file), Sha256.of(zio.Chunk.fromArray(zip)))
          home <- JdkStage.stage(lock, machine(root))
          java <- ZIO.attempt(Files.isRegularFile(home.path.resolve("bin").resolve("java")))
          versionOk = home.version.exists(version => version.value == v21.value && version.major == v21.major)
        yield assertTrue(home.hasJavac, versionOk, java)
    ,
    test("a checksum mismatch does not keep the archive"):
      ZIO.scoped:
        for
          root <- temp
          zip  = jdkZip("21.0.2")
          file = root.resolve("jdk.zip")
          _    <- ZIO.attempt(Files.write(file, zip))
          other = Sha256.of(zio.Chunk.fromArray(Array[Byte](9)))
          lock  = JdkLock(major21, v21, ArchiveSource.Local(file), other)
          out  <- JdkStage.stage(lock, machine(root)).either
        yield assertTrue(
          out == Left(StageError.Checksum(other, Sha256.of(zio.Chunk.fromArray(zip)))),
          !Files.exists(root.resolve("cache").resolve("jdks").resolve(other.value).resolve("archive")),
        )
    ,
    test("a zip entry that escapes the cache is rejected"):
      ZIO.scoped:
        for
          root <- temp
          zip  = zipOf("../evil" -> Array[Byte](1))
          file = root.resolve("slip.zip")
          _    <- ZIO.attempt(Files.write(file, zip))
          sha  = Sha256.of(zio.Chunk.fromArray(zip))
          lock = JdkLock(major21, v21, ArchiveSource.Local(file), sha)
          out  <- JdkStage.stage(lock, machine(root)).either
        yield assertTrue(
          out == Left(StageError.UnsafePath("../evil")),
          !Files.exists(root.resolve("cache").resolve("jdks").resolve(sha.value).resolve("evil")),
        )
    ,
    test("a remote url is not fetched"):
      ZIO.scoped:
        for
          root <- temp
          url  <- ZIO.fromEither(HttpsUrl.from("https://example.test/jdk.zip"))
          sha  = Sha256.of(zio.Chunk.fromArray(Array[Byte](1)))
          lock = JdkLock(major21, v21, ArchiveSource.Remote(url), sha)
          out  <- JdkStage.stage(lock, machine(root)).either
        yield assertTrue(out == Left(StageError.Remote(url)), !Files.exists(root.resolve("cache").resolve("jdks")))
    ,
    test("a local tar.gz keeps the exec bit, and its java runs"):
      ZIO.scoped:
        for
          root  <- temp
          env   <- System.envs
          tree  = root.resolve("tree")
          _     <- ZIO.attempt {
                     val home = tree.resolve("jdk-21.0.2+13")
                     Files.createDirectories(home.resolve("bin"))
                     Files.writeString(home.resolve("bin").resolve("java"), "#!/bin/sh\necho staged java\n")
                     Files.setPosixFilePermissions(home.resolve("bin").resolve("java"), PosixFilePermissions.fromString("rwxr-xr-x"))
                     Files.writeString(home.resolve("bin").resolve("javac"), "")
                     Files.writeString(home.resolve("release"), "JAVA_VERSION=\"21.0.2\"\n")
                     Files.createDirectories(root.resolve("pack"))
                     Files.createDirectories(root.resolve("run"))
                   }
          file  = root.resolve("jdk.tar.gz")
          packed <- TestProcess.command(List("tar", "-czf", file.toString, "-C", tree.toString, "jdk-21.0.2+13"), root.resolve("pack"), env).runCollect
          bytes <- ZIO.attempt(Files.readAllBytes(file))
          lock  = JdkLock(major21, v21, ArchiveSource.Local(file), Sha256.of(zio.Chunk.fromArray(bytes)))
          home  <- JdkStage.stage(lock, machine(root).copy(env = env))
          java  = home.path.resolve("bin").resolve("java")
          mode  <- ZIO.attempt(Files.getPosixFilePermissions(java).asScala.toSet)
          ran   <- TestProcess.command(List(java.toString), root.resolve("run"), Map.empty).runCollect
        yield assertTrue(
          packed.lastOption.contains(TestEvent.Exited(ProcessExit(0))),
          home.version.map(_.value) == Right("21.0.2"),
          mode.contains(PosixFilePermission.OWNER_EXECUTE),
          ran.toList == List(TestEvent.Out("staged java"), TestEvent.Exited(ProcessExit(0))),
        )
    @@ TestAspect.withLiveSystem
    ,
    test("a failed unpack leaves no partial directory and no temp file"):
      ZIO.scoped:
        for
          root <- temp
          zip  = zipOf("jdk/bin/javac" -> Array.emptyByteArray, "../evil" -> Array[Byte](1))
          file = root.resolve("slip.zip")
          _    <- ZIO.attempt(Files.write(file, zip))
          sha  = Sha256.of(zio.Chunk.fromArray(zip))
          out  <- JdkStage.stage(JdkLock(major21, v21, ArchiveSource.Local(file), sha), machine(root)).either
          left <- ZIO.attempt(names(root.resolve("cache").resolve("jdks").resolve(sha.value)))
        yield assertTrue(out == Left(StageError.UnsafePath("../evil")), left == List("archive"))
    ,
    test("a complete unpack already in place is kept"):
      ZIO.scoped:
        for
          root <- temp
          zip  = jdkZip("21.0.2")
          file = root.resolve("jdk.zip")
          _    <- ZIO.attempt(Files.write(file, zip))
          sha  = Sha256.of(zio.Chunk.fromArray(zip))
          kept = root.resolve("cache").resolve("jdks").resolve(sha.value).resolve("unpack").resolve("jdk")
          _    <- ZIO.attempt {
                    Files.createDirectories(kept.resolve("bin"))
                    Files.writeString(kept.resolve("bin").resolve("javac"), "")
                    Files.writeString(kept.resolve("release"), "JAVA_VERSION=\"21.0.2\"\n")
                    Files.writeString(kept.resolve("theirs"), "")
                  }
          home <- JdkStage.stage(JdkLock(major21, v21, ArchiveSource.Local(file), sha), machine(root))
          left <- ZIO.attempt(names(root.resolve("cache").resolve("jdks").resolve(sha.value)))
          mark <- ZIO.attempt(Files.isRegularFile(kept.resolve("theirs")))
        yield assertTrue(home.path == kept, mark, left == List("archive", "unpack"))
    ,
  )

  private val startup = suite("startup")(
    test("locations follow the os and the env"):
      assertTrue(
        JdkLocations.coursierCache("/Users/a", Os.Mac, Map.empty) == Path.of("/Users/a/Library/Caches/Coursier"),
        JdkLocations.coursierCache("/Users/a", Os.Linux, Map.empty) == Path.of("/Users/a/.cache/coursier"),
        JdkLocations.coursierCache("/Users/a", Os.Mac, Map("COURSIER_CACHE" -> "/opt/cs")) == Path.of("/opt/cs"),
        JdkLocations.sdkmanJava("/Users/a", Map("SDKMAN_DIR" -> "/opt/sdk")) == Path.of("/opt/sdk/candidates/java"),
      )
    ,
    test("an empty home does not see this machine's JDKs"):
      ZIO.scoped:
        for
          root <- temp
          view <- JdkLocations.probe(machine(root), None).flatMap(Jdk.scan)
        yield assertTrue(Jdk.resolve(major21, view).is(_.subtype[Resolution.Provision]).major == major21)
    ,
    test("a coursier cache without a JDK, and no cs on PATH, is not an installed manager"):
      ZIO.scoped:
        for
          root <- temp
          _    <- ZIO.attempt(Files.createDirectories(root.resolve(".cache").resolve("coursier").resolve("arc").resolve("https")))
          view <- JdkLocations.probe(machine(root), None).flatMap(Jdk.scan)
        yield assertTrue(
          view.coursier == ManagerSlot.Absent,
          Jdk.resolve(major21, view).is(_.subtype[Resolution.Provision]).major == major21,
        )
    ,
    test("cs on PATH, or a JDK in coursier's cache, is an installed manager"):
      ZIO.scoped:
        for
          root <- temp
          bin  = root.resolve("bin")
          jdk  = root.resolve(".cache").resolve("coursier").resolve("arc").resolve("https").resolve("jdk-17")
          _    <- ZIO.attempt {
                    Files.createDirectories(bin)
                    Files.writeString(bin.resolve("cs"), "")
                    Files.createDirectories(jdk.resolve("bin"))
                    Files.writeString(jdk.resolve("bin").resolve("javac"), "")
                    Files.writeString(jdk.resolve("release"), "JAVA_VERSION=\"17.0.9\"\n")
                  }
          onPath <- JdkLocations.probe(machine(root.resolve("elsewhere")).copy(env = Map("PATH" -> bin.toString)), None).flatMap(Jdk.scan)
          cached <- JdkLocations.probe(machine(root), None).flatMap(Jdk.scan)
        yield assertTrue(
          onPath.coursier == ManagerSlot.Present(Nil),
          cached.coursier == ManagerSlot.Present(List(JdkHome(jdk, hasJavac = true, version = Right(v17)))),
        )
    ,
    test("scan reads javac and the release file"):
      ZIO.scoped:
        for
          root <- temp
          jdk  = root.resolve("java-home")
          _    <- ZIO.attempt {
                    Files.createDirectories(jdk.resolve("bin"))
                    Files.writeString(jdk.resolve("bin").resolve("javac"), "")
                    Files.writeString(jdk.resolve("release"), "JAVA_VERSION=\"21.0.2\"\n")
                  }
          view <- Jdk.scan(ScanInput(None, Some(jdk), None, ManagerProbe.Absent, ManagerProbe.Absent, root.resolve("jdks")))
        yield assertTrue(
          Jdk.resolve(major21, view).is(_.subtype[Resolution.Selected]).chosen ==
            Candidate.Match(Origin.JavaHome, jdk, v21)
        )
    ,
    test("no lock continues on the current JVM"):
      ZIO.scoped:
        for
          root <- temp
          out  <- Toolchain.prepare(machine(root))
        yield assertTrue(out == Startup.Continue && !Files.exists(root.resolve("cache").resolve("jdks")))
    ,
    test("a bare machine stages a file url and plans a re-exec"):
      ZIO.scoped:
        for
          root <- temp
          zip  = jdkZip("21.0.2")
          file = root.resolve("jdk.zip")
          _    <- ZIO.attempt(Files.write(file, zip))
          _    <- writeLock(root, file, zip)
          out  <- Toolchain.prepare(machine(root))
          shown = out match
            case Startup.Replace(javaBin) =>
              val file = Files.isRegularFile(javaBin)
              val ends = javaBin.endsWith(Path.of("bin", "java"))
              if file && ends then "ok" else s"bad file=$file ends=$ends path=$javaBin"
            case other => other.toString
        yield assertTrue(shown == "ok")
    ,
    test("an https lock on a bare machine does not download"):
      ZIO.scoped:
        for
          root <- temp
          _    <- ZIO.attempt {
                    Files.createDirectories(root.resolve(".histos"))
                    Files.writeString(
                      root.resolve(".histos").resolve("jdk.lock"),
                      s"""major 21
                         |version 21.0.2
                         |url https://example.test/jdk.zip
                         |sha256 ${"ab" * 32}
                         |""".stripMargin,
                    )
                  }
          out <- Toolchain.prepare(machine(root))
        yield assertTrue(
          out.is(_.subtype[Startup.Refuse]).report.contains("provision temurin 21"),
          out.is(_.subtype[Startup.Refuse]).report.contains("https://example.test/jdk.zip"),
          !Files.exists(root.resolve("cache").resolve("jdks")),
        )
    ,
    test("a settled process does not re-exec"):
      ZIO.scoped:
        for
          root <- temp
          _    <- ZIO.attempt {
                    Files.createDirectories(root.resolve(".histos"))
                    Files.writeString(root.resolve(".histos").resolve("jdk.lock"), "not a lock")
                  }
          out  <- Toolchain.prepare(machine(root).copy(settled = Settled.Yes))
        yield assertTrue(out == Startup.Continue)
    ,
    test("re-exec keeps the jar and the arguments"):
      val command = Toolchain.command(Path.of("/jdks/21/bin/java"), Path.of("/app.jar"), List("run", "inspect"))
      assertTrue(command == List("/jdks/21/bin/java", Toolchain.NativeAccess, "-jar", "/app.jar", "run", "inspect"))
    ,
    test("the re-exec guard is the digit 1"):
      assertTrue(Settled.from(Map("HISTOS_JDK_SETTLED" -> "1")) == Settled.Yes && Settled.from(Map("HISTOS_JDK_SETTLED" -> "true")) == Settled.No)
    ,
  )

  private val handOff = suite("hand-off")(
    test("the hand-off returns the child's status and marks the child settled"):
      ZIO.scoped:
        for
          root    <- temp
          stub    <- script(root, s"echo \"$$HISTOS_JDK_SETTLED\" > '$root/settled'\nexit 7\n")
          code    <- Toolchain.replace(stub, root.resolve("app.jar"), Nil)
          settled <- ZIO.attempt(Files.readString(root.resolve("settled")).trim)
        yield assertTrue(code == 7, settled == "1")
    ,
    test("interrupting the hand-off kills the child and what it started"):
      ZIO.scoped:
        for
          root   <- temp
          stub   <- script(root, s"echo $$$$ > '$root/leader.pid'\nsleep 300 &\necho $$! > '$root/child.pid'\nwait\n")
          fiber  <- Toolchain.replace(stub, root.resolve("app.jar"), Nil).fork
          leader <- awaitPid(root.resolve("leader.pid"))
          child  <- awaitPid(root.resolve("child.pid"))
          exit   <- fiber.interrupt
          dead   <- ZIO.foreach(leader.toList ++ child.toList)(awaitDead)
        yield assertTrue(exit.isInterrupted, leader.nonEmpty, child.nonEmpty, dead == List(true, true))
    ,
    test("an interrupted hand-off lets the child handle SIGTERM before anything is killed"):
      ZIO.scoped:
        for
          root   <- temp
          stub   <- script(root, trapsTerm(root))
          fiber  <- Toolchain.replace(stub, root.resolve("app.jar"), Nil).fork
          leader <- awaitPid(root.resolve("leader.pid"))
          child  <- awaitPid(root.resolve("child.pid"))
          _      <- fiber.interrupt
          status <- ZIO.attempt(Option.when(Files.isRegularFile(root.resolve("status")))(Files.readString(root.resolve("status")).trim))
        yield assertTrue(leader.nonEmpty, child.nonEmpty, status.contains("130"))
  ) @@ TestAspect.withLiveClock

  /**
   * The child traps TERM and exits 130. The leader ignores TERM, which the child must not inherit,
   * so it starts the child first, then records the child's real exit status.
   */
  private def trapsTerm(root: Path): String =
    s"""sh -c 'trap "exit 130" TERM; echo $$$$ > "$$0"; while :; do sleep 0.2; done' '$root/child.pid' 2>/dev/null &
       |trap '' TERM
       |echo $$$$ > '$root/leader.pid'
       |wait $$!
       |echo $$? > '$root/status'
       |""".stripMargin

  /** A stand-in for `java`: the hand-off runs whatever executable it is given. */
  private def script(root: Path, body: String): Task[Path] =
    ZIO.attempt:
      val path = root.resolve("java")
      Files.writeString(path, s"#!/bin/sh\n$body")
      Files.setPosixFilePermissions(path, java.nio.file.attribute.PosixFilePermissions.fromString("rwx------"))

  private def awaitPid(path: Path): UIO[Option[Long]] =
    ZIO.suspendSucceed {
      readPid(path) match
        case Some(pid) => ZIO.succeed(Some(pid))
        case None      => ZIO.sleep(20.millis) *> awaitPid(path)
    }.timeout(2.seconds).map(_.flatten)

  private def readPid(path: Path): Option[Long] =
    if !Files.isRegularFile(path) then None
    else
      try Files.readString(path).trim.toLongOption.filter(_ > 0)
      catch case _: java.io.IOException => None

  private def awaitDead(pid: Long): UIO[Boolean] =
    ZIO.suspendSucceed {
      if ProcessHandle.of(pid).toScala.exists(_.isAlive) then ZIO.sleep(20.millis) *> awaitDead(pid) else ZIO.succeed(true)
    }.timeout(1.second).map(_.getOrElse(false))

  private def machine(root: Path): Machine =
    Machine(Map.empty, root.toString, Os.Linux, root.resolve("cache"), root, None, Settled.No)

  private def writeLock(root: Path, archive: Path, bytes: Array[Byte]): zio.Task[Unit] =
    ZIO.attempt {
      Files.createDirectories(root.resolve(".histos"))
      val lock = JdkLock(major21, v21, ArchiveSource.Local(archive), Sha256.of(zio.Chunk.fromArray(bytes)))
      Files.writeString(root.resolve(".histos").resolve("jdk.lock"), JdkLock.render(lock))
    }

  private def jdkZip(version: String): Array[Byte] =
    val release = s"JAVA_VERSION=\"$version\"\n".getBytes(java.nio.charset.StandardCharsets.UTF_8)
    zipOf("jdk/bin/javac" -> Array.emptyByteArray, "jdk/bin/java" -> Array.emptyByteArray, "jdk/release" -> release)

  private def zipOf(entries: (String, Array[Byte])*): Array[Byte] =
    val buffer = new java.io.ByteArrayOutputStream
    val output = new java.util.zip.ZipOutputStream(buffer)
    val crc    = new java.util.zip.CRC32
    entries.foreach: (name, body) =>
      crc.reset()
      crc.update(body)
      val entry = new java.util.zip.ZipEntry(name)
      entry.setMethod(java.util.zip.ZipEntry.STORED)
      entry.setSize(body.length.toLong)
      entry.setCrc(crc.getValue)
      output.putNextEntry(entry)
      output.write(body)
      output.closeEntry()
    output.close()
    buffer.toByteArray

  private def names(dir: Path): List[String] =
    if !Files.isDirectory(dir) then Nil
    else
      val listed = Files.list(dir)
      try listed.iterator.asScala.flatMap(path => Option(path.getFileName).map(_.toString)).toList.sorted
      finally listed.close()

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-jdk"))): dir =>
      delete(dir)

  private def delete(dir: Path): UIO[Unit] =
    ZIO.attempt {
      if Files.exists(dir) then
        val walk = Files.walk(dir)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.ignore

  def spec = suite("jdk")(policy, versions, locks, stage, startup, handOff)
