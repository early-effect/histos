package histos.jdk

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

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
      assertTrue:
        Jdk.resolve(major21, view) match
          case Resolution.Selected(chosen, _) =>
            chosen.origin == Origin.JavaHome && chosen.home == Path.of("/jdks/21")
          case _ => false
    ,
    test("a wrong JAVA_HOME falls through to PATH"):
      val view = bare.copy(
        javaHome = Located.Found(home("/jdks/17", v17)),
        onPath = Located.Found(home("/jdks/21", v21)),
      )
      assertTrue:
        Jdk.resolve(major21, view) match
          case Resolution.Selected(chosen, _) => chosen.origin == Origin.Path
          case _                              => false
    ,
    test("an unreadable JAVA_HOME falls through to PATH"):
      val view = bare.copy(
        javaHome = Located.Found(JdkHome(Path.of("/bad"), hasJavac = true, version = Left(ReleaseError.NoFile))),
        onPath = Located.Found(home("/jdks/21", v21)),
      )
      assertTrue:
        Jdk.resolve(major21, view) match
          case Resolution.Selected(chosen, _) => chosen.origin == Origin.Path
          case _                              => false
    ,
    test("a present manager without the pin does not use the cache"):
      val view = bare.copy(
        coursier = ManagerSlot.Present(List(home("/cs/17", v17))),
        cached = List(home("/cache/21", v21)),
      )
      assertTrue:
        Jdk.resolve(major21, view) match
          case Resolution.Install(InstallHint.Coursier(major), _) => major == major21
          case _                                                  => false
    ,
    test("sdkman is used when coursier has no match"):
      val view = bare.copy(
        coursier = ManagerSlot.Present(Nil),
        sdkman = ManagerSlot.Present(List(home("/sdk/21", v21))),
        cached = List(home("/cache/17", v17)),
      )
      assertTrue:
        Jdk.resolve(major21, view) match
          case Resolution.Selected(chosen, _) => chosen.origin == Origin.Sdkman
          case _                              => false
    ,
    test("both managers without the pin ask for coursier, not the cache"):
      val view = bare.copy(
        coursier = ManagerSlot.Present(Nil),
        sdkman = ManagerSlot.Present(Nil),
        cached = List(home("/cache/21", v21)),
      )
      assertTrue:
        Jdk.resolve(major21, view) match
          case Resolution.Install(InstallHint.Coursier(major), trace) =>
            major == major21 &&
              trace.contains(Candidate.Empty(Origin.Coursier)) &&
              trace.contains(Candidate.Empty(Origin.Sdkman)) &&
              !trace.exists:
                case Candidate.Match(Origin.Cache, _, _) => true
                case _                                   => false
          case _ => false
    ,
    test("sdkman alone is told to list java"):
      val view = bare.copy(sdkman = ManagerSlot.Present(Nil))
      assertTrue:
        Jdk.resolve(major21, view) match
          case Resolution.Install(hint, _) => hint.command == "sdk list java"
          case _                           => false
    ,
    test("the cache is used only when no manager is installed"):
      val view = bare.copy(cached = List(home("/cache/21", v21)))
      assertTrue:
        Jdk.resolve(major21, view) match
          case Resolution.Selected(chosen, _) => chosen.origin == Origin.Cache
          case _                              => false
    ,
    test("a bare machine provisions"):
      assertTrue:
        Jdk.resolve(major21, bare) match
          case Resolution.Provision(major, _) => major == major21
          case _                              => false
    ,
    test("a wrong explicit override stops"):
      val view = bare.copy(
        explicit = Located.Found(home("/jdks/17", v17)),
        javaHome = Located.Found(home("/jdks/21", v21)),
      )
      assertTrue:
        Jdk.resolve(major21, view) match
          case Resolution.OverrideRejected(Reject.WrongMajor(found), _) => found == major17
          case _                                                        => false
    ,
    test("a missing explicit override stops"):
      val view = bare.copy(
        explicit = Located.Missing(Path.of("/missing")),
        javaHome = Located.Found(home("/jdks/21", v21)),
      )
      assertTrue:
        Jdk.resolve(major21, view) match
          case Resolution.OverrideRejected(Reject.Absent, _) => true
          case _                                             => false
    ,
  )

  private val versions = suite("versions")(
    test("a modern release file yields its major"):
      assertTrue:
        JdkVersion.fromRelease("JAVA_VERSION=\"21.0.2\"\n") match
          case Right(version) => version.value == "21.0.2" && version.major == major21
          case Left(_)        => false
    ,
    test("a 1.8 release file yields major 8"):
      assertTrue:
        JdkVersion.from("1.8.0_402") match
          case Right(version) => version.major == major8
          case Left(_)        => false
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
      assertTrue:
        JdkLock.parse(JdkLock.render(lock)) match
          case Right(found) =>
            found.major == major21 && found.version == v21 && found.source == ArchiveSource.Local(path) && found.sha256 == sha
          case Left(_) => false
    ,
    test("the major has to agree with the version"):
      val text = s"major 21\nversion 17.0.9\nurl file:///opt/a.zip\nsha256 ${"ab" * 32}\n"
      assertTrue:
        JdkLock.parse(text) match
          case Left(LockError.MajorDisagrees(declared, found)) => declared == major21 && found == major17
          case _                                               => false
    ,
    test("https is remote and http is rejected"):
      val sha  = "ab" * 32
      val http = s"major 21\nversion 21.0.2\nurl http://example.test/jdk.zip\nsha256 $sha\n"
      val https = s"major 21\nversion 21.0.2\nurl https://example.test/jdk.zip\nsha256 $sha\n"
      assertTrue:
        (JdkLock.parse(http), JdkLock.parse(https)) match
          case (Left(LockError.BadUrl(_)), Right(lock)) =>
            lock.source match
              case ArchiveSource.Remote(url) => url.value == "https://example.test/jdk.zip"
              case _                         => false
          case _ => false
    ,
    test("an unknown field is not dropped"):
      val text = s"major 21\nversion 21.0.2\nurl file:///opt/a.zip\nsha256 ${"ab" * 32}\nextra 1\n"
      assertTrue:
        JdkLock.parse(text) match
          case Left(LockError.UnknownField("extra")) => true
          case _                                     => false
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
          home <- JdkStage.stage(lock, root.resolve("jdks"))
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
          out  <- JdkStage.stage(lock, root.resolve("jdks")).either
        yield assertTrue:
          out match
            case Left(StageError.Checksum(expected, _)) =>
              expected == other && !Files.exists(root.resolve("jdks").resolve(other.value).resolve("archive"))
            case _ => false
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
          out  <- JdkStage.stage(lock, root.resolve("jdks")).either
        yield assertTrue:
          out match
            case Left(StageError.UnsafePath(_)) => !Files.exists(root.resolve("jdks").resolve(sha.value).resolve("evil"))
            case _                              => false
    ,
    test("a remote url is not fetched"):
      ZIO.scoped:
        for
          root <- temp
          url  <- ZIO.fromEither(HttpsUrl.from("https://example.test/jdk.zip"))
          sha  = Sha256.of(zio.Chunk.fromArray(Array[Byte](1)))
          lock = JdkLock(major21, v21, ArchiveSource.Remote(url), sha)
          out  <- JdkStage.stage(lock, root.resolve("jdks")).either
        yield assertTrue:
          out match
            case Left(StageError.Remote(found)) => found == url && !Files.exists(root.resolve("jdks"))
            case _                              => false
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
        yield assertTrue:
          Jdk.resolve(major21, view) match
            case Resolution.Provision(major, _) => major == major21
            case _                              => false
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
        yield assertTrue:
          Jdk.resolve(major21, view) match
            case Resolution.Selected(chosen, _) =>
              chosen.origin == Origin.JavaHome && chosen.version.value == "21.0.2"
            case _ => false
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
        yield assertTrue:
          out match
            case Startup.Refuse(report) =>
              report.contains("provision temurin 21") &&
                report.contains("https://example.test/jdk.zip") &&
                !Files.exists(root.resolve("cache").resolve("jdks"))
            case _ => false
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

  def spec = suite("jdk")(policy, versions, locks, stage, startup) @@ TestAspect.sequential
