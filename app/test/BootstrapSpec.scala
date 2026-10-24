package histos.cli

import histos.jdk.Sha256
import histos.process.{ChildError, ChildEvent, ChildProcess, ProcessExit}
import histos.publish.LauncherScript
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.*
import zio.test.*

object BootstrapSpec extends ZIOSpecDefault:
  private val version = "0.0.1-SNAPSHOT"
  private val revision = "0.0.1-20261023.224241-14"
  private val bytes = "fixture launcher".getBytes(StandardCharsets.UTF_8)
  private val sha = Sha256.of(Chunk.fromArray(bytes)).value

  private enum FixtureError:
    case Io(error: IoFailure)
    case Process(error: ChildError)
    case Exit

  private final case class Fixture(root: Path, bin: Path, jar: Path):
    def cache: Path = root.resolve("xdg/histos")
    def remote: Path = cache.resolve(s"launchers/$revision/$sha/histos.jar")
    def downloads: Path = root.resolve("downloads.log")

  private final case class Ran(
      exit: Option[ProcessExit],
      output: String,
      arguments: List[String]
  )

  private def io[A](path: Path, fault: IoFault)(
      body: => A
  ): IO[FixtureError, A] =
    ZIO
      .attemptBlocking(body)
      .mapError(_ => FixtureError.Io(IoFailure(path, fault)))

  private def write(path: Path, text: String): IO[FixtureError, Unit] =
    Workspace.writeString(path, text).mapError(FixtureError.Io(_))

  private def read(path: Path): IO[FixtureError, String] =
    Workspace
      .readIfPresent(path)
      .mapError(FixtureError.Io(_))
      .map(_.getOrElse(""))

  private def layout: ZIO[Scope, FixtureError, Fixture] =
    for
      root <- Workspace.work("histos-boot").mapError(FixtureError.Io(_))
      bin <- Workspace
        .createDirectories(root.resolve("bin"))
        .mapError(FixtureError.Io(_))
      _ <- write(root.resolve("histos"), LauncherScript.posix(version))
      _ <- write(root.resolve(".histos/assembly.sha256"), sha + "\n")
      _ <- write(
        root.resolve(".histos/launcher.lock"),
        s"repository http://127.0.0.1:1234/repo\nversion $version\nrevision $revision\nsha256 $sha\n"
      )
      jar <- io(root, IoFault.Write)(
        Files.write(root.resolve("app.jar"), bytes)
      )
      _ <- write(root.resolve("app.jar.sha256"), sha + "\n")
      _ <- executable(
        bin.resolve("java"),
        "#!/bin/sh\nprintf '%s\\n' \"$@\" > \"$HISTOS_JAVA_LOG\"\n"
      )
      _ <- executable(bin.resolve("curl"), downloader)
      _ <- executable(
        bin.resolve("uname"),
        """#!/bin/sh
          |case $1 in
          |  -s) printf '%s\n' "${HISTOS_TEST_OS:-Darwin}" ;;
          |  -m) printf '%s\n' "${HISTOS_TEST_CPU:-arm64}" ;;
          |  *) exit 1 ;;
          |esac
          |""".stripMargin
      )
      _ <- io(bin, IoFault.Create):
        List(
          "/usr/bin/stat",
          "/usr/bin/id",
          "/usr/bin/readlink",
          "/usr/bin/sort",
          "/usr/bin/dirname",
          "/usr/bin/tr",
          "/usr/bin/awk",
          "/usr/bin/shasum",
          "/usr/bin/sha256sum",
          "/usr/bin/tar",
          "/usr/bin/find",
          "/usr/bin/mktemp",
          "/bin/mkdir",
          "/bin/chmod",
          "/bin/rm",
          "/bin/mv",
          "/bin/cat",
          "/bin/cp",
          "/usr/bin/gzip",
          "/bin/gzip"
        ).foreach: source =>
          val path = Path.of(source)
          val target = bin.resolve(path.getFileName)
          if Files.isRegularFile(path) && !Files.exists(target) then
            Files.createSymbolicLink(target, path)
    yield Fixture(root, bin, jar)

  private def executable(path: Path, text: String): IO[FixtureError, Unit] =
    write(path, text) *> io(path, IoFault.Write)(
      path.toFile.setExecutable(true)
    ).unit

  private val downloader =
    """#!/bin/sh
      |set -eu
      |url=; out=
      |while [ "$#" -gt 0 ]; do
      |  case $1 in -o) out=$2; shift 2 ;; --proto|--proto-redir) shift 2 ;; -*) shift ;; *) url=$1; shift ;; esac
      |done
      |printf '%s\n' "$url" >> "$HISTOS_DOWNLOAD_LOG"
      |if [ "${HISTOS_FETCH_FAIL:-}" = yes ]; then printf partial > "$out"; exit 1; fi
      |case $url in
      |  *maven-metadata.xml) echo 'startup must not read metadata' >&2; exit 2 ;;
      |  *.jar.sha256) cp "$HISTOS_DOWNLOAD.sha256" "$out" ;;
      |  */histos-0.0.1-20261023.224241-14.jar) cp "$HISTOS_DOWNLOAD" "$out" ;;
      |  file:///*) cp "${url#file://}" "$out" ;;
      |  *) echo 'unexpected download' >&2; exit 3 ;;
      |esac
      |""".stripMargin

  private def run(
      fixture: Fixture,
      args: List[String] = Nil,
      extra: Map[String, String] = Map.empty
  ): IO[FixtureError, Ran] =
    for
      work <- io(fixture.root, IoFault.Temp)(
        Files.createTempDirectory(fixture.root, "child-")
      )
      log = work.resolve("java.log")
      env = Map(
        "HISTOS_JAVA_LOG" -> log.toString,
        "HISTOS_DOWNLOAD" -> fixture.jar.toString,
        "HISTOS_DOWNLOAD_LOG" -> fixture.downloads.toString,
        "XDG_CACHE_HOME" -> fixture.root.resolve("xdg").toString,
        "HOME" -> fixture.root.toString,
        "PATH" -> fixture.bin.toString
      ) ++ extra
      events <- ChildProcess
        .command(
          List("/bin/sh", fixture.root.resolve("histos").toString) ++ args,
          work,
          env
        )
        .runCollect
        .mapError(FixtureError.Process(_))
      arguments <- read(log).map(_.linesIterator.toList)
    yield Ran(
      events.collect { case ChildEvent.Exited(code) => code }.lastOption,
      events
        .collect {
          case ChildEvent.Out(text) => text; case ChildEvent.Err(text) => text
        }
        .mkString("\n"),
      arguments
    )

  private def source(fixture: Fixture): Map[String, String] = Map(
    "HISTOS_ASSEMBLY" -> fixture.jar.toString
  )

  private def leftovers(fixture: Fixture): IO[FixtureError, List[Path]] =
    io(fixture.cache, IoFault.Walk):
      if !Files.isDirectory(fixture.cache) then Nil
      else
        val walk = Files.walk(fixture.cache)
        try
          walk.iterator.asScala
            .filter(path =>
              path.getFileName.toString.startsWith(
                ".bootstrap."
              ) || path.getFileName.toString.endsWith(".partial")
            )
            .toList
        finally walk.close()

  private def lockedJdk(fixture: Fixture): IO[FixtureError, Sha256] =
    for
      _ <- executable(
        fixture.root.resolve("fake-jdk/bin/java"),
        "#!/bin/sh\nprintf '%s\\n' \"locked-jdk\" \"$0\" \"$@\" > \"$HISTOS_JAVA_LOG\"\n"
      )
      _ <- write(
        fixture.root.resolve("fake-jdk/lib/libjvm.so"),
        "locked library"
      )
      _ <- write(fixture.root.resolve("fake-jdk/lib/modules"), "locked image")
      _ <- write(
        fixture.root.resolve("fake-jdk/release"),
        "JAVA_VERSION=\"21.0.2\"\n"
      )
      _ <- io(fixture.root, IoFault.Create):
        val link = fixture.root.resolve("fake-jdk/lib/current")
        if !Files.exists(link) then
          Files.createSymbolicLink(link, Path.of("libjvm.so"))
      .unit
      work <- Workspace
        .createDirectories(fixture.root.resolve("pack"))
        .mapError(FixtureError.Io(_))
      archive = fixture.root.resolve("jdk.tgz")
      events <- ChildProcess
        .command(
          List(
            "/usr/bin/tar",
            "-czf",
            archive.toString,
            "-C",
            fixture.root.resolve("fake-jdk").toString,
            "."
          ),
          work,
          Map("PATH" -> fixture.bin.toString)
        )
        .runCollect
        .mapError(FixtureError.Process(_))
      _ <- ZIO
        .fail(FixtureError.Exit)
        .unless(events.lastOption.contains(ChildEvent.Exited(ProcessExit(0))))
      data <- io(archive, IoFault.Read)(Files.readAllBytes(archive))
      sha = Sha256.of(Chunk.fromArray(data))
      _ <- write(
        fixture.root.resolve(".histos/jdk.lock"),
        s"major 21\nversion 21.0.2\narchive windows-x86_64 file:///unrelated/windows.zip ${"0" * 64}\narchive macos-aarch64 ${archive.toUri.toASCIIString} ${sha.value}\narchive linux-x86_64 file:///unrelated/linux.tgz ${"1" * 64}\n"
      )
    yield sha

  def spec = suite("bootstrap script")(
    test(
      "source assemblies use their own digest and preserve requested arguments"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          ran <- run(
            fixture,
            List("graph", "a b", "", "literal;$value"),
            source(fixture)
          )
          temps <- leftovers(fixture)
        yield assertTrue(
          ran.exit.contains(ProcessExit(0)),
          ran.arguments == List(
            "--enable-native-access=ALL-UNNAMED",
            "-jar",
            fixture.jar.toString,
            "graph",
            "a b",
            "",
            "literal;$value"
          ),
          temps.isEmpty
        )
    ,
    test("source digest mismatch refuses execution"):
      ZIO.scoped:
        for
          fixture <- layout
          _ <- write(fixture.root.resolve(".histos/assembly.sha256"), "0" * 64)
          ran <- run(fixture, extra = source(fixture))
          temps <- leftovers(fixture)
        yield assertTrue(
          ran.exit.contains(ProcessExit(1)),
          ran.output.contains("checksum mismatch"),
          ran.arguments.isEmpty,
          temps.isEmpty
        )
    ,
    test("missing java without a lock preserves the no-lock bootstrap error"):
      ZIO.scoped:
        for
          fixture <- layout
          _ <- Workspace
            .delete(fixture.bin.resolve("java"))
            .mapError(FixtureError.Io(_))
          ran <- run(fixture, extra = source(fixture))
        yield assertTrue(
          ran.exit.contains(ProcessExit(1)),
          ran.output.contains("java is not on PATH"),
          ran.output.contains("jdk.lock"),
          ran.arguments.isEmpty
        )
    ,
    test(
      "the immutable remote launcher is downloaded without metadata or a sidecar"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          ran <- run(fixture, List("graph"))
          downloads <- read(fixture.downloads)
          temps <- leftovers(fixture)
        yield assertTrue(
          ran.exit.contains(ProcessExit(0)),
          ran.arguments.contains(fixture.remote.toString),
          ran.arguments.contains("graph"),
          downloads.linesIterator.toList == List(
            s"http://127.0.0.1:1234/repo/rocks/earlyeffect/histos/$version/histos-$revision.jar"
          ),
          temps.isEmpty
        )
    ,
    test(
      "a downloaded jar that agrees with a remote sidecar still cannot evade the committed pin"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          _ <- write(
            fixture.jar,
            "wrong bytes whose remote sidecar would agree"
          )
          wrongSha <- io(fixture.jar, IoFault.Read)(
            Sha256.of(Chunk.fromArray(Files.readAllBytes(fixture.jar))).value
          )
          _ <- write(fixture.root.resolve("app.jar.sha256"), wrongSha + "\n")
          ran <- run(fixture)
          exists <- io(fixture.remote, IoFault.Stat)(
            Files.exists(fixture.remote)
          )
          temps <- leftovers(fixture)
        yield assertTrue(
          ran.exit.contains(ProcessExit(1)),
          ran.output.contains("checksum mismatch"),
          ran.arguments.isEmpty,
          !exists,
          temps.isEmpty
        )
    ,
    test(
      "warm remote startup is offline and a corrupt cached jar is revalidated"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          first <- run(fixture)
          warm <- run(fixture, extra = Map("HISTOS_FETCH_FAIL" -> "yes"))
          _ <- write(fixture.remote, "corrupt cache")
          corrupt <- run(fixture, extra = Map("HISTOS_FETCH_FAIL" -> "yes"))
          repaired <- run(fixture)
          downloaded <- read(fixture.downloads)
          actual <- io(fixture.remote, IoFault.Read)(
            Sha256.of(Chunk.fromArray(Files.readAllBytes(fixture.remote))).value
          )
        yield assertTrue(
          first.exit.contains(ProcessExit(0)),
          warm.exit.contains(ProcessExit(0)),
          corrupt.exit.contains(ProcessExit(1)),
          corrupt.arguments.isEmpty,
          repaired.exit.contains(ProcessExit(0)),
          actual == sha,
          downloaded.linesIterator.size == 3
        )
    ,
    test(
      "partial failed downloads never become a cache hit and leave no temporary files"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          failed <- run(fixture, extra = Map("HISTOS_FETCH_FAIL" -> "yes"))
          exists <- io(fixture.remote, IoFault.Stat)(
            Files.exists(fixture.remote)
          )
          temps <- leftovers(fixture)
          recovered <- run(fixture)
        yield assertTrue(
          failed.exit.contains(ProcessExit(1)),
          !exists,
          temps.isEmpty,
          recovered.exit.contains(ProcessExit(0))
        )
    ,
    test(
      "concurrent cold launches use independent staging and preserve arguments"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          runs <- ZIO.foreachPar(List("first", "second", "third"))(arg =>
            run(fixture, List(arg)).map(arg -> _)
          )
          temps <- leftovers(fixture)
          actual <- io(fixture.remote, IoFault.Read)(
            Sha256.of(Chunk.fromArray(Files.readAllBytes(fixture.remote))).value
          )
        yield assertTrue(
          runs.forall((arg, ran) =>
            ran.exit.contains(ProcessExit(0)) && ran.arguments.lastOption
              .contains(arg)
          ),
          actual == sha,
          temps.isEmpty
        )
    ,
    test(
      "a locked archive is used instead of unrelated java on PATH and is warm offline"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          jdkSha <- lockedJdk(fixture)
          first <- run(fixture, List("graph"), source(fixture))
          warm <- run(
            fixture,
            extra = source(fixture) + ("HISTOS_FETCH_FAIL" -> "yes")
          )
          _ <- write(
            fixture.cache.resolve(s"jdks/${jdkSha.value}/archive"),
            "corrupt archive"
          )
          corrupt <- run(
            fixture,
            extra = source(fixture) + ("HISTOS_FETCH_FAIL" -> "yes")
          )
          repaired <- run(fixture, extra = source(fixture))
          temps <- leftovers(fixture)
        yield assertTrue(
          first.exit.contains(ProcessExit(0)),
          first.arguments.headOption.contains("locked-jdk"),
          warm.exit.contains(ProcessExit(0)),
          warm.arguments.headOption.contains("locked-jdk"),
          corrupt.exit.contains(ProcessExit(1)),
          corrupt.arguments.isEmpty,
          repaired.exit.contains(ProcessExit(0)),
          temps.isEmpty
        )
    ,
    test(
      "concurrent locked JDK installs are complete, not nested inside a racing destination"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          _ <- lockedJdk(fixture)
          runs <- ZIO.foreachPar(List("a", "b"))(arg =>
            run(fixture, List(arg), source(fixture))
          )
          temps <- leftovers(fixture)
        yield assertTrue(
          runs.forall(ran =>
            ran.exit.contains(ProcessExit(0)) && ran.arguments.headOption
              .contains("locked-jdk")
          ),
          temps.isEmpty
        )
    ,
    test("an updater-staged launcher is reusable offline and still rehashed"):
      ZIO.scoped:
        for
          fixture <- layout
          staged = fixture.root.resolve(
            s".histos/launcher-cache/$sha/histos.jar"
          )
          _ <- write(staged, new String(bytes, StandardCharsets.UTF_8))
          warm <- run(fixture, extra = Map("HISTOS_FETCH_FAIL" -> "yes"))
          _ <- write(staged, "corrupt staged jar")
          corrupt <- run(fixture, extra = Map("HISTOS_FETCH_FAIL" -> "yes"))
        yield assertTrue(
          warm.exit.contains(ProcessExit(0)),
          warm.arguments.contains(staged.toString),
          corrupt.exit.contains(ProcessExit(1)),
          corrupt.arguments.isEmpty
        )
    ,
    test(
      "unsafe archive links never escape staging or become an installed JDK"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          _ <- Workspace
            .createDirectories(fixture.root.resolve("fake-jdk"))
            .mapError(FixtureError.Io(_))
          _ <- io(fixture.root, IoFault.Create)(
            Files.createSymbolicLink(
              fixture.root.resolve("fake-jdk/escape"),
              fixture.root.resolve("outside")
            )
          )
          _ <- lockedJdk(fixture)
          refused <- run(fixture, extra = source(fixture))
          outside <- io(fixture.root, IoFault.Stat)(
            Files.exists(fixture.root.resolve("outside"))
          )
          temps <- leftovers(fixture)
        yield assertTrue(
          refused.exit.contains(ProcessExit(1)),
          refused.output.contains("unsafe jdk archive"),
          refused.arguments.isEmpty,
          !outside,
          temps.isEmpty
        )
    ,
    test(
      "a modified installed Java is repaired from the verified archive without a download"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          _ <- lockedJdk(fixture)
          first <- run(fixture, extra = source(fixture))
          files <- io(fixture.cache, IoFault.Walk):
            val walk = Files.walk(fixture.cache)
            try
              walk.iterator.asScala
                .filter(path => path.endsWith(Path.of("bin/java")))
                .toList
            finally walk.close()
          binary <- ZIO
            .fromOption(files.find(Files.isRegularFile(_)))
            .orElseFail(FixtureError.Exit)
          _ <- write(binary, "corrupt installed executable")
          repaired <- run(
            fixture,
            extra = source(fixture) + ("HISTOS_FETCH_FAIL" -> "yes")
          )
          temps <- leftovers(fixture)
        yield assertTrue(
          first.exit.contains(ProcessExit(0)),
          repaired.exit.contains(ProcessExit(0)),
          repaired.arguments.headOption.contains("locked-jdk"),
          temps.isEmpty
        )
    ,
    test(
      "warm integrity repairs changed libraries, image, release, contained links and permissions before Java"
    ):
      ZIO.scoped:
        ZIO
          .foreach(
            List(
              "library",
              "image",
              "release",
              "link",
              "mode",
              "added",
              "removed",
              "directory-mode"
            )
          ): mutation =>
            for
              fixture <- layout
              _ <- lockedJdk(fixture)
              first <- run(fixture, extra = source(fixture))
              binary <- ZIO
                .fromOption(first.arguments.lift(1))
                .orElseFail(FixtureError.Exit)
              home = Path.of(binary).resolve("../..").normalize
              _ <- mutation match
                case "library" =>
                  write(home.resolve("lib/libjvm.so"), "changed library")
                case "image" =>
                  write(home.resolve("lib/modules"), "changed image")
                case "release" =>
                  write(home.resolve("release"), "changed release")
                case "link" =>
                  io(home, IoFault.Write):
                    Files.delete(home.resolve("lib/current"))
                    Files.createSymbolicLink(
                      home.resolve("lib/current"),
                      Path.of("modules")
                    )
                  .unit
                case "added" =>
                  write(home.resolve("lib/extra-code"), "unexpected content")
                case "removed" =>
                  Workspace
                    .delete(home.resolve("release"))
                    .mapError(FixtureError.Io(_))
                case "directory-mode" =>
                  io(home, IoFault.Write)(
                    Files.setPosixFilePermissions(
                      home.resolve("lib"),
                      java.nio.file.attribute.PosixFilePermissions
                        .fromString("r-x------")
                    )
                  ).unit
                case _ =>
                  io(home, IoFault.Write)(
                    home.resolve("lib/libjvm.so").toFile.setExecutable(true)
                  ).unit
              repaired <- run(
                fixture,
                extra = source(fixture) + ("HISTOS_FETCH_FAIL" -> "yes")
              )
              chosen <- ZIO
                .fromOption(repaired.arguments.lift(1))
                .orElseFail(FixtureError.Exit)
              library <- read(
                Path.of(chosen).resolve("../../lib/libjvm.so").normalize
              )
              temps <- leftovers(fixture)
            yield assertTrue(
              first.exit.contains(ProcessExit(0)),
              repaired.exit.contains(ProcessExit(0)),
              chosen != binary,
              library == "locked library",
              temps.isEmpty
            )
          .map(_.foldLeft(assertTrue(true))(_ && _))
    ,
    test(
      "composed escaping symlinks are rejected before installation while ordinary contained links work"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          _ <- Workspace
            .createDirectories(fixture.root.resolve("fake-jdk/d/a"))
            .mapError(FixtureError.Io(_))
          _ <- io(fixture.root, IoFault.Create):
            Files.createSymbolicLink(
              fixture.root.resolve("fake-jdk/d/a/up"),
              Path.of("..")
            )
            Files.createSymbolicLink(
              fixture.root.resolve("fake-jdk/escape"),
              Path.of("d/a/up/../..")
            )
          _ <- lockedJdk(fixture)
          refused <- run(fixture, extra = source(fixture))
          temps <- leftovers(fixture)
        yield assertTrue(
          refused.exit.contains(ProcessExit(1)),
          refused.arguments.isEmpty,
          refused.output.contains("unsafe jdk archive link graph"),
          temps.isEmpty
        )
    ,
    test(
      "archive graph validation resolves components before parents and rejects hardlink traversal and cycles"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          template = LauncherScript.posix(version)
          start = template.indexOf("validate_graph() {")
          end = template.indexOf("runtime_hash()", start)
          _ <- ZIO.fail(FixtureError.Exit).unless(start >= 0 && end > start)
          checker =
            "#!/bin/sh\nset -eu\nfail() { echo \"$*\" >&2; exit 1; }\n" + template
              .substring(start, end) + "validate_graph \"$1\"\n"
          script = fixture.root.resolve("graph.sh")
          _ <- write(script, checker)
          results <- ZIO.foreach(
            List(
              ("-\tfile\n" + "h\talias\tfile\n", 0),
              ("-\tfile\n" + "l\talias\tfile\n", 0),
              (
                "d\td\nd\td/a\nl\td/a/up\t..\nh\talias\td/a/up/../../file\n-\tfile\n",
                1
              ),
              ("l\ta\tb\nl\tb\ta\n", 1),
              ("-\t./file\n-\tfile\n", 1),
              ("d\td\nd\td/a\nl\td/a/up\t..\nl\tescape\td/a/up/../..\n", 1)
            )
          ): (graph, expected) =>
            for
              path <- io(fixture.root, IoFault.Temp)(
                Files.createTempFile(fixture.root, "graph-", ".tsv")
              )
              _ <- write(path, graph)
              work <- io(fixture.root, IoFault.Temp)(
                Files.createTempDirectory(fixture.root, "graph-run-")
              )
              events <- ChildProcess
                .command(
                  List("/bin/sh", script.toString, path.toString),
                  work,
                  Map("PATH" -> fixture.bin.toString)
                )
                .runCollect
                .mapError(FixtureError.Process(_))
            yield assertTrue(
              events.lastOption.contains(
                ChildEvent.Exited(ProcessExit(expected))
              )
            )
        yield results.foldLeft(assertTrue(true))(_ && _)
    ,
    test(
      "directory, dangling and unreadable lock/override paths never activate PATH trust"
    ):
      ZIO.scoped:
        ZIO
          .foreach(List("jdk.lock", "toolchain.local")): name =>
            for
              fixture <- layout
              path = fixture.root.resolve(".histos").resolve(name)
              _ <- Workspace
                .createDirectories(path)
                .mapError(FixtureError.Io(_))
              directory <- run(fixture, extra = source(fixture))
              _ <- Workspace.delete(path).mapError(FixtureError.Io(_))
              _ <- io(path, IoFault.Create)(
                Files.createSymbolicLink(path, fixture.root.resolve("missing"))
              )
              dangling <- run(fixture, extra = source(fixture))
              _ <- Workspace.delete(path).mapError(FixtureError.Io(_))
              _ <- write(path, "unreadable")
              _ <- io(path, IoFault.Write)(
                Files.setPosixFilePermissions(
                  path,
                  Set.empty[java.nio.file.attribute.PosixFilePermission].asJava
                )
              )
              unreadable <- run(fixture, extra = source(fixture))
              _ <- io(path, IoFault.Write)(path.toFile.setReadable(true))
            yield assertTrue(
              List(directory, dangling, unreadable).forall(ran =>
                ran.exit.contains(ProcessExit(1)) && ran.arguments.isEmpty
              )
            )
          .map(_.foldLeft(assertTrue(true))(_ && _))
    ,
    test("a shared writable cache is refused before downloads or Java"):
      ZIO.scoped:
        for
          fixture <- layout
          _ <- Workspace
            .createDirectories(fixture.cache)
            .mapError(FixtureError.Io(_))
          _ <- io(fixture.cache, IoFault.Write)(
            Files.setPosixFilePermissions(
              fixture.cache,
              java.nio.file.attribute.PosixFilePermissions
                .fromString("rwxrwxrwx")
            )
          )
          refused <- run(fixture, extra = source(fixture))
          requests <- read(fixture.downloads)
        yield assertTrue(
          refused.exit.contains(ProcessExit(1)),
          refused.arguments.isEmpty,
          requests.isEmpty,
          refused.output.contains("cache is not owner-private")
        )
    ,
    test(
      "a writable non-sticky cache ancestor is refused even with a private leaf"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          shared <- Workspace
            .createDirectories(fixture.root.resolve("shared"))
            .mapError(FixtureError.Io(_))
          _ <- io(shared, IoFault.Write)(
            Files.setPosixFilePermissions(
              shared,
              java.nio.file.attribute.PosixFilePermissions
                .fromString("rwxrwxrwx")
            )
          )
          refused <- run(
            fixture,
            extra = source(fixture) + ("XDG_CACHE_HOME" -> shared.toString)
          )
          requests <- read(fixture.downloads)
        yield assertTrue(
          refused.exit.contains(ProcessExit(1)),
          refused.arguments.isEmpty,
          requests.isEmpty,
          refused.output.contains("unsafe cache ancestor")
        )
    ,
    test(
      "toolchain.local is an explicit local trust override even when a lock is present"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          _ <- lockedJdk(fixture)
          _ <- write(
            fixture.root.resolve(".histos/toolchain.local"),
            fixture.root.toString + "\n"
          )
          _ <- executable(
            fixture.root.resolve("bin/java"),
            "#!/bin/sh\nprintf '%s\\n' \"local-trust\" \"$@\" > \"$HISTOS_JAVA_LOG\"\n"
          )
          ran <- run(
            fixture,
            extra = source(fixture) + ("HISTOS_FETCH_FAIL" -> "yes")
          )
          downloads <- read(fixture.downloads)
        yield assertTrue(
          ran.exit.contains(ProcessExit(0)),
          ran.arguments.headOption.contains("local-trust"),
          downloads.isEmpty
        )
    ,
    test("B7 valid local trust does not bypass a present invalid lock"):
      ZIO.scoped:
        ZIO
          .foreach(List("directory", "dangling", "malformed", "url")): kind =>
            for
              fixture <- layout
              _ <- write(
                fixture.root.resolve(".histos/toolchain.local"),
                fixture.root.toString + "\n"
              )
              path = fixture.root.resolve(".histos/jdk.lock")
              _ <- kind match
                case "directory" =>
                  Workspace
                    .createDirectories(path)
                    .mapError(FixtureError.Io(_))
                    .unit
                case "dangling" =>
                  io(path, IoFault.Create)(
                    Files
                      .createSymbolicLink(path, fixture.root.resolve("missing"))
                  ).unit
                case "malformed" => write(path, "major 21\nversion bad\n")
                case _           =>
                  write(
                    path,
                    s"major 21\nversion 21.0.2\narchive windows-x86_64 file:///tmp/%00.zip $sha\n"
                  )
              ran <- run(fixture, extra = source(fixture))
              downloads <- read(fixture.downloads)
            yield assertTrue(
              ran.exit.contains(ProcessExit(1)),
              ran.arguments.isEmpty,
              downloads.isEmpty
            )
          .map(_.foldLeft(assertTrue(true))(_ && _))
    ,
    test(
      "interruption after local copy or extraction removes owned scratch before publication"
    ):
      ZIO.scoped:
        ZIO
          .foreach(List("copy", "extract")): phase =>
            for
              fixture <- layout
              jdkSha <- lockedJdk(fixture)
              marker = fixture.root.resolve("boundary.entered")
              gate = fixture.root.resolve("boundary.gate")
              work <- io(fixture.root, IoFault.Temp)(
                Files.createTempDirectory(fixture.root, "fifo-")
              )
              created <- ChildProcess
                .command(
                  List("/usr/bin/mkfifo", gate.toString),
                  work,
                  Map.empty
                )
                .runCollect
                .mapError(FixtureError.Process(_))
              _ <- ZIO
                .fail(FixtureError.Exit)
                .unless(
                  created.lastOption.contains(ChildEvent.Exited(ProcessExit(0)))
                )
              _ <-
                if phase == "copy" then
                  executable(
                    fixture.bin.resolve("curl"),
                    downloader.replace(
                      "cp \"${url#file://}\" \"$out\"",
                      "cp \"${url#file://}\" \"$out\"; printf \"%s\\n\" \"$PPID\" > \"$HISTOS_BOUNDARY_MARKER\"; read -r line < \"$HISTOS_BOUNDARY_GATE\""
                    )
                  )
                else
                  Workspace
                    .delete(fixture.bin.resolve("tar"))
                    .mapError(FixtureError.Io(_)) *>
                    executable(
                      fixture.bin.resolve("tar"),
                      """#!/bin/sh
                  |extract=no
                  |for arg in "$@"; do if [ "$arg" = -xf ]; then extract=yes; fi; done
                  |if [ "$extract" = yes ]; then
                  |  /usr/bin/tar "$@" || exit 1
                  |  printf '%s\n' "$PPID" > "$HISTOS_BOUNDARY_MARKER"
                  |  read -r line < "$HISTOS_BOUNDARY_GATE"
                  |else exec /usr/bin/tar "$@"; fi
                  |""".stripMargin
                    )
              fiber <- run(
                fixture,
                extra = source(fixture) ++ Map(
                  "HISTOS_BOUNDARY_MARKER" -> marker.toString,
                  "HISTOS_BOUNDARY_GATE" -> gate.toString
                )
              ).forkScoped
              entered <- read(marker)
                .repeatUntil(_.nonEmpty)
                .raceFirst(fiber.await *> ZIO.fail(FixtureError.Exit))
              pid <- ZIO
                .fromOption(entered.trim.toLongOption)
                .orElseFail(FixtureError.Exit)
              signalWork <- io(fixture.root, IoFault.Temp)(
                Files.createTempDirectory(fixture.root, "signal-")
              )
              signalled <- ChildProcess
                .command(
                  List("/bin/kill", "-TERM", s"-$pid"),
                  signalWork,
                  Map.empty
                )
                .runCollect
                .mapError(FixtureError.Process(_))
              _ <- ZIO
                .fail(FixtureError.Exit)
                .unless(
                  signalled.lastOption
                    .contains(ChildEvent.Exited(ProcessExit(0)))
                )
              stopped <- fiber.join
              temps <- leftovers(fixture)
              installed <- io(fixture.cache, IoFault.Walk):
                val destination = fixture.cache.resolve(s"jdks/${jdkSha.value}")
                if !Files.isDirectory(destination) then List.empty[Path]
                else
                  val listing = Files.list(destination)
                  try
                    listing.iterator.asScala
                      .filter(_.getFileName.toString.startsWith("install."))
                      .toList
                  finally listing.close()
            yield assertTrue(
              stopped.exit.contains(ProcessExit(1)),
              stopped.arguments.isEmpty,
              temps.isEmpty,
              installed.isEmpty
            )
          .map(_.foldLeft(assertTrue(true))(_ && _))
    ,
    test(
      "host-specific tables select only the exact archive and never download another host"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          _ <- lockedJdk(fixture)
          ran <- run(fixture, extra = source(fixture))
          downloads <- read(fixture.downloads)
          lock <- read(fixture.root.resolve(".histos/jdk.lock"))
          _ <- write(
            fixture.root.resolve(".histos/jdk.lock"),
            lock.linesIterator
              .filterNot(_.startsWith("archive macos-aarch64 "))
              .mkString("\n")
          )
          missing <- run(fixture, extra = source(fixture))
          after <- read(fixture.downloads)
        yield assertTrue(
          ran.exit.contains(ProcessExit(0)),
          ran.arguments.headOption.contains("locked-jdk"),
          downloads.linesIterator.toList == List(
            fixture.root.resolve("jdk.tgz").toUri.toASCIIString
          ),
          missing.exit.contains(ProcessExit(1)),
          missing.output.contains("no archive for runtime host macos-aarch64"),
          missing.arguments.isEmpty,
          after == downloads
        )
    ,
    test(
      "OS and architecture aliases normalize safely, unsupported hosts never fall back"
    ):
      ZIO.scoped:
        ZIO
          .foreach(
            List(
              ("Darwin", "arm64", "macos-aarch64"),
              ("Darwin", "x86_64", "macos-x86_64"),
              ("Linux", "amd64", "linux-x86_64"),
              ("Linux", "aarch64", "linux-aarch64")
            )
          ): (os, cpu, host) =>
            for
              fixture <- layout
              jdkSha <- lockedJdk(fixture)
              _ <- write(
                fixture.root.resolve(".histos/jdk.lock"),
                s"major 21\nversion 21.0.2\narchive $host ${fixture.root.resolve("jdk.tgz").toUri.toASCIIString} ${jdkSha.value}\n"
              )
              ran <- run(
                fixture,
                extra = source(fixture) ++ Map(
                  "HISTOS_TEST_OS" -> os,
                  "HISTOS_TEST_CPU" -> cpu
                )
              )
              unsupported <- run(
                fixture,
                extra = source(fixture) ++ Map(
                  "HISTOS_TEST_OS" -> os,
                  "HISTOS_TEST_CPU" -> "mips"
                )
              )
            yield assertTrue(
              ran.exit.contains(ProcessExit(0)),
              unsupported.exit.contains(ProcessExit(1)),
              unsupported.output.contains("no archive for runtime host"),
              unsupported.arguments.isEmpty
            )
          .map(_.foldLeft(assertTrue(true))(_ && _))
    ,
    test("an explicit local override does not require the host in the table"):
      ZIO.scoped:
        for
          fixture <- layout
          _ <- lockedJdk(fixture)
          lock <- read(fixture.root.resolve(".histos/jdk.lock"))
          _ <- write(
            fixture.root.resolve(".histos/jdk.lock"),
            lock.linesIterator
              .filterNot(_.startsWith("archive macos-aarch64 "))
              .mkString("\n")
          )
          _ <- write(
            fixture.root.resolve(".histos/toolchain.local"),
            fixture.root.toString + "\n"
          )
          ran <- run(fixture, extra = source(fixture))
          downloads <- read(fixture.downloads)
        yield assertTrue(
          ran.exit.contains(ProcessExit(0)),
          ran.arguments.contains(fixture.jar.toString),
          downloads.isEmpty
        )
    ,
    test("JDK schema errors are refused before using PATH java"):
      ZIO.scoped:
        for
          fixture <- layout
          jdkSha <- lockedJdk(fixture)
          valid <- read(fixture.root.resolve(".histos/jdk.lock"))
          results <- ZIO.foreach(
            List(
              valid + "major 21\n",
              valid + "unknown value\n",
              valid.replace("major 21", "major 0"),
              valid.replace("version 21.0.2", "version 22.0.2"),
              valid.replace("version 21.0.2", "version ../21"),
              valid.replace(jdkSha.value, "invalid"),
              valid.replace(
                "archive windows-x86_64 file:",
                "archive windows-x86_64 http:"
              ),
              valid + "url file:///legacy.tgz\n",
              valid + "sha256 " + ("0" * 64) + "\n",
              valid.replace("archive windows-x86_64", "archive windows-amd64"),
              valid + valid.linesIterator
                .filter(_.startsWith("archive macos-aarch64 "))
                .mkString("\n") + "\n",
              valid.linesIterator
                .filterNot(_.startsWith("archive "))
                .mkString("\n"),
              valid.linesIterator
                .filterNot(_.startsWith("archive macos-aarch64 "))
                .mkString("\n")
            )
          ): text =>
            write(fixture.root.resolve(".histos/jdk.lock"), text) *> run(
              fixture,
              extra = source(fixture)
            )
        yield assertTrue(
          results.forall(ran =>
            ran.exit.contains(ProcessExit(1)) && ran.arguments.isEmpty
          )
        )
    ,
    test("POSIX bootstrap matches the shared URL corpus before any download"):
      ZIO.scoped:
        for
          repo <- zio.System.env("HISTOS_REPO").mapError(_ => FixtureError.Exit)
          root <- ZIO
            .fromOption(repo.map(Path.of(_)))
            .orElseFail(FixtureError.Exit)
          text <- Workspace
            .readString(root.resolve("engine/test/fixtures/launcher-urls.tsv"))
            .mapError(FixtureError.Io(_))
          rows <- ZIO.foreach(
            text.linesIterator.filterNot(_.startsWith("#")).toList
          ): line =>
            line.split("\\t", 3).toList match
              case kind :: accepted :: encoded :: Nil =>
                io(root, IoFault.Read)(
                  new String(
                    java.util.Base64.getDecoder.decode(encoded),
                    StandardCharsets.UTF_8
                  )
                )
                  .map(raw => (kind, accepted == "1", raw))
              case _ => ZIO.fail(FixtureError.Exit)
          runtimeText <- Workspace
            .readString(
              root.resolve("engine/test/fixtures/runtime-lock/urls.tsv")
            )
            .mapError(FixtureError.Io(_))
          runtimeRows <- ZIO.foreach(runtimeText.linesIterator.toList): line =>
            line.split("\t", 2).toList match
              case expected :: encoded :: Nil =>
                ZIO.succeed(
                  (
                    "archive",
                    expected == "accept",
                    encoded
                      .replace("{TAB}", "\t")
                      .replace("{CR}", "\r")
                      .replace("{LF}", "\n")
                      .replace("{NUL}", "\u0000")
                  )
                )
              case _ => ZIO.fail(FixtureError.Exit)
          fixture <- layout
          corpusSha <- lockedJdk(fixture)
          original <- read(fixture.root.resolve(".histos/jdk.lock"))
          warmed <- run(fixture)
          _ <- Workspace
            .delete(fixture.root.resolve(".histos/jdk.lock"))
            .mapError(FixtureError.Io(_))
          before <- read(fixture.downloads)
          results <- ZIO.foreach(rows ++ runtimeRows): (kind, accepted, raw) =>
            val configure = if kind == "repository" then
              Workspace
                .delete(fixture.root.resolve(".histos/jdk.lock"))
                .mapError(FixtureError.Io(_)) *>
                write(
                  fixture.root.resolve(".histos/launcher.lock"),
                  s"repository $raw\nversion $version\nrevision $revision\nsha256 $sha\n"
                )
            else
              val body = original.linesIterator
                .map(line =>
                  if line.startsWith("archive macos-aarch64 ") then
                    s"archive macos-aarch64 $raw ${corpusSha.value}"
                  else line
                )
                .mkString("\n")
              write(fixture.root.resolve(".histos/jdk.lock"), body)
            for
              _ <- configure
              ran <- run(
                fixture,
                extra = if kind == "archive" then source(fixture) else Map.empty
              )
              foreign <-
                if kind == "archive" then
                  val table = original.linesIterator
                    .map(line =>
                      if line.startsWith("archive windows-x86_64 ") then
                        s"archive windows-x86_64 $raw ${corpusSha.value}"
                      else line
                    )
                    .mkString("\n")
                  write(fixture.root.resolve(".histos/jdk.lock"), table) *>
                    run(fixture, extra = source(fixture)).map(Some(_))
                else ZIO.none
            yield assertTrue(
              ran.exit.contains(ProcessExit(if accepted then 0 else 1)),
              accepted || ran.arguments.isEmpty,
              foreign.forall(result =>
                result.exit.contains(ProcessExit(if accepted then 0 else 1)) &&
                  (accepted || result.arguments.isEmpty)
              )
            )
          after <- read(fixture.downloads)
        yield results.foldLeft(
          assertTrue(warmed.exit.contains(ProcessExit(0)), before == after)
        )(_ && _)
    ,
    test("launcher schema errors are refused before a download"):
      ZIO.scoped:
        for
          fixture <- layout
          valid <- read(fixture.root.resolve(".histos/launcher.lock"))
          results <- ZIO.foreach(
            List(
              valid + "revision latest\n",
              valid + "unknown value\n",
              valid.replace(revision, "0.0.1-SNAPSHOT"),
              valid.replace("20261023", "20260230"),
              valid.replace("20261023.224241", "2026102.3224241"),
              valid.replace("127.0.0.1", "example.com"),
              valid.replace(s"sha256 $sha", "sha256 broken"),
              valid.replace("version 0.0.1-SNAPSHOT", "version 0.0.2-SNAPSHOT")
            )
          ): text =>
            write(fixture.root.resolve(".histos/launcher.lock"), text) *> run(
              fixture
            )
          downloads <- read(fixture.downloads)
        yield assertTrue(
          results.forall(ran =>
            ran.exit.contains(ProcessExit(1)) && ran.arguments.isEmpty
          ),
          downloads.isEmpty
        )
  ) @@ TestAspect.withLiveSystem @@ TestAspect.withLiveClock
