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
  private val version  = "0.0.1-SNAPSHOT"
  private val revision = "0.0.1-20261023.224241-14"
  private val bytes    = "fixture launcher".getBytes(StandardCharsets.UTF_8)
  private val sha      = Sha256.of(Chunk.fromArray(bytes)).value

  private enum FixtureError:
    case Io(error: IoFailure)
    case Process(error: ChildError)
    case Exit

  private final case class Fixture(root: Path, bin: Path, jar: Path):
    def cache: Path     = root.resolve("xdg/histos")
    def remote: Path    = cache.resolve(s"launchers/$revision/$sha/histos.jar")
    def downloads: Path = root.resolve("downloads.log")

  private final case class Ran(
      exit: Option[ProcessExit],
      output: String,
      arguments: List[String],
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
      bin  <- Workspace
        .createDirectories(root.resolve("bin"))
        .mapError(FixtureError.Io(_))
      _ <- write(root.resolve("histos"), LauncherScript.posix(version))
      _ <- write(root.resolve(".histos/assembly.sha256"), sha + "\n")
      _ <- write(
        root.resolve(".histos/launcher.lock"),
        s"repository http://127.0.0.1:1234/repo\nversion $version\nrevision $revision\nsha256 $sha\n",
      )
      jar <- io(root, IoFault.Write)(
        Files.write(root.resolve("app.jar"), bytes)
      )
      _ <- write(root.resolve("app.jar.sha256"), sha + "\n")
      _ <- executable(
        bin.resolve("java"),
        "#!/bin/sh\nprintf '%s\\n' \"$@\" > \"$HISTOS_JAVA_LOG\"\n",
      )
      _ <- executable(bin.resolve("curl"), downloader)
      _ <- io(bin, IoFault.Create):
        List(
          "/usr/bin/stat",
          "/usr/bin/id",
          "/usr/bin/dirname",
          "/usr/bin/tr",
          "/usr/bin/awk",
          "/usr/bin/shasum",
          "/usr/bin/sha256sum",
          "/usr/bin/mktemp",
          "/bin/mkdir",
          "/bin/chmod",
          "/bin/rm",
          "/bin/mv",
          "/bin/cp",
        ).foreach: source =>
          val path   = Path.of(source)
          val target = bin.resolve(path.getFileName)
          if Files.isRegularFile(path) && !Files.exists(target) then Files.createSymbolicLink(target, path)
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
      extra: Map[String, String] = Map.empty,
  ): IO[FixtureError, Ran] =
    for
      work <- io(fixture.root, IoFault.Temp)(
        Files.createTempDirectory(fixture.root, "child-")
      )
      log = work.resolve("java.log")
      env = Map(
        "HISTOS_JAVA_LOG"     -> log.toString,
        "HISTOS_DOWNLOAD"     -> fixture.jar.toString,
        "HISTOS_DOWNLOAD_LOG" -> fixture.downloads.toString,
        "XDG_CACHE_HOME"      -> fixture.root.resolve("xdg").toString,
        "HOME"                -> fixture.root.toString,
        "PATH"                -> fixture.bin.toString,
      ) ++ extra
      events <- ChildProcess
        .command(
          List("/bin/sh", fixture.root.resolve("histos").toString) ++ args,
          work,
          env,
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
      arguments,
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
              path.getFileName.toString.startsWith(".bootstrap.") ||
                path.getFileName.toString.endsWith(".partial")
            )
            .toList
        finally walk.close()

  def spec = suite("bootstrap script")(
    test(
      "source assemblies use their own digest and preserve requested arguments"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          ran     <- run(
            fixture,
            List("graph", "a b", "", "literal;$value"),
            source(fixture),
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
            "literal;$value",
          ),
          temps.isEmpty,
        )
    ,
    test("source digest mismatch refuses execution"):
      ZIO.scoped:
        for
          fixture <- layout
          _       <- write(fixture.root.resolve(".histos/assembly.sha256"), "0" * 64)
          ran     <- run(fixture, extra = source(fixture))
          temps   <- leftovers(fixture)
        yield assertTrue(
          ran.exit.contains(ProcessExit(1)),
          ran.output.contains("checksum mismatch"),
          ran.arguments.isEmpty,
          temps.isEmpty,
        )
    ,
    test("missing java refuses to start"):
      ZIO.scoped:
        for
          fixture <- layout
          _       <- Workspace
            .delete(fixture.bin.resolve("java"))
            .mapError(FixtureError.Io(_))
          ran <- run(fixture, extra = source(fixture))
        yield assertTrue(
          ran.exit.contains(ProcessExit(1)),
          ran.output.contains("java is not on PATH"),
          ran.arguments.isEmpty,
        )
    ,
    test(
      "the immutable remote launcher is downloaded without metadata or a sidecar"
    ):
      ZIO.scoped:
        for
          fixture   <- layout
          ran       <- run(fixture, List("graph"))
          downloads <- read(fixture.downloads)
          temps     <- leftovers(fixture)
        yield assertTrue(
          ran.exit.contains(ProcessExit(0)),
          ran.arguments.contains(fixture.remote.toString),
          ran.arguments.contains("graph"),
          downloads.linesIterator.toList == List(
            s"http://127.0.0.1:1234/repo/rocks/earlyeffect/histos/$version/histos-$revision.jar"
          ),
          temps.isEmpty,
        )
    ,
    test(
      "a downloaded jar that agrees with a remote sidecar still cannot evade the committed pin"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          _       <- write(
            fixture.jar,
            "wrong bytes whose remote sidecar would agree",
          )
          wrongSha <- io(fixture.jar, IoFault.Read)(
            Sha256.of(Chunk.fromArray(Files.readAllBytes(fixture.jar))).value
          )
          _      <- write(fixture.root.resolve("app.jar.sha256"), wrongSha + "\n")
          ran    <- run(fixture)
          exists <- io(fixture.remote, IoFault.Stat)(
            Files.exists(fixture.remote)
          )
          temps <- leftovers(fixture)
        yield assertTrue(
          ran.exit.contains(ProcessExit(1)),
          ran.output.contains("checksum mismatch"),
          ran.arguments.isEmpty,
          !exists,
          temps.isEmpty,
        )
    ,
    test(
      "warm remote startup is offline and a corrupt cached jar is revalidated"
    ):
      ZIO.scoped:
        for
          fixture    <- layout
          first      <- run(fixture)
          warm       <- run(fixture, extra = Map("HISTOS_FETCH_FAIL" -> "yes"))
          _          <- write(fixture.remote, "corrupt cache")
          corrupt    <- run(fixture, extra = Map("HISTOS_FETCH_FAIL" -> "yes"))
          repaired   <- run(fixture)
          downloaded <- read(fixture.downloads)
          actual     <- io(fixture.remote, IoFault.Read)(
            Sha256.of(Chunk.fromArray(Files.readAllBytes(fixture.remote))).value
          )
        yield assertTrue(
          first.exit.contains(ProcessExit(0)),
          warm.exit.contains(ProcessExit(0)),
          corrupt.exit.contains(ProcessExit(1)),
          corrupt.arguments.isEmpty,
          repaired.exit.contains(ProcessExit(0)),
          actual == sha,
          downloaded.linesIterator.size == 3,
        )
    ,
    test(
      "partial failed downloads never become a cache hit and leave no temporary files"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          failed  <- run(fixture, extra = Map("HISTOS_FETCH_FAIL" -> "yes"))
          exists  <- io(fixture.remote, IoFault.Stat)(
            Files.exists(fixture.remote)
          )
          temps     <- leftovers(fixture)
          recovered <- run(fixture)
        yield assertTrue(
          failed.exit.contains(ProcessExit(1)),
          !exists,
          temps.isEmpty,
          recovered.exit.contains(ProcessExit(0)),
        )
    ,
    test(
      "concurrent cold launches use independent staging and preserve arguments"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          runs    <- ZIO.foreachPar(List("first", "second", "third"))(arg => run(fixture, List(arg)).map(arg -> _))
          temps   <- leftovers(fixture)
          actual  <- io(fixture.remote, IoFault.Read)(
            Sha256.of(Chunk.fromArray(Files.readAllBytes(fixture.remote))).value
          )
        yield assertTrue(
          runs.forall((arg, ran) =>
            ran.exit.contains(ProcessExit(0)) && ran.arguments.lastOption
              .contains(arg)
          ),
          actual == sha,
          temps.isEmpty,
        )
    ,
    test("an updater-staged launcher is reusable offline and still rehashed"):
      ZIO.scoped:
        for
          fixture <- layout
          staged = fixture.root.resolve(
            s".histos/launcher-cache/$sha/histos.jar"
          )
          _       <- write(staged, new String(bytes, StandardCharsets.UTF_8))
          warm    <- run(fixture, extra = Map("HISTOS_FETCH_FAIL" -> "yes"))
          _       <- write(staged, "corrupt staged jar")
          corrupt <- run(fixture, extra = Map("HISTOS_FETCH_FAIL" -> "yes"))
        yield assertTrue(
          warm.exit.contains(ProcessExit(0)),
          warm.arguments.contains(staged.toString),
          corrupt.exit.contains(ProcessExit(1)),
          corrupt.arguments.isEmpty,
        )
    ,
    test("a shared writable cache is refused before downloads or Java"):
      ZIO.scoped:
        for
          fixture <- layout
          _       <- Workspace
            .createDirectories(fixture.cache)
            .mapError(FixtureError.Io(_))
          _ <- io(fixture.cache, IoFault.Write)(
            Files.setPosixFilePermissions(
              fixture.cache,
              java.nio.file.attribute.PosixFilePermissions
                .fromString("rwxrwxrwx"),
            )
          )
          refused  <- run(fixture, extra = source(fixture))
          requests <- read(fixture.downloads)
        yield assertTrue(
          refused.exit.contains(ProcessExit(1)),
          refused.arguments.isEmpty,
          requests.isEmpty,
          refused.output.contains("cache is not owner-private"),
        )
    ,
    test(
      "a writable non-sticky cache ancestor is refused even with a private leaf"
    ):
      ZIO.scoped:
        for
          fixture <- layout
          shared  <- Workspace
            .createDirectories(fixture.root.resolve("shared"))
            .mapError(FixtureError.Io(_))
          _ <- io(shared, IoFault.Write)(
            Files.setPosixFilePermissions(
              shared,
              java.nio.file.attribute.PosixFilePermissions
                .fromString("rwxrwxrwx"),
            )
          )
          refused <- run(
            fixture,
            extra = source(fixture) + ("XDG_CACHE_HOME" -> shared.toString),
          )
          requests <- read(fixture.downloads)
        yield assertTrue(
          refused.exit.contains(ProcessExit(1)),
          refused.arguments.isEmpty,
          requests.isEmpty,
          refused.output.contains("unsafe cache ancestor"),
        )
    ,
    test("POSIX bootstrap accepts the launcher URL corpus before any download"):
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
              case "repository" :: accepted :: encoded :: Nil =>
                io(root, IoFault.Read)(
                  new String(
                    java.util.Base64.getDecoder.decode(encoded),
                    StandardCharsets.UTF_8,
                  )
                ).map(raw => Some(accepted == "1", raw))
              case "archive" :: _ :: _ :: Nil => ZIO.succeed(None)
              case _                          => ZIO.fail(FixtureError.Exit)
          fixture <- layout
          warmed  <- run(fixture)
          before  <- read(fixture.downloads)
          results <- ZIO.foreach(rows.flatten): (accepted, raw) =>
            for
              _ <- write(
                fixture.root.resolve(".histos/launcher.lock"),
                s"repository $raw\nversion $version\nrevision $revision\nsha256 $sha\n",
              )
              ran <- run(fixture)
            yield assertTrue(
              ran.exit.contains(ProcessExit(if accepted then 0 else 1)),
              accepted || ran.arguments.isEmpty,
            )
          after <- read(fixture.downloads)
        yield results.foldLeft(
          assertTrue(
            rows.flatten.nonEmpty,
            warmed.exit.contains(ProcessExit(0)),
            before == after,
          )
        )(_ && _)
    ,
    test("launcher schema errors are refused before a download"):
      ZIO.scoped:
        for
          fixture <- layout
          valid   <- read(fixture.root.resolve(".histos/launcher.lock"))
          results <- ZIO.foreach(
            List(
              valid + "revision latest\n",
              valid + "unknown value\n",
              valid.replace(revision, "0.0.1-SNAPSHOT"),
              valid.replace("20261023", "20260230"),
              valid.replace("20261023.224241", "2026102.3224241"),
              valid.replace("127.0.0.1", "example.com"),
              valid.replace(s"sha256 $sha", "sha256 broken"),
              valid.replace("version 0.0.1-SNAPSHOT", "version 0.0.2-SNAPSHOT"),
            )
          ): text =>
            write(fixture.root.resolve(".histos/launcher.lock"), text) *> run(
              fixture
            )
          downloads <- read(fixture.downloads)
        yield assertTrue(
          results.forall(ran => ran.exit.contains(ProcessExit(1)) && ran.arguments.isEmpty),
          downloads.isEmpty,
        ),
  ) @@ TestAspect.withLiveSystem @@ TestAspect.withLiveClock
end BootstrapSpec
