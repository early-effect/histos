package histos.cli

import histos.jdk.Sha256
import histos.process.{ChildError, ChildEvent, ChildProcess, ProcessExit}
import histos.publish.LauncherScript
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

object BootstrapSpec extends ZIOSpecDefault:
  private val version  = "0.0.1-SNAPSHOT"
  private val revision = "0.0.1-20261101.030257-1"
  private val bytes    = "fixture launcher".getBytes(StandardCharsets.UTF_8)
  private val sha      = Sha256.of(Chunk.fromArray(bytes)).value
  private val snapshots = "https://central.sonatype.com/repository/maven-snapshots"
  private val javaGuide =
    """A JDK 21 or newer is required. Histos does not install it.
      |macOS: brew install --cask temurin@21
      |Linux and Windows: https://adoptium.net/temurin/releases?version=21""".stripMargin
  private val csGuide =
    """The cs command from Coursier 2.1 or newer is required. Histos does not install it.
      |macOS: brew install coursier/formulas/coursier
      |The Homebrew core formula provides coursier and does not.
      |Linux and Windows: https://get-coursier.io/docs/cli-installation and put the cs binary on PATH.""".stripMargin

  private enum FixtureError:
    case Io(error: IoFailure)
    case Process(error: ChildError)

  private final case class Fixture(root: Path, bin: Path, jar: Path)

  private final case class Ran(
      exit: Option[ProcessExit],
      output: String,
      arguments: List[String],
      coursier: String,
  )

  private def write(path: Path, text: String): IO[FixtureError, Unit] =
    Workspace.writeString(path, text).mapError(FixtureError.Io(_))

  private def read(path: Path): IO[FixtureError, String] =
    Workspace.readIfPresent(path).mapError(FixtureError.Io(_)).map(_.getOrElse(""))

  private def layout: ZIO[Scope, FixtureError, Fixture] =
    for
      root <- Workspace.work("histos-boot").mapError(FixtureError.Io(_))
      bin  <- Workspace.createDirectories(root.resolve("bin")).mapError(FixtureError.Io(_))
      _    <- write(root.resolve("histos"), LauncherScript.posix(version))
      _    <- write(root.resolve(".histos/assembly.sha256"), sha + "\n")
      _    <- write(root.resolve(".histos/build.properties"), s"histos.version=$version\n")
      jar  <- ZIO
        .attemptBlocking(Files.write(root.resolve("app.jar"), bytes))
        .mapError(_ => FixtureError.Io(IoFailure(root, IoFault.Write)))
      _ <- executable(
        bin.resolve("java"),
        """#!/bin/sh
          |if [ "$1" = "-version" ]; then
          |  printf 'openjdk version "%s"\n' "${HISTOS_JAVA_VERSION:-21.0.1}" >&2
          |  exit 0
          |fi
          |printf '%s\n' "$@" > "$HISTOS_JAVA_LOG"
          |""".stripMargin,
      )
      _ <- executable(
        bin.resolve("cs"),
        """#!/bin/sh
          |if [ "$1" = "version" ]; then
          |  printf '%s\n' "${HISTOS_CS_VERSION:-2.1.26}"
          |  exit 0
          |fi
          |{
          |  printf 'TTL=%s\n' "${COURSIER_TTL-}"
          |  printf '%s\n' "$@"
          |} >> "$HISTOS_CS_LOG"
          |if [ "${HISTOS_CS_FAIL:-}" = yes ]; then exit 1; fi
          |case "${HISTOS_CS_LINES:-1}" in
          |  0) exit 0 ;;
          |  2) printf '%s\n%s\n' "$HISTOS_CS_JAR" "$HISTOS_CS_JAR" ;;
          |  *) printf '%s\n' "$HISTOS_CS_JAR" ;;
          |esac
          |""".stripMargin,
      )
      _ <- ZIO
        .attemptBlocking:
          List("/usr/bin/dirname", "/usr/bin/awk", "/usr/bin/tr", "/usr/bin/shasum", "/usr/bin/sha256sum")
            .foreach: source =>
              val path   = Path.of(source)
              val target = bin.resolve(path.getFileName)
              if Files.isRegularFile(path) && !Files.exists(target) then Files.createSymbolicLink(target, path)
        .mapError(_ => FixtureError.Io(IoFailure(bin, IoFault.Create)))
    yield Fixture(root, bin, jar)

  private def executable(path: Path, text: String): IO[FixtureError, Unit] =
    write(path, text) *> ZIO
      .attemptBlocking(path.toFile.setExecutable(true))
      .mapError(_ => FixtureError.Io(IoFailure(path, IoFault.Write)))
      .unit

  private def run(
      fixture: Fixture,
      args: List[String] = Nil,
      extra: Map[String, String] = Map.empty,
  ): IO[FixtureError, Ran] =
    for
      work <- ZIO
        .attemptBlocking(Files.createTempDirectory(fixture.root, "child-"))
        .mapError(_ => FixtureError.Io(IoFailure(fixture.root, IoFault.Temp)))
      log = work.resolve("java.log")
      cs  = work.resolve("cs.log")
      env = Map(
        "HISTOS_JAVA_LOG" -> log.toString,
        "HISTOS_CS_LOG"   -> cs.toString,
        "HISTOS_CS_JAR"   -> fixture.jar.toString,
        "HOME"            -> fixture.root.toString,
        "PATH"            -> fixture.bin.toString,
      ) ++ extra
      events <- ChildProcess
        .command(List("/bin/sh", fixture.root.resolve("histos").toString) ++ args, work, env)
        .runCollect
        .mapError(FixtureError.Process(_))
      arguments <- read(log).map(_.linesIterator.toList)
      coursier  <- read(cs)
    yield Ran(
      events.collect { case ChildEvent.Exited(code) => code }.lastOption,
      events.collect { case ChildEvent.Out(text) => text; case ChildEvent.Err(text) => text }.mkString("\n"),
      arguments,
      coursier,
    )

  private def source(fixture: Fixture): Map[String, String] =
    Map("HISTOS_ASSEMBLY" -> fixture.jar.toString)

  def spec = suite("bootstrap script")(
    test("a local assembly never calls cs and keeps the requested arguments"):
      ZIO.scoped:
        for
          fixture <- layout
          ran     <- run(fixture, List("graph", "a b", "", "literal;$value"), source(fixture))
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
          ran.coursier.isEmpty,
        )
    ,
    test(".jvmopts lines are the JVM arguments, one per line"):
      ZIO.scoped:
        for
          fixture <- layout
          _       <- write(
            fixture.root.resolve(".jvmopts"),
            "# comment\n\n-Xmx1g\r\n-Dname=a b\n-Dname=it's\n-Xss8m",
          )
          ran <- run(fixture, List("graph", ""), source(fixture))
        yield assertTrue(
          ran.exit.contains(ProcessExit(0)),
          ran.arguments == List(
            "-Xmx1g",
            "-Dname=a b",
            "-Dname=it's",
            "-Xss8m",
            "--enable-native-access=ALL-UNNAMED",
            "-jar",
            fixture.jar.toString,
            "graph",
            "",
          ),
          ran.coursier.isEmpty,
        )
    ,
    test("a jvm option that selects the program is refused"):
      ZIO.scoped:
        for
          fixture <- layout
          _       <- write(fixture.root.resolve(".jvmopts"), "-Xmx1g\n-jar\nevil.jar\n")
          ran     <- run(fixture, extra = source(fixture))
        yield assertTrue(
          ran.exit.contains(ProcessExit(1)),
          ran.output.contains("jvm option selects the program: -jar"),
          ran.arguments.isEmpty,
        )
    ,
    test("an assembly checksum mismatch refuses execution"):
      ZIO.scoped:
        for
          fixture <- layout
          _       <- write(fixture.root.resolve(".histos/assembly.sha256"), "0" * 64)
          ran     <- run(fixture, extra = source(fixture))
        yield assertTrue(
          ran.exit.contains(ProcessExit(1)),
          ran.output.contains("checksum mismatch"),
          ran.arguments.isEmpty,
          ran.coursier.isEmpty,
        )
    ,
    test("missing java prints the floor before coursier is considered"):
      ZIO.scoped:
        for
          fixture <- layout
          _       <- Workspace.delete(fixture.bin.resolve("java")).mapError(FixtureError.Io(_))
          _       <- Workspace.delete(fixture.bin.resolve("cs")).mapError(FixtureError.Io(_))
          ran     <- run(fixture)
        yield assertTrue(
          ran.exit.contains(ProcessExit(1)),
          ran.output.contains("java is not on PATH"),
          ran.output.contains(javaGuide),
          !ran.output.contains("Coursier 2.1"),
          ran.coursier.isEmpty,
        )
    ,
    test("missing cs prints the floor and how to install it"):
      ZIO.scoped:
        for
          fixture <- layout
          _       <- Workspace.delete(fixture.bin.resolve("cs")).mapError(FixtureError.Io(_))
          ran     <- run(fixture)
        yield assertTrue(
          ran.exit.contains(ProcessExit(1)),
          ran.output.contains("coursier is not on PATH"),
          ran.output.contains(csGuide),
          ran.arguments.isEmpty,
        )
    ,
    test("java older than 21 names the version and the floor"):
      ZIO.scoped:
        for
          fixture <- layout
          ran     <- run(fixture, extra = Map("HISTOS_JAVA_VERSION" -> "1.8.0_292"))
        yield assertTrue(
          ran.exit.contains(ProcessExit(1)),
          ran.output.contains("java 1.8.0_292 is older than 21"),
          ran.output.contains(javaGuide),
          ran.coursier.isEmpty,
          ran.arguments.isEmpty,
        )
    ,
    test("cs older than 2.1 names the version and the floor"):
      ZIO.scoped:
        for
          fixture <- layout
          ran     <- run(fixture, extra = Map("HISTOS_CS_VERSION" -> "2.0.16"))
        yield assertTrue(
          ran.exit.contains(ProcessExit(1)),
          ran.output.contains("coursier 2.0.16 is older than 2.1"),
          ran.output.contains(csGuide),
          ran.arguments.isEmpty,
        )
    ,
    test("a snapshot pointer rechecks with ttl 0 and the snapshot repository"):
      ZIO.scoped:
        for
          fixture <- layout
          ran     <- run(fixture, List("graph"), Map("COURSIER_TTL" -> "1h"))
        yield assertTrue(
          ran.exit.contains(ProcessExit(0)),
          ran.coursier ==
            s"""TTL=0s
               |fetch
               |--no-default
               |-r
               |$snapshots
               |rocks.earlyeffect:histos:$version
               |""".stripMargin,
          ran.arguments == List(
            "--enable-native-access=ALL-UNNAMED",
            "-jar",
            fixture.jar.toString,
            "graph",
          ),
        )
    ,
    test("a timestamp fetch does not set ttl and names that revision"):
      ZIO.scoped:
        for
          fixture <- layout
          _       <- write(fixture.root.resolve(".histos/build.properties"), s"histos.version=$revision\n")
          ran     <- run(fixture, extra = Map("COURSIER_TTL" -> "1h"))
        yield assertTrue(
          ran.exit.contains(ProcessExit(0)),
          ran.coursier.startsWith("TTL=\n"),
          ran.coursier.contains(s"rocks.earlyeffect:histos:$revision"),
          ran.coursier.contains("--no-default"),
          !ran.coursier.contains("TTL=0s"),
          ran.arguments.contains("-jar"),
        )
    ,
    test("HISTOS_LAUNCHER_REPO replaces the repository"):
      ZIO.scoped:
        for
          fixture <- layout
          ran     <- run(fixture, extra = Map("HISTOS_LAUNCHER_REPO" -> "http://127.0.0.1:9/repo"))
        yield assertTrue(
          ran.exit.contains(ProcessExit(0)),
          ran.coursier.contains("http://127.0.0.1:9/repo"),
          !ran.coursier.contains(snapshots),
        )
    ,
    test("a failed fetch does not exec java"):
      ZIO.scoped:
        for
          fixture <- layout
          ran     <- run(fixture, extra = Map("HISTOS_CS_FAIL" -> "yes"))
        yield assertTrue(
          ran.exit.contains(ProcessExit(1)),
          ran.output.contains("launcher fetch failed"),
          ran.arguments.isEmpty,
        )
    ,
    test("zero or two fetched paths fail before java"):
      ZIO.scoped:
        for
          fixture <- layout
          none    <- run(fixture, extra = Map("HISTOS_CS_LINES" -> "0"))
          two     <- run(fixture, extra = Map("HISTOS_CS_LINES" -> "2"))
        yield assertTrue(
          none.exit.contains(ProcessExit(1)),
          two.exit.contains(ProcessExit(1)),
          none.output.contains("launcher fetch did not print one jar"),
          two.output.contains("launcher fetch did not print one jar"),
          none.arguments.isEmpty,
          two.arguments.isEmpty,
        )
    ,
    test("a channel that does not boot fails before cs"):
      ZIO.scoped:
        for
          fixture <- layout
          _       <- write(fixture.root.resolve(".histos/build.properties"), "histos.version=0.0.2-SNAPSHOT\n")
          ran     <- run(fixture)
        yield assertTrue(
          ran.exit.contains(ProcessExit(1)),
          ran.output.contains("invalid build.properties"),
          ran.coursier.isEmpty,
          ran.arguments.isEmpty,
        )
    ,
  ) @@ TestAspect.withLiveSystem @@ TestAspect.withLiveClock
end BootstrapSpec
