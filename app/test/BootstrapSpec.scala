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
  private val version = "0.0.1-SNAPSHOT"
  private val bytes   = "fixture launcher".getBytes(StandardCharsets.UTF_8)
  private val sha     = Sha256.of(Chunk.fromArray(bytes)).value
  private val javaGuide =
    """A JDK 21 or newer is required. Histos does not install it.
      |macOS: brew install --cask temurin@21
      |Linux: https://adoptium.net/temurin/releases?version=21""".stripMargin

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
      _    <- write(root.resolve("build.properties"), s"histos.version=$version\n")
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
      env = Map(
        "HISTOS_JAVA_LOG" -> log.toString,
        "HOME"            -> fixture.root.toString,
        "PATH"            -> fixture.bin.toString,
      ) ++ extra
      events <- ChildProcess
        .command(List("/bin/sh", fixture.root.resolve("histos").toString) ++ args, work, env)
        .runCollect
        .mapError(FixtureError.Process(_))
      arguments <- read(log).map(_.linesIterator.toList)
    yield Ran(
      events.collect { case ChildEvent.Exited(code) => code }.lastOption,
      events.collect { case ChildEvent.Out(text) => text; case ChildEvent.Err(text) => text }.mkString("\n"),
      arguments,
      "",
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
    test("missing java prints the floor"):
      ZIO.scoped:
        for
          fixture <- layout
          _       <- Workspace.delete(fixture.bin.resolve("java")).mapError(FixtureError.Io(_))
          ran     <- run(fixture)
        yield assertTrue(
          ran.exit.contains(ProcessExit(1)),
          ran.output.contains("java is not on PATH"),
          ran.output.contains(javaGuide),
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
          ran.arguments.isEmpty,
        )
    ,
    test("without a local assembly the checkout script execs the user shim"):
      ZIO.scoped:
        for
          fixture <- layout
          log = fixture.root.resolve("shim.log")
          _ <- executable(
            fixture.root.resolve(".histos/bin/histos"),
            s"""#!/bin/sh
               |printf '%s\\n' "$$@" > "${log}"
               |exit 0
               |""".stripMargin,
          )
          ran  <- run(fixture, List("test", "app"))
          seen <- read(log)
        yield assertTrue(
          ran.exit.contains(ProcessExit(0)),
          seen == "test\napp\n",
          ran.arguments.isEmpty,
        )
    ,
    test("a missing user shim names the install"):
      ZIO.scoped:
        for
          fixture <- layout
          ran     <- run(fixture, List("test"))
        yield assertTrue(
          ran.exit.contains(ProcessExit(1)),
          ran.output.contains("histos is not installed"),
          ran.arguments.isEmpty,
        ),
  ) @@ TestAspect.withLiveSystem @@ TestAspect.withLiveClock
end BootstrapSpec
