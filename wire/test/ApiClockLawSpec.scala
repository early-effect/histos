package histos

import histos.report.BuildFault
import histos.server.{Call, WatchPolicy}
import histos.version.{CommitSha, ShipNumber}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import _root_.scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO}
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** A local assembly whose snapshot is not the revision this checkout would publish fails and names `histos assembly`.
  * No local assembly does not.
  */
object ApiClockLawSpec extends ZIOSpecDefault:
  private val want     = "0.2.9-0123456789ab-SNAPSHOT"
  private val have     = "0.2.9-ba9876543210-SNAPSHOT"
  private val markHex  = "ab" * 32
  private val otherHex = "cd" * 32
  private val head     = "0123456789abcdef0123456789abcdef01234567"
  private val histos   = ShipNumber.trusted(0, 2, 9)
  private val early    = ShipNumber.trusted(1, 2, 3)

  private val spaced =
    """Ship("lib").next(9, 8, 7)(api)
      |Ship("histos")
      |  .next(
      |    0,
      |    2,
      |    9
      |  )(api, app)
      |Ship("later").next(1, 0, 0)(api, app)
      |""".stripMargin

  private val earlyFirst =
    """Ship("early").next(1, 2, 3)(api, app)
      |Ship("histos").next(0, 2, 9)(api, app)
      |""".stripMargin

  def spec = suite("api clock laws")(
    test("publishing reads the first ship whose members include app"):
      assertTrue(
        CommitSha.from(head).map(SnapshotId.render(histos, _)) == Right(want),
        SnapshotId.publishing(spaced).contains(histos),
        SnapshotId.publishing(earlyFirst).contains(early),
        SnapshotId.publishing("""Ship("lib").next(1, 0, 0)(engine)""").isEmpty,
        SnapshotId.publishing("""ship("histos", ship"0.2.9")(api, app)""").isEmpty,
      )
    ,
    test("no local assembly does not fail, and the stamp file wins over the id baked into the process"):
      work.flatMap { repo =>
        for
          plain <- ApiClock.judge(repo, Call.Compile(Nil), have, Some(want))
          _     <- mark(repo)
          _     <- write(ApiClock.stampFile(repo), ApiClock.source(want))
          filed <- ApiClock.judge(repo, Call.Compile(Nil), have, Some(want))
          _     <- write(ApiClock.stampFile(repo), ApiClock.source(""))
          baked <- ApiClock.judge(repo, Call.Compile(Nil), want, Some(want))
        yield assertTrue(plain == ApiClock.Admit.Go, filed == ApiClock.Admit.Go, baked == ApiClock.Admit.Go)
      }
    ,
    test("a different snapshot fails and names histos assembly, and the hatch continues"):
      work.flatMap { repo =>
        val compile = Call.Compile(Nil)
        for
          _       <- mark(repo)
          blocked <- ApiClock.judge(repo, compile, "", Some(want))
          watched <- ApiClock.judge(repo, Call.Watch(compile, WatchPolicy.Restart), have, Some(want))
          allowed <- ApiClock.judge(repo, Call.Assembly(None), "", Some(want))
          version <- ApiClock.judge(repo, Call.Version, "", Some(want))
          reboot  <- ApiClock.judge(repo, Call.Reboot, "", Some(want))
          held    <- ApiClock.judge(repo, Call.Watch(Call.Version, WatchPolicy.Restart), have, Some(want))
          unknown <- ApiClock.judge(repo, compile, have, None)
          _       <- write(ApiClock.stampFile(repo), ApiClock.source(have))
          stamped <- ApiClock.judge(repo, compile, want, Some(want))
          _       <- write(ApiClock.hatchFile(repo), "")
          warned  <- ApiClock.judge(repo, compile, have, Some(want))
        yield assertTrue(
          blocked == ApiClock.Admit.Stop(BuildFault.StaleApi("", want)),
          watched == ApiClock.Admit.Stop(BuildFault.StaleApi(have, want)),
          stamped == ApiClock.Admit.Stop(BuildFault.StaleApi(have, want)),
          allowed == ApiClock.Admit.Go,
          version == ApiClock.Admit.Go,
          reboot == ApiClock.Admit.Go,
          held == ApiClock.Admit.Go,
          unknown == ApiClock.Admit.Go,
          ApiClock.skipsReboot(Call.Assembly(None)),
          ApiClock.skipsReboot(Call.Version),
          ApiClock.skipsReboot(Call.Watch(Call.Version, WatchPolicy.Restart)),
          !ApiClock.skipsReboot(Call.Reboot),
          !ApiClock.skipsReboot(compile),
          !ApiClock.skipsReboot(Call.Watch(compile, WatchPolicy.Restart)),
          warned == ApiClock.Admit.Warn(continued(have, want)),
          BuildFault.StaleApi("", want).message ==
            s"toolchain is no snapshot id; this checkout would publish $want. Run histos assembly.",
        )
      }
    ,
    test("version names the assembly checksum and the snapshot this checkout would publish"):
      work.flatMap { repo =>
        val line = "histos 0.2.8"
        for
          absent <- ApiClock.render(repo, line, have, Some(want))
          _      <- mark(repo)
          none   <- ApiClock.render(repo, line, have, None)
          stale  <- ApiClock.render(repo, line, have, Some(want))
          _      <- write(ApiClock.stampFile(repo), ApiClock.source(want))
          fresh  <- ApiClock.render(repo, line, "", Some(want))
        yield assertTrue(
          absent == s"$line\nserver 0.2.8\nno local assembly",
          none == s"$line\nserver 0.2.8 assembly abababab\nno ship snapshot",
          stale == s"$line\nserver 0.2.8 assembly abababab\n${BuildFault.StaleApi(have, want).message}",
          fresh == s"$line\nserver 0.2.8 assembly abababab\nsnapshot $want",
          ApiClock.rebootText(Some(markHex), Some(otherHex), "/tmp/server.sock") ==
            "rebooting server onto abababab (was cdcdcdcd) at /tmp/server.sock",
          ApiClock.rebootText(None, None, "/tmp/server.sock") ==
            "rebooting server onto none (was none) at /tmp/server.sock",
          ApiClock.mcpNote(Some(want), Some(want)) == s"mcp $want",
          ApiClock.mcpNote(None, Some(want)) == s"mcp $want",
          ApiClock.mcpNote(Some(have), Some(want)) == "mcp is not this assembly",
          ApiClock.mcpNote(None, None) == "mcp is not this assembly",
        )
      }
    ,
    test("current is the snapshot coordinate of the app ship at HEAD"):
      work.flatMap { repo =>
        val file  = repo.resolve("histos.scala")
        val hooks = repo.resolve("hooks")
        for
          bare        <- SnapshotId.current(repo)
          _           <- write(file, """ships(Ship("histos").next(0, 2, 9)(api, app))""")
          uncommitted <- SnapshotId.current(repo)
          _           <- git(repo, "init")
          _           <- write(hooks.resolve(".keep"), "")
          _           <- git(repo, "config", "user.email", "clock@example.com")
          _           <- git(repo, "config", "user.name", "clock")
          _           <- git(repo, "config", "commit.gpgsign", "false")
          _           <- git(repo, "config", "core.hooksPath", hooks.toString)
          _           <- git(repo, "add", "histos.scala")
          _           <- git(repo, "commit", "-m", "stamp")
          raw         <- git(repo, "rev-parse", "HEAD")
          got         <- SnapshotId.current(repo)
        yield assertTrue(
          bare.isEmpty,
          uncommitted.isEmpty,
          got == CommitSha.from(raw).toOption.map(SnapshotId.render(histos, _)),
        )
      },
  )

  private def continued(held: String, wanted: String): String =
    s"${BuildFault.StaleApi(held, wanted).message} .histos/${ApiClock.hatchName} is set, so this command continues."

  private def mark(repo: Path): ZIO[Any, Throwable, Unit] =
    write(repo.resolve(".histos").resolve("assembly.sha256"), markHex + "\n")

  private def write(path: Path, text: String): ZIO[Any, Throwable, Unit] =
    ZIO.attemptBlocking {
      Files.createDirectories(path.getParent)
      Files.writeString(path, text, StandardCharsets.UTF_8)
    }.unit

  private final case class GitFailed(detail: String)

  private def git(repo: Path, args: String*): ZIO[Any, GitFailed, String] =
    ZIO
      .attemptBlocking {
        val command = List("git", "-C", repo.toString) ++ args.toList
        val process = new ProcessBuilder(command*)
          .redirectError(ProcessBuilder.Redirect.DISCARD)
          .start()
        val text = String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8).trim
        (process.waitFor(), text)
      }
      .mapError(error => GitFailed(Option(error.getMessage).getOrElse(error.getClass.getSimpleName)))
      .flatMap:
        case (0, text) => ZIO.succeed(text)
        case (code, _) => ZIO.fail(GitFailed(s"git ${args.mkString(" ")} exited $code"))

  private def work: ZIO[Scope, Nothing, Path] =
    ZIO.acquireRelease(
      ZIO.attemptBlocking(Files.createTempDirectory("api-clock")).orDie
    )(root => ZIO.attemptBlocking(remove(root)).orDie)

  private def remove(root: Path): Unit =
    if Files.exists(root) then
      val walk  = Files.walk(root)
      val paths =
        try walk.iterator.asScala.toList
        finally walk.close()
      paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
end ApiClockLawSpec
