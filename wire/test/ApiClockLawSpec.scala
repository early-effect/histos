package histos

import histos.server.ServerError
import histos.version.{CommitSha, ShipNumber}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import _root_.scala.jdk.CollectionConverters.*

import zio.{Ref, Scope, UIO, ZIO}
import zio.test.{TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** A local toolchain built from another commit, or a dirty tree, warns. The command runs. There is no stop.
  */
object ApiClockLawSpec extends ZIOSpecDefault:
  private val want   = "0.2.9-0123456789ab-SNAPSHOT"
  private val head   = "0123456789abcdef0123456789abcdef01234567"
  private val other  = "fedcba9876543210fedcba9876543210fedcba98"
  private val histos = ShipNumber.trusted(0, 2, 9)
  private val early  = ShipNumber.trusted(1, 2, 3)

  private val spaced =
    """Ship("lib").next(9, 8, 7)(api)
      |Ship("histos")
      |  .next(
      |    0,
      |    2,
      |    9
      |  )(api, app).toolchain(launcher = app, client = client)
      |Ship("later").next(1, 0, 0)(api, app)
      |""".stripMargin

  private val earlyFirst =
    """Ship("early").next(1, 2, 3)(api, app).toolchain(launcher = app, client = client)
      |Ship("histos").next(0, 2, 9)(api, app)
      |""".stripMargin

  def spec = suite("api clock laws")(
    test("publishing reads the first ship that names toolchain"):
      assertTrue(
        CommitSha.from(head).map(SnapshotId.render(histos, _)) == Right(want),
        SnapshotId.publishing(spaced).contains(histos),
        SnapshotId.publishing(earlyFirst).contains(early),
        SnapshotId.publishing("""Ship("lib").next(1, 0, 0)(engine)""").isEmpty,
        SnapshotId.publishing("""ship("histos", ship"0.2.9")(api, app)""").isEmpty,
      )
    ,
    test("a local toolchain warns only when its packed commit is not this checkout"):
      val fresh  = ToolchainIdentity(want, head, false)
      val dirty  = ToolchainIdentity(want, head, true)
      val moved  = ToolchainIdentity(want, other, false)
      val line   = "histos 0.2.8"
      val socket = Path.of("/tmp/server.sock")
      assertTrue(
        ApiClock.decide(false, Some(moved), Some(head)) == ApiClock.Admit.Go,
        ApiClock.decide(true, Some(fresh), Some(head)) == ApiClock.Admit.Go,
        ApiClock.decide(true, Some(moved), Some(head)) ==
          ApiClock.Admit.Warn(ToolchainSkew.line(other, false, head)),
        ApiClock.decide(true, Some(dirty), Some(head)) ==
          ApiClock.Admit.Warn(ToolchainSkew.line(head, true, head)),
        ApiClock.decide(true, None, Some(head)) == ApiClock.Admit.Go,
        ToolchainSkew.line(other, false, head) ==
          s"local toolchain built from $other; this checkout is $head",
        ToolchainSkew.line(head, true, head) ==
          s"local toolchain built from $head (dirty); this checkout is $head",
        ApiClock.versionLines(line) == s"$line\nserver 0.2.8",
        ApiClock.rebootText("histos 0.3.1", "histos 0.2.8", socket.toString) ==
          "rebooting server onto histos 0.3.1 (was histos 0.2.8) at /tmp/server.sock",
        ApiClock.mcpNote(Some(want)) == s"mcp $want",
        ApiClock.mcpNote(None) == "mcp has no toolchain id",
        ApiClock.restart(socket, "histos 0.3.1", "histos 0.2.8") ==
          ServerError.Restart(socket, "histos 0.3.1", "histos 0.2.8"),
      )
    ,
    test("current is the snapshot coordinate of the toolchain ship at HEAD, and reads HEAD only then"):
      work.flatMap { repo =>
        val file = repo.resolve("histos.scala")
        for
          asked   <- Ref.make(List.empty[Path])
          bare    <- SnapshotId.current(repo, reading(asked, Some(WireSamples.commit)))
          _       <- write(file, """ships(Ship("lib").next(0, 2, 9)(api))""")
          library <- SnapshotId.current(repo, reading(asked, Some(WireSamples.commit)))
          _       <- write(
            file,
            """ships(Ship("histos").next(0, 2, 9)(api, app).toolchain(launcher = app, client = client))""",
          )
          headless <- SnapshotId.current(repo, reading(asked, None))
          got      <- SnapshotId.current(repo, reading(asked, Some(WireSamples.commit)))
          reads    <- asked.get
        yield assertTrue(
          bare.isEmpty,
          library.isEmpty,
          headless.isEmpty,
          got.contains(want),
          reads == List(repo, repo),
        )
      },
  )

  /** A HEAD reader that records each checkout it is asked about. */
  private def reading(asked: Ref[List[Path]], head: Option[CommitSha]): Path => UIO[Option[CommitSha]] =
    at => asked.update(_ :+ at).as(head)

  private def write(path: Path, text: String): ZIO[Any, Throwable, Unit] =
    ZIO.attemptBlocking {
      Files.createDirectories(path.getParent)
      Files.writeString(path, text, StandardCharsets.UTF_8)
    }.unit

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
