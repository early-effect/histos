package histos.server

import histos.{LawMachine, Resolution}
import histos.bsp.Frame
import histos.cache.ContentStore
import histos.cli.{Cli, CliExit, Completed, Playback}
import histos.client.Session
import histos.load.Loader
import histos.publish.IvyRoot
import histos.report.CallResult
import histos.resolve.Resolve
import histos.workspace.Workspace
import slayer.stubbed

import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.{Channels, ServerSocketChannel}
import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

/** A mismatched record is rebooted onto the command's own version and jar. The call runs on the replacement, not on the
  * driver that was already up. The old driver records the replacement before it answers the reboot, so the wait for it
  * never sleeps on the test clock.
  */
object DriverLawSpec extends ZIOSpecDefault:
  private val pin   = "histos.version=1.2.3\n"
  private val shown = "histos 1.2.3"
  private val sha   = "ab" * 32

  def spec = suite("driver laws")(
    test("a different version reboots and the call runs on the replacement"):
      drive(recorded = "stale", mark = None)
    ,
    test("a new assembly checksum reboots when the version line matches"):
      drive(recorded = shown, mark = Some(sha))
    ,
    test("the MCP reboot of a different version reboots once, and the replacement hears nothing"):
      replaced("stale", None): repo =>
        ZIO.scoped:
          Session.desk.flatMap: desk =>
            Session
              .perform(repo, Path.of("api.jar"), LawMachine.idle, desk, Call.Reboot(Successor(Some(shown), None)))
              .either
              .provideSomeLayer[Scope](
                stubbed[Loader] ++ stubbed[Resolve] ++ stubbed[ContentStore] ++
                  ZLayer.succeed(IvyRoot.at(Path.of("ivy-never-written")))
              )
      .map: (result, calls, jar) =>
        val reported = result match
          case Right(CallResult.Reported(_)) => true
          case _                             => false
        assertTrue(reported, calls == Chunk(Call.encode(Call.Reboot(Successor(Some(shown), Some(jar)))))),
  ) @@ TestAspect.timeout(20.seconds)

  private def drive(recorded: String, mark: Option[String]): UIO[TestResult] =
    replaced(recorded, mark): repo =>
      Ref
        .make(0)
        .flatMap: missed =>
          Cli
            .compileAt(
              repo,
              Nil,
              missed.update(_ + 1).as(Completed(CliExit.Success, "miss", "", Playback.Held)),
            )
            .provideLayer(LawMachine.redirected)
            .zip(missed.get)
    .map: (done, calls, jar) =>
      val (completed, n) = done
      assertTrue(
        n == 0,
        completed.exit == CliExit.Success,
        completed.out != "miss",
        calls == Chunk(Call.encode(Call.Reboot(Successor(Some(shown), Some(jar)))), Call.encode(Call.Compile(Nil))),
      )

  /** A record names an old peer under the law's directory. The old peer writes a record for the next peer before it
    * answers, as a server that relaunches does. Each peer answers one call. The requester runs as `shown` with the
    * law's jar, and `act` is what it does. The result is what `act` returned, every call the peers heard, and that jar.
    */
  private def replaced[A](recorded: String, mark: Option[String])(act: Path => UIO[A]): UIO[(A, Chunk[String], Path)] =
    ZIO.scoped {
      for
        root <- short
        repo = root.resolve("repo")
        old  = root.resolve("old")
        next = root.resolve("next")
        heard <- Ref.make(Chunk.empty[String])
        _     <- ZIO.attemptBlocking {
          List(repo, old, next).foreach(dir => Files.createDirectories(dir))
          Files.writeString(repo.resolve("build.properties"), pin)
          mark.foreach { value =>
            Files.createDirectories(AssemblyMark.file(repo).getParent)
            Files.writeString(AssemblyMark.file(repo), value + "\n")
          }
        }.orDie
        fingerprint <- must(Fingerprint.read(repo, Resolution.Solve))
        oldSocket  = old.resolve("s")
        nextSocket = next.resolve("s")
        _        <- must(Record.write(repo, recorded, fingerprint, oldSocket, None))
        oldPeer  <- listen(oldSocket)
        nextPeer <- listen(nextSocket)
        _        <- answer(
          oldPeer,
          heard,
          must(Record.write(repo, shown, fingerprint, nextSocket, mark)),
        ).forkScoped
        _ <- answer(nextPeer, heard, ZIO.unit).forkScoped
        jar = root.resolve("next.jar")
        _     <- TestSystem.putEnv("HISTOS_VERSION", shown)
        _     <- TestSystem.putEnv("HISTOS_ASSEMBLY", jar.toString)
        done  <- act(repo)
        calls <- heard.get
      yield (done, calls, jar)
    }

  /** The law's own directory, removed after it. A Unix socket path fits in about 100 bytes, so it is under `/tmp`. */
  private def short: ZIO[Scope, Nothing, Path] =
    ZIO.acquireRelease(
      ZIO.attemptBlocking(Files.createTempDirectory(Path.of("/tmp"), "hd")).orDie
    )(path => Workspace.delete(path).catchAll(error => ZIO.dieMessage(error.message)))

  private def listen(socket: Path): ZIO[Scope, Nothing, ServerSocketChannel] =
    ZIO.acquireRelease(
      ZIO.attemptBlocking {
        Files.deleteIfExists(socket)
        val server = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
        server.bind(UnixDomainSocketAddress.of(socket))
        server
      }.orDie
    )(server => ZIO.attempt(server.close()).ignore)

  private def answer(
      server: ServerSocketChannel,
      heard: Ref[Chunk[String]],
      before: UIO[Unit],
  ): UIO[Unit] =
    // The accept stays interruptible, so a peer nobody calls ends with the law.
    ZIO
      .attemptBlockingInterrupt(server.accept())
      .orDie
      .flatMap { channel =>
        val input  = Channels.newInputStream(channel)
        val output = Channels.newOutputStream(channel)
        Frame
          .read(input)
          .flatMap {
            case Some(payload) =>
              heard.update(_ :+ payload) *>
                before *>
                Frame.write(output, Say.wire(Message.Ready)) *>
                Frame.write(output, Say.wire(Message.Done(CallResult.reported())))
            case None => ZIO.unit
          }
          .catchAll(error => ZIO.dieMessage(error.toString))
          .ensuring(ZIO.attempt(channel.close()).ignore)
      }

  private def must[A](effect: IO[ServerError, A]): UIO[A] =
    effect.catchAll(error => ZIO.dieMessage(error.message))
end DriverLawSpec
