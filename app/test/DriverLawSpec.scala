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

/** A mismatched version is rebooted onto the command's own version. The call runs on the replacement, not on the driver
  * that was already up. The old driver records the replacement before it answers the reboot, so the wait for it never
  * sleeps on the test clock.
  */
object DriverLawSpec extends ZIOSpecDefault:
  private val pin   = "histos.version=1.2.3\n"
  private val shown = "histos 1.2.3"

  def spec = suite("driver laws")(
    test("a different version reboots and the call runs on the replacement"):
      drive("stale")
    ,
    test("a matching version stays on the recorded server"):
      drive(shown)
    ,
    test("the MCP reboot of a different version reboots once, and the replacement hears nothing"):
      replaced("stale"): repo =>
        ZIO.scoped:
          Session.desk.flatMap: desk =>
            Session
              .perform(repo, Path.of("api.jar"), LawMachine.idle, desk, Call.Reboot(Successor(Some(shown))))
              .either
              .provideSomeLayer[Scope](
                stubbed[Loader] ++ stubbed[Resolve] ++ stubbed[ContentStore] ++
                  ZLayer.succeed(IvyRoot.at(Path.of("ivy-never-written")))
              )
      .map: (result, calls) =>
        val reported = result match
          case Right(CallResult.Reported(_)) => true
          case _                             => false
        assertTrue(reported, calls == Chunk(Call.encode(Call.Reboot(Successor(Some(shown)))))),
  ) @@ TestAspect.timeout(20.seconds)

  private def drive(recorded: String): UIO[TestResult] =
    replaced(recorded): repo =>
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
    .map: (done, calls) =>
      val (completed, n) = done
      val expected       =
        if recorded == shown then Chunk(Call.encode(Call.Compile(Nil)))
        else Chunk(Call.encode(Call.Reboot(Successor(Some(shown)))), Call.encode(Call.Compile(Nil)))
      assertTrue(
        n == 0,
        completed.exit == CliExit.Success,
        completed.out != "miss",
        calls == expected,
      )

  /** A record names an old peer under the law's directory. The old peer writes a record for the next peer before it
    * answers, as a server that relaunches does. Each peer answers one call. The requester runs as `shown`, and `act` is
    * what it does. The result is what `act` returned and every call the peers heard.
    */
  private def replaced[A](recorded: String)(act: Path => UIO[A]): UIO[(A, Chunk[String])] =
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
          must(Record.write(repo, shown, fingerprint, nextSocket, None)),
        ).forkScoped
        _     <- answer(nextPeer, heard, ZIO.unit).forkScoped
        _     <- TestSystem.putEnv("HISTOS_VERSION", shown)
        done  <- act(repo)
        calls <- heard.get
      yield (done, calls)
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
