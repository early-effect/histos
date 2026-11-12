package histos.server

import histos.{LawMachine, Resolution}
import histos.bsp.Frame
import histos.cli.{Cli, CliExit, Completed, Playback}
import histos.report.CallResult
import histos.workspace.Workspace

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
      drive(recorded = shown, mark = Some(sha)),
  ) @@ TestAspect.timeout(20.seconds)

  private def drive(recorded: String, mark: Option[String]): UIO[TestResult] =
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
        _      <- TestSystem.putEnv("HISTOS_VERSION", shown)
        _      <- TestSystem.putEnv("HISTOS_ASSEMBLY", jar.toString)
        missed <- Ref.make(0)
        done   <- Cli
          .compileAt(
            repo,
            Nil,
            missed.update(_ + 1).as(Completed(CliExit.Success, "miss", "", Playback.Held)),
          )
          .provideLayer(LawMachine.redirected)
        n     <- missed.get
        calls <- heard.get
      yield assertTrue(
        n == 0,
        done.exit == CliExit.Success,
        done.out != "miss",
        calls == Chunk(Call.encode(Call.Reboot(Successor(Some(shown), Some(jar)))), Call.encode(Call.Compile(Nil))),
      )
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
    ZIO.scoped {
      ZIO
        .acquireRelease(ZIO.attemptBlockingInterrupt(server.accept()).orDie)(channel =>
          ZIO.attempt(channel.close()).ignore
        )
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
        }
    }

  private def must[A](effect: IO[ServerError, A]): UIO[A] =
    effect.catchAll(error => ZIO.dieMessage(error.message))
end DriverLawSpec
