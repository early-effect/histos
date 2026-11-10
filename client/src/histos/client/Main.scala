package histos.client

import histos.bsp.Frame
import histos.build.BuildEvent
import histos.cli.{CliExit, CompileConsole, Show}
import histos.server.{AssemblyMark, Call, Message, ServerError, ServerFile}
import histos.report.CallResult

import java.nio.file.{Files, Path, StandardOpenOption}

import scala.annotation.tailrec
import scala.scalanative.libc.signal
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.CInt

import zio.json.*
import zio.{ExitCode, IO, Scope, ZIO, ZIOAppDefault, durationInt}

/** Batch commands. `server`, `bsp`, and `mcp` never reach this process. Zinc stays in the JVM server.
  *
  * ZIO never installs its shutdown hook on Scala Native, and nothing there consults `gracefulShutdownTimeout`.
  * Returning from `run` calls `exit`, which can hang while the pool and the GC threads are still up. This process
  * flushes and calls `_exit` after a batch session and after the JVM child returns. SIGINT and SIGTERM do the same.
  */
object Main extends ZIOAppDefault:
  signal.signal(signal.SIGINT, onSignal(_))
  signal.signal(signal.SIGTERM, onSignal(_))

  private def onSignal(sig: CInt): Unit =
    unistd._exit((128 + sig).toInt)

  override def run =
    for
      args <- ZIO.serviceWith[zio.ZIOAppArgs](_.getArgs)
      code <- Batch.parse(args.toList) match
        case None       => jvm(args.toList)
        case Some(call) => batch(call)
    yield ExitCode(code)

  private def batch(call: Call): ZIO[Scope, Nothing, Int] =
    val repo = Path.of("").toAbsolutePath.normalize
    talk(repo, call).foldZIO(
      error => ZIO.succeed(leave(CliExit.LoadFailed.code, Some(error.message))),
      code => ZIO.succeed(leave(code, None)),
    )

  /** Print `line` when one is present, then `_exit`. `_exit` does not return. The code keeps the caller's type. */
  private def leave(code: Int, line: Option[String]): Int =
    line.foreach(text => System.err.println(text))
    halt(code)

  private def halt(code: Int): Int =
    System.out.flush()
    System.err.flush()
    unistd._exit(code)
    code

  private def talk(repo: Path, call: Call): ZIO[Scope, ServerError, Int] =
    readRecord(repo).flatMap:
      case None         => boot(repo).flatMap(socket => exchange(repo, socket, call))
      case Some(record) =>
        agrees(repo, record).flatMap: ok =>
          val socket = Path.of(record.socket)
          if ok then
            exchange(repo, socket, call).catchSome:
              case ServerError.Unreachable(_) =>
                boot(repo).flatMap(fresh => exchange(repo, fresh, call))
          else replace(repo, socket, call)

  private def exchange(repo: Path, socket: Path, call: Call): ZIO[Scope, ServerError, Int] =
    ZIO
      .acquireRelease(UnixPipe.connect(socket))(session => ZIO.succeed(session.close()))
      .flatMap: session =>
        ZIO
          .attemptBlocking(drive(repo, socket, session, call))
          .mapError(_ => ServerError.Io(histos.server.ServerIo.Read(socket)))
          .flatMap(ZIO.fromEither)

  private def drive(
      repo: Path,
      socket: Path,
      session: UnixPipe.Session,
      call: Call,
  ): Either[ServerError, Int] =
    Frame.put(session.output, call.toJson) match
      case Left(_)  => Left(ServerError.Io(histos.server.ServerIo.Write(socket)))
      case Right(_) =>
        Frame.take(session.input) match
          case Left(_)     => Left(ServerError.Io(histos.server.ServerIo.Read(socket)))
          case Right(head) =>
            ready(head) match
              case Left(error) => Left(error)
              case Right(_)    => frames(repo, socket, session.input, call)

  private def ready(frame: Option[String]): Either[ServerError, Unit] =
    frame.flatMap(_.fromJson[Message].toOption) match
      case Some(Message.Ready) => Right(())
      case _                   => Left(ServerError.Protocol)

  @tailrec
  private def frames(
      repo: Path,
      socket: Path,
      input: java.io.InputStream,
      call: Call,
  ): Either[ServerError, Int] =
    Frame.take(input) match
      case Left(_)              => Left(ServerError.Io(histos.server.ServerIo.Read(socket)))
      case Right(None)          => Left(ServerError.Protocol)
      case Right(Some(payload)) =>
        payload.fromJson[Message] match
          case Right(Message.Event(event)) =>
            show(repo, event)
            frames(repo, socket, input, call)
          case Right(Message.Done(result)) => Right(finish(call, result))
          case _                           => Left(ServerError.Protocol)

  private def show(repo: Path, event: BuildEvent): Unit =
    Show(repo, event).foreach:
      case CompileConsole.Line.Out(text) => System.out.println(text)
      case CompileConsole.Line.Err(text) => System.err.println(text)

  private def finish(call: Call, result: CallResult): Int =
    val exit = CliExit.from(call, result)
    result match
      case CallResult.Fault(fault) =>
        val text = histos.doctor.FaultText.show(fault.diagnose)
        if text.nonEmpty then System.err.println(text)
        exit.code
      case _ => exit.code

  private def replace(repo: Path, socket: Path, call: Call): ZIO[Scope, ServerError, Int] =
    exchange(repo, socket, Call.Reboot).ignore *>
      boot(repo).flatMap { fresh =>
        readRecord(repo).flatMap {
          case Some(record) =>
            agrees(repo, record).flatMap: ok =>
              if ok then exchange(repo, fresh, call)
              else ZIO.fail(ServerError.DifferentVersion)
          case None =>
            exchange(repo, fresh, call)
        }
      }

  private def readRecord(repo: Path): IO[ServerError, Option[ServerFile]] =
    val file = repo.resolve(".histos").resolve("server.json")
    ZIO
      .attemptBlocking {
        if !Files.isRegularFile(file) then None
        else ServerFile.parse(Files.readString(file))
      }
      .mapError(_ => ServerError.Io(histos.server.ServerIo.Read(file)))

  /** Unset `HISTOS_VERSION` is a client the wrapper did not launch. It does not judge the server. */
  private def agrees(repo: Path, record: ServerFile): IO[ServerError, Boolean] =
    ZIO
      .attempt(Option(System.getenv("HISTOS_VERSION")).filter(_.nonEmpty))
      .mapError(_ => ServerError.Protocol)
      .flatMap:
        case None        => ZIO.succeed(true)
        case Some(found) =>
          AssemblyMark.read(repo).map(mark => found == record.version && mark == record.assembly)

  /** One starter holds the gate. Everyone waits until `server.sock` accepts. */
  private def boot(repo: Path): IO[ServerError, Path] =
    val home   = repo.resolve(".histos")
    val gate   = home.resolve("server.starting")
    val log    = home.resolve("server.log")
    val socket = home.resolve("server.sock")
    ZIO.acquireReleaseWith(
      ZIO
        .attemptBlocking {
          Files.createDirectories(home)
          java.nio.channels.FileChannel.open(gate, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
        }
        .mapError(_ => ServerError.Io(histos.server.ServerIo.Bind(gate)))
    )(channel => ZIO.attemptBlocking(channel.close()).ignore): channel =>
      ZIO
        .attemptBlocking(Option(channel.tryLock()))
        .mapError(_ => ServerError.Io(histos.server.ServerIo.Bind(gate)))
        .flatMap:
          case None    => untilUp(socket)
          case Some(_) => spawn(repo, log) *> untilUp(socket)

  private def spawn(repo: Path, log: Path): IO[ServerError, Unit] =
    ZIO
      .attemptBlocking {
        Option(log.getParent).foreach(dir => Files.createDirectories(dir))
        if !Files.exists(log) then Files.createFile(log)
        val java = Option(System.getenv("HISTOS_JAVA")).filter(_.nonEmpty).getOrElse("java")
        val jar  = Option(System.getenv("HISTOS_JAR"))
          .filter(_.nonEmpty)
          .getOrElse(
            repo.resolve("dist").resolve("histos.jar").toString
          )
        val command = java :: "--enable-native-access=ALL-UNNAMED" :: "-jar" :: jar :: "server" :: Nil
        new ProcessBuilder(command*)
          .directory(repo.toFile)
          .redirectInput(ProcessBuilder.Redirect.from(Path.of("/dev/null").toFile))
          .redirectErrorStream(true)
          .redirectOutput(ProcessBuilder.Redirect.appendTo(log.toFile))
          .start()
        ()
      }
      .mapError(_ => ServerError.Io(histos.server.ServerIo.Bind(log)))

  private def untilUp(socket: Path): IO[ServerError, Path] =
    def poll(left: Int): IO[ServerError, Path] =
      UnixPipe
        .connect(socket)
        .foldZIO(
          {
            case ServerError.Unreachable(_) if left > 0 =>
              ZIO.sleep(50.millis) *> poll(left - 1)
            case other => ZIO.fail(other)
          },
          session => ZIO.succeed(session.close()).as(socket),
        )
    poll(1200)

  private def jvm(args: List[String]): ZIO[Any, Nothing, Int] =
    ZIO
      .attemptBlocking {
        val java = Option(System.getenv("HISTOS_JAVA")).filter(_.nonEmpty).getOrElse("java")
        val jar  = Option(System.getenv("HISTOS_JAR"))
          .filter(_.nonEmpty)
          .getOrElse(
            Path.of("").toAbsolutePath.normalize.resolve("dist").resolve("histos.jar").toString
          )
        val command = java :: "--enable-native-access=ALL-UNNAMED" :: "-jar" :: jar :: args
        new ProcessBuilder(command*).inheritIO().start().waitFor()
      }
      .orElseSucceed(1)
      .map(halt)
end Main
