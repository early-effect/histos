package histos.client

import histos.bsp.Frame
import histos.build.BuildEvent
import histos.cli.{CliExit, CompileConsole, Show}
import histos.server.{Call, Message, ServerError}
import histos.report.CallResult

import java.nio.file.{Files, Path, StandardOpenOption}

import zio.json.*
import zio.{Console, ExitCode, IO, Scope, ZIO, ZIOAppDefault, durationInt}

/** Batch commands. `server`, `bsp`, and `mcp` never reach this process. Zinc stays in the JVM server. */
object Main extends ZIOAppDefault:
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
      error => Console.printLineError(error.message).ignore.as(CliExit.LoadFailed.code),
      code => ZIO.succeed(code),
    )

  private def talk(repo: Path, call: Call): ZIO[Scope, ServerError, Int] =
    readRecord(repo).flatMap:
      case None         => boot(repo).flatMap(socket => exchange(repo, socket, call))
      case Some(record) =>
        version(record).flatMap: ok =>
          if !ok then ZIO.fail(ServerError.DifferentVersion)
          else
            val socket = Path.of(record.socket)
            exchange(repo, socket, call).catchSome:
              case ServerError.Unreachable(_) =>
                boot(repo).flatMap(fresh => exchange(repo, fresh, call))

  private def exchange(repo: Path, socket: Path, call: Call): ZIO[Scope, ServerError, Int] =
    ZIO
      .acquireRelease(UnixPipe.connect(socket))(session => ZIO.succeed(session.close()))
      .flatMap: session =>
        for
          _ <- Frame
            .write(session.output, call.toJson)
            .mapError(_ => ServerError.Io(histos.server.ServerIo.Write(socket)))
          head <- Frame.read(session.input).mapError(_ => ServerError.Io(histos.server.ServerIo.Read(socket)))
          _    <- ZIO.fromEither(ready(head))
          code <- frames(repo, socket, session.input, call)
        yield code

  private def ready(frame: Option[String]): Either[ServerError, Unit] =
    frame.flatMap(_.fromJson[Message].toOption) match
      case Some(Message.Ready) => Right(())
      case _                   => Left(ServerError.Protocol)

  private def frames(
      repo: Path,
      socket: Path,
      input: java.io.InputStream,
      call: Call,
  ): IO[ServerError, Int] =
    Frame
      .read(input)
      .mapError(_ => ServerError.Io(histos.server.ServerIo.Read(socket)))
      .flatMap:
        case None          => ZIO.fail(ServerError.Protocol)
        case Some(payload) =>
          payload.fromJson[Message] match
            case Right(Message.Event(event)) =>
              show(repo, event) *> frames(repo, socket, input, call)
            case Right(Message.Done(result)) => finish(call, result)
            case _                           => ZIO.fail(ServerError.Protocol)

  private def show(repo: Path, event: BuildEvent): IO[Nothing, Unit] =
    CompileConsole.show(
      Show(repo, event),
      line => Console.printLine(line).orDie,
      line => Console.printLineError(line).orDie,
    )

  private def finish(call: Call, result: CallResult): IO[ServerError, Int] =
    val exit = CliExit.from(call, result)
    result match
      case CallResult.Fault(fault) if fault.message.nonEmpty =>
        Console.printLineError(fault.message).orDie.as(exit.code)
      case _ => ZIO.succeed(exit.code)

  private final case class Recorded(version: String, fingerprint: String, socket: String) derives JsonCodec

  private def readRecord(repo: Path): IO[ServerError, Option[Recorded]] =
    val file = repo.resolve(".histos").resolve("server.json")
    ZIO
      .attemptBlocking {
        if !Files.isRegularFile(file) then None
        else Files.readString(file).fromJson[Recorded].toOption
      }
      .mapError(_ => ServerError.Io(histos.server.ServerIo.Read(file)))

  private def version(record: Recorded): IO[ServerError, Boolean] =
    ZIO
      .attempt(Option(System.getenv("HISTOS_VERSION")).filter(_.nonEmpty))
      .mapError(_ => ServerError.Protocol)
      .map:
        case None        => true
        case Some(found) => found == record.version

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
end Main
