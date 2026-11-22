package histos.client

import histos.ApiClock
import histos.bsp.Frame
import histos.build.BuildEvent
import histos.cli.{Appearance, CliExit, ColorFlag, CompileConsole, Paint, Peeled, Show}
import histos.server.{
  Attendance,
  Call,
  CatalogQuery,
  Message,
  RunBytes,
  RunLaunch,
  ServerError,
  ServerFile,
  ServerHandoff,
  ServerStamp,
  ServerWait,
}
import histos.report.CallResult
import histos.report.WireJson.given zio.json.JsonCodec[histos.lock.InvocationEnvelope]

import java.nio.file.{Files, Path, StandardOpenOption}

import scala.jdk.CollectionConverters.*
import scala.scalanative.libc.signal
import scala.scalanative.libc.stdlib
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.{CInt, Ptr}
import scala.scalanative.unsigned.*

import zio.json.*
import zio.{ExitCode, IO, Runtime, Scope, UIO, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer, durationInt}

/** Batch commands. `server`, `bsp`, and `mcp` never reach this process. Zinc stays in the JVM server.
  *
  * ZIO never installs its shutdown hook on Scala Native, and nothing there consults `gracefulShutdownTimeout`.
  * Returning from `run` calls `exit`, which can hang while the pool and the GC threads are still up. This process
  * flushes and calls `_exit` after a batch session, after the JVM child returns, and after a fatal error. SIGINT and
  * SIGTERM do the same.
  */
object Main extends ZIOAppDefault:
  private val interruptNote: Ptr[Byte] =
    val note = stdlib.malloc(1.toCSize)
    if note != null then note(0) = 'I'.toByte
    note

  @volatile private var armed           = false
  @volatile private var pass            = false
  @volatile private var hits            = 0
  @volatile private var wakeWrite: CInt = -1

  signal.signal(signal.SIGINT, onSignal(_))
  signal.signal(signal.SIGTERM, onSignal(_))

  /** ZIO on Native rethrows a fatal error into the worker thread, which dies without ending its fiber, so the run would
    * wait forever. `leave` ends the process first. ZIO's own handler after it only gives the hook its `Nothing`.
    */
  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
    Runtime.setReportFatal: thrown =>
      val fatal = Fatal.of(thrown)
      leave(fatal.exit.code, Some(fatal.line))
      Runtime.defaultReportFatal(thrown)

  /** `SIGTERM` always ends the process. The first `SIGINT` during a run is a frame. The second ends the process. While
    * this process is waiting on the jar, `SIGINT` belongs to that jar.
    */
  private def onSignal(sig: CInt): Unit =
    if sig == signal.SIGTERM then unistd._exit((128 + sig).toInt)
    else if pass then ()
    else if !armed || wakeWrite < 0 || interruptNote == null then unistd._exit((128 + sig).toInt)
    else
      hits += 1
      if hits >= 2 then unistd._exit(130)
      else unistd.write(wakeWrite, interruptNote, 1.toCSize)

  /** The stdin relay calls these around a program run. A signal handler only writes the interrupt byte. */
  private[client] def arm(fd: CInt): Unit =
    wakeWrite = fd
    hits = 0
    armed = true

  private[client] def disarm(): Unit =
    armed = false
    wakeWrite = -1

  override def run =
    for
      args <- ZIO.serviceWith[zio.ZIOAppArgs](_.getArgs)
      raw    = args.toList
      peeled = ColorFlag.peel(raw)
      code <- Batch.parse(peeled.args) match
        case None                                           => jvm(raw)
        case Some(Batch.Parsed.Ready(call))                 => batch(call, seen(peeled))
        case Some(Batch.Parsed.Program(id, platform, argv)) =>
          batch(Call.Run(id, platform, shell, argv), seen(peeled))
        case Some(Batch.Parsed.Names(query)) =>
          query match
            case None        => ZIO.succeed(halt(0))
            case Some(asked) => catalog(asked)
    yield ExitCode(code)

  /** Names from a server that is already listening. No record, a different version, or any failure prints nothing and
    * does not start a server or a JVM.
    */
  private def catalog(query: CatalogQuery): ZIO[Scope, Nothing, Int] =
    val repo  = Path.of("").toAbsolutePath.normalize
    val quiet = ZIO.succeed(halt(0))
    readRecord(repo).foldZIO(
      _ => quiet,
      {
        case None         => quiet
        case Some(record) =>
          agrees(record).foldZIO(
            _ => quiet,
            ok =>
              if !ok then quiet
              else
                val socket = Path.of(record.socket)
                listening(socket).flatMap { live =>
                  if !live then quiet
                  else exchange(repo, socket, Call.Catalog(query), Appearance.plain).foldZIO(_ => quiet, _ => quiet)
                },
          )
      },
    )

  /** A terminal and an unset `NO_COLOR` take color. `--truecolor` does not turn color on. */
  private def seen(peeled: Peeled): Appearance =
    val terminal  = unistd.isatty(1) == 1
    val noColor   = Option(System.getenv("NO_COLOR"))
    val colorTerm = Option(System.getenv("COLORTERM"))
    Appearance(
      Paint.decide(peeled.color, terminal, noColor),
      Paint.depth(peeled.truecolor, colorTerm),
      peeled.log,
    )

  /** This process's environment and working directory. The program's streams stay pipes. */
  private def shell: RunLaunch.Shell =
    RunLaunch.Shell(environment, Path.of("").toAbsolutePath)

  private def environment: Map[String, String] =
    val raw = System.getenv()
    if raw == null then Map.empty else raw.asScala.toMap

  private def batch(call: Call, appearance: Appearance): ZIO[Scope, Nothing, Int] =
    val repo = Path.of("").toAbsolutePath.normalize
    ApiClock.admit(repo).flatMap { admitted =>
      val noted = admitted match
        case ApiClock.Admit.Warn(line)          => ZIO.succeed(java.lang.System.err.println(line))
        case ApiClock.Admit.Rebuild(_, fact, _) => ZIO.succeed(java.lang.System.err.println(fact))
        case ApiClock.Admit.Go                  => ZIO.unit
      noted *>
        talk(repo, call, appearance).foldZIO(
          error => ZIO.succeed(leave(CliExit.Failed.code, Some(error.message))),
          code => ZIO.succeed(leave(code, None)),
        )
    }

  /** Print `line` when one is present, then `_exit`. `_exit` does not return. The code keeps the caller's type. */
  private def leave(code: Int, line: Option[String]): Int =
    line.foreach(text => System.err.println(text))
    halt(code)

  private def halt(code: Int): Int =
    System.out.flush()
    System.err.flush()
    unistd._exit(code)
    code

  private def talk(repo: Path, call: Call, appearance: Appearance): ZIO[Scope, ServerError, Int] =
    readRecord(repo).flatMap:
      case None         => boot(repo).flatMap(socket => exchange(repo, socket, call, appearance))
      case Some(record) =>
        agrees(record).flatMap: ok =>
          val socket = Path.of(record.socket)
          if ok then
            exchange(repo, socket, call, appearance).catchSome:
              case ServerError.Unreachable(_) =>
                listening(socket).flatMap { live =>
                  if live then ZIO.fail(ServerError.Unreachable(socket))
                  else clear(repo) *> boot(repo).flatMap(fresh => exchange(repo, fresh, call, appearance))
                }
          else replace(repo, socket, record.version, call, appearance)

  private def exchange(repo: Path, socket: Path, call: Call, appearance: Appearance): ZIO[Scope, ServerError, Int] =
    ZIO
      .acquireRelease(UnixPipe.connect(socket))(session => ZIO.succeed(session.close()))
      .flatMap: session =>
        drive(repo, socket, session, call, appearance).flatMap(ZIO.fromEither)

  private def drive(
      repo: Path,
      socket: Path,
      session: UnixPipe.Session,
      call: Call,
      appearance: Appearance,
  ): ZIO[Scope, Nothing, Either[ServerError, Int]] =
    val prompts = unistd.isatty(0) == 1 && unistd.isatty(1) == 1
    CallerFacts.native(call, environment, prompts).flatMap { posted =>
      ZIO.attemptBlocking(Frame.put(session.output, posted.toJson)).orDie.flatMap {
        case Left(_)  => ZIO.succeed(Left(ServerError.Io(histos.server.ServerIo.Write(socket))))
        case Right(_) =>
          ZIO.attemptBlocking(Frame.take(session.input)).orDie.flatMap {
            case Left(_)     => ZIO.succeed(Left(ServerError.Io(histos.server.ServerIo.Read(socket))))
            case Right(head) =>
              ready(head) match
                case Left(error) => ZIO.succeed(Left(error))
                case Right(_)    =>
                  copyInput(call, session.output) *> frames(repo, socket, session.input, Show.start, appearance)
          }
      }
    }

  /** Stdin frames start after the call is written, so this fiber is the socket's only writer. */
  private def copyInput(call: Call, output: java.io.OutputStream): ZIO[Scope, Nothing, Unit] =
    call.attendance match
      case Attendance.Program                 => StdinRelay.attach(output)
      case Attendance.Held | Attendance.Quiet => ZIO.unit

  private def ready(frame: Option[String]): Either[ServerError, Unit] =
    frame.flatMap(_.fromJson[Message].toOption) match
      case Some(Message.Ready) => Right(())
      case _                   => Left(ServerError.Protocol)

  private def frames(
      repo: Path,
      socket: Path,
      input: java.io.InputStream,
      shown: Show.Shown,
      appearance: Appearance,
  ): ZIO[Any, Nothing, Either[ServerError, Int]] =
    ZIO.attemptBlocking(Frame.take(input)).orDie.flatMap {
      case Left(_)              => ZIO.succeed(Left(ServerError.Io(histos.server.ServerIo.Read(socket))))
      case Right(None)          => ZIO.succeed(Left(ServerError.Protocol))
      case Right(Some(payload)) =>
        payload.fromJson[Message] match
          case Right(Message.Event(BuildEvent.Layout(width))) =>
            frames(repo, socket, input, shown.copy(column = width), appearance)
          case Right(Message.Event(BuildEvent.Theme(value))) =>
            frames(repo, socket, input, shown.copy(theme = value.orElse(histos.theme.Theme.standard)), appearance)
          case Right(Message.Event(event)) =>
            ZIO.succeed(show(repo, event, shown, appearance)) *>
              frames(repo, socket, input, shown, appearance)
          case Right(Message.Output(encoded)) =>
            RunBytes.decode(encoded) match
              case Some(bytes) =>
                ZIO.succeed {
                  System.out.write(bytes.toArray)
                  System.out.flush()
                } *> frames(repo, socket, input, shown, appearance)
              case None => ZIO.succeed(Left(ServerError.Protocol))
          case Right(Message.Done(result)) => ZIO.succeed(Right(finish(result)))
          case _                           => ZIO.succeed(Left(ServerError.Protocol))
    }

  private def show(repo: Path, event: BuildEvent, shown: Show.Shown, appearance: Appearance): Unit =
    Show(repo, event, shown.column, appearance.log).foreach:
      case line: CompileConsole.Line.Out =>
        System.out.println(Paint.paint(line.spans, shown.theme, appearance))
      case line: CompileConsole.Line.Err =>
        System.err.println(Paint.paint(line.spans, shown.theme, appearance))

  private def finish(result: CallResult): Int =
    val exit = CliExit.from(result)
    result match
      case CallResult.Fault(fault) =>
        val text = histos.doctor.FaultText.show(fault.diagnose)
        if text.nonEmpty then System.err.println(text)
        exit.code
      case _ => exit.code

  /** Stop the recorded server and start this command's, under one lock. The record is removed by the server as it
    * exits, and the next server writes the next record.
    */
  private def replace(
      repo: Path,
      socket: Path,
      was: String,
      call: Call,
      appearance: Appearance,
  ): ZIO[Scope, ServerError, Int] =
    val line = ApiClock.stopText(was, socket.toString)
    ZIO.succeed(java.lang.System.err.println(line)) *>
      handoff(repo, socket, appearance).flatMap(fresh => exchange(repo, fresh, call, appearance))

  private def handoff(
      repo: Path,
      socket: Path,
      appearance: Appearance,
  ): ZIO[Scope, ServerError, Path] =
    val home = repo.resolve(".histos")
    val gate = home.resolve("server.starting")
    val log  = home.resolve("server.log")
    ServerStamp.note(repo).flatMap { before =>
      held(home, gate)(
        won = readRecord(repo).flatMap {
          case Some(record) =>
            agrees(record).flatMap { ok =>
              val current = Path.of(record.socket)
              if ok then ZIO.succeed(current)
              else stopThenStart(repo, current, log, before, appearance)
            }
          case None => startChild(repo, socket, log, before)
        },
        lost = untilUp(socket) *> awaitWritten(repo, socket, before) *> agreed(repo, socket),
      )
    }

  /** The server at `socket` exits, then this command's server starts. A record with nobody listening is removed here.
    */
  private def stopThenStart(
      repo: Path,
      socket: Path,
      log: Path,
      before: Option[java.nio.file.attribute.FileTime],
      appearance: Appearance,
  ): ZIO[Scope, ServerError, Path] =
    listening(socket).flatMap { live =>
      ServerHandoff.step(recorded = true, listening = live, same = false) match
        case ServerHandoff.Step.Use   => ZIO.succeed(socket)
        case ServerHandoff.Step.Start =>
          clear(repo) *> startChild(repo, socket, log, before)
        case ServerHandoff.Step.Stop =>
          exchange(repo, socket, Call.Stop, appearance).either.flatMap { result =>
            readRecord(repo).flatMap {
              case None    => startChild(repo, socket, log, before)
              case Some(_) =>
                result match
                  case Right(_) => awaitGone(repo, socket) *> startChild(repo, socket, log, before)
                  case Left(_)  =>
                    listening(socket).flatMap { still =>
                      if still then ZIO.fail(ServerError.DidNotStop(socket))
                      else clear(repo) *> startChild(repo, socket, log, before)
                    }
            }
          }
    }

  private def agreed(repo: Path, socket: Path): IO[ServerError, Path] =
    readRecord(repo).flatMap {
      case Some(record) =>
        agrees(record).flatMap { ok =>
          if ok then ZIO.succeed(Path.of(record.socket))
          else
            ZIO
              .attempt(Option(System.getenv("HISTOS_VERSION")).filter(_.nonEmpty))
              .mapError(_ => ServerError.Protocol)
              .flatMap { found =>
                val wanted = found match
                  case Some(version) => version
                  case None          => record.version
                ZIO.fail(ApiClock.restart(socket, wanted, record.version))
              }
        }
      case None => ZIO.fail(ServerError.Unreachable(socket))
    }

  private def awaitGone(repo: Path, socket: Path): IO[ServerError, Unit] =
    def poll(left: Int): IO[ServerError, Unit] =
      readRecord(repo).flatMap {
        case None                => ZIO.unit
        case Some(_) if left > 0 => ZIO.sleep(50.millis) *> poll(left - 1)
        case Some(_)             => ZIO.fail(ServerError.DidNotStop(socket))
      }
    poll(1200)

  private def listening(socket: Path): UIO[Boolean] =
    UnixPipe
      .connect(socket)
      .fold(
        _ => false,
        session =>
          session.close(); true,
      )

  private def clear(repo: Path): UIO[Unit] =
    ZIO.attemptBlocking(Files.deleteIfExists(ServerStamp.file(repo))).ignore

  private def readRecord(repo: Path): IO[ServerError, Option[ServerFile]] =
    val file = repo.resolve(".histos").resolve("server.json")
    ZIO
      .attemptBlocking {
        if !Files.isRegularFile(file) then None
        else ServerFile.parse(Files.readString(file))
      }
      .mapError(_ => ServerError.Io(histos.server.ServerIo.Read(file)))

  /** Unset `HISTOS_VERSION` is a client the wrapper did not launch. It does not judge the server. */
  private def agrees(record: ServerFile): IO[ServerError, Boolean] =
    ZIO
      .attempt(Option(System.getenv("HISTOS_VERSION")).filter(_.nonEmpty))
      .mapError(_ => ServerError.Protocol)
      .map:
        case None        => true
        case Some(found) => found == record.version

  /** One starter holds the gate. Everyone waits until the checkout's socket accepts. */
  private def boot(repo: Path): IO[ServerError, Path] =
    histos.server.SocketPath.at(repo).flatMap(socket => bootAt(repo, socket))

  private def bootAt(repo: Path, socket: Path): IO[ServerError, Path] =
    val home = repo.resolve(".histos")
    val gate = home.resolve("server.starting")
    val log  = home.resolve("server.log")
    ServerStamp
      .note(repo)
      .flatMap: before =>
        held(home, gate)(
          won = startChild(repo, socket, log, before),
          lost = untilUp(socket) *> awaitWritten(repo, socket, before).as(socket),
        )

  private def held[R](home: Path, gate: Path)(
      won: ZIO[R, ServerError, Path],
      lost: ZIO[R, ServerError, Path],
  ): ZIO[R, ServerError, Path] =
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
          case None    => lost
          case Some(_) => won

  private def startChild(
      repo: Path,
      socket: Path,
      log: Path,
      before: Option[java.nio.file.attribute.FileTime],
  ): IO[ServerError, Path] =
    for
      mark    <- logMark(log)
      process <- spawn(repo, log)
      _       <- ServerWait.awaitListening(
        connect(socket),
        ZIO.attempt(process.isAlive).orDie,
        exitedDetail(log, mark, process),
        socket,
        1200,
      )
      _ <- awaitWritten(repo, socket, before)
    yield socket

  private def spawn(repo: Path, log: Path): IO[ServerError, Process] =
    val missing = ServerError.Io(histos.server.ServerIo.Bind(log))
    ZIO
      .attempt(Option(System.getenv("HISTOS_JAR")).filter(_.nonEmpty))
      .mapError(_ => missing)
      .flatMap:
        case None      => ZIO.fail(missing)
        case Some(jar) =>
          ZIO
            .attemptBlocking {
              Option(log.getParent).foreach(dir => Files.createDirectories(dir))
              if !Files.exists(log) then Files.createFile(log)
              val java    = Option(System.getenv("HISTOS_JAVA")).filter(_.nonEmpty).getOrElse("java")
              val command =
                java :: "--enable-native-access=ALL-UNNAMED" :: "--sun-misc-unsafe-memory-access=allow" :: "-jar" :: jar :: "server" :: Nil
              new ProcessBuilder(command*)
                .directory(repo.toFile)
                .redirectInput(ProcessBuilder.Redirect.from(Path.of("/dev/null").toFile))
                .redirectErrorStream(true)
                .redirectOutput(ProcessBuilder.Redirect.appendTo(log.toFile))
                .start()
            }
            .mapError(_ => missing)

  private def logMark(log: Path): UIO[Long] =
    ZIO.attemptBlocking(if Files.isRegularFile(log) then Files.size(log) else 0L).orDie

  /** Text appended to the log after `mark`. The exit code when that text is empty. */
  private def exitedDetail(log: Path, mark: Long, process: Process): UIO[String] =
    ZIO.attemptBlocking {
      val text =
        if Files.isRegularFile(log) then
          val bytes = Files.readAllBytes(log)
          val from  =
            if mark <= 0 then 0
            else if mark >= bytes.length then bytes.length
            else mark.toInt
          new String(bytes, from, bytes.length - from, java.nio.charset.StandardCharsets.UTF_8).trim
        else ""
      if text.nonEmpty then text
      else scala.util.Try(process.exitValue()).toOption.fold("")(_.toString)
    }.orDie

  private def connect(socket: Path): IO[ServerError, Unit] =
    UnixPipe.connect(socket).flatMap(session => ZIO.succeed(session.close()))

  /** The socket can accept before the new server rewrites `server.json`. A record from before `before` is the previous
    * server.
    */
  private def awaitWritten(
      repo: Path,
      socket: Path,
      before: Option[java.nio.file.attribute.FileTime],
  ): IO[ServerError, Unit] =
    def poll(left: Int): IO[ServerError, Unit] =
      ServerStamp
        .note(repo)
        .flatMap: now =>
          if ServerStamp.writtenAfter(before, now) then ZIO.unit
          else if left > 0 then ZIO.sleep(50.millis) *> poll(left - 1)
          else ZIO.fail(ServerError.Unreachable(socket))
    poll(1200)

  private def untilUp(socket: Path): IO[ServerError, Path] =
    ServerWait
      .awaitListening(connect(socket), ZIO.succeed(true), ZIO.succeed(""), socket, 1200)
      .as(socket)

  private def jvm(args: List[String]): ZIO[Any, Nothing, Int] =
    ZIO
      .attemptBlocking {
        val java = Option(System.getenv("HISTOS_JAVA")).filter(_.nonEmpty).getOrElse("java")
        val jar  = Option(System.getenv("HISTOS_JAR")).filter(_.nonEmpty)
        jar match
          case None       => 1
          case Some(path) =>
            val command =
              java :: "--enable-native-access=ALL-UNNAMED" :: "--sun-misc-unsafe-memory-access=allow" :: "-jar" :: path :: args
            pass = true
            try new ProcessBuilder(command*).inheritIO().start().waitFor()
            finally pass = false
      }
      .orElseSucceed(1)
      .map(halt)
end Main
