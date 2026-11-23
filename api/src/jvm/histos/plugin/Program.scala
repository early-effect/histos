package histos.plugin

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.nio.file.Path

import scala.jdk.CollectionConverters.*

import zio.{Fiber, IO, Promise, Scope, UIO, ZIO, durationInt}

/** A line the child wrote. Out and err stay apart so a console can show them on the right stream.
  */
enum ProgramEvent:
  case Out(line: String)
  case Err(line: String)

/** Run a program. Both pipes are read while it runs, so a full pipe cannot stall it, and the exit code comes back only
  * after both pipes have closed. Interrupt destroys the process. A plugin uses this instead of starting a thread of its
  * own.
  */
object Program:
  /** Wait for the program. Each line is reported on `events` as the child writes it. */
  def run(argv: List[String], directory: Path, events: PluginEvents.Invocation): IO[PluginRunError, Int] =
    ZIO.scoped(start(argv, directory, events).flatMap(_.join))

  /** The fiber belongs to the caller scope. Closing that scope destroys the process, which is how a preview server dies
    * with the generation that started it.
    */
  def start(
      argv: List[String],
      directory: Path,
      events: PluginEvents.Invocation,
      watch: ProgramEvent => UIO[Unit] = _ => ZIO.unit,
  ): ZIO[Scope, PluginRunError, Fiber[PluginRunError, Int]] =
    body(argv, directory, events, watch).forkScoped

  private def body(
      argv: List[String],
      directory: Path,
      events: PluginEvents.Invocation,
      watch: ProgramEvent => UIO[Unit],
  ): ZIO[Scope, PluginRunError, Int] =
    argv match
      case Nil          => ZIO.fail(refuse("a program needs a command"))
      case command :: _ =>
        for
          readers <- ZIO.scopeWith(_.fork)
          process <- ZIO.acquireRelease(launch(argv, directory))(destroy)
          outDone <- Promise.make[Nothing, Unit]
          errDone <- Promise.make[Nothing, Unit]
          _    <- readers.extend(read(process.getInputStream, outDone, ProgramEvent.Out(_), events, watch).forkScoped)
          _    <- readers.extend(read(process.getErrorStream, errDone, ProgramEvent.Err(_), events, watch).forkScoped)
          code <- ZIO.attemptBlockingInterrupt(process.waitFor()).mapError(_ => refuse(s"$command was interrupted"))
          drained <- (outDone.await *> errDone.await).timeout(1.second)
          _       <- ZIO.when(drained.isEmpty)(
            destroy(process) *> ZIO.fail(refuse(s"$command exited $code and left a pipe open"))
          )
        yield code

  private def read(
      input: InputStream,
      done: Promise[Nothing, Unit],
      event: String => ProgramEvent,
      events: PluginEvents.Invocation,
      watch: ProgramEvent => UIO[Unit],
  ): UIO[Unit] =
    val reader          = new BufferedReader(new InputStreamReader(input, StandardCharsets.UTF_8))
    def step: UIO[Unit] =
      ZIO
        .attemptBlockingInterrupt(Option(reader.readLine()))
        .orElseSucceed(None)
        .flatMap:
          case None       => done.succeed(()).unit
          case Some(line) =>
            val happened = event(line)
            tell(events, happened) *> watch(happened) *> step
    step.ensuring(ZIO.attemptBlocking(reader.close()).ignore *> done.succeed(()).unit)

  /** A closed invocation still runs the program. The line is one plain detail, so a control character is dropped rather
    * than failing the task.
    */
  private def tell(events: PluginEvents.Invocation, happened: ProgramEvent): UIO[Unit] =
    val raw = happened match
      case ProgramEvent.Out(line) => line
      case ProgramEvent.Err(line) => line
    val text = raw.filterNot(_.isControl)
    if text.isEmpty then ZIO.unit
    else
      PluginDetail.from(text) match
        case Left(_)       => ZIO.unit
        case Right(detail) =>
          val event = happened match
            case ProgramEvent.Out(_) => InvocationPluginEvent.Message(detail)
            case ProgramEvent.Err(_) => InvocationPluginEvent.Err(detail)
          events.emit(event).orElseSucceed(())

  private def launch(argv: List[String], directory: Path): IO[PluginRunError, Process] =
    ZIO
      .attemptBlocking {
        val builder = new java.lang.ProcessBuilder(argv.asJava)
        builder.directory(directory.toFile)
        builder.start()
      }
      .mapError(thrown => refuse(s"could not start ${argv.mkString(" ")}: ${thrown.getClass.getName}"))

  private def destroy(process: Process): UIO[Unit] =
    ZIO.attemptBlocking(process.destroyForcibly()).ignore

  private def refuse(message: String): PluginRunError =
    PluginDetail.from(message) match
      case Right(detail) => PluginRunError.Failed(detail)
      case Left(_)       => PluginRunError.Defect("PluginDetail")
end Program
