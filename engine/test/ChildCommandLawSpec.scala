package histos.process

import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import scala.jdk.OptionConverters.*

import zio.{Chunk, ZIO}
import zio.durationInt
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}

/** A linked program runs through the group launcher. The status is the program's status. */
object ChildCommandLawSpec extends ZIOSpecDefault:
  private val path = Map("PATH" -> "/bin:/usr/bin")

  private val orphanScript =
    """echo suite-line
      |/bin/sleep 600 &
      |echo $! > orphan.pid
      |exit 0
      |""".stripMargin

  /** The drain bound is real time. The orphan holds an operating-system pipe, and the test clock cannot close it. */
  private val orphanHeld =
    test("a child that outlives the command is an orphan, its line is kept, and the child is ended")(
      Workspace
        .work("histos-child-orphan")
        .flatMap: dir =>
          val script = dir.resolve("hold.sh")
          val pid    = dir.resolve("orphan.pid")
          for
            _   <- Workspace.writeString(script, orphanScript)
            ran <- ChildProcess
              .command(List("/bin/sh", script.toString), dir, path)
              .either
              .runCollect
              .timeout(30.seconds)
            living <- stillAlive(pid)
          yield assertTrue(reported(ran), !living)
    ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(45.seconds)

  def spec = suite("child command laws")(
    test("an empty argv is no program, echo's line is stdout, and a missing program exits 127"):
      Workspace
        .work("histos-child-command")
        .flatMap: dir =>
          for
            empty   <- ChildProcess.command(Nil, dir, path).runCollect.either
            echoed  <- ChildProcess.command(List("/bin/echo", "histos-child"), dir, path).runCollect.either
            stopped <- ChildProcess.command(List("/usr/bin/false"), dir, path).runCollect.either
            missing <- ChildProcess.command(List("/bin/no-such-histos-bin"), dir, path).runCollect.either
          yield assertTrue(
            empty == Left(ChildError.NoProgram),
            shown(echoed) == "0|histos-child",
            shown(stopped) == "1|",
            shown(missing) == "127|",
          )
    ,
    orphanHeld,
  ) @@ TestAspect.withLiveSystem

  private def shown(events: Either[ChildError, Chunk[ChildEvent]]): String =
    events match
      case Left(error)  => error.toString
      case Right(chunk) =>
        val code = chunk.collect { case ChildEvent.Exited(exit) => exit.value.toString }.mkString(",")
        val out  = chunk.collect { case ChildEvent.Out(text) => text }.mkString(",")
        s"$code|$out"

  /** `None` means the command was still running when the law's bound elapsed. An exit event would record the suite as
    * finished while the child still held the output.
    */
  private def reported(ran: Option[Chunk[Either[ChildError, ChildEvent]]]): Boolean =
    ran match
      case None         => false
      case Some(events) =>
        val line = events.exists:
          case Right(ChildEvent.Out("suite-line")) => true
          case _                                   => false
        val orphaned = events.exists:
          case Left(ChildError.Orphan(0)) => true
          case _                          => false
        val exited = events.exists:
          case Right(ChildEvent.Exited(_)) => true
          case _                           => false
        line && orphaned && !exited

  /** True when the pid file names a process that is still running. A missing pid is still running: the law did not see
    * the child end.
    */
  private def stillAlive(pid: Path): ZIO[Any, Nothing, Boolean] =
    ZIO
      .attemptBlocking(Option.when(Files.isRegularFile(pid))(Files.readString(pid)))
      .orDie
      .flatMap:
        case None       => ZIO.succeed(true)
        case Some(text) =>
          text.trim.toLongOption match
            case None         => ZIO.succeed(true)
            case Some(number) =>
              ProcessHandle.of(number).toScala match
                case None         => ZIO.succeed(false)
                case Some(handle) =>
                  ZIO.fromCompletableFuture(handle.onExit()).timeout(2.seconds).orDie.map(_.isEmpty)
end ChildCommandLawSpec
