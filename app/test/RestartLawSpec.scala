package histos.server

import histos.build.BuildEvent
import histos.report.{BuildFault, CallResult}
import histos.watch.SourceWatch
import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import zio.{Ref, Schedule, ZIO, durationInt, duration2DurationOps}
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}

/** A restart watch replaces a running program when the change is accepted, leaves it when the change is refused, and
  * stops it when the watch is cancelled.
  */
object RestartLawSpec extends ZIOSpecDefault:
  def spec = suite("restart watch laws")(
    test("an accepted change starts the next child, a refused change keeps the pid, and cancel stops it"):
      ZIO.scoped {
        for
          repo    <- Workspace.work("histos-restart").catchAll(error => ZIO.dieMessage(error.message))
          source  <- ZIO.succeed(repo.resolve("src").resolve("App.scala"))
          _       <- Workspace.createDirectories(source.getParent)
          _       <- Workspace.writeString(source, "one\n")
          started <- Ref.make(0)
          allow   <- Ref.make(true)
          live    <- Ref.make(Option.empty[Child])
          say      = quiet
          child    = stand(repo, started, live)
          roots    = ZIO.succeed(Watching.Watched(List(repo.resolve("src")), Set.empty))
          again    = (changed: List[Path]) => allow.get.map(ok => ok && changed.nonEmpty)
          fiber   <- Watching.beside(say, child, roots, again).fork
          first   <- awaitPid(live)
          _       <- Workspace.writeString(source, "two\n")
          second  <- awaitNext(live, first.pid)
          kept     = first.pid
          _       <- allow.set(false)
          _       <- Workspace.writeString(source, "three\n")
          _       <- ZIO.sleep(SourceWatch.Poll + SourceWatch.Settle + SourceWatch.Poll)
          during  <- live.get
          alive   <- during match
            case Some(found) => running(found.pid)
            case None        => ZIO.succeed(false)
          _    <- fiber.interrupt
          gone <- gone(second.pid)
          times <- started.get
        yield assertTrue(
          second.pid != kept,
          during.map(_.pid) == Some(second.pid),
          alive,
          !gone,
          times == 2,
        )
      }
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(20.seconds) @@ TestAspect.sequential

  private final case class Child(pid: Long)

  private def stand(repo: Path, started: Ref[Int], live: Ref[Option[Child]]): ZIO[Any, ServerError, Unit] =
    ZIO.scoped {
      for
        work <- ZIO.attemptBlocking(Files.createTempDirectory(repo, "child")).orDie
        file  = work.resolve("pid")
        process <- ZIO
          .attemptBlocking {
            new ProcessBuilder("sh", "-c", "echo $$ > pid; exec sleep 30")
              .directory(work.toFile)
              .redirectOutput(ProcessBuilder.Redirect.DISCARD)
              .redirectError(ProcessBuilder.Redirect.DISCARD)
              .start()
          }
          .orDie
        _   <- started.update(_ + 1)
        pid <- readPid(file)
        _   <- live.set(Some(Child(pid)))
        _   <- ZIO.addFinalizer(ZIO.succeed(process.destroyForcibly()).ignore)
        _   <- ZIO.attemptBlockingInterrupt(process.waitFor()).orDie
      yield ()
    }

  private def readPid(file: Path): ZIO[Any, Nothing, Long] =
    ZIO
      .attemptBlocking(Files.readString(file).trim.toLong)
      .retry(Schedule.spaced(20.millis) && Schedule.recurs(50))
      .orDie

  private def awaitPid(live: Ref[Option[Child]]): ZIO[Any, Nothing, Child] =
    live.get.flatMap:
      case Some(found) => ZIO.succeed(found)
      case None        => ZIO.sleep(20.millis) *> awaitPid(live)

  private def awaitNext(live: Ref[Option[Child]], previous: Long): ZIO[Any, Nothing, Child] =
    live.get.flatMap:
      case Some(found) if found.pid != previous => ZIO.succeed(found)
      case _                                    => ZIO.sleep(20.millis) *> awaitNext(live, previous)

  private def gone(pid: Long): ZIO[Any, Nothing, Boolean] =
    running(pid).flatMap:
      case false => ZIO.succeed(false)
      case true  => ZIO.sleep(20.millis) *> gone(pid)

  private def running(pid: Long): ZIO[Any, Nothing, Boolean] =
    ZIO
      .attemptBlocking {
        new ProcessBuilder("kill", "-0", pid.toString).start().waitFor() == 0
      }
      .orElseSucceed(false)

  private def quiet: Say =
    new Say(_ => ZIO.unit)
end RestartLawSpec
