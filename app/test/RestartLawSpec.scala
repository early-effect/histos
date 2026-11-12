package histos.server

import histos.build.BuildEvent
import histos.watch.SourceWatch
import histos.workspace.Workspace

import java.nio.file.Path

import zio.{Chunk, Queue, Ref, ZIO, duration2DurationOps, durationInt}
import zio.test.{TestAspect, TestClock, ZIOSpecDefault, assertTrue, suite, test}

/** A restart watch replaces a running program when the change is accepted, leaves it when the change is refused, and
  * stops it when the watch is cancelled. The program is a fiber that runs until it is interrupted, and the poll runs on
  * the test clock.
  */
object RestartLawSpec extends ZIOSpecDefault:
  private enum Seen:
    case Watching
    case Started(child: Int)
    case Stopped(child: Int)

  def spec = suite("restart watch laws")(
    test("an accepted change starts the next child, a refused change keeps it, and cancel stops it"):
      for
        repo <- Workspace.work("histos-restart")
        source = repo.resolve("src").resolve("App.scala")
        _       <- Workspace.writeString(source, "one\n")
        seen    <- Queue.unbounded[Seen]
        started <- Ref.make(0)
        allow   <- Ref.make(true)
        child = started
          .updateAndGet(_ + 1)
          .flatMap(n => seen.offer(Seen.Started(n)) *> ZIO.never.onInterrupt(seen.offer(Seen.Stopped(n))))
        roots = ZIO.succeed(Watching.Watched(List(repo.resolve("src")), Set.empty))
        again = (changed: List[Path]) => allow.get.map(ok => ok && changed.nonEmpty)
        say   = new Say({
          case Message.Event(BuildEvent.Out("watching")) => seen.offer(Seen.Watching).unit
          case _                                         => ZIO.unit
        })
        watch    <- Watching.beside(say, child, roots, again).fork
        first    <- seen.takeN(2)
        _        <- Workspace.writeString(source, "two two\n")
        _        <- TestClock.adjust(SourceWatch.Poll + SourceWatch.Settle)
        replaced <- seen.takeN(2)
        _        <- allow.set(false)
        _        <- Workspace.writeString(source, "three three three\n")
        _        <- TestClock.adjust(SourceWatch.Poll + SourceWatch.Settle)
        kept     <- seen.takeAll
        _        <- watch.interrupt
        stopped  <- seen.takeAll
      yield assertTrue(
        first.toSet == Set(Seen.Started(1), Seen.Watching),
        replaced == Chunk(Seen.Stopped(1), Seen.Started(2)),
        kept == Chunk.empty,
        stopped == Chunk(Seen.Stopped(2)),
      )
  ) @@ TestAspect.timeout(60.seconds)
end RestartLawSpec
