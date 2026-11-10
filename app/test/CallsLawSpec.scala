package histos.server

import histos.build.BuildEvent
import histos.cache.{CacheCleanMode, CacheEntry, ContentStore}
import histos.report.{BuildFault, CallResult, Rejection, Report}
import histos.schedule.{CacheError, Digest}
import histos.task.{RunId, TaskError}
import histos.workspace.{IoFailure, IoFault}
import slayer.{stub, stubbed}

import java.nio.file.Path
import java.time.Instant

import zio.*
import zio.durationInt
import zio.test.*

object CallsLawSpec extends ZIOSpecDefault:
  private val digest =
    Digest.trusted("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
  private val entry = CacheEntry(digest, Path.of("blob"), 4L, Instant.EPOCH)
  private val id    = RunId.trusted("run-1")

  def spec = suite("calls laws")(
    test("cache path prints the store root"):
      val program =
        for
          _               <- stub[ContentStore](_.root)(Path.of("cas"))
          (result, heard) <- speak(say => Calls.cachePath(say))
        yield assertTrue(
          result == Right(()),
          heard == Chunk(Message.Event(BuildEvent.Out("cas")), Message.Done(CallResult.reported())),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[ContentStore])
    ,
    test("an empty cache list is silent and a failure is a cache fault"):
      val failure = CacheError.Io(IoFailure(Path.of("cas"), IoFault.Read))
      val empty   =
        for
          _     <- stub[ContentStore](_.list)(ZIO.succeed(Nil))
          heard <- speak(say => Calls.cacheList(say))
        yield heard
      val broken =
        for
          _     <- stub[ContentStore](_.list)(ZIO.fail(failure))
          heard <- speak(say => Calls.cacheList(say))
        yield heard
      val program =
        for
          quiet  <- empty
          failed <- broken
        yield assertTrue(
          quiet == (Right(()), Chunk(Message.Done(CallResult.reported()))),
          failed == (Right(()), Chunk(Message.Done(CallResult.Fault(BuildFault.Cache(failure))))),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[ContentStore])
    ,
    test("a cache list prints digest and size"):
      val program =
        for
          _               <- stub[ContentStore](_.list)(ZIO.succeed(List(entry)))
          (result, heard) <- speak(say => Calls.cacheList(say))
        yield assertTrue(
          result == Right(()),
          heard == Chunk(
            Message.Event(BuildEvent.Out(s"${digest.value} 4")),
            Message.Done(CallResult.reported()),
          ),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[ContentStore])
    ,
    test("clean names the mode and the retention window at the epoch clock"):
      val program =
        for
          _       <- stubClean(Instant.EPOCH, CacheCleanMode.Remove, List(entry))
          removed <- speak(say => Calls.cacheClean(CacheCleanMode.Remove, say))
          _       <- stubClean(Instant.EPOCH, CacheCleanMode.DryRun, Nil)
          dry     <- speak(say => Calls.cacheClean(CacheCleanMode.DryRun, say))
        yield assertTrue(
          removed == (
            Right(()),
            Chunk(
              Message.Event(BuildEvent.Out(s"removed ${digest.value}")),
              Message.Done(CallResult.reported()),
            ),
          ),
          dry == (
            Right(()),
            Chunk(Message.Event(BuildEvent.Out("would remove nothing")), Message.Done(CallResult.reported())),
          ),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[ContentStore])
    ,
    test("clean reads the fiber clock after it moves"):
      val later   = Instant.EPOCH.plus(java.time.Duration.ofDays(1))
      val program =
        for
          _     <- TestClock.adjust(1.day)
          _     <- stubClean(later, CacheCleanMode.Remove, List(entry))
          heard <- speak(say => Calls.cacheClean(CacheCleanMode.Remove, say))
        yield assertTrue(
          heard == (
            Right(()),
            Chunk(
              Message.Event(BuildEvent.Out(s"removed ${digest.value}")),
              Message.Done(CallResult.reported()),
            ),
          )
        )
      program.provideSomeLayer[TestEnvironment](stubbed[ContentStore])
    ,
    test("an unknown run and a finished fiber are not cancelled"):
      val unknown = Chunk(Message.Done(CallResult.Fault(BuildFault.Task(TaskError.UnknownRun(id)))))
      for
        runs    <- Ref.make(Map.empty[RunId, Fiber[ServerError, Unit]])
        missing <- speak(say => Calls.cancel(runs, id, say))
        fiber   <- ZIO.unit.fork
        _       <- fiber.join
        _       <- runs.set(Map(id -> fiber))
        done    <- speak(say => Calls.cancel(runs, id, say))
        left    <- runs.get
      yield assertTrue(missing == (Right(()), unknown), done == (Right(()), unknown), left.isEmpty)
    ,
    test("cancel removes the id before it interrupts the fiber"):
      for
        runs    <- Ref.make(Map.empty[RunId, Fiber[ServerError, Unit]])
        seen    <- Ref.make(Option.empty[Boolean])
        started <- Promise.make[Nothing, Unit]
        // The body has to be running, so the interrupt handler is in place before cancel.
        fiber <- (started.succeed(()) *> ZIO.never)
          .onInterrupt(runs.get.flatMap(map => seen.set(Some(!map.contains(id)))))
          .fork
        _               <- started.await
        _               <- runs.set(Map(id -> fiber))
        (result, heard) <- speak(say => Calls.cancel(runs, id, say))
        gone            <- seen.get
        left            <- runs.get
      yield assertTrue(
        result == Right(()),
        gone == Some(true),
        left.isEmpty,
        heard == Chunk(Message.Event(BuildEvent.Out("cancelled")), Message.Done(CallResult.reported())),
      )
    ,
    test("a tracked body sees its id before it runs and a success has no terminal frame"):
      for
        runs            <- Ref.make(Map.empty[RunId, Fiber[ServerError, Unit]])
        seen            <- Ref.make(false)
        minted          <- Ref.make(Option.empty[String])
        (result, heard) <- speak { say =>
          Calls.tracked(runs, say) {
            for
              id  <- say.run
              map <- runs.get
              _   <- seen.set(id.exists(value => map.contains(RunId.trusted(value))))
              _   <- minted.set(id)
            yield ()
          }
        }
        present <- seen.get
        id      <- minted.get
        left    <- runs.get
      yield assertTrue(
        result == Right(()),
        present,
        left.isEmpty,
        id match
          case Some(value) => heard == Chunk(Message.Event(BuildEvent.Out(s"run $value")))
          case None        => false,
      )
    ,
    test("a tracked failure ends with the error line and done, and an interrupt is a fault frame"):
      for
        runs    <- Ref.make(Map.empty[RunId, Fiber[ServerError, Unit]])
        failed  <- speak(say => Calls.tracked(runs, say)(ZIO.fail(ServerError.NoServer)))
        stopped <- speak(say => Calls.tracked(runs, say)(ZIO.interrupt))
        died    <- speak(say => Calls.tracked(runs, say)(ZIO.die(new IllegalStateException("boom"))))
        left    <- runs.get
      yield assertTrue(
        failed._1 == Right(()),
        trackedFailure(failed._2, ServerError.NoServer.message),
        stopped._1 == Right(()),
        terminal(stopped._2, CallResult.Fault(BuildFault.Interrupted)),
        died._1 == Right(()),
        terminal(died._2, CallResult.Fault(BuildFault.Rejected(Rejection.RunFailed))),
        left.isEmpty,
      )
    ,
    test("empty text is not sent and a report's error lines precede the terminal frame"):
      val report = Report(Chunk.empty, Chunk("a", "b"), None, None, Chunk.empty, Chunk.empty, None)
      for
        silent <- speak(say => Calls.tell(say, ""))
        line   <- speak(say => Calls.tell(say, "ada"))
        faults <- speak(say => Calls.finish(say, CallResult.Fault(BuildFault.Interrupted)))
        empty  <- speak(say => Calls.finish(say, CallResult.reported()))
        noted  <- speak(say => Calls.finish(say, CallResult.Reported(report)))
      yield assertTrue(
        silent == (Right(()), Chunk.empty),
        line == (Right(()), Chunk(Message.Event(BuildEvent.Out("ada")))),
        faults == (Right(()), Chunk(Message.Done(CallResult.Fault(BuildFault.Interrupted)))),
        empty == (Right(()), Chunk(Message.Done(CallResult.reported()))),
        noted == (
          Right(()),
          Chunk(
            Message.Event(BuildEvent.Err("a")),
            Message.Event(BuildEvent.Err("b")),
            Message.Done(CallResult.Reported(report)),
          ),
        ),
      ),
  )

  private def stubClean(
      expect: Instant,
      mode: CacheCleanMode,
      rows: List[CacheEntry],
  ): URIO[slayer.Stubbed[ContentStore], Unit] =
    stub[ContentStore](_.clean(slayer.any[Instant], slayer.any[java.time.Duration], slayer.any[CacheCleanMode])) {
      (now: Instant, retention: java.time.Duration, found: CacheCleanMode) =>
        if now == expect && retention == ContentStore.Retention && found == mode then ZIO.succeed(rows)
        else ZIO.die(new IllegalStateException(s"clean $now $retention $found"))
    }

  private def speak[R, E](use: Say => ZIO[R, E, Unit]): ZIO[R, Nothing, (Either[E, Unit], Chunk[Message])] =
    for
      messages <- Ref.make(Chunk.empty[Message])
      say = new Say(message => messages.update(_ :+ message))
      result <- use(say).either
      heard  <- messages.get
    yield (result, heard)

  private def trackedFailure(heard: Chunk[Message], sentence: String): Boolean =
    heard match
      case Chunk(
            Message.Event(BuildEvent.Out(text)),
            Message.Event(BuildEvent.Err(err)),
            Message.Done(CallResult.Reported(report)),
          ) =>
        text.startsWith("run ") &&
        err == sentence &&
        report.err == Chunk(sentence) &&
        report.run.contains(text.drop(4))
      case _ => false

  private def terminal(heard: Chunk[Message], result: CallResult): Boolean =
    heard match
      case Chunk(Message.Event(BuildEvent.Out(text)), Message.Done(done)) =>
        text.startsWith("run ") && done == result
      case _ => false
end CallsLawSpec
