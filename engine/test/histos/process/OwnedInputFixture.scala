package histos.process

import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger

import zio.{Chunk, Console, Promise, Scope, ZIO, ZIOAppDefault}

/** Runs with real inherited stdin in a child JVM so a stuck close has an external deadline. */
object OwnedInputFixture extends ZIOAppDefault:
  private enum FixtureError:
    case Mode, End, DescriptorLeak, Signal, Input

  def run =
    for
      args <- getArgs
      _    <- args.toList match
        case "close" :: Nil     => session
        case "eof" :: Nil       => untilEnd
        case "interrupt" :: Nil => interrupted
        case "repeat" :: Nil    => repeated
        case "signal" :: Nil    => restored
        case _                  => ZIO.fail(FixtureError.Mode)
      _ <- ZIO.attemptBlocking(java.lang.System.in.available()).mapError(_ => FixtureError.Input)
      _ <- Console.printLineError("owned input closed").mapError(_ => FixtureError.Input)
    yield ()

  private def first: ZIO[Scope, FixtureError, Unit] =
    for
      relay <- LiveInput.open
      _     <- relay match
        case RunRelay.Open(take) =>
          take.flatMap:
            case Some(RelayCommand.Stdin(bytes)) => emit(bytes)
            case _                               => ZIO.fail(FixtureError.End)
        case RunRelay.Closed => ZIO.fail(FixtureError.End)
    yield ()

  private val session = ZIO.scoped(first)

  private val untilEnd = ZIO.scoped:
    LiveInput.open.flatMap:
      case RunRelay.Open(take) =>
        def loop: ZIO[Any, FixtureError, Unit] =
          take.flatMap:
            case Some(RelayCommand.Stdin(bytes)) => emit(bytes) *> loop
            case Some(RelayCommand.End)          => ZIO.unit
            case Some(RelayCommand.Interrupt)    => loop
            case None                            => ZIO.fail(FixtureError.End)
        loop
      case RunRelay.Closed => ZIO.fail(FixtureError.End)

  private val interrupted = ZIO.scoped:
    for
      ready <- Promise.make[Nothing, Unit]
      fiber <- ZIO.scoped(first *> ready.succeed(()) *> ZIO.never).forkScoped
      _     <- ready.await
      exit  <- fiber.interrupt
      _     <- ZIO.fail(FixtureError.End).unless(exit.isInterrupted)
    yield ()

  private val repeated =
    for
      _      <- session
      before <- descriptors
      _      <- ZIO.foreachDiscard(1 to 20)(_ => ZIO.scoped(LiveInput.open *> ZIO.yieldNow))
      after  <- descriptors
      _      <- ZIO.fail(FixtureError.DescriptorLeak).unless(after == before)
    yield ()

  private val restored = ZIO.scoped:
    for
      pipe <- ZIO.acquireRelease(
        ZIO.fromOption(Signals.pipe()).orElseFail(FixtureError.Signal)
      )(pipe =>
        ZIO.succeed {
          Signals.close(pipe.read)
          Signals.close(pipe.write)
        }
      )
      _ <- ZIO.acquireRelease(ZIO.succeed(Signals.arm(pipe.write, new AtomicInteger(0))))(held =>
        ZIO.succeed(Signals.disarm(held))
      )
      before <- handler(pipe)
      _      <- session
      after  <- handler(pipe)
      _      <- ZIO.fail(FixtureError.Signal).unless(before == after)
    yield ()

  private def handler(pipe: Signals.Pipe) = ZIO.acquireReleaseWith(
    ZIO.fromOption(Signals.arm(pipe.write, new AtomicInteger(0))).orElseFail(FixtureError.Signal)
  )(held => ZIO.succeed(Signals.disarm(Some(held))))(held => ZIO.succeed(held.previous))

  private def descriptors =
    ZIO
      .attemptBlocking {
        val entries = Files.list(Path.of("/dev/fd"))
        try entries.count()
        finally entries.close()
      }
      .mapError(_ => FixtureError.Input)

  private def emit(bytes: Chunk[Byte]) = ZIO.succeed:
    java.lang.System.out.write(bytes.toArray)
    java.lang.System.out.flush()
end OwnedInputFixture
