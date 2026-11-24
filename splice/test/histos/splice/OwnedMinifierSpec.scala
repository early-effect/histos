package histos.splice

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

import zio.{IO, Ref, ZIO, durationInt}
import zio.test.*

object OwnedMinifierSpec extends ZIOSpecDefault:
  enum Failure:
    case File(path: Path)
    case Rejected
    case MissingLaunch

  def spec =
    val tests = suite("owned minifier launch")(
      test("an invocation observer failure retains its typed error and closes the real process and temporary tree") {
        for
          cache    <- io(Path.of("coursier-cache"))(coursier.cache.FileCache().location.toPath)
          observed <- Ref.make[Option[(EsbuildNative.Started, Boolean)]](None)
          result   <- EsbuildNative
            .optimizeObserved("globalThis.answer = 42;", cache, false) { started =>
              io(started.temporary)(Files.isRegularFile(started.temporary.resolve("in.js")))
                .flatMap(input => observed.set(Some(started -> input))) *> ZIO.fail(Failure.Rejected)
            }
            .either
          launched <- observed.get.someOrFail(Failure.MissingLaunch)
          (started, inputExisted) = launched
          dead    <- io(started.temporary)(ProcessHandle.of(started.pid).toScala.forall(process => !process.isAlive))
          removed <- io(started.temporary)(Files.notExists(started.temporary))
        yield assertTrue(result == Left(Failure.Rejected), inputExisted, started.pid > 0L, dead, removed)
      },
      test("the internal full handoff invokes the real minifier and returns the ordinary full bytes") {
        ZIO.scoped:
          for
            root <- ZIO.acquireRelease(
              io(Path.of("owned-minifier-spec"))(Files.createTempDirectory("owned-minifier-spec-"))
            )(root =>
              ZIO.attemptBlocking {
                val walk  = Files.walk(root)
                val paths = try walk.iterator.asScala.toList.sortBy(_.getNameCount).reverse
                finally walk.close()
                paths.foreach(Files.deleteIfExists(_))
              }.ignore
            )
            input    = root.resolve("input.js")
            ordinary = root.resolve("ordinary.js")
            observed = root.resolve("observed.js")
            _       <- io(input)(Files.writeString(input, "globalThis.answer = 40 + 2;"))
            launch  <- Ref.make[Option[EsbuildNative.Started]](None)
            _       <- Splice.full(input, ordinary)
            _       <- Splice.fullObserved(input, observed)(started => launch.set(Some(started)))
            started <- launch.get.someOrFail(Failure.MissingLaunch)
            plain   <- io(ordinary)(Files.readString(ordinary))
            watched <- io(observed)(Files.readString(observed))
            removed <- io(started.temporary)(Files.notExists(started.temporary))
          yield assertTrue(plain == watched, watched.contains("42"), removed)
      },
    )
    tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(1.minute)

  private def io[A](path: Path)(body: => A): IO[Failure, A] =
    ZIO.attemptBlocking(body).mapError(_ => Failure.File(path))
end OwnedMinifierSpec
