package histos.cache

import histos.jdk.Sha256Digest
import histos.schedule.CacheError
import histos.suite.HistosSpec
import histos.workspace.Workspace

import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime
import java.nio.charset.StandardCharsets.UTF_8

import zio.{Chunk, Promise, Scope, ZIO, ZIOAppDefault, Console, durationInt}
import java.io.{BufferedReader, InputStreamReader}
import scala.jdk.CollectionConverters.*
import zio.test.*

object LinkReplaySpec extends HistosSpec:
  private val body = "console.log('complete');\n" * 1024
  private val key  = Sha256Digest.of(Chunk.fromArray("link-key".getBytes(UTF_8)))

  private val tests = suite("shared link replay")(
    test("a valid returned script is never reopened for writing on a warm hit") {
      fixture { (root, store, source) =>
        for
          _     <- LinkReplay.save(store, key, source)
          first <- LinkReplay.load(store, key, root)
          path  <- ZIO
            .fromOption(first)
            .orElseFail(CacheError.Io(histos.workspace.IoFailure(root, histos.workspace.IoFault.Read)))
          sentinel = FileTime.fromMillis(123456789L)
          _        <- ZIO.attemptBlocking(Files.setLastModifiedTime(path, sentinel)).orDie
          again    <- LinkReplay.load(store, key, root)
          modified <- ZIO.attemptBlocking(Files.getLastModifiedTime(path)).orDie
          text     <- ZIO.attemptBlocking(Files.readString(path)).orDie
        yield assertTrue(again.contains(path), modified == sentinel, text == body)
      }
    },
    test("generic product hits also preserve an already complete destination") {
      fixture { (root, store, _) =>
        val dest  = root.resolve("product.bin")
        val bytes = Chunk.fromArray(body.getBytes(UTF_8))
        for
          _ <- ProductCache.materialize(
            histos.schedule.RemoteCache.unconfigured,
            store,
            key,
            dest,
            ZIO.attemptBlocking(Files.write(dest, bytes.toArray)).orDie.as(bytes),
          )
          sentinel = FileTime.fromMillis(123456789L)
          _     <- ZIO.attemptBlocking(Files.setLastModifiedTime(dest, sentinel)).orDie
          again <- ProductCache.materialize(
            histos.schedule.RemoteCache.unconfigured,
            store,
            key,
            dest,
            ZIO.dieMessage("cache miss"),
          )
          modified <- ZIO.attemptBlocking(Files.getLastModifiedTime(dest)).orDie
        yield assertTrue(again == bytes, modified == sentinel)
      }
    },
    test("a stopped competing publisher leaves the earlier returned script and no temporary file") {
      fixture { (root, store, source) =>
        for
          _         <- LinkReplay.save(store, key, source)
          ready     <- Promise.make[Nothing, Unit]
          hold      <- Promise.make[Nothing, Unit]
          first     <- LinkReplay.load(store, key, root, _ => ready.succeed(()) *> hold.await).forkScoped
          _         <- ready.await
          second    <- LinkReplay.load(store, key, root)
          path      <- required(second, root)
          read      <- ZIO.foreach(1 to 100)(_ => ZIO.attemptBlocking(Files.readString(path)).orDie).forkScoped
          exit      <- first.interrupt
          observed  <- read.join
          temporary <- ZIO.attemptBlocking {
            val files = Files.list(root.resolve("linked"))
            try files.iterator.asScala.filter(path => path.getFileName.toString.startsWith(".replay-")).toList
            finally files.close()
          }.orDie
        yield assertTrue(exit.isInterrupted, observed.forall(_ == body), temporary.isEmpty)
      }
    },
    test("concurrent product keep additions retain prior values without tying another project's lifetime") {
      fixture { (root, store, _) =>
        val project = CacheProject.of(root)
        val other   = CacheProject.of(root.resolve("other"))
        val prior   = histos.schedule.Digest.trusted(key.value)
        val values  = (1 to 20)
          .map(n => histos.schedule.Digest.trusted(Sha256Digest.of(Chunk.fromArray(n.toString.getBytes(UTF_8))).value))
          .toSet
        val separate = histos.schedule.Digest.trusted(Sha256Digest.of(Chunk.fromArray("other".getBytes(UTF_8))).value)
        for
          _         <- store.replace(project, Set(prior))
          _         <- store.replace(other, Set(separate))
          _         <- ZIO.foreachParDiscard(values)(digest => store.retain(project, Set(digest)))
          all       <- store.kept
          now       <- ZIO.clockWith(_.instant)
          _         <- store.forget(other, now)
          remaining <- store.kept
        yield assertTrue((values + prior + separate).subsetOf(all), remaining == values + prior)
      }
    },
    test("two processes publishing the same key never truncate an earlier returned script") {
      fixture { (root, store, source) =>
        for
          _         <- LinkReplay.save(store, key, source)
          home      <- ZIO.attemptBlocking(Option(System.getProperty("java.home"))).orDie
          java      <- ZIO.fromOption(home).orElseFail(io(root))
          cp        <- ZIO.attemptBlocking(Option(System.getProperty("java.class.path"))).orDie
          classpath <- ZIO.fromOption(cp).orElseFail(io(root))
          child     <- ZIO.acquireRelease(
            ZIO
              .attemptBlocking(
                new ProcessBuilder(
                  Path.of(java, "bin", "java").toString,
                  "-cp",
                  classpath,
                  "histos.cache.LinkReplayPublisher",
                  store.root.toString,
                  root.toString,
                  key.value,
                ).start()
              )
              .orDie
          )(process =>
            ZIO.attemptBlocking {
              if process.isAlive then process.destroyForcibly()
              process.waitFor()
              ()
            }.orDie
          )
          reader <- ZIO.acquireRelease(
            ZIO.attemptBlocking(new BufferedReader(new InputStreamReader(child.getInputStream, UTF_8))).orDie
          )(stream => ZIO.attemptBlocking(stream.close()).orDie)
          line      <- ZIO.attemptBlocking(Option(reader.readLine())).orDie
          _         <- ZIO.fail(io(root)).unless(line.contains("prepared"))
          second    <- LinkReplay.load(store, key, root)
          path      <- required(second, root)
          reading   <- Promise.make[Nothing, Unit]
          published <- Promise.make[Nothing, Unit]
          observed  <- ZIO.scoped {
            for
              stream <- ZIO.acquireRelease(ZIO.attemptBlocking(Files.newInputStream(path)).orDie)(stream =>
                ZIO.attemptBlocking(stream.close()).orDie
              )
              _     <- reading.succeed(())
              _     <- published.await
              held  <- ZIO.attemptBlocking(new String(stream.readAllBytes(), UTF_8)).orDie
              paths <- ZIO.foreach(1 to 100)(_ => ZIO.attemptBlocking(Files.readString(path)).orDie)
            yield held :: paths.toList
          }.forkScoped
          _ <- reading.await
          _ <- ZIO.attemptBlocking {
            child.getOutputStream.write("publish\n".getBytes(UTF_8))
            child.getOutputStream.flush()
          }.orDie
          code       <- ZIO.attemptBlocking(child.waitFor()).orDie
          _          <- published.succeed(())
          reads      <- observed.join
          finalBytes <- ZIO.attemptBlocking(Files.readString(path)).orDie
        yield assertTrue(code == 0, reads.forall(_ == body), finalBytes == body)
      }
    },
  )

  private val clockTests = suite("cache environment clock")(
    test("link and generic product saves stamp CAS with the environment clock") {
      fixture { (root, store, source) =>
        val at    = java.time.Instant.ofEpochSecond(1234567L)
        val other = Sha256Digest.of(Chunk.fromArray("generic-key".getBytes(UTF_8)))
        for
          _ <- TestClock.setTime(at)
          _ <- LinkReplay.save(store, key, source)
          _ <- ProductCache.materialize(
            histos.schedule.RemoteCache.unconfigured,
            store,
            other,
            root.resolve("other.bin"),
            ZIO.succeed(Chunk.fromArray("generic".getBytes(UTF_8))),
          )
          entries <- store.list
        yield assertTrue(
          entries.find(_.digest.value == key.value).map(_.lastUsed).contains(at),
          entries.find(_.digest.value == other.value).map(_.lastUsed).contains(at),
        )
      }
    }
  )

  def spec = suite("cache publication")(tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(2.minutes), clockTests)

  private def io(root: Path): CacheError =
    CacheError.Io(histos.workspace.IoFailure(root, histos.workspace.IoFault.Read))
  private def required(value: Option[Path], root: Path): ZIO[Any, CacheError, Path] =
    ZIO.fromOption(value).orElseFail(io(root))

  private def fixture[A](
      use: (Path, ContentStore, Path) => ZIO[Scope, CacheError | histos.schedule.RemoteError, A]
  ): ZIO[Any, CacheError | histos.schedule.RemoteError, A] =
    ZIO.scoped {
      for
        root   <- Workspace.work("link-replay").catchAll(error => ZIO.dieMessage(error.message))
        cas    <- ZIO.attemptBlocking(Files.createDirectories(root.resolve("cas"))).orDie
        source <- ZIO.attemptBlocking(Files.writeString(root.resolve("script.js"), body)).orDie
        result <- use(root, ContentStore.in(cas), source)
      yield result
    }
end LinkReplaySpec

object LinkReplayPublisher extends ZIOAppDefault:
  def run = getArgs.flatMap(args =>
    args.toList match
      case List(cas, work, rawKey) =>
        val error = CacheError.Io(histos.workspace.IoFailure(Path.of(work), histos.workspace.IoFault.Write))
        for
          key <- ZIO.fromEither(histos.jdk.Sha256.from(rawKey)).orElseFail(error)
          _   <- LinkReplay.load(
            ContentStore.in(Path.of(cas)),
            key,
            Path.of(work),
            _ => Console.printLine("prepared").orElseFail(error) *> Console.readLine.orElseFail(error).unit,
          )
        yield ()
      case _ => ZIO.fail(CacheError.Io(histos.workspace.IoFailure(Path.of("args"), histos.workspace.IoFault.Read)))
  )
