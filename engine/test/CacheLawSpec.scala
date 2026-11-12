package histos.cache

import histos.schedule.Digest
import histos.workspace.Workspace

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import java.time.{Duration, Instant}

import zio.{Chunk, ZIO}
import zio.test.*

object CacheLawSpec extends ZIOSpecDefault:
  private val now    = Instant.parse("2026-06-01T00:00:00Z")
  private val digest = Digest.trusted("ab" * 32)
  private val other  = Digest.trusted("cd" * 32)
  private val bytes  = Chunk.fromArray("hello".getBytes)

  def spec = suite("cache laws")(
    test("a put then a get is the same bytes, and a missing key is not a hit"):
      scoped: root =>
        val store = ContentStore.in(root)
        for
          _       <- store.put(digest, bytes, now)
          found   <- store.read(digest)
          missing <- store.read(other)
          again   <- ContentStore.actions(store).get(other)
          _       <- ContentStore.actions(store).put(digest, "manifest")
          text    <- ContentStore.actions(store).get(digest)
        yield assertTrue(found == Some(bytes), missing == None, again == None, text == Some("manifest"))
    ,
    test("each project keeps its own digests"):
      scoped: root =>
        val store = ContentStore.in(root)
        val left  = CacheProject.of(root.resolve("left"))
        val right = CacheProject.of(root.resolve("right"))
        for
          _    <- store.replace(left, Set(digest))
          _    <- store.replace(right, Set(other))
          both <- store.kept
          _    <- store.replace(left, Set.empty)
          kept <- store.kept
        yield assertTrue(
          left != right,
          CacheProject.of(root.resolve("left")) == left,
          both == Set(digest, other),
          kept == Set(other),
        )
    ,
    test("a dry run lists a stale entry and leaves it, and remove deletes only what is stale and unkept"):
      scoped: root =>
        val store   = ContentStore.in(root)
        val project = CacheProject.of(root)
        val old     = now.minus(Duration.ofDays(31))
        val cutoff  = now.minus(ContentStore.Retention)
        val keptOld = Digest.trusted("e" * 64)
        val stale   = Digest.trusted("f" * 64)
        val fresh   = Digest.trusted("a1" + "b" * 62)
        for
          _       <- store.put(keptOld, bytes, old)
          _       <- store.put(stale, bytes, old)
          _       <- store.put(fresh, bytes, now.minus(Duration.ofDays(1)))
          _       <- store.replace(project, Set(keptOld))
          listed  <- store.clean(now, ContentStore.Retention, CacheCleanMode.DryRun)
          still   <- store.read(stale)
          gone    <- store.clean(now, ContentStore.Retention, CacheCleanMode.Remove)
          after   <- store.read(stale)
          held    <- store.read(keptOld)
          recent  <- store.read(fresh)
          stay    <- stamp(root.resolve("tmp").resolve("stay"), cutoff).orDie
          drop    <- stamp(root.resolve("tmp").resolve("drop"), cutoff.minusSeconds(1)).orDie
          _       <- store.clean(now, ContentStore.Retention, CacheCleanMode.Remove)
          stayed  <- ZIO.attemptBlocking(Files.exists(stay)).orDie
          dropped <- ZIO.attemptBlocking(Files.exists(drop)).orDie
        yield assertTrue(
          listed.map(_.digest) == List(stale) || listed.map(_.digest).toSet == Set(stale),
          still == Some(bytes),
          gone.map(_.digest).toSet == Set(stale),
          after == None,
          held == Some(bytes),
          recent == Some(bytes),
          stayed,
          !dropped,
        )
    ,
    test("forget drops that project's entries even when they are fresh, and a temp stamped now stays"):
      scoped: root =>
        val store = ContentStore.in(root)
        val left  = CacheProject.of(root.resolve("left"))
        val right = CacheProject.of(root.resolve("right"))
        for
          _       <- store.put(digest, bytes, now)
          _       <- store.put(other, bytes, now)
          _       <- store.replace(left, Set(digest))
          _       <- store.replace(right, Set(other))
          temp    <- stamp(root.resolve("tmp").resolve("now"), now).orDie
          removed <- store.forget(left, now)
          lost    <- store.read(digest)
          kept    <- store.read(other)
          stayed  <- ZIO.attemptBlocking(Files.exists(temp)).orDie
        yield assertTrue(
          removed.map(_.digest).toSet == Set(digest),
          lost == None,
          kept == Some(bytes),
          stayed,
        )
    ,
    test("home follows XDG_CACHE_HOME and otherwise the user cache"):
      assertTrue(
        ContentStore.home(Some("/xdg"), "/home") == Path.of("/xdg", "histos"),
        ContentStore.home(Some(""), "/home") == Path.of("/home", ".cache", "histos"),
        ContentStore.home(None, "/home") == Path.of("/home", ".cache", "histos"),
        CacheCleanMode.values.forall(mode => CacheCleanMode.from(mode.token) == Right(mode)),
        CacheCleanMode.from("wipe") == Left(CacheCleanModeError.Unknown("wipe")),
      ),
  )

  private def scoped[A](use: Path => zio.ZIO[Any, Any, A]): zio.ZIO[Any, Any, A] =
    ZIO.scoped(Workspace.work("histos-cache").flatMap(use))

  private def stamp(path: Path, at: Instant): zio.IO[Throwable, Path] =
    ZIO.attemptBlocking:
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      Files.writeString(path, "t")
      Files.setLastModifiedTime(path, FileTime.from(at))
      path
end CacheLawSpec
