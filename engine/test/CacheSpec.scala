package histos.cache

import histos.jdk.Sha256
import histos.schedule.{Action, ActionId, Digest, Input, InputName, InputValue}
import histos.task.TaskKind

import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime
import java.time.Instant

import scala.jdk.CollectionConverters.*

import zio.*
import zio.test.*

object CacheSpec extends ZIOSpecDefault:
  private val at = Instant.parse("2020-01-01T00:00:00Z")
  private val later = Instant.parse("2020-03-01T00:00:00Z")

  private def digest(label: String): Digest =
    ActionId.from(label) match
      case Right(id) => Digest.of(Action(id, TaskKind.Inspect, List(Input(InputName.trusted("n"), InputValue.Text(label))), Nil), Map.empty)
      case Left(_)   => Digest.of(Action(idOf("n"), TaskKind.Inspect, Nil, Nil), Map.empty)

  private def idOf(label: String): ActionId =
    ActionId.from(label) match
      case Right(id) => id
      case Left(_)   => idOf("n")

  private def store: ZIO[Scope, Nothing, ContentStore] =
    ZIO.acquireRelease(ZIO.succeed(ContentStore.in(Files.createTempDirectory("histos-cache")))): saved =>
      ZIO.succeed {
        val walk = Files.walk(saved.root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore

  private val bytes = Chunk.fromArray("payload".getBytes(java.nio.charset.StandardCharsets.UTF_8))

  private val cache = suite("content store")(
    test("a put reads back and leaves no temp file"):
      ZIO.scoped:
        for
          saved <- store
          key = digest("one")
          _ <- saved.put(key, bytes, at)
          found <- saved.read(key)
          empty <- ZIO.attempt {
            val tmp = saved.root.resolve("tmp")
            if !Files.exists(tmp) then true
            else
              val list = Files.list(tmp)
              try !list.iterator.hasNext
              finally list.close()
          }
        yield assertTrue(found.contains(bytes) && empty)
    ,
    test("clean --dry-run lists an old entry and does not delete it"):
      ZIO.scoped:
        for
          saved <- store
          key = digest("old")
          _ <- saved.put(key, bytes, at)
          preview <- saved.clean(later, ContentStore.Retention, dryRun = true)
          still <- saved.read(key)
        yield assertTrue(preview.map(_.digest) == List(key) && still.contains(bytes))
    ,
    test("clean deletes an old entry and keeps a remembered one"):
      ZIO.scoped:
        for
          saved <- store
          drop = digest("drop")
          hold = digest("hold")
          _ <- saved.put(drop, bytes, at)
          _ <- saved.put(hold, bytes, at)
          _ <- saved.remember(hold)
          removed <- saved.clean(later, ContentStore.Retention, dryRun = false)
          gone <- saved.read(drop)
          kept <- saved.read(hold)
        yield assertTrue(removed.map(_.digest) == List(drop) && gone.isEmpty && kept.contains(bytes))
    ,
    test("clean lists an old plugin class directory, keeps a remembered one, and deletes the rest"):
      ZIO.scoped:
        for
          saved <- store
          old = pluginDigest("plugin-old")
          held = pluginDigest("plugin-held")
          fresh = pluginDigest("plugin-fresh")
          _ <- plant(saved, old, at)
          _ <- plant(saved, held, at)
          _ <- saved.remember(held)
          partial = saved.root.resolve("plugins").resolve(".partial-old")
          _ <- ZIO.attempt {
                 Files.createDirectories(partial)
                 Files.writeString(partial.resolve("Echo.class"), "class")
               }
          preview <- saved.clean(later, ContentStore.Retention, dryRun = true)
          still <- ZIO.attempt(Files.isDirectory(saved.root.resolve("plugins").resolve(old.value)))
          removed <- saved.clean(later, ContentStore.Retention, dryRun = false)
          gone <- ZIO.attempt(Files.exists(saved.root.resolve("plugins").resolve(old.value)))
          kept <- ZIO.attempt(Files.isDirectory(saved.root.resolve("plugins").resolve(held.value)))
          _ <- plant(saved, fresh, later)
          after <- saved.clean(later, ContentStore.Retention, dryRun = false)
          remains <- ZIO.attempt(Files.isDirectory(saved.root.resolve("plugins").resolve(fresh.value)))
          partialLeft <- ZIO.attempt(Files.isDirectory(partial))
        yield assertTrue(
          preview.map(_.digest) == List(old),
          still,
          removed.map(_.digest) == List(old),
          !gone,
          kept,
          after.isEmpty,
          remains,
          partialLeft,
        )
    ,
    test("a fresh entry is kept"):
      ZIO.scoped:
        for
          saved <- store
          key = digest("fresh")
          _ <- saved.put(key, bytes, later)
          removed <- saved.clean(later, ContentStore.Retention, dryRun = false)
        yield assertTrue(removed.isEmpty)
    ,
    test("the home directory follows XDG, then the user cache"):
      assertTrue(
        ContentStore.home(Some("/xdg"), "/home/me") == Path.of("/xdg/histos"),
        ContentStore.home(Some(""), "/home/me") == Path.of("/home/me/.cache/histos"),
        ContentStore.home(None, "/home/me") == Path.of("/home/me/.cache/histos"),
      ),
  )

  private def pluginDigest(label: String): Digest =
    val hex = Sha256.of(Chunk.fromArray(label.getBytes(java.nio.charset.StandardCharsets.UTF_8))).value
    Digest.from(hex).getOrElse(digest(label))

  private def plant(saved: ContentStore, key: Digest, at: Instant): zio.Task[Unit] =
    ZIO.attempt {
      val dir = saved.root.resolve("plugins").resolve(key.value)
      Files.createDirectories(dir)
      Files.writeString(dir.resolve("Echo.class"), "class")
      Files.setLastModifiedTime(dir, FileTime.from(at))
      ()
    }

  def spec = cache
