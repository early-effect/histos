package histos.cache

import histos.jdk.Sha256
import histos.schedule.{Action, ActionId, CacheError, Digest, Input, InputName, InputValue}
import histos.workspace.{IoFailure, IoFault}
import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime
import java.time.Instant

import scala.jdk.CollectionConverters.*

import zio.*
import zio.test.*

object CacheSpec extends histos.SuiteClock:
  private val at    = Instant.parse("2020-01-01T00:00:00Z")
  private val later = Instant.parse("2020-03-01T00:00:00Z")

  private def digest(label: String): Digest =
    ActionId.from(label) match
      case Right(id) =>
        Digest.of(
          Action(
            id,
            List(Input(InputName.trusted("n"), InputValue.Text(label))),
            Nil,
          ),
          Map.empty,
        )
      case Left(_) => Digest.of(Action(idOf("n"), Nil, Nil), Map.empty)

  private def idOf(label: String): ActionId =
    ActionId.from(label) match
      case Right(id) => id
      case Left(_)   => idOf("n")

  private def store: ZIO[Scope, Nothing, ContentStore] =
    ZIO.acquireRelease(
      ZIO.succeed(ContentStore.in(Files.createTempDirectory("histos-cache")))
    ): saved =>
      ZIO.succeed {
        val walk  = Files.walk(saved.root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths
          .sortBy(_.getNameCount)
          .reverse
          .foreach(path => Files.deleteIfExists(path))
      }.ignore

  private val bytes =
    Chunk.fromArray("payload".getBytes(java.nio.charset.StandardCharsets.UTF_8))
  private val project   = CacheProject.of(Path.of("/repo/a"))
  private val elsewhere = CacheProject.of(Path.of("/repo/b"))

  private val cache = suite("content store")(
    test("a put reads back and leaves no temp file"):
      ZIO.scoped:
        for
          saved <- store
          key = digest("one")
          _     <- saved.put(key, bytes, at)
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
    test("a directory at an object path is a typed stat failure"):
      ZIO.scoped:
        for
          saved <- store
          key        = digest("directory")
          objectPath = saved.root.resolve("cas").resolve(key.value.take(2)).resolve(key.value.drop(2))
          _     <- ZIO.attempt(Files.createDirectories(objectPath))
          found <- saved.read(key).either
        yield assertTrue(found == Left(CacheError.Io(IoFailure(objectPath, IoFault.Stat))))
    ,
    test("clean --dry-run lists an old entry and does not delete it"):
      ZIO.scoped:
        for
          saved <- store
          key = digest("old")
          _       <- saved.put(key, bytes, at)
          preview <- saved.clean(later, ContentStore.Retention, CacheCleanMode.DryRun)
          still   <- saved.read(key)
        yield assertTrue(
          preview.map(_.digest) == List(key) && still.contains(bytes)
        )
    ,
    test("clean deletes an old entry and keeps a remembered one"):
      ZIO.scoped:
        for
          saved <- store
          drop = digest("drop")
          hold = digest("hold")
          _       <- saved.put(drop, bytes, at)
          _       <- saved.put(hold, bytes, at)
          _       <- saved.replace(project, Set(hold))
          removed <- saved.clean(later, ContentStore.Retention, CacheCleanMode.Remove)
          gone    <- saved.read(drop)
          kept    <- saved.read(hold)
        yield assertTrue(
          removed.map(_.digest) == List(drop) && gone.isEmpty && kept.contains(
            bytes
          )
        )
    ,
    test(
      "clean lists an old plugin class directory, keeps a remembered one, and deletes the rest"
    ):
      ZIO.scoped:
        for
          saved <- store
          old   = pluginDigest("plugin-old")
          held  = pluginDigest("plugin-held")
          fresh = pluginDigest("plugin-fresh")
          _ <- plant(saved, old, at)
          _ <- plant(saved, held, at)
          _ <- saved.replace(project, Set(held))
          partial = saved.root.resolve("plugins").resolve(".partial-old")
          _ <- ZIO.attempt {
            Files.createDirectories(partial)
            Files.writeString(partial.resolve("Echo.class"), "class")
          }
          preview <- saved.clean(later, ContentStore.Retention, CacheCleanMode.DryRun)
          still   <- ZIO.attempt(
            Files.isDirectory(saved.root.resolve("plugins").resolve(old.value))
          )
          removed <- saved.clean(later, ContentStore.Retention, CacheCleanMode.Remove)
          gone    <- ZIO.attempt(
            Files.exists(saved.root.resolve("plugins").resolve(old.value))
          )
          kept <- ZIO.attempt(
            Files.isDirectory(saved.root.resolve("plugins").resolve(held.value))
          )
          _       <- plant(saved, fresh, later)
          after   <- saved.clean(later, ContentStore.Retention, CacheCleanMode.Remove)
          remains <- ZIO.attempt(
            Files.isDirectory(
              saved.root.resolve("plugins").resolve(fresh.value)
            )
          )
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
    test("clean sweeps an old temp and leaves a fresh one"):
      ZIO.scoped:
        for
          saved <- store
          old   = saved.root.resolve("tmp").resolve("old")
          fresh = saved.root.resolve("tmp").resolve("fresh")
          _ <- ZIO.attempt {
            Files.createDirectories(old.getParent)
            Files.writeString(old, "leftover")
            Files.writeString(fresh, "live")
            Files.setLastModifiedTime(old, FileTime.from(at))
            Files.setLastModifiedTime(fresh, FileTime.from(later))
          }
          _          <- saved.clean(later, ContentStore.Retention, CacheCleanMode.DryRun)
          previewOld <- ZIO.attempt(Files.isRegularFile(old))
          removed    <- saved.clean(later, ContentStore.Retention, CacheCleanMode.Remove)
          gone       <- ZIO.attempt(Files.exists(old))
          kept       <- ZIO.attempt(Files.isRegularFile(fresh))
        yield assertTrue(previewOld, removed.isEmpty, !gone, kept)
    ,
    test(
      "each project keeps its own set, and replacing one does not drop the other"
    ):
      ZIO.scoped:
        for
          saved <- store
          hold  = digest("hold")
          other = digest("other")
          drop  = digest("drop")
          _         <- saved.put(hold, bytes, at)
          _         <- saved.put(other, bytes, at)
          _         <- saved.put(drop, bytes, at)
          _         <- saved.replace(project, Set(hold))
          _         <- saved.replace(elsewhere, Set(other))
          first     <- saved.clean(later, ContentStore.Retention, CacheCleanMode.Remove)
          _         <- saved.replace(project, Set.empty)
          second    <- saved.clean(later, ContentStore.Retention, CacheCleanMode.Remove)
          holdGone  <- saved.read(hold)
          otherKept <- saved.read(other)
        yield assertTrue(
          first.map(_.digest) == List(drop),
          second.map(_.digest) == List(hold),
          holdGone.isEmpty,
          otherKept.contains(bytes),
        )
    ,
    test("a legacy keep file is still honored"):
      ZIO.scoped:
        for
          saved <- store
          hold = digest("legacy")
          _ <- saved.put(hold, bytes, at)
          _ <- ZIO.attempt(
            Files.writeString(saved.root.resolve("keep"), hold.value + "\n")
          )
          kept    <- saved.kept
          removed <- saved.clean(later, ContentStore.Retention, CacheCleanMode.Remove)
          still   <- saved.read(hold)
        yield assertTrue(
          kept.contains(hold),
          removed.isEmpty,
          still.contains(bytes),
        )
    ,
    test("replacing a project's set does not append"):
      ZIO.scoped:
        for
          saved <- store
          hold  = digest("hold")
          other = digest("other")
          _     <- ZIO.foreachParDiscard(List(hold, other))(key => saved.replace(project, Set(key)))
          after <- saved.kept
        yield assertTrue(
          after.size == 1,
          after.contains(hold) || after.contains(other),
        )
    ,
    test("a fresh entry is kept"):
      ZIO.scoped:
        for
          saved <- store
          key = digest("fresh")
          _       <- saved.put(key, bytes, later)
          removed <- saved.clean(later, ContentStore.Retention, CacheCleanMode.Remove)
        yield assertTrue(removed.isEmpty)
    ,
    test(
      "forget drops this project's entries of any age and leaves the other project"
    ):
      ZIO.scoped:
        for
          saved <- store
          shared    = digest("shared")
          exclusive = digest("exclusive")
          other     = digest("other")
          pluginKey = digest("plugin")
          _ <- saved.put(shared, bytes, at)
          _ <- saved.put(exclusive, bytes, later)
          _ <- saved.put(other, bytes, at)
          _ <- plant(saved, pluginKey, at)
          _ <- saved.replace(project, Set(shared, exclusive, pluginKey))
          _ <- saved.replace(elsewhere, Set(shared, other))
          now     = later
          stamped = saved.root.resolve("tmp").resolve("now")
          old     = saved.root.resolve("tmp").resolve("old")
          _ <- ZIO.attempt {
            Files.createDirectories(stamped.getParent)
            Files.writeString(stamped, "live")
            Files.writeString(old, "leftover")
            Files.setLastModifiedTime(stamped, FileTime.from(now))
            Files.setLastModifiedTime(old, FileTime.from(at))
          }
          gone          <- saved.forget(project, now)
          sharedStill   <- saved.read(shared)
          exclusiveGone <- saved.read(exclusive)
          otherStill    <- saved.read(other)
          pluginGone    <- ZIO.attempt(
            !Files.exists(saved.root.resolve("plugins").resolve(pluginKey.value))
          )
          projectKeep <- ZIO.attempt(
            Files.exists(saved.root.resolve("keep").resolve(project.value))
          )
          otherKeep <- ZIO.attempt(
            Files.isRegularFile(
              saved.root.resolve("keep").resolve(elsewhere.value)
            )
          )
          nowStays <- ZIO.attempt(Files.isRegularFile(stamped))
          oldGone  <- ZIO.attempt(!Files.exists(old))
        yield assertTrue(
          gone.map(_.digest).toSet == Set(exclusive, pluginKey),
          sharedStill.contains(bytes),
          exclusiveGone.isEmpty,
          otherStill.contains(bytes),
          pluginGone,
          !projectKeep,
          otherKeep,
          nowStays,
          oldGone,
        )
    ,
    test("the home directory follows XDG, then the user cache"):
      assertTrue(
        ContentStore.home(Some("/xdg"), "/home/me") == Path.of("/xdg/histos"),
        ContentStore.home(Some(""), "/home/me") == Path.of(
          "/home/me/.cache/histos"
        ),
        ContentStore.home(None, "/home/me") == Path.of("/home/me/.cache/histos"),
      ),
  )

  private def pluginDigest(label: String): Digest =
    val hex = Sha256
      .of(
        Chunk.fromArray(label.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      )
      .value
    Digest.from(hex).getOrElse(digest(label))

  private def plant(
      saved: ContentStore,
      key: Digest,
      at: Instant,
  ): zio.Task[Unit] =
    ZIO.attempt {
      val dir = saved.root.resolve("plugins").resolve(key.value)
      Files.createDirectories(dir)
      Files.writeString(dir.resolve("Echo.class"), "class")
      Files.setLastModifiedTime(dir, FileTime.from(at))
      ()
    }

  def spec = cache
end CacheSpec
