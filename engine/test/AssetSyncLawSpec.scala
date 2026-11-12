package histos.pack

import histos.plugin.RepoPath
import histos.workspace.Workspace
import histos.{AssetError, GeneratedAsset, ResourceName}

import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

/** Unmanaged resources and generated assets share one class directory. The ownership list lives beside it. */
object AssetSyncLawSpec extends ZIOSpecDefault:
  def spec = suite("asset sync")(
    test("generated text replaces an unmanaged file, a stale file is removed, and a class file stays"):
      Workspace
        .work("histos-asset-sync")
        .flatMap: root =>
          val classes   = root.resolve("classes")
          val resources = root.resolve("resources")
          val manifest  = root.resolve("analysis").resolve(AssetSync.Manifest)
          val payload   = root.resolve("payload.txt")
          for
            _         <- Workspace.writeString(resources.resolve("note.txt"), "plain")
            _         <- Workspace.writeString(resources.resolve("extra.txt"), "kept")
            _         <- Workspace.writeString(payload, "file")
            _         <- Workspace.writeString(classes.resolve("old.txt"), "stale")
            _         <- Workspace.writeString(classes.resolve("Hello.class"), "class")
            _         <- Workspace.writeString(manifest, "old.txt\nHello.class\n")
            unmanaged <- Resources.filesAt(resources)
            _         <- AssetSync.sync(
              classes,
              Some(manifest),
              unmanaged,
              List(
                GeneratedAsset.Text(ResourceName("note.txt"), "generated"),
                GeneratedAsset.File(ResourceName("side.txt"), RepoPath("payload.txt")),
              ),
              root,
            )
            note    <- Workspace.readString(classes.resolve("note.txt"))
            extra   <- Workspace.readString(classes.resolve("extra.txt"))
            side    <- Workspace.readString(classes.resolve("side.txt"))
            klass   <- Workspace.readString(classes.resolve("Hello.class"))
            listed  <- Workspace.readString(manifest)
            oldGone <- gone(classes.resolve("old.txt"))
            version <- gone(classes.resolve("META-INF").resolve("histos").resolve("version"))
          yield assertTrue(
            note == "generated",
            extra == "kept",
            side == "file",
            klass == "class",
            oldGone,
            version,
            listed == "extra.txt\nnote.txt\nside.txt\n",
          )
    ,
    test("a class-file collision is refused before a stale file is deleted"):
      Workspace
        .work("histos-asset-collide")
        .flatMap: root =>
          val classes  = root.resolve("classes")
          val manifest = root.resolve("analysis").resolve(AssetSync.Manifest)
          val note     = classes.resolve("note.txt")
          for
            _      <- Workspace.writeString(classes.resolve("Hello.class"), "class")
            _      <- Workspace.writeString(note, "keep")
            _      <- Workspace.writeString(manifest, "note.txt\n")
            result <- AssetSync
              .sync(
                classes,
                Some(manifest),
                Nil,
                List(GeneratedAsset.Text(ResourceName("Hello.class"), "no")),
                root,
              )
              .either
            kept   <- Workspace.readString(note)
            klass  <- Workspace.readString(classes.resolve("Hello.class"))
            listed <- Workspace.readString(manifest)
          yield assertTrue(
            result == Left(AssetError.Collides(ResourceName("Hello.class"))),
            kept == "keep",
            klass == "class",
            listed == "note.txt\n",
          )
    ,
    test("a missing file, a repeated name, a bad manifest line, and an escaping name are typed errors"):
      Workspace
        .work("histos-asset-refuse")
        .flatMap: root =>
          val classes  = root.resolve("classes")
          val manifest = root.resolve("analysis").resolve(AssetSync.Manifest)
          val outside  = root.resolve("out.txt")
          val note     = ResourceName("note.txt")
          for
            _       <- Workspace.writeString(manifest, "../x\n")
            _       <- Workspace.writeString(classes.resolve("note.txt"), "keep")
            missing <- AssetSync
              .sync(
                classes,
                None,
                Nil,
                List(GeneratedAsset.File(ResourceName("gone.txt"), RepoPath("missing.txt"))),
                root,
              )
              .either
            twice <- AssetSync
              .sync(
                classes,
                None,
                Nil,
                List(GeneratedAsset.Text(note, "a"), GeneratedAsset.Text(note, "b")),
                root,
              )
              .either
            bad <- AssetSync
              .sync(classes, Some(manifest), Nil, List(GeneratedAsset.Text(note, "c")), root)
              .either
            escaped <- AssetSync
              .sync(classes, None, Nil, List(GeneratedAsset.Text(ResourceName.trusted("../out.txt"), "no")), root)
              .either
            kept        <- Workspace.readString(classes.resolve("note.txt"))
            outsideGone <- gone(outside)
            missingPath = root.resolve("missing.txt")
          yield assertTrue(
            missing == Left(AssetError.Missing(missingPath)),
            twice == Left(AssetError.Duplicate(note)),
            bad == Left(AssetError.Unreadable(manifest)),
            escaped == Left(AssetError.Escaped("../out.txt")),
            kept == "keep",
            outsideGone,
          ),
  )

  private def gone(path: Path) =
    ZIO.attemptBlocking(!Files.exists(path)).orDie
end AssetSyncLawSpec
