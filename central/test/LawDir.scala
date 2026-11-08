package histos.central

import java.nio.file.{Files, Path}

import zio.*

/** One temp root for a suite. Each law takes a child so parallel tests do not share a directory. */
object LawDir:
  val layer: ZLayer[Scope, Nothing, Path] =
    ZLayer.scoped:
      ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("histos-law-")).orDie)(release)

  def child(root: Path, label: String): UIO[Path] =
    ZIO.attemptBlocking(Files.createTempDirectory(root, label)).orDie

  private def release(root: Path): UIO[Unit] =
    ZIO.attemptBlocking(delete(root)).ignore

  private def delete(root: Path): Unit =
    if Files.exists(root) then
      val walk = Files.walk(root)
      try
        val paths = List.newBuilder[Path]
        val items = walk.iterator()
        while items.hasNext do paths += items.next()
        paths.result().sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      finally walk.close()
end LawDir
