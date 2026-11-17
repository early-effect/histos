package histos.load

import histos.plugin.{Digests, PluginLoadError}
import histos.workspace.IoFault

import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** Class-entry identity across one generation classpath. Equal bytes share an origin. Different bytes name both. */
private[load] object ClasspathIndex:
  def include(seen: Map[String, Origin], path: Path): IO[LoadError, Map[String, Origin]] =
    entries(path).flatMap: found =>
      found.foldLeft[IO[LoadError, Map[String, Origin]]](ZIO.succeed(seen)): (acc, entry) =>
        acc.flatMap: current =>
          current.get(entry.name) match
            case Some(prior) if prior.digest == entry.digest =>
              ZIO.succeed(current)
            case Some(prior) =>
              ZIO.fail(
                LoadError.Plugin(PluginLoadError.ClasspathConflict(entry.name, prior.path, path))
              )
            case None =>
              ZIO.succeed(current.updated(entry.name, Origin(path, entry.digest)))

  def includeAll(paths: List[Path]): IO[LoadError, Map[String, Origin]] =
    ZIO.foldLeft(paths)(Map.empty[String, Origin])(include)

  final case class Origin(path: Path, digest: String)

  private final case class Entry(name: String, digest: String)

  private def entries(path: Path): IO[LoadError, List[Entry]] =
    ZIO
      .attemptBlocking(if Files.isDirectory(path) then directory(path) else jar(path))
      .mapError(BuildFile.failed(path, IoFault.Read))

  private def directory(root: Path): List[Entry] =
    val walk = Files.walk(root)
    try
      walk.iterator.asScala.toList.flatMap: path =>
        Option(path.getFileName)
          .map(_.toString)
          .filter(name => name.endsWith(".class"))
          .map: _ =>
            val name  = root.relativize(path).toString.replace('\\', '/')
            val bytes = Files.readAllBytes(path)
            Entry(name, Digests.sha256(bytes))
    finally walk.close()

  private def jar(path: Path): List[Entry] =
    val zip = new ZipFile(path.toFile)
    try
      zip.entries.asScala.toList.flatMap: entry =>
        val name = entry.getName
        if entry.isDirectory || !name.endsWith(".class") || !safe(name) then None
        else
          val stream = zip.getInputStream(entry)
          try Some(Entry(name, Digests.sha256(stream.readAllBytes())))
          finally stream.close()
    finally zip.close()

  private def safe(name: String): Boolean =
    !name.startsWith("/") && !name.contains("..")
end ClasspathIndex
