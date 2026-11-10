package histos.pack

import histos.{AssetError, ResourceName}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** Files under a resource directory. Entry names are the path relative to that directory. Sync copies them into the
  * class directory, so run, test, and package read one tree.
  */
object Resources:
  def files(sourceBase: Path): IO[JarError, List[(String, Path)]] =
    filesAt(sourceBase.resolve("main").resolve("resources")).mapBoth(
      {
        case AssetError.Escaped(raw)         => JarError.Escaped(raw)
        case AssetError.Duplicate(name)      => JarError.Duplicate(name.value)
        case AssetError.Collides(name)       => JarError.Duplicate(name.value)
        case AssetError.Missing(path)        => JarError.Io(JarIo.Read(path))
        case AssetError.Unreadable(path)     => JarError.Io(JarIo.Read(path))
        case AssetError.CommittedEntry(path) => JarError.Io(JarIo.Read(path))
      },
      _.map((name, path) => name.value -> path),
    )

  /** Regular files under `root`. A missing directory is empty. A name that leaves the directory is refused. */
  def filesAt(root: Path): IO[AssetError, List[(ResourceName, Path)]] =
    ZIO
      .attemptBlocking {
        if !Files.isDirectory(root) then Right(Nil)
        else
          val base  = root.toAbsolutePath.normalize
          val walk  = Files.walk(root)
          val found =
            try walk.iterator.asScala.filter(path => Files.isRegularFile(path)).toList
            finally walk.close()
          found
            .sortBy(_.toString)
            .foldLeft[Either[AssetError, List[(ResourceName, Path)]]](Right(Nil)): (acc, file) =>
              acc.flatMap: seen =>
                val absolute = file.toAbsolutePath.normalize
                val relative = base.relativize(absolute)
                val name     = relative.iterator.asScala.map(_.toString).mkString("/")
                if !absolute.startsWith(base) then Left(AssetError.Escaped(name))
                else
                  ResourceName.from(name) match
                    case Left(_)                                                     => Left(AssetError.Escaped(name))
                    case Right(safe) if safe.value == AssetError.refusedName =>
                      Left(AssetError.CommittedEntry(file))
                    case Right(safe) => Right(seen :+ (safe, file))
      }
      .mapError(_ => AssetError.Unreadable(root))
      .flatMap(ZIO.fromEither)
end Resources
