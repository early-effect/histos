package histos.pack

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** Files under `src/main/resources`. Entry names are the path relative to that directory. */
object Resources:
  def files(sourceBase: Path): IO[JarError, List[(String, Path)]] =
    val root = sourceBase.resolve("main").resolve("resources")
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
            .foldLeft[Either[JarError, List[(String, Path)]]](Right(Nil)): (acc, file) =>
              acc.flatMap: seen =>
                val absolute = file.toAbsolutePath.normalize
                val relative = base.relativize(absolute)
                val name     = relative.iterator.asScala.map(_.toString).mkString("/")
                if !absolute.startsWith(base) || rejected(name) then Left(JarError.Escaped(name))
                else Right(seen :+ (name, file))
      }
      .mapError(_ => JarError.Io(JarIo.Read(root)))
      .flatMap(ZIO.fromEither)

  private def rejected(name: String): Boolean =
    name.isEmpty ||
      name.startsWith("/") ||
      name.contains('\\') ||
      name.split("/", -1).exists(part => part.isEmpty || part == "." || part == "..")
end Resources
