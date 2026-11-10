package histos.load

import histos.workspace.{IoFailure, IoFault}

import java.nio.file.{Files, LinkOption, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

private[load] object ReservedPackage:
  def binary(name: String): Boolean =
    name == "histos" || name.startsWith("histos.") ||
      name == "zio" || name.startsWith("zio.") ||
      name == "scala" || name.startsWith("scala.")

  def path(name: String): Boolean =
    name.startsWith("histos/") || name.startsWith("zio/") || name.startsWith(
      "scala/"
    )

  /** A `.class` file under `histos`, `zio`, or `scala`. */
  def engineClass(root: Path): IO[LoadError, Option[String]] =
    ZIO
      .attemptBlocking:
        if !Files.isDirectory(root) then None
        else
          val walk = Files.walk(root)
          try
            walk.iterator.asScala.collectFirst:
              case file if classFile(file) && path(rel(root, file)) =>
                rel(root, file).stripSuffix(".class").replace('/', '.')
          finally walk.close()
      .mapError(_ => LoadError.Io(IoFailure(root, IoFault.Walk)))

  private def rel(root: Path, file: Path): String =
    root.relativize(file).toString.replace('\\', '/')

  private def classFile(file: Path): Boolean =
    Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS) &&
      Option(file.getFileName).exists(_.toString.endsWith(".class"))
end ReservedPackage
