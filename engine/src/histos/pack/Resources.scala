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

  /** Conventional resources use the same scope, platform, and Scala-prefix rules as sources. */
  def selected(
      root: Path,
      version: histos.ScalaVersion,
      platform: histos.Platform,
      scope: histos.plugin.SourceSet,
  ): IO[AssetError, List[(ResourceName, Path)]] =
    paths(root).flatMap: files =>
      val selected = files.flatMap { path =>
        histos.zinc.Sources.resourceEntry(root, path, version, platform, scope).map(_ -> path)
      }
      ZIO.foreach(selected) { (entry, path) =>
        ZIO.fromEither(named(entry, path)).map(_ -> path)
      }

  /** Regular files under `root`. A missing directory is empty. A name that leaves the directory is refused. */
  def filesAt(root: Path): IO[AssetError, List[(ResourceName, Path)]] =
    val base = root.toAbsolutePath.normalize
    paths(root).flatMap: files =>
      ZIO.foreach(files): file =>
        val absolute = file.toAbsolutePath.normalize
        if !absolute.startsWith(base) then ZIO.fail(AssetError.Escaped(file.toString))
        else
          val name = base.relativize(absolute).iterator.asScala.map(_.toString).mkString("/")
          ZIO.fromEither(named(name, file)).map(_ -> file)

  private def named(raw: String, path: Path): Either[AssetError, ResourceName] =
    ResourceName
      .from(raw)
      .left
      .map(_ => AssetError.Escaped(raw))
      .flatMap(validate(_, path))

  private[histos] def validate(name: ResourceName, path: Path): Either[AssetError, ResourceName] =
    if name.value == AssetError.refusedName then Left(AssetError.CommittedEntry(path)) else Right(name)

  private def paths(root: Path): IO[AssetError, List[Path]] =
    ZIO
      .attemptBlocking {
        if !Files.isDirectory(root) then Nil
        else
          val walk = Files.walk(root)
          try walk.iterator.asScala.filter(Files.isRegularFile(_)).toList.sortBy(_.toString)
          finally walk.close()
      }
      .mapError(_ => AssetError.Unreadable(root))
end Resources
