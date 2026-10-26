package histos.load

import histos.plugin.{ApiDigest, EntryName, HistosPlugin, PluginCoordinate, PluginIo, PluginLoadError}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** The direct jar's entry, the api digest it was built against, and a reserved class if one is present.
  */
private[load] final case class ReadPlugin(
    entry: EntryName,
    published: ApiDigest,
    leak: Option[String],
)

/** Reads `META-INF/histos/plugin` and `META-INF/histos/version` from the direct jar only.
  */
private[load] object PluginJar:
  def read(dependency: histos.Dep, jar: Path): IO[LoadError, ReadPlugin] =
    ZIO
      .attemptBlocking(open(dependency, jar))
      .mapError(_ => LoadError.Plugin(PluginLoadError.Io(PluginIo.Open(jar))))
      .flatMap:
        case Left(error) => ZIO.fail(LoadError.Plugin(error))
        case Right(read) => ZIO.succeed(read)

  /** The entry resource copied into a path plugin's class directory. There is no version resource.
    */
  def readTree(
      coordinate: PluginCoordinate,
      classes: Path,
  ): IO[LoadError, EntryName] =
    ZIO
      .attemptBlocking(tree(coordinate, classes))
      .mapError(_ => LoadError.Plugin(PluginLoadError.Io(PluginIo.Read(classes))))
      .flatMap:
        case Left(error) => ZIO.fail(LoadError.Plugin(error))
        case Right(name) => ZIO.succeed(name)

  private def open(
      dependency: histos.Dep,
      jar: Path,
  ): Either[PluginLoadError, ReadPlugin] =
    val coordinate = PluginCoordinate.Published(dependency)
    val zip        = new ZipFile(jar.toFile)
    try entry(zip, coordinate).flatMap(name => version(zip, coordinate, name))
    finally zip.close()

  private def tree(
      coordinate: PluginCoordinate,
      classes: Path,
  ): Either[PluginLoadError, EntryName] =
    val file = classes.resolve(HistosPlugin.entryResource)
    if !Files.isRegularFile(file) then Left(PluginLoadError.NoEntry(coordinate.render))
    else
      named(
        coordinate,
        Files
          .readString(file)
          .split("\n", -1)
          .toList
          .map(_.trim)
          .filter(_.nonEmpty),
      )

  private def entry(
      zip: ZipFile,
      coordinate: PluginCoordinate,
  ): Either[PluginLoadError, EntryName] =
    lines(zip, HistosPlugin.entryResource) match
      case None        => Left(PluginLoadError.NoEntry(coordinate.render))
      case Some(found) => named(coordinate, found)

  private def named(
      coordinate: PluginCoordinate,
      found: List[String],
  ): Either[PluginLoadError, EntryName] =
    found match
      case Nil         => Left(PluginLoadError.NoEntry(coordinate.render))
      case _ :: _ :: _ =>
        Left(PluginLoadError.AmbiguousEntry(coordinate.render))
      case line :: Nil =>
        EntryName.from(line) match
          case Left(_)                                           => Left(PluginLoadError.NoEntry(coordinate.render))
          case Right(name) if ReservedPackage.binary(name.value) =>
            Left(PluginLoadError.ReservedPackage(name.value))
          case Right(name) => Right(name)

  private def version(
      zip: ZipFile,
      coordinate: PluginCoordinate,
      entry: EntryName,
  ): Either[PluginLoadError, ReadPlugin] =
    lines(zip, ApiDigest.resource) match
      case None | Some(Nil) | Some(_ :: _ :: _) =>
        Left(PluginLoadError.Unversioned(coordinate.render))
      case Some(line :: Nil) =>
        ApiDigest.from(line) match
          case Left(_)          => Left(PluginLoadError.Unversioned(coordinate.render))
          case Right(published) =>
            Right(ReadPlugin(entry, published, leaked(zip)))

  private def lines(zip: ZipFile, name: String): Option[List[String]] =
    Option(zip.getEntry(name)).map: entry =>
      val stream = zip.getInputStream(entry)
      try
        new String(stream.readAllBytes(), StandardCharsets.UTF_8)
          .split("\n", -1)
          .toList
          .map(_.trim)
          .filter(_.nonEmpty)
      finally stream.close()

  private def leaked(zip: ZipFile): Option[String] =
    zip.entries.asScala
      .map(_.getName)
      .collectFirst:
        case name if name.endsWith(".class") && ReservedPackage.path(name) =>
          name.stripSuffix(".class").replace('/', '.')
end PluginJar
