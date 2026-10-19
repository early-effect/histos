package histos.load

import histos.plugin.{ApiDigest, EntryName, HistosPlugin, PluginCoordinate, PluginIo, PluginLoadError}

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** The direct jar passed the entry, version, and package checks. The class is not loaded yet. */
private[load] final case class Inspected(entry: EntryName, coordinate: PluginCoordinate)

/** Reads `META-INF/histos/plugin` and `META-INF/histos/version` from the direct jar only. */
private[load] object PluginJar:
  def inspect(dependency: histos.Dep, jar: Path, launcher: ApiDigest): IO[LoadError, Inspected] =
    ZIO.attempt(read(dependency, jar, launcher)).mapError(_ => LoadError.Plugin(PluginLoadError.Io(PluginIo.Open))).flatMap:
      case Left(error)      => ZIO.fail(LoadError.Plugin(error))
      case Right(inspected) => ZIO.succeed(inspected)

  private def read(dependency: histos.Dep, jar: Path, launcher: ApiDigest): Either[PluginLoadError, Inspected] =
    val coordinate = PluginCoordinate.Published(dependency)
    val zip = new ZipFile(jar.toFile)
    try entry(zip, coordinate).flatMap(name => version(zip, dependency, coordinate, launcher, name))
    finally zip.close()

  private def entry(zip: ZipFile, coordinate: PluginCoordinate): Either[PluginLoadError, EntryName] =
    lines(zip, HistosPlugin.entryResource) match
      case None | Some(Nil)        => Left(PluginLoadError.NoEntry(coordinate.render))
      case Some(_ :: _ :: _)       => Left(PluginLoadError.AmbiguousEntry(coordinate.render))
      case Some(line :: Nil)       =>
        EntryName.from(line) match
          case Left(_)                              => Left(PluginLoadError.NoEntry(coordinate.render))
          case Right(name) if reserved(name.value)  => Left(PluginLoadError.ReservedPackage(name.value))
          case Right(name)                          => Right(name)

  private def version(
      zip: ZipFile,
      dependency: histos.Dep,
      coordinate: PluginCoordinate,
      launcher: ApiDigest,
      entry: EntryName,
  ): Either[PluginLoadError, Inspected] =
    lines(zip, ApiDigest.resource) match
      case None | Some(Nil) | Some(_ :: _ :: _) => Left(PluginLoadError.Unversioned(coordinate.render))
      case Some(line :: Nil)                    =>
        ApiDigest.from(line) match
          case Left(_) => Left(PluginLoadError.Unversioned(coordinate.render))
          case Right(published) if published != launcher =>
            Left(PluginLoadError.VersionMismatch(dependency, published, launcher))
          case Right(_) =>
            leaked(zip) match
              case Some(binary) => Left(PluginLoadError.EngineLeak(coordinate, binary))
              case None         => Right(Inspected(entry, coordinate))

  private def lines(zip: ZipFile, name: String): Option[List[String]] =
    Option(zip.getEntry(name)).map: entry =>
      val stream = zip.getInputStream(entry)
      try new String(stream.readAllBytes(), StandardCharsets.UTF_8).split("\n", -1).toList.map(_.trim).filter(_.nonEmpty)
      finally stream.close()

  private def leaked(zip: ZipFile): Option[String] =
    zip.entries.asScala.map(_.getName).collectFirst:
      case name if name.endsWith(".class") && reservedPath(name) =>
        name.stripSuffix(".class").replace('/', '.')

  private def reservedPath(name: String): Boolean =
    name.startsWith("histos/") || name.startsWith("zio/") || name.startsWith("scala/")

  private def reserved(binary: String): Boolean =
    def under(root: String): Boolean =
      binary == root || binary.startsWith(s"$root.")
    under("histos") || under("zio") || under("scala")
