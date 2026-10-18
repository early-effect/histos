package histos.zinc

import histos.{Platform, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** The binary series of a full Scala version. `3.10.0` is Scala 3. `2.13.18` is Scala 2.13. */
enum ScalaSeries:
  case Scala3
  case Scala2(minor: Int)

object ScalaSeries:
  def of(version: ScalaVersion): Option[ScalaSeries] =
    version.value.split("\\.", 3) match
      case Array("3", _, _)     => Some(ScalaSeries.Scala3)
      case Array("2", minor, _) => minor.toIntOption.map(ScalaSeries.Scala2(_))
      case _                    => None

  private def directory(series: ScalaSeries): String =
    series match
      case ScalaSeries.Scala3        => "scala-3"
      case ScalaSeries.Scala2(minor) => s"scala-2.$minor"

  def fromDirectory(name: String): Option[ScalaSeries] =
    val prefix = "scala-2."
    if name == directory(ScalaSeries.Scala3) then Some(ScalaSeries.Scala3)
    else if name.startsWith(prefix) then name.drop(prefix.length).toIntOption.map(ScalaSeries.Scala2(_))
    else None

/** A directory name that filters sources. A package directory with the same name does not. */
enum SourceFilter:
  case PlatformDir(platform: Platform)
  case SeriesDir(series: ScalaSeries)
  case ExactDir(version: ScalaVersion)

object SourceFilter:
  def parse(name: String): Option[SourceFilter] =
    platform(name).orElse(scalaDir(name))

  private def platform(name: String): Option[SourceFilter] =
    name match
      case "jvm"    => Some(SourceFilter.PlatformDir(Platform.Jvm))
      case "js"     => Some(SourceFilter.PlatformDir(Platform.Js))
      case "native" => Some(SourceFilter.PlatformDir(Platform.Native))
      case _        => None

  private def scalaDir(name: String): Option[SourceFilter] =
    val prefix = "scala-"
    if !name.startsWith(prefix) then None
    else
      ScalaVersion.from(name.drop(prefix.length)) match
        case Right(version) => Some(SourceFilter.ExactDir(version))
        case Left(_)        => ScalaSeries.fromDirectory(name).map(SourceFilter.SeriesDir(_))

  def allows(filter: SourceFilter, version: ScalaVersion, platform: Platform): Boolean =
    filter match
      case PlatformDir(required) => required == platform
      case ExactDir(required)    => required == version
      case SeriesDir(required)   => ScalaSeries.of(version).contains(required)

object Sources:
  /** `.scala` sources, and `.java` sources on the JVM, whose path filters match. */
  def select(root: Path, version: ScalaVersion, platform: Platform): IO[CompileError, List[Path]] =
    ZIO.attempt(list(root, version, platform)).mapError(_ => CompileError.Io(CompileIo.Read))

  private def list(root: Path, version: ScalaVersion, platform: Platform): List[Path] =
    if !Files.isDirectory(root) then Nil
    else
      val walk = Files.walk(root)
      try
        walk.iterator.asScala.filter(path => include(root, path, version, platform)).toList.sortBy(_.toString)
      finally walk.close()

  private def include(root: Path, path: Path, version: ScalaVersion, platform: Platform): Boolean =
    Files.isRegularFile(path) && source(path, platform) && allows(root, path, version, platform)

  private def source(path: Path, platform: Platform): Boolean =
    Option(path.getFileName).map(_.toString) match
      case Some(name) if name.endsWith(".scala") => true
      case Some(name) if name.endsWith(".java")  => platform == Platform.Jvm
      case _                                      => false

  /** Filters apply before the first package directory. `src` and `main` are layout, not packages. */
  private def allows(root: Path, path: Path, version: ScalaVersion, platform: Platform): Boolean =
    prefix(root.relativize(path).iterator.asScala.map(_.toString).toList.dropRight(1))
      .forall(filter => SourceFilter.allows(filter, version, platform))

  private val layout = Set("src", "main")

  private def prefix(directories: List[String]): List[SourceFilter] =
    directories match
      case Nil => Nil
      case name :: rest =>
        SourceFilter.parse(name) match
          case Some(filter) => filter :: prefix(rest)
          case None         => if layout.contains(name) then prefix(rest) else Nil
