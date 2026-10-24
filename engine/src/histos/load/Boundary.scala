package histos.load

import histos.workspace.{IoFailure, IoFault}

import java.nio.file.Path

import zio.{IO, ZIO}

/** Where a binary name is allowed to be defined. */
private[load] enum ClassHome:
  case Boundary, Refused, Open

private[load] object ClassHome:
  def of(name: String): ClassHome =
    if boundary(name) then ClassHome.Boundary
    else if name.startsWith("histos.") then ClassHome.Refused
    else ClassHome.Open

  /** Api surface, ZIO, izumi-reflect (`zio.Tag`), and Scala. Never a plugin jar. */
  def boundary(name: String): Boolean =
    name.startsWith("zio.") ||
      name.startsWith("izumi.reflect.") ||
      name.startsWith("scala.") ||
      name.startsWith("histos.version.") ||
      name.startsWith("histos.plugin.") ||
      root(name)

  /** Package `histos` with no subpackage. `histos.HistosBuild` is in. `histos.schedule.Sendable` is not. */
  private def root(name: String): Boolean =
    name.startsWith("histos.") && !name.substring("histos.".length).contains('.')

/** Jars the api classloader already supplies. A plugin cell must not put a second copy on a flat classpath. */
private[load] object Boundary:
  def supplied(name: String): Boolean =
    name == "scala-library" || name == "scala3-library_3" || name == "zio_3" || name == "zio-stacktracer_3" ||
      name == "izumi-reflect_3" || name.startsWith("histos-api")

  /**
   * `ZIO.succeed` summons `Trace` from `zio-stacktracer`. `zio.Tag` extends `izumi.reflect.Tag`.
   * Neither class is in the zio jar. A compile that sees `Tag` without izumi-reflect aborts in the compiler.
   */
  def jars(apiJar: Path): IO[LoadError, List[Path]] =
    ZIO.attemptBlocking {
      val libs = List(
        classOf[scala.collection.immutable.Nil.type],
        classOf[scala.quoted.Quotes],
        zioClass,
        classOf[zio.internal.stacktracer.Tracer],
        classOf[izumi.reflect.Tag[Any]],
      ).flatMap(codeSource)
      (apiJar :: libs).distinct
    }.mapError(_ => LoadError.Io(IoFailure(apiJar, IoFault.Read)))

  def codeSource(cls: Class[?]): Option[Path] =
    Option(cls.getProtectionDomain).flatMap(domain => Option(domain.getCodeSource)).flatMap: source =>
      Option(source.getLocation).flatMap: url =>
        scala.util.Try(Path.of(url.toURI)).toOption

  private def zioClass: Class[?] =
    classOf[zio.ZIO[Any, Any, Any]]
