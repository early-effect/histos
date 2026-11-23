package histos.load

import histos.workspace.{IoFailure, IoFault}

import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** Where a binary name is allowed to be defined. */
private[load] enum ClassHome:
  case Boundary, Refused, Open

private[load] object ClassHome:
  def of(name: String): ClassHome =
    if boundary(name) then ClassHome.Boundary
    else if name.startsWith("histos.") then ClassHome.Refused
    else ClassHome.Open

  /** Packages present in the running API artifact, plus the host process packages. Engine packages are not in that
    * artifact, so they stay refused.
    */
  def boundary(name: String): Boolean =
    hostProcess(name) || exported.contains(packageOf(name))

  private def hostProcess(name: String): Boolean =
    name.startsWith("zio.") || name.startsWith("izumi.reflect.") || name.startsWith("scala.")

  private def packageOf(name: String): String =
    val dot = name.lastIndexOf('.')
    if dot < 0 then "" else name.substring(0, dot)

  private lazy val exported: Set[String] =
    histos.load.Boundary.codeSource(classOf[histos.HistosBuild]) match
      case Some(path) if Files.isDirectory(path) => packagesInTree(path)
      case Some(path)                            => packagesInJar(path)
      case None                                  => Set.empty

  private def packagesInTree(root: Path): Set[String] =
    val walk = Files.walk(root)
    try
      walk.iterator.asScala.foldLeft(Set.empty[String]): (seen, path) =>
        val classEntry = Files.isRegularFile(path) && Option(path.getFileName).exists(name => classFile(name.toString))
        if classEntry then
          seen + packageOf(root.relativize(path).toString.replace('\\', '/').stripSuffix(".class").replace('/', '.'))
        else seen
    finally walk.close()

  private def packagesInJar(path: Path): Set[String] =
    val zip = new ZipFile(path.toFile)
    try
      zip.entries.asScala.foldLeft(Set.empty[String]): (seen, entry) =>
        val name = entry.getName
        if classFile(name) then seen + packageOf(name.stripSuffix(".class").replace('/', '.'))
        else seen
    finally zip.close()

  private def classFile(name: String): Boolean =
    name.endsWith(".class") && !name.startsWith("META-INF/")
end ClassHome

/** Jars the api classloader already supplies. A plugin cell must not put a second copy on a flat classpath. */
private[load] object Boundary:
  /** `ZIO.succeed` summons `Trace` from `zio-stacktracer`. `zio.Tag` extends `izumi.reflect.Tag`. Neither class is in
    * the zio jar. A compile that sees `Tag` without izumi-reflect aborts in the compiler.
    */
  def jars(apiJar: Path): IO[LoadError, List[Path]] =
    ZIO
      .attemptBlocking {
        val libs = List(
          classOf[scala.collection.immutable.Nil.type],
          classOf[scala.quoted.Quotes],
          zioClass,
          classOf[zio.internal.stacktracer.Tracer],
          classOf[izumi.reflect.Tag[Any]],
        ).flatMap(codeSource)
        (apiJar :: libs).distinct
      }
      .mapError(_ => LoadError.Io(IoFailure(apiJar, IoFault.Read)))

  def codeSource(cls: Class[?]): Option[Path] =
    Option(cls.getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap: source =>
        Option(source.getLocation).flatMap: url =>
          scala.util.Try(Path.of(url.toURI)).toOption

  private def zioClass: Class[?] =
    classOf[zio.ZIO[Any, Any, Any]]
end Boundary
