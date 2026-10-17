package histos.zinc

import histos.ScalaVersion

import java.net.{URL, URLClassLoader}
import java.util.Enumeration
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.zip.{ZipException, ZipFile}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/**
 * The Scala 3 compiler a module names. It is fetched off the engine classpath, because the engine
 * runs on one Scala 3 version and a module may name another.
 */
private[zinc] object Scala3:
  private[zinc] final class Toolchain private[Scala3] (
      private[zinc] val bridgeJar: Path,
      private[zinc] val runtime: List[Path],
      private[zinc] val compilerClasspath: List[Path],
  ):
    /**
     * Parent is the platform loader, plus `xsbti` from Zinc. The engine's Scala library and
     * compiler are not visible here. `xsbti` is shared because the compiler calls Zinc's callback.
     */
    private[zinc] def openLoader(engine: ClassLoader): URLClassLoader =
      new URLClassLoader(
        (compilerClasspath ++ runtime).map(_.toUri.toURL).toArray,
        new EngineBoundary(engine),
      )

  private[zinc] def jars(version: ScalaVersion): IO[CompileError, Toolchain] =
    ZIO.attemptBlockingInterrupt(fetch(version)).mapError(_ => CompileError.Toolchain(ToolchainMiss.Fetch(version))).flatMap(ZIO.fromEither)

  private val Organization = coursier.Organization("org.scala-lang")
  private val ListClass = "scala/collection/immutable/List.class"
  private val Scala3Runtime = "scala/runtime/Scala3RunTime.class"
  private val LibraryProperties = "library.properties"
  private val VersionNumber = "version.number="

  private enum Module:
    case Compiler, Bridge

    def artifact: String = this match
      case Compiler => "scala3-compiler_3"
      case Bridge   => "scala3-sbt-bridge"

    def dependency(version: ScalaVersion): coursier.core.Dependency =
      coursier.Dependency(
        coursier.Module(Organization, coursier.ModuleName(artifact)),
        coursier.version.VersionConstraint(version.value),
      )

  private enum Found:
    case Bridge(path: Path)
    case Compiler(path: Path)
    case Stdlib(path: Path, version: ScalaVersion, scala3: Boolean)
    case Scala3Lib(path: Path)
    case Interface(path: Path)
    case Support(path: Path)

  private def fetch(version: ScalaVersion): Either[CompileError, Toolchain] =
    val files = coursier.cache.FileCache()
    val got = coursier.Fetch(files)(using coursier.util.Task.sync)
      .noMirrors
      .withGradleModuleSupport(false)
      .addDependencies(Module.values.toSeq.map(_.dependency(version))*)
      .eitherResult()
    got match
      case Left(_)       => Left(CompileError.Toolchain(ToolchainMiss.Fetch(version)))
      case Right(result) => assemble(version, result.files.map(_.toPath).toList)

  private def assemble(version: ScalaVersion, paths: List[Path]): Either[CompileError, Toolchain] =
    val found = paths.map(inspect)
    found.collect { case Left(path) => path } match
      case Nil =>
        val jars = found.collect { case Right(item) => item }
        for
          bridge   <- only(version, jars.collect { case Found.Bridge(path) => path }, CompilerPart.Scala3Bridge)
          compiler <- only(version, jars.collect { case Found.Compiler(path) => path }, CompilerPart.Scala3Compiler)
          runtime  <- runtimeOf(version, jars)
        yield
          val support = jars.collect { case Found.Support(path) => path }
          new Toolchain(bridge, runtime, compiler :: support)
      case foreign =>
        Left(CompileError.Toolchain(ToolchainMiss.Unexpected(version, foreign)))

  /** `scala-library` is the classpath a program sees. Before the library merge it also needs `scala3-library`. */
  private def runtimeOf(version: ScalaVersion, jars: List[Found]): Either[CompileError, List[Path]] =
    val stdlibs = jars.collect { case Found.Stdlib(path, found, scala3) => (path, found, scala3) }
    val legacy = jars.collect { case Found.Scala3Lib(path) => path }
    stdlibs match
      case (path, found, scala3) :: Nil if libraryOk(version, found) && scala3 && legacy.isEmpty =>
        Right(List(path))
      case (_, found, true) :: Nil if libraryOk(version, found) =>
        Left(CompileError.Toolchain(ToolchainMiss.Duplicated(version, CompilerPart.Scala3Library)))
      case (path, found, false) :: Nil if libraryOk(version, found) =>
        legacy match
          case lib :: Nil => Right(List(path, lib))
          case Nil        => Left(CompileError.Toolchain(ToolchainMiss.Missing(version, CompilerPart.Scala3Library)))
          case _          => Left(CompileError.Toolchain(ToolchainMiss.Duplicated(version, CompilerPart.Scala3Library)))
      case (_, found, _) :: Nil =>
        Left(CompileError.Toolchain(ToolchainMiss.Library(version, found)))
      case Nil =>
        Left(CompileError.Toolchain(ToolchainMiss.Missing(version, CompilerPart.Library)))
      case _ =>
        Left(CompileError.Toolchain(ToolchainMiss.Duplicated(version, CompilerPart.Library)))

  /** A merged Scala 3 library reports the requested version. An older compiler still ships a 2.13 library. */
  private def libraryOk(requested: ScalaVersion, found: ScalaVersion): Boolean =
    found == requested || ScalaSeries.of(found).contains(ScalaSeries.Scala2(13))

  private def only(version: ScalaVersion, paths: List[Path], part: CompilerPart): Either[CompileError, Path] =
    paths match
      case path :: Nil => Right(path)
      case Nil         => Left(CompileError.Toolchain(ToolchainMiss.Missing(version, part)))
      case _           => Left(CompileError.Toolchain(ToolchainMiss.Duplicated(version, part)))

  private def inspect(path: Path): Either[Path, Found] =
    open(path)(zip => classify(path, zip)) match
      case Some(found) => Right(found)
      case None        => Left(path)

  private def classify(path: Path, zip: ZipFile): Found =
    val names = zip.entries.asScala.map(_.getName).toSet
    if names.contains(ClassMarker.DottyBridge.entry) then Found.Bridge(path)
    else if names.contains(ClassMarker.DottyDriver.entry) then Found.Compiler(path)
    else if names.exists(_.startsWith("xsbti/")) then Found.Interface(path)
    else if names.contains(ListClass) then
      libraryVersion(zip) match
        case Some(version) => Found.Stdlib(path, version, names.contains(Scala3Runtime))
        case None          => Found.Support(path)
    else if names.contains(Scala3Runtime) then Found.Scala3Lib(path)
    else Found.Support(path)

  private def libraryVersion(zip: ZipFile): Option[ScalaVersion] =
    text(zip, LibraryProperties).flatMap: body =>
      val raw = body.linesIterator.map(_.trim).collectFirst:
        case line if line.startsWith(VersionNumber) => line.drop(VersionNumber.length)
      raw.flatMap(value => ScalaVersion.from(value).toOption)

  private def text(zip: ZipFile, name: String): Option[String] =
    Option(zip.getEntry(name)).map: entry =>
      val stream = zip.getInputStream(entry)
      try new String(stream.readAllBytes, StandardCharsets.UTF_8)
      finally stream.close()

  private def open[A](jar: Path)(use: ZipFile => A): Option[A] =
    try
      val zip = new ZipFile(jar.toFile)
      try Some(use(zip))
      finally zip.close()
    catch case _: ZipException => None

/**
 * The only types a project compiler shares with the engine. Everything else resolves on the
 * platform loader, so the engine's Scala library cannot satisfy a compile.
 */
private final class EngineBoundary(engine: ClassLoader) extends ClassLoader(ClassLoader.getPlatformClassLoader):
  override protected def loadClass(name: String, resolve: Boolean): Class[?] =
    if EngineBoundary.shared(name) then
      val loaded = engine.loadClass(name)
      if resolve then resolveClass(loaded)
      loaded
    else super.loadClass(name, resolve)

  override def getResource(name: String): URL =
    if EngineBoundary.sharedResource(name) then engine.getResource(name)
    else super.getResource(name)

  override def getResources(name: String): Enumeration[URL] =
    if EngineBoundary.sharedResource(name) then engine.getResources(name)
    else super.getResources(name)

private object EngineBoundary:
  def shared(name: String): Boolean = name.startsWith("xsbti.")
  def sharedResource(name: String): Boolean = name.startsWith("xsbti/")
