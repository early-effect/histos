package histos.zinc

import histos.ScalaVersion
import histos.resolve.LockedTools

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.zip.{ZipException, ZipFile}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** A Scala 2.13 compiler, fetched for the patch a module names. Scala 2.12 and older are not this compiler.
  */
object Scala213:

  /** Fetched off the engine classpath. The engine's Scala library is a Scala 3 library, and a 2.13 compiler on that
    * classpath breaks Zinc.
    *
    * `runtime` is the only classpath a compiled program may see. `compilerClasspath` is the compiler and its
    * dependencies, never that program.
    */
  private[zinc] object Toolchain:
    private[Scala213] def load(
        library: Path,
        compiler: Path,
        reflect: Path,
        bridge: Path,
        support: List[Path],
    ): Toolchain =
      new Toolchain(library, compiler, reflect, bridge, support)

  private[zinc] final class Toolchain private (
      private[zinc] val library: Path,
      private[zinc] val compiler: Path,
      private val reflect: Path,
      private val bridge: Path,
      private val support: List[Path],
  ):
    private[zinc] def bridgeJar: Path               = bridge
    private[zinc] def runtime: List[Path]           = List(library)
    private[zinc] def compilerClasspath: List[Path] =
      compiler :: reflect :: support
    private[zinc] def openLoader(): URLClassLoader =
      new URLClassLoader(
        (compiler :: reflect :: library :: support).map(_.toUri.toURL).toArray,
        ClassLoader.getPlatformClassLoader,
      )

  private[zinc] def jars(
      scalaVersion: ScalaVersion
  ): ZIO[LockedTools, CompileError, Toolchain] =
    ZIO
      .serviceWithZIO[LockedTools](_.compiler(scalaVersion))
      .mapError(CompileError.Resolved(_))
      .flatMap: classpath =>
        ZIO
          .attemptBlocking(
            assemble(scalaVersion, classpath.artifacts.map(_.path))
          )
          .mapError(_ =>
            CompileError.Toolchain(
              ToolchainMiss
                .Unexpected(scalaVersion, classpath.artifacts.map(_.path))
            )
          )
          .flatMap(ZIO.fromEither)

  private val LibraryProperties = "library.properties"
  private val VersionNumber     = "version.number="

  private enum Role:
    case Library, Compiler, Reflect, Bridge, JLine, Diff

  private enum Inspected:
    case Jar(path: Path, role: Role)
    case Foreign(path: Path)
    case NotJar

  private enum Support:
    case JLine, Diff

    def prefix: String = this match
      case JLine => "org/jline/"
      case Diff  => "com/github/difflib/"

    def role: Role = this match
      case JLine => Role.JLine
      case Diff  => Role.Diff
  end Support

  private def assemble(
      scalaVersion: ScalaVersion,
      paths: List[Path],
  ): Either[CompileError, Toolchain] =
    val inspected = paths.map(path => inspect(scalaVersion, path))
    inspected.collect { case Inspected.Foreign(path) => path } match
      case Nil =>
        val known = inspected.collect { case Inspected.Jar(path, role) =>
          role -> path
        }
        for
          library <- one(
            scalaVersion,
            known,
            CompilerPart.Library,
            Role.Library,
          )
          compiler <- one(
            scalaVersion,
            known,
            CompilerPart.Compiler,
            Role.Compiler,
          )
          reflect <- one(
            scalaVersion,
            known,
            CompilerPart.Reflect,
            Role.Reflect,
          )
          bridge <- one(scalaVersion, known, CompilerPart.Bridge, Role.Bridge)
        yield
          val support = known.collect:
            case (Role.JLine, path) => path
            case (Role.Diff, path)  => path
          Toolchain.load(library, compiler, reflect, bridge, support)
      case foreign =>
        Left(
          CompileError.Toolchain(
            ToolchainMiss.Unexpected(scalaVersion, foreign)
          )
        )

  private def one(
      scalaVersion: ScalaVersion,
      known: List[(Role, Path)],
      part: CompilerPart,
      role: Role,
  ): Either[CompileError, Path] =
    known.collect { case (`role`, path) => path } match
      case path :: Nil => Right(path)
      case Nil         =>
        Left(CompileError.Toolchain(ToolchainMiss.Missing(scalaVersion, part)))
      case _ =>
        Left(
          CompileError.Toolchain(ToolchainMiss.Duplicated(scalaVersion, part))
        )

  private def inspect(scalaVersion: ScalaVersion, path: Path): Inspected =
    open(path): zip =>
      roleOf(scalaVersion, zip) match
        case Some(role) => Inspected.Jar(path, role)
        case None       => Inspected.Foreign(path)
    .getOrElse(Inspected.NotJar)

  private def roleOf(scalaVersion: ScalaVersion, zip: ZipFile): Option[Role] =
    if ClassMarker.XsbtBridge.present(zip) then Some(Role.Bridge)
    else if ClassMarker.NscGlobal.present(zip) then Some(Role.Compiler)
    else if ClassMarker.JavaUniverse.present(zip) then Some(Role.Reflect)
    else if libraryVersion(zip).contains(scalaVersion) then Some(Role.Library)
    else
      Support.values.collectFirst {
        case support if hasClass(zip, support.prefix) => support.role
      }

  private def hasClass(zip: ZipFile, prefix: String): Boolean =
    zip.entries.asScala.exists(entry => entry.getName.startsWith(prefix) && entry.getName.endsWith(".class"))

  private def libraryVersion(zip: ZipFile): Option[ScalaVersion] =
    text(zip, LibraryProperties).flatMap: body =>
      val raw = body.linesIterator
        .map(_.trim)
        .collectFirst:
          case line if line.startsWith(VersionNumber) =>
            line.drop(VersionNumber.length)
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
end Scala213
