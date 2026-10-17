package histos.zinc

import histos.jdk.{JdkVersion, Sha256}
import histos.schedule.{Action, ActionId, Input, InputName, InputValue}
import histos.task.TaskKind
import histos.{Platform, ScalaVersion}

import java.net.{URI, URL, URLClassLoader}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.Optional
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import sbt.internal.inc.{IncrementalCompilerImpl, Locate, MappedFileConverter, ScalaInstance, Stamps, ZincUtil}
import sbt.util.Logger
import xsbti.compile.{CompileOrder, IncOptions, PerClasspathEntryLookup}

import zio.{Chunk, IO, ZIO}

/** One scalac flag. The compiler text is produced here, not accepted from the caller. */
enum ScalacFlag:
  case EncodingUtf8

opaque type ScalacFlags = List[ScalacFlag]

object ScalacFlags:
  val empty: ScalacFlags = Nil

  def of(flag: ScalacFlag, flags: ScalacFlag*): ScalacFlags = flag :: flags.toList

  extension (flags: ScalacFlags)
    def canonical: String =
      flags.map:
        case ScalacFlag.EncodingUtf8 => "encoding-utf8"
      .mkString(",")

    private[zinc] def argv: List[String] =
      flags.flatMap:
        case ScalacFlag.EncodingUtf8 => List("-encoding", "UTF-8")

enum ClasspathBan:
  case Scala3Compiler
  case Histos

enum CompileIo:
  case Read, Write, Compile, Bridge

/** A line the compiler produced. It is text because the compiler speaks text. */
opaque type CompileDiagnostic = String

object CompileDiagnostic:
  def fromCompiler(raw: String): CompileDiagnostic = raw
  extension (diagnostic: CompileDiagnostic) def text: String = diagnostic

enum CompileError:
  case UnsupportedPlatform(platform: Platform)
  case UnsupportedScala(version: ScalaVersion)
  case NoSources
  case Banned(path: Path, ban: ClasspathBan)
  case Failed(diagnostics: List[CompileDiagnostic])
  case NoBridge
  case Io(op: CompileIo)

  def message: String = this match
    case UnsupportedPlatform(platform) => s"compile does not support $platform"
    case UnsupportedScala(version)     => s"compile supports ${JvmCompile.Scala390.value}, not ${version.value}"
    case NoSources                     => "compile has no sources"
    case Banned(path, ban)             => s"$path is ${ban.toString.toLowerCase} and is not a user dependency"
    case Failed(diagnostics)           => diagnostics.map(_.text).mkString("\n")
    case NoBridge                      => "the Scala 3 compiler bridge is not on the engine classpath"
    case Io(op)                        => s"compile failed while ${op.toString.toLowerCase}"

final case class ResolvedJdk(home: Path, version: JdkVersion)

final case class CompileToolchain(
    version: ScalaVersion,
    platform: Platform,
    jdk: ResolvedJdk,
    flags: ScalacFlags,
)

final case class CompileRequest(
    toolchain: CompileToolchain,
    sources: List[Path],
    classpath: List[Path],
    classes: Path,
    analysis: Path,
)

final case class CompileOutput(classes: Path, classFiles: Int)

object CompileKey:
  /** Identity of a compile. The classes directory is not part of it. */
  def of(request: CompileRequest, sources: Sha256): Sha256 =
    val tool = request.toolchain
    val text =
      s"""${tool.version.value}
         |${tool.platform}
         |${tool.jdk.version.value}
         |${tool.flags.canonical}
         |${sources.value}
         |""".stripMargin
    Sha256.of(Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)))

  def action(id: ActionId, key: Sha256): Action =
    Action(id, TaskKind.Compile, List(Input(InputName.trusted("compile"), InputValue.Hash(key))), Nil)

object JvmCompile:
  val Scala390: ScalaVersion = ScalaVersion.trusted("3.9.0")

  def sourceDigest(sources: List[Path]): IO[CompileError, Sha256] = hashSources(sources)

  def compile(request: CompileRequest): IO[CompileError, CompileOutput] =
    val tool = request.toolchain
    if tool.platform != Platform.Jvm then ZIO.fail(CompileError.UnsupportedPlatform(tool.platform))
    else if tool.version != Scala390 then ZIO.fail(CompileError.UnsupportedScala(tool.version))
    else
      ZIO.foreachDiscard(request.classpath)(rejectBanned) *>
        (if request.sources.isEmpty then ZIO.fail(CompileError.NoSources)
         else hashSources(request.sources) *> invoke(request))

  private def rejectBanned(path: Path): IO[CompileError, Unit] =
    ban(path).flatMap:
      case Some(reason) => ZIO.fail(CompileError.Banned(path, reason))
      case None         => ZIO.unit

  private def ban(path: Path): IO[CompileError, Option[ClasspathBan]] =
    ZIO.attempt {
      if Files.isDirectory(path) then directoryBan(path)
      else if Files.isRegularFile(path) then jarBan(path)
      else None
    }.mapError(_ => CompileError.Io(CompileIo.Read))

  private def directoryBan(path: Path): Option[ClasspathBan] =
    if Files.isRegularFile(path.resolve("dotty").resolve("tools").resolve("dotc").resolve("Driver.class")) then
      Some(ClasspathBan.Scala3Compiler)
    else if Files.isRegularFile(path.resolve("histos").resolve("HistosBuild.class")) then Some(ClasspathBan.Histos)
    else None

  private def jarBan(path: Path): Option[ClasspathBan] =
    val zip = new ZipFile(path.toFile)
    try
      if zip.getEntry("dotty/tools/dotc/Driver.class") != null then Some(ClasspathBan.Scala3Compiler)
      else if zip.getEntry("histos/HistosBuild.class") != null then Some(ClasspathBan.Histos)
      else None
    finally zip.close()

  private def hashSources(sources: List[Path]): IO[CompileError, Sha256] =
    ZIO.foreach(sources.sortBy(_.toString)): path =>
      ZIO.attempt {
        val body = Files.readAllBytes(path)
        Chunk.fromArray(path.toString.getBytes(StandardCharsets.UTF_8)) ++ Chunk.single(0.toByte) ++ Chunk.fromArray(body)
      }.mapError(_ => CompileError.Io(CompileIo.Read))
    .map(parts => Sha256.of(parts.foldLeft(Chunk.empty[Byte])(_ ++ _)))

  private def invoke(request: CompileRequest): IO[CompileError, CompileOutput] =
    ZIO.attemptBlockingInterrupt(runZinc(request)).mapError(_ => CompileError.Io(CompileIo.Compile)).flatMap(ZIO.fromEither)

  private def runZinc(request: CompileRequest): Either[CompileError, CompileOutput] =
    val libraries = libraryJars
    val compiler  = codeSource(classOf[dotty.tools.dotc.Driver]).toList
    bridgeJar match
      case None                                                                => Left(CompileError.NoBridge)
      case Some(_) if libraries.isEmpty || compiler.isEmpty                    => Left(CompileError.Io(CompileIo.Bridge))
      case Some(bridge)                                                        => compileOne(request, bridge, libraries, compiler)

  private def compileOne(
      request: CompileRequest,
      bridge: Path,
      libraries: List[Path],
      compiler: List[Path],
  ): Either[CompileError, CompileOutput] =
    val libraryLoader = new URLClassLoader(libraries.map(_.toUri.toURL).toArray, ClassLoader.getPlatformClassLoader)
    try
      val instance = new ScalaInstance(
        Scala390.value,
        getClass.getClassLoader,
        getClass.getClassLoader,
        libraryLoader,
        libraries.map(_.toFile).toArray,
        compiler.map(_.toFile).toArray,
        (libraries ++ compiler).map(_.toFile).toArray,
        Some(Scala390.value),
      )
      val zinc       = new IncrementalCompilerImpl
      val options    = xsbti.compile.ClasspathOptionsUtil.noboot(Scala390.value)
      val scalac     = ZincUtil.scalaCompiler(instance, bridge, options)
      val compilers  = zinc.compilers(instance, options, Some(request.toolchain.jdk.home), scalac)
      val converter  = MappedFileConverter.empty
      val reporter   = CollectingReporter()
      val lookup     = new PerClasspathEntryLookup:
        def analysis(entry: xsbti.VirtualFile)     = Optional.empty
        def definesClass(entry: xsbti.VirtualFile) = Locate.definesClass(entry)
      val setup = zinc.setup(
        lookup,
        false,
        request.analysis,
        xsbti.compile.CompilerCache.fresh,
        IncOptions.of(),
        reporter,
        None,
        None,
        Array.empty[xsbti.T2[String, String]],
      )
      Files.createDirectories(request.classes)
      Files.createDirectories(request.analysis)
      val classpath = (request.classes :: libraries ++ request.classpath).map(converter.toVirtualFile).toArray
      val sources   = request.sources.map(converter.toVirtualFile).toArray
      val inputs = zinc.inputs(
        classpath,
        sources,
        request.classes,
        None,
        request.toolchain.flags.argv.toArray,
        Array.empty[String],
        100,
        Array.empty,
        CompileOrder.Mixed,
        compilers,
        setup,
        zinc.emptyPreviousResult,
        Optional.empty,
        converter,
        Stamps.timeWrapBinaryStamps(converter),
      )
      try
        zinc.compile(inputs, Logger.Null)
        if reporter.hasErrors then Left(CompileError.Failed(reporter.diagnostics))
        else Right(CompileOutput(request.classes, countClasses(request.classes)))
      catch case _: xsbti.CompileFailed => Left(CompileError.Failed(reporter.diagnostics))
    finally libraryLoader.close()

  private def countClasses(root: Path): Int =
    if !Files.isDirectory(root) then 0
    else
      val walk = Files.walk(root)
      try walk.iterator.asScala.count(path => Option(path.getFileName).exists(_.toString.endsWith(".class")))
      finally walk.close()

  private def bridgeJar: Option[Path] =
    val loader = getClass.getClassLoader
    loader.getResources("dotty/tools/xsbt/CompilerBridge.class").asScala.toList.flatMap(jarOf).headOption

  private def libraryJars: List[Path] =
    List(classOf[scala.collection.immutable.List[?]], classOf[scala.quoted.Quotes]).flatMap(codeSource).distinct

  private def codeSource(cls: Class[?]): Option[Path] =
    Option(cls.getProtectionDomain).flatMap(domain => Option(domain.getCodeSource)).flatMap: source =>
      Option(source.getLocation).flatMap(jarOf)

  private def jarOf(url: URL): Option[Path] =
    url.getProtocol match
      case "jar" =>
        val spec = url.getPath
        val bang = spec.indexOf('!')
        if bang <= 0 then None
        else
          try Some(Path.of(URI.create(spec.substring(0, bang))))
          catch case _: IllegalArgumentException | _: java.net.URISyntaxException => None
      case "file" =>
        try Some(Path.of(url.toURI))
        catch case _: IllegalArgumentException | _: java.net.URISyntaxException => None
      case _ => None

private final class CollectingReporter extends xsbti.Reporter:
  private var found: List[xsbti.Problem] = Nil

  def diagnostics: List[CompileDiagnostic] =
    found.reverse.map(problem => CompileDiagnostic.fromCompiler(problem.message))

  def reset(): Unit = found = Nil
  def hasErrors: Boolean = found.exists(_.severity == xsbti.Severity.Error)
  def hasWarnings: Boolean = found.exists(_.severity == xsbti.Severity.Warn)
  def printSummary(): Unit = ()
  def problems(): Array[xsbti.Problem] = found.reverse.toArray
  def log(problem: xsbti.Problem): Unit = found = problem :: found
  def comment(position: xsbti.Position, message: String): Unit = ()
end CollectingReporter
