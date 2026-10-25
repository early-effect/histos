package histos.zinc

import histos.jdk.JdkRuntime
import histos.jdk.Sha256
import histos.workspace.{IoFailure, IoFault}
import histos.{Platform, ScalaVersion}
import histos.resolve.{LockedTools, ResolveError}

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.Optional
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.control.NonFatal

import sbt.internal.inc.{
  FileAnalysisStore,
  IncrementalCompilerImpl,
  Locate,
  MappedFileConverter,
  Stamps
}
import sbt.util.Logger
import xsbti.compile.{
  CompileOrder,
  IncOptions,
  PerClasspathEntryLookup,
  PreviousResult
}

import zio.{Chunk, IO, ZIO}

export histos.ScalacFlag
export histos.ScalacFlags

enum ClasspathBan:
  case Scala3Compiler
  case Scala2Compiler
  case Histos

  def label: String = this match
    case Scala3Compiler => "the Scala 3 compiler"
    case Scala2Compiler => "the Scala 2 compiler"
    case Histos         => "histos"

/** A line the compiler produced. It is text because the compiler speaks text.
  */
opaque type CompileDiagnostic = String

object CompileDiagnostic:
  def fromCompiler(raw: String): CompileDiagnostic = raw
  extension (diagnostic: CompileDiagnostic) def text: String = diagnostic

/** A jar the Scala 2.13 toolchain must contain exactly once. */
enum CompilerPart:
  case Library, Compiler, Reflect, Bridge, Scala3Compiler, Scala3Bridge,
    Scala3Library

  def label: String = this match
    case Library        => "scala-library"
    case Compiler       => "scala-compiler"
    case Reflect        => "scala-reflect"
    case Bridge         => "scala2-sbt-bridge"
    case Scala3Compiler => "scala3-compiler"
    case Scala3Bridge   => "scala3-sbt-bridge"
    case Scala3Library  => "scala3-library"

/** Why the compiler a module named could not be loaded. */
enum ToolchainMiss:
  case Fetch(version: ScalaVersion)
  case Missing(version: ScalaVersion, part: CompilerPart)
  case Duplicated(version: ScalaVersion, part: CompilerPart)
  case Unexpected(version: ScalaVersion, jars: List[Path])
  case Library(requested: ScalaVersion, found: ScalaVersion)
  case Plugin(version: ScalaVersion)

  def message: String = this match
    case Fetch(version) => s"the ${version.value} compiler could not be fetched"
    case Plugin(version) =>
      s"semanticdb-scalac for ${version.value} could not be fetched"
    case Missing(version, part) =>
      s"the ${version.value} fetch has no ${part.label}"
    case Duplicated(version, part) =>
      s"the ${version.value} fetch has more than one ${part.label}"
    case Unexpected(version, jars) =>
      val names = jars.map(path =>
        Option(path.getFileName).map(_.toString).getOrElse(path.toString)
      )
      s"the ${version.value} fetch included ${names.mkString(", ")}"
    case Library(requested, found) =>
      s"the ${requested.value} fetch brought Scala library ${found.value}"

enum CompileError:
  case Resolved(error: ResolveError)
  case UnsupportedPlatform(platform: Platform)
  case UnsupportedScala(version: ScalaVersion)
  case NoSources
  case Banned(path: Path, ban: ClasspathBan)
  case Failed(diagnostics: List[CompileDiagnostic])
  case Toolchain(miss: ToolchainMiss)
  case Io(failure: histos.workspace.IoFailure)
  case JsLibrary(miss: JsLibraryMiss)
  case NativeFetch(error: histos.native.NativeFetch)
  case NativeOrigin(error: histos.native.NativeToolchainError)
  case NativeLibrary(text: String)

  def message: String = this match
    case Resolved(error)               => error.message
    case UnsupportedPlatform(platform) => s"compile does not support $platform"
    case UnsupportedScala(version)     =>
      s"compile supports Scala 3 and Scala 2.13, not ${version.value}"
    case NoSources         => "compile has no sources"
    case Banned(path, ban) =>
      s"$path is ${ban.label} and is not a user dependency"
    case Failed(diagnostics) => diagnostics.map(_.text).mkString("\n")
    case Toolchain(miss)     => miss.message
    case Io(failure)         => failure.message
    case JsLibrary(miss)     => miss.message
    case NativeFetch(error)  => error.message
    case NativeOrigin(error) => error.message
    case NativeLibrary(text) => text

/** Why the Scala.js standard library did not arrive. The compiler's `-scalajs`
  * backend needs it instead of `scala-library`.
  */
enum JsLibraryMiss:
  case Unavailable
  case Empty

  def message: String = this match
    case Unavailable => "the Scala.js library could not be fetched"
    case Empty       => "the Scala.js library resolve was empty"

final case class CompileToolchain(
    version: ScalaVersion,
    platform: Platform,
    jdk: JdkRuntime,
    flags: ScalacFlags
)

final case class CompileRequest(
    toolchain: CompileToolchain,
    sources: List[Path],
    classpath: List[Path],
    classes: Path,
    analysis: Path,
    artifactHashes: List[Sha256],
    compilationTools: List[histos.resolve.ToolInput] = Nil,
    compilerPlugins: List[histos.resolve.ToolInput] = Nil
)

final case class CompileOutput(
    classes: Path,
    classFiles: Int,
    key: Sha256,
    changed: Boolean
)

/** Zinc positions are 1-based. Dotty positions are already 0-based. */
enum PositionBase:
  case Zinc
  case Dotty

enum CompileSeverity:
  case Error, Warn, Info

/** One compiler message. A missing path is a log line, not a squiggle. */
final case class CompileProblem(
    message: String,
    path: Option[Path],
    severity: CompileSeverity,
    startLine: Option[Int],
    startColumn: Option[Int],
    endLine: Option[Int],
    endColumn: Option[Int],
    base: PositionBase
):
  def text: String = message

object CompileProblem:
  def text(message: String): CompileProblem =
    CompileProblem(
      message,
      None,
      CompileSeverity.Error,
      None,
      None,
      None,
      None,
      PositionBase.Zinc
    )

  def fromZinc(problem: xsbti.Problem): CompileProblem =
    val position = problem.position
    CompileProblem(
      problem.message,
      position.sourcePath.toScala.map(Path.of(_)),
      severity(problem.severity),
      opt(position.startLine),
      opt(position.startColumn),
      opt(position.endLine),
      opt(position.endColumn),
      PositionBase.Zinc
    )

  private def severity(level: xsbti.Severity): CompileSeverity =
    level match
      case xsbti.Severity.Error => CompileSeverity.Error
      case xsbti.Severity.Warn  => CompileSeverity.Warn
      case xsbti.Severity.Info  => CompileSeverity.Info

  private def opt(value: java.util.Optional[Integer]): Option[Int] =
    value.toScala.map(_.intValue)

/** SemanticDB flags beside `ScalacFlags.argv`. They are not flag cases:
  * `-sourceroot` carries a path.
  */
object SemanticDb:
  def plugin(version: ScalaVersion): ZIO[LockedTools, CompileError, Path] =
    ZIO.serviceWithZIO[LockedTools]: tools =>
      val request =
        histos.zinc.CompilerRequests.semanticdb(
          version,
          tools.catalog,
          tools.repositories
        )
      tools
        .materialize(request)
        .mapError(CompileError.Resolved(_))
        .flatMap: classpath =>
          classpath.artifacts.find(artifact =>
            request.roots.exists(root =>
              root.organization == artifact.organization && root.name == artifact.name
            )
          ) match
            case Some(artifact) => ZIO.succeed(artifact.path)
            case None           =>
              ZIO.fail(CompileError.Toolchain(ToolchainMiss.Plugin(version)))

  def argv(repo: Path, version: ScalaVersion): List[String] =
    val root = repo.toAbsolutePath.normalize.toString
    ScalaSeries.of(version) match
      case Some(ScalaSeries.Scala3) => List("-Xsemanticdb", "-sourceroot", root)
      case Some(ScalaSeries.Scala2(_)) =>
        List("-Yrangepos", s"-P:semanticdb:sourceroot:$root")
      case None => Nil

object CompileKey:
  def withTools(key: Sha256, hashes: List[Sha256]): Sha256 =
    Sha256.of(
      Chunk.fromArray(
        (key.value + "\n" + hashes.map(_.value).mkString("\n"))
          .getBytes(StandardCharsets.UTF_8)
      )
    )

  /** Identity of a compile. The classes directory is not part of it. Artifact
    * checksums are.
    */
  def of(request: CompileRequest, sources: Sha256): Sha256 =
    val tool = request.toolchain
    val text =
      s"""${tool.version.value}
         |${tool.platform}
         |${tool.jdk.version.value}
         |${histos.toolchain.Runtimes.identityToken(tool.jdk.identity)}
         |${tool.flags.canonical}
         |${sources.value}
         |${request.artifactHashes.map(_.value).mkString("\n")}
         |${request.compilationTools.map(_.fingerprint.value).mkString("\n")}
         |""".stripMargin
    Sha256.of(Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)))

object JvmCompile:

  def sourceDigest(sources: List[Path]): IO[CompileError, Sha256] = hashSources(
    sources
  )

  def compile(
      request: CompileRequest,
      extra: List[String] = Nil,
      hear: CompileProblem => Unit = _ => ()
  ): ZIO[LockedTools, CompileError, CompileOutput] =
    val tool = request.toolchain
    if tool.platform != Platform.Jvm then
      ZIO.fail(CompileError.UnsupportedPlatform(tool.platform))
    else if !supported(tool.version) then
      ZIO.fail(CompileError.UnsupportedScala(tool.version))
    else compilePrepared(request, Nil, extra, hear)

  /** Uses the batch's compiler pool when a compile is already inside one. */
  private[zinc] def compilePooled(
      pool: CompilerPool,
      request: CompileRequest,
      extra: List[String],
      hear: CompileProblem => Unit
  ): ZIO[LockedTools, CompileError, CompileOutput] =
    val tool = request.toolchain
    if tool.platform != Platform.Jvm then
      ZIO.fail(CompileError.UnsupportedPlatform(tool.platform))
    else if !supported(tool.version) then
      ZIO.fail(CompileError.UnsupportedScala(tool.version))
    else if request.sources.isEmpty then ZIO.fail(CompileError.NoSources)
    else
      ZIO.foreachDiscard(request.classpath)(rejectBanned) *>
        hashSources(request.sources).flatMap: digest =>
          invoke(
            request,
            extra,
            hear,
            None,
            CompileKey.of(request, digest),
            Some(pool)
          )

  /** Compile with the Scala compiler the request names. Plugins are not user
    * dependencies. `hear` is called on the compiler thread for each diagnostic.
    */
  private[histos] def compilePrepared(
      request: CompileRequest,
      plugins: List[Path],
      extra: List[String] = Nil,
      hear: CompileProblem => Unit = _ => ()
  ): ZIO[LockedTools, CompileError, CompileOutput] =
    ZIO.foreachDiscard(request.classpath)(rejectBanned) *>
      (if request.sources.isEmpty then ZIO.fail(CompileError.NoSources)
       else
         hashSources(request.sources).flatMap: digest =>
           val key = CompileKey.of(request, digest)
           invoke(request, plugins.map(pluginArg) ++ extra, hear, key = key))

  private def pluginArg(plugin: Path): String =
    s"-Xplugin:${plugin.toAbsolutePath}"

  private def rejectBanned(path: Path): IO[CompileError, Unit] =
    ban(path).flatMap:
      case Some(reason) => ZIO.fail(CompileError.Banned(path, reason))
      case None         => ZIO.unit

  private def ban(path: Path): IO[CompileError, Option[ClasspathBan]] =
    ZIO
      .attemptBlocking {
        if Files.isDirectory(path) then directoryBan(path)
        else if Files.isRegularFile(path) then jarBan(path)
        else None
      }
      .mapError(_ => CompileError.Io(IoFailure(path, IoFault.Stat)))

  /** A classes directory is a `dependsOn` output. It may contain the build
    * trait. A compiler jar is a declared library.
    */
  private def directoryBan(path: Path): Option[ClasspathBan] =
    ClassMarker.userBans
      .collectFirst:
        case marker
            if marker != ClassMarker.HistosBuild && Files.isRegularFile(
              marker.under(path)
            ) =>
          marker.classpathBan
      .flatten

  /** The packaged build API is not a module dependency. `histos.scala` is
    * compiled against it on its own.
    */
  private def jarBan(path: Path): Option[ClasspathBan] =
    val zip = new ZipFile(path.toFile)
    try
      if ClassMarker.HistosBuild.present(zip) || ClassMarker.PluginEntry
          .present(zip)
      then Some(ClasspathBan.Histos)
      else None
    finally zip.close()

  private def hashSources(sources: List[Path]): IO[CompileError, Sha256] =
    ZIO
      .foreach(sources.sortBy(_.toString)): path =>
        ZIO
          .attemptBlocking {
            val body = Files.readAllBytes(path)
            Chunk.fromArray(
              path.toString.getBytes(StandardCharsets.UTF_8)
            ) ++ Chunk.single(0.toByte) ++ Chunk.fromArray(body)
          }
          .mapError(_ => CompileError.Io(IoFailure(path, IoFault.Read)))
      .map(parts => Sha256.of(parts.foldLeft(Chunk.empty[Byte])(_ ++ _)))

  /** Compiler jars plus the library they linked against. BSP names this Scala
    * version with these jars.
    */
  private[histos] def scalaJars(
      version: ScalaVersion
  ): ZIO[LockedTools, CompileError, List[Path]] =
    ScalaSeries.of(version) match
      case Some(ScalaSeries.Scala3) =>
        Scala3
          .jars(version)
          .map(toolchain =>
            (toolchain.compilerClasspath ++ toolchain.runtime).distinct
          )
      case Some(ScalaSeries.Scala2(13)) =>
        Scala213
          .jars(version)
          .map(toolchain =>
            (toolchain.compilerClasspath ++ toolchain.runtime).distinct
          )
      case _ =>
        ZIO.fail(CompileError.UnsupportedScala(version))

  /** The standard library the compiler linked against. A test run puts it ahead
    * of locked jars.
    */
  private[histos] def runtime(
      version: ScalaVersion
  ): ZIO[LockedTools, CompileError, List[Path]] =
    ScalaSeries.of(version) match
      case Some(ScalaSeries.Scala3)     => Scala3.jars(version).map(_.runtime)
      case Some(ScalaSeries.Scala2(13)) =>
        Scala213.jars(version).map(jars => List(jars.library))
      case _ => ZIO.fail(CompileError.UnsupportedScala(version))

  /** A locked `scala-library` or `scala3-library` is a second copy. The
    * compiler already puts `runtime` on the classpath, and two versions make
    * `scala.caps` both an object and a package.
    */
  private[histos] def isStandardLibrary(path: Path): Boolean =
    Option(path.getFileName)
      .map(_.toString)
      .exists(name =>
        name.startsWith("scala-library-") || name.startsWith("scala3-library_")
      )

  /** Scala 3, any patch. Scala 2.13, any patch. Nothing older. */
  private def supported(version: ScalaVersion): Boolean =
    ScalaSeries.of(version) match
      case Some(ScalaSeries.Scala3)     => true
      case Some(ScalaSeries.Scala2(13)) => true
      case _                            => false

  /** Compile with `stdlib` on the user classpath instead of the JVM standard
    * library. The compiler loader is unchanged.
    */
  private[histos] def compileWith(
      request: CompileRequest,
      stdlib: List[Path],
      plugins: List[Path],
      extra: List[String],
      hear: CompileProblem => Unit = _ => ()
  ): ZIO[LockedTools, CompileError, CompileOutput] =
    if !supported(request.toolchain.version) then
      ZIO.fail(CompileError.UnsupportedScala(request.toolchain.version))
    else
      ZIO.foreachDiscard(request.classpath ++ plugins)(rejectBanned) *>
        (if request.sources.isEmpty then ZIO.fail(CompileError.NoSources)
         else
           hashSources(request.sources).flatMap: digest =>
             val key = CompileKey.of(request, digest)
             invoke(
               request,
               plugins.map(pluginArg) ++ extra,
               hear,
               Some(stdlib),
               key
             ))

  private def invoke(
      request: CompileRequest,
      extra: List[String],
      hear: CompileProblem => Unit,
      stdlib: Option[List[Path]] = None,
      key: Sha256,
      pool: Option[CompilerPool] = None
  ): ZIO[LockedTools, CompileError, CompileOutput] =
    pool match
      case Some(found) => drive(found, request, extra, hear, stdlib, key)
      case None        =>
        ZIO.scoped(
          CompilerPool.make.flatMap(drive(_, request, extra, hear, stdlib, key))
        )

  private def drive(
      pool: CompilerPool,
      request: CompileRequest,
      extra: List[String],
      hear: CompileProblem => Unit,
      stdlib: Option[List[Path]],
      key: Sha256
  ): ZIO[LockedTools, CompileError, CompileOutput] =
    val version = request.toolchain.version
    ZIO.serviceWithZIO[LockedTools]: tools =>
      for
        compiler <- tools
          .selected(
            histos.zinc.CompilerRequests.compiler(version, tools.repositories)
          )
          .mapError(CompileError.Resolved(_))
        output <- ScalaSeries.of(version) match
          case Some(ScalaSeries.Scala3) =>
            Scala3
              .jars(version)
              .flatMap: toolchain =>
                val inputs =
                  compiler :: request.compilerPlugins ++ request.compilationTools
                pool.compile(
                  request,
                  CompilerArtifacts(
                    stdlib.getOrElse(toolchain.runtime),
                    toolchain.compilerClasspath,
                    toolchain.bridgeJar,
                    inputs
                  ),
                  extra,
                  hear,
                  CompileKey.withTools(
                    key,
                    (compiler :: request.compilerPlugins).map(_.fingerprint)
                  )
                )
          case Some(ScalaSeries.Scala2(13)) =>
            for
              jars <- Scala213.jars(version)
              semantic <- tools
                .selected(
                  histos.zinc.CompilerRequests
                    .semanticdb(version, tools.catalog, tools.repositories)
                )
                .mapError(CompileError.Resolved(_))
              plugin <- SemanticDb.plugin(version)
              inputs = List(
                compiler,
                semantic
              ) ++ request.compilerPlugins ++ request.compilationTools
              compiled <- pool.compile(
                request,
                CompilerArtifacts(
                  jars.runtime,
                  jars.compilerClasspath,
                  jars.bridgeJar,
                  inputs
                ),
                s"-Xplugin:${plugin.toAbsolutePath.normalize}" :: extra,
                hear,
                CompileKey.withTools(
                  key,
                  (List(compiler, semantic) ++ request.compilerPlugins)
                    .map(_.fingerprint)
                )
              )
            yield compiled
          case _ => ZIO.fail(CompileError.UnsupportedScala(version))
      yield output

  /** One compile against a pooled compiler. The analysis file is the previous
    * result and the next one.
    */
  private[zinc] def once(
      request: CompileRequest,
      prepared: Prepared,
      extra: List[String],
      hear: CompileProblem => Unit,
      key: Sha256
  ): Either[CompileError, CompileOutput] =
    val zinc = new IncrementalCompilerImpl
    val converter = MappedFileConverter.empty
    val reporter = CollectingReporter(hear)
    val lookup = new PerClasspathEntryLookup:
      def analysis(entry: xsbti.VirtualFile) = Optional.empty
      def definesClass(entry: xsbti.VirtualFile) = Locate.definesClass(entry)
    Files.createDirectories(request.classes)
    Files.createDirectories(request.analysis)
    val setup = zinc.setup(
      lookup,
      false,
      request.analysis,
      prepared.cache,
      IncOptions.of(),
      reporter,
      None,
      None,
      Array(new xsbti.T2[String, String]:
        def get1(): String = "histos.toolchain"
        def get2(): String =
          prepared.toolFingerprint + "\n" + request.compilationTools
            .map(_.fingerprint.value)
            .mkString("\n"))
    )
    val classpath = (request.classes :: prepared.runtime ++ request.classpath)
      .map(converter.toVirtualFile)
      .toArray
    val sources = request.sources.map(converter.toVirtualFile).toArray
    val inputs = zinc.inputs(
      classpath,
      sources,
      request.classes,
      None,
      (request.toolchain.flags.argv ++ extra).toArray,
      Array.empty[String],
      100,
      Array.empty,
      CompileOrder.Mixed,
      prepared.compilers,
      setup,
      remembered(zinc, request.analysis),
      Optional.empty,
      converter,
      Stamps.timeWrapBinaryStamps(converter)
    )
    try
      val result = zinc.compile(inputs, Logger.Null)
      FileAnalysisStore
        .binary(request.analysis.resolve("inc.zip").toFile)
        .set(result)
      if reporter.hasErrors then Left(CompileError.Failed(reporter.diagnostics))
      else
        Right(
          CompileOutput(
            request.classes,
            countClasses(request.classes),
            key,
            result.hasModified
          )
        )
    catch
      case _: xsbti.CompileFailed =>
        Left(CompileError.Failed(reporter.diagnostics))

  private def remembered(
      zinc: IncrementalCompilerImpl,
      dir: Path
  ): PreviousResult =
    val got = FileAnalysisStore.binary(dir.resolve("inc.zip").toFile).get()
    got.toScala.fold(zinc.emptyPreviousResult)(zinc.previousResult)

  private def countClasses(root: Path): Int =
    if !Files.isDirectory(root) then 0
    else
      val walk = Files.walk(root)
      try
        walk.iterator.asScala.count: path =>
          Option(path.getFileName).map(_.toString) match
            case Some(name) =>
              name.endsWith(".class") || name.endsWith(".sjsir") || name
                .endsWith(".nir")
            case None => false
      finally walk.close()

private final class CollectingReporter(hear: CompileProblem => Unit)
    extends xsbti.Reporter:
  private val found =
    new java.util.concurrent.ConcurrentLinkedQueue[xsbti.Problem]

  def diagnostics: List[CompileDiagnostic] =
    found.iterator.asScala.toList.map(problem =>
      CompileDiagnostic.fromCompiler(problem.message)
    )

  def reset(): Unit = found.clear()
  def hasErrors: Boolean =
    found.iterator.asScala.exists(_.severity == xsbti.Severity.Error)
  def hasWarnings: Boolean =
    found.iterator.asScala.exists(_.severity == xsbti.Severity.Warn)
  def printSummary(): Unit = ()
  def problems(): Array[xsbti.Problem] = found.iterator.asScala.toArray
  def log(problem: xsbti.Problem): Unit =
    found.add(problem)
    hear(CompileProblem.fromZinc(problem))
  def comment(position: xsbti.Position, message: String): Unit = ()
end CollectingReporter
