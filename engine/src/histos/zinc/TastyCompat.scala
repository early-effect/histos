package histos.zinc

import histos.workspace.{IoFailure, IoFault}
import histos.{Module, Platform, ProjectName, ScalaVersion}

import java.lang.reflect.Method
import java.net.{URL, URLClassLoader}
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** The three TASTy numbers in a file header, and the three the compiler that will read it publishes. */
private[zinc] object TastyCompat:
  final case class Numbers(major: Int, minor: Int, experimental: Int)

  /** The depender's `tasty-core`. The loader stays open until `close`. */
  final class Reader(
      val numbers: Numbers,
      acceptsFile: Numbers => Either[CompileError, Boolean],
      closeLoader: () => Unit,
  ):
    def accepts(file: Numbers): Either[CompileError, Boolean] =
      acceptsFile(file)

    def close(): Unit =
      closeLoader()
  end Reader

  def open(version: ScalaVersion, jars: List[Path]): Either[CompileError, Reader] =
    val unexpected = CompileError.Toolchain(ToolchainMiss.Unexpected(version, jars))
    if !jars.exists(jar => Option(jar.getFileName).exists(_.toString.startsWith("tasty-core_3"))) then Left(unexpected)
    else
      val loader = new URLClassLoader(jars.map(_.toUri.toURL).toArray, ClassLoader.getPlatformClassLoader)
      try
        val cls    = loader.loadClass("dotty.tools.tasty.TastyFormat$")
        val module = cls.getField("MODULE$").get(null)
        val opened =
          for
            major        <- integer(cls, module, "MajorVersion", unexpected)
            minor        <- integer(cls, module, "MinorVersion", unexpected)
            experimental <- integer(cls, module, "ExperimentalVersion", unexpected)
            method       <- compatible(cls, unexpected)
          yield Reader(
            Numbers(major, minor, experimental),
            file => accepts(method, module, file, Numbers(major, minor, experimental), unexpected),
            () => loader.close(),
          )
        if opened.isLeft then loader.close()
        opened
      catch
        case err: LinkageError =>
          loader.close()
          Left(CompileError.Failed(List(CompileDiagnostic.fromCompiler(err.toString))))
        case _: ReflectiveOperationException | _: java.io.IOException =>
          loader.close()
          Left(unexpected)

  def header(dir: Path): IO[CompileError, Option[Numbers]] =
    ZIO
      .attemptBlocking(first(dir))
      .mapError(_ => CompileError.Io(IoFailure(dir, IoFault.Read)))

  private def accepts(
      method: Method,
      module: Object,
      file: Numbers,
      compiler: Numbers,
      unexpected: CompileError,
  ): Either[CompileError, Boolean] =
    try
      method.invoke(
        module,
        Integer.valueOf(file.major),
        Integer.valueOf(file.minor),
        Integer.valueOf(file.experimental),
        Integer.valueOf(compiler.major),
        Integer.valueOf(compiler.minor),
        Integer.valueOf(compiler.experimental),
      ) match
        case flag: java.lang.Boolean => Right(flag.booleanValue)
        case _                       => Left(unexpected)
    catch case _: ReflectiveOperationException => Left(unexpected)

  private def integer(
      cls: Class[?],
      module: Object,
      name: String,
      unexpected: CompileError,
  ): Either[CompileError, Int] =
    try
      cls.getMethod(name).invoke(module) match
        case value: java.lang.Integer => Right(value.intValue)
        case _                        => Left(unexpected)
    catch case _: ReflectiveOperationException => Left(unexpected)

  private def compatible(cls: Class[?], unexpected: CompileError): Either[CompileError, Method] =
    try
      Right(
        cls.getMethod(
          "isVersionCompatible",
          Integer.TYPE,
          Integer.TYPE,
          Integer.TYPE,
          Integer.TYPE,
          Integer.TYPE,
          Integer.TYPE,
        )
      )
    catch case _: ReflectiveOperationException => Left(unexpected)

  /** The first `.tasty` header in `dir`. A directory with no TASTy file has no answer. */
  private def first(dir: Path): Option[Numbers] =
    if !Files.isDirectory(dir) then None
    else
      val walk = Files.walk(dir)
      try
        walk.iterator.asScala
          .find(path => Option(path.getFileName).exists(_.toString.endsWith(".tasty")))
          .flatMap(read)
      finally walk.close()

  private def read(path: Path): Option[Numbers] =
    val bytes = Files.readAllBytes(path)
    if bytes.length < 7 || !magic(bytes) then None
    else
      nats(bytes, 4, 3) match
        case Some(List(major, minor, experimental)) => Some(Numbers(major, minor, experimental))
        case _                                      => None

  private def magic(bytes: Array[Byte]): Boolean =
    (bytes(0) & 0xff) == 0x5c && (bytes(1) & 0xff) == 0xa1 && (bytes(2) & 0xff) == 0xab && (bytes(3) & 0xff) == 0x1f

  private def nats(bytes: Array[Byte], at: Int, count: Int): Option[List[Int]] =
    if count == 0 then Some(Nil)
    else
      nat(bytes, at, 0).flatMap: (value, next) =>
        nats(bytes, next, count - 1).map(value :: _)

  /** Big-endian base 128. Every byte but the last has the high bit unset. */
  private def nat(bytes: Array[Byte], at: Int, acc: Int): Option[(Int, Int)] =
    if at >= bytes.length then None
    else
      val piece = bytes(at) & 0xff
      val next  = (acc << 7) | (piece & 0x7f)
      if (piece & 0x80) != 0 then Some((next, at + 1))
      else nat(bytes, at + 1, next)
end TastyCompat

/** Which declared cell a depender waits for. An exact cell wins. */
private[histos] object UpstreamCells:
  private type Cell = (ProjectName, ScalaVersion, Platform)

  def isScala3(version: ScalaVersion): Boolean =
    version.value.startsWith("3.")

  def is213(version: ScalaVersion): Boolean =
    version.value.startsWith("2.13.")

  def rank(version: ScalaVersion): (Int, Int, Int) =
    val head                = version.value.takeWhile(ch => ch.isDigit || ch == '.')
    val parts               = head.split('.')
    def at(index: Int): Int =
      if index >= parts.length then 0
      else parts(index).toIntOption.getOrElse(0)
    (at(0), at(1), at(2))

  def required(
      depender: ScalaVersion,
      upstream: Module,
      platform: Platform,
      declared: Set[Cell],
  ): List[Cell] =
    val cells = upstream.versions.all.flatMap: item =>
      val key = (upstream.name, item, platform)
      if declared.contains(key) then List(key) else Nil
    val exact = (upstream.name, depender, platform)
    if declared.contains(exact) then List(exact)
    else if is213(depender) then
      cells.filter((_, item, _) => is213(item)).sortBy((_, item, _) => rank(item)).takeRight(1)
    else if isScala3(depender) then cells.filter((_, item, _) => isScala3(item))
    else Nil

  /** The cell compile would put on the classpath. Exact wins. Otherwise the newest declared version this compiler can
    * read: same series, not a newer minor.
    */
  def chosen(
      depender: ScalaVersion,
      upstream: Module,
      platform: Platform,
      modules: List[Module],
  ): Option[ScalaVersion] =
    val declared = modules.flatMap(item => item.versions.all.map(version => (item.name, version, platform))).toSet
    val eligible =
      required(depender, upstream, platform, declared).filter((_, item, _) => atMost(item, depender))
    eligible.sortBy((_, item, _) => rank(item)).lastOption.map((_, version, _) => version)

  private def atMost(version: ScalaVersion, limit: ScalaVersion): Boolean =
    val (major, minor, patch)                = rank(version)
    val (limitMajor, limitMinor, limitPatch) = rank(limit)
    major < limitMajor || (major == limitMajor && (minor < limitMinor || (minor == limitMinor && patch <= limitPatch)))
end UpstreamCells
