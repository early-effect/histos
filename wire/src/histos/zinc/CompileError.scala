package histos.zinc

import histos.jdk.Sha256
import histos.native.{NativeFetch, NativeToolchainError}
import histos.resolve.ResolveError
import histos.schedule.CacheError
import histos.workspace.IoFailure
import histos.{Platform, ScalaVersion}

import java.nio.file.Path

enum ClasspathBan:
  case Scala3Compiler
  case Scala2Compiler
  case Histos

  def label: String = this match
    case Scala3Compiler => "the Scala 3 compiler"
    case Scala2Compiler => "the Scala 2 compiler"
    case Histos         => "histos"
end ClasspathBan

/** A line the compiler produced. It is text because the compiler speaks text.
  */
opaque type CompileDiagnostic = String

object CompileDiagnostic:
  def fromCompiler(raw: String): CompileDiagnostic           = raw
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
end CompilerPart

/** Why the compiler a module named could not be loaded. */
enum ToolchainMiss:
  case Fetch(version: ScalaVersion)
  case Missing(version: ScalaVersion, part: CompilerPart)
  case Duplicated(version: ScalaVersion, part: CompilerPart)
  case Unexpected(version: ScalaVersion, jars: List[Path])
  case Library(requested: ScalaVersion, found: ScalaVersion)
  case Plugin(version: ScalaVersion)
  case Coverage(version: ScalaVersion)

  def message: String = this match
    case Fetch(version)  => s"the ${version.value} compiler could not be fetched"
    case Plugin(version) =>
      s"semanticdb-scalac for ${version.value} could not be fetched"
    case Coverage(version) =>
      s"scoverage for ${version.value} could not be selected"
    case Missing(version, part) =>
      s"the ${version.value} fetch has no ${part.label}"
    case Duplicated(version, part) =>
      s"the ${version.value} fetch has more than one ${part.label}"
    case Unexpected(version, jars) =>
      val names = jars.map(path => Option(path.getFileName).map(_.toString).getOrElse(path.toString))
      s"the ${version.value} fetch included ${names.mkString(", ")}"
    case Library(requested, found) =>
      s"the ${requested.value} fetch brought Scala library ${found.value}"
end ToolchainMiss

enum CompileError:
  case Resolved(error: ResolveError)
  case UnsupportedPlatform(platform: Platform)
  case UnsupportedScala(version: ScalaVersion)
  case NoSources

  /** `source` is not inside `checkout`, so it cannot take a repo-relative row. */
  case Outside(source: Path, checkout: Path)
  case Banned(path: Path, ban: ClasspathBan)
  case Failed(diagnostics: List[CompileDiagnostic])
  case Toolchain(miss: ToolchainMiss)
  case Io(failure: histos.workspace.IoFailure)
  case JsLibrary(miss: JsLibraryMiss)
  case NativeFetch(error: histos.native.NativeFetch)
  case NativeOrigin(error: histos.native.NativeToolchainError)
  case NativeLibrary(text: String)
  case Stored(error: CacheError)

  /** A Native source produced class files and no NIR beside them. The store does not keep that product. */
  case NoNir(source: Path)

  def message: String = this match
    case Resolved(error)               => error.message
    case UnsupportedPlatform(platform) => s"compile does not support $platform"
    case UnsupportedScala(version)     =>
      s"compile supports Scala 3 and Scala 2.13, not ${version.value}"
    case NoSources                 => "compile has no sources"
    case Outside(source, checkout) => s"$source is outside $checkout"
    case Banned(path, ban)         =>
      s"$path is ${ban.label} and is not a user dependency"
    case Failed(diagnostics) => diagnostics.map(_.text).mkString("\n")
    case Toolchain(miss)     => miss.message
    case Io(failure)         => failure.message
    case JsLibrary(miss)     => miss.message
    case NativeFetch(error)  => error.message
    case NativeOrigin(error) => error.message
    case NativeLibrary(text) => text
    case Stored(error)       => error.message
    case NoNir(source)       => s"$source produced no NIR"
end CompileError

/** Why the Scala.js standard library did not arrive. The compiler's `-scalajs` backend needs it instead of
  * `scala-library`.
  */
enum JsLibraryMiss:
  case Unavailable
  case Empty

  def message: String = this match
    case Unavailable => "the Scala.js library could not be fetched"
    case Empty       => "the Scala.js library resolve was empty"
end JsLibraryMiss
