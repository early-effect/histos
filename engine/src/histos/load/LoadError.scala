package histos.load

import histos.BuildError

import java.nio.file.Path

/** Which load step threw. The exception text is not the error. */
enum LoadIo:
  case Stat, Temp, Write, Compile, Classpath, OpenLoader, Walk, LoadClass

/** A line the Scala 3 compiler produced. It is text because the compiler speaks text. */
opaque type CompilerMessage = String

object CompilerMessage:
  def fromCompiler(raw: String): CompilerMessage = raw
  extension (message: CompilerMessage) def text: String = message

/** A JVM binary name, as `Class.getName` reports it. */
opaque type BinaryName = String

object BinaryName:
  def fromClass(cls: Class[?]): BinaryName = cls.getName
  extension (name: BinaryName) def value: String = name

enum LoadError:
  case MissingFile(path: Path)
  case CompileFailed(diagnostics: List[CompilerMessage])
  case NoEntry
  case AmbiguousEntry(classes: List[BinaryName])
  case BrokenGraph(error: BuildError)
  case MissingApi(path: Path)
  case NoHome
  case Io(op: LoadIo)

  def message: String = this match
    case MissingFile(path)          => s"no build file at $path"
    case CompileFailed(diagnostics) => diagnostics.map(_.text).mkString("\n")
    case NoEntry                    => "histos.scala does not define one object that extends HistosBuild"
    case AmbiguousEntry(classes)    =>
      s"histos.scala defines more than one HistosBuild: ${classes.map(_.value).mkString(", ")}"
    case BrokenGraph(error)         => error.message
    case MissingApi(path)           => s"$path has no bootstrap api jar"
    case NoHome                     => "no user home, so the api jar cannot be cached"
    case Io(op)                     => s"load failed while ${op.toString.toLowerCase}"
