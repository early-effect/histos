package histos

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

import java.nio.file.Path

import _root_.scala.util.Try

/** An absolute path of a binary named `node`. A Js launch checks that the file exists and is executable. */
opaque type NodePath = String

object NodePath:
  given CanEqual[NodePath, NodePath] = CanEqual.derived

  /** A literal (`NodePath("/usr/bin/node")`), rejected at compile time when it is not an absolute `node` binary. */
  inline def apply(inline raw: String): NodePath = ${ NodePathMacro.literal('raw) }

  def from(raw: String): Either[NodePathError, NodePath] =
    val text = raw.strip
    if text.isEmpty then Left(NodePathError.Empty)
    else if text.exists(_ == '\u0000') then Left(NodePathError.Invalid(text))
    else
      Try(Path.of(text)).toEither.left
        .map(_ => NodePathError.Invalid(text))
        .flatMap: path =>
          if !path.isAbsolute then Left(NodePathError.Relative(text))
          else named(path, text)

  extension (path: NodePath) def value: String = path

  @publicInBinary private[histos] def trusted(raw: String): NodePath = raw

  private def named(path: Path, raw: String): Either[NodePathError, NodePath] =
    Option(path.getFileName).map(_.toString) match
      case Some("node") => Right(trusted(path.normalize.toString))
      case _            => Left(NodePathError.NotBinary(raw))
end NodePath

enum NodePathError:
  case Empty
  case Relative(raw: String)
  case NotBinary(raw: String)
  case Invalid(raw: String)

  def message: String = this match
    case Empty          => "is empty"
    case Relative(raw)  => s"is not absolute: $raw"
    case NotBinary(raw) => s"is not a node binary: $raw"
    case Invalid(raw)   => s"is not a path: $raw"
end NodePathError

private object NodePathMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[NodePath] =
    import quotes.reflect.report
    raw.value match
      case None =>
        report.errorAndAbort("NodePath(...) takes a literal. Use NodePath.from at runtime.", raw)
      case Some(text) =>
        NodePath.from(text) match
          case Left(error) => report.errorAndAbort(error.message, raw)
          case Right(path) =>
            import NodePath.value
            '{ NodePath.trusted(${ Expr(path.value) }) }
