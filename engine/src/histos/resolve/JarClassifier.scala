package histos.resolve

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

opaque type JarClassifier = String

object JarClassifier:
  given CanEqual[JarClassifier, JarClassifier] = CanEqual.derived
  inline def apply(inline raw: String): JarClassifier = ${
    JarClassifierMacro.literal('raw)
  }
  def from(raw: String): Either[ClassifierError, JarClassifier] =
    if raw.isEmpty then Left(ClassifierError.Empty)
    else if raw.forall(character =>
        character.isLetterOrDigit && character <= 127 || character == '-' || character == '_' || character == '.'
      )
    then Right(trusted(raw))
    else Left(ClassifierError.Invalid(raw))
  extension (classifier: JarClassifier) def value: String = classifier
  @publicInBinary private[histos] def trusted(raw: String): JarClassifier = raw

enum ClassifierError:
  case Empty
  case Invalid(raw: String)
  def message: String = this match
    case Empty        => "a jar classifier is not empty"
    case Invalid(raw) => s"'$raw' is not a jar classifier"

private object JarClassifierMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[JarClassifier] =
    raw.value match
      case None =>
        quotes.reflect.report.errorAndAbort(
          "JarClassifier(...) takes a literal. Use JarClassifier.from at runtime.",
          raw
        )
      case Some(value) =>
        JarClassifier.from(value) match
          case Left(error) =>
            quotes.reflect.report.errorAndAbort(error.message, raw)
          case Right(_) => '{ JarClassifier.trusted(${ Expr(value) }) }
