package histos

import _root_.scala.deriving.Mirror

import compiletime.constValue

import zio.json.*
import zio.test.*

/** Round-trip and the document zio-json rejects. Equalities stay in `assertTrue`, so a bad case shows in the diff. */
object JsonLaw:
  inline def enumLaw[A <: _root_.scala.reflect.Enum: JsonCodec](
      law: String,
      values: List[A],
  )(using mirror: Mirror.SumOf[A]): Spec[Any, Nothing] =
    checked(law, values, constValue[Tuple.Size[mirror.MirroredElemTypes]])

  private def checked[A <: _root_.scala.reflect.Enum: JsonCodec](
      law: String,
      values: List[A],
      cases: Int,
  ): Spec[Any, Nothing] =
    test(law):
      val back = values.map(value => value.toJson.fromJson[A])
      val kind = values match
        case head :: _ => head.toJson
        case Nil       => ""
      val quoted                    = kind.startsWith("\"")
      val number: Either[String, A] =
        if quoted then Left("(expected '\"' got '1')") else Left("(expected '{' got '1')")
      val alien: Either[String, A] =
        if quoted then Left("(invalid enumeration value)") else Left("(invalid disambiguator)")
      val covered  = values.map(_.ordinal) == (0 until cases).toList
      val document =
        assertTrue(
          covered,
          back == values.map(Right(_)),
          "1".fromJson[A] == number,
          (if quoted then "\"" else "{").fromJson[A] == Left("Unexpected end of input"),
          (if quoted then "\"__nope__\"" else "{\"__nope__\":{}}").fromJson[A] == alien,
        )
      if quoted then document
      else document && assertTrue("{}".fromJson[A] == Left("(expected non-empty object)"))

  /** A string codec. `bad` is the text inside the string, not a hand-built document. */
  def textLaw[A: JsonCodec](law: String, good: A, bad: String, message: String): Spec[Any, Nothing] =
    test(law):
      assertTrue(
        good.toJson.fromJson[A] == Right(good),
        bad.toJson.fromJson[A] == Left(s"($message)"),
        "1".fromJson[A] == Left("(expected string)"),
        "\"".fromJson[A] == Left("Unexpected end of input"),
      )

  def values[A: JsonCodec](law: String, items: List[A]): Spec[Any, Nothing] =
    test(law):
      assertTrue(items.map(item => item.toJson.fromJson[A]) == items.map(Right(_)))

  /** The decoder fails with the same typed message `read` returns. A valid `raw` fails the law. */
  def rejectedText[E, A: JsonCodec](
      good: A,
      raw: String,
      read: String => Either[E, A],
      message: E => String,
  ): TestResult =
    read(raw) match
      case Left(error) =>
        assertTrue(
          good.toJson.fromJson[A] == Right(good),
          raw.toJson.fromJson[A] == Left(s"(${message(error)})"),
          "1".fromJson[A] == Left("(expected string)"),
          "\"".fromJson[A] == Left("Unexpected end of input"),
        )
      case Right(_) =>
        assertTrue(raw.toJson.fromJson[A] == Left(s"$raw was accepted"))
end JsonLaw
