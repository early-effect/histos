package histos

import _root_.scala.deriving.Mirror

import zio.json.*
import zio.test.*

/** Round trips and golden documents. Equalities stay in `assertTrue`, so a bad case shows in the diff. No law here
  * reads zio-json's own error text.
  */
object JsonLaw:
  /** One sample per case, and each sample decodes to itself. */
  inline def enumLaw[A <: _root_.scala.reflect.Enum: JsonCodec](
      law: String,
      values: List[A],
  )(using Mirror.SumOf[A]): Spec[Any, Nothing] =
    trips(law, values, EnumLaw.covered(values))

  /** One document per case. The JVM server writes these bytes and the native client reads them, and the law runs on
    * both platforms against the same literal.
    */
  inline def golden[A <: _root_.scala.reflect.Enum: JsonCodec](
      law: String,
      rows: List[(A, String)],
  )(using Mirror.SumOf[A]): Spec[Any, Nothing] =
    documents(law, rows, EnumLaw.covered(rows.map(_._1)))

  /** Documents for some values of a type, with no coverage gate. */
  def sent[A: JsonCodec](law: String, rows: List[(A, String)]): Spec[Any, Nothing] =
    test(law)(pinned(rows))

  def values[A: JsonCodec](law: String, items: List[A]): Spec[Any, Nothing] =
    test(law):
      assertTrue(items.map(item => item.toJson.fromJson[A]) == items.map(Right(_)))

  /** A text codec over a parser. The sample round-trips, and text the parser refuses does not decode. */
  def parsed[E, A: JsonCodec](good: A, raw: String, read: String => Either[E, A]): TestResult =
    assertTrue(
      good.toJson.fromJson[A] == Right(good),
      read(raw).isLeft,
      raw.toJson.fromJson[A].isLeft,
    )

  private def trips[A: JsonCodec](law: String, values: List[A], covered: Boolean): Spec[Any, Nothing] =
    test(law):
      assertTrue(covered, values.map(value => value.toJson.fromJson[A]) == values.map(Right(_)))

  private def documents[A: JsonCodec](law: String, rows: List[(A, String)], covered: Boolean): Spec[Any, Nothing] =
    test(law)(assertTrue(covered) && pinned(rows))

  private def pinned[A: JsonCodec](rows: List[(A, String)]): TestResult =
    val values = rows.map(_._1)
    assertTrue(
      values.map(_.toJson) == rows.map(_._2),
      rows.map(row => row._2.fromJson[A]) == values.map(Right(_)),
    )
end JsonLaw
