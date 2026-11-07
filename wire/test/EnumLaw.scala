package histos

import _root_.scala.deriving.Mirror

import compiletime.constValue

/** One sample per enum case, in declaration order. A skipped case or a new case fails the law. */
object EnumLaw:
  inline def covered[A <: _root_.scala.reflect.Enum](values: List[A])(using mirror: Mirror.SumOf[A]): Boolean =
    values.map(_.ordinal) == (0 until constValue[Tuple.Size[mirror.MirroredElemTypes]]).toList
