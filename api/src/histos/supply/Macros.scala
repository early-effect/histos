package histos.supply

import _root_.scala.quoted.*

private object SupplyMacros:
  def timeZone(raw: Expr[String])(using Quotes): Expr[TimeZone] =
    text("TimeZone", raw, TimeZone.from, _.message, value => '{ TimeZone.trusted(${ Expr(value) }) })

  def branch(raw: Expr[String])(using Quotes): Expr[BranchName] =
    text("BranchName", raw, BranchName.from, _.message, value => '{ BranchName.trusted(${ Expr(value) }) })

  def days(raw: Expr[Int])(using Quotes): Expr[Days] =
    whole("Days", raw, Days.from, _.message, value => '{ Days.trusted(${ Expr(value) }) })

  def hours(raw: Expr[Int])(using Quotes): Expr[Hours] =
    whole("Hours", raw, Hours.from, _.message, value => '{ Hours.trusted(${ Expr(value) }) })

  def ghsa(raw: Expr[String])(using Quotes): Expr[AdvisoryId] =
    text("AdvisoryId", raw, AdvisoryId.from, _.message, value => '{ AdvisoryId.trusted(${ Expr(value) }) })

  def reason(raw: Expr[String])(using Quotes): Expr[Reason] =
    text("Reason", raw, Reason.from, _.message, value => '{ Reason.trusted(${ Expr(value) }) })

  def date(raw: Expr[String])(using Quotes): Expr[Date] =
    text("Date", raw, Date.from, _.message, value => '{ Date.trusted(${ Expr(value) }) })

  def label(raw: Expr[String])(using Quotes): Expr[LabelName] =
    text("LabelName", raw, LabelName.from, _.message, value => '{ LabelName.trusted(${ Expr(value) }) })

  def person(raw: Expr[String])(using Quotes): Expr[PersonName] =
    text("PersonName", raw, PersonName.from, _.message, value => '{ PersonName.trusted(${ Expr(value) }) })

  def email(raw: Expr[String])(using Quotes): Expr[Email] =
    text("Email", raw, Email.from, _.message, value => '{ Email.trusted(${ Expr(value) }) })

  def ghsaInterp(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[AdvisoryId] =
    one(sc, args, "ghsa").fold(err => quotes.reflect.report.errorAndAbort(err), ghsa(_))

  def reasonInterp(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[Reason] =
    one(sc, args, "reason").fold(err => quotes.reflect.report.errorAndAbort(err), reason(_))

  def dateInterp(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[Date] =
    one(sc, args, "date").fold(err => quotes.reflect.report.errorAndAbort(err), date(_))

  private def text[Error, Value](
      name: String,
      raw: Expr[String],
      parse: String => Either[Error, Value],
      message: Error => String,
      trusted: String => Expr[Value],
  )(using Quotes): Expr[Value] =
    import quotes.reflect.report
    raw.value match
      case None        => report.errorAndAbort(s"$name(...) takes a literal. Use $name.from at runtime.", raw)
      case Some(value) =>
        parse(value) match
          case Left(error) => report.errorAndAbort(message(error), raw)
          case Right(_)    => trusted(value)

  private def whole[Error, Value](
      name: String,
      raw: Expr[Int],
      parse: Int => Either[Error, Value],
      message: Error => String,
      trusted: Int => Expr[Value],
  )(using Quotes): Expr[Value] =
    import quotes.reflect.report
    raw.value match
      case None        => report.errorAndAbort(s"$name(...) takes a literal. Use $name.from at runtime.", raw)
      case Some(value) =>
        parse(value) match
          case Left(error) => report.errorAndAbort(message(error), raw)
          case Right(_)    => trusted(value)

  private def one(sc: Expr[StringContext], args: Expr[Seq[String]], what: String)(using
      Quotes
  ): Either[String, Expr[String]] =
    import quotes.reflect.*
    def literal(term: Term): Option[String] =
      term match
        case Inlined(_, _, inner)                                                      => literal(inner)
        case Apply(_, List(Typed(Repeated(List(Literal(StringConstant(raw))), _), _))) => Some(raw)
        case _                                                                         => None
    args match
      case Varargs(Nil) =>
        literal(sc.asTerm) match
          case Some(raw) => Right(Expr(raw))
          case None      => Left(s"""$what"..." takes a literal""")
      case _ => Left(s"""$what"..." takes no arguments""")
end SupplyMacros

/** Interpolators for callers outside this package. The macro object stays package-private. */
object Interp:
  inline def ghsa(inline sc: StringContext, inline args: String*): AdvisoryId =
    ${ SupplyMacros.ghsaInterp('sc, 'args) }

  inline def reason(inline sc: StringContext, inline args: String*): Reason =
    ${ SupplyMacros.reasonInterp('sc, 'args) }

  inline def date(inline sc: StringContext, inline args: String*): Date =
    ${ SupplyMacros.dateInterp('sc, 'args) }
end Interp
