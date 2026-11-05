package histos.doctor

import _root_.scala.quoted.*

private[doctor] object DoctorMacros:
  def checkId(raw: Expr[String])(using Quotes): Expr[CheckId] =
    literal("CheckId", raw, CheckId.from, _.message, text => '{ CheckId.trusted(${ Expr(text) }) })

  private def literal[Error, Value](
      name: String,
      raw: Expr[String],
      parse: String => Either[Error, Value],
      message: Error => String,
      trusted: String => Expr[Value],
  )(using Quotes): Expr[Value] =
    import quotes.reflect.report
    raw.value match
      case None       => report.errorAndAbort(s"$name(...) takes a literal. Use $name.from at runtime.", raw)
      case Some(text) =>
        parse(text) match
          case Left(error) => report.errorAndAbort(message(error), raw)
          case Right(_)    => trusted(text)
end DoctorMacros
