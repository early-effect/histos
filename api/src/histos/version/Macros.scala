package histos.version

import _root_.scala.quoted.*

private[histos] object ShipNumberMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[ShipNumber] =
    import quotes.reflect.report
    raw.value match
      case None => report.errorAndAbort("ShipNumber(...) takes a literal. Use ShipNumber.from at runtime.", raw)
      case Some(text) =>
        ShipNumber.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(number) =>
            '{ ShipNumber.trusted(${ Expr(number.major) }, ${ Expr(number.minor) }, ${ Expr(number.patch) }) }

private[histos] object ManualVersionMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[ManualVersion] =
    import quotes.reflect.report
    raw.value match
      case None => report.errorAndAbort("ManualVersion(...) takes a literal. Use ManualVersion.from at runtime.", raw)
      case Some(text) =>
        ManualVersion.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ ManualVersion.trusted(${ Expr(text) }) }

private[histos] object CommitShaMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[CommitSha] =
    import quotes.reflect.report
    raw.value match
      case None => report.errorAndAbort("CommitSha(...) takes a literal. Use CommitSha.from at runtime.", raw)
      case Some(text) =>
        CommitSha.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ CommitSha.trusted(${ Expr(text.toLowerCase) }) }
