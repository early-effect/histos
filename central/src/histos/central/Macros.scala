package histos.central

import scala.quoted.*

private object DeploymentIdMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[DeploymentId] =
    import quotes.reflect.report
    raw.value match
      case None => report.errorAndAbort("DeploymentId(...) takes a literal. Use DeploymentId.from at runtime.", raw)
      case Some(text) =>
        DeploymentId.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(id) => '{ DeploymentId.trusted(${ Expr(id.value) }) }

private object EntryPathMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[EntryPath] =
    import quotes.reflect.report
    raw.value match
      case None       => report.errorAndAbort("EntryPath(...) takes a literal. Use EntryPath.from at runtime.", raw)
      case Some(text) =>
        EntryPath.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ EntryPath.trusted(${ Expr(text) }) }
