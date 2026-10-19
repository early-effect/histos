package histos.plugin

import _root_.scala.quoted.*

private[histos] object PluginMacros:
  def pluginId(raw: Expr[String])(using Quotes): Expr[PluginId] =
    literal(raw, "PluginId", text => textOf(PluginId.from(text))(_.message)(_.value), canonical =>
      '{ PluginId.trusted(${ Expr(canonical) }) },
    )

  def taskName(raw: Expr[String])(using Quotes): Expr[TaskName] =
    literal(raw, "TaskName", text => textOf(TaskName.from(text))(_.message)(_.value), canonical =>
      '{ TaskName.trusted(${ Expr(canonical) }) },
    )

  def commandName(raw: Expr[String])(using Quotes): Expr[CommandName] =
    literal(raw, "CommandName", text => textOf(CommandName.from(text))(_.message)(_.value), canonical =>
      '{ CommandName.trusted(${ Expr(canonical) }) },
    )

  def jobName(raw: Expr[String])(using Quotes): Expr[JobName] =
    literal(raw, "JobName", text => textOf(JobName.from(text))(_.message)(_.value), canonical =>
      '{ JobName.trusted(${ Expr(canonical) }) },
    )

  def entryName(raw: Expr[String])(using Quotes): Expr[EntryName] =
    literal(raw, "EntryName", text => textOf(EntryName.from(text))(_.message)(_.value), canonical =>
      '{ EntryName.trusted(${ Expr(canonical) }) },
    )

  def noteKey(raw: Expr[String])(using Quotes): Expr[NoteKey] =
    literal(raw, "NoteKey", text => textOf(NoteKey.from(text))(_.message)(_.value), canonical =>
      '{ NoteKey.trusted(${ Expr(canonical) }) },
    )

  def noteText(raw: Expr[String])(using Quotes): Expr[NoteText] =
    literal(raw, "NoteText", text => textOf(NoteText.from(text))(_.message)(_.value), canonical =>
      '{ NoteText.trusted(${ Expr(canonical) }) },
    )

  def outputName(raw: Expr[String])(using Quotes): Expr[OutputName] =
    literal(raw, "OutputName", text => textOf(OutputName.from(text))(_.message)(_.value), canonical =>
      '{ OutputName.trusted(${ Expr(canonical) }) },
    )

  def paramName(raw: Expr[String])(using Quotes): Expr[ParamName] =
    literal(raw, "ParamName", text => textOf(ParamName.from(text))(_.message)(_.value), canonical =>
      '{ ParamName.trusted(${ Expr(canonical) }) },
    )

  def flagName(raw: Expr[String])(using Quotes): Expr[FlagName] =
    literal(raw, "FlagName", text => textOf(FlagName.from(text))(_.message)(_.value), canonical =>
      '{ FlagName.trusted(${ Expr(canonical) }) },
    )

  def envName(raw: Expr[String])(using Quotes): Expr[EnvName] =
    literal(raw, "EnvName", text => textOf(EnvName.from(text))(_.message)(_.value), canonical =>
      '{ EnvName.trusted(${ Expr(canonical) }) },
    )

  def inputKey(raw: Expr[String])(using Quotes): Expr[InputKey] =
    literal(raw, "InputKey", text => textOf(InputKey.from(text))(_.message)(_.value), canonical =>
      '{ InputKey.trusted(${ Expr(canonical) }) },
    )

  def repoPath(raw: Expr[String])(using Quotes): Expr[RepoPath] =
    literal(raw, "RepoPath", text => textOf(RepoPath.from(text))(_.message)(_.value), canonical =>
      '{ RepoPath.trusted(${ Expr(canonical) }) },
    )

  def pluginDetail(raw: Expr[String])(using Quotes): Expr[PluginDetail] =
    literal(raw, "PluginDetail", text => textOf(PluginDetail.from(text))(_.message)(_.value), canonical =>
      '{ PluginDetail.trusted(${ Expr(canonical) }) },
    )

  def apiDigest(raw: Expr[String])(using Quotes): Expr[ApiDigest] =
    literal(raw, "ApiDigest", text => textOf(ApiDigest.from(text))(_.message)(_.value), canonical =>
      '{ ApiDigest.trusted(${ Expr(canonical) }) },
    )

  def contentHash(raw: Expr[String])(using Quotes): Expr[ContentHash] =
    literal(raw, "ContentHash", text => textOf(ContentHash.from(text))(_.message)(_.value), canonical =>
      '{ ContentHash.trusted(${ Expr(canonical) }) },
    )

  def cron(raw: Expr[String])(using Quotes): Expr[Cron] =
    literal(raw, "Cron", text => textOf(Cron.from(text))(_.message)(_.value), canonical =>
      '{ Cron.trusted(${ Expr(canonical) }) },
    )

  private def textOf[E, A](parsed: Either[E, A])(message: E => String)(value: A => String): Either[String, String] =
    parsed.left.map(message).map(value)

  private def literal[A](
      raw: Expr[String],
      what: String,
      parse: String => Either[String, String],
      succeed: String => Expr[A],
  )(using Quotes): Expr[A] =
    import quotes.reflect.report
    raw.value match
      case None => report.errorAndAbort(s"$what(...) takes a literal. Use $what.from at runtime.", raw)
      case Some(text) =>
        parse(text) match
          case Left(message) => report.errorAndAbort(message, raw)
          case Right(canonical) => succeed(canonical)
