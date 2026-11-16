package histos.plugin

import _root_.scala.quoted.*

private[histos] object PluginMacros:
  def pluginId(raw: Expr[String])(using Quotes): Expr[PluginId] =
    literal(
      raw,
      "PluginId",
      text => PluginId.from(text).map(_.value),
      _.message,
      canonical => '{ PluginId.trusted(${ Expr(canonical) }) },
    )

  def taskName(raw: Expr[String])(using Quotes): Expr[TaskName] =
    literal(
      raw,
      "TaskName",
      text => TaskName.from(text).map(_.value),
      _.message,
      canonical => '{ TaskName.trusted(${ Expr(canonical) }) },
    )

  def commandName(raw: Expr[String])(using Quotes): Expr[CommandName] =
    literal(
      raw,
      "CommandName",
      text => CommandName.from(text).map(_.value),
      _.message,
      canonical => '{ CommandName.trusted(${ Expr(canonical) }) },
    )

  def jobName(raw: Expr[String])(using Quotes): Expr[JobName] =
    literal(
      raw,
      "JobName",
      text => JobName.from(text).map(_.value),
      _.message,
      canonical => '{ JobName.trusted(${ Expr(canonical) }) },
    )

  def entryName(raw: Expr[String])(using Quotes): Expr[EntryName] =
    literal(
      raw,
      "EntryName",
      text => EntryName.from(text).map(_.value),
      _.message,
      canonical => '{ EntryName.trusted(${ Expr(canonical) }) },
    )

  def noteKey(raw: Expr[String])(using Quotes): Expr[NoteKey] =
    literal(
      raw,
      "NoteKey",
      text => NoteKey.from(text).map(_.value),
      _.message,
      canonical => '{ NoteKey.trusted(${ Expr(canonical) }) },
    )

  def noteText(raw: Expr[String])(using Quotes): Expr[NoteText] =
    literal(
      raw,
      "NoteText",
      text => NoteText.from(text).map(_.value),
      _.message,
      canonical => '{ NoteText.trusted(${ Expr(canonical) }) },
    )

  def outputName(raw: Expr[String])(using Quotes): Expr[OutputName] =
    literal(
      raw,
      "OutputName",
      text => OutputName.from(text).map(_.value),
      _.message,
      canonical => '{ OutputName.trusted(${ Expr(canonical) }) },
    )

  def paramName(raw: Expr[String])(using Quotes): Expr[ParamName] =
    literal(
      raw,
      "ParamName",
      text => ParamName.from(text).map(_.value),
      _.message,
      canonical => '{ ParamName.trusted(${ Expr(canonical) }) },
    )

  def flagName(raw: Expr[String])(using Quotes): Expr[FlagName] =
    literal(
      raw,
      "FlagName",
      text => FlagName.from(text).map(_.value),
      _.message,
      canonical => '{ FlagName.trusted(${ Expr(canonical) }) },
    )

  def envName(raw: Expr[String])(using Quotes): Expr[EnvName] =
    literal(
      raw,
      "EnvName",
      text => EnvName.from(text).map(_.value),
      _.message,
      canonical => '{ EnvName.trusted(${ Expr(canonical) }) },
    )

  def inputKey(raw: Expr[String])(using Quotes): Expr[InputKey] =
    literal(
      raw,
      "InputKey",
      text => InputKey.from(text).map(_.value),
      _.message,
      canonical => '{ InputKey.trusted(${ Expr(canonical) }) },
    )

  def repoPath(raw: Expr[String])(using Quotes): Expr[RepoPath] =
    literal(
      raw,
      "RepoPath",
      text => RepoPath.from(text).map(_.value),
      _.message,
      canonical => '{ RepoPath.trusted(${ Expr(canonical) }) },
    )

  def pluginDetail(raw: Expr[String])(using Quotes): Expr[PluginDetail] =
    literal(
      raw,
      "PluginDetail",
      text => PluginDetail.from(text).map(_.value),
      _.message,
      canonical => '{ PluginDetail.trusted(${ Expr(canonical) }) },
    )

  def apiDigest(raw: Expr[String])(using Quotes): Expr[ApiDigest] =
    literal(
      raw,
      "ApiDigest",
      text => ApiDigest.from(text).map(_.value),
      _.message,
      canonical => '{ ApiDigest.trusted(${ Expr(canonical) }) },
    )

  def contentHash(raw: Expr[String])(using Quotes): Expr[ContentHash] =
    literal(
      raw,
      "ContentHash",
      text => ContentHash.from(text).map(_.value),
      _.message,
      canonical => '{ ContentHash.trusted(${ Expr(canonical) }) },
    )

  def runnerName(raw: Expr[String])(using Quotes): Expr[RunnerName] =
    literal(
      raw,
      "RunnerName",
      text => RunnerName.from(text).map(_.value),
      _.message,
      canonical => '{ RunnerName.trusted(${ Expr(canonical) }) },
    )

  def environmentName(raw: Expr[String])(using Quotes): Expr[EnvironmentName] =
    literal(
      raw,
      "EnvironmentName",
      text => EnvironmentName.from(text).map(_.value),
      _.message,
      canonical => '{ EnvironmentName.trusted(${ Expr(canonical) }) },
    )

  def targetName(raw: Expr[String])(using Quotes): Expr[TargetName] =
    literal(
      raw,
      "TargetName",
      text => TargetName.from(text).map(_.value),
      _.message,
      canonical => '{ TargetName.trusted(${ Expr(canonical) }) },
    )

  def actionName(raw: Expr[String])(using Quotes): Expr[ActionName] =
    literal(
      raw,
      "ActionName",
      text => ActionName.from(text).map(_.value),
      _.message,
      canonical => '{ ActionName.trusted(${ Expr(canonical) }) },
    )

  def secretName(raw: Expr[String])(using Quotes): Expr[SecretName] =
    literal(
      raw,
      "SecretName",
      text => SecretName.from(text).map(_.value),
      _.message,
      canonical => '{ SecretName.trusted(${ Expr(canonical) }) },
    )

  def groupName(raw: Expr[String])(using Quotes): Expr[GroupName] =
    literal(
      raw,
      "GroupName",
      text => GroupName.from(text).map(_.value),
      _.message,
      canonical => '{ GroupName.trusted(${ Expr(canonical) }) },
    )

  def cron(raw: Expr[String])(using Quotes): Expr[Cron] =
    literal(
      raw,
      "Cron",
      text => Cron.from(text).map(_.value),
      _.message,
      canonical => '{ Cron.trusted(${ Expr(canonical) }) },
    )

  def sourceId(raw: Expr[String])(using Quotes): Expr[SourceId] =
    literal(
      raw,
      "SourceId",
      text => SourceId.from(text).map(_.value),
      _.message,
      canonical => '{ SourceId.trusted(${ Expr(canonical) }) },
    )

  def serviceKey(raw: Expr[String])(using Quotes): Expr[ServiceKey] =
    literal(
      raw,
      "ServiceKey",
      text => ServiceKey.from(text).map(_.value),
      _.message,
      canonical => '{ ServiceKey.trusted(${ Expr(canonical) }) },
    )

  def lockRevision(raw: Expr[String])(using Quotes): Expr[LockRevision] =
    literal(
      raw,
      "LockRevision",
      text => LockRevision.from(text).map(_.value),
      _.message,
      canonical => '{ LockRevision.trusted(${ Expr(canonical) }) },
    )

  def proposalId(raw: Expr[String])(using Quotes): Expr[ProposalId] =
    literal(
      raw,
      "ProposalId",
      text => ProposalId.from(text).map(_.value),
      _.message,
      canonical => '{ ProposalId.trusted(${ Expr(canonical) }) },
    )

  def recipeNodeId(raw: Expr[String])(using Quotes): Expr[RecipeNodeId] =
    literal(
      raw,
      "RecipeNodeId",
      text => RecipeNodeId.from(text).map(_.value),
      _.message,
      canonical => '{ RecipeNodeId.trusted(${ Expr(canonical) }) },
    )

  def generationId(raw: Expr[String])(using Quotes): Expr[GenerationId] =
    literal(
      raw,
      "GenerationId",
      text => GenerationId.from(text).map(_.value),
      _.message,
      canonical => '{ GenerationId.trusted(${ Expr(canonical) }) },
    )

  def invocationId(raw: Expr[String])(using Quotes): Expr[InvocationId] =
    literal(
      raw,
      "InvocationId",
      text => InvocationId.from(text).map(_.value),
      _.message,
      canonical => '{ InvocationId.trusted(${ Expr(canonical) }) },
    )

  def clientId(raw: Expr[String])(using Quotes): Expr[ClientId] =
    literal(
      raw,
      "ClientId",
      text => ClientId.from(text).map(_.value),
      _.message,
      canonical => '{ ClientId.trusted(${ Expr(canonical) }) },
    )

  def commandToken(raw: Expr[String])(using Quotes): Expr[CommandToken] =
    literal(
      raw,
      "CommandToken",
      text => CommandToken.from(text).map(_.value),
      _.message,
      canonical => '{ CommandToken.trusted(${ Expr(canonical) }) },
    )

  def recipeFeature(raw: Expr[String])(using Quotes): Expr[RecipeFeature] =
    literal(
      raw,
      "RecipeFeature",
      text => RecipeFeature.from(text).map(_.value),
      _.message,
      canonical => '{ RecipeFeature.trusted(${ Expr(canonical) }) },
    )

  def recipeInput(raw: Expr[String])(using Quotes): Expr[RecipeInput] =
    literal(
      raw,
      "RecipeInput",
      text => RecipeInput.from(text).map(_.value),
      _.message,
      canonical => '{ RecipeInput.trusted(${ Expr(canonical) }) },
    )

  def artifactPath(raw: Expr[String])(using Quotes): Expr[ArtifactPath] =
    literal(
      raw,
      "ArtifactPath",
      text => ArtifactPath.from(text).map(_.value),
      _.message,
      canonical => '{ ArtifactPath.trusted(${ Expr(canonical) }) },
    )

  private def literal[E, A](
      raw: Expr[String],
      what: String,
      parse: String => Either[E, String],
      message: E => String,
      succeed: String => Expr[A],
  )(using Quotes): Expr[A] =
    import quotes.reflect.report
    raw.value match
      case None       => report.errorAndAbort(s"$what(...) takes a literal. Use $what.from at runtime.", raw)
      case Some(text) =>
        parse(text) match
          case Left(error)      => report.errorAndAbort(message(error), raw)
          case Right(canonical) => succeed(canonical)
end PluginMacros
