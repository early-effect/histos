package histos.cli

import histos.plugin.{CliParam, CommandSpec, FlagName, NamedText, PluginArgs, PluginId, PluginTaskId}

/** A contributed command and the plugin that owns it. The plugin does not parse argv. */
final case class BoundCommand(plugin: PluginId, spec: CommandSpec)

object PluginCommands:
  def parse(commands: List[BoundCommand], args: List[String]): Either[CliParseError, (PluginTaskId, PluginArgs)] =
    args match
      case Nil => Left(CliParseError.UnknownCommand)
      case name :: rest =>
        commands.find(_.spec.name.value == name) match
          case None       => Left(CliParseError.UnknownCommand)
          case Some(bound) => arguments(bound, rest).map(parsed => (PluginTaskId(bound.plugin, bound.spec.task), parsed))

  def usage(commands: List[BoundCommand]): String =
    val extra = commands.map(bound => s"       ${bound.spec.name.value}  ${bound.spec.summary.value}")
    if extra.isEmpty then Cli.usageText else (Cli.usageText :: extra).mkString("\n")

  private def arguments(bound: BoundCommand, args: List[String]): Either[CliParseError, PluginArgs] =
    val required = bound.spec.params.collect { case CliParam.Text(name) => name }
    val optional = bound.spec.params.collect { case CliParam.OptionalText(name) => name }
    val flags = bound.spec.params.collect { case CliParam.Flag(name) => name }.toSet
    if optional.length > 1 then Left(CliParseError.Rejected)
    else
      split(args, flags).flatMap: (positionals, seen) =>
        val limit = required.length + optional.length
        if positionals.length < required.length then Left(CliParseError.MissingArgument)
        else if positionals.length > limit then Left(CliParseError.UnexpectedArguments)
        else
          val texts = required.zip(positionals).map((name, value) => NamedText(name, value))
          val more = optional.zip(positionals.drop(required.length)).map((name, value) => NamedText(name, value))
          Right(PluginArgs(texts ++ more, seen))

  private def split(
      args: List[String],
      flags: Set[FlagName],
  ): Either[CliParseError, (List[String], Set[FlagName])] =
    def walk(
        rest: List[String],
        positionals: List[String],
        seen: Set[FlagName],
    ): Either[CliParseError, (List[String], Set[FlagName])] =
      rest match
        case Nil => Right((positionals.reverse, seen))
        case token :: tail if token.startsWith("--") =>
          FlagName.from(token.drop(2)) match
            case Right(flag) if flags.contains(flag) => walk(tail, positionals, seen + flag)
            case _                                   => Left(CliParseError.Rejected)
        case token :: tail => walk(tail, token :: positionals, seen)
    walk(args, Nil, Set.empty)
