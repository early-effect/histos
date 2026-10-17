package histos.cli

import zio.{IO, ZIO}
import zio.cli.{Args, CliConfig, Command as CliCommand, CommandDirective, HelpDoc, Options, ValidationError, ValidationErrorType}

/** zio-cli grammar. It parses into [[Command]] and does not run anything. */
object Grammar:
  private val version = CliCommand("version").map(_ => Command.Version)
  private val graph   = CliCommand("graph").map(_ => Command.Graph)
  private val run     = CliCommand("run", Args.text("id")).map(Command.Run(_))
  private val mcp     = CliCommand("mcp").map(_ => Command.Mcp)
  private val acp     = CliCommand("acp").map(_ => Command.Acp)
  private val path    = CliCommand("path").map(_ => Command.CachePath)
  private val list    = CliCommand("list").map(_ => Command.CacheList)
  private val clean   = CliCommand("clean", Options.boolean("dry-run")).map(Command.CacheClean(_))
  private val cache   = CliCommand("cache").subcommands(path | list | clean)
  private val root    = CliCommand("histos").subcommands(version | graph | run | mcp | acp | cache)

  def parse(args: List[String]): IO[ValidationError, Command] =
    root.parse("histos" :: args, CliConfig.default).flatMap:
      case CommandDirective.UserDefined(Nil, command) => ZIO.succeed(command)
      case CommandDirective.UserDefined(_, _)         =>
        ZIO.fail(ValidationError(ValidationErrorType.InvalidArgument, HelpDoc.p(Cli.usageText)))
      case CommandDirective.BuiltIn(_) =>
        ZIO.fail(ValidationError(ValidationErrorType.InvalidArgument, HelpDoc.p(Cli.usageText)))
