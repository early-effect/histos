package histos.cli

import histos.task.TaskId

import zio.{IO, ZIO}
import zio.cli.{Args, CliConfig, Command as CliCommand, CommandDirective, Options, ValidationError, ValidationErrorType}

/** zio-cli grammar. The wire value still has the raw task token. [[parse]] turns that token into a [[TaskId]]. */
object Grammar:
  private enum Wire:
    case Version, Graph, Mcp, Acp, CachePath, CacheList, Toolchain
    case Run(id: String)
    case CacheClean(dryRun: Boolean)
    case Resolve(update: Boolean, check: Boolean)

  private val version   = CliCommand("version").map(_ => Wire.Version)
  private val graph     = CliCommand("graph").map(_ => Wire.Graph)
  private val run       = CliCommand("run", Args.text("id")).map(Wire.Run(_))
  private val mcp       = CliCommand("mcp").map(_ => Wire.Mcp)
  private val acp       = CliCommand("acp").map(_ => Wire.Acp)
  private val toolchain = CliCommand("toolchain").map(_ => Wire.Toolchain)
  private val path      = CliCommand("path").map(_ => Wire.CachePath)
  private val list      = CliCommand("list").map(_ => Wire.CacheList)
  private val clean     = CliCommand("clean", Options.boolean("dry-run")).map(Wire.CacheClean(_))
  private val cache     = CliCommand("cache").subcommands(path | list | clean)
  private val resolve   =
    CliCommand("resolve", Options.boolean("update") ++ Options.boolean("check")).map { case (update, check) =>
      Wire.Resolve(update, check)
    }
  private val root      =
    CliCommand("histos").subcommands(version | graph | run | mcp | acp | toolchain | cache | resolve)

  def parse(args: List[String]): IO[CliParseError, Command] =
    root.parse("histos" :: args, CliConfig.default).mapError(reject).flatMap:
      case CommandDirective.UserDefined(Nil, wire) => command(wire)
      case CommandDirective.UserDefined(_, _)      => ZIO.fail(CliParseError.UnexpectedArguments)
      case CommandDirective.BuiltIn(_)             => ZIO.fail(CliParseError.Help)

  private def command(wire: Wire): IO[CliParseError, Command] =
    wire match
      case Wire.Version            => ZIO.succeed(Command.Version)
      case Wire.Graph              => ZIO.succeed(Command.Graph)
      case Wire.Mcp                => ZIO.succeed(Command.Mcp)
      case Wire.Acp                => ZIO.succeed(Command.Acp)
      case Wire.CachePath          => ZIO.succeed(Command.CachePath)
      case Wire.CacheList          => ZIO.succeed(Command.CacheList)
      case Wire.Toolchain          => ZIO.succeed(Command.Toolchain)
      case Wire.CacheClean(dryRun) => ZIO.succeed(Command.CacheClean(dryRun))
      case Wire.Resolve(true, true) => ZIO.fail(CliParseError.Rejected)
      case Wire.Resolve(update, check) =>
        val action =
          if update then ResolveAction.Update
          else if check then ResolveAction.Check
          else ResolveAction.Verify
        ZIO.succeed(Command.Resolve(action))
      case Wire.Run(raw)           =>
        ZIO.fromEither(TaskId.from(raw)).mapBoth(_ => CliParseError.BadTaskId, Command.Run(_))

  private def reject(error: ValidationError): CliParseError =
    error.validationErrorType match
      case ValidationErrorType.CommandMismatch                     => CliParseError.UnknownCommand
      case ValidationErrorType.InvalidArgument | ValidationErrorType.MissingValue =>
        CliParseError.MissingArgument
      case _                                                       => CliParseError.Rejected

