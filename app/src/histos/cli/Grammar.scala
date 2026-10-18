package histos.cli

import histos.task.TaskId
import histos.workflow.WorkflowAction

import zio.{IO, ZIO}
import zio.cli.{Args, CliConfig, Command as CliCommand, CommandDirective, Options, ValidationError, ValidationErrorType}

/** zio-cli grammar. The wire value still has the raw task token. [[parse]] turns that token into a [[TaskId]]. */
object Grammar:
  private enum Wire:
    case Version, Graph, Mcp, Acp, Bsp, CachePath, CacheList, Toolchain
    case Run(id: String)
    case Test(projects: List[String])
    case Compile(projects: List[String])
    case Assembly(destination: Option[String])
    case Publish(repository: String, projects: List[String])
    case CacheClean(dryRun: Boolean)
    case Resolve(update: Boolean, check: Boolean)
    case Workflow(check: Boolean, update: Boolean)

  private val version   = CliCommand("version").map(_ => Wire.Version)
  private val graph     = CliCommand("graph").map(_ => Wire.Graph)
  private val run       = CliCommand("run", Args.text("id")).map(Wire.Run(_))
  private val test      = CliCommand("test", Args.text("project").*).map(names => Wire.Test(names.toList))
  private val compile   = CliCommand("compile", Args.text("project").*).map(names => Wire.Compile(names.toList))
  private val assembly  =
    CliCommand("assembly", Args.text("destination").atMost(1)).map(paths => Wire.Assembly(paths.headOption))
  private val publish  =
    CliCommand("publish", Args.text("repository") ++ Args.text("project").*).map { case (repository, projects) =>
      Wire.Publish(repository, projects.toList)
    }
  private val mcp       = CliCommand("mcp").map(_ => Wire.Mcp)
  private val acp       = CliCommand("acp").map(_ => Wire.Acp)
  private val bsp       = CliCommand("bsp").map(_ => Wire.Bsp)
  private val toolchain = CliCommand("toolchain").map(_ => Wire.Toolchain)
  private val path      = CliCommand("path").map(_ => Wire.CachePath)
  private val list      = CliCommand("list").map(_ => Wire.CacheList)
  private val clean     = CliCommand("clean", Options.boolean("dry-run")).map(Wire.CacheClean(_))
  private val cache     = CliCommand("cache").subcommands(path | list | clean)
  private val resolve   =
    CliCommand("resolve", Options.boolean("update") ++ Options.boolean("check")).map { case (update, check) =>
      Wire.Resolve(update, check)
    }
  private val workflow  =
    CliCommand("workflow", Options.boolean("check") ++ Options.boolean("update")).map { case (check, update) =>
      Wire.Workflow(check, update)
    }
  private val root      =
    CliCommand("histos").subcommands(
      version | graph | run | test | compile | assembly | publish | mcp | acp | bsp | toolchain | cache | resolve | workflow,
    )

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
      case Wire.Bsp                => ZIO.succeed(Command.Bsp)
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
      case Wire.Workflow(true, true) => ZIO.fail(CliParseError.Rejected)
      case Wire.Workflow(_, update) =>
        ZIO.succeed(Command.Workflow(if update then WorkflowAction.Update else WorkflowAction.Check))
      case Wire.Test(projects) => ZIO.succeed(Command.Test(projects))
      case Wire.Compile(projects) => ZIO.succeed(Command.Compile(projects))
      case Wire.Assembly(destination) => ZIO.succeed(Command.Assembly(destination))
      case Wire.Publish(repository, projects) => ZIO.succeed(Command.Publish(repository, projects))
      case Wire.Run(raw)           =>
        ZIO.fromEither(TaskId.from(raw)).mapBoth(_ => CliParseError.BadTaskId, Command.Run(_))

  private def reject(error: ValidationError): CliParseError =
    error.validationErrorType match
      case ValidationErrorType.CommandMismatch                     => CliParseError.UnknownCommand
      case ValidationErrorType.InvalidArgument | ValidationErrorType.MissingValue =>
        CliParseError.MissingArgument
      case _                                                       => CliParseError.Rejected

